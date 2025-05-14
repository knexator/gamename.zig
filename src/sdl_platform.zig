const c = @cImport({
    @cDefine("SDL_DISABLE_OLD_NAMES", {});
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_revision.h");
    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_MAIN_HANDLED' should be defined before including 'SDL_main.h'.
    @cDefine("SDL_MAIN_HANDLED", {});
    @cInclude("SDL3/SDL_main.h");
});

const game = @import("game.zig");
const PlatformGives = game.PlatformGives;

comptime {
    std.testing.refAllDeclsRecursive(game);
    std.testing.refAllDeclsRecursive(Vec2);
}

const hot_reloading = @import("build_options").game_dynlib_path != null;
var my_game: if (@import("build_options").game_dynlib_path) |game_dynlib_path| struct {
    const Self = @This();

    api: *const game.CApi = &game.game_api,
    dyn_lib: ?std.DynLib = null,
    last_inode: std.fs.File.INode = 0,
    state: game.GameState,

    fn init(gpa: std.mem.Allocator, sdl_gl: game.Gl) !Self {
        return .{ .state = try .init(gpa, sdl_gl) };
    }

    fn deinit(self: *Self, gpa: std.mem.Allocator) void {
        self.state.deinit(gpa);
    }

    fn reload(self: *Self, gpa: std.mem.Allocator, sdl_gl: game.Gl) void {
        self.api.reload(&self.state, &gpa, &sdl_gl);
    }

    fn update(self: *Self, platform_gives: PlatformGives) !bool {
        try self.maybeReloadApi();
        return self.api.update(&self.state, &platform_gives);
    }

    // TODO(zig): use std.fs.watch.Watch once it works
    fn maybeReloadApi(self: *Self) !void {
        const should_reload = blk: {
            const f = try std.fs.openFileAbsolute(game_dynlib_path, .{});
            defer f.close();
            const cur_inode = (try f.stat()).inode;
            if (self.last_inode != cur_inode) {
                self.last_inode = cur_inode;
                break :blk true;
            } else break :blk self.dyn_lib == null;
        };

        if (!should_reload) return;

        if (self.dyn_lib) |*dyn_lib| dyn_lib.close();
        // const path = "./zig-out/lib/libgame.so";
        const path = if (@import("builtin").os.tag == .windows) blk: {
            try std.fs.copyFileAbsolute(game_dynlib_path, game_dynlib_path ++ ".tmp", .{});
            break :blk game_dynlib_path ++ ".tmp";
        } else game_dynlib_path;
        self.dyn_lib = try .open(path);
        self.api = self.dyn_lib.?.lookup(*const game.CApi, "game_api") orelse return error.LookupFail;
        std.log.debug("reloaded game code", .{});
    }
} else game.GameState = undefined;

var window_size: UVec2 = Vec2.new(game.metadata.desired_aspect_ratio, 1).scale(512).toInt(usize);
fn getWindowRect() Rect {
    return .{
        .top_left = .zero,
        .size = window_size.tof32(),
    };
}

/// positions are in [0..1]x[0..1]
var mouse = Mouse{ .cur = .init, .prev = .init };
var keyboard = Keyboard{ .cur = .init, .prev = .init };

const Sounds = std.meta.FieldEnum(@TypeOf(game.sounds));
var sound_data: std.EnumArray(Sounds, Sound) = .initUndefined();
var sound_queue: std.EnumSet(Sounds) = .initEmpty();

const Loops = std.meta.FieldEnum(@TypeOf(game.loops));
var loops: std.EnumArray(Loops, Loop) = .initUndefined();
var loop_volumes: std.EnumArray(Loops, f32) = .initFill(0);

const Renderables = std.meta.FieldEnum(@TypeOf(game.renderables));

var gl_procs: gl.ProcTable = undefined;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
pub fn main() !void {
    const gpa, const is_debug = gpa: {
        if (@import("builtin").os.tag == .wasi) break :gpa .{ std.heap.wasm_allocator, false };
        break :gpa switch (@import("builtin").mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
        };
    };
    defer if (is_debug) {
        // _ = debug_allocator.deinit();
        // assert(debug_allocator.deinit() == .ok);
    };

    var render_queue: game.RenderQueue = .init(gpa);
    defer render_queue.deinit();

    errdefer |err| if (err == error.SdlError) std.log.err("SDL error: {s}", .{c.SDL_GetError()});

    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_SetMainReady' should be called before calling 'SDL_Init'.
    c.SDL_SetMainReady();

    try errify(c.SDL_SetAppMetadata(
        game.metadata.name,
        "0.0.0",
        std.fmt.comptimePrint("com.{s}.{s}", .{ game.metadata.author, game.metadata.name }),
    ));

    try errify(c.SDL_Init(c.SDL_INIT_VIDEO | c.SDL_INIT_AUDIO));
    defer c.SDL_Quit();

    errify(c.SDL_SetHint(c.SDL_HINT_RENDER_VSYNC, "1")) catch {};

    try errify(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MAJOR_VERSION, gl.info.version_major));
    try errify(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MINOR_VERSION, gl.info.version_minor));
    if (gl.info.profile) |profile| try errify(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_PROFILE_MASK, switch (profile) {
        .core => c.SDL_GL_CONTEXT_PROFILE_CORE,
        else => @compileError("TODO"),
    }));
    try errify(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_FLAGS, c.SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG));

    try errify(c.SDL_GL_SetAttribute(c.SDL_GL_MULTISAMPLEBUFFERS, 1));
    try errify(c.SDL_GL_SetAttribute(c.SDL_GL_MULTISAMPLESAMPLES, 16));

    const sdl_window: *c.SDL_Window = try errify(c.SDL_CreateWindow(
        game.metadata.name,
        @intCast(window_size.x),
        @intCast(window_size.y),
        c.SDL_WINDOW_OPENGL | c.SDL_WINDOW_RESIZABLE | (if (hot_reloading) c.SDL_WINDOW_ALWAYS_ON_TOP else 0),
    ));
    defer c.SDL_DestroyWindow(sdl_window);

    try errify(c.SDL_SetWindowAspectRatio(
        sdl_window,
        game.metadata.desired_aspect_ratio,
        game.metadata.desired_aspect_ratio,
    ));

    const gl_context = try errify(c.SDL_GL_CreateContext(sdl_window));
    defer errify(c.SDL_GL_DestroyContext(gl_context)) catch {};
    try errify(c.SDL_GL_MakeCurrent(sdl_window, gl_context));
    defer errify(c.SDL_GL_MakeCurrent(sdl_window, null)) catch {};
    if (!gl_procs.init(c.SDL_GL_GetProcAddress)) return error.GlInitFailed;
    gl.makeProcTableCurrent(&gl_procs);
    defer gl.makeProcTableCurrent(null);

    gl.Viewport(0, 0, @intCast(window_size.x), @intCast(window_size.y));
    gl.Enable(gl.BLEND);
    gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    // TODO: defer unloading
    inline for (comptime std.enums.values(Sounds)) |sound| {
        const path = @field(game.sounds, @tagName(sound));
        sound_data.set(sound, try .init(path));
    }

    const audio_device = try errify(c.SDL_OpenAudioDevice(
        c.SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
        &Sound.spec,
    ));
    defer c.SDL_CloseAudioDevice(audio_device);

    // TODO: defer unloading
    inline for (comptime std.enums.values(Loops)) |loop| {
        const path = @field(game.loops, @tagName(loop));
        try loops.getPtr(loop).init(path, audio_device);
    }

    var audio_streams_buf: [8]*c.SDL_AudioStream = undefined;
    var audio_streams: []*c.SDL_AudioStream = audio_streams_buf[0..0];

    defer while (audio_streams.len != 0) {
        c.SDL_DestroyAudioStream(audio_streams[audio_streams.len - 1]);
        audio_streams.len -= 1;
    };
    while (audio_streams.len < audio_streams_buf.len) {
        audio_streams.len += 1;
        audio_streams[audio_streams.len - 1] = try errify(c.SDL_CreateAudioStream(&Sound.spec, null));
    }

    try errify(c.SDL_BindAudioStreams(audio_device, @ptrCast(audio_streams.ptr), @intCast(audio_streams.len)));

    const sdl_gl = struct {
        pub const vtable: game.Gl = .{
            .clear = clear,
            .buildRenderable = buildRenderable,
            .useRenderable = useRenderable,
        };

        pub fn clear(color: FColor) void {
            gl.ClearBufferfv(gl.COLOR, 0, &color.toArray());
        }

        pub fn buildRenderable(
            vertex_src: [:0]const u8,
            fragment_src: [:0]const u8,
            attributes: []const game.Gl.VertexInfo.In,
            uniforms: []const game.Gl.UniformInfo.In,
        ) !game.Gl.Renderable {
            const program = try (ProgramInfo{
                .vertex = vertex_src,
                .fragment = fragment_src,
            }).load();

            var vao: c_uint = undefined;
            gl.GenVertexArrays(1, @ptrCast(&vao));

            gl.BindVertexArray(vao);

            var vbo: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&vbo));
            var ebo: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&ebo));

            gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, ebo);
            defer gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);

            gl.BindBuffer(gl.ARRAY_BUFFER, vbo);
            defer gl.BindBuffer(gl.ARRAY_BUFFER, 0);

            defer gl.BindVertexArray(0);

            for (attributes) |attribute| {
                // TODO: error check
                const attrib: gl.uint = @intCast(gl.GetAttribLocation(program, attribute.name));
                gl.EnableVertexAttribArray(attrib);
                // TODO NOW
                const VertexData = extern struct { a_position: Vec2 };
                const T = Vec2;
                gl.VertexAttribPointer(
                    attrib,
                    howManyElementsIn(T),
                    getVertexAttribType(T),
                    isVertexAttribNormalized(T),
                    @sizeOf(VertexData),
                    @offsetOf(VertexData, "a_position"),
                );
            }

            var uniforms_data = std.BoundedArray(game.Gl.UniformInfo, 8).init(0) catch unreachable;
            for (uniforms) |uniform| {
                const location = gl.GetUniformLocation(program, uniform.name);
                if (location == -1) return error.UniformLocationError;
                uniforms_data.append(.{
                    .location = location,
                    .name = uniform.name,
                    .kind = uniform.kind,
                }) catch return error.TooManyUniforms;
            }

            return .{
                .program = @enumFromInt(program),
                .vao = @enumFromInt(vao),
                .vbo = @enumFromInt(vbo),
                .ebo = @enumFromInt(ebo),
                .uniforms = uniforms_data,
            };
        }

        pub fn useRenderable(
            renderable: game.Gl.Renderable,
            vertices_ptr: *const anyopaque,
            vertices_len: usize,
            // vertices: []const anyopaque,
            // TODO: make triangles optional, since they could be precomputed
            triangles: []const [3]game.Gl.IndexType,
            uniforms: []const game.Gl.UniformInfo.Runtime,
            // TODO: textures, multiple textures
        ) void {
            {
                gl.BindBuffer(gl.ARRAY_BUFFER, @intFromEnum(renderable.vbo));
                gl.BufferData(
                    gl.ARRAY_BUFFER,
                    @intCast(renderable.sizeOfVertex() * vertices_len),
                    @ptrCast(vertices_ptr),
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ARRAY_BUFFER, 0);
            }

            {
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, @intFromEnum(renderable.ebo));
                gl.BufferData(
                    gl.ELEMENT_ARRAY_BUFFER,
                    @intCast(@sizeOf([3]game.Gl.IndexType) * triangles.len),
                    triangles.ptr,
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);
            }

            // if (texture) |t| gl.BindTexture(gl.TEXTURE_2D, t);
            // defer if (texture) |_| gl.BindTexture(gl.TEXTURE_2D, 0);

            gl.BindVertexArray(@intFromEnum(renderable.vao));
            defer gl.BindVertexArray(0);

            gl.UseProgram(@intFromEnum(renderable.program));
            defer gl.UseProgram(0);

            for (uniforms) |uniform| {
                // const u = uniform.location;
                // TODO
                const u = blk: {
                    for (renderable.uniforms.slice()) |u| {
                        if (std.mem.eql(u8, u.name, uniform.name)) break :blk u.location;
                    } else unreachable;
                };
                switch (uniform.value) {
                    .FColor => |v| gl.Uniform4f(u, v.r, v.g, v.b, v.a),
                    .Rect => |v| gl.Uniform4f(u, v.top_left.x, v.top_left.y, v.size.y, v.size.y),
                    .Point => |v| gl.Uniform4f(u, v.pos.x, v.pos.y, v.turns, v.scale),
                }
            }

            gl.DrawElements(gl.TRIANGLES, @intCast(3 * triangles.len), switch (game.Gl.IndexType) {
                u16 => gl.UNSIGNED_SHORT,
                else => @compileError("not implemented"),
            }, 0);
        }

        // TODO: move these to Gl
        fn getVertexAttribType(x: type) comptime_int {
            return switch (x) {
                f32 => gl.FLOAT,
                Vec2 => gl.FLOAT,
                Color => gl.UNSIGNED_BYTE,
                else => @compileError(std.fmt.comptimePrint("unhandled type: {any}", .{x})),
            };
        }

        fn howManyElementsIn(x: type) comptime_int {
            return switch (x) {
                f32 => 1,
                Vec2 => 2,
                Color => 4,
                else => @compileError(std.fmt.comptimePrint("unhandled type: {any}", .{x})),
            };
        }

        fn isVertexAttribNormalized(x: type) comptime_int {
            return switch (x) {
                f32 => gl.FALSE,
                Vec2 => gl.FALSE,
                Color => gl.TRUE,
                else => @compileError(std.fmt.comptimePrint("unhandled type: {any}", .{x})),
            };
        }
    };

    // TODO: use comptime magic
    var renderables = blk: {
        const fill_shape = game.renderables.fill_shape;
        break :blk .{
            .fill_shape = try Renderable(
                fill_shape.VertexData,
                fill_shape.IndexType,
                fill_shape.UniformTypes,
            ).init(
                fill_shape.vertex,
                fill_shape.fragment,
            ),
        };
    };
    defer renderables.fill_shape.deinit();

    // TODO: move to the game layer
    var text_renderer: TextRenderer = try .init(gpa, gpa);
    defer text_renderer.deinit();

    my_game = try .init(gpa, sdl_gl.vtable);
    // TODO: gl on deinit
    defer my_game.deinit(gpa);

    var sdl_platform: PlatformGives = .{
        .gpa = gpa,
        .render_queue = &render_queue,
        .getMouse = struct {
            pub fn anon(camera: Rect) Mouse {
                var result = mouse;
                result.cur.position = camera.applyToLocalPosition(result.cur.position);
                result.prev.position = camera.applyToLocalPosition(result.prev.position);
                // TODO: delete these fields
                result.cur.client_pos = .zero;
                result.prev.client_pos = .zero;
                return result;
            }
        }.anon,
        .keyboard = keyboard,
        .aspect_ratio = window_size.aspectRatio(),
        .delta_seconds = 0,
        .global_seconds = 0,
        .sound_queue = &sound_queue,
        .loop_volumes = &loop_volumes,
        .gl = sdl_gl.vtable,
    };

    var timer = try std.time.Timer.start();

    main_loop: while (true) {
        {
            var event: c.SDL_Event = undefined;
            while (c.SDL_PollEvent(&event)) {
                switch (event.type) {
                    c.SDL_EVENT_QUIT => break :main_loop,
                    c.SDL_EVENT_WINDOW_RESIZED => {
                        window_size = .new(
                            @intCast(event.window.data1),
                            @intCast(event.window.data2),
                        );
                        gl.Viewport(0, 0, @intCast(window_size.x), @intCast(window_size.y));
                    },
                    c.SDL_EVENT_MOUSE_BUTTON_DOWN, c.SDL_EVENT_MOUSE_BUTTON_UP => {
                        const is_pressed = event.button.down;
                        switch (event.button.button) {
                            c.SDL_BUTTON_LEFT => mouse.cur.buttons.left = is_pressed,
                            c.SDL_BUTTON_RIGHT => mouse.cur.buttons.right = is_pressed,
                            c.SDL_BUTTON_MIDDLE => mouse.cur.buttons.middle = is_pressed,
                            else => {},
                        }
                    },
                    c.SDL_EVENT_MOUSE_MOTION => {
                        mouse.cur.position = getWindowRect().localFromWorldPosition(
                            .new(event.motion.x, event.motion.y),
                        );
                    },
                    c.SDL_EVENT_MOUSE_WHEEL => {
                        mouse.cur.scrolled = if (event.wheel.y == 0)
                            .none
                        else if (event.wheel.y < 0)
                            .down
                        else
                            .up;
                    },
                    c.SDL_EVENT_KEY_DOWN, c.SDL_EVENT_KEY_UP => {
                        if (hot_reloading and event.key.scancode == c.SDL_SCANCODE_F5) {
                            // TODO: complete reload, respawning the window etc
                            // TODO: rename this to reset
                            my_game.reload(gpa, sdl_platform.gl);
                        } else {
                            const is_pressed = event.type == c.SDL_EVENT_KEY_DOWN;
                            inline for (sdl_scancode_to_keyboard_button) |pair| {
                                const sdl_scancode = pair[0];
                                const key = pair[1];
                                if (event.key.scancode == sdl_scancode) {
                                    @field(keyboard.cur.keys, @tagName(key)) = is_pressed;
                                    break;
                                }
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        // while (timekeeper.consume()) {
        {
            const ns_since_last_frame = timer.lap();
            sdl_platform.render_queue.pending_commands.clearRetainingCapacity();
            sdl_platform.delta_seconds = math.tof32(ns_since_last_frame) / std.time.ns_per_s;
            sdl_platform.global_seconds += sdl_platform.delta_seconds;
            sdl_platform.aspect_ratio = window_size.aspectRatio();
            sdl_platform.keyboard = keyboard;
            sdl_platform.sound_queue.* = .initEmpty();
            if (try my_game.update(sdl_platform)) break :main_loop;
            // try game.update(1.0 / 60.0);
            mouse.prev = mouse.cur;
            mouse.cur.scrolled = .none;
            keyboard.prev = keyboard.cur;
        }

        // Sound
        {
            // We have created eight SDL audio streams. When we want to play a sound effect,
            // we loop through the streams for the first one that isn't playing any audio
            // and write the audio to that stream.
            // This is a kind of stupid and naive way of handling audio, but it's very easy to
            // set up and use. A proper program would probably use an audio mixing callback.
            var stream_index: usize = 0;
            var it = sound_queue.iterator();
            iterate_sounds: while (it.next()) |sound| {
                const stream = find_available_stream: while (stream_index < audio_streams.len) {
                    defer stream_index += 1;
                    const stream = audio_streams[stream_index];
                    if (try errify(c.SDL_GetAudioStreamAvailable(stream)) == 0) {
                        break :find_available_stream stream;
                    }
                } else {
                    break :iterate_sounds;
                };
                const data = sound_data.get(sound).data;
                try errify(c.SDL_PutAudioStreamData(stream, data.ptr, @intCast(data.len)));
            }
        }

        // Loops
        {
            var it = loop_volumes.iterator();
            while (it.next()) |entry| {
                try errify(c.SDL_SetAudioStreamGain(loops.get(entry.key).stream, entry.value.*));
            }
        }

        // Draw
        {
            defer sdl_platform.render_queue.pending_commands.clearRetainingCapacity();
            var it = sdl_platform.render_queue.pending_commands.constIterator(0);
            // TODO: don't use gpa as the scratch allocator
            while (it.next()) |cmd| try render(renderables, text_renderer, cmd.*, gpa);

            try errify(c.SDL_GL_SwapWindow(sdl_window));
        }

        // timekeeper.produce(c.SDL_GetPerformanceCounter());
    }
}

fn Renderable(VertexData: type, IndexType: type, UniformTypes: type) type {
    return struct {
        const Self = @This();

        program: c_uint,
        vao: c_uint, // vertex array object: the draw call state, roughly
        vbo: c_uint, // vertex buffer object: the vertex data itself
        ebo: c_uint, // element buffer object: the triangle indices

        uniforms: std.EnumArray(Uniforms, c_int),

        const Uniforms = std.meta.FieldEnum(UniformTypes);

        pub fn init(vertex: [:0]const u8, fragment: [:0]const u8) !Self {
            // To keep things simple, this example doesn't check for shader compilation/linking errors.
            // A more robust program would call 'GetProgram/Shaderiv' to check for errors.
            const program = try (ProgramInfo{ .vertex = vertex, .fragment = fragment }).load();

            var vao: c_uint = undefined;
            gl.GenVertexArrays(1, @ptrCast(&vao));

            gl.BindVertexArray(vao);

            var vbo: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&vbo));
            var ebo: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&ebo));

            gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, ebo);
            defer gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);

            gl.BindBuffer(gl.ARRAY_BUFFER, vbo);
            defer gl.BindBuffer(gl.ARRAY_BUFFER, 0);

            defer gl.BindVertexArray(0);

            inline for (@typeInfo(VertexData).@"struct".fields) |field| {
                const attrib: gl.uint = @intCast(gl.GetAttribLocation(program, "a_" ++ field.name));
                gl.EnableVertexAttribArray(attrib);
                gl.VertexAttribPointer(
                    attrib,
                    howManyElementsIn(field.type),
                    getVertexAttribType(field.type),
                    isVertexAttribNormalized(field.type),
                    @sizeOf(VertexData),
                    @offsetOf(VertexData, field.name),
                );
            }

            var uniforms: std.EnumArray(Uniforms, c_int) = .initUndefined();
            inline for (@typeInfo(Uniforms).@"enum".fields) |f| {
                const uniform = gl.GetUniformLocation(program, "u_" ++ f.name);
                assert(uniform != -1);
                uniforms.set(@enumFromInt(f.value), uniform);
            }

            return .{
                .program = program,
                .vao = vao,
                .vbo = vbo,
                .ebo = ebo,
                .uniforms = uniforms,
            };
        }

        pub fn deinit(self: *Self) void {
            defer gl.DeleteProgram(self.program);
            defer gl.DeleteVertexArrays(1, @ptrCast(&self.vao));
            defer gl.DeleteBuffers(1, @ptrCast(&self.vbo));
            defer gl.DeleteBuffers(1, @ptrCast(&self.ebo));
        }

        pub fn draw(
            self: Self,
            vertices: []const VertexData,
            // TODO: make triangles optional, since they could be precomputed
            triangles: []const [3]IndexType,
            uniforms: UniformTypes,
            // TODO: allow multiple textures
            texture: ?c_uint,
        ) void {
            {
                gl.BindBuffer(gl.ARRAY_BUFFER, self.vbo);
                gl.BufferData(gl.ARRAY_BUFFER, @intCast(@sizeOf(VertexData) * vertices.len), @ptrCast(vertices.ptr), gl.DYNAMIC_DRAW);
                gl.BindBuffer(gl.ARRAY_BUFFER, 0);
            }

            {
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, self.ebo);
                gl.BufferData(gl.ELEMENT_ARRAY_BUFFER, @intCast(@sizeOf([3]IndexType) * triangles.len), triangles.ptr, gl.DYNAMIC_DRAW);
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);
            }

            if (texture) |t| gl.BindTexture(gl.TEXTURE_2D, t);
            defer if (texture) |_| gl.BindTexture(gl.TEXTURE_2D, 0);

            gl.BindVertexArray(self.vao);
            defer gl.BindVertexArray(0);

            gl.UseProgram(self.program);
            defer gl.UseProgram(0);

            inline for (@typeInfo(Uniforms).@"enum".fields) |f| {
                const u = self.uniforms.get(@enumFromInt(f.value));
                const v = @field(uniforms, f.name);
                switch (@FieldType(UniformTypes, f.name)) {
                    FColor => gl.Uniform4f(u, v.r, v.g, v.b, v.a),
                    Rect => gl.Uniform4f(u, v.top_left.x, v.top_left.y, v.size.y, v.size.y),
                    Point => gl.Uniform4f(u, v.pos.x, v.pos.y, v.turns, v.scale),
                    else => |t| @compileError(std.fmt.comptimePrint("Unhandled uniform type: {any}", .{t})),
                }
            }

            gl.DrawElements(gl.TRIANGLES, @intCast(3 * triangles.len), switch (IndexType) {
                u16 => gl.UNSIGNED_SHORT,
                else => @compileError("not implemented"),
            }, 0);
        }

        fn getVertexAttribType(x: type) comptime_int {
            return switch (x) {
                f32 => gl.FLOAT,
                Vec2 => gl.FLOAT,
                Color => gl.UNSIGNED_BYTE,
                else => @compileError(std.fmt.comptimePrint("unhandled type: {any}", .{x})),
            };
        }

        fn howManyElementsIn(x: type) comptime_int {
            return switch (x) {
                f32 => 1,
                Vec2 => 2,
                Color => 4,
                else => @compileError(std.fmt.comptimePrint("unhandled type: {any}", .{x})),
            };
        }

        fn isVertexAttribNormalized(x: type) comptime_int {
            return switch (x) {
                f32 => gl.FALSE,
                Vec2 => gl.FALSE,
                Color => gl.TRUE,
                else => @compileError(std.fmt.comptimePrint("unhandled type: {any}", .{x})),
            };
        }
    };
}

const TextRenderer = struct {
    texture: c_uint,
    font_info: std.json.Parsed(FontJsonInfo),
    renderable: Renderable(
        extern struct { position: Vec2, texcoord: Vec2 },
        u16,
        struct {},
    ),

    const RectSides = struct {
        left: f32,
        bottom: f32,
        right: f32,
        top: f32,
    };
    const FontJsonInfo = struct {
        atlas: struct {
            type: enum { mtsdf },
            distanceRange: f32,
            distanceRangeMiddle: f32,
            /// pixels per em
            size: f32,
            width: usize,
            height: usize,
            yOrigin: enum { top },
        },
        metrics: struct {
            emSize: f32,
            lineHeight: f32,
            /// ??
            ascender: f32,
            /// ??
            descender: f32,
            /// ??
            underlineY: f32,
            /// ??
            underlineThickness: f32,
        },
        glyphs: []struct {
            unicode: u8,
            advance: f32,
            planeBounds: ?RectSides = null,
            atlasBounds: ?RectSides = null,
        },
        kerning: []struct {
            unicode1: u8,
            unicode2: u8,
            advance: f32,
        },
    };

    pub fn init(gpa: std.mem.Allocator, scratch: std.mem.Allocator) !TextRenderer {
        var texture: c_uint = undefined;
        {
            gl.GenTextures(1, @ptrCast(&texture));
            gl.BindTexture(gl.TEXTURE_2D, texture);
            defer gl.BindTexture(gl.TEXTURE_2D, 0);
            // gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            // gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
            gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
            gl.TexParameterf(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

            const zstbi = @import("zstbi");
            zstbi.init(scratch);
            defer zstbi.deinit();

            var image = try zstbi.Image.loadFromMemory(@embedFile("./fonts/Arial.png"), 0);
            defer image.deinit();
            const has_alpha = switch (image.num_components) {
                3 => false,
                4 => true,
                else => unreachable,
            };

            gl.TexImage2D(
                gl.TEXTURE_2D,
                0,
                if (has_alpha) gl.RGBA else gl.RGB,
                @intCast(image.width),
                @intCast(image.height),
                0,
                if (has_alpha) gl.RGBA else gl.RGB,
                gl.UNSIGNED_BYTE,
                image.data.ptr,
            );
            gl.GenerateMipmap(gl.TEXTURE_2D);
        }

        // TODO: parse the font data at comptime
        const font_info = try std.json.parseFromSlice(FontJsonInfo, gpa, @embedFile("./fonts/Arial.json"), .{});

        return .{
            .texture = texture,
            .font_info = font_info,
            .renderable = try .init(
                \\#version 300 es
                \\
                \\in vec2 a_position;
                \\in vec2 a_texcoord;
                \\
                \\out vec2 v_texcoord;
                \\
                \\// 0,0 => -1,1 (top left)
                \\// 1,0 => 1,1 (top right)
                \\vec2 clipFromCam(vec2 p) {
                \\  return (p * 2.0 - 1.0) * vec2(1, -1);
                \\}
                \\
                \\void main() {
                \\  v_texcoord = a_texcoord;
                \\  gl_Position = vec4(clipFromCam(a_position), 0, 1);
                \\}
            ,
                \\#version 300 es
                \\
                \\precision highp float;
                \\out vec4 out_color;
                \\
                \\in vec2 v_texcoord;
                \\uniform sampler2D u_texture;
                \\
                \\float median(float r, float g, float b) {
                \\  return max(min(r, g), min(max(r, g), b));
                \\}
                \\
                \\float inverseLerp(float a, float b, float t) {
                \\  return (t - a) / (b - a);
                \\}
                \\
                \\void main() {
                \\  // assume square texture
                \\  float sdf_texture_size = float(textureSize(u_texture, 0).x);
                \\  // the values in the sdf texture should be remapped to (-sdf_pxrange/2, +sdf_pxrange/2)
                // TODO: get sdf_pxrange from the font data
                \\  float sdf_pxrange = 2.0;
                \\  vec3 raw = texture(u_texture, v_texcoord).rgb;
                \\  float distance_in_texels = (median(raw.r, raw.g, raw.b) - 0.5) * sdf_pxrange;
                \\  // density of the texture on screen; assume uniform scaling.
                \\  // for some reason, the value is half of what it should.
                \\  float texels_per_pixel = 2.0 * fwidth(v_texcoord.x) * sdf_texture_size;
                \\  float distance_in_pixels = distance_in_texels / texels_per_pixel;
                \\  // over how many screen pixels do the transition
                \\  float transition_pixels = 1.0;
                \\  float alpha = clamp(inverseLerp(-transition_pixels / 2.0, transition_pixels / 2.0, distance_in_pixels), 0.0, 1.0);
                \\  // TODO: premultiply alpha?
                \\  out_color = vec4(vec3(1.0), alpha);
                \\}
            ),
        };
    }

    pub fn deinit(self: *TextRenderer) void {
        self.font_info.deinit();
        self.renderable.deinit();
        gl.DeleteTextures(1, @ptrCast(&self.texture));
    }

    // TODO: kerning
    // TODO: single draw call, maybe
    pub fn drawText(self: TextRenderer, camera: Rect, bottom_left: Vec2, text: []const u8, em: f32) void {
        var cursor: Vec2 = bottom_left;
        for (text) |char| {
            cursor = self.drawLetter(camera, cursor, char, em);
        }
    }

    // TODO: use a map
    pub fn drawLetter(self: TextRenderer, camera: Rect, bottom_left: Vec2, letter: u8, em: f32) Vec2 {
        const glyph_info = blk: {
            for (self.font_info.value.glyphs) |glyph| {
                if (glyph.unicode == letter) break :blk glyph;
            } else unreachable;
        };
        if (glyph_info.atlasBounds) |b| {
            const s = UVec2.new(
                self.font_info.value.atlas.width,
                self.font_info.value.atlas.height,
            ).tof32();
            const p = glyph_info.planeBounds orelse unreachable;

            self.renderable.draw(&.{
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.left, p.bottom).scale(em))),
                    .texcoord = Vec2.new(b.left, b.bottom).div(s),
                },
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.right, p.bottom).scale(em))),
                    .texcoord = Vec2.new(b.right, b.bottom).div(s),
                },
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.left, p.top).scale(em))),
                    .texcoord = Vec2.new(b.left, b.top).div(s),
                },
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.right, p.top).scale(em))),
                    .texcoord = Vec2.new(b.right, b.top).div(s),
                },
            }, &.{ .{ 0, 1, 2 }, .{ 3, 2, 1 } }, .{}, self.texture);
        }

        return bottom_left.addX(em * glyph_info.advance);
    }
};

// TODO: delete this method
fn render(renderables: anytype, text_renderer: TextRenderer, cmd: game.RenderQueue.Command, scratch: std.mem.Allocator) !void {
    _ = scratch;
    _ = renderables;
    switch (cmd) {
        .clear => unreachable,
        .shape => unreachable,
        .precomputed_shape => unreachable,
        .text => |text| {
            text_renderer.drawText(text.camera, text.bottom_left, text.line, text.em);
        },
    }
}

// TODO: delete/improve this
pub const ProgramInfo = struct {
    // TODO: remove
    const preamble: [:0]const u8 = "";
    vertex: [:0]const u8,
    fragment: [:0]const u8,

    fn doShader(stage: enum { vertex, fragment }, source: [:0]const u8) !gl.uint {
        assert(std.mem.startsWith(u8, source, "#version 300 es"));
        const result = gl.CreateShader(switch (stage) {
            .vertex => gl.VERTEX_SHADER,
            .fragment => gl.FRAGMENT_SHADER,
        });
        gl.ShaderSource(
            result,
            2,
            &.{ preamble.ptr, source.ptr },
            &.{ @intCast(preamble.len), @intCast(source.len) },
        );
        gl.CompileShader(result);

        var success: gl.int = undefined;
        gl.GetShaderiv(result, gl.COMPILE_STATUS, &success);
        if (success == 0) {
            var info_log: [512]u8 = undefined;
            var info_len: gl.sizei = undefined;
            gl.GetShaderInfoLog(result, info_log.len, &info_len, &info_log);
            std.log.err("Failed to compile shader: {s}", .{info_log[0..@intCast(info_len)]});
            return error.ShaderCreationError;
        }

        return result;
    }

    pub fn load(self: ProgramInfo) !gl.uint {
        const vertex_shader = try doShader(.vertex, self.vertex);
        defer gl.DeleteShader(vertex_shader);

        const fragment_shader = try doShader(.fragment, self.fragment);
        defer gl.DeleteShader(fragment_shader);

        const program = gl.CreateProgram();

        gl.AttachShader(program, vertex_shader);
        gl.AttachShader(program, fragment_shader);
        gl.LinkProgram(program);

        var success: gl.int = undefined;
        gl.GetProgramiv(program, gl.LINK_STATUS, &success);
        if (success == 0) {
            var info_log: [512]u8 = undefined;
            var info_len: gl.sizei = undefined;
            gl.GetProgramInfoLog(program, info_log.len, &info_len, &info_log);
            std.log.err("Failed to link shader: {s}", .{info_log[0..@intCast(info_len)]});
            return error.ShaderCreationError;
        }

        return program;
    }
};

const Sound = struct {
    // Taken from apple.wav
    const spec: c.SDL_AudioSpec = .{
        .format = c.SDL_AUDIO_U8,
        .channels = 1,
        .freq = 44100,
    };

    data: []u8,

    pub fn init(comptime path: []const u8) !Sound {
        const wav = @embedFile(path);
        const stream: *c.SDL_IOStream = try errify(c.SDL_IOFromConstMem(wav, wav.len));
        var data_ptr: ?[*]u8 = undefined;
        var data_len: u32 = undefined;
        var sound_spec: c.SDL_AudioSpec = undefined;
        try errify(c.SDL_LoadWAV_IO(stream, true, &sound_spec, &data_ptr, &data_len));
        // TODO: what if each sound has a different sound spec?
        // TODO: look into SDL_SetAudioStreamFormat as a possible solution
        assert(std.meta.eql(sound_spec, spec));
        const data = data_ptr.?[0..data_len];
        return .{ .data = data };
    }

    pub fn deinit(self: *Sound) void {
        c.SDL_free(self.data.ptr);
    }
};

const Loop = struct {
    data: []u8,
    spec: c.SDL_AudioSpec,
    stream: *c.SDL_AudioStream,

    pub fn init(self: *Loop, comptime path: []const u8, audio_device: c.SDL_AudioDeviceID) !void {
        // TODO: errdefers
        const wav = @embedFile(path);
        const io_stream: *c.SDL_IOStream = try errify(c.SDL_IOFromConstMem(wav, wav.len));
        var data_ptr: ?[*]u8 = undefined;
        var data_len: u32 = undefined;
        var audio_spec: c.SDL_AudioSpec = undefined;
        try errify(c.SDL_LoadWAV_IO(io_stream, true, &audio_spec, &data_ptr, &data_len));
        const data = data_ptr.?[0..data_len];
        const audio_stream = try errify(c.SDL_CreateAudioStream(&audio_spec, null));
        try errify(c.SDL_SetAudioStreamGain(audio_stream, 0.0));
        try errify(c.SDL_BindAudioStream(audio_device, audio_stream));
        try errify(c.SDL_SetAudioStreamGetCallback(audio_stream, &callback, self));
        self.* = .{ .data = data, .spec = audio_spec, .stream = audio_stream };
    }

    pub fn deinit(self: *Loop) void {
        c.SDL_DestroyAudioStream(self.stream);
        c.SDL_free(self.data.ptr);
    }

    fn callback(userdata: ?*anyopaque, stream: ?*c.SDL_AudioStream, additional_amount: c_int, total_amount: c_int) callconv(.c) void {
        _ = total_amount;
        if (additional_amount == 0) return;
        const self: *Loop = @ptrCast(@alignCast(userdata));
        const data = self.data;
        errify(c.SDL_PutAudioStreamData(stream.?, data.ptr, @intCast(data.len))) catch unreachable;
    }
};

const sdl_scancode_to_keyboard_button = [_]std.meta.Tuple(&.{ c.SDL_Scancode, KeyboardButton }){
    .{ c.SDL_SCANCODE_A, .KeyA },
    .{ c.SDL_SCANCODE_B, .KeyB },
    .{ c.SDL_SCANCODE_C, .KeyC },
    .{ c.SDL_SCANCODE_D, .KeyD },
    .{ c.SDL_SCANCODE_E, .KeyE },
    .{ c.SDL_SCANCODE_F, .KeyF },
    .{ c.SDL_SCANCODE_G, .KeyG },
    .{ c.SDL_SCANCODE_H, .KeyH },
    .{ c.SDL_SCANCODE_I, .KeyI },
    .{ c.SDL_SCANCODE_J, .KeyJ },
    .{ c.SDL_SCANCODE_K, .KeyK },
    .{ c.SDL_SCANCODE_L, .KeyL },
    .{ c.SDL_SCANCODE_M, .KeyM },
    .{ c.SDL_SCANCODE_N, .KeyN },
    .{ c.SDL_SCANCODE_O, .KeyO },
    .{ c.SDL_SCANCODE_P, .KeyP },
    .{ c.SDL_SCANCODE_Q, .KeyQ },
    .{ c.SDL_SCANCODE_R, .KeyR },
    .{ c.SDL_SCANCODE_S, .KeyS },
    .{ c.SDL_SCANCODE_T, .KeyT },
    .{ c.SDL_SCANCODE_U, .KeyU },
    .{ c.SDL_SCANCODE_V, .KeyV },
    .{ c.SDL_SCANCODE_W, .KeyW },
    .{ c.SDL_SCANCODE_X, .KeyX },
    .{ c.SDL_SCANCODE_Y, .KeyY },
    .{ c.SDL_SCANCODE_Z, .KeyZ },
    .{ c.SDL_SCANCODE_1, .Digit1 },
    .{ c.SDL_SCANCODE_2, .Digit2 },
    .{ c.SDL_SCANCODE_3, .Digit3 },
    .{ c.SDL_SCANCODE_4, .Digit4 },
    .{ c.SDL_SCANCODE_5, .Digit5 },
    .{ c.SDL_SCANCODE_6, .Digit6 },
    .{ c.SDL_SCANCODE_7, .Digit7 },
    .{ c.SDL_SCANCODE_8, .Digit8 },
    .{ c.SDL_SCANCODE_9, .Digit9 },
    .{ c.SDL_SCANCODE_0, .Digit0 },
    .{ c.SDL_SCANCODE_RETURN, .Enter },
    .{ c.SDL_SCANCODE_ESCAPE, .Escape },
    .{ c.SDL_SCANCODE_BACKSPACE, .Backspace },
    .{ c.SDL_SCANCODE_TAB, .Tab },
    .{ c.SDL_SCANCODE_SPACE, .Space },
    .{ c.SDL_SCANCODE_MINUS, .Minus },
    .{ c.SDL_SCANCODE_EQUALS, .Equal },
    .{ c.SDL_SCANCODE_LEFTBRACKET, .BracketLeft },
    .{ c.SDL_SCANCODE_RIGHTBRACKET, .BracketRight },
    .{ c.SDL_SCANCODE_BACKSLASH, .Backslash },
    .{ c.SDL_SCANCODE_NONUSHASH, .IntlBackslash },
    .{ c.SDL_SCANCODE_SEMICOLON, .Semicolon },
    .{ c.SDL_SCANCODE_APOSTROPHE, .Quote },
    .{ c.SDL_SCANCODE_GRAVE, .Backquote },
    .{ c.SDL_SCANCODE_COMMA, .Comma },
    .{ c.SDL_SCANCODE_PERIOD, .Period },
    .{ c.SDL_SCANCODE_SLASH, .Slash },
    .{ c.SDL_SCANCODE_CAPSLOCK, .CapsLock },
    .{ c.SDL_SCANCODE_F1, .F1 },
    .{ c.SDL_SCANCODE_F2, .F2 },
    .{ c.SDL_SCANCODE_F3, .F3 },
    .{ c.SDL_SCANCODE_F4, .F4 },
    .{ c.SDL_SCANCODE_F5, .F5 },
    .{ c.SDL_SCANCODE_F6, .F6 },
    .{ c.SDL_SCANCODE_F7, .F7 },
    .{ c.SDL_SCANCODE_F8, .F8 },
    .{ c.SDL_SCANCODE_F9, .F9 },
    .{ c.SDL_SCANCODE_F10, .F10 },
    .{ c.SDL_SCANCODE_F11, .F11 },
    .{ c.SDL_SCANCODE_F12, .F12 },
};

const std = @import("std");
const assert = std.debug.assert;

const gl = @import("gl");

const kommon = @import("kommon");
const math = kommon.math;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const Color = math.Color;
const FColor = math.FColor;
const Camera = math.Camera;
const Point = math.Point;
const Rect = math.Rect;
const errify = kommon.sdl.errify;
const Timekeeper = kommon.Timekeeper;
const Mouse = game.Mouse;
const Keyboard = game.Keyboard;
const KeyboardButton = game.KeyboardButton;

pub const std_options: std.Options = .{
    // TODO: remove before final release
    .log_level = .debug,
};
