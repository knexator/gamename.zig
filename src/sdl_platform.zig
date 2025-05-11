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
    std.testing.refAllDeclsRecursive(Triangulator);
    std.testing.refAllDeclsRecursive(Vec2);
}

const hot_reloading = @import("build_options").game_dynlib_path != null;
var my_game: if (@import("build_options").game_dynlib_path) |game_dynlib_path| struct {
    const Self = @This();

    api: *const game.CApi = &game.game_api,
    dyn_lib: ?std.DynLib = null,
    last_inode: std.fs.File.INode = 0,
    state: game.GameState,

    fn init(gpa: std.mem.Allocator) !Self {
        return .{ .state = try .init(gpa) };
    }

    fn deinit(self: *Self, gpa: std.mem.Allocator) void {
        self.state.deinit(gpa);
    }

    fn reload(self: *Self, gpa: std.mem.Allocator) void {
        self.api.reload(&self.state, &gpa);
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

var window_size: UVec2 = Vec2.new(game.metadata.desired_aspect_ratio, 1).scale(600).toInt(usize);
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
        assert(debug_allocator.deinit() == .ok);
    };

    var render_queue: game.RenderQueue = .init(gpa);
    defer render_queue.deinit();

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
    };

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

    // TODO: cleanup
    var asdf_renderer: ArbitraryShapeRenderer = try .init();
    defer asdf_renderer.deinit();

    my_game = try .init(gpa);
    defer my_game.deinit(gpa);

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
                            my_game.reload(gpa);
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
            while (it.next()) |cmd| try render(asdf_renderer, cmd.*, gpa);

            try errify(c.SDL_GL_SwapWindow(sdl_window));
        }

        // timekeeper.produce(c.SDL_GetPerformanceCounter());
    }
}

const ArbitraryShapeRenderer = struct {
    const program_info: ProgramInfo = .{
        .vertex =
        \\uniform vec4 u_rect; // as top_left, size
        \\uniform vec4 u_point; // as pos, turns, scale
        \\
        \\in vec2 a_position;
        \\
        \\void main() {
        \\  float c = cos(u_point.z * 6.283185307179586);
        \\  float s = sin(u_point.z * 6.283185307179586);
        \\  vec2 world_position = u_point.xy + u_point.w * (mat2x2(c,s,-s,c) * a_position);
        \\  vec2 camera_position = (world_position - u_rect.xy) / u_rect.zw;
        \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
        \\}
        ,
        .fragment =
        \\precision highp float;
        \\out vec4 out_color;
        \\
        \\uniform vec4 u_color;
        \\void main() {
        \\  out_color = u_color;
        \\}
        ,
    };

    program: c_uint,
    vao: c_uint, // vertex array object: the draw call state, roughly
    vbo: c_uint, // vertex buffer object: the vertex data itself
    ebo: c_uint, // element buffer object: the triangle indices

    color_uniform: c_int,
    rect_uniform: c_int,
    point_uniform: c_int,

    // TODO: cleaning
    const VertexData = extern struct { position: Vec2 };
    const IndexType = game.RenderQueue.PrecomputedShape.IndexType;

    pub fn init() !ArbitraryShapeRenderer {
        // To keep things simple, this example doesn't check for shader compilation/linking errors.
        // A more robust program would call 'GetProgram/Shaderiv' to check for errors.
        const program = try program_info.load();

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

        const color_uniform = gl.GetUniformLocation(program, "u_color");
        assert(color_uniform != -1);
        const rect_uniform = gl.GetUniformLocation(program, "u_rect");
        assert(rect_uniform != -1);
        const point_uniform = gl.GetUniformLocation(program, "u_point");
        assert(point_uniform != -1);

        return .{
            .program = program,
            .vao = vao,
            .vbo = vbo,
            .ebo = ebo,
            .color_uniform = color_uniform,
            .rect_uniform = rect_uniform,
            .point_uniform = point_uniform,
        };
    }

    pub fn deinit(self: *ArbitraryShapeRenderer) void {
        defer gl.DeleteProgram(self.program);
        defer gl.DeleteVertexArrays(1, @ptrCast(&self.vao));
        defer gl.DeleteBuffers(1, @ptrCast(&self.vbo));
        defer gl.DeleteBuffers(1, @ptrCast(&self.ebo));
    }

    pub fn drawV2(
        self: ArbitraryShapeRenderer,
        camera: Rect,
        parent: Point,
        positions: []const Vec2,
        triangles: []const [3]IndexType,
        color: Color,
    ) void {
        const cpu_buffer: []const VertexData = @ptrCast(positions);

        {
            gl.BindBuffer(gl.ARRAY_BUFFER, self.vbo);
            gl.BufferData(gl.ARRAY_BUFFER, @intCast(@sizeOf(VertexData) * cpu_buffer.len), @ptrCast(cpu_buffer.ptr), gl.DYNAMIC_DRAW);
            gl.BindBuffer(gl.ARRAY_BUFFER, 0);
        }

        {
            gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, self.ebo);
            gl.BufferData(gl.ELEMENT_ARRAY_BUFFER, @intCast(@sizeOf([3]IndexType) * triangles.len), triangles.ptr, gl.DYNAMIC_DRAW);
            gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);
        }

        gl.BindVertexArray(self.vao);
        defer gl.BindVertexArray(0);

        gl.UseProgram(self.program);
        defer gl.UseProgram(0);

        const fcol = color.toFColor();
        gl.Uniform4f(self.color_uniform, fcol.r, fcol.g, fcol.b, fcol.a);

        gl.Uniform4f(self.rect_uniform, camera.top_left.x, camera.top_left.y, camera.size.y, camera.size.y);
        gl.Uniform4f(self.point_uniform, parent.pos.x, parent.pos.y, parent.turns, parent.scale);

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

fn render(asdf_renderer: ArbitraryShapeRenderer, cmd: game.RenderQueue.Command, scratch: std.mem.Allocator) !void {
    switch (cmd) {
        .clear => |color| {
            gl.ClearBufferfv(gl.COLOR, 0, &color.toFColor().toArray());
        },
        .shape => |shape| {
            // TODO: cache triangulation
            const IndexType = u16;
            if (shape.fill) |color| {
                assert(shape.local_points.len >= 3);
                const indices = try Triangulator.triangulate(IndexType, scratch, shape.local_points);
                defer scratch.free(indices);

                asdf_renderer.drawV2(
                    shape.camera,
                    shape.parent_world_point,
                    shape.local_points,
                    indices,
                    color,
                );
            }

            if (shape.stroke) |color| {
                _ = color;
                @panic("TODO");
                // try errify(c.SDL_SetRenderDrawColor(sdl_renderer, color.r, color.g, color.b, color.a));
                // try errify(c.SDL_RenderLines(sdl_renderer, screen_positions.ptr, @intCast(screen_positions.len)));
                // try errify(c.SDL_RenderLine(
                //     sdl_renderer,
                //     kommon.last(c.SDL_FPoint, screen_positions).?.x,
                //     kommon.last(c.SDL_FPoint, screen_positions).?.y,
                //     screen_positions[0].x,
                //     screen_positions[0].y,
                // ));
            }
        },
        .precomputed_shape => |shape| {
            asdf_renderer.drawV2(
                shape.camera,
                shape.parent_world_point,
                shape.data.local_points,
                shape.data.triangles,
                shape.fill,
            );
        },
    }
}

pub const ProgramInfo = struct {
    const preamble: [:0]const u8 =
        \\#version 300 es
        \\
    ;
    vertex: [:0]const u8,
    fragment: [:0]const u8,

    fn doShader(stage: enum { vertex, fragment }, source: [:0]const u8) !gl.uint {
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
            return error.ShaderCompileError;
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
            return error.ShaderCompileError;
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
const Triangulator = kommon.Triangulator;
const math = kommon.math;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const Color = math.Color;
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
