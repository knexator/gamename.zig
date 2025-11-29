const c = @cImport({
    @cDefine("SDL_DISABLE_OLD_NAMES", {});
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_revision.h");
    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_MAIN_HANDLED' should be defined before including 'SDL_main.h'.
    @cDefine("SDL_MAIN_HANDLED", {});
    @cInclude("SDL3/SDL_main.h");
});

const tracy = GameState.tracy;

const GameState = @import("GameState");
const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
const stuff = GameState.stuff;
const Gl = kommon.Gl;

comptime {
    std.testing.refAllDecls(GameState);
}

const hot_reloading = @import("build_options").game_dynlib_path != null;
var my_game: if (@import("build_options").game_dynlib_path) |game_dynlib_path| struct {
    const Self = @This();
    const CApi = kommon.engine.CApiFor(GameState);

    api: *const CApi = &GameState.game_api,
    dyn_lib: ?std.DynLib = null,
    last_inode: std.fs.File.INode = 0,
    state: GameState,

    fn preload(dst: *Self, sdl_gl: Gl) !void {
        if (@hasDecl(GameState, "preload")) {
            try dst.state.preload(sdl_gl);
        }
    }

    fn init(dst: *Self, gpa: std.mem.Allocator, sdl_gl: Gl, loaded_images: std.EnumArray(GameState.Images, *const anyopaque), random_seed: u64) !void {
        try dst.state.init(gpa, sdl_gl, loaded_images, random_seed);
    }

    fn deinit(self: *Self, gpa: std.mem.Allocator) void {
        self.state.deinit(gpa);
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

        if (self.dyn_lib) |*dyn_lib| {
            self.api.beforeHotReload(&self.state);
            dyn_lib.close();
        }
        // const path = "./zig-out/lib/libgame.so";
        const path = if (@import("builtin").os.tag == .windows) blk: {
            try std.fs.copyFileAbsolute(game_dynlib_path, game_dynlib_path ++ ".tmp", .{});
            break :blk game_dynlib_path ++ ".tmp";
        } else game_dynlib_path;
        self.dyn_lib = try .open(path);
        self.api = self.dyn_lib.?.lookup(*const CApi, "game_api") orelse return error.LookupFail;
        self.api.afterHotReload(&self.state);
        std.log.debug("reloaded game code", .{});
    }
} else GameState = undefined;

var window_size: UVec2 = Vec2.new(stuff.metadata.desired_aspect_ratio, 1).scale(512).toInt(usize);
fn getWindowRect() Rect {
    return .{
        .top_left = .zero,
        .size = window_size.tof32(),
    };
}

/// positions are in [0..1]x[0..1]
var mouse = Mouse{ .cur = .init, .prev = .init, .cur_time = 0 };
fn setButtonChanged(button: kommon.input.MouseButton) void {
    mouse.setChanged(button);
}
var keyboard = Keyboard{ .cur = .init, .prev = .init, .cur_time = 0 };
fn setKeyChanged(key: KeyboardButton) void {
    keyboard.setChanged(key);
}

const Sounds = std.meta.FieldEnum(@TypeOf(stuff.sounds));
var sound_data: std.EnumArray(Sounds, Sound) = .initUndefined();
var sound_queue: std.EnumSet(Sounds) = .initEmpty();

const Loops = std.meta.FieldEnum(@TypeOf(stuff.loops));
var loops: std.EnumArray(Loops, Loop) = .initUndefined();
var loop_volumes: std.EnumArray(Loops, f32) = .initFill(0);

// TODO: allow loading images during the game
const PreloadedImages = std.meta.FieldEnum(@TypeOf(stuff.preloaded_images));
var preloaded_images: std.EnumArray(PreloadedImages, zstbi.Image) = .initUndefined();
var images_pointers: std.EnumArray(GameState.Images, *const anyopaque) = .initUndefined();

var other_images: std.SegmentedList(zstbi.Image, 16) = .{};

var gl_procs: gl.ProcTable = undefined;

// TODO: remove this
var global_gpa_BAD: std.mem.Allocator = undefined;

const assets = @import("assets");
const fonts = @import("fonts");
fn embedAsset(comptime path: []const u8) []const u8 {
    if (comptime std.mem.startsWith(u8, path, "fonts")) {
        return fonts.get.file(path["fonts".len..]);
    } else {
        return assets.get.file(path["assets".len..]);
    }
}

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
pub fn main() !void {
    tracy.setThreadName("Main");
    defer tracy.message("Graceful main thread exit");

    // TODO: remove this and always use gpa
    // this allocator is allowed to leak memory, as a quick hack
    const leaking_allocator = if (@import("builtin").os.tag == .wasi) std.heap.wasm_allocator else std.heap.smp_allocator;
    const gpa, const is_debug = gpa: {
        if (@import("builtin").os.tag == .wasi) break :gpa .{ std.heap.wasm_allocator, false };
        break :gpa switch (@import("builtin").mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
        };
    };
    defer if (is_debug) {
        // TODO: uncomment
        // assert(debug_allocator.deinit() == .ok);
    };
    global_gpa_BAD = leaking_allocator;

    errdefer |err| if (err == error.SdlError) std.log.err("SDL error: {s}", .{c.SDL_GetError()});

    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_SetMainReady' should be called before calling 'SDL_Init'.
    c.SDL_SetMainReady();

    try errify(c.SDL_SetAppMetadata(
        stuff.metadata.name,
        "0.0.0",
        std.fmt.comptimePrint("com.{s}.{s}", .{ stuff.metadata.author, stuff.metadata.name }),
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

    if (@import("builtin").os.tag != .linux) {
        try errify(c.SDL_GL_SetAttribute(c.SDL_GL_MULTISAMPLEBUFFERS, 1));
        try errify(c.SDL_GL_SetAttribute(c.SDL_GL_MULTISAMPLESAMPLES, 16));
    } else {
        std.log.err("TODO: don't skip multisampling on Linux", .{});
    }

    try errify(c.SDL_GL_SetAttribute(c.SDL_GL_STENCIL_SIZE, 8));

    const sdl_window: *c.SDL_Window = try errify(c.SDL_CreateWindow(
        stuff.metadata.name,
        @intCast(window_size.x),
        @intCast(window_size.y),
        c.SDL_WINDOW_OPENGL | c.SDL_WINDOW_RESIZABLE | (if (hot_reloading) c.SDL_WINDOW_ALWAYS_ON_TOP else 0),
    ));
    defer c.SDL_DestroyWindow(sdl_window);

    try errify(c.SDL_SetWindowAspectRatio(
        sdl_window,
        stuff.metadata.desired_aspect_ratio,
        stuff.metadata.desired_aspect_ratio,
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
        const path = @field(stuff.sounds, @tagName(sound));
        sound_data.set(sound, try .init(path));
    }

    const audio_device = try errify(c.SDL_OpenAudioDevice(
        c.SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
        &Sound.spec,
    ));
    defer c.SDL_CloseAudioDevice(audio_device);

    // TODO: defer unloading
    inline for (comptime std.enums.values(Loops)) |loop| {
        const path = @field(stuff.loops, @tagName(loop));
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

    zstbi.init(leaking_allocator);
    // TODO: uncomment next line
    // defer zstbi.deinit();
    // TODO: defer unloading images
    inline for (comptime std.enums.values(PreloadedImages)) |image| {
        const path = @field(stuff.preloaded_images, @tagName(image));
        preloaded_images.set(image, try zstbi.Image.loadFromMemory(embedAsset(path), 0));
        images_pointers.set(image, preloaded_images.getPtrConst(image));
    }

    const sdl_gl = struct {
        pub const vtable: Gl = .{
            .clear = clear,
            .buildRenderable = buildRenderable,
            .setRenderableData = setRenderableData,
            .useRenderable = useRenderable,
            .useRenderableWithExistingData = useRenderableWithExistingData,
            .buildTexture2D = buildTexture2D,
            .buildInstancedRenderable = buildInstancedRenderable,
            .useInstancedRenderable = useInstancedRenderable,
            .loadTextureDataFromBase64 = loadTextureDataFromBase64,
            .loadTextureDataFromFilename = loadTextureDataFromFilename,
            .startStencil = startStencil,
            .whiteStencil = whiteStencil,
            .blackStencil = blackStencil,
            .doneStencil = doneStencil,
            .stopStencil = stopStencil,
        };

        fn startStencil() void {
            gl.ClearStencil(0);
            gl.Clear(gl.STENCIL_BUFFER_BIT);
            gl.Enable(gl.STENCIL_TEST);
            gl.ColorMask(gl.FALSE, gl.FALSE, gl.FALSE, gl.FALSE);
            whiteStencil();
        }

        fn whiteStencil() void {
            gl.StencilFunc(gl.ALWAYS, 1, 0xFF);
            gl.StencilOp(gl.KEEP, gl.KEEP, gl.REPLACE);
        }

        fn blackStencil() void {
            gl.StencilFunc(gl.ALWAYS, 0, 0xFF);
            gl.StencilOp(gl.KEEP, gl.KEEP, gl.REPLACE);
        }

        fn doneStencil() void {
            gl.ColorMask(gl.TRUE, gl.TRUE, gl.TRUE, gl.TRUE);
            gl.StencilFunc(gl.EQUAL, 1, 0xFF);
            gl.StencilOp(gl.KEEP, gl.KEEP, gl.KEEP);
        }

        fn stopStencil() void {
            gl.Disable(gl.STENCIL_TEST);
        }

        pub fn clear(color: FColor) void {
            gl.ClearBufferfv(gl.COLOR, 0, &color.toArray());
        }

        pub fn loadTextureDataFromBase64(base64: []const u8) *const anyopaque {
            var decoder = std.base64.standard.Decoder;
            const size = decoder.calcSizeForSlice(base64) catch |err| switch (err) {
                else => @panic("TODO"),
            };
            const data = global_gpa_BAD.alloc(u8, size) catch @panic("TODO");
            decoder.decode(data, base64) catch @panic("TODO");
            const image = zstbi.Image.loadFromMemory(data, 4) catch @panic("TODO");
            other_images.append(global_gpa_BAD, image) catch @panic("TODO");
            return other_images.at(other_images.len - 1);
        }

        pub fn loadTextureDataFromFilename(path: [:0]const u8) *const anyopaque {
            const image = zstbi.Image.loadFromFile(path, 0) catch @panic(path);
            other_images.append(global_gpa_BAD, image) catch @panic("TODO");
            return other_images.at(other_images.len - 1);
        }

        pub fn buildTexture2D(data: *const anyopaque, pixelart: bool) Gl.Texture {
            const image: *const zstbi.Image = @alignCast(@ptrCast(data));

            const has_alpha = switch (image.num_components) {
                3 => false,
                4 => true,
                else => unreachable,
            };

            var texture: c_uint = undefined;
            gl.GenTextures(1, @ptrCast(&texture));
            gl.BindTexture(gl.TEXTURE_2D, texture);
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
            if (pixelart) {
                gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
                gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
            } else {
                gl.GenerateMipmap(gl.TEXTURE_2D);
                gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
                // TODO: let user choose quality
                gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);
            }

            return .{ .id = texture, .resolution = .new(image.width, image.height) };
        }

        pub fn buildRenderable(
            vertex_src: [:0]const u8,
            fragment_src: [:0]const u8,
            attributes: Gl.VertexInfo.Collection,
            uniforms: []const Gl.UniformInfo.In,
        ) !Gl.Renderable {
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

            for (attributes.attribs, 0..) |attribute, k| {
                const maybe_index = gl.GetAttribLocation(program, attribute.name);
                if (maybe_index == -1) {
                    std.log.err("Attribute not found: {s}", .{attribute.name});
                    return error.AttributeLocationError;
                }
                const index: gl.uint = @intCast(maybe_index);
                gl.EnableVertexAttribArray(index);
                gl.VertexAttribPointer(
                    index,
                    @intFromEnum(attribute.kind.count()),
                    @intFromEnum(attribute.kind.type()),
                    if (attribute.kind.normalized()) gl.TRUE else gl.FALSE,
                    // TODO: check in debugger if this is computed once
                    @intCast(attributes.getStride()),
                    attributes.getOffset(k),
                );
            }

            var uniforms_data = std.BoundedArray(Gl.UniformInfo, 8).init(0) catch unreachable;
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

        pub fn setRenderableData(
            renderable: Gl.Renderable,
            vertices_ptr: *const anyopaque,
            vertices_len_bytes: usize,
            triangles: []const [3]Gl.IndexType,
            mode: Gl.UsageMode,
        ) void {
            gl.BindBuffer(gl.ARRAY_BUFFER, @intFromEnum(renderable.vbo));
            gl.BufferData(
                gl.ARRAY_BUFFER,
                @intCast(vertices_len_bytes),
                @ptrCast(vertices_ptr),
                switch (mode) {
                    .static => gl.STATIC_DRAW,
                    .dynamic => gl.DYNAMIC_DRAW,
                },
            );
            gl.BindBuffer(gl.ARRAY_BUFFER, 0);

            gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, @intFromEnum(renderable.ebo));
            gl.BufferData(
                gl.ELEMENT_ARRAY_BUFFER,
                @intCast(@sizeOf([3]Gl.IndexType) * triangles.len),
                triangles.ptr,
                switch (mode) {
                    .static => gl.STATIC_DRAW,
                    .dynamic => gl.DYNAMIC_DRAW,
                },
            );
            gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);
        }

        pub fn useRenderable(
            renderable: Gl.Renderable,
            vertices_ptr: *const anyopaque,
            vertices_len_bytes: usize,
            // vertices: []const anyopaque,
            // TODO: make triangles optional, since they could be precomputed
            triangles: []const [3]Gl.IndexType,
            uniforms: []const Gl.UniformInfo.Runtime,
            // TODO: multiple textures
            texture: ?Gl.Texture,
        ) void {
            setRenderableData(renderable, vertices_ptr, vertices_len_bytes, triangles, .dynamic);
            useRenderableWithExistingData(renderable, triangles.len, uniforms, texture);
        }

        pub fn useRenderableWithExistingData(
            renderable: Gl.Renderable,
            n_triangles: usize,
            uniforms: []const Gl.UniformInfo.Runtime,
            // TODO: multiple textures
            texture: ?Gl.Texture,
        ) void {
            if (texture) |t| gl.BindTexture(gl.TEXTURE_2D, t.id);
            defer if (texture) |_| gl.BindTexture(gl.TEXTURE_2D, 0);

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
                    .Rect => |v| gl.Uniform4f(u, v.top_left.x, v.top_left.y, v.size.x, v.size.y),
                    .Point => |v| gl.Uniform4f(u, v.pos.x, v.pos.y, v.turns, v.scale),
                    .f32 => |v| gl.Uniform1f(u, v),
                }
            }

            gl.DrawElements(gl.TRIANGLES, @intCast(3 * n_triangles), switch (Gl.IndexType) {
                u16 => gl.UNSIGNED_SHORT,
                u32 => gl.UNSIGNED_INT,
                else => @compileError("not implemented"),
            }, 0);
        }

        pub fn buildInstancedRenderable(
            vertex_src: [:0]const u8,
            fragment_src: [:0]const u8,
            per_vertex_attributes: Gl.VertexInfo.Collection,
            per_instance_attributes: Gl.VertexInfo.Collection,
            uniforms: []const Gl.UniformInfo.In,
        ) !Gl.InstancedRenderable {
            const program = try (ProgramInfo{
                .vertex = vertex_src,
                .fragment = fragment_src,
            }).load();

            var vao: c_uint = undefined;
            gl.GenVertexArrays(1, @ptrCast(&vao));

            gl.BindVertexArray(vao);

            // TODO: single GenBuffers call
            var vbo_vertices: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&vbo_vertices));
            var vbo_instances: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&vbo_instances));
            var ebo: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&ebo));

            gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, ebo);
            // defer gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);

            defer gl.BindVertexArray(0);

            // https://webgl2fundamentals.org/webgl/lessons/webgl-instanced-drawing.html

            {
                gl.BindBuffer(gl.ARRAY_BUFFER, vbo_vertices);
                // defer gl.BindBuffer(gl.ARRAY_BUFFER, 0);

                const attributes = per_vertex_attributes;
                for (attributes.attribs, 0..) |attribute, k| {
                    const index: gl.uint = @intCast(gl.GetAttribLocation(program, attribute.name));
                    if (index == -1) return error.AttributeLocationError;
                    gl.EnableVertexAttribArray(index);
                    gl.VertexAttribPointer(
                        index,
                        @intFromEnum(attribute.kind.count()),
                        @intFromEnum(attribute.kind.type()),
                        if (attribute.kind.normalized()) gl.TRUE else gl.FALSE,
                        // TODO: check in debugger if this is computed once
                        @intCast(attributes.getStride()),
                        attributes.getOffset(k),
                    );
                }
            }

            {
                gl.BindBuffer(gl.ARRAY_BUFFER, vbo_instances);
                // defer gl.BindBuffer(gl.ARRAY_BUFFER, 0);

                const attributes = per_instance_attributes;
                for (attributes.attribs, 0..) |attribute, k| {
                    const index: gl.uint = @intCast(gl.GetAttribLocation(program, attribute.name));
                    if (index == -1) return error.AttributeLocationError;
                    gl.EnableVertexAttribArray(index);
                    gl.VertexAttribPointer(
                        index,
                        @intFromEnum(attribute.kind.count()),
                        @intFromEnum(attribute.kind.type()),
                        if (attribute.kind.normalized()) gl.TRUE else gl.FALSE,
                        // TODO: check in debugger if this is computed once
                        @intCast(attributes.getStride()),
                        attributes.getOffset(k),
                    );
                    gl.VertexAttribDivisor(index, 1);
                }
            }

            var uniforms_data = std.BoundedArray(Gl.UniformInfo, 8).init(0) catch unreachable;
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
                .vbo_vertices = @enumFromInt(vbo_vertices),
                .vbo_instances = @enumFromInt(vbo_instances),
                .ebo = @enumFromInt(ebo),
                .uniforms = uniforms_data,
            };
        }

        pub fn useInstancedRenderable(
            renderable: Gl.InstancedRenderable,
            // TODO: make the vertex data optional, since it could be precomputed
            vertex_data_ptr: *const anyopaque,
            vertex_data_len_bytes: usize,
            // TODO: make triangles optional, since they could be precomputed
            triangles: []const [3]Gl.IndexType,
            instance_data_ptr: *const anyopaque,
            instance_data_len_bytes: usize,
            instance_count: usize,
            uniforms: []const Gl.UniformInfo.Runtime,
            // TODO: multiple textures
            texture: ?Gl.Texture,
        ) void {
            {
                gl.BindBuffer(gl.ARRAY_BUFFER, @intFromEnum(renderable.vbo_vertices));
                gl.BufferData(
                    gl.ARRAY_BUFFER,
                    @intCast(vertex_data_len_bytes),
                    @ptrCast(vertex_data_ptr),
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ARRAY_BUFFER, 0);
            }

            {
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, @intFromEnum(renderable.ebo));
                gl.BufferData(
                    gl.ELEMENT_ARRAY_BUFFER,
                    @intCast(@sizeOf([3]Gl.IndexType) * triangles.len),
                    triangles.ptr,
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);
            }

            {
                gl.BindBuffer(gl.ARRAY_BUFFER, @intFromEnum(renderable.vbo_instances));
                gl.BufferData(
                    gl.ARRAY_BUFFER,
                    @intCast(instance_data_len_bytes),
                    @ptrCast(instance_data_ptr),
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ARRAY_BUFFER, 0);
            }

            // TODO
            assert(texture == null);
            // if (texture) |t| gl.BindTexture(gl.TEXTURE_2D, t.id);
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
                    .Rect => |v| gl.Uniform4f(u, v.top_left.x, v.top_left.y, v.size.x, v.size.y),
                    .Point => |v| gl.Uniform4f(u, v.pos.x, v.pos.y, v.turns, v.scale),
                    .f32 => |v| gl.Uniform1f(u, v),
                }
            }

            gl.DrawElementsInstanced(gl.TRIANGLES, @intCast(3 * triangles.len), switch (Gl.IndexType) {
                u16 => gl.UNSIGNED_SHORT,
                u32 => gl.UNSIGNED_INT,
                else => @compileError("not implemented"),
            }, null, @intCast(instance_count));
        }
    };

    var sdl_platform: PlatformGives = .{
        .gpa = gpa,
        .mouse = mouse,
        .keyboard = keyboard,
        .setKeyChanged = setKeyChanged,
        .setButtonChanged = setButtonChanged,
        .aspect_ratio = window_size.aspectRatio(),
        .delta_seconds = 0,
        .global_seconds = 0,
        .sound_queue = &sound_queue,
        .loop_volumes = &loop_volumes,
        // TODO
        .queuedSeconds = struct {
            pub fn anon() f32 {
                std.debug.panic("TODO", .{});
                // return 1.0;
            }
        }.anon,
        // TODO
        .sample_rate = 48000,
        // TODO
        .enqueueSamples = struct {
            pub fn anon(src: []const f32) void {
                _ = src;
            }
        }.anon,
        .gl = sdl_gl.vtable,
        // TODO
        .downloadAsFile = undefined,
        .askUserForFile = undefined,
        .userUploadedFile = struct {
            pub fn anon() ?std.io.AnyReader {
                return null;
            }
        }.anon,
        .forgetUserUploadedFile = undefined,
        // TODO
        .setCursor = struct {
            pub fn anon(cursor: Mouse.Cursor) void {
                _ = cursor;
            }
        }.anon,
    };

    if (@hasField(@TypeOf(my_game), "preload")) {
        try my_game.preload(sdl_platform.gl);
    }

    var seed: u64 = undefined;
    try std.posix.getrandom(std.mem.asBytes(&seed));
    try my_game.init(sdl_platform.gpa, sdl_platform.gl, images_pointers, seed);
    // TODO: gl on deinit
    defer my_game.deinit(sdl_platform.gpa);

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
                        inline for (sdl_button_to_mouse_button) |pair| {
                            const sdl_button = pair[0];
                            const button = pair[1];
                            if (event.button.button == sdl_button) {
                                @field(mouse.cur.buttons, @tagName(button)) = is_pressed;
                                @field(mouse.last_change_at, @tagName(button)) = sdl_platform.global_seconds;
                                break;
                            }
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
                            // my_game.reset(gpa, sdl_platform.gl);
                        } else {
                            if (event.key.repeat) continue;
                            const is_pressed = event.type == c.SDL_EVENT_KEY_DOWN;
                            inline for (sdl_scancode_to_keyboard_button) |pair| {
                                const sdl_scancode = pair[0];
                                const key = pair[1];
                                if (event.key.scancode == sdl_scancode) {
                                    @field(keyboard.cur.keys, @tagName(key)) = is_pressed;
                                    @field(keyboard.last_change_at, @tagName(key)) = sdl_platform.global_seconds;
                                    break;
                                }
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        // Update & Draw
        {
            keyboard.cur_time = sdl_platform.global_seconds;
            mouse.cur_time = sdl_platform.global_seconds;
            const ns_since_last_frame = timer.lap();
            sdl_platform.delta_seconds = math.tof32(ns_since_last_frame) / std.time.ns_per_s;
            sdl_platform.global_seconds += sdl_platform.delta_seconds;
            sdl_platform.aspect_ratio = window_size.aspectRatio();
            sdl_platform.mouse = mouse;
            sdl_platform.keyboard = keyboard;
            sdl_platform.sound_queue.* = .initEmpty();
            if (try my_game.update(sdl_platform)) break :main_loop;
            mouse.prev = mouse.cur;
            mouse.cur.scrolled = .none;
            keyboard.prev = keyboard.cur;
        }

        // Sound
        if (@typeInfo(Sounds).@"enum".fields.len > 0) {
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
        if (@typeInfo(Loops).@"enum".fields.len > 0) {
            var it = loop_volumes.iterator();
            while (it.next()) |entry| {
                try errify(c.SDL_SetAudioStreamGain(loops.get(entry.key).stream, entry.value.*));
            }
        }

        try errify(c.SDL_GL_SwapWindow(sdl_window));
        tracy.frameMark();
    }
}

// TODO: delete/improve this
pub const ProgramInfo = struct {
    const preamble: [:0]const u8 =
        \\#version 330 core
        \\
    ;
    vertex: [:0]const u8,
    fragment: [:0]const u8,

    fn doShader(stage: enum { vertex, fragment }, source: [:0]const u8) !gl.uint {
        assert(!std.mem.startsWith(u8, source, "#version"));
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

    // Taken from chesstory
    // const spec: c.SDL_AudioSpec = .{
    //     .format = c.SDL_AUDIO_S16LE,
    //     .channels = 2,
    //     .freq = 44100,
    // };

    data: []u8,

    pub fn init(comptime path: []const u8) !Sound {
        const wav = embedAsset(path);
        const stream: *c.SDL_IOStream = try errify(c.SDL_IOFromConstMem(wav.ptr, wav.len));
        var data_ptr: ?[*]u8 = undefined;
        var data_len: u32 = undefined;
        var sound_spec: c.SDL_AudioSpec = undefined;
        try errify(c.SDL_LoadWAV_IO(stream, true, &sound_spec, &data_ptr, &data_len));
        // TODO: what if each sound has a different sound spec?
        // TODO: look into SDL_SetAudioStreamFormat as a possible solution
        if (!std.meta.eql(sound_spec, spec)) {
            std.log.debug("{any}", .{sound_spec});
        }
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
        const wav = embedAsset(path);
        const io_stream: *c.SDL_IOStream = try errify(c.SDL_IOFromConstMem(wav.ptr, wav.len));
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

const sdl_button_to_mouse_button = [_]std.meta.Tuple(&.{ c_int, kommon.input.MouseButton }){
    .{ c.SDL_BUTTON_LEFT, .left },
    .{ c.SDL_BUTTON_RIGHT, .right },
    .{ c.SDL_BUTTON_MIDDLE, .middle },
};

const sdl_scancode_to_keyboard_button = [_]std.meta.Tuple(&.{ c.SDL_Scancode, KeyboardButton }){
    .{ c.SDL_SCANCODE_LEFT, .ArrowLeft },
    .{ c.SDL_SCANCODE_RIGHT, .ArrowRight },
    .{ c.SDL_SCANCODE_UP, .ArrowUp },
    .{ c.SDL_SCANCODE_DOWN, .ArrowDown },
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
    .{ c.SDL_SCANCODE_LSHIFT, .ShiftLeft },
    .{ c.SDL_SCANCODE_RSHIFT, .ShiftRight },
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
const Mouse = kommon.input.Mouse;
const Keyboard = kommon.input.Keyboard;
const KeyboardButton = kommon.input.KeyboardButton;
const zstbi = @import("zstbi");

pub const std_options: std.Options = .{
    // TODO: remove before final release
    .log_level = .debug,
};
