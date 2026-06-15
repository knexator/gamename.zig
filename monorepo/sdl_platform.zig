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

    fn init(
        dst: *Self,
        runtime_params: kommon.engine.InitRuntimeParamsFor(GameState),
        comptime comptime_params: kommon.engine.InitComptimeParamsFor(GameState),
    ) !void {
        try dst.state.init(runtime_params, comptime_params);
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

var sdl_window: *c.SDL_Window = undefined;
var sdl_device: *c.SDL_GPUDevice = undefined;
var sdl_renderer: SdlRenderer = undefined;

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
var pending_text_input: kommon.CircularBuffer(
    std.BoundedArray(u8, 4),
    64,
) = .empty;
fn startTextInput(textinput_area_in_0101_coords: ?Rect) void {
    if (textinput_area_in_0101_coords) |rect| {
        // TODO(now): use the area
        _ = rect;
        // c.SDL_SetTextInputArea(sdl_window, .{ .x = ..., }, cursor: c_int ??)
    }
    errify(c.SDL_StartTextInput(sdl_window)) catch @panic("bad");
}
fn stopTextInput() void {
    errify(c.SDL_StopTextInput(sdl_window)) catch @panic("bad");
}
fn consumeTextInput() ?std.BoundedArray(u8, 4) {
    return pending_text_input.popFirst();
}

// TODO: reconsider
var getitem_lastreader: std.io.FixedBufferStream([]const u8) = undefined;
var getitem_lastreader_reader: std.io.FixedBufferStream([]const u8).Reader = undefined;

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

    var frame_arena: std.heap.ArenaAllocator = .init(gpa);
    defer frame_arena.deinit();

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

    sdl_device = try errify(c.SDL_CreateGPUDevice(
        c.SDL_GPU_SHADERFORMAT_SPIRV,
        @import("builtin").mode == .Debug,
        null,
    ));
    defer c.SDL_DestroyGPUDevice(sdl_device);

    sdl_window = try errify(c.SDL_CreateWindow(
        stuff.metadata.name,
        @intCast(window_size.x),
        @intCast(window_size.y),
        c.SDL_WINDOW_RESIZABLE | (if (true and hot_reloading) c.SDL_WINDOW_ALWAYS_ON_TOP else 0),
    ));
    defer c.SDL_DestroyWindow(sdl_window);

    try errify(c.SDL_ClaimWindowForGPUDevice(sdl_device, sdl_window));
    defer c.SDL_ReleaseWindowFromGPUDevice(sdl_device, sdl_window);

    try errify(c.SDL_SetWindowAspectRatio(
        sdl_window,
        stuff.metadata.desired_aspect_ratio,
        stuff.metadata.desired_aspect_ratio,
    ));

    sdl_renderer = try .init(sdl_device, sdl_window);
    defer sdl_renderer.deinit(sdl_device);

    for (0..SdlRenderer.texture_count) |k| {
        try sdl_renderer.setTexture(sdl_device, k + 1, .{ .w = 2, .h = 2, .pixels = &.{
            .{ 255, 255, 255, 255 },
            .{ 255, 0, 0, 255 },
            .{ 0, 255, 0, 255 },
            .{ 0, 0, 255, 255 },
        } }, &.{
            .min_filter = c.SDL_GPU_FILTER_NEAREST,
            .mag_filter = c.SDL_GPU_FILTER_NEAREST,
            .mipmap_mode = c.SDL_GPU_SAMPLERMIPMAPMODE_NEAREST,
            .address_mode_u = c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
            .address_mode_v = c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
            .address_mode_w = c.SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE,
        });
    }

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

    // TODO: error instead of panic
    const sdl_renderer_vtable: kommon.Renderer.VTable = .{
        .addModel = struct {
            fn anon(vertices: []const kommon.Renderer.Vertex, indices: []const [3]u16) kommon.Renderer.ModelInfo {
                return sdl_renderer.addModel(vertices, indices) catch @panic("TODO");
            }
        }.anon,
        .addDrawable = struct {
            fn anon(drawable: kommon.Renderer.Drawable, model_info: kommon.Renderer.ModelInfo) void {
                return sdl_renderer.addDrawable(drawable, model_info) catch @panic("TODO");
            }
        }.anon,
    };

    const recording_log_file: ?std.fs.File = if (@import("builtin").mode == .Debug)
        try std.fs.cwd().createFile("recording.txt", .{})
    else
        null;

    defer if (recording_log_file) |*f| f.close();

    var sdl_platform: PlatformGives = .{
        .gpa = gpa,
        .frame_arena = frame_arena.allocator(),
        .mouse = mouse,
        .keyboard = keyboard,
        .setKeyChanged = setKeyChanged,
        .setButtonChanged = setButtonChanged,
        .startTextInput = startTextInput,
        .stopTextInput = stopTextInput,
        .consumeTextInput = consumeTextInput,
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
        .renderer = sdl_renderer_vtable,
        .gl = undefined,
        // TODO
        .downloadAsFile = undefined,
        .downloadActiveFramebuffer = undefined,
        .askUserForFile = undefined,
        .userUploadedFile = struct {
            pub fn anon() ?std.io.AnyReader {
                return null;
            }
        }.anon,
        .forgetUserUploadedFile = undefined,
        // TODO
        .getItem = struct {
            pub fn anon(key: []const u8) ?std.io.AnyReader {
                const base_path: [*c]u8 = errify(c.SDL_GetPrefPath(
                    GameState.stuff.metadata.author,
                    GameState.stuff.metadata.name,
                )) catch std.debug.panic("couldn't get save folder, {s}", .{c.SDL_GetError()});
                defer c.SDL_free(base_path);
                const save_path = std.fmt.allocPrintZ(global_gpa_BAD, "{s}{s}.save", .{ base_path, key }) catch std.debug.panic("Out of Memory!?", .{});
                defer global_gpa_BAD.free(save_path);

                var datasize: usize = 0;
                const dataptr: [*]u8 = @ptrCast(errify(c.SDL_LoadFile(save_path, &datasize)) catch return null);
                const data: []const u8 = dataptr[0..datasize];

                getitem_lastreader = std.io.fixedBufferStream(data);
                getitem_lastreader_reader = getitem_lastreader.reader();
                return getitem_lastreader_reader.any();
            }
        }.anon,
        .setItem = struct {
            pub fn anon(key: []const u8, value: []const u8) void {
                const base_path: [*c]u8 = errify(c.SDL_GetPrefPath(
                    GameState.stuff.metadata.author,
                    GameState.stuff.metadata.name,
                )) catch std.debug.panic("couldn't get save folder, {s}", .{c.SDL_GetError()});
                defer c.SDL_free(base_path);
                const save_path = std.fmt.allocPrintZ(global_gpa_BAD, "{s}{s}.save", .{ base_path, key }) catch std.debug.panic("Out of Memory!?", .{});
                defer global_gpa_BAD.free(save_path);

                errify(c.SDL_SaveFile(save_path, value.ptr, value.len)) catch std.debug.panic("error during saving", .{});
            }
        }.anon,
        // TODO
        .setCursor = struct {
            pub fn anon(cursor: Mouse.Cursor) void {
                _ = cursor;
            }
        }.anon,
        .recording_log = if (recording_log_file) |*f| f.writer().any() else null,
    };

    if (@hasField(@TypeOf(my_game), "preload")) {
        try my_game.preload(sdl_platform.gl);
    }

    var seed: u64 = undefined;
    try std.posix.getrandom(std.mem.asBytes(&seed));
    try my_game.init(
        .{
            .gpa = sdl_platform.gpa,
            // .gl = sdl_platform.gl,
            .loaded_images = images_pointers,
            .random_seed = seed,
        },
        .{
            // TODO(platform): tweakable params for sdl backend
            .tweakable = struct {
                pub fn fcolor(_: []const u8, _: *FColor) void {}
                pub fn float(_: []const u8, _: *f32, _: f32, _: f32) void {}
                pub fn texture(_: []const u8, _: *Gl.Texture) void {}
            },
        },
    );
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
                        // TODO?
                        // gl.Viewport(0, 0, @intCast(window_size.x), @intCast(window_size.y));
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
                    c.SDL_EVENT_TEXT_INPUT => {
                        const text: []const u8 = std.mem.span(event.text.text);
                        try pending_text_input.append(try .fromSlice(text));
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
                                    @field(keyboard.manually_changed, @tagName(key)) = false;
                                    break;
                                }
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        const cmdbuf = try errify(c.SDL_AcquireGPUCommandBuffer(sdl_device));

        // Update & Draw
        {
            var maybe_swapchain_texture: ?*c.SDL_GPUTexture = undefined;
            var w: u32, var h: u32 = .{ undefined, undefined };
            try errify(c.SDL_WaitAndAcquireGPUSwapchainTexture(cmdbuf, sdl_window, &maybe_swapchain_texture, &w, &h));

            try sdl_renderer.startFrame(sdl_device);
            errdefer sdl_renderer.undoStartFrame(sdl_device);
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

            if (maybe_swapchain_texture) |swapchain_texture| {
                try sdl_renderer.endFrame(sdl_device, cmdbuf, swapchain_texture);
            }

            mouse.prev = mouse.cur;
            mouse.cur.scrolled = .none;
            keyboard.prev = keyboard.cur;
            _ = frame_arena.reset(.retain_capacity);
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

        try errify(c.SDL_SubmitGPUCommandBuffer(cmdbuf));
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
    .{ c.SDL_SCANCODE_DELETE, .Delete },
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
    .{ c.SDL_SCANCODE_LCTRL, .ControlLeft },
    .{ c.SDL_SCANCODE_RCTRL, .ControlRight },
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
const Canvas = kommon.Canvas;
const zstbi = @import("zstbi");

pub const std_options: std.Options = .{
    // TODO: remove before final release
    .log_level = .debug,
};

fn loadShader(
    comptime name: []const u8,
    sampler_count: u32,
    uniform_buffer_count: u32,
    storage_buffer_count: u32,
    storage_texture_count: u32,
) !*c.SDL_GPUShader {
    const stage = comptime if (std.mem.endsWith(u8, name, "vert"))
        c.SDL_GPU_SHADERSTAGE_VERTEX
    else if (std.mem.endsWith(u8, name, "frag"))
        c.SDL_GPU_SHADERSTAGE_FRAGMENT
    else
        unreachable;

    // TODO
    // const code = @embedFile(name);
    const code = @embedFile("kommon/shaders_v2/" ++ name ++ ".spv");
    const shader_info: c.SDL_GPUShaderCreateInfo = .{
        .code = code.ptr,
        .code_size = code.len,
        .entrypoint = "main",
        .format = c.SDL_GPU_SHADERFORMAT_SPIRV,
        .stage = stage,
        .num_samplers = sampler_count,
        .num_uniform_buffers = uniform_buffer_count,
        .num_storage_buffers = storage_buffer_count,
        .num_storage_textures = storage_texture_count,
    };
    return errify(c.SDL_CreateGPUShader(sdl_device, &shader_info));
}

fn Buffer(T: type) type {
    return struct {
        const Self = @This();

        total_count: usize,
        mapped_ptr: ?[*]T,
        pushed_count: usize,

        sdl_buffer: *c.SDL_GPUBuffer,
        sdl_transfer_buffer: *c.SDL_GPUTransferBuffer,

        pub fn init(device: *c.SDL_GPUDevice, total_count: u32, usage: enum { vertex, index, indirect, storage }) !Self {
            const total_size: u32 = @sizeOf(T) * total_count;

            const sdl_buffer = try errify(c.SDL_CreateGPUBuffer(device, &.{
                .usage = switch (usage) {
                    .indirect => c.SDL_GPU_BUFFERUSAGE_INDIRECT,
                    .index => c.SDL_GPU_BUFFERUSAGE_INDEX,
                    .vertex => c.SDL_GPU_BUFFERUSAGE_VERTEX,
                    .storage => c.SDL_GPU_BUFFERUSAGE_GRAPHICS_STORAGE_READ,
                },
                .size = total_size,
            }));
            errdefer c.SDL_ReleaseGPUBuffer(device, sdl_buffer);

            const sdl_transfer_buffer: *c.SDL_GPUTransferBuffer = try errify(c.SDL_CreateGPUTransferBuffer(
                device,
                &.{
                    .usage = c.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
                    .size = total_size,
                },
            ));
            errdefer c.SDL_ReleaseGPUTransferBuffer(device, sdl_transfer_buffer);

            return .{
                .total_count = total_count,
                .mapped_ptr = null,
                .pushed_count = 0,
                .sdl_buffer = sdl_buffer,
                .sdl_transfer_buffer = sdl_transfer_buffer,
            };
        }

        pub fn deinit(self: *Self, device: *c.SDL_GPUDevice) void {
            c.SDL_ReleaseGPUTransferBuffer(device, self.sdl_transfer_buffer);
            c.SDL_ReleaseGPUBuffer(device, self.sdl_buffer);
        }

        pub fn startMap(self: *Self, device: *c.SDL_GPUDevice) !void {
            const tb = self.sdl_transfer_buffer;
            self.mapped_ptr = @ptrCast(@alignCast(try errify(
                c.SDL_MapGPUTransferBuffer(device, tb, true),
            )));
            self.pushed_count = 0;
        }

        pub fn endMap(self: *Self, device: *c.SDL_GPUDevice) void {
            c.SDL_UnmapGPUTransferBuffer(device, self.sdl_transfer_buffer);
            self.mapped_ptr = null;
        }

        pub fn push(self: *Self, items: []const T) !void {
            if (self.mapped_ptr == null) @panic("can only push between .startMap and .endMap");
            if (items.len > (self.total_count - self.pushed_count)) return error.RanOutOfBuffer;
            @memcpy(self.mapped_ptr.?[0..items.len], items);
            self.mapped_ptr.? += items.len;
            self.pushed_count += items.len;
        }

        pub fn upload(self: *Self, copy_pass: *c.SDL_GPUCopyPass) void {
            if (self.mapped_ptr != null) @panic("can't upload between .startMap and .endMap");
            if (self.pushed_count == 0) return;
            c.SDL_UploadToGPUBuffer(copy_pass, &.{
                .transfer_buffer = self.sdl_transfer_buffer,
                .offset = 0,
            }, &.{
                .buffer = self.sdl_buffer,
                .offset = 0,
                .size = @intCast(@sizeOf(T) * self.pushed_count),
            }, true);
        }
    };
}

const SdlRenderer = struct {
    const Vertex = kommon.Renderer.Vertex;
    const Drawable = kommon.Renderer.Drawable;
    const ModelInfo = kommon.Renderer.ModelInfo;
    const Texture = kommon.Renderer.Texture;

    pipeline: *c.SDL_GPUGraphicsPipeline,
    vertex_buffer: Buffer(Vertex),
    index_buffer: Buffer([3]u16),
    drawable_buffer: Buffer(Drawable),
    drawcall_buffer: Buffer(c.SDL_GPUIndexedIndirectDrawCommand),

    textures: [texture_count]?*c.SDL_GPUTexture,
    samplers: [texture_count]?*c.SDL_GPUSampler,
    const texture_count = 2;

    const max_vertex_count = std.math.maxInt(u16);
    const max_drawables_count = std.math.maxInt(u16);
    comptime { // make sure i didn't confuse the power of two
        assert(65_000 < max_vertex_count and max_vertex_count < 66_000);
    }

    pub fn init(device: *c.SDL_GPUDevice, window: *c.SDL_Window) !SdlRenderer {
        var result: SdlRenderer = undefined;

        result.textures = @splat(null);
        result.samplers = @splat(null);

        result.pipeline = blk: {
            const vertex_shader = try loadShader(
                "RendererUber.vert",
                0,
                0,
                1,
                0,
            );
            defer c.SDL_ReleaseGPUShader(device, vertex_shader);

            const fragment_shader = try loadShader(
                "RendererUber.frag",
                texture_count,
                0,
                0,
                0,
            );
            defer c.SDL_ReleaseGPUShader(device, fragment_shader);

            const vertex_attributes: [@typeInfo(Vertex).@"struct".fields.len]c.SDL_GPUVertexAttribute = .{
                .{ .location = 0, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = @offsetOf(Vertex, "relative_pos") },
                .{ .location = 1, .buffer_slot = 0, .format = c.SDL_GPU_VERTEXELEMENTFORMAT_FLOAT2, .offset = @offsetOf(Vertex, "uv") },
            };
            const vertex_buffer_descriptions: [1]c.SDL_GPUVertexBufferDescription = .{
                .{ .slot = 0, .pitch = @sizeOf(Vertex), .input_rate = c.SDL_GPU_VERTEXINPUTRATE_VERTEX, .instance_step_rate = 0 },
            };

            const pipeline_create_info: c.SDL_GPUGraphicsPipelineCreateInfo = .{
                .target_info = .{
                    .num_color_targets = 1,
                    .color_target_descriptions = &.{
                        .format = c.SDL_GetGPUSwapchainTextureFormat(device, window),
                        .blend_state = .{
                            .enable_blend = true,
                            // TODO: revise
                            .src_color_blendfactor = c.SDL_GPU_BLENDFACTOR_SRC_ALPHA,
                            .dst_color_blendfactor = c.SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
                            .color_blend_op = c.SDL_GPU_BLENDOP_ADD,
                            .src_alpha_blendfactor = c.SDL_GPU_BLENDFACTOR_ONE,
                            .dst_alpha_blendfactor = c.SDL_GPU_BLENDFACTOR_ONE_MINUS_SRC_ALPHA,
                            .alpha_blend_op = c.SDL_GPU_BLENDOP_ADD,
                        },
                    },
                },
                .vertex_input_state = .{
                    .num_vertex_buffers = vertex_buffer_descriptions.len,
                    .vertex_buffer_descriptions = &vertex_buffer_descriptions,
                    .num_vertex_attributes = vertex_attributes.len,
                    .vertex_attributes = &vertex_attributes,
                },
                .primitive_type = c.SDL_GPU_PRIMITIVETYPE_TRIANGLELIST,
                .vertex_shader = vertex_shader,
                .fragment_shader = fragment_shader,
            };

            break :blk try errify(c.SDL_CreateGPUGraphicsPipeline(device, &pipeline_create_info));
        };
        errdefer c.SDL_ReleaseGPUGraphicsPipeline(device, result.pipeline);

        result.vertex_buffer = try .init(device, max_vertex_count, .vertex);
        errdefer result.vertex_buffer.deinit(device);

        result.index_buffer = try .init(device, max_vertex_count, .index);
        errdefer result.index_buffer.deinit(device);

        result.drawable_buffer = try .init(device, max_drawables_count, .storage);
        errdefer result.drawable_buffer.deinit(device);

        result.drawcall_buffer = try .init(device, max_drawables_count, .indirect);
        errdefer result.drawcall_buffer.deinit(device);

        return result;
    }

    pub fn deinit(self: *SdlRenderer, device: *c.SDL_GPUDevice) void {
        for (self.textures) |p| if (p != null) c.SDL_ReleaseGPUTexture(device, p.?);
        for (self.samplers) |p| if (p != null) c.SDL_ReleaseGPUSampler(device, p.?);
        self.drawcall_buffer.deinit(device);
        self.drawable_buffer.deinit(device);
        self.index_buffer.deinit(device);
        self.vertex_buffer.deinit(device);
        c.SDL_ReleaseGPUGraphicsPipeline(device, self.pipeline);
    }

    pub fn startFrame(self: *SdlRenderer, device: *c.SDL_GPUDevice) !void {
        try self.vertex_buffer.startMap(device);
        try self.index_buffer.startMap(device);
        try self.drawable_buffer.startMap(device);
        try self.drawcall_buffer.startMap(device);
    }

    fn undoStartFrame(self: *SdlRenderer, device: *c.SDL_GPUDevice) void {
        self.vertex_buffer.endMap(device);
        self.index_buffer.endMap(device);
        self.drawable_buffer.endMap(device);
        self.drawcall_buffer.endMap(device);
    }

    pub fn endFrame(self: *SdlRenderer, device: *c.SDL_GPUDevice, cmdbuf: *c.SDL_GPUCommandBuffer, swapchain_texture: *c.SDL_GPUTexture) !void {
        self.vertex_buffer.endMap(device);
        self.index_buffer.endMap(device);
        self.drawable_buffer.endMap(device);
        self.drawcall_buffer.endMap(device);

        if (self.drawcall_buffer.pushed_count == 0) return;

        const copy_pass = try errify(c.SDL_BeginGPUCopyPass(cmdbuf));
        self.vertex_buffer.upload(copy_pass);
        self.index_buffer.upload(copy_pass);
        self.drawable_buffer.upload(copy_pass);
        self.drawcall_buffer.upload(copy_pass);
        c.SDL_EndGPUCopyPass(copy_pass);

        const color_target_info: c.SDL_GPUColorTargetInfo = .{
            .texture = swapchain_texture,
            // TODO: configurable
            .clear_color = .{ .r = 0.3, .g = 0.4, .b = 0.5, .a = 1.0 },
            .load_op = c.SDL_GPU_LOADOP_CLEAR,
            .store_op = c.SDL_GPU_STOREOP_STORE,
        };

        const render_pass = c.SDL_BeginGPURenderPass(cmdbuf, &color_target_info, 1, null);

        c.SDL_BindGPUGraphicsPipeline(render_pass, self.pipeline);
        c.SDL_BindGPUVertexBuffers(render_pass, 0, &.{
            .buffer = self.vertex_buffer.sdl_buffer,
            .offset = 0,
        }, 1);
        c.SDL_BindGPUIndexBuffer(render_pass, &.{
            .buffer = self.index_buffer.sdl_buffer,
            .offset = 0,
        }, c.SDL_GPU_INDEXELEMENTSIZE_16BIT);
        c.SDL_BindGPUVertexStorageBuffers(render_pass, 0, &[1]*c.SDL_GPUBuffer{
            self.drawable_buffer.sdl_buffer,
        }, 1);

        var sampler_bindings: [texture_count]c.SDL_GPUTextureSamplerBinding = undefined;
        for (&sampler_bindings, self.textures, self.samplers) |*binding, texture, sampler| {
            binding.* = .{ .texture = texture, .sampler = sampler };
        }
        c.SDL_BindGPUFragmentSamplers(render_pass, 0, &sampler_bindings, texture_count);

        c.SDL_DrawGPUIndexedPrimitivesIndirect(
            render_pass,
            self.drawcall_buffer.sdl_buffer,
            0,
            @intCast(self.drawcall_buffer.pushed_count),
        );

        c.SDL_EndGPURenderPass(render_pass);
    }

    pub fn setTextureFromBmp(self: *SdlRenderer, device: *c.SDL_GPUDevice, id: usize, comptime bmp_path: []const u8, sampler_info: *const c.SDL_GPUSamplerCreateInfo) !void {
        const bmp_bytes = @embedFile(bmp_path);
        const bmp_io = try errify(c.SDL_IOFromConstMem(bmp_bytes.ptr, bmp_bytes.len));
        const bmp_surface = try errify(c.SDL_LoadBMP_IO(bmp_io, true));
        defer c.SDL_DestroySurface(bmp_surface);

        try self.setTexture(device, id, .{
            .w = @intCast(bmp_surface.*.w),
            .h = @intCast(bmp_surface.*.h),
            .pixels = @as([*]const [4]u8, @ptrCast(bmp_surface.*.pixels))[0..@intCast(bmp_surface.*.w * bmp_surface.*.h)],
        }, sampler_info);
    }

    pub fn setTexture(self: *SdlRenderer, device: *c.SDL_GPUDevice, id: usize, data: Texture, sampler_info: *const c.SDL_GPUSamplerCreateInfo) !void {
        assert(0 < id and id <= texture_count);
        assert(data.w * data.h == data.pixels.len);

        const sampler = try errify(c.SDL_CreateGPUSampler(device, sampler_info));
        errdefer c.SDL_ReleaseGPUSampler(device, sampler);

        const texture = try errify(c.SDL_CreateGPUTexture(device, &.{
            .type = c.SDL_GPU_TEXTURETYPE_2D,
            .format = c.SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
            .width = data.w,
            .height = data.h,
            .layer_count_or_depth = 1,
            .num_levels = 1,
            .usage = c.SDL_GPU_TEXTUREUSAGE_SAMPLER,
        }));
        errdefer c.SDL_ReleaseGPUTexture(device, texture);

        const transfer_buffer = try errify(c.SDL_CreateGPUTransferBuffer(device, &.{
            .usage = c.SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD,
            .size = data.w * data.h * 4,
        }));
        defer c.SDL_ReleaseGPUTransferBuffer(device, transfer_buffer);

        const ptr: [*][4]u8 = @ptrCast(@alignCast(try errify(
            c.SDL_MapGPUTransferBuffer(device, transfer_buffer, false),
        )));
        @memcpy(ptr[0 .. data.w * data.h], data.pixels);
        c.SDL_UnmapGPUTransferBuffer(device, transfer_buffer);

        const upload_cmd_buf = try errify(c.SDL_AcquireGPUCommandBuffer(device));

        const copy_pass = try errify(c.SDL_BeginGPUCopyPass(upload_cmd_buf));
        c.SDL_UploadToGPUTexture(copy_pass, &.{
            .transfer_buffer = transfer_buffer,
            .offset = 0,
        }, &.{
            .texture = texture,
            .w = data.w,
            .h = data.h,
            .d = 1,
        }, false);
        c.SDL_EndGPUCopyPass(copy_pass);

        try errify(c.SDL_SubmitGPUCommandBuffer(upload_cmd_buf));

        if (self.textures[id - 1] != null or self.samplers[id - 1] != null) @panic("TODO");
        self.textures[id - 1] = texture;
        self.samplers[id - 1] = sampler;
    }

    pub fn addModel(self: *SdlRenderer, vertices: []const Vertex, indices: []const [3]u16) !ModelInfo {
        const result: ModelInfo = .{
            .num_indices = @intCast(indices.len * 3),
            .first_index = @intCast(self.index_buffer.pushed_count * 3),
            .vertex_offset = @intCast(self.vertex_buffer.pushed_count),
        };

        try self.vertex_buffer.push(vertices);
        try self.index_buffer.push(indices);

        return result;
    }

    pub fn addDrawable(self: *SdlRenderer, drawable: Drawable, model_info: ModelInfo) !void {
        assert(self.drawable_buffer.pushed_count == self.drawcall_buffer.pushed_count);
        const drawable_id = self.drawcall_buffer.pushed_count;

        try self.drawable_buffer.push(&.{drawable});
        try self.drawcall_buffer.push(&.{.{
            .num_indices = model_info.num_indices,
            .num_instances = 1,
            .first_index = model_info.first_index,
            .vertex_offset = model_info.vertex_offset,
            .first_instance = @intCast(drawable_id),
        }});
    }
};
