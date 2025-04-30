const c = @cImport({
    @cDefine("SDL_DISABLE_OLD_NAMES", {});
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_revision.h");
    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_MAIN_HANDLED' should be defined before including 'SDL_main.h'.
    @cDefine("SDL_MAIN_HANDLED", {});
    @cInclude("SDL3/SDL_main.h");
});

const PlatformGives = @import("game.zig").PlatformGives;
const game = @import("game.zig");
comptime {
    std.testing.refAllDeclsRecursive(game);
}

var my_game: if (@import("build_options").game_dynlib_path) |game_dynlib_path| struct {
    const Self = @This();

    api: *const game.CApi = &game.game_api,
    dyn_lib: ?std.DynLib = null,
    state: game.GameState,

    fn init() Self {
        return .{ .state = .init() };
    }

    fn update(self: *Self, platform_gives: PlatformGives) !void {
        try self.maybeReloadApi();
        self.api.update(&self.state, &platform_gives);
    }

    // TODO: use std.fs.watch.Watch
    fn maybeReloadApi(self: *Self) !void {
        if (self.dyn_lib) |*dyn_lib| dyn_lib.close();
        // const path = "./zig-out/lib/libgame.so";
        const path = if (@import("builtin").os.tag == .windows) blk: {
            try std.fs.copyFileAbsolute(game_dynlib_path, game_dynlib_path ++ ".tmp", .{});
            break :blk game_dynlib_path ++ ".tmp";
        } else game_dynlib_path;
        self.dyn_lib = try .open(path);
        self.api = self.dyn_lib.?.lookup(*const game.CApi, "game_api") orelse return error.LookupFail;
    }
} else game.GameState = undefined;

var window_size: UVec2 = .new(1280, 720);
fn getWindowRect() Rect {
    return .{
        .top_left = .zero,
        .size = window_size.tof32(),
    };
}

/// positions are in [0..1]x[0..1]
var mouse = Mouse{ .cur = .init, .prev = .init };

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
        _ = debug_allocator.deinit();
    };

    var render_queue: @import("renderer.zig").RenderQueue = .init(gpa);
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
        .aspect_ratio = window_size.aspectRatio(),
        .delta_seconds = 0,
    };

    errdefer |err| if (err == error.SdlError) std.log.err("SDL error: {s}", .{c.SDL_GetError()});

    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_SetMainReady' should be called before calling 'SDL_Init'.
    c.SDL_SetMainReady();

    try errify(c.SDL_SetAppMetadata(
        "Gamename",
        "0.0.0",
        "com.authorname.gamename",
    ));

    try errify(c.SDL_Init(c.SDL_INIT_VIDEO));
    defer c.SDL_Quit();

    errify(c.SDL_SetHint(c.SDL_HINT_RENDER_VSYNC, "1")) catch {};

    const sdl_window: *c.SDL_Window, const sdl_renderer: *c.SDL_Renderer = create_window_and_renderer: {
        var sdl_window: ?*c.SDL_Window = null;
        var sdl_renderer: ?*c.SDL_Renderer = null;
        try errify(c.SDL_CreateWindowAndRenderer(
            "Gamename",
            @intCast(window_size.x),
            @intCast(window_size.y),
            0,
            &sdl_window,
            &sdl_renderer,
        ));
        break :create_window_and_renderer .{ sdl_window.?, sdl_renderer.? };
    };
    defer c.SDL_DestroyRenderer(sdl_renderer);
    defer c.SDL_DestroyWindow(sdl_window);

    my_game = .init();

    var timer = try std.time.Timer.start();

    main_loop: while (true) {
        {
            var event: c.SDL_Event = undefined;
            while (c.SDL_PollEvent(&event)) {
                switch (event.type) {
                    c.SDL_EVENT_QUIT => break :main_loop,
                    c.SDL_EVENT_WINDOW_RESIZED => window_size = .new(
                        @intCast(event.window.data1),
                        @intCast(event.window.data2),
                    ),
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
                    else => {},
                }
            }
        }

        // while (timekeeper.consume()) {
        {
            const ns_since_last_frame = timer.lap();
            sdl_platform.render_queue.pending_commands.clearRetainingCapacity();
            sdl_platform.delta_seconds = math.tof32(ns_since_last_frame) / std.time.ns_per_s;
            sdl_platform.aspect_ratio = window_size.aspectRatio();
            try my_game.update(sdl_platform);
            // try game.update(1.0 / 60.0);
            // mouse.prev = mouse.cur;
            // mouse.cur.scrolled = .none;
            // keyboard.prev = keyboard.cur;
        }

        // Draw
        {
            defer sdl_platform.render_queue.pending_commands.clearRetainingCapacity();
            var it = sdl_platform.render_queue.pending_commands.constIterator(0);
            // TODO: don't use gpa as the scratch allocator
            while (it.next()) |cmd| try render(sdl_renderer, cmd.*, gpa);

            try errify(c.SDL_RenderPresent(sdl_renderer));
        }

        // timekeeper.produce(c.SDL_GetPerformanceCounter());
    }
}

fn render(sdl_renderer: *c.SDL_Renderer, cmd: @import("renderer.zig").RenderQueue.Command, scratch: std.mem.Allocator) !void {
    switch (cmd) {
        .clear => |color| {
            try errify(c.SDL_SetRenderDrawColor(sdl_renderer, color.r, color.g, color.b, color.a));
            try errify(c.SDL_RenderClear(sdl_renderer));
        },
        .shape => |shape| {
            const screen_positions = try scratch.alloc(c.SDL_FPoint, shape.local_points.len);
            defer scratch.free(screen_positions);

            const screen: Rect = .{
                .top_left = .zero,
                .size = window_size.tof32(),
            };

            for (shape.local_points, screen_positions) |local_pos, *screen_pos| {
                const cur = screen.applyToLocalPosition(
                    shape.camera.localFromWorldPosition(
                        shape.parent_world_point.applyToLocalPosition(local_pos),
                    ),
                );
                screen_pos.* = .{ .x = cur.x, .y = cur.y };
            }

            // TODO: allow non-convex polygons
            if (shape.fill) |color| {
                const vertices = try scratch.alloc(c.SDL_Vertex, screen_positions.len);
                defer scratch.free(vertices);

                for (screen_positions, vertices) |pos, *vertex| {
                    vertex.* = c.SDL_Vertex{
                        .position = c.SDL_FPoint{ .x = pos.x, .y = pos.y },
                        .color = @bitCast(color.toFColor()),
                        .tex_coord = c.SDL_FPoint{ .x = 0, .y = 0 },
                    };
                }

                assert(vertices.len >= 3);
                const indices = try scratch.alloc(c_int, 3 * (vertices.len - 2));
                defer scratch.free(indices);
                for (0..vertices.len - 2) |k| {
                    indices[k * 3 + 0] = 0;
                    indices[k * 3 + 1] = @intCast(k + 1);
                    indices[k * 3 + 2] = @intCast(k + 2);
                }

                try errify(c.SDL_RenderGeometry(
                    sdl_renderer,
                    null,
                    vertices.ptr,
                    @intCast(vertices.len),
                    indices.ptr,
                    @intCast(indices.len),
                ));
            }

            if (shape.stroke) |color| {
                try errify(c.SDL_SetRenderDrawColor(sdl_renderer, color.r, color.g, color.b, color.a));
                try errify(c.SDL_RenderLines(sdl_renderer, screen_positions.ptr, @intCast(screen_positions.len)));
                try errify(c.SDL_RenderLine(
                    sdl_renderer,
                    kommon.last(c.SDL_FPoint, screen_positions).?.x,
                    kommon.last(c.SDL_FPoint, screen_positions).?.y,
                    screen_positions[0].x,
                    screen_positions[0].y,
                ));
            }
        },
    }
}

const std = @import("std");
const assert = std.debug.assert;

const kommon = @import("kommon");
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

pub const std_options: std.Options = .{
    // TODO: remove before final release
    .log_level = .debug,
};
