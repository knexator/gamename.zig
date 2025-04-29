//! must not import anything from main.zig, the platform layer

// https://nullprogram.com/blog/2014/12/23/

pub const PlatformGives = struct {
    gpa: std.mem.Allocator,
    render_queue: *@import("renderer.zig").RenderQueue,
    getAspectRatio: *const fn () f32,
};

pub const GameState = struct {
    n: usize = 0,
    n2: usize = 0,

    pub fn init() GameState {
        return .{};
    }

    pub fn deinit(self: *GameState) void {
        _ = self;
    }

    pub fn update(self: *GameState, platform_gives: PlatformGives) !void {
        try platform_gives.render_queue.clear(.gray(128));

        const camera: Rect = .{
            .top_left = .zero,
            .size = .new(3 * platform_gives.getAspectRatio(), 3),
        };
        try platform_gives.render_queue.drawShape(camera, .{}, &.{
            .new(1, 1),
            .new(2, 1),
            .new(2, 2),
            .new(1, 2),
        }, .black, .white);

        std.log.debug("Update, n is {d} and n2 is {d}", .{ self.n, self.n2 });
        self.n += 1;
        self.n2 += 1;
        // return error.hola;
    }
};

pub const CApi = extern struct {
    // TODO: init should take an allocator
    // init: *const fn () *GameState,
    update: *const @TypeOf(_update),
    // TODO: unload/reload

    fn _update(game: *GameState, platform_gives: *const PlatformGives) callconv(.c) void {
        game.update(platform_gives.*) catch unreachable;
    }
};

pub export const game_api: CApi = .{
    .update = CApi._update,
};

test "foo" {
    try std.testing.expect(true);
}

const std = @import("std");
const math = @import("kommon").math;
const Color = math.Color;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
