pub const GameState = @This();
const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};
pub const tracy = @import("tracy");

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "frogger",
        .author = "knexator",
        .desired_aspect_ratio = 3.0 / 4.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

usual: kommon.Usual,
frames: std.ArrayList(FrameState),
accumulated_error: Vec2 = .zero,

const CircularObstacle = struct {
    path: math.Circle,
    size: f32,
    t: f32,
    speed: f32,

    fn body(self: CircularObstacle) math.Circle {
        const center = self.path.center.add(Vec2.fromPolar(self.path.radius, self.t));
        return .{ .center = center, .radius = self.size };
    }
};

const FrameState = struct {
    time: f32,
    player_pos: Vec2,
    obstacle: CircularObstacle,

    pub const initial: FrameState = .{
        .time = 0,
        .player_pos = .new(0, 0),
        .obstacle = .{
            .path = .{ .center = .new(3, 3), .radius = 2 },
            .size = 0.5,
            .t = 0,
            .speed = 0.2,
        },
    };

    const step_time = 1.0 / 30.0;
    const step_dist = 1 * step_time;
    fn next(self: *const FrameState, delta: Vec2) FrameState {
        assert(@abs(delta.mag() - step_dist) < 0.0001);

        var result: FrameState = self.*;
        result.time += step_time;
        result.player_pos.addInPlace(delta);
        result.obstacle.t += result.obstacle.speed * step_time;
        return result;
    }

    fn draw(self: *const FrameState, camera: Rect, canvas: *Canvas) !void {
        canvas.fillCircle(camera, self.player_pos, 1.0, .white);
        canvas.fillCircleV2(camera, self.obstacle.body(), .cyan);
    }
};

pub fn init(
    dst: *GameState,
    runtime_params: kommon.engine.InitRuntimeParamsFor(GameState),
    comptime _: kommon.engine.InitComptimeParamsFor(GameState),
) !void {
    const gpa = runtime_params.gpa;
    const gl = runtime_params.gl;
    const loaded_images = runtime_params.loaded_images;
    const random_seed = runtime_params.random_seed;

    dst.usual.init(gpa, random_seed, try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}));
    dst.frames = .init(gpa);
    try dst.frames.append(.initial);
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    _ = gpa;
    self.usual.deinit(undefined);
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    _ = self;
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);
    const camera: Rect = .withAspectRatio(.{ .top_left = .zero, .size = .new(6, 8) }, platform.aspect_ratio, .grow, .center);
    const mouse = platform.getMouse(camera);

    if (mouse.cur.isDown(.left)) {
        self.accumulated_error.addInPlace(mouse.deltaPos());
    }

    while (self.accumulated_error.mag() >= FrameState.step_dist) {
        const delta = Vec2.zero.towardsPure(self.accumulated_error, FrameState.step_dist);
        self.accumulated_error.towards(.zero, FrameState.step_dist);
        const new = self.frames.getLast().next(delta);
        try self.frames.append(new);
    }

    platform.gl.clear(.black);
    try self.frames.getLast().draw(camera, &self.usual.canvas);
    return false;
}

const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;

const kommon = @import("kommon");
const Triangulator = kommon.Triangulator;
const math = kommon.math;
const tof32 = math.tof32;
const Color = math.UColor;
const FColor = math.FColor;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const IVec2 = math.IVec2;
const funk = kommon.funktional;
const maybeMirror = math.maybeMirror;
const Noise = kommon.Noise;
const last = kommon.last;
pub const Mouse = kommon.input.Mouse;
pub const Keyboard = kommon.input.Keyboard;
pub const KeyboardButton = kommon.input.KeyboardButton;
pub const PrecomputedShape = kommon.renderer.PrecomputedShape;
pub const RenderableInfo = kommon.renderer.RenderableInfo;
pub const Gl = kommon.Gl;
pub const Canvas = kommon.Canvas;
pub const TextRenderer = Canvas.TextRenderer;
pub const Mem = kommon.Mem;
pub const Key = kommon.Key;
pub const LazyState = kommon.LazyState;
pub const EdgePos = kommon.grid2D.EdgePos;
