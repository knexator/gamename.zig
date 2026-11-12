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

const CarlaneObstacle = struct {
    period: f32,
    t: f32,
    path: math.Segment,
    size: Vec2,

    fn body(self: CarlaneObstacle) Rect {
        return .fromCenterAndSize(self.path.at(self.t / self.period), self.size);
    }
};

const CarlaneObstacleV2 = struct {
    data: []const u8,
    car_size: Vec2,
    t: f32,
    path: math.Segment,

    fn body(self: CarlaneObstacleV2, k: usize) ?Rect {
        assert(k < self.data.len);
        if (self.data[k] == '.') return null;
        const t = self.t + @as(f32, @floatFromInt(k));
        return .fromCenterAndSize(self.path.at(@mod(t / @as(f32, @floatFromInt(self.data.len)), 1)), self.car_size);
    }
};

const FrameState = struct {
    time: f32,
    player: math.Circle,
    obstacle: CircularObstacle,
    obstacles: [0]CarlaneObstacle,
    obstaclesV2: [6]CarlaneObstacleV2,
    ever_lost: bool,

    pub const initial: FrameState = .{
        .time = 0,
        .player = .{ .center = .new(5, 0), .radius = 0.5 },
        .obstacle = .{
            .path = .{ .center = .new(3, 3), .radius = 2 },
            .size = 0.5,
            .t = 0,
            .speed = 0.2,
        },
        .obstacles = .{},
        .obstaclesV2 = .{
            .{ .t = 0, .data = ".........xxx.......", .path = .{ .a = .new(-1, 6), .b = .new(13, 6) }, .car_size = .one },
            .{ .t = 0, .data = ".x....x........", .path = .{ .a = .new(-1, 7.5), .b = .new(13, 7.5) }, .car_size = .one },
            .{ .t = 0, .data = "...Xxxx.......xx.......x...", .path = .{ .a = .new(-1, 9), .b = .new(13, 9) }, .car_size = .one },
            .{ .t = 0, .data = "..Xx...x......", .path = .{ .a = .new(-1, 10.5), .b = .new(13, 10.5) }, .car_size = .one },
            .{ .t = 0, .data = "....X....xxx....", .path = .{ .a = .new(-1, 12), .b = .new(13, 12) }, .car_size = .one },
            .{ .t = 0, .data = ".....Xxx..", .path = .{ .a = .new(-1, 13.5), .b = .new(13, 13.5) }, .car_size = .one },
        },
        .ever_lost = false,
    };

    const step_time = 1.0 / 30.0;
    const step_dist = 1 * step_time;
    fn next(self: *const FrameState, delta: Vec2) FrameState {
        assert(@abs(delta.mag() - step_dist) < 0.0001);

        var result: FrameState = self.*;
        result.time += step_time;
        result.player.center.addInPlace(delta);
        result.obstacle.t += result.obstacle.speed * step_time;
        for (&result.obstacles) |*obs| obs.t = @mod(obs.t + step_time, obs.period);
        for (&result.obstaclesV2) |*obs| obs.t = obs.t + step_time;
        result.ever_lost = self.ever_lost or result.obstacle.body().overlaps(result.player);
        for (result.obstacles) |obs| result.ever_lost = result.ever_lost or obs.body().overlapsCircle(result.player);
        for (result.obstaclesV2) |obs| {
            for (0..obs.data.len) |k| {
                if (obs.body(k)) |r| {
                    result.ever_lost = result.ever_lost or r.overlapsCircle(result.player);
                }
            }
        }
        return result;
    }

    fn draw(self: *const FrameState, camera: Rect, canvas: *Canvas) !void {
        canvas.fillCircleV2(camera, self.player, .white);
        canvas.fillCircleV2(camera, self.obstacle.body(), .cyan);
        for (self.obstacles) |obs| canvas.fillRect(camera, obs.body(), .cyan);
        for (self.obstaclesV2) |obs| {
            for (0..obs.data.len) |k| {
                if (obs.body(k)) |r| {
                    canvas.fillRect(camera, r, .cyan);
                }
            }
        }
        if (self.ever_lost) try canvas.drawText(0, camera, "Lost!", .centeredAt(.new(3, 1)), 1, .red);
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
    const camera: Rect = .withAspectRatio(.{ .top_left = .zero, .size = .new(12, 16) }, platform.aspect_ratio, .grow, .center);
    const mouse = platform.getMouse(camera);

    if (mouse.cur.isDown(.right)) {
        var undoed_time: f32 = 0;
        while (self.frames.items.len > 1 and undoed_time < platform.delta_seconds * 12) {
            _ = self.frames.pop();
            undoed_time += FrameState.step_time;
        }
    }

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
