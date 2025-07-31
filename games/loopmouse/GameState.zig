const PlayerState = struct {
    pos: Vec2,
};

/// all times are in [0,1)
const RewardedDirection = struct {
    start: f32,
    duration: f32,
    pos: Vec2,
    dir: Vec2,

    fn isActive(self: RewardedDirection, t: f32) bool {
        var after = t - self.start;
        if (after < 0) after += 1;
        assert(after >= 0);
        return after < self.duration;
    }
};

pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "loopmouse",
        .author = "knexator",
        .desired_aspect_ratio = 4.0 / 3.0,
    },
    .sounds = .{},
    .loops = .{},
    .preloaded_images = .{
        // TODO: don't require this here
        .arial_atlas = "fonts/Arial.png",
    },
};
pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const COLORS = struct {
    bg: FColor = .gray(0.5),
}{};

const loop_duration = 1.5;

started: bool = false,

point_areas: std.SegmentedList(RewardedDirection, 256) = .{},

history: std.SegmentedList(PlayerState, 1024) = .{},
history_ages: std.SegmentedList(f32, 1024) = .{},

random: std.Random.DefaultPrng,
canvas: Canvas,
mem: Mem,
smooth: kommon.LazyState,
// lazy_state: LocalDecisions,

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.smooth = .init(dst.mem.forever.allocator());
    // dst.lazy_state = .init(gpa);
    // TODO: random seed
    dst.random = .init(1);
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    // self.mem.deinit();
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    _ = self;
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    _ = self.mem.frame.reset(.retain_capacity);
    _ = self.mem.scratch.reset(.retain_capacity);
    self.smooth.last_delta_seconds = platform.delta_seconds;
    // self.lazy_state.frameStart();

    // TODO
    const camera = (Rect.from(.{
        .{ .top_left = .zero },
        .{ .size = Vec2.new(4, 3).scale(16) },
    })).withAspectRatio(platform.aspect_ratio, .grow, .top_left);
    const mouse = platform.getMouse(camera);

    if (!self.started) {
        platform.gl.clear(COLORS.bg);
        var text = self.canvas.textBatch(0);
        try text.addText("Click to start", .centeredAt(camera.getAt(.new(0.5, 0.25))), 3, .white);
        text.draw(camera);

        self.canvas.strokeCircle(128, camera, camera.getCenter(), 4, 0.1, .white);
        self.canvas.fillCircle(camera, mouse.cur.position, 1, .white);

        if (mouse.cur.position.sub(camera.getCenter()).magSq() < 4 * 4 and mouse.wasPressed(.left)) {
            self.started = true;
            try self.history_ages.append(self.mem.forever.allocator(), 0);
            try self.history.append(self.mem.forever.allocator(), .{ .pos = mouse.cur.position });
            try self.point_areas.append(self.mem.forever.allocator(), .{
                .start = 0.05,
                .duration = 0.5,
                .pos = camera.getAt(.new(0.75, 0.25)),
                .dir = Vec2.new(1, 1).normalized(),
            });
        }

        return false;
    }

    const prev_t = self.history_ages.at(self.history_ages.len - 1).*;
    const cur_t: f32 = platform.delta_seconds + prev_t;
    const cur_state: PlayerState = .{ .pos = mouse.cur.position };
    try self.history_ages.append(self.mem.level.allocator(), cur_t);
    try self.history.append(self.mem.level.allocator(), cur_state);

    const entered_new_loop = @mod(cur_t, loop_duration) < @mod(prev_t, loop_duration);
    const loop_index: u32 = @intFromFloat(@divFloor(cur_t, loop_duration));
    if (entered_new_loop and loop_index % 1 == 0) {
        const random: math.Random = .init(self.random.random());
        try self.point_areas.append(self.mem.level.allocator(), .{
            .start = random.between(0, 1),
            .duration = 0.5 + random.around0(0.1),
            .pos = random.inRect(camera.plusMargin(-10)),
            .dir = random.direction(),
        });
    }

    var active_indices: std.ArrayList(usize) = .init(self.mem.frame.allocator());
    {
        try active_indices.append(self.history_ages.len - 1);
        var t = cur_t - loop_duration;
        while (t >= 0) : (t -= loop_duration) {
            try active_indices.append(self.historyIndexAt(t));
        }
    }

    platform.gl.clear(COLORS.bg);

    var point_areas_it = self.point_areas.constIterator(0);

    point_areas_it.set(0);
    while (point_areas_it.next()) |point_area| {
        if (point_area.isActive(@mod(cur_t / loop_duration, 1.0))) {
            self.canvas.line(camera, &.{
                point_area.pos.sub(point_area.dir.scale(5)),
                point_area.pos.add(point_area.dir.scale(5)),
                point_area.pos.add(point_area.dir.scale(5)).add(point_area.dir.rotate(3.0 / 8.0).scale(2)),
            }, 1, FColor.black.withAlpha(0.5));
        }
    }

    var points: std.ArrayList(Vec2) = .init(self.mem.frame.allocator());
    {
        var it = self.history.constIterator(0);
        while (it.next()) |p| {
            try points.append(p.pos);
        }
    }
    self.canvas.line(camera, points.items, 0.1, .black);
    points.deinit();

    for (active_indices.items) |i| {
        const state = self.history.at(i);
        self.canvas.fillCircle(camera, state.pos, 1.0, .white);
    }

    for (active_indices.items[1..]) |i| {
        const state = self.history.at(i);
        if (state.pos.sub(cur_state.pos).magSq() < 4) {
            self.history.shrink(i);
            self.history_ages.shrink(i);
            break;
        }
    }

    return false;
}

fn historyAt(self: GameState, t: f32) PlayerState {
    const index = self.historyIndexAt(t);
    return self.history.at(index).*;
}

fn historyIndexAt(self: GameState, t: f32) usize {
    assert(self.history_ages.len > 0);
    var low: usize = 0;
    var high: usize = self.history_ages.len - 1;

    while (low < high) {
        // Avoid overflowing in the midpoint calculation
        const mid = low + (high - low) / 2;
        switch (std.math.order(t, self.history_ages.at(mid).*)) {
            .eq => return mid,
            .gt => low = mid + 1,
            .lt => high = mid,
        }
    }
    assert(low == high);
    return low;
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
// pub const LocalDecisions = @import("../chesstory/GameState.zig").LocalDecisions;
