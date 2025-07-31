const PlayerState = struct {
    pos: Vec2,
    alive: bool,
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

const doors: []const struct {
    triggers: []const Rect,
    obstacles: []const Rect,

    pub fn basic(t: Vec2, o: Vec2) @This() {
        const door: Rect = .fromCenterAndSize(.zero, .new(2, 10));
        const trigger: Rect = .fromCenterAndSize(.zero, .both(5));
        return .{ .triggers = &.{trigger.move(t)}, .obstacles = &.{door.move(o)} };
    }
} = &.{
    .basic(.new(15, 5), .new(15, 15)),
    .basic(.new(25, 5), .new(25, 15)),
    .basic(.new(30, 15), .new(30, 25)),
    .basic(.new(20, 15), .new(20, 25)),
    .basic(.new(10, 15), .new(10, 25)),
};

history: std.SegmentedList(PlayerState, 1024) = .{},
history_ages: std.SegmentedList(f32, 1024) = .{},

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
        .{ .size = Vec2.new(4, 3).scale(10) },
    })).withAspectRatio(platform.aspect_ratio, .grow, .top_left);
    const mouse = platform.getMouse(camera);

    const prev_state: PlayerState = if (self.history_ages.len == 0) .{ .alive = true, .pos = mouse.cur.position } else self.history.at(self.history.len - 1).*;
    const cur_t: f32 = if (self.history_ages.len == 0) 0 else platform.delta_seconds + self.history_ages.at(self.history_ages.len - 1).*;
    const cur_state: PlayerState = .{ .pos = mouse.cur.position, .alive = prev_state.alive };
    try self.history_ages.append(self.mem.level.allocator(), cur_t);
    try self.history.append(self.mem.level.allocator(), cur_state);

    var active_indices: std.ArrayList(usize) = .init(self.mem.frame.allocator());
    {
        try active_indices.append(self.history_ages.len - 1);
        var t = cur_t - loop_duration;
        while (t > 0) : (t -= loop_duration) {
            try active_indices.append(self.historyIndexAt(t));
        }
    }

    const doors_opened = try self.mem.frame.allocator().alloc(bool, doors.len);
    for (doors_opened, doors) |*dst, door| {
        var all_triggers_active = true;
        for (door.triggers) |trigger| {
            const trigger_active: bool = blk: for (active_indices.items) |i| {
                const state = self.history.at(i);
                if (state.alive and trigger.contains(state.pos)) {
                    break :blk true;
                }
            } else false;

            if (!trigger_active) {
                all_triggers_active = false;
                break;
            }
        }
        dst.* = all_triggers_active;
    }

    for (doors, doors_opened) |door, open| {
        if (open) continue;
        for (door.obstacles) |r| {
            if (r.contains(cur_state.pos)) {
                self.history.at(self.history.len - 1).alive = false;
            }
        }
    }

    platform.gl.clear(COLORS.bg);

    for (doors, doors_opened) |door, open| {
        for (door.triggers) |r| {
            self.canvas.strokeRect(camera, r, 0.1, .black);
        }
        for (door.obstacles) |r| {
            if (open) {
                self.canvas.strokeRect(camera, r, 0.1, .white);
            } else {
                self.canvas.fillRect(camera, r, .gray(0.8));
            }
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
        if (state.alive) {
            self.canvas.fillCircle(camera, state.pos, 1.0, .white);
        } else {
            self.canvas.strokeCircle(128, camera, state.pos, 1.0, 0.1, .white);
        }
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
