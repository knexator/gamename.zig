pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "cc25",
        .author = "knexator",
        .desired_aspect_ratio = BOARD_SIZE.addY(SLIDERS_N).aspectRatio(),
    },
    .sounds = .{},
    .loops = .{},
    .preloaded_images = .{
        // TODO: don't require this here
        .arial_atlas = "fonts/Arial.png",
    },
};
pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const COLORS: struct {
    white: FColor = .fromHex("#ffebd8"),
    orange: FColor = .fromHex("#ff7f00"),
    blue: FColor = .fromHex("#4f67ff"),
    black: FColor = .fromHex("#19011a"),
} = .{};

const camera: Rect = .{ .top_left = .zero, .size = BOARD_SIZE.addY(SLIDERS_N) };

const BOARD_SIZE: Vec2 = .both(13);
const SLIDERS_N = 4;
const ARMS_N = 4;
const SLIDER_HALFSIZE = 2;

arm_left: Arm = .{ .base = BOARD_SIZE.sub(.both(1)).mul(.new(0, 0.5)), .segments = .{
    .{ .base_length = 2, .dir = .yneg, .influence = .{ 1, 0, 0, 0 } },
    .{ .base_length = 2, .dir = .xpos, .influence = .{ 0, 1, 0, 0 } },
    .{ .base_length = 2, .dir = .ypos, .influence = .{ 0, 0, 1, 0 } },
    .{ .base_length = 2, .dir = .xpos, .influence = .{ 0, 0, 0, 1 } },
} },
arm_right: Arm = .{ .base = BOARD_SIZE.sub(.both(1)).mul(.new(1, 0.5)), .segments = .{
    .{ .base_length = 2, .dir = .yneg, .influence = .{ 0, 1, 0, 0 } },
    .{ .base_length = 2, .dir = .xneg, .influence = .{ 0, 0, 1, 0 } },
    .{ .base_length = 2, .dir = .ypos, .influence = .{ 0, 0, 0, 1 } },
    .{ .base_length = 2, .dir = .xneg, .influence = .{ 1, 0, 0, 0 } },
} },

sliders: [SLIDERS_N]Slider = @splat(.{}),
active_slider: ?usize = null,

usual: kommon.Usual,

const Slider = struct {
    value: f32 = 0,
    hot_t: f32 = 0,
};

const Arm = struct {
    base: Vec2,
    segments: [ARMS_N]ArmSegment,
};

const ArmSegment = struct {
    base_length: f32,
    dir: Vec2,
    influence: [SLIDERS_N]f32,

    pub fn length(arm: *const ArmSegment, sliders: *const [SLIDERS_N]Slider) f32 {
        var result: f32 = arm.base_length;
        for (sliders, arm.influence) |s, i| {
            result += i * s.value;
        }
        return result;
    }
};

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
    random_seed: u64,
    // tweakable: type,
    // tweakable: struct {
    //     fcolor: fn (name: []const u8, value: *FColor) void,
    // },
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);

    dst.usual.init(
        gpa,
        random_seed,
        try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}),
    );
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

fn sliderPos(k: usize, t: f32) Vec2 {
    assert(math.inRangeClosed(t, -1, 1));
    return BOARD_SIZE.mul(.new(0.5, 1)).addY(tof32(k) + 0.5).addX(t * SLIDER_HALFSIZE);
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);

    const mouse = platform.getMouse(camera);

    const hot_slider: ?usize = for (self.sliders, 0..) |s, k| {
        const handle = sliderPos(k, s.value / SLIDER_HALFSIZE);
        if (handle.distTo(mouse.cur.position) < 0.5) break k;
    } else null;

    if (self.active_slider == null and mouse.wasPressed(.left)) {
        self.active_slider = hot_slider;
    } else if (self.active_slider != null and !mouse.cur.isDown(.left)) {
        self.active_slider = null;
    } else if (self.active_slider) |active| {
        self.sliders[active].value = math.clamp(mouse.cur.position.x - camera.size.x / 2, -SLIDER_HALFSIZE, SLIDER_HALFSIZE);
    }

    for (&self.sliders, 0..) |*p, k| {
        math.lerp_towards(&p.hot_t, if (k == hot_slider) 1 else 0, 0.6, platform.delta_seconds);
        if (k != self.active_slider) math.lerp_towards(&p.value, @round(p.value), 0.6, platform.delta_seconds);
    }

    const canvas = &self.usual.canvas;
    platform.gl.clear(COLORS.black);

    const grid: kommon.grid2D.Grid2D(void, BOARD_SIZE.toInt(usize)) = .initUndefinedV2(BOARD_SIZE.toInt(usize));
    var it = grid.iterator();
    while (it.next()) |p| {
        canvas.borderRect(camera, grid.getTileRect(.{ .size = BOARD_SIZE, .top_left = .zero }, p), 0.05, .inner, COLORS.white);
    }

    inline for (.{ self.arm_left, self.arm_right }) |arm| {
        var base_pos: Vec2 = arm.base;
        for (arm.segments, 0..) |segment, k| {
            const next_pos = base_pos.add(segment.dir.scale(segment.length(&self.sliders)));
            canvas.line(camera, &.{ base_pos.add(.half), next_pos.add(.half) }, 0.5, if (k % 2 == 0) COLORS.blue else COLORS.orange);
            base_pos = next_pos;
        }
    }

    for (self.sliders, 0..) |s, k| {
        const handle = sliderPos(k, s.value / SLIDER_HALFSIZE);
        canvas.fillCircle(camera, handle, 0.5, COLORS.white);
    }

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
