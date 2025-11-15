pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "cc25",
        .author = "knexator",
        .desired_aspect_ratio = 1.0 / 1.0,
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

const camera: Rect = .{ .top_left = .zero, .size = .both(4) };

usual: kommon.Usual,

pieces: [3]PieceThing = .{
    .{
        .window = .{ .top_left = .zero, .size = .new(2, 2) },
        .handle = .{ .top_left = .new(2, 0), .size = .new(1, 2) },
    },
    .{
        .window = .{ .top_left = .zero, .size = .new(2, 2) },
        .handle = .{ .top_left = .new(0, 2), .size = .new(2, 1) },
    },
    .{
        .window = .{ .top_left = .zero, .size = .new(2, 2) },
        .handle = .{ .top_left = .new(2, 0), .size = .new(1, 2) },
    },
},
active_piece: ?*PieceThing = null,

const PieceThing = struct {
    window: Rect,
    handle: Rect,
    hot_t: f32 = 0,
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

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);

    const mouse = platform.getMouse(camera);

    const hot_piece: ?*PieceThing = for (&self.pieces) |*p| {
        if (p.handle.contains(mouse.cur.position)) break p;
        if (!p.window.contains(mouse.cur.position)) break null;
    } else null;

    if (self.active_piece == null and mouse.wasPressed(.left)) {
        self.active_piece = hot_piece;
    } else if (self.active_piece != null and !mouse.cur.isDown(.left)) {
        self.active_piece = null;
    } else if (self.active_piece) |active| {
        active.handle = active.handle.move(mouse.deltaPos());
        active.window = active.window.move(mouse.deltaPos());
    }

    for (&self.pieces) |*p| {
        math.lerp_towards(&p.hot_t, if (p == hot_piece) 1 else 0, 0.6, platform.delta_seconds);
    }

    const canvas = &self.usual.canvas;
    platform.gl.clear(COLORS.black);

    for (0..self.pieces.len) |k| {
        canvas.fillRect(camera, camera, COLORS.black.withAlpha(0.25));
        const p: *const PieceThing = &self.pieces[self.pieces.len - k - 1];
        canvas.gl.startStencil();
        canvas.fillRect(camera, camera, .white);
        canvas.gl.blackStencil();
        canvas.fillRect(camera, p.window, .black);
        canvas.gl.doneStencil();
        canvas.fillRect(camera, camera, COLORS.orange);
        canvas.gl.stopStencil();
        canvas.borderRect(camera, p.window, 0.1, .inner, COLORS.orange);
        canvas.fillRect(camera, p.handle.plusMargin(0.05 * p.hot_t - 0.1), COLORS.blue);
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
