pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

const core = @import("core.zig");
const Drawer = @import("Drawer.zig");

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "vaulogy",
        .author = "knexator",
        .desired_aspect_ratio = 16.0 / 9.0,
    },
    .sounds = .{},
    .loops = .{},
    .preloaded_images = .{
        // TODO: don't require this here
        .arial_atlas = "fonts/Arial.png",
    },
};
pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

var COLORS: struct {
    bg: FColor = .gray(0.5),
} = .{};

usual: kommon.Usual,

drawer: Drawer,

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

    // tweakable.fcolor("bg", &COLORS.bg);

    dst.drawer = try .init(&dst.usual);
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
    // const mem = &self.usual.mem;
    // const canvas = &self.usual.canvas;

    const camera = (Rect{ .top_left = .zero, .size = .new(16, 9) })
        .withAspectRatio(platform.aspect_ratio, .grow, .top_left);
    const mouse = platform.getMouse(camera);

    platform.gl.clear(COLORS.bg);
    try self.drawer.drawSexpr(camera, .{
        .is_pattern = 0,
        .pos = .{ .pos = mouse.cur.position },
        .value = &.{ .pair = .{
            .left = &.{ .atom_lit = .{ .value = "Hermes" } },
            .right = &.{ .pair = .{
                .left = &.{ .atom_lit = .{ .value = "Ares" } },
                .right = Sexpr.builtin.nil,
            } },
        } },
        // .value = &.{ .atom_lit = .{ .value = "Hermes" } },
        // .value = Sexpr.builtin.nil,
    });

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

const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const PhysicalSexpr = @import("physical.zig").PhysicalSexpr;
const ViewHelper = @import("physical.zig").ViewHelper;
