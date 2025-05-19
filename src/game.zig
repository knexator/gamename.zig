//! must not import anything from main.zig, the platform layer

// https://nullprogram.com/blog/2014/12/23/

pub const stuff = GameState.stuff;
pub const metadata = stuff.metadata;
pub const sounds = stuff.sounds;
pub const loops = stuff.loops;
pub const preloaded_images = stuff.preloaded_images;
// TODO: a bit weird
pub const Images = GameState.Images;

pub const PlatformGives = struct {
    gpa: std.mem.Allocator,
    getMouse: *const fn (camera: Rect) Mouse,
    keyboard: Keyboard,
    aspect_ratio: f32,
    delta_seconds: f32,
    // idk if this should be given by the platform
    global_seconds: f32,
    sound_queue: *std.EnumSet(std.meta.FieldEnum(@TypeOf(sounds))),
    loop_volumes: *std.EnumArray(std.meta.FieldEnum(@TypeOf(loops)), f32),
    gl: Gl,
};

// TODO: choose at comptime
// pub const GameState = @import("games/papuzo/GameState.zig");
pub const GameState = @import("games/snakanake/GameState.zig");

pub const CApi = extern struct {
    update: *const @TypeOf(_update),
    reload: *const @TypeOf(_reload),

    fn _update(game: *GameState, platform_gives: *const PlatformGives) callconv(.c) bool {
        return game.update(platform_gives.*) catch unreachable;
    }

    fn _reload(dst: *GameState, gpa: *const std.mem.Allocator, gl: *const Gl) callconv(.c) void {
        dst.deinit(gpa.*);
        // TODO
        dst.* = GameState.init(gpa.*, gl.*, undefined) catch unreachable;
    }
};

pub export const game_api: CApi = .{
    .reload = CApi._reload,
    .update = CApi._update,
};

comptime {
    std.testing.refAllDecls(GameState);
}

const std = @import("std");
const assert = std.debug.assert;

const kommon = @import("kommon");
const Triangulator = kommon.Triangulator;
const math = kommon.math;
const Color = math.Color;
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
pub const Mouse = kommon.input.Mouse;
pub const Keyboard = kommon.input.Keyboard;
pub const KeyboardButton = kommon.input.KeyboardButton;
pub const PrecomputedShape = kommon.renderer.PrecomputedShape;
pub const RenderableInfo = kommon.renderer.RenderableInfo;
pub const Gl = kommon.Gl;
pub const Canvas = kommon.Canvas;
pub const TextRenderer = Canvas.TextRenderer;
