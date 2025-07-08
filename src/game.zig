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
    setKeyChanged: *const fn (key: KeyboardButton) void,
    setButtonChanged: *const fn (button: kommon.input.MouseButton) void,
    aspect_ratio: f32,
    delta_seconds: f32,
    // idk if this should be given by the platform
    global_seconds: f32,
    sound_queue: *std.EnumSet(std.meta.FieldEnum(@TypeOf(sounds))),
    loop_volumes: *std.EnumArray(std.meta.FieldEnum(@TypeOf(loops)), f32),
    gl: Gl,

    pub fn wasKeyPressedOrRetriggered(self: @This(), key: KeyboardButton, retrigger_time: f32) bool {
        if (self.keyboard.wasPressed(key)) return true;
        if (self.keyboard.cur.isDown(key) and self.keyboard.timeSinceChange(key) > retrigger_time) {
            self.setKeyChanged(key);
            return true;
        }
        return false;
    }

    pub fn wasButtonPressedOrRetriggered(self: @This(), button: kommon.input.MouseButton, retrigger_time: f32) bool {
        const mouse = self.getMouse(.unit);
        if (mouse.wasPressed(button)) return true;
        if (mouse.cur.isDown(button) and mouse.timeSinceChange(button) > retrigger_time) {
            self.setButtonChanged(button);
            return true;
        }
        return false;
    }
};

// TODO: choose at comptime
// pub const GameState = @import("games/alchemy/GameState.zig");
pub const GameState = @import("games/hexditor/GameState.zig");
// pub const GameState = @import("games/octopus/GameState.zig");
// pub const GameState = @import("games/tres_undos/GameState.zig");
// pub const GameState = @import("games/akari/GameState.zig");
// pub const GameState = @import("games/fleury_ui/GameState.zig");
// pub const GameState = @import("games/snakanake/GameState.zig");

pub const CApi = extern struct {
    update: *const @TypeOf(_update),
    beforeHotReload: *const @TypeOf(_beforeHotReload),
    afterHotReload: *const @TypeOf(_afterHotReload),

    fn _update(game: *GameState, platform_gives: *const PlatformGives) callconv(.c) bool {
        return game.update(platform_gives.*) catch unreachable;
    }

    fn _beforeHotReload(game: *GameState) callconv(.c) void {
        return game.beforeHotReload() catch unreachable;
    }

    fn _afterHotReload(game: *GameState) callconv(.c) void {
        return game.afterHotReload() catch unreachable;
    }
};

pub export const game_api: CApi = .{
    .update = CApi._update,
    .beforeHotReload = CApi._beforeHotReload,
    .afterHotReload = CApi._afterHotReload,
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
