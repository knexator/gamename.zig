pub fn CApiFor(comptime GameState: type) type {
    return extern struct {
        update: *const @TypeOf(_update) = _update,
        beforeHotReload: *const @TypeOf(_beforeHotReload) = _beforeHotReload,
        afterHotReload: *const @TypeOf(_afterHotReload) = _afterHotReload,

        fn _update(game: *GameState, platform_gives: *const PlatformGivesFor(GameState)) callconv(.c) bool {
            return game.update(platform_gives.*) catch unreachable;
        }

        fn _beforeHotReload(game: *GameState) callconv(.c) void {
            return game.beforeHotReload() catch unreachable;
        }

        fn _afterHotReload(game: *GameState) callconv(.c) void {
            return game.afterHotReload() catch unreachable;
        }
    };
}

pub fn PlatformGivesFor(comptime GameState: type) type {
    const stuff = GameState.stuff;
    const sounds = stuff.sounds;
    const loops = stuff.loops;
    return struct {
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
        sample_rate: f32,
        enqueueSamples: *const fn (src: []const f32) void,
        queuedSeconds: *const fn () f32,
        gl: Gl,
        downloadAsFile: *const fn (filename: []const u8, contents: []const u8) void,
        askUserForFile: *const fn () void,
        userUploadedFile: *const fn () ?std.io.AnyReader,
        forgetUserUploadedFile: *const fn () void,

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
}

const std = @import("std");
const kommon = @import("kommon.zig");
const Rect = kommon.math.Rect;
const Mouse = kommon.input.Mouse;
const Keyboard = kommon.input.Keyboard;
const KeyboardButton = kommon.input.KeyboardButton;
const Gl = kommon.Gl;
