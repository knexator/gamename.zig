const kommon = @import("kommon.zig");
const Vec2 = kommon.math.Vec2;
const Camera = kommon.math.Camera;
const Rect = kommon.math.Rect;

pub const MouseButton = enum { left, right, middle };
pub const MouseState = struct {
    /// client_pos is in ([0..aspect_ratio], [0..1])
    client_pos: Vec2,
    /// the new thing
    position: Vec2,
    scrolled: enum {
        up,
        down,
        none,

        pub fn toNumber(self: @This()) f32 {
            return switch (self) {
                .none => 0,
                .up => 1,
                .down => -1,
            };
        }

        pub fn toInt(self: @This()) isize {
            return switch (self) {
                .none => 0,
                .up => 1,
                .down => -1,
            };
        }
    },
    buttons: kommon.meta.BoolFlags(MouseButton, false),
    // buttons: std.enums.EnumSet(MouseButton),

    pub const init: MouseState = .{
        .client_pos = .zero,
        .position = .zero,
        .scrolled = .none,
        .buttons = .{
            .left = false,
            .middle = false,
            .right = false,
        },
    };

    pub fn pos(self: MouseState, camera: Camera) Vec2 {
        return camera.worldFromScreenPosition(self.client_pos);
    }

    pub fn posV2(self: MouseState, camera: Rect) Vec2 {
        return camera.top_left.add(self.client_pos.scale(camera.size.y));
    }

    pub fn isDown(self: MouseState, button: MouseButton) bool {
        return switch (button) {
            .left => self.buttons.left,
            .middle => self.buttons.middle,
            .right => self.buttons.right,
        };
    }
};

pub const Mouse = struct {
    cur: MouseState,
    prev: MouseState,
    last_change_at: kommon.meta.StructFromEnum(MouseButton, f32, false) = undefined,
    cur_time: f32,

    pub const Cursor = enum(u8) { default, could_grab, grabbing, pointer };

    pub fn deltaPos(self: Mouse) Vec2 {
        return self.cur.position.sub(self.prev.position);
    }

    pub fn lastChangeAt(self: @This(), button: MouseButton) f32 {
        return switch (button) {
            inline else => |x| @field(self.last_change_at, @tagName(x)),
        };
    }

    pub fn timeSinceChange(self: @This(), button: MouseButton) f32 {
        return self.cur_time - self.lastChangeAt(button);
    }

    pub fn setChanged(self: *@This(), button: MouseButton) void {
        return switch (button) {
            inline else => |x| @field(self.last_change_at, @tagName(x)) = self.cur_time,
        };
    }

    pub fn wasPressed(self: Mouse, button: MouseButton) bool {
        return self.cur.isDown(button) and !self.prev.isDown(button);
    }

    pub fn wasReleased(self: Mouse, button: MouseButton) bool {
        return !self.cur.isDown(button) and self.prev.isDown(button);
    }
};

pub fn CustomKeyboardState(CustomKeyboardButton: type) type {
    return struct {
        // TODO: try setting it to packed
        keys: kommon.meta.BoolFlags(CustomKeyboardButton, false),

        pub const init: @This() = std.mem.zeroes(@This());

        pub fn isDown(self: @This(), button: CustomKeyboardButton) bool {
            return switch (button) {
                inline else => |x| @field(self.keys, @tagName(x)),
            };
        }

        pub fn isShiftDown(self: @This()) bool {
            return self.isDown(.ShiftLeft) or self.isDown(.ShiftRight);
        }
    };
}

pub fn CustomKeyboard(CustomKeyboardButton: type) type {
    return struct {
        cur: CustomKeyboardState(CustomKeyboardButton),
        prev: CustomKeyboardState(CustomKeyboardButton),
        last_change_at: kommon.meta.StructFromEnum(CustomKeyboardButton, f32, false) = undefined,
        cur_time: f32,

        pub fn lastChangeAt(self: @This(), button: CustomKeyboardButton) f32 {
            return switch (button) {
                inline else => |x| @field(self.last_change_at, @tagName(x)),
            };
        }

        pub fn timeSinceChange(self: @This(), button: CustomKeyboardButton) f32 {
            return self.cur_time - self.lastChangeAt(button);
        }

        pub fn setChanged(self: *@This(), button: CustomKeyboardButton) void {
            return switch (button) {
                inline else => |x| @field(self.last_change_at, @tagName(x)) = self.cur_time,
            };
        }

        pub fn wasPressed(self: @This(), button: CustomKeyboardButton) bool {
            return self.cur.isDown(button) and !self.prev.isDown(button);
        }

        pub fn wasReleased(self: @This(), button: CustomKeyboardButton) bool {
            return !self.cur.isDown(button) and self.prev.isDown(button);
        }
    };
}

pub const KeyboardButton = enum(u8) {
    pub fn digit(n: usize) KeyboardButton {
        return switch (n) {
            0 => .Digit0,
            1 => .Digit1,
            2 => .Digit2,
            3 => .Digit3,
            4 => .Digit4,
            5 => .Digit5,
            6 => .Digit6,
            7 => .Digit7,
            8 => .Digit8,
            9 => .Digit9,
            else => unreachable,
        };
    }

    pub const directional_keys: [4]struct {
        dir: Vec2,
        keys: []const KeyboardButton,
    } = .{
        .{ .dir = .xpos, .keys = &.{
            .ArrowRight, .KeyD,
        } },
        .{ .dir = .ypos, .keys = &.{
            .ArrowDown, .KeyS,
        } },
        .{ .dir = .xneg, .keys = &.{
            .ArrowLeft, .KeyA,
        } },
        .{ .dir = .yneg, .keys = &.{
            .ArrowUp, .KeyW,
        } },
    };

    Escape,
    Digit1,
    Digit2,
    Digit3,
    Digit4,
    Digit5,
    Digit6,
    Digit7,
    Digit8,
    Digit9,
    Digit0,
    Minus,
    Equal,
    Backspace,
    Tab,
    KeyQ,
    KeyW,
    KeyE,
    KeyR,
    KeyT,
    KeyY,
    KeyU,
    KeyI,
    KeyO,
    KeyP,
    BracketLeft,
    BracketRight,
    Enter,
    ControlLeft,
    KeyA,
    KeyS,
    KeyD,
    KeyF,
    KeyG,
    KeyH,
    KeyJ,
    KeyK,
    KeyL,
    Semicolon,
    Quote,
    Backquote,
    ShiftLeft,
    Backslash,
    KeyZ,
    KeyX,
    KeyC,
    KeyV,
    KeyB,
    KeyN,
    KeyM,
    Comma,
    Period,
    Slash,
    ShiftRight,
    NumpadMultiply,
    AltLeft,
    Space,
    CapsLock,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    F9,
    F10,
    Pause,
    ScrollLock,
    Numpad7,
    Numpad8,
    Numpad9,
    NumpadSubtract,
    Numpad4,
    Numpad5,
    Numpad6,
    NumpadAdd,
    Numpad1,
    Numpad2,
    Numpad3,
    Numpad0,
    NumpadDecimal,
    PrintScreen,
    IntlBackslash,
    F11,
    F12,
    NumpadEqual,
    F13,
    F14,
    F15,
    F16,
    F17,
    F18,
    F19,
    F20,
    F21,
    F22,
    F23,
    KanaMode,
    Lang2,
    Lang1,
    IntlRo,
    F24,
    Convert,
    NonConvert,
    IntlYen,
    NumpadComma,
    MediaTrackPrevious,
    MediaTrackNext,
    NumpadEnter,
    ControlRight,
    AudioVolumeMute,
    LaunchApp2,
    MediaPlayPause,
    MediaStop,
    VolumeDown,
    VolumeUp,
    BrowserHome,
    NumpadDivide,
    AltRight,
    NumLock,
    Home,
    ArrowUp,
    PageUp,
    ArrowLeft,
    ArrowRight,
    End,
    ArrowDown,
    PageDown,
    Insert,
    Delete,
    MetaLeft,
    MetaRight,
    ContextMenu,
    Power,
    BrowserSearch,
    BrowserFavorites,
    BrowserRefresh,
    BrowserStop,
    BrowserForward,
    BrowserBack,
    LaunchApp1,
    LaunchMail,
    MediaSelect,
};

pub const Keyboard = CustomKeyboard(KeyboardButton);

const std = @import("std");
