pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "tres_undos",
        .author = "knexator",
        .desired_aspect_ratio = 1152.0 / 648.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

canvas: Canvas,
mem: struct {
    frame: std.heap.ArenaAllocator,

    pub fn init(gpa: std.mem.Allocator) @This() {
        return .{ .frame = .init(gpa) };
    }

    pub fn deinit(self: *@This()) void {
        self.frame.deinit();
    }

    pub fn onFrameBegin(self: *@This()) void {
        _ = self.frame.reset(.retain_capacity);
    }
},

smooth: LazyState,

pub fn init(
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !GameState {
    return .{
        .canvas = try .init(gl, gpa, &.{@embedFile("../../fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}),
        .mem = .init(gpa),
        .smooth = .init(gpa),
    };
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
}

const LazyState = struct {
    f32s: std.AutoHashMap(Key, f32),
    fcolors: std.AutoHashMap(Key, FColor),
    rects: std.AutoHashMap(Key, Rect),

    pub fn init(gpa: std.mem.Allocator) LazyState {
        return .{
            .f32s = .init(gpa),
            .fcolors = .init(gpa),
            .rects = .init(gpa),
        };
    }

    pub fn deinit(self: *LazyState) void {
        self.arena.deinit();
        self.f32s.deinit();
        self.fcolors.deinit();
        self.rects.deinit();
    }

    pub fn float(self: *LazyState, key: Key, goal: f32) !f32 {
        const gop = try self.f32s.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* = std.math.lerp(gop.value_ptr.*, goal, 0.2);
        } else {
            gop.value_ptr.* = goal;
        }
        return gop.value_ptr.*;
    }

    pub fn fcolor(self: *LazyState, key: Key, goal: FColor) !FColor {
        const gop = try self.fcolors.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* = .lerp(gop.value_ptr.*, goal, 0.2);
        } else {
            gop.value_ptr.* = goal;
        }
        return gop.value_ptr.*;
    }

    pub fn rect(self: *LazyState, key: Key, goal: Rect) !Rect {
        const gop = try self.rects.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* = .lerp(gop.value_ptr.*, goal, 0.2);
        } else {
            gop.value_ptr.* = goal;
        }
        return gop.value_ptr.*;
    }
};

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.mem.onFrameBegin();
    // const camera: Rect = .{ .top_left = .zero, .size = .both(4) };
    // const mouse = platform.getMouse(camera);
    platform.gl.clear(.gray(0.5));
    return false;
}

pub const Key = enum(u64) {
    _,

    pub fn fromString(str: []const u8) Key {
        return @enumFromInt(std.hash.Wyhash.hash(0, str));
    }

    pub fn fromFormat(comptime fmt: []const u8, args: anytype) Key {
        var buf: [0x1000]u8 = undefined;
        return fromString(std.fmt.bufPrint(&buf, fmt, args) catch panic("Key fmt was too long", .{}));
    }
};

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
pub const Mouse = kommon.input.Mouse;
pub const Keyboard = kommon.input.Keyboard;
pub const KeyboardButton = kommon.input.KeyboardButton;
pub const PrecomputedShape = kommon.renderer.PrecomputedShape;
pub const RenderableInfo = kommon.renderer.RenderableInfo;
pub const Gl = kommon.Gl;
pub const Canvas = kommon.Canvas;
pub const TextRenderer = Canvas.TextRenderer;
