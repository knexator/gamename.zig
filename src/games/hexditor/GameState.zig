pub const Hexditor = struct {
    values: std.ArrayList(u8),

    pub fn init(alloc: std.mem.Allocator) Hexditor {
        return .{ .values = .init(alloc) };
    }

    pub fn draw(self: Hexditor, canvas: *Canvas, camera: Rect) !void {
        const COLORS = struct {
            text: FColor = .white,
            bg_panel: FColor = .fromHex("#222222"),
            bg_checker: [2]FColor = .{ .fromHex("#222211"), .fromHex("#112222") },
        }{};

        const cell_size = camera.size.x / 16.0;
        var bg_rects: std.ArrayList(Canvas.FilledRect) = .init(canvas.frame_arena.allocator());
        var fg_text = canvas.textBatch(0);

        for (0..16) |k| {
            try bg_rects.append(.{ .pos = .{
                .top_left = .new(cell_size * tof32(k), 0),
                .size = .both(cell_size),
            }, .color = COLORS.bg_panel });
            // }, .color = COLORS.bg_checker[@mod(k, 2)] });
            try fg_text.addFmt(
                "{x}",
                .{k},
                .{ .center = bg_rects.getLast().pos.getCenter() },
                cell_size * 0.75,
                COLORS.text,
            );
        }

        // var rest_of_bytes = self.values.items;
        // var row_top_y = cell_size;
        var line_index: usize = 0;
        while (tof32(line_index + 2) * cell_size < camera.size.y) : (line_index += 1) {
            for (0..16) |k| {
                try bg_rects.append(.{ .pos = .{
                    .top_left = .new(cell_size * tof32(k), tof32(line_index + 1) * cell_size),
                    .size = .both(cell_size),
                }, .color = COLORS.bg_checker[@mod(k + line_index, 2)] });
                try fg_text.addFmt(
                    "{x:02}",
                    .{kommon.safeAt(u8, self.values.items, 16 * line_index + k) orelse 0},
                    .{ .center = bg_rects.getLast().pos.getCenter() },
                    cell_size * 0.75,
                    COLORS.text,
                );
            }
        }

        canvas.fillRects(camera, bg_rects.items);
        fg_text.draw(camera);
    }
};

pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "hexditor",
        .author = "knexator",
        .desired_aspect_ratio = 4.0 / 3.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

canvas: Canvas,
mem: Mem,

core: Hexditor,

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("../../fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.core = .init(dst.mem.forever.allocator());
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
    try self.core.draw(&self.canvas, Rect.unit.withAspectRatio(
        platform.aspect_ratio,
        .grow,
        .top_left,
    ));
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
const Camera = math.Camera;
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
pub const Mem = @import("../tres_undos/GameState.zig").Mem;
pub const Key = @import("../akari/GameState.zig").Key;
pub const LazyState = @import("../akari/GameState.zig").LazyState;
pub const EdgePos = kommon.grid2D.EdgePos;
