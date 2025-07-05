const InputState = struct {
    hovering: ?usize,
    grabbing: ?usize,
};

const BoardState = struct {
    // const PlacedElement = struct { pos: Vec2, id: usize };
    placed: std.ArrayList(struct { pos: Vec2, id: usize }),

    pub fn init(gpa: std.mem.Allocator) BoardState {
        return .{
            .placed = .init(gpa),
        };
    }

    pub fn addInitialElements(self: *BoardState) !void {
        for (0..4) |k| {
            try self.placed.append(.{
                .id = k + 1,
                .pos = .both(tof32(k)),
            });
        }
    }

    fn elementRect(pos: Vec2) Rect {
        return .{ .top_left = pos, .size = .one };
    }

    fn elementLabel(pos: Vec2) Canvas.TextRenderer.TextPosition {
        return .{
            .hor = .center,
            .ver = .median,
            .pos = pos.add(.half),
        };
    }

    pub fn combine(self: *BoardState, active: usize, pasive: usize) !void {
        assert(active != pasive);
        if (AlchemyData.combinationOf(
            self.placed.items[active].id,
            self.placed.items[pasive].id,
        )) |new_id| {
            self.placed.items[pasive].id = new_id;
            self.placed.items[pasive].pos = .lerp(
                self.placed.items[pasive].pos,
                self.placed.items[active].pos,
                0.5,
            );
            _ = self.placed.swapRemove(active);
        }
    }

    pub fn draw(self: BoardState, canvas: *Canvas, camera: Rect) !void {
        const COLORS = struct {
            text: FColor = .white,
            bg_panel: FColor = .fromHex("#222222"),
        }{};

        var bg_rects: std.ArrayList(Canvas.FilledRect) = .init(canvas.frame_arena.allocator());
        var fg_text = canvas.textBatch(0);

        for (self.placed.items) |element| {
            try bg_rects.append(.{
                .pos = elementRect(element.pos),
                .color = COLORS.bg_panel,
            });
            try fg_text.addText(
                AlchemyData.names[element.id],
                elementLabel(element.pos),
                0.75,
                COLORS.text,
            );
        }

        canvas.fillRects(camera, bg_rects.items);
        fg_text.draw(camera);
    }
};

const AlchemyData = struct {
    const names: []const []const u8 = @import("names.zon");
    const images_base64: []const []const u8 = @import("images.zon");
    const recipes: []const []const [2]usize = @import("base.zon");

    pub fn combinationOf(active_id: usize, pasive_id: usize) ?usize {
        for (recipes, 0..) |r, k| {
            for (r) |pair| {
                if (pair[0] == active_id and pair[1] == pasive_id) return k;
                if (pair[1] == active_id and pair[0] == pasive_id) return k;
            }
        } else return null;
    }
};

pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "alchemy",
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

board: BoardState,
input_state: InputState = .{ .grabbing = null, .hovering = null },

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("../../fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.board = .init(gpa);
    try dst.board.addInitialElements();
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

    const camera = (Rect{
        .top_left = .zero,
        .size = .both(7),
    }).withAspectRatio(
        platform.aspect_ratio,
        .grow,
        .top_center,
    );
    const mouse = platform.getMouse(camera);

    for (self.board.placed.items, 0..) |e, k| {
        if (k == self.input_state.grabbing) continue;
        if (BoardState.elementRect(e.pos).contains(mouse.cur.position)) {
            self.input_state.hovering = k;
        }
    }
    if (mouse.wasPressed(.left)) {
        assert(self.input_state.grabbing == null);
        self.input_state.grabbing = self.input_state.hovering;
        self.input_state.hovering = null;
    }
    if (self.input_state.grabbing) |grabbing_index| {
        self.board.placed.items[grabbing_index].pos.addInPlace(mouse.cur.position.sub(mouse.prev.position));
    }
    if (mouse.wasReleased(.left)) {
        if (self.input_state.grabbing != null and self.input_state.hovering != null) {
            try self.board.combine(self.input_state.grabbing.?, self.input_state.hovering.?);
        }
        self.input_state.grabbing = null;
    }

    platform.gl.clear(.black);
    try self.board.draw(&self.canvas, camera);

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
