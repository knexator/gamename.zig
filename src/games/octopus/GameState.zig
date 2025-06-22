pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "octopus",
        .author = "knexator",
        .desired_aspect_ratio = 1.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const board_size: UVec2 = .both(8);
const octopus_pos: UVec2 = .new(3, 3);
const octopus_color: FColor = .fromHex("#D819FF");

canvas: Canvas,
mem: Mem,
smooth: LazyState,

focus: union(enum) { none, lines: struct { tile: UVec2, which: enum { idk, draw, erase } } } = .none,

edges: kommon.Grid2DEdges(bool),

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    var mem: Mem = .init(gpa);
    const edges: kommon.Grid2DEdges(bool) = try .initFill(mem.level.allocator(), board_size, false);
    edges.atSafePtr(.{ .pos = octopus_pos.add(.zero), .dir = IVec2.e1.neg() }).?.* = true;
    edges.atSafePtr(.{ .pos = octopus_pos.add(.zero), .dir = IVec2.e2.neg() }).?.* = true;
    edges.atSafePtr(.{ .pos = octopus_pos.add(.e1), .dir = IVec2.e1 }).?.* = true;
    edges.atSafePtr(.{ .pos = octopus_pos.add(.e1), .dir = IVec2.e2.neg() }).?.* = true;
    edges.atSafePtr(.{ .pos = octopus_pos.add(.e2), .dir = IVec2.e1.neg() }).?.* = true;
    edges.atSafePtr(.{ .pos = octopus_pos.add(.e2), .dir = IVec2.e2 }).?.* = true;
    edges.atSafePtr(.{ .pos = octopus_pos.add(.one), .dir = IVec2.e1 }).?.* = true;
    edges.atSafePtr(.{ .pos = octopus_pos.add(.one), .dir = IVec2.e2 }).?.* = true;
    dst.* = .{
        .edges = edges,
        .canvas = try .init(gl, gpa, &.{@embedFile("../../fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}),
        .mem = mem,
        .smooth = .init(gpa),
    };
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    self.edges.deinit(self.mem.level.allocator());
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
    const camera: Rect = .{ .top_left = .zero, .size = board_size.tof32() };
    const mouse = platform.getMouse(camera);
    platform.gl.clear(.gray(0.5));

    const board: kommon.Grid2D(void) = try .initUndefined(self.mem.frame.allocator(), board_size);
    var it = board.iterator();
    while (it.next()) |pos| {
        const key: Key = .fromFormat("tile {d} {d}", .{ pos.x, pos.y });
        const rect = board.getTileRect(camera, pos);
        const mouse_over = rect.contains(mouse.cur.position);
        const is_active = switch (self.focus) {
            .none => false,
            .lines => |l| l.tile.equals(pos),
        };
        const is_hot = mouse_over and (self.focus == .none or is_active);
        const hot_t = try self.smooth.float(
            .fromFormat("hot {d}", .{key}),
            if (is_hot) 1.0 else 0.0,
        );
        const active_t = try self.smooth.float(
            .fromFormat("active {d}", .{key}),
            if (is_active) 1.0 else 0.0,
        );
        self.canvas.fillRect(camera, rect, try self.smooth.fcolor(
            .fromFormat("bg {d}", .{key}),
            .white,
        ));
        self.canvas.fillRect(camera, rect, FColor.gray(0.5).withAlpha(hot_t * 0.4 - active_t * 0.2));
    }

    switch (self.focus) {
        .none => {
            if (mouse.wasPressed(.left)) {
                if (board.tileAt(mouse.cur.position, camera)) |tile| {
                    self.focus = .{ .lines = .{ .tile = tile, .which = .idk } };
                }
            }
        },
        .lines => |*l| {
            if (mouse.wasReleased(.left)) {
                self.focus = .none;
            } else if (board.tileAt(mouse.cur.position, camera)) |new_tile| {
                if (!new_tile.equals(l.tile)) {
                    if (kommon.grid2D.EdgePos.between(l.tile, new_tile)) |edge_pos| {
                        const edge_ptr = self.edges.atSafePtr(edge_pos).?;
                        switch (l.which) {
                            .idk => {
                                l.which = if (edge_ptr.*) .erase else .draw;
                                edge_ptr.* = !edge_ptr.*;
                            },
                            .erase => edge_ptr.* = false,
                            .draw => edge_ptr.* = true,
                        }
                    }
                    l.tile = new_tile;
                }
            }
        },
    }

    for (0..board_size.x + 1) |k| {
        self.canvas.line(camera, &.{ .new(tof32(k), 0), .new(tof32(k), board_size.y) }, 0.02, .black);
    }
    for (0..board_size.y + 1) |k| {
        self.canvas.line(camera, &.{ .new(0, tof32(k)), .new(board_size.y, tof32(k)) }, 0.02, .black);
    }
    var edge_it = self.edges.iterator();
    while (edge_it.next()) |edge| {
        const has_line = self.edges.at(edge);
        if (has_line) {
            self.canvas.line(camera, &.{
                edge.pos.tof32().add(.half),
                edge.pos.addSigned(edge.dir).tof32().add(.half),
            }, 0.1, octopus_color);
        }
    }

    self.canvas.fillRect(camera, (Rect{ .top_left = octopus_pos.tof32(), .size = .both(2) }).plusMargin(-0.1), octopus_color);

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
