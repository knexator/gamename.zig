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
const starting_edges_local: [8]EdgePos = .{
    .{ .pos = .zero, .dir = .ne2 },
    .{ .pos = .e1, .dir = .ne2 },
    .{ .pos = .e1, .dir = .e1 },
    .{ .pos = .one, .dir = .e1 },
    .{ .pos = .one, .dir = .e2 },
    .{ .pos = .e2, .dir = .e2 },
    .{ .pos = .e2, .dir = .ne1 },
    .{ .pos = .zero, .dir = .ne1 },
};
const target = .{
    [_]IVec2{ .ne2, .e1, .e1 },
    [_]IVec2{ .e1, .e1, .e1, .e2 },
    [_]IVec2{ .e1, .e2, .e2, .e1, .ne2 },
    [_]IVec2{ .e2, .e2, .e1, .e1, .e2, .ne1 },
    [_]IVec2{ .e2, .ne1, .ne1, .e2, .e1, .e1, .e1 },
    [_]IVec2{ .ne1, .ne1, .e2, .e2, .ne1, .ne2, .ne2, .ne2 },
    [_]IVec2{ .ne1, .ne2, .ne1, .ne2, .ne2, .ne2, .e1, .e2, .e2 },
    [_]IVec2{ .ne2, .ne2, .ne2, .e1, .e1, .e1, .e1, .e1, .e2, .ne1 },
};

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
    for (starting_edges_local) |edge| {
        edges.atSafePtr(edge.translate(octopus_pos)).?.* = true;
    }
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

    const blocked: kommon.Grid2D(bool) = try .initFill(self.mem.frame.allocator(), board_size, false);
    blocked.set(octopus_pos.add(.zero), true);
    blocked.set(octopus_pos.add(.e1), true);
    blocked.set(octopus_pos.add(.e2), true);
    blocked.set(octopus_pos.add(.one), true);

    const tentacle_at = try tentacleAtFromEdges(self.edges, blocked, self.mem.frame.allocator());
    var tentacles = try tentaclesFromEdges(self.edges, self.mem.frame.allocator());

    for ([8]KeyboardButton{
        .Digit1, .Digit2, .Digit3, .Digit4,
        .Digit5, .Digit6, .Digit7, .Digit8,
    }, tentacles) |key, tentacle| {
        if (platform.wasKeyPressedOrRetriggered(key, 0.2)) {
            if (platform.keyboard.cur.isDown(.ShiftLeft) or platform.keyboard.cur.isDown(.ShiftRight)) {
                self.shrinkTentacle(tentacle);
            } else {
                self.maybeGrowTentacle(tentacle, tentacle_at);
            }
        }
    }

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
            if (platform.wasButtonPressedOrRetriggered(.left, 0.2)) {
                if (board.tileAt(mouse.cur.position, camera)) |tile| {
                    // self.focus = .{ .lines = .{ .tile = tile, .which = .idk } };
                    if (tentacle_at.at2(tile)) |id| {
                        self.maybeGrowTentacle(tentacles[id], tentacle_at);
                    }
                }
            } else if (platform.wasButtonPressedOrRetriggered(.right, 0.2)) {
                if (board.tileAt(mouse.cur.position, camera)) |tile| {
                    if (tentacle_at.at2(tile)) |id| {
                        if (tentacles[id].getLast().nextPos().equals(tile)) {
                            self.shrinkTentacle(tentacles[id]);
                        } else while (!tentacles[id].getLast().nextPos().equals(tile)) {
                            self.shrinkTentacle(tentacles[id]);
                            tentacles = try tentaclesFromEdges(self.edges, self.mem.frame.allocator());
                        }
                    }
                }
            }
        },
        .lines => |*l| {
            if (true) unreachable;
            if (mouse.wasReleased(.left)) {
                self.focus = .none;
            } else if (board.tileAt(mouse.cur.position, camera)) |new_tile| {
                if (!new_tile.equals(l.tile)) {
                    if (kommon.grid2D.EdgePos.between(l.tile, new_tile)) |edge_pos| {
                        if (tentacle_at.at2(new_tile) == null or tentacle_at.at2(new_tile) == tentacle_at.at2(l.tile)) {
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
                    }
                    l.tile = new_tile;
                }
            }
        },
    }

    for (tentacles, 0..) |lst, k| {
        for (lst.items) |edge| {
            const color = FColor.fromHsv(tof32(k) / 8.0, 1.0, 1.0);
            const cam = camera.move(Vec2.half.neg());
            self.canvas.line(cam, &.{
                edge.pos.tof32(),
                edge.pos.addSigned(edge.dir).tof32(),
            }, 0.25, color);
            self.canvas.fillCircle(cam, edge.pos.tof32(), 0.3, color);
            self.canvas.fillCircle(cam, edge.pos.addSigned(edge.dir).tof32(), 0.25, color);
        }
    }

    for (0..board_size.x + 1) |k| {
        self.canvas.line(camera, &.{ .new(tof32(k), 0), .new(tof32(k), board_size.y) }, 0.02, .black);
    }
    for (0..board_size.y + 1) |k| {
        self.canvas.line(camera, &.{ .new(0, tof32(k)), .new(board_size.y, tof32(k)) }, 0.02, .black);
    }
    // var edge_it = self.edges.iterator();
    // while (edge_it.next()) |edge| {
    //     const has_line = self.edges.at(edge);
    //     if (has_line) {
    //         self.canvas.line(camera, &.{
    //             edge.pos.tof32().add(.half),
    //             edge.pos.addSigned(edge.dir).tof32().add(.half),
    //         }, 0.1, octopus_color);
    //     }
    // }

    inline for (@typeInfo(@TypeOf(target)).@"struct".fields, starting_edges_local) |field, starting_edge_local| {
        const dirs = @field(target, field.name);
        var cur = starting_edge_local.translate(octopus_pos);
        self.canvas.line(camera, &.{
            cur.pos.tof32().add(.half),
            cur.pos.addSigned(cur.dir).tof32().add(.half),
        }, 0.1, octopus_color.withAlpha(0.1));
        for (dirs) |d| {
            cur = .{ .pos = cur.pos.addSigned(cur.dir), .dir = d };
            self.canvas.line(camera, &.{
                cur.pos.tof32().add(.half),
                cur.pos.addSigned(cur.dir).tof32().add(.half),
            }, 0.1, octopus_color.withAlpha(0.1));
        }
    }

    self.canvas.fillRect(camera, (Rect{ .top_left = octopus_pos.tof32(), .size = .both(2) }).plusMargin(-0.1), octopus_color);

    return false;
}

fn tentacleAtFromEdges(edges: kommon.Grid2DEdges(bool), blocked: kommon.Grid2D(bool), allocator: std.mem.Allocator) !kommon.Grid2D(?usize) {
    const tentacle_at: kommon.Grid2D(?usize) = try .initFill(allocator, board_size, null);
    for ([_]IVec2{
        .new(0, -1), .new(1, -1),
        .new(2, 0),  .new(2, 1),
        .new(1, 2),  .new(0, 2),
        .new(-1, 1), .new(-1, 0),
    }, 0..) |delta, k| {
        tentacle_at.set(octopus_pos.addSigned(delta), k);
        var queue: std.fifo.LinearFifo(UVec2, .Dynamic) = .init(allocator);
        try queue.writeItem(octopus_pos.addSigned(delta));
        while (queue.readItem()) |pos| {
            for (IVec2.cardinal_directions) |dir| {
                const new_pos = pos.cast(isize).add(dir);
                if (tentacle_at.inBoundsSigned(new_pos) and
                    tentacle_at.atSigned(new_pos) == null and
                    blocked.atSigned(new_pos) == false and
                    edges.at(.{ .pos = pos, .dir = dir }) == true)
                {
                    tentacle_at.setSigned(new_pos, k);
                    try queue.writeItem(new_pos.cast(usize));
                }
            }
        }
    }
    return tentacle_at;
}

fn tentaclesFromEdges(edges: kommon.Grid2DEdges(bool), allocator: std.mem.Allocator) ![8]std.ArrayList(EdgePos) {
    var tentacles: [8]std.ArrayList(kommon.grid2D.EdgePos) = undefined;
    for (&tentacles, starting_edges_local) |*dst, starting_edge_local| {
        dst.* = .init(allocator);
        try dst.append(starting_edge_local.translate(octopus_pos));
        main: while (true) {
            const cur_end = dst.getLast().pos.addSigned(dst.getLast().dir);
            // TODO: remove this check
            for (dst.items) |e| if (e.pos.equals(cur_end)) break :main;

            const next_dir: ?IVec2 = blk: for (IVec2.cardinal_directions) |dir| {
                if (!dir.equals(dst.getLast().dir.neg()) and
                    edges.atSafe(.{ .pos = cur_end, .dir = dir }) orelse false)
                {
                    break :blk dir;
                }
            } else null;
            if (next_dir) |d| {
                try dst.append(.{ .pos = cur_end, .dir = d });
            } else break;
        }
    }
    return tentacles;
}

fn shrinkTentacle(self: *GameState, tentacle: std.ArrayList(EdgePos)) void {
    const last_edge = tentacle.getLast();
    if (tentacle.items.len > 1) {
        self.edges.set(last_edge, false);
    }
}

fn maybeGrowTentacle(self: *GameState, tentacle: std.ArrayList(EdgePos), tentacle_at: kommon.Grid2D(?usize)) void {
    const last_edge = tentacle.getLast();
    const last_pos = last_edge.pos.addSigned(last_edge.dir);
    const desired_edge: EdgePos = .{ .pos = last_pos, .dir = last_edge.dir };
    if (isFreeAfterEdge(tentacle_at, desired_edge)) {
        self.edges.set(desired_edge, true);
    } else {
        const turn_p: EdgePos = .{ .pos = last_pos, .dir = last_edge.dir.rotate(1) };
        const turn_n: EdgePos = .{ .pos = last_pos, .dir = last_edge.dir.rotate(-1) };
        if (isFreeAfterEdge(tentacle_at, turn_p) and
            !isFreeAfterEdge(tentacle_at, turn_n))
        {
            self.edges.set(turn_p, true);
        } else if (!isFreeAfterEdge(tentacle_at, turn_p) and
            isFreeAfterEdge(tentacle_at, turn_n))
        {
            self.edges.set(turn_n, true);
        }
    }
}

fn isFreeAfterEdge(tentacle_at: kommon.Grid2D(?usize), edge: EdgePos) bool {
    if (!tentacle_at.inBoundsSigned(edge.pos.cast(isize).add(edge.dir))) return false;
    return tentacle_at.at2(edge.pos.addSigned(edge.dir)) == null;
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
pub const EdgePos = kommon.grid2D.EdgePos;
