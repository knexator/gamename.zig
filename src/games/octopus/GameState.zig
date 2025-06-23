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
const octopus_color_2: FColor = .fromHex("#C31EFF");
const octopus_color_body: FColor = .fromHex("#AE23FF");
const spot_color: FColor = .fromHex("#D866FF");
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

canvas: Canvas,
mem: Mem,
smooth: LazyState,

focus: union(enum) { none, lines: struct { tile: UVec2, which: enum { idk, draw, erase } } } = .none,
edges: kommon.Grid2DEdges(bool),
clues_tiles: kommon.Grid2D(?TileClue),
move_history: std.ArrayList(DeltaMove),
time_of_last_move: f32 = -std.math.inf(f32),

const DeltaMove = struct { where: EdgePos, added: bool };

const ClueLen = union(enum) {
    exact: usize,
    odd,
    even,
    idk,
    pub fn fits(self: ClueLen, n: usize) bool {
        return switch (self) {
            .exact => |v| n == v,
            .idk => true,
            .odd => @mod(n, 2) == 1,
            .even => @mod(n, 2) == 0,
        };
    }
};
const TileClue = union(enum) {
    starts_here_with_len: ClueLen,
    ends_here_with_len: ClueLen,

    pub fn isSatisfied(clue: TileClue, pos: UVec2, tentacles: [8]std.ArrayList(EdgePos)) bool {
        switch (clue) {
            .starts_here_with_len => |l| for (tentacles) |tentacle| {
                if (tentacle.items[0].nextPos().equals(pos)) return l.fits(tentacle.items.len);
            } else return false,
            .ends_here_with_len => |l| for (tentacles) |tentacle| {
                if (tentacle.getLast().nextPos().equals(pos)) return l.fits(tentacle.items.len);
            } else return false,
        }
    }
};

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
        .move_history = .init(gpa),
        .clues_tiles = try .initFill(mem.level.allocator(), board_size, null),
    };
    dst.clues_tiles.set(.new(2, 3), .{ .starts_here_with_len = .{ .exact = 8 } });
    dst.clues_tiles.set(.new(3, 5), .{ .starts_here_with_len = .{ .exact = 8 } });
    dst.clues_tiles.set(.new(1, 6), .{ .ends_here_with_len = .{ .exact = 8 } });
    dst.clues_tiles.set(.new(1, 7), .{ .ends_here_with_len = .{ .exact = 8 } });
    dst.clues_tiles.set(.new(1, 1), .{ .ends_here_with_len = .{ .exact = 6 } });
    dst.clues_tiles.set(.new(6, 1), .{ .ends_here_with_len = .{ .exact = 5 } });
    dst.clues_tiles.set(.new(0, 4), .{ .ends_here_with_len = .odd });
    dst.clues_tiles.set(.new(6, 6), .{ .ends_here_with_len = .odd });
    dst.clues_tiles.set(.new(7, 6), .{ .ends_here_with_len = .idk });
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

    if (platform.wasKeyPressedOrRetriggered(.KeyZ, 0.2)) {
        if (self.move_history.pop()) |delta| {
            self.edges.set(delta.where, !delta.added);
            self.time_of_last_move = -std.math.inf(f32);
        }
    }

    const tentacle_at = try tentacleAtFromEdges(self.edges, blocked, self.mem.frame.allocator());
    const tentacles = try tentaclesFromEdges(self.edges, self.mem.frame.allocator());

    const board: kommon.Grid2D(void) = try .initUndefined(self.mem.frame.allocator(), board_size);
    var it = board.iterator();
    while (it.next()) |pos| {
        const key: Key = .fromFormat("tile {d} {d}", .{ pos.x, pos.y });
        const rect = board.getTileRect(camera, pos);
        const mouse_over = rect.contains(mouse.cur.position) and !blocked.at2(pos);
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
        self.canvas.fillRect(camera, rect, if (pos.isEven()) .fromHex("#72FFD2") else .fromHex("#32FFD9"));
        self.canvas.fillRect(camera, rect, FColor.gray(0.5).withAlpha(hot_t * 0.4 - active_t * 0.2));
    }

    switch (self.focus) {
        .none => {
            if (mouse.wasPressed(.left)) {
                if (board.tileAt(mouse.cur.position, camera)) |tile| {
                    if (!blocked.at2(tile)) {
                        self.focus = .{ .lines = .{ .tile = tile, .which = .idk } };
                    }
                }
            }
        },
        .lines => |*l| {
            if (mouse.wasReleased(.left)) {
                self.focus = .none;
            } else if (board.tileAt(mouse.cur.position, camera)) |new_tile| {
                if (!new_tile.equals(l.tile) and !blocked.at2(new_tile)) {
                    if (kommon.grid2D.EdgePos.between(l.tile, new_tile)) |edge_pos| {
                        // if (tentacle_at.at2(new_tile) == null or tentacle_at.at2(new_tile) == tentacle_at.at2(l.tile)) {
                        if (true) {
                            const edge_ptr = self.edges.atSafePtr(edge_pos).?;
                            const old_value = edge_ptr.*;
                            switch (l.which) {
                                .idk => {
                                    l.which = if (edge_ptr.*) .erase else .draw;
                                    edge_ptr.* = !edge_ptr.*;
                                },
                                .erase => edge_ptr.* = false,
                                .draw => edge_ptr.* = true,
                            }
                            if (edge_ptr.* != old_value) {
                                try self.move_history.append(.{ .where = edge_pos, .added = edge_ptr.* });
                                self.time_of_last_move = platform.global_seconds;
                            }
                        }
                    }
                    l.tile = new_tile;
                }
            }
        },
    }

    // for (tentacles, 0..) |lst, k| {
    //     for (lst.items) |edge| {
    //         const color = FColor.fromHsv(tof32(k) / 8.0, 1.0, 1.0);
    //         const cam = camera.move(Vec2.half.neg());
    //         self.canvas.line(cam, &.{
    //             edge.pos.tof32(),
    //             edge.pos.addSigned(edge.dir).tof32(),
    //         }, 0.25, color);
    //         self.canvas.fillCircle(cam, edge.pos.tof32(), 0.3, color);
    //         self.canvas.fillCircle(cam, edge.pos.addSigned(edge.dir).tof32(), 0.25, color);
    //     }
    // }

    // for (0..board_size.x + 1) |k| {
    //     self.canvas.line(camera, &.{ .new(tof32(k), 0), .new(tof32(k), board_size.y) }, 0.02, .black);
    // }
    // for (0..board_size.y + 1) |k| {
    //     self.canvas.line(camera, &.{ .new(0, tof32(k)), .new(board_size.y, tof32(k)) }, 0.02, .black);
    // }

    var edge_it = self.edges.iterator();
    while (edge_it.next()) |edge| {
        if (tentacle_at.at2(edge.pos) != null or tentacle_at.at2(edge.nextPos()) != null) continue;
        const has_line = self.edges.at(edge);
        if (has_line) {
            self.canvas.line(camera, &.{
                edge.pos.tof32().add(.half),
                edge.pos.addSigned(edge.dir).tof32().add(.half),
            }, 0.3, octopus_color);
        }
    }

    for (tentacles) |tentacle| {
        for (tentacle.items, 0..) |edge, k| {
            const dist_to_tip = tentacle.items.len - k;
            const color: FColor = if (@mod(dist_to_tip, 2) == 0) octopus_color_2 else octopus_color;
            var spot_pos = edge.middle();
            if (k == 0) {
                const end = edge.nextPos().tof32();
                const start = octopus_pos.tof32().add(.half);
                const middle_1 = Vec2.lerp(
                    edge.pos.tof32(),
                    .lerp(start, end, 0.4),
                    0.5,
                );
                const middle_2 = Vec2.lerp(
                    edge.pos.tof32(),
                    .lerp(start, end, 0.8),
                    0.5,
                );
                self.canvas.line(camera, &.{
                    start.add(.half),
                    middle_1.add(.half),
                    middle_2.add(.half),
                    end.add(.half),
                }, 0.3, color);
                spot_pos = .lerp(middle_2, end, 0.3);
            } else {
                self.canvas.line(camera, &.{
                    edge.pos.tof32().add(.half),
                    edge.pos.addSigned(edge.dir).tof32().add(.half),
                }, 0.3, color);
                self.canvas.fillCircle(camera, edge.pos.tof32().add(.half), 0.15, color);
            }
            self.canvas.fillCircle(camera, edge.nextPos().tof32().add(.half), 0.15, color);
            switch (@mod(dist_to_tip, 3)) {
                1 => {},
                2 => {},
                0 => self.canvas.fillCircle(camera, spot_pos.add(.half), 0.15, spot_color),
                else => unreachable,
            }
        }
    }

    // self.canvas.fillRect(camera, (Rect{ .top_left = octopus_pos.tof32(), .size = .both(2) }).plusMargin(-0.1), octopus_color);
    self.canvas.fillCircle(camera, octopus_pos.add(.one).tof32(), 0.7, octopus_color_body);

    // for (tentacles, 0..) |tentacle, k| {
    //     const len = try std.fmt.allocPrint(self.mem.frame.allocator(), "{d}", .{tentacle.items.len});
    //     try self.canvas.drawTextLine(
    //         0,
    //         camera,
    //         .{ .center = octopus_pos.add(.one).tof32().add(Vec2.fromTurns((5.5 + tof32(k)) / 8.0).scale(0.75)) },
    //         len,
    //         0.4,
    //         .black,
    //     );
    // }

    it.reset();
    while (it.next()) |pos| {
        if (self.clues_tiles.at2(pos)) |clue| {
            switch (clue) {
                .starts_here_with_len, .ends_here_with_len => |c| try self.canvas.drawTextLine(
                    0,
                    camera,
                    .{ .center = pos.tof32().add(.half) },
                    switch (c) {
                        .exact => |v| "0123456789ABCD"[v .. v + 1],
                        .odd => ".",
                        .even => ":",
                        .idk => "?",
                    },
                    1,
                    if (clue.isSatisfied(pos, tentacles)) .green else .red,
                ),
            }
        }
    }

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
