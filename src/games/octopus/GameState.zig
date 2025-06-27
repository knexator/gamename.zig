pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: mark edge cross

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "octopus",
        .author = "knexator",
        .desired_aspect_ratio = 3.0 / 4.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const EDITING = true;

const LevelInfo = struct {
    board_size: UVec2 = .both(8),
    octopus_pos: UVec2 = .new(3, 3),
    clues: []const struct { pos: UVec2, clue: TileClue },
    blocks: []const UVec2,
};

const level_infos: []const LevelInfo = &.{
    .{
        .clues = &.{},
        .blocks = &.{
            .new(1, 0),
            .new(5, 0),
            .new(4, 1),
            .new(7, 1),
            .new(0, 2),
            .new(7, 3),
            .new(0, 4),
            .new(6, 5),
            .new(0, 6),
            .new(2, 7),
            .new(5, 7),
            .new(7, 7),
        },
    },
    // .{
    //     .clues = &.{
    //         .{ .pos = .new(3, 2), .clue = .{ .starts_here_with_len = .{ .exact = 4 } } },
    //         .{ .pos = .new(4, 2), .clue = .{ .starts_here_with_len = .{ .exact = 5 } } },
    //         .{ .pos = .new(5, 3), .clue = .{ .starts_here_with_len = .{ .exact = 6 } } },
    //         .{ .pos = .new(5, 4), .clue = .{ .starts_here_with_len = .{ .exact = 7 } } },
    //         .{ .pos = .new(4, 5), .clue = .{ .starts_here_with_len = .{ .exact = 8 } } },
    //         .{ .pos = .new(3, 5), .clue = .{ .starts_here_with_len = .{ .exact = 9 } } },
    //         .{ .pos = .new(2, 4), .clue = .{ .starts_here_with_len = .{ .exact = 10 } } },
    //         .{ .pos = .new(2, 3), .clue = .{ .starts_here_with_len = .{ .exact = 11 } } },
    //         .{ .pos = .new(7, 4), .clue = .{ .ends_here_with_len = .{ .exact = 6 } } },
    //         .{ .pos = .new(6, 7), .clue = .{ .ends_here_with_len = .{ .exact = 7 } } },
    //         .{ .pos = .new(1, 2), .clue = .{ .ends_here_with_len = .{ .exact = 10 } } },
    //         .{ .pos = .new(6, 1), .clue = .{ .ends_here_with_len = .{ .exact = 11 } } },
    //     },
    //     .blocks = &.{},
    // },
    .{
        .clues = &.{
            .{ .pos = .new(2, 3), .clue = .{ .starts_here_with_len = .{ .exact = 15 } } },
            .{ .pos = .new(5, 3), .clue = .{ .starts_here_with_len = .{ .exact = 15 } } },
            .{ .pos = .new(1, 1), .clue = .{ .ends_here_with_len = .{ .exact = 15 } } },
            .{ .pos = .new(6, 1), .clue = .{ .ends_here_with_len = .{ .exact = 15 } } },
            .{ .pos = .new(5, 4), .clue = .{ .starts_here_with_len = .{ .exact = 9 } } },
            .{ .pos = .new(3, 5), .clue = .{ .starts_here_with_len = .{ .exact = 9 } } },
            .{ .pos = .new(1, 6), .clue = .{ .ends_here_with_len = .{ .exact = 9 } } },
        },
        .blocks = &.{},
    },
    .{
        .clues = &.{
            .{ .pos = .new(2, 3), .clue = .{ .starts_here_with_len = .{ .exact = 8 } } },
            .{ .pos = .new(3, 5), .clue = .{ .starts_here_with_len = .{ .exact = 8 } } },
            .{ .pos = .new(1, 6), .clue = .{ .ends_here_with_len = .{ .exact = 8 } } },
            .{ .pos = .new(1, 7), .clue = .{ .ends_here_with_len = .{ .exact = 8 } } },
            .{ .pos = .new(1, 1), .clue = .{ .ends_here_with_len = .{ .exact = 6 } } },
            .{ .pos = .new(6, 1), .clue = .{ .ends_here_with_len = .{ .exact = 5 } } },
            .{ .pos = .new(0, 4), .clue = .{ .ends_here_with_len = .odd } },
            .{ .pos = .new(6, 6), .clue = .{ .ends_here_with_len = .odd } },
            .{ .pos = .new(7, 6), .clue = .{ .ends_here_with_len = .idk } },
        },
        .blocks = &.{},
    },
};

const LevelState = struct {
    board_size: UVec2 = .both(8),
    octopus_pos: UVec2 = .new(3, 3),
    edges: kommon.Grid2DEdges(bool),
    clues_tiles: kommon.Grid2D(?TileClue),
    blocked: kommon.Grid2D(bool),
    move_history: std.ArrayList(DeltaMove),
    time_of_last_move: f32 = -std.math.inf(f32),

    fn init(dst: *LevelState, info: LevelInfo, old_edges: ?kommon.Grid2DEdges(bool), mem: *Mem) !void {
        const alloc = mem.level.allocator();
        if (old_edges != null) assert(old_edges.?.size.equals(info.board_size));
        const board_size = info.board_size;
        dst.* = .{
            .board_size = info.board_size,
            .octopus_pos = info.octopus_pos,
            .edges = old_edges orelse try .initFill(mem.forever.allocator(), board_size, false),
            .clues_tiles = try .initFill(alloc, board_size, null),
            .blocked = try .initFill(alloc, board_size, false),
            .move_history = try .initCapacity(alloc, 1),
        };
        for (starting_edges_local) |edge| {
            dst.edges.set(edge.translate(info.octopus_pos), true);
        }
        for ([4]UVec2{ .zero, .e1, .e2, .one }) |d| {
            dst.blocked.set(info.octopus_pos.add(d), true);
        }
        for (info.blocks) |p| {
            dst.blocked.set(p, true);
        }
        for (info.clues) |c| {
            dst.clues_tiles.set(c.pos, c.clue);
        }
    }

    fn isInitial(self: LevelState, pos: UVec2) bool {
        for (starting_edges_local) |e| {
            if (e.translate(self.octopus_pos).nextPos().equals(pos)) return true;
        } else return false;
    }

    fn undo(self: *LevelState) void {
        if (self.move_history.pop()) |delta| {
            self.edges.set(delta.where, !delta.added);
            self.time_of_last_move = -std.math.inf(f32);
        }
    }

    fn allTilesVisited(self: LevelState, scratch: std.mem.Allocator) !bool {
        const info = try infoFromEdges(self.edges, self.blocked, self.board_size, self.octopus_pos, scratch);
        var it = self.blocked.iterator();
        while (it.next()) |p| {
            if (self.blocked.at2(p)) continue;
            if (isBody(self.octopus_pos, p)) continue;
            if (info.tentacle_at.at2(p) == null) return false;
        } else return true;
    }

    fn solved(self: LevelState, scratch: std.mem.Allocator) !bool {
        const info = try infoFromEdges(self.edges, self.blocked, self.board_size, self.octopus_pos, scratch);
        if (info.all_tentacles.len != 8) return false;
        var it = self.blocked.iterator();
        while (it.next()) |p| {
            if (self.blocked.at2(p)) continue;
            if (isBody(self.octopus_pos, p)) continue;
            if (info.tentacle_at.at2(p) == null) return false;
            if (self.clues_tiles.at2(p)) |clue| {
                if (!clue.isSatisfied(p, info.all_tentacles)) return false;
            }
        } else return true;
    }
};

const clue_solved_color: FColor = .fromHex("#3F00A5");
const bg_color: FColor = .fromHex("#6FD3CA");
const grid_color_1: FColor = .fromHex("#C8FCBA");
const grid_color_2: FColor = .fromHex("#DEFFB5");
const octopus_color_2: FColor = .fromHex("#AF23EF");
const octopus_color: FColor = .fromHex("#7E27AD");
const octopus_color_body: FColor = .fromHex("#D564FD");
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

state: union(enum) {
    menu: MenuState,
    loading: LoadingState,
    playing,
} = .{ .menu = .{} },

old_edges: [level_infos.len]?kommon.Grid2DEdges(bool) = @splat(null),
focus: union(enum) { none, lines: struct { tile: UVec2, which: enum { idk, draw, erase } } } = .none,
level_state: LevelState,
level_index: usize,
visual_tentacles_distance: [8]f32 = @splat(1.0),
// visual_tentacles: [8]std.ArrayList(TentacleVisualPoint),
// visual_tentacles: [8]std.ArrayList(Vec2),
const VISUAL_POINTS_PER_SEGMENT = 4;

const MenuState = struct {
    const level_positions: []const Vec2 = &.{ .new(1, 1), .new(2, 1), .new(3, 1) };

    pub fn update(game: *GameState, platform: PlatformGives, loading_t: ?f32) !bool {
        const camera = (Rect{ .top_left = .new(0, (loading_t orelse 0) * -5), .size = .both(4) }).withAspectRatio(platform.aspect_ratio, .grow, .top_center);
        game.canvas.fillRect(camera, .{ .top_left = .zero, .size = camera.size }, bg_color);
        const mouse = platform.getMouse(camera);
        for (level_positions, 0..) |level_pos, level_index| {
            const level_radius = 0.3;
            const hovered = (mouse.cur.position.sub(level_pos).magSq() < level_radius * level_radius) and loading_t == null;

            game.canvas.fillCircle(camera, level_pos, level_radius, try game.smooth.fcolor(
                .fromFormat("bg {d}", .{level_index}),
                if (hovered) .white else .black,
            ));
            try game.canvas.drawTextLine(
                0,
                camera,
                .{ .center = level_pos },
                try std.fmt.allocPrint(game.mem.frame.allocator(), "{d}", .{level_index}),
                level_radius * 1.5,
                try game.smooth.fcolor(
                    .fromFormat("text {d}", .{level_index}),
                    if (hovered) .black else .white,
                ),
            );

            if (hovered and mouse.wasPressed(.left)) {
                game.state = .{ .loading = try LoadingState.init(level_index, game) };
            }
        }
        // _ = game;
        // const camera = Rect.from
        //     .withAspectRatio(platform.aspect_ratio, .grow, .bottom_center);
        // const mouse = platform.getMouse(camera);
        // for ([_]KeyboardButton{ .Digit1, .Digit2 }, 0..) |key, k| {
        //     if (platform.keyboard.wasPressed(key)) {
        //         game.state = .{ .loading = try LoadingState.init(k, game) };
        //     }
        // }

        return false;
    }
};

const LoadingState = struct {
    t: f32,
    // false if going back to menu
    loading: bool,

    pub fn init(target_index: usize, game: *GameState) !LoadingState {
        try game.level_state.init(level_infos[target_index], game.old_edges[target_index], &game.mem);
        game.level_index = target_index;
        return .{ .t = 0, .loading = true };
    }

    pub fn toMenu() LoadingState {
        return .{ .t = 1, .loading = false };
    }

    pub fn update(self: *LoadingState, game: *GameState, platform: PlatformGives) !bool {
        // platform.gl.clear(bg_color);
        _ = try updateGame(game, platform);
        const board_size = game.level_state.board_size;
        const board_rect: Rect = .{ .top_left = .zero, .size = board_size.tof32() };
        const camera = board_rect.withAspectRatio(3.0 / 4.0, .grow, .top_center).withAspectRatio(platform.aspect_ratio, .grow, .top_center);
        const t = math.easings.easeInOutCubic(self.t);
        game.canvas.fillRect(camera, camera.move(.new(0, tof32(board_size.y) * t)), bg_color);
        _ = try MenuState.update(game, platform, t);

        if (self.loading) {
            math.towards(&self.t, 1, 2.0 * platform.delta_seconds);
            if (self.t >= 1) {
                game.state = .playing;
            }
        } else {
            math.towards(&self.t, 0, 2.0 * platform.delta_seconds);
            if (self.t <= 0) {
                game.old_edges[game.level_index] = game.level_state.edges;
                game.state = .{ .menu = .{} };
            }
        }
        return false;
    }
};

// TODO: move focus/level_state/visual_tentacles_distance here
// const PlayingState = struct {
//     pub fn draw(game: *GameState, platform: PlatformGives) !void {}
// };

const TentacleVisualPoint = struct { pos: Vec2, dist_to_base: f32 };

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

    pub fn isSatisfied(clue: TileClue, pos: UVec2, tentacles: []const []const EdgePos) bool {
        switch (clue) {
            .starts_here_with_len => |l| for (tentacles) |tentacle| {
                if (tentacle[0].nextPos().equals(pos)) return l.fits(tentacle.len);
            } else return false,
            .ends_here_with_len => |l| for (tentacles) |tentacle| {
                if (tentacle[tentacle.len - 1].nextPos().equals(pos)) return l.fits(tentacle.len);
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
    dst.mem = .init(gpa);
    try dst.level_state.init(level_infos[1], null, &dst.mem);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("../../fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.smooth = .init(gpa);
    // dst.visual_tentacles = undefined;
    // for (&dst.visual_tentacles, starting_edges_local) |*t, s| {
    //     t.* = try .initCapacity(dst.mem.level.allocator(), 20 * VISUAL_POINTS_PER_SEGMENT);
    //     const edge = s.translate(dst.level_state.octopus_pos);
    //     const end = edge.nextPos().tof32();
    //     const start = dst.level_state.octopus_pos.tof32().add(.half);
    //     const middle_1 = Vec2.lerp(
    //         edge.pos.tof32(),
    //         .lerp(start, end, 0.4),
    //         0.5,
    //     );
    //     const middle_2 = Vec2.lerp(
    //         edge.pos.tof32(),
    //         .lerp(start, end, 0.8),
    //         0.5,
    //     );
    //     try t.*.appendSlice(&.{
    //         .{ .pos = end.add(.half), .dist_to_base = 1.0 },
    //         .{ .pos = middle_2.add(.half), .dist_to_base = 2.0 / 3.0 },
    //         .{ .pos = middle_1.add(.half), .dist_to_base = 1.0 / 3.0 },
    //         .{ .pos = start.add(.half), .dist_to_base = 0.0 },
    //     });
    // }
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
    return switch (self.state) {
        .menu => MenuState.update(self, platform, null),
        .loading => |*l| l.update(self, platform),
        .playing => updateGame(self, platform),
    };
}

fn updateGame(self: *GameState, platform: PlatformGives) !bool {
    const board_size = self.level_state.board_size;

    const board_rect: Rect = .{ .top_left = .zero, .size = board_size.tof32() };

    const camera = board_rect.withAspectRatio(3.0 / 4.0, .grow, .top_center).withAspectRatio(platform.aspect_ratio, .grow, .top_center);
    const mouse = platform.getMouse(camera);
    platform.gl.clear(bg_color);

    defer if (platform.keyboard.wasPressed(.Escape)) {
        self.state = .{ .loading = .toMenu() };
    };

    if (platform.wasKeyPressedOrRetriggered(.KeyZ, 0.2)) {
        self.level_state.undo();
    }

    const octopus_pos = self.level_state.octopus_pos;
    const edges = &self.level_state.edges;
    for (starting_edges_local) |s| {
        edges.set(s.translate(octopus_pos), true);
    }
    const blocked = self.level_state.blocked;
    const info = try infoFromEdges(edges.*, blocked, board_size, octopus_pos, self.mem.frame.allocator());
    const tentacles = info.all_tentacles[0..8];
    const tentacle_at = info.tentacle_at;

    const board: kommon.Grid2D(void) = try .initUndefined(self.mem.frame.allocator(), board_size);
    var it = board.iterator();
    while (it.next()) |pos| {
        const key: Key = .fromFormat("tile {d} {d}", .{ pos.x, pos.y });
        const rect = board.getTileRect(board_rect, pos);
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
        self.canvas.fillRect(camera, rect, if (pos.isEven()) grid_color_1 else grid_color_2);
        self.canvas.fillRect(camera, rect, FColor.gray(0.5).withAlpha(hot_t * 0.4 - active_t * 0.2));

        if (blocked.at2(pos) and !isBody(octopus_pos, pos)) {
            self.canvas.fillRectWithRoundCorners(camera, rect.plusMargin(-0.1), 0.1, .gray(0.5));
        }
    }

    switch (self.focus) {
        .none => {
            if (mouse.wasPressed(.left)) {
                if (board.tileAt(mouse.cur.position, board_rect)) |tile| {
                    if (!blocked.at2(tile)) {
                        self.focus = .{ .lines = .{ .tile = tile, .which = .idk } };
                    }
                }
            }
        },
        .lines => |*l| {
            if (mouse.wasReleased(.left)) {
                self.focus = .none;
            } else if (board.tileAt(mouse.cur.position, board_rect)) |new_tile| {
                if (!new_tile.equals(l.tile) and !blocked.at2(new_tile)) {
                    if (EdgePos.between(l.tile, new_tile)) |edge_pos| {
                        const edge_ptr = edges.atSafePtr(edge_pos).?;
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
                            try self.level_state.move_history.append(.{ .where = edge_pos, .added = edge_ptr.* });
                            self.level_state.time_of_last_move = platform.global_seconds;
                            // when adding an edge, check that it's all still valid
                            if (edge_ptr.* and self.level_state.isInitial(l.tile) and self.level_state.isInitial(new_tile)) {
                                _ = self.level_state.move_history.pop();
                                edge_ptr.* = false;
                            } else if (edge_ptr.*) {
                                const illegal_connection: bool = blk: {
                                    if (tentacle_at.at2(l.tile) == null) break :blk false;
                                    if (tentacle_at.at2(new_tile) == null) break :blk false;
                                    const id_1 = tentacle_at.at2(l.tile).?.id;
                                    const id_2 = tentacle_at.at2(new_tile).?.id;
                                    if (id_1 == id_2) break :blk true;
                                    if (id_1 < 8 and id_2 < 8) break :blk true;
                                    break :blk false;
                                };

                                edge_ptr.* = false;
                                defer edge_ptr.* = true;
                                var edges_to_remove: std.ArrayList(EdgePos) = .init(self.mem.frame.allocator());

                                inline for (&.{ l.tile, new_tile }) |p| {
                                    var local_edges = activeEdgesAround(edges.*, p);
                                    assert(local_edges.len <= 2);
                                    if (illegal_connection or local_edges.len == 2) {
                                        for (local_edges.constSlice()) |e| {
                                            try edges_to_remove.append(e);
                                        }
                                    }
                                }

                                for (edges_to_remove.items) |edge| {
                                    assert(edges.at(edge));
                                    try self.level_state.move_history.append(.{ .where = edge, .added = false });
                                    edges.set(edge, false);
                                }
                            }
                        }
                    }
                    l.tile = new_tile;
                }
            }
        },
    }

    // for (0..board_size.x + 1) |k| {
    //     self.canvas.line(camera, &.{ .new(tof32(k), 0), .new(tof32(k), board_size.y) }, 0.02, .black);
    // }
    // for (0..board_size.y + 1) |k| {
    //     self.canvas.line(camera, &.{ .new(0, tof32(k)), .new(board_size.y, tof32(k)) }, 0.02, .black);
    // }

    for (info.all_tentacles[8..]) |tentacle| {
        for (tentacle) |edge| {
            const color = octopus_color;
            self.canvas.line(camera, &.{
                edge.pos.tof32().add(.half),
                edge.pos.addSigned(edge.dir).tof32().add(.half),
            }, 0.3, color);
            self.canvas.fillCircle(camera, edge.pos.tof32().add(.half), 0.15, color);
            self.canvas.fillCircle(camera, edge.nextPos().tof32().add(.half), 0.15, color);
        }
    }

    for (&self.visual_tentacles_distance, tentacles) |*visual_tentacle_distance, real_tentacle| {
        math.towards(visual_tentacle_distance, tof32(real_tentacle.len), @abs(visual_tentacle_distance.* - tof32(real_tentacle.len)) - 2);
        math.towards(visual_tentacle_distance, tof32(real_tentacle.len), platform.delta_seconds * 16.0);
        // math.lerp_towards(visual_tentacle_distance, tof32(real_tentacle.len), 0.6, platform.delta_seconds);
    }

    // for (&self.visual_tentacles, tentacles) |*visual_tentacle, real_tentacle| {
    //     const missing_dist = tof32(real_tentacle.len) - visual_tentacle.items[0].dist_to_base;
    //     if (missing_dist > 0) {
    //         const delta_dist = @min(missing_dist, platform.delta_seconds);
    //         for (visual_tentacle.items) |*p| {
    //             p.dist_to_base += delta_dist;
    //             p.pos = tentaclePosAt(real_tentacle, p.dist_to_base);
    //         }
    //     }
    //     // for (visual_tentacle.items[1..], 0..) |*v, k| {
    //     // }
    // }

    // for (&self.visual_tentacles, tentacles) |*visual_tentacle, real_tentacle| {
    //     while (visual_tentacle.items.len < VISUAL_POINTS_PER_SEGMENT * real_tentacle.len) {
    //         try visual_tentacle.append(octopus_pos.tof32().add(.one));
    //     }
    //     for (visual_tentacle.items, 0..) |*v, k| {
    //         const dist_to_tip = tof32(k) / VISUAL_POINTS_PER_SEGMENT;
    //         const dist_to_base = tof32(real_tentacle.len) - dist_to_tip;
    //         const target_pos: Vec2 = if (dist_to_base <= 0)
    //             octopus_pos.tof32()
    //         else if (dist_to_base <= 1) blk: {
    //             const edge = real_tentacle[0];
    //             const end = edge.nextPos().tof32();
    //             const start = octopus_pos.tof32().add(.half);
    //             const middle_1 = Vec2.lerp(
    //                 edge.pos.tof32(),
    //                 .lerp(start, end, 0.4),
    //                 0.5,
    //             );
    //             const middle_2 = Vec2.lerp(
    //                 edge.pos.tof32(),
    //                 .lerp(start, end, 0.8),
    //                 0.5,
    //             );
    //             if (real_tentacle.ptr == tentacles[0].ptr) {
    //                 // std.log.debug("start: {any}\nend: {any}\ndist: {d}", .{ start, end, dist_to_base });
    //             }
    //             // TODO: improve
    //             break :blk if (dist_to_base < 1.0 / 3.0)
    //                 .lerp(start, middle_1, dist_to_base / 3.0)
    //             else if (dist_to_base < 2.0 / 3.0)
    //                 .lerp(middle_1, middle_2, (dist_to_base - 1.0 / 3.0) / 3.0)
    //             else
    //                 end;
    //             // TODO: investigate
    //             // .lerp(middle_2, end, (dist_to_base - 2.0 / 3.0) / 3.0);
    //         } else blk: {
    //             const edge = real_tentacle[@intFromFloat(@floor(dist_to_base - 0.001))];
    //             break :blk edge.pos.tof32().add(edge.dir.tof32().scale(dist_to_base - @floor(dist_to_base - 0.001)));
    //         };
    //         Vec2.towards(v, target_pos.add(.half), 8.0 * platform.delta_seconds);
    //     }
    // }

    var all_spots: std.ArrayList(Vec2) = .init(self.mem.scratch.allocator());
    for (self.visual_tentacles_distance, tentacles) |visual_tentacle_distance, real_tentacle| {
        var a = @min(tof32(real_tentacle.len), visual_tentacle_distance);
        const SPACING = 0.1;
        while (a >= SPACING) : (a -= SPACING) {
            const dist_to_tip = visual_tentacle_distance - a;
            if (a > tof32(real_tentacle.len)) continue;
            const a_pos = tentaclePosAt(real_tentacle, a, octopus_pos);
            const b_pos = tentaclePosAt(real_tentacle, a - SPACING, octopus_pos);
            const color: FColor = (if (@mod(@as(isize, @intFromFloat(@floor(dist_to_tip))), 2) == 0) octopus_color_2 else octopus_color);
            self.canvas.line(camera, &.{ a_pos, b_pos }, 0.3, color);
            self.canvas.fillCircle(camera, a_pos, 0.15, color);
            self.canvas.fillCircle(camera, b_pos, 0.15, color);

            if (@abs(@mod(dist_to_tip, 3) - 2.25) < SPACING / 2.0) {
                try all_spots.append(a_pos);
            }
        }
        // p.pos = tentaclePosAt(real_tentacle, p.dist_to_base);
    }
    for (all_spots.items) |p| self.canvas.fillCircle(camera, p, 0.15, spot_color);

    // for (self.visual_tentacles) |visual_tentacle| {
    //     for (visual_tentacle.items[0 .. visual_tentacle.items.len - 1], visual_tentacle.items[1..], 0..) |a, b, k| {
    //         // const color: FColor = octopus_color;
    //         const color: FColor = (if (@mod(@as(isize, @intFromFloat(@floor(tof32(k) / VISUAL_POINTS_PER_SEGMENT - 0.001))), 2) == 0) octopus_color_2 else octopus_color);
    //         // const color: FColor = (if (@mod(k, 2) == 0) octopus_color_2 else octopus_color);
    //         self.canvas.line(camera, &.{ a.pos, b.pos }, 0.3, color);
    //         // self.canvas.line(camera, &.{ a, b }, 0.3, color);
    //         self.canvas.fillCircle(camera, a.pos, 0.15, color);
    //         self.canvas.fillCircle(camera, b.pos, 0.15, color);
    //     }
    // }

    if (false) for (tentacles) |tentacle| {
        for (tentacle, 0..) |edge, k| {
            const dist_to_tip = tentacle.len - k;
            const color: FColor = (if (@mod(dist_to_tip, 2) == 0) octopus_color_2 else octopus_color).withAlpha(0.1);
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
    };

    // self.canvas.fillRect(camera, (Rect{ .top_left = octopus_pos.tof32(), .size = .both(2) }).plusMargin(-0.1), octopus_color_body);
    // self.canvas.fillCircle(camera, octopus_pos.add(.one).tof32(), 0.7, octopus_color_body);
    // self.canvas.fillRect(camera, (Rect{ .top_left = octopus_pos.tof32(), .size = .both(2) }).plusMargin(-0.1), octopus_color_body);
    self.canvas.fillRectWithRoundCorners(camera, (Rect{ .top_left = octopus_pos.tof32(), .size = .both(2) }).plusMargin(-0.15), 0.2, octopus_color_body);
    if (try self.level_state.solved(self.mem.scratch.allocator())) {
        self.canvas.line(camera, &funk.mapOOP(octopus_pos.tof32().add(.one), .add, &.{
            .new(-0.2, 0.6),
            .new(-0.15, 0.65),
            .new(-0.1, 0.68),
            .new(-0.05, 0.69),
            .new(0, 0.695),
            .new(0.05, 0.69),
            .new(0.1, 0.68),
            .new(0.15, 0.65),
            .new(0.2, 0.6),
        }), 0.05, octopus_color);
    }
    for ([2]Vec2{ .new(-0.3, 0.4), .new(0.35, 0.4) }) |p| {
        const eye_center = octopus_pos.add(.one).tof32().add(p);
        const r1 = 0.22;
        const r2 = 0.15;
        self.canvas.fillCircle(camera, eye_center, r1, .white);
        self.canvas.fillCircle(camera, eye_center.towardsPure(mouse.cur.position, r1 - r2), r2, .black);
    }

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
        if (self.level_state.clues_tiles.at2(pos)) |clue| {
            const color: FColor = if (clue.isSatisfied(pos, tentacles)) clue_solved_color else .black;
            switch (clue) {
                .starts_here_with_len, .ends_here_with_len => |c| switch (c) {
                    .even => @panic("TODO"),
                    .odd => for (0..3) |k| {
                        self.canvas.fillCircle(camera, pos.tof32().add(.half).add(.fromPolar(0.3, 1.0 / 12.0 + tof32(k) / 3.0)), 0.1, color);
                    },
                    else => try self.canvas.drawTextLine(
                        0,
                        camera,
                        .{ .center = pos.tof32().add(.half) },
                        switch (c) {
                            .exact => |v| try std.fmt.allocPrint(self.mem.frame.allocator(), "{d}", .{v}),
                            .odd, .even => unreachable,
                            .idk => "?",
                        },
                        1.0,
                        color,
                    ),
                },
            }
        }
    }

    const extra_panel_rect: Rect = .from(.{
        .{ .top_center = board_rect.get(.bottom_center) },
        .{ .size = camera.get(.size).addY(-board_rect.size.x) },
    });
    if (self.level_state.clues_tiles.tileAt(mouse.cur.position, board_rect)) |pos| {
        if (self.level_state.clues_tiles.at2(pos)) |clue| {
            switch (clue) {
                .starts_here_with_len, .ends_here_with_len => |c| {
                    // const text = std.fmt.allocPrint(self.mem.frame.allocator(), "with odd length", args: anytype)
                    const len_text = switch (c) {
                        .exact => |v| try std.fmt.allocPrint(self.mem.frame.allocator(), "of length {d}", .{v}),
                        .odd => "with odd length",
                        .even => "with even length",
                        .idk => "with unknown length",
                    };
                    const lines: []const []const u8 = &.{
                        "A tentacle", len_text, switch (clue) {
                            .starts_here_with_len => "starts here.",
                            .ends_here_with_len => "ends here.",
                        },
                    };
                    try self.canvas.drawTextLines(
                        0,
                        camera,
                        .center,
                        .{ .center = extra_panel_rect.get(.center) },
                        lines,
                        0.6,
                        1.0,
                        if (clue.isSatisfied(pos, tentacles)) clue_solved_color else .fromHex("#700029"),
                    );
                },
            }
        }

        if (EDITING and mouse.wasPressed(.right)) {
            const ptr = self.level_state.blocked.getPtr(pos);
            ptr.* = !ptr.*;
        }
    }

    if (self.level_index == 0) {
        try self.canvas.drawTextLine(
            0,
            camera,
            .{ .center = extra_panel_rect.get(.center) },
            "Visit every tile.",
            0.6,
            if (try self.level_state.allTilesVisited(self.mem.scratch.allocator())) clue_solved_color else .fromHex("#700029"),
        );

        if (try self.level_state.allTilesVisited(self.mem.scratch.allocator()) and info.all_tentacles.len == 8) {
            try self.canvas.drawTextLine(
                0,
                camera,
                .{ .center = extra_panel_rect.get(.center).addY(1.0) },
                "Esc for more levels",
                0.6,
                .black,
            );
        }
    }

    // if (solved) {
    //     const button: Rect = extra_panel_rect.with(.{ .size = .new(1.75, 1) }, .bottom_center).plusMargin(-0.15);
    //     self.canvas.fillRect(camera, button, .white);
    //     try self.canvas.drawTextLine(0, camera, .{ .center = button.get(.center) }, "Back", 0.5, .black);
    //     if (button.contains(mouse.cur.position) and mouse.wasPressed(.left)) {
    //         self.state = .{ .loading = .toMenu() };
    //     }
    // }

    if (false) {
        var edge_it = self.edges.iterator();
        while (edge_it.next()) |edge| {
            if (self.edges.at(edge)) {
                self.canvas.line(camera, &.{
                    edge.pos.tof32().add(.half),
                    edge.pos.addSigned(edge.dir).tof32().add(.half),
                }, 0.2, .black);
            }
        }
    }

    return false;
}

fn isBody(octopus_pos: UVec2, pos: UVec2) bool {
    for (&[4]UVec2{ .zero, .e1, .e2, .one }) |p| {
        if (octopus_pos.add(p).equals(pos)) return true;
    } else return false;
}

fn tentaclePosAt(tentacle: []const EdgePos, dist_to_base: f32, octopus_pos: UVec2) Vec2 {
    assert(0 <= dist_to_base and dist_to_base <= tof32(tentacle.len));
    if (tof32(tentacle.len) == dist_to_base) {
        return kommon.last(tentacle).?.nextPos().tof32().add(.half);
    } else if (dist_to_base <= 1) {
        const t = dist_to_base;

        const edge = tentacle[0];
        // const start = octopus_pos.tof32().add(.one).add(.fromPolar(0.5, 0));
        const control = edge.pos.tof32().add(edge.dir.tof32().scale(0.5)).add(.half);
        const end = edge.nextPos().tof32().add(.half);
        const start = octopus_pos.tof32().add(.half).towardsPure(edge.pos.tof32(), 0.4).add(.half);

        const p1 = Vec2.lerp(start, control, t);
        const p2 = Vec2.lerp(control, end, t);

        return .lerp(p1, p2, t);
    } else {
        const edge = tentacle[@intFromFloat(@floor(dist_to_base))];
        const fract = @mod(dist_to_base, 1.0);
        return edge.pos.tof32().add(edge.dir.tof32().scale(fract)).add(.half);
    }
}

fn activeEdgesAround(edges: kommon.Grid2DEdges(bool), pos: UVec2) std.BoundedArray(EdgePos, 4) {
    var res = std.BoundedArray(EdgePos, 4).init(0) catch unreachable;
    for (IVec2.cardinal_directions) |d| {
        if (edges.atSafe(.{ .pos = pos, .dir = d }) orelse false) {
            res.append(.{ .pos = pos, .dir = d }) catch unreachable;
        }
    }
    return res;
}

const TentacleTile = struct { id: usize, dist: usize };
fn infoFromEdges(edges: kommon.Grid2DEdges(bool), blocked: kommon.Grid2D(bool), board_size: UVec2, octopus_pos: UVec2, allocator: std.mem.Allocator) !struct {
    all_tentacles: []const []const EdgePos,
    tentacle_at: kommon.Grid2D(?TentacleTile),
} {
    var all_tentacles: std.ArrayList([]const EdgePos) = .init(allocator);
    const tentacle_at: kommon.Grid2D(?TentacleTile) = try .initFill(allocator, board_size, null);
    for (starting_edges_local, 0..) |starting_edge_local, k| {
        var dst: std.ArrayList(EdgePos) = .init(allocator);
        try dst.append(starting_edge_local.translate(octopus_pos));
        main: while (true) {
            const cur_end = dst.getLast().nextPos();
            tentacle_at.set(cur_end, .{ .id = k, .dist = dst.items.len });

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
        try all_tentacles.append(try dst.toOwnedSlice());
    }
    var it = tentacle_at.iterator();
    while (it.next()) |first_pos| {
        if (blocked.at2(first_pos)) continue;
        if (tentacle_at.at2(first_pos) == null) {
            var all_tiles: std.ArrayList(EdgePos) = .init(allocator);
            for (IVec2.cardinal_directions) |dir| {
                if (edges.atSafe(.{ .pos = first_pos, .dir = dir }) orelse false) {
                    try all_tiles.append(.{ .pos = first_pos, .dir = dir });
                }
            }

            if (all_tiles.items.len > 0) {
                // TODO: proper dist
                tentacle_at.set(first_pos, .{ .id = all_tentacles.items.len, .dist = 0 });

                var pending = try all_tiles.clone();
                defer pending.deinit();
                while (pending.pop()) |cur_edge| {
                    // TODO: proper dist
                    tentacle_at.set(cur_edge.nextPos(), .{ .id = all_tentacles.items.len, .dist = 0 });
                    const cur = cur_edge.nextPos();
                    for (IVec2.cardinal_directions) |dir| {
                        if (edges.atSafe(.{ .pos = cur, .dir = dir }) orelse false) {
                            if (tentacle_at.at2(cur.addSigned(dir)) == null) {
                                try all_tiles.append(.{ .pos = cur, .dir = dir });
                                try pending.append(.{ .pos = cur, .dir = dir });
                            }
                        }
                    }
                }
                try all_tentacles.append(try all_tiles.toOwnedSlice());
            }
        }
    }
    return .{
        .all_tentacles = try all_tentacles.toOwnedSlice(),
        .tentacle_at = tentacle_at,
    };
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
