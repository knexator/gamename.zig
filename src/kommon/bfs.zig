/// T should have:
/// - .init : T
/// - .Context
/// - fn isSolved(T) bool
/// - fn nextMoves(T) Iterator(T)
pub fn BFS(T: type) type {
    return struct {
        pub fn isSolvable(gpa: std.mem.Allocator) !bool {
            var visited: std.HashMap(T, void, T.Context, std.hash_map.default_max_load_percentage) = .init(gpa);
            defer visited.deinit();
            var queue: std.fifo.LinearFifo(T, .Dynamic) = .init(gpa);
            defer queue.deinit();

            try visited.putNoClobber(.init, {});
            try queue.writeItem(.init);

            while (queue.readItem()) |state| {
                var it = state.nextMoves();
                while (it.next()) |next_state| {
                    const asdf = try visited.getOrPut(next_state);
                    if (!asdf.found_existing) {
                        if (next_state.isSolved()) return true;
                        try queue.writeItem(next_state);
                    }
                }
            }

            return false;
        }

        pub fn findSolution(gpa: std.mem.Allocator) !?[]T {
            var visited: std.HashMap(T, void, T.Context, std.hash_map.default_max_load_percentage) = .init(gpa);
            defer visited.deinit();
            var queue: std.fifo.LinearFifo(T, .Dynamic) = .init(gpa);
            defer queue.deinit();
            var came_from: std.HashMap(T, T, T.Context, std.hash_map.default_max_load_percentage) = .init(gpa);
            defer came_from.deinit();

            try visited.putNoClobber(T.init, {});
            try queue.writeItem(T.init);

            while (queue.readItem()) |state| {
                var it = state.nextMoves();
                while (it.next()) |next_state| {
                    const asdf = try visited.getOrPut(next_state);
                    if (!asdf.found_existing) {
                        if (next_state.isSolved()) {
                            var reversed_path: std.ArrayList(T) = .init(gpa);
                            try reversed_path.append(next_state);
                            try reversed_path.append(state);
                            var cur = state;
                            while (came_from.get(cur)) |prev| {
                                try reversed_path.append(prev);
                                cur = prev;
                            }
                            const result = try reversed_path.toOwnedSlice();
                            std.mem.reverse(T, result);
                            return result;
                        }
                        try queue.writeItem(next_state);
                        try came_from.putNoClobber(next_state, state);
                    }
                }
            }

            return null;
        }
    };
}

const std = @import("std");

test "chess puzzle" {
    const kommon = @import("kommon.zig");
    const IVec2 = kommon.math.IVec2;

    const GameState = struct {
        const Tile = enum { hole, wall, rook, bishop, pawn, horse };
        state: [5][4]Tile,
        hole_pos: IVec2,

        const GameState = @This();

        pub const init: GameState = .{ .state = .{
            .{ .wall, .wall, .wall, .horse },
            .{ .wall, .wall, .wall, .hole },
            .{ .bishop, .rook, .bishop, .rook },
            .{ .rook, .bishop, .rook, .bishop },
            .{ .pawn, .pawn, .pawn, .pawn },
        }, .hole_pos = .new(3, 1) };

        pub const Context = std.hash_map.AutoContext(GameState);

        pub fn isSolved(self: GameState) bool {
            return self.getAt(.new(1, 2)) == .horse;
        }

        const NextMovesIterator = struct {
            state: GameState,
            checked_horse: bool = false,
            dirs: kommon.itertools.ForEachIterator(IVec2) = .{ .index = 0, .buffer = &[8]IVec2{
                .new(1, 0),
                .new(1, 1),
                .new(0, 1),
                .new(-1, 1),
                .new(-1, 0),
                .new(-1, -1),
                .new(0, -1),
                .new(1, -1),
            } },

            pub fn next(self: *NextMovesIterator) ?GameState {
                if (!self.checked_horse) {
                    self.checked_horse = true;
                    return self.state.getStateAfterMovingHorse() orelse self.next();
                }
                if (self.dirs.next()) |delta| {
                    return self.state.getStateAfterMoveHoleTowards(delta) orelse self.next();
                }
                return null;
            }
        };
        pub fn nextMoves(self: GameState) NextMovesIterator {
            return .{ .state = self };
        }

        pub fn format(
            self: @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            std.debug.assert(std.meta.eql(options, .{}));
            std.debug.assert(std.mem.eql(u8, fmt, ""));

            for (self.state) |row| {
                for (row) |tile| {
                    try writer.writeAll(switch (tile) {
                        .wall => " ",
                        .hole => "O",
                        .rook => "#",
                        .bishop => "X",
                        .pawn => "A",
                        .horse => "?",
                    });
                }
                try writer.writeAll("\n");
            }
        }

        fn getStateAfterMoveHoleTowards(self: GameState, delta: IVec2) ?GameState {
            const pos = self.hole_pos.add(delta);
            const piece_to_move = self.getAt(pos);
            return switch (piece_to_move) {
                .wall => null,
                .hole => unreachable,
                .horse => null,
                .rook => if (delta.x == 0 or delta.y == 0)
                    self.withHoleAt(pos)
                else
                    null,
                .bishop => if (delta.x == 0 or delta.y == 0)
                    null
                else
                    self.withHoleAt(pos),
                .pawn => if (delta.equals(.new(0, 1)))
                    self.withHoleAt(pos)
                else
                    null,
            };
        }

        fn getStateAfterMovingHorse(self: GameState) ?GameState {
            for ([8]IVec2{
                .new(2, -1),
                .new(2, 1),
                .new(1, 2),
                .new(-1, 2),
                .new(-2, -1),
                .new(-2, -1),
                .new(1, -2),
                .new(-1, -2),
            }) |delta| {
                const maybe_horse_pos = self.hole_pos.add(delta);
                if (self.getAt(maybe_horse_pos) == .horse) {
                    return self.withHoleAt(maybe_horse_pos);
                }
            }
            return null;
        }

        fn withHoleAt(self: GameState, pos: IVec2) GameState {
            var result = self;
            const old_hole_pos = self.hole_pos;
            const old_value = self.getAt(pos);
            result.state[@intCast(old_hole_pos.y)][@intCast(old_hole_pos.x)] = old_value;
            result.state[@intCast(pos.y)][@intCast(pos.x)] = .hole;
            result.hole_pos = pos;
            result.sanityCheck();
            return result;
        }

        fn getAt(self: GameState, pos: IVec2) Tile {
            return if (kommon.math.inRange(pos.x, 0, 4) and kommon.math.inRange(pos.y, 0, 5))
                self.state[@intCast(pos.y)][@intCast(pos.x)]
            else
                .wall;
        }

        fn sanityCheck(self: GameState) void {
            std.debug.assert(self.getAt(self.hole_pos) == .hole);
        }
    };

    const path = try BFS(GameState).findSolution(std.testing.allocator);
    defer std.testing.allocator.free(path.?);
    try std.testing.expectEqual(GameState.init, path.?[0]);
    try std.testing.expect(path.?[path.?.len - 1].isSolved());
}
