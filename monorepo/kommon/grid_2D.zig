const std = @import("std");
const assert = std.debug.assert;

const kommon = @import("kommon.zig");
const UVec2 = kommon.math.UVec2;
const IVec2 = kommon.math.IVec2;
const Rect = kommon.math.Rect;

pub fn Grid2D_Deprecated(T: type) type {
    return Grid2D(T, null);
}

pub fn Grid2D(T: type, max_size: ?UVec2) type {
    // 0 1 2
    // 3 4 5
    // 6 7 8
    return struct {
        size: UVec2,
        data: if (max_size) |s| [s.x * s.y]T else []T,

        const Self = @This();

        pub fn initUndefined(allocator: std.mem.Allocator, size: UVec2) !Self {
            if (max_size != null) @compileError("use the bounded version");
            return .{ .size = size, .data = try allocator.alloc(T, size.x * size.y) };
        }

        pub fn initFill(allocator: std.mem.Allocator, size: UVec2, fill: T) !Self {
            if (max_size != null) @compileError("use the bounded version");
            const result: Self = try .initUndefined(allocator, size);
            @memset(result.data, fill);
            return result;
        }

        pub fn initUndefinedV2(size: UVec2) Self {
            if (max_size == null) @compileError("only avaiable for bounded grid2d");
            const s = max_size.?;
            assert(size.x <= s.x and size.y <= s.y);
            return .{
                .size = size,
                .data = undefined,
            };
        }

        pub fn initFillV2(size: UVec2, fill: T) Self {
            if (max_size == null) @compileError("only avaiable for bounded grid2d");
            var result: Self = .initUndefinedV2(size);
            @memset(&result.data, fill);
            return result;
        }

        pub fn deinit(self: Self, allocator: if (max_size != null) void else std.mem.Allocator) void {
            if (max_size != null) @compileError("use the bounded version");
            allocator.free(self.data);
        }

        // TODO: remove this method
        pub fn at(self: Self, i: usize, j: usize) T {
            return self.data[self.indexOf(.new(i, j))];
        }

        pub fn at2(self: Self, pos: UVec2) T {
            return self.data[self.indexOf(pos)];
        }

        pub fn atSigned(self: Self, pos: IVec2) T {
            return self.data[self.indexOfSigned(pos)];
        }

        pub fn atSignedSafe(self: Self, pos: IVec2) ?T {
            if (!self.inBoundsSigned(pos)) return null;
            return self.data[self.indexOfSigned(pos)];
        }

        pub fn getPtr(self: if (max_size == null) Self else *Self, pos: UVec2) *T {
            return &self.data[self.indexOf(pos)];
        }

        pub fn set(self: if (max_size == null) Self else *Self, pos: UVec2, value: T) void {
            self.data[self.indexOf(pos)] = value;
        }

        pub fn setSigned(self: Self, pos: IVec2, value: T) void {
            self.data[self.indexOfSigned(pos)] = value;
        }

        fn indexOf(self: Self, pos: UVec2) usize {
            if (!self.inBoundsUnsigned(pos)) std.debug.panic("OoB: Grid2D of size {any} accessed at position {any}", .{ self.size, pos });
            return pos.y * self.size.x + pos.x;
        }

        fn indexOfSigned(self: Self, pos: IVec2) usize {
            if (!self.inBoundsSigned(pos)) std.debug.panic("OoB: Grid2D of size {any} accessed at position {any}", .{ self.size, pos });
            return self.indexOf(pos.cast(usize));
        }

        pub fn iterator(self: Self) GridIterator {
            return .init(self.size);
        }

        pub fn iteratorSigned(self: Self) GridSignedIterator {
            return .{ .grid_iterator = .init(self.size) };
        }

        const RayIterator = struct {
            grid: Self,
            pos: ?UVec2,
            dir: IVec2,

            pub fn next(self: *RayIterator) ?UVec2 {
                if (self.pos) |pos| {
                    const new_pos = pos.cast(isize).add(self.dir);
                    if (self.grid.inBoundsSigned(new_pos)) {
                        self.pos = new_pos.cast(usize);
                    } else {
                        self.pos = null;
                    }
                    return pos;
                } else return null;
            }
        };

        pub fn rayIterator(self: Self, pos: UVec2, dir: IVec2) RayIterator {
            return .{ .grid = self, .pos = pos, .dir = dir };
        }

        pub fn fromAsciiAndMap(allocator: std.mem.Allocator, ascii: []const u8, comptime map_fn: fn (v: u8) T) !Self {
            const ascii_grid: Grid2D(u8, null) = try .fromAscii(allocator, ascii);
            defer ascii_grid.deinit(allocator);
            return try ascii_grid.map(allocator, T, map_fn);
        }

        pub fn fromAscii(allocator: std.mem.Allocator, ascii: []const u8) !Self {
            if (T != u8) @compileError("fromAscii only works on Grid2D(u8)");
            // TODO: windows line endings
            var lines = std.mem.splitScalar(u8, ascii, '\n');
            const width = lines.peek().?.len;
            const height = kommon.itertools.iteratorLen(lines);
            const data = try allocator.alloc(T, width * height);
            errdefer allocator.free(data);
            var j: usize = 0;
            while (lines.next()) |line| {
                if (line.len != width) return error.NotAnAsciiRectangle;
                std.mem.copyForwards(T, data[j * width ..], line);
                j += 1;
            }
            return .{
                .size = .new(width, height),
                .data = data,
            };
        }

        pub fn fromAsciiWide(comptime N: usize, allocator: std.mem.Allocator, ascii: []const u8) !Self {
            if (T != [N]u8) @compileError("fromAsciiWide only works on Grid2D([N]u8)");
            // TODO: windows line endings
            var lines = std.mem.splitScalar(u8, ascii, '\n');
            const width = @divExact(lines.peek().?.len, N);
            const height = kommon.itertools.iteratorLen(lines);
            const data = try allocator.alloc(T, width * height);
            errdefer allocator.free(data);
            var j: usize = 0;
            while (lines.next()) |line| {
                if (line.len != width * N) return error.NotAnAsciiRectangle;
                std.mem.copyForwards(u8, std.mem.sliceAsBytes(data[j * width ..]), line);
                j += 1;
            }
            return .{
                .size = .new(width, height),
                .data = data,
            };
        }

        pub fn map(self: Self, allocator: std.mem.Allocator, comptime NewType: type, comptime map_fn: fn (v: T) NewType) !Grid2D(NewType, max_size) {
            const new_data = try allocator.alloc(NewType, self.data.len);
            for (0..self.size.y) |j| {
                for (0..self.size.x) |i| {
                    new_data[j * self.size.x + i] = map_fn(self.at(i, j));
                }
            }
            return .{
                .size = self.size,
                .data = new_data,
            };
        }

        pub fn mapWithCtx(self: Self, allocator: std.mem.Allocator, comptime NewType: type, ctx: anytype, comptime map_fn: fn (v: T, ctx: @TypeOf(ctx)) NewType) !Grid2D(NewType) {
            const new_data = try allocator.alloc(NewType, self.data.len);
            for (0..self.size.y) |j| {
                for (0..self.size.x) |i| {
                    new_data[j * self.size.x + i] = map_fn(self.at(i, j), ctx);
                }
            }
            return .{
                .size = self.size,
                .data = new_data,
            };
        }

        pub fn bounds(self: Self) ?kommon.math.URect {
            if (T != bool) @compileError("crop only works on Grid2D(bool)");

            const left: usize = blk: {
                for (0..self.size.x) |i| {
                    for (0..self.size.y) |j| {
                        if (self.at(i, j)) {
                            break :blk i;
                        }
                    }
                } else return null;
            };

            const right: usize = blk: {
                for (0..self.size.x) |n_i| {
                    const i = self.size.x - 1 - n_i;
                    for (0..self.size.y) |j| {
                        if (self.at(i, j)) {
                            break :blk i;
                        }
                    }
                } else return null;
            };

            const up: usize = blk: {
                for (0..self.size.y) |j| {
                    for (0..self.size.x) |i| {
                        if (self.at(i, j)) {
                            break :blk j;
                        }
                    }
                } else return null;
            };

            const down: usize = blk: {
                for (0..self.size.y) |n_j| {
                    const j = self.size.y - 1 - n_j;
                    for (0..self.size.x) |i| {
                        if (self.at(i, j)) {
                            break :blk j;
                        }
                    }
                } else return null;
            };

            return .fromCorners(
                .new(left, up),
                .new(right, down),
            );
        }

        pub fn findSingle(self: Self, value: T) !UVec2 {
            var result: ?UVec2 = null;
            var it = self.iterator();
            while (it.next()) |pos| {
                if (std.meta.eql(self.at2(pos), value)) {
                    if (result != null) return error.TooMany;
                    result = pos;
                }
            }
            if (result == null) return error.TooFew;
            return result.?;
        }

        pub fn filterValues(self: Self, allocator: std.mem.Allocator, values: []const T) !Grid2D(bool) {
            return try self.mapWithCtx(allocator, bool, values, struct {
                pub fn anon(v: T, vs: []const T) bool {
                    return std.mem.indexOfScalar(T, vs, v) != null;
                }
            }.anon);
        }

        pub fn cropped(original: Self, allocator: std.mem.Allocator, rect: kommon.math.URect) !Self {
            var result: Self = try .initUndefined(allocator, rect.inner_size.add(.both(1)));

            var it = result.iterator();
            while (it.next()) |pos| {
                result.set(pos, try original.at2(pos.add(rect.top_left))) catch unreachable;
            }

            return result;
        }

        pub fn getTileRect(self: Self, whole_rect: Rect, tile: UVec2) Rect {
            assert(self.inBoundsUnsigned(tile));
            const tile_size = whole_rect.size.div(self.size.tof32());
            return .{
                .top_left = whole_rect.top_left.add(tile_size.mul(tile.tof32())),
                .size = tile_size,
            };
        }

        // TODO: improve performance trivially
        pub fn tileAt(self: Self, pos: kommon.math.Vec2, whole_rect: Rect) ?UVec2 {
            var it = self.iterator();
            while (it.next()) |tile| {
                if (self.getTileRect(whole_rect, tile).contains(pos)) return tile;
            } else return null;
        }

        pub fn inBoundsSigned(self: Self, pos: IVec2) bool {
            return pos.x >= 0 and pos.y >= 0 and self.inBoundsUnsigned(pos.cast(usize));
        }

        pub fn inBoundsUnsigned(self: Self, pos: UVec2) bool {
            return pos.x < self.size.x and pos.y < self.size.y;
        }

        pub fn mirrorVertically(self: *Self) void {
            for (0..@divFloor(self.size.y, 2)) |j| {
                for (0..self.size.x) |i| {
                    std.mem.swap(
                        T,
                        self.getPtr(.new(i, j)),
                        self.getPtr(.new(i, self.size.y - j - 1)),
                    );
                }
            }
        }

        pub fn mirrorHorizontally(self: *Self) void {
            for (0..@divFloor(self.size.x, 2)) |i| {
                for (0..self.size.y) |j| {
                    std.mem.swap(
                        T,
                        self.getPtr(.new(i, j)),
                        self.getPtr(.new(self.size.x - i - 1, j)),
                    );
                }
            }
        }
    };
}

pub const EdgePos = struct {
    pos: UVec2,
    dir: IVec2,

    pub fn nextPos(self: EdgePos) UVec2 {
        assert(self.validDir());
        return self.pos.addSigned(self.dir);
    }

    pub fn middle(self: EdgePos) kommon.math.Vec2 {
        assert(self.validDir());
        return self.pos.tof32().add(self.dir.tof32().scale(0.5));
    }

    pub fn translate(self: EdgePos, p: UVec2) EdgePos {
        assert(self.validDir());
        return .{ .pos = self.pos.add(p), .dir = self.dir };
    }

    pub fn translateNew(self: EdgePos, d: IVec2) EdgePos {
        assert(self.validDir());
        return .{ .pos = self.pos.addSigned(d), .dir = self.dir };
    }

    pub fn sameAs(self: EdgePos, other: EdgePos) bool {
        assert(self.validDir());
        assert(other.validDir());
        return (self.pos.equals(other.pos) and self.dir.equals(other.dir)) or
            (self.pos.equals(other.nextPos()) and self.dir.equals(other.dir.neg()));
    }

    pub fn between(a: UVec2, b: UVec2) ?EdgePos {
        const r: EdgePos = .{ .pos = a, .dir = b.subToSigned(a) };
        if (!r.validDir()) return null;
        return r;
    }

    fn validDir(self: EdgePos) bool {
        return self.dir.isCardinalDirection();
    }
};

/// only inner edges
pub fn Grid2DEdges(T: type) type {
    // x 0 x 1 x
    // 0   1   2
    // x 2 x 3 x
    // 3   4   5
    // x 4 x 5 x
    return struct {
        size: UVec2,
        data_hor: []T,
        data_ver: []T,

        const Self = @This();

        pub fn initUndefined(allocator: std.mem.Allocator, size: UVec2) !Self {
            return .{
                .size = size,
                .data_hor = try allocator.alloc(T, size.x * (size.y - 1)),
                .data_ver = try allocator.alloc(T, (size.x - 1) * size.y),
            };
        }

        pub fn initFill(allocator: std.mem.Allocator, size: UVec2, value: T) !Self {
            const result: Self = try .initUndefined(allocator, size);
            result.fill(value);
            return result;
        }

        pub fn fill(self: Self, value: T) void {
            @memset(self.data_hor, value);
            @memset(self.data_ver, value);
        }

        pub fn deinit(self: Self, allocator: std.mem.Allocator) void {
            allocator.free(self.data_hor);
            allocator.free(self.data_ver);
        }

        pub fn at(self: Self, edge: EdgePos) T {
            return self.atSafePtr(edge).?.*;
        }

        pub fn atSafe(self: Self, edge: EdgePos) ?T {
            if (self.atSafePtr(edge)) |ptr| {
                return ptr.*;
            } else return null;
        }

        pub fn set(self: *Self, edge: EdgePos, value: T) void {
            self.atSafePtr(edge).?.* = value;
        }

        pub fn atSafePtr(self: Self, edge: EdgePos) ?*T {
            assert(edge.validDir());
            if (!self.inBounds(edge)) return null;
            if (edge.dir.x == 0) {
                return &self.data_hor[self.indexOfHorBelow(if (edge.dir.y > 0) edge.pos else edge.pos.decY())];
            } else {
                return &self.data_ver[self.indexOfVerRight(if (edge.dir.x > 0) edge.pos else edge.pos.decX())];
            }
        }

        fn indexOfHorBelow(self: Self, pos: UVec2) usize {
            assert(pos.x < self.size.x);
            assert(pos.y < self.size.y - 1);
            return pos.y * self.size.x + pos.x;
        }

        fn indexOfVerRight(self: Self, pos: UVec2) usize {
            assert(pos.x < self.size.x - 1);
            assert(pos.y < self.size.y);
            return pos.y * (self.size.x - 1) + pos.x;
        }

        pub fn inBounds(self: Self, edge: EdgePos) bool {
            assert(edge.validDir());
            const big_pos = edge.pos.scale(2).cast(isize).add(edge.dir);
            return 0 <= big_pos.x and big_pos.x < self.size.x * 2 - 1 and
                0 <= big_pos.y and big_pos.y < self.size.y * 2 - 1;
        }

        const EdgeIterator = struct {
            hor: GridIterator,
            ver: GridIterator,

            pub fn init(size: UVec2) EdgeIterator {
                return .{
                    .hor = .init(size.decY()),
                    .ver = .init(size.decX()),
                };
            }

            pub fn reset(self: *EdgeIterator) void {
                self.hor.reset();
                self.ver.reset();
            }

            pub fn next(self: *EdgeIterator) ?EdgePos {
                if (self.hor.next()) |pos| {
                    return .{ .pos = pos, .dir = .e2 };
                } else if (self.ver.next()) |pos| {
                    return .{ .pos = pos, .dir = .e1 };
                } else {
                    return null;
                }
            }
        };

        pub fn iterator(self: Self) EdgeIterator {
            return .init(self.size);
        }
    };
}

test "foo" {
    const raw_grid = try kommon.Grid2D(u8).fromAscii(std.testing.allocator,
        \\.....
        \\.111.
        \\..1..
        \\.....
    );
    defer raw_grid.deinit(std.testing.allocator);

    const bool_grid = try raw_grid.filterValues(std.testing.allocator, "1");
    defer bool_grid.deinit(std.testing.allocator);

    const rect = bool_grid.bounds().?;
    try std.testing.expectEqual(kommon.math.URect{
        .top_left = .new(1, 1),
        .inner_size = .new(2, 1),
    }, rect);
}

// TODO: move to itertools
const GridIterator = struct {
    i: kommon.itertools.Iterator(usize),
    j: kommon.itertools.Iterator(usize),

    pub fn init(size: UVec2) GridIterator {
        return .{
            .i = .init(0, size.x - 1),
            .j = .init(0, size.y - 1),
        };
    }

    pub fn reset(self: *GridIterator) void {
        self.i.reset();
        self.j.reset();
    }

    pub fn next(self: *GridIterator) ?UVec2 {
        // ideal:
        // for (0..self.size.height) |j| {
        //     for (0..self.size.width) |i| {
        //         yield .new(i, j);
        //     }
        // }

        if (self.j.cur()) |j| {
            if (self.i.next()) |i| {
                return .new(i, j);
            } else {
                self.j.advance();
                self.i.reset();
                return self.next();
            }
        } else {
            return null;
        }
    }
};

const GridSignedIterator = struct {
    grid_iterator: GridIterator,

    pub fn reset(self: *GridSignedIterator) void {
        self.grid_iterator.reset();
    }

    pub fn next(self: *GridSignedIterator) ?IVec2 {
        if (self.grid_iterator.next()) |p|
            return p.cast(isize)
        else
            return null;
    }
};
