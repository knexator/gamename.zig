const std = @import("std");
const assert = std.debug.assert;

const kommon = @import("kommon.zig");
const UVec2 = kommon.math.UVec2;
const IVec2 = kommon.math.IVec2;
const Rect = kommon.math.Rect;

pub fn Grid2D(T: type) type {
    // 0 1 2
    // 3 4 5
    // 6 7 8
    return struct {
        size: UVec2,
        data: []T,

        const Self = @This();

        pub fn initUndefined(allocator: std.mem.Allocator, size: UVec2) !Self {
            return .{ .size = size, .data = try allocator.alloc(T, size.x * size.y) };
        }

        pub fn initFill(allocator: std.mem.Allocator, size: UVec2, fill: T) !Self {
            const result: Self = .{ .size = size, .data = try allocator.alloc(T, size.x * size.y) };
            @memset(result.data, fill);
            return result;
        }

        pub fn deinit(self: Self, allocator: std.mem.Allocator) void {
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

        pub fn getPtr(self: Self, pos: UVec2) *T {
            return &self.data[self.indexOf(pos)];
        }

        pub fn set(self: Self, pos: UVec2, value: T) void {
            self.data[self.indexOf(pos)] = value;
        }

        fn indexOf(self: Self, pos: UVec2) usize {
            if (!self.inBoundsUnsigned(pos)) std.debug.panic("OoB: Grid2D of size {any} accessed at position {any}", .{ self.size, pos });
            return pos.y * self.size.x + pos.x;
        }

        fn indexOfSigned(self: Self, pos: IVec2) usize {
            if (!self.inBoundsSigned(pos)) std.debug.panic("OoB: Grid2D of size {any} accessed at position {any}", .{ self.size, pos });
            return self.indexOf(pos.cast(usize));
        }

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

        const GridIterator = struct {
            grid: Self,
            i: kommon.itertools.Iterator(usize),
            j: kommon.itertools.Iterator(usize),

            pub fn init(grid: Self) GridIterator {
                return .{
                    .grid = grid,
                    .i = .init(0, grid.size.x - 1),
                    .j = .init(0, grid.size.y - 1),
                };
            }

            pub fn reset(self: *GridIterator) void {
                self.i.reset();
                self.j.reset();
            }

            pub fn next(self: *GridIterator) ?UVec2 {
                // ideal:
                // for (0..self.grid.height) |j| {
                //     for (0..self.grid.width) |i| {
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

        pub fn iterator(self: Self) GridIterator {
            return .init(self);
        }

        pub fn iteratorSigned(self: Self) GridSignedIterator {
            return .{ .grid_iterator = .init(self) };
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
            const ascii_grid: Grid2D(u8) = try .fromAscii(allocator, ascii);
            defer ascii_grid.deinit(allocator);
            return try ascii_grid.map(allocator, T, map_fn);
        }

        pub fn fromAscii(allocator: std.mem.Allocator, ascii: []const u8) !Self {
            if (T != u8) @compileError("fromAscii only works on Grid2D(u8)");
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

        pub fn map(self: Self, allocator: std.mem.Allocator, comptime NewType: type, comptime map_fn: fn (v: T) NewType) !Grid2D(NewType) {
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

        pub fn inBoundsSigned(self: Self, pos: IVec2) bool {
            return pos.x >= 0 and pos.y >= 0 and self.inBoundsUnsigned(pos.cast(usize));
        }

        pub fn inBoundsUnsigned(self: Self, pos: UVec2) bool {
            return pos.x < self.size.x and pos.y < self.size.y;
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
