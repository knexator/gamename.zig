// naming from https://www.geometrictools.com/Documentation/TriangulationByEarClipping.pdf

// TODO: caching
pub const Triangulator = struct {
    pub const Triangle = struct {
        points: [3]Vec2,

        // works by checking that p is on the same side for all 3 sides of the triangle
        pub fn contains(self: Triangle, p: Vec2) bool {
            const a = self.points[0];
            const b = self.points[1];
            const c = self.points[2];

            const d1 = sign(p, a, b);
            const d2 = sign(p, b, c);
            const d3 = sign(p, c, a);

            const has_neg = (d1 < 0) or (d2 < 0) or (d3 < 0);
            const has_pos = (d1 > 0) or (d2 > 0) or (d3 > 0);

            // HACK: consider points on the border as outside the triangle
            const eps = std.math.floatEps(f32);
            const has_zero = @abs(d1) < eps or @abs(d2) < eps or @abs(d3) < eps;

            return !has_zero and !(has_neg and has_pos);
        }

        /// On which side of the l1-l2 line does p fall?
        fn sign(p: Vec2, l1: Vec2, l2: Vec2) f32 {
            return Vec2.cross(p.sub(l2), l1.sub(l2));
        }

        fn area(tri: Triangle) f32 {
            const a = tri.points[0];
            const b = tri.points[1];
            const c = tri.points[2];

            return @abs((a.x * (b.y - c.y) +
                b.x * (c.y - a.y) +
                c.x * (a.y - b.y)) / 2);
        }
    };

    /// positive order = .e1, .e2, -.e1, -.e2
    pub const VertexOrder = enum { pos, neg };

    const Vertex = struct {
        prev: *Vertex,
        next: *Vertex,

        position: Vec2,
        original_index: usize,

        // TODO: cache convex/reflex for better performance

        fn isConvex(self: Vertex, order: VertexOrder) bool {
            const prev = self.prev.position;
            const curr = self.position;
            const next = self.next.position;

            const cross = Vec2.cross(
                curr.sub(prev),
                next.sub(curr),
            );
            return switch (order) {
                .pos => cross < 0,
                .neg => cross > 0,
            };
        }

        pub fn isEar(self: Vertex, order: VertexOrder) bool {
            if (self.isConvex(order)) return false;

            const tri: Triangle = .{ .points = .{
                self.prev.position,
                self.position,
                self.next.position,
            } };

            var cur = self.next.next;
            while (cur != self.prev) : (cur = cur.next) {
                if (tri.contains(cur.position)) return false;
            } else return true;
        }
    };

    /// Get the set of indices that triangulate a polygon. Assumes the vertices are in e1->e2 order
    pub fn triangulate(
        comptime IndexType: type,
        allocator: std.mem.Allocator,
        vertices: []const Vec2,
        /// null = guess
        vertex_order: ?VertexOrder,
    ) error{ BadVertexOrder, OutOfMemory }![][3]IndexType {
        if (vertex_order) |order| {
            assert(switch (@typeInfo(IndexType)) {
                .int, .comptime_int => true,
                else => false,
            });
            var backing_array: []Vertex = try allocator.alloc(Vertex, vertices.len);
            defer allocator.free(backing_array);
            for (backing_array, vertices, 0..) |*dst, pos, k| {
                dst.* = .{
                    .position = pos,
                    .original_index = k,
                    .next = &backing_array[@mod(k + 1, vertices.len)],
                    .prev = &backing_array[@mod(k + vertices.len - 1, vertices.len)],
                };
            }

            var handle: *const Vertex = &backing_array[0];
            const result: [][3]IndexType = try allocator.alloc([3]IndexType, vertices.len - 2);
            errdefer allocator.free(result);
            for (result) |*dst| {
                const original_handle = handle;
                while (!handle.isEar(order)) {
                    handle = handle.next;
                    if (handle == original_handle) return error.BadVertexOrder;
                }

                dst.* = .{
                    @intCast(handle.prev.original_index),
                    @intCast(handle.original_index),
                    @intCast(handle.next.original_index),
                };
                handle.prev.next = handle.next;
                handle.next.prev = handle.prev;
                handle = handle.next;
            }

            return result;
        } else {
            return triangulate(IndexType, allocator, vertices, .pos) catch |err| switch (err) {
                else => err,
                error.BadVertexOrder => triangulate(IndexType, allocator, vertices, .neg) catch |err2| switch (err2) {
                    else => err2,
                    error.BadVertexOrder => unreachable,
                },
            };
        }
    }

    pub fn polygonArea(vertices: []const Vec2, indices: []const [3]usize) f32 {
        var result: f32 = 0;
        for (indices) |tri_indices| {
            const tri: Triangle = .{ .points = .{
                vertices[tri_indices[0]],
                vertices[tri_indices[1]],
                vertices[tri_indices[2]],
            } };
            result += tri.area();
        }
        return result;
    }
};

test "triangle area" {
    const tri: Triangulator.Triangle = .{ .points = .{
        .new(0, 0),
        .new(4, 0),
        .new(1, 1),
    } };
    try std.testing.expectEqual(2, tri.area());
}

test "polygon area" {
    const vertices: []const Vec2 = &.{
        .new(0, 0),
        .new(4, 0),
        .new(1, 1),
        .new(0, 4),
    };
    const indices: []const [3]usize = &.{
        .{ 0, 1, 2 },
        .{ 2, 3, 0 },
    };
    const real_area = 4;
    const computed_area = Triangulator.polygonArea(vertices, indices);
    try std.testing.expectEqual(real_area, computed_area);
}

test "triangulate triangle" {
    const vertices: []const Vec2 = &.{
        .new(5, 0),
        .new(0, 1),
        .new(0, -1),
    };
    const real_area = 5;

    const indices = try Triangulator.triangulate(usize, std.testing.allocator, vertices, null);
    defer std.testing.allocator.free(indices);

    try std.testing.expectEqual(vertices.len - 2, indices.len);

    const computed_area = Triangulator.polygonArea(vertices, indices);
    try std.testing.expectEqual(real_area, computed_area);

    // would be nice for this to be less hardcoded
    try std.testing.expectEqual(indices[0], .{ 2, 0, 1 });
}

test "triangulate rect" {
    const vertices: []const Vec2 = &.{
        .new(0, 0),
        .new(1, 0),
        .new(1, 2),
        .new(0, 2),
    };
    const real_area = 2;

    const indices = try Triangulator.triangulate(usize, std.testing.allocator, vertices, null);
    defer std.testing.allocator.free(indices);

    try std.testing.expectEqual(vertices.len - 2, indices.len);

    const computed_area = Triangulator.polygonArea(vertices, indices);
    try std.testing.expectEqual(real_area, computed_area);

    // would be nice for this to be less hardcoded
    try std.testing.expectEqual(indices[0], .{ 3, 0, 1 });
    try std.testing.expectEqual(indices[1], .{ 3, 1, 2 });
}

test "triangulate non-convex polygon" {
    const vertices: []const Vec2 = &.{
        .new(0, 0),
        .new(4, 0),
        .new(1, 1),
        .new(0, 4),
    };
    const real_area = 4;

    const indices = try Triangulator.triangulate(usize, std.testing.allocator, vertices, null);
    defer std.testing.allocator.free(indices);

    try std.testing.expectEqual(vertices.len - 2, indices.len);

    const computed_area = Triangulator.polygonArea(vertices, indices);
    try std.testing.expectEqual(real_area, computed_area);
}

test "triangulate with holes" {
    const vertices: []const Vec2 = &.{
        .fromPolar(1, 0),
        .fromPolar(1, -1.0 / 3.0),
        .fromPolar(1, -2.0 / 3.0),
        .fromPolar(1, 0),
        .fromPolar(2, 0),
        .fromPolar(2, 1.0 / 3.0),
        .fromPolar(2, 2.0 / 3.0),
        .fromPolar(2, 0),
    };

    const indices = try Triangulator.triangulate(usize, std.testing.allocator, vertices, null);
    defer std.testing.allocator.free(indices);

    try std.testing.expectEqual(vertices.len - 2, indices.len);
}

test "can cast to plain indices" {
    const v1: []const [3]usize = &.{
        .{ 0, 1, 2 },
        .{ 3, 4, 5 },
        .{ 6, 7, 8 },
    };
    const v2: []const usize = &.{
        0, 1, 2,
        3, 4, 5,
        6, 7, 8,
    };

    try std.testing.expectEqualSlices(usize, v2, @ptrCast(v1));
}

test "opposite order triangulation" {
    const vertices: []const Vec2 = &.{
        .fromPolar(1, 0),
        .fromPolar(1, -1.0 / 3.0),
        .fromPolar(1, -2.0 / 3.0),
    };

    const indices = try Triangulator.triangulate(usize, std.testing.allocator, vertices, null);
    defer std.testing.allocator.free(indices);

    try std.testing.expectEqual(1, indices.len);
}

const std = @import("std");
const assert = std.debug.assert;

const math = @import("math.zig");
const Vec2 = math.Vec2;
