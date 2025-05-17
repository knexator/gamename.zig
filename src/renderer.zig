// TODO: maybe delete this file?

pub const RenderableInfo = struct {
    VertexData: type,
    IndexType: type,
    UniformTypes: type,
    vertex: [:0]const u8,
    fragment: [:0]const u8,

    // TODO: pub fn fromType(...)
};

pub const PrecomputedShape = struct {
    pub const IndexType = u16;

    local_points: []const Vec2,
    triangles: []const [3]IndexType,

    pub fn fromPoints(gpa: std.mem.Allocator, points: []const Vec2) !PrecomputedShape {
        std.debug.assert(points.len >= 3);
        const triangles = try Triangulator.triangulate(IndexType, gpa, points);
        return .{
            // TODO: clarify ownership
            .local_points = try gpa.dupe(Vec2, points),
            .triangles = triangles,
        };
    }
    
    pub fn deinit(self: PrecomputedShape, gpa: std.mem.Allocator) void {
        gpa.free(self.local_points);
        gpa.free(self.triangles);
    }
};

const std = @import("std");
const kommon = @import("kommon");
const math = kommon.math;
const Color = math.Color;
const FColor = math.FColor;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const Triangulator = kommon.Triangulator;
