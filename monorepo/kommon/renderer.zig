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
    pub const IndexType = Gl.IndexType;

    local_points: []const Vec2,
    triangles: []const [3]IndexType,
    fill_shape_renderable: ?Gl.Renderable,

    /// takes ownership of points
    pub fn fromOwnedPoints(gpa: std.mem.Allocator, points: []const Vec2, gl: ?Gl) !PrecomputedShape {
        std.debug.assert(points.len >= 3);
        const triangles = try Triangulator.triangulate(IndexType, gpa, points, null);
        return .{
            .local_points = points,
            .triangles = triangles,
            .fill_shape_renderable = if (gl) |g| blk: {
                const asdf = try g.buildRenderable(
                    Canvas.fill_shape_info.vertex,
                    Canvas.fill_shape_info.fragment,
                    .{ .attribs = &.{
                        .{ .name = "a_position", .kind = .Vec2 },
                    } },
                    &.{
                        .{ .name = "u_camera", .kind = .Rect },
                        .{ .name = "u_point", .kind = .Point },
                        .{ .name = "u_color", .kind = .FColor },
                    },
                );
                g.setRenderableData(
                    asdf,
                    points.ptr,
                    points.len * @sizeOf(Vec2),
                    triangles,
                    .static,
                );
                break :blk asdf;
            } else null,
        };
    }

    pub fn fromPoints(gpa: std.mem.Allocator, points: []const Vec2, gl: ?Gl) !PrecomputedShape {
        std.debug.assert(points.len >= 3);
        const triangles = try Triangulator.triangulate(IndexType, gpa, points, null);
        return .{
            // TODO: clarify ownership
            .local_points = try gpa.dupe(Vec2, points),
            .triangles = triangles,
            .fill_shape_renderable = if (gl) |g| blk: {
                const asdf = try g.buildRenderable(
                    Canvas.fill_shape_info.vertex,
                    Canvas.fill_shape_info.fragment,
                    .{ .attribs = &.{
                        .{ .name = "a_position", .kind = .Vec2 },
                    } },
                    &.{
                        .{ .name = "u_camera", .kind = .Rect },
                        .{ .name = "u_point", .kind = .Point },
                        .{ .name = "u_color", .kind = .FColor },
                    },
                );
                g.setRenderableData(
                    asdf,
                    points.ptr,
                    points.len * @sizeOf(Vec2),
                    triangles,
                    .static,
                );
                break :blk asdf;
            } else null,
        };
    }

    pub fn deinit(self: PrecomputedShape, gpa: std.mem.Allocator) void {
        gpa.free(self.local_points);
        gpa.free(self.triangles);
    }
};

const std = @import("std");
const Vec2 = @import("math.zig").Vec2;
const Triangulator = @import("triangulator.zig").Triangulator;
const Gl = @import("Gl.zig");
const Canvas = @import("Canvas.zig");
