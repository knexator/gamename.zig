// TODO: maybe delete this file?

pub const RenderableInfo = struct {
    VertexData: type,
    IndexType: type,
    UniformTypes: type,
    vertex: [:0]const u8,
    fragment: [:0]const u8,

    // TODO: pub fn fromType(...)
};

// TODO(renderapi): remove this from this layer
pub const fill_atom_info: RenderableInfo = .{
    .VertexData = extern struct { position: Vec2 },
    .IndexType = u16,
    .UniformTypes = struct {
        color: FColor,
        rect: Rect,
        point: Point,
        noise_z: f32,
    },
    .vertex =
    \\precision highp float;
    \\uniform vec4 u_camera; // as top_left, size
    \\uniform vec4 u_point; // as pos, turns, scale
    \\
    \\in vec2 a_position;
    \\uniform vec2 u_pos_offset;
    \\out vec2 v_local_pos;
    \\#define TAU 6.283185307179586
    \\void main() {
    \\  v_local_pos = a_position + u_pos_offset;
    \\  float c = cos(u_point.z * TAU);
    \\  float s = sin(u_point.z * TAU);
    \\  vec2 world_position = u_point.xy + u_point.w * (mat2x2(c,s,-s,c) * a_position);
    \\  vec2 camera_position = (world_position - u_camera.xy) / u_camera.zw;
    \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
    \\}
    ,
    .fragment = 
    \\precision highp float;
    \\out vec4 out_color;
    \\
++ kommon.shaders.perlin_noise ++
    \\
    \\uniform sampler2D u_texture;
    \\uniform vec4 u_color;
    \\uniform float u_noise_z;
    \\in vec2 v_local_pos;
    \\void main() {
    \\  float t1 = perlin_noise(vec3(v_local_pos * 2.0, 0.0 + u_noise_z * 20.0));
    // \\  float t2 = perlin_noise(vec3(v_local_pos * 6.0, 10.0 + u_noise_z * 20.0)) - 0.5;
    // \\  float t3 = perlin_noise(vec3(v_local_pos * 10.0, 20.0 + u_noise_z * 20.0)) - 0.5;
    // \\  float t = mix(0.8, 1.0, smoothstep(-0.75, -0.25, t1 * 0.75 + t2 * 0.25 + t3 * 0.125));
    \\  float t = mix(1.0, 0.5, smoothstep(0.35, 0.45, t1));
    // \\  float t = mix(0.5, 1.0, smoothstep(-0.50, -0.45, t1));
    // \\  float t = mix(0.8, 1.0, smoothstep(-0.25, 0.0, t1));
    // \\  out_color = u_color * vec4(vec3(t), 1.0);
    //\\  vec2 uv = (v_local_pos * vec2(0.5, 1) * vec2(0.9 * 0.9, 0.95) + vec2(0.25, 1)) * 0.5;
    \\ // -1..1 in both axis
    \\  vec2 from_center = v_local_pos * vec2(0.5, 1) + vec2(-0.75, 0);
    \\  vec2 uv = (from_center * 0.9 + vec2(1.0)) * 0.5;
    \\  out_color = (texture(u_texture, uv) + vec4(vec3(0.25), 0)) * mix(u_color.gbra, u_color, t);
    \\}
    ,
};

pub const PrecomputedShape = struct {
    pub const IndexType = Gl.IndexType;

    local_points: []const Vec2,
    triangles: []const [3]IndexType,
    fill_shape_renderable: ?Gl.Renderable,
    fill_atom_renderable: ?Gl.Renderable,

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
                        .fromType(Vec2, "a_position"),
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
            // TODO(renderapi): remove this from here
            .fill_atom_renderable = if (gl) |g| blk: {
                const asdf = try g.buildRenderable(
                    fill_atom_info.vertex,
                    fill_atom_info.fragment,
                    .{ .attribs = &.{
                        .fromType(Vec2, "a_position"),
                    } },
                    &.{
                        .{ .name = "u_camera", .kind = .Rect },
                        .{ .name = "u_point", .kind = .Point },
                        .{ .name = "u_color", .kind = .FColor },
                        .{ .name = "u_noise_z", .kind = .f32 },
                        .{ .name = "u_pos_offset", .kind = .Vec2 },
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
                        .fromType(Vec2, "a_position"),
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
            .fill_atom_renderable = if (gl) |g| blk: {
                const asdf = try g.buildRenderable(
                    fill_atom_info.vertex,
                    fill_atom_info.fragment,
                    .{ .attribs = &.{
                        .fromType(Vec2, "a_position"),
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
const kommon = @import("kommon.zig");
const math = @import("math.zig");
const FColor = math.FColor;
const Rect = math.Rect;
const Point = math.Point;
const UVec2 = math.UVec2;
const IVec2 = math.IVec2;
const funk = @import("funktional.zig");
