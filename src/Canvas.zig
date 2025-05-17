//! Game-layer wrapper around Gl, with common drawing utilities

/// for stuff that lives only for this frame
frame_arena: std.heap.ArenaAllocator,
/// only valid for a frame!
gl: Gl,

fill_shape_renderable: Gl.Renderable,

DEFAULT_SHAPES: struct {
    circle_128: PrecomputedShape,
    square: PrecomputedShape,

    // TODO(zig): remove once we get comptime allocation
    pub fn init(gpa: std.mem.Allocator) !@This() {
        return .{
            .circle_128 = try .fromPoints(gpa, &funk.map(
                Vec2.fromTurns,
                &funk.linspace01(
                    128,
                    false,
                ),
            )),
            .square = .{
                .local_points = &.{.zero, .e1, .e2, .one},
                .triangles = &.{ .{0, 1, 2}, .{3, 2, 1} },
            },
        };
    }

    pub fn deinit(self: @This(), gpa: std.mem.Allocator) void {
        self.circle_128.deinit(gpa);
    }
},

pub const Canvas = @This();

pub fn init(gl: Gl, gpa: std.mem.Allocator) !Canvas {
    const fill_shape_info: RenderableInfo = .{
        .VertexData = extern struct { position: Vec2 },
        .IndexType = u16,
        .UniformTypes = struct {
            color: FColor,
            rect: Rect,
            point: Point,
        },
        .vertex =
        \\uniform vec4 u_rect; // as top_left, size
        \\uniform vec4 u_point; // as pos, turns, scale
        \\
        \\in vec2 a_position;
        \\#define TAU 6.283185307179586
        \\void main() {
        \\  float c = cos(u_point.z * TAU);
        \\  float s = sin(u_point.z * TAU);
        \\  vec2 world_position = u_point.xy + u_point.w * (mat2x2(c,s,-s,c) * a_position);
        \\  vec2 camera_position = (world_position - u_rect.xy) / u_rect.zw;
        \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
        \\}
        ,
        .fragment =
        \\precision highp float;
        \\out vec4 out_color;
        \\
        \\uniform vec4 u_color;
        \\void main() {
        \\  out_color = u_color;
        \\}
        ,
    };

    return .{
        .gl = gl,
        .frame_arena = .init(gpa),
        .fill_shape_renderable = try gl.buildRenderable(
            fill_shape_info.vertex,
            fill_shape_info.fragment,
            .{ .attribs = &.{
                .{ .name = "a_position", .kind = .Vec2 },
            } },
            &.{
                .{ .name = "u_rect", .kind = .Rect },
                .{ .name = "u_point", .kind = .Point },
                .{ .name = "u_color", .kind = .FColor },
            },
        ),
        .DEFAULT_SHAPES = try .init(gpa),
    };
}

pub fn deinit(self: Canvas, gl: Gl, gpa: std.mem.Allocator) void {
    self.DEFAULT_SHAPES.deinit(gpa);
    _ = gl;
}

// TODO: multiple shapes in one draw call
pub fn fillShape(
    self: Canvas,
    camera: Rect,
    parent: Point,
    shape: PrecomputedShape,
    color: Color,
) void {
    self.gl.useRenderable(self.fill_shape_renderable, shape.local_points.ptr, shape.local_points.len * @sizeOf(Vec2), shape.triangles, &.{
        .{ .name = "u_color", .value = .{ .FColor = color.toFColor() } },
        .{ .name = "u_point", .value = .{ .Point = parent } },
        .{ .name = "u_rect", .value = .{ .Rect = camera } },
    }, null);
}

pub fn fillCircle(
    self: Canvas,
    camera: Rect,
    center: Vec2,
    radius: f32,
    color: Color,
) void {
    self.fillShape(
        camera, 
        .{ .pos = center, .scale = radius }, 
        self.DEFAULT_SHAPES.circle_128, 
        color,
    );
}

pub fn fillSquare(
    self: Canvas,
    camera: Rect,
    top_left: Vec2,
    side: f32,
    color: Color,
) void {
    self.fillShape(
        camera, 
        .{ .pos = top_left, .scale = side }, 
        self.DEFAULT_SHAPES.square, 
        color,
    );
}

pub fn fillRect(
    self: Canvas,
    camera: Rect,
    rect: Rect,
    color: Color,
) void {
    self.fillShape(
        camera, 
        .{ .pos = rect.top_left }, 
        .{
            .local_points = &.{
                .new(0, 0),
                .new(rect.size.x, 0),
                .new(0, rect.size.y),
                .new(rect.size.x, rect.size.y),
            },
            .triangles = self.DEFAULT_SHAPES.square.triangles,
        },
        color,
    );
}
    
pub fn fillArc(
    self: *Canvas,
    camera: Rect,
    center: Vec2,
    radius: f32,
    turns_start: f32,
    turns_end: f32,
    color: Color
) !void {
    self.fillShape(camera, .{
        .pos = center,
        .scale = radius,
    }, try self.tmpShape(&(funk.mapWithCtx(
        struct {
            pub fn anon(ctx: struct {min: f32, max: f32}, t: f32) Vec2 {
                return .fromTurns(std.math.lerp(ctx.min, ctx.max, t));
            }
        }.anon,
        &funk.linspace01(128, true),
        .{.min = turns_start, .max = turns_end},
    ) ++ [1]Vec2{.zero})), color);
}

pub fn fillCrown(
    self: *Canvas,
    camera: Rect,
    center: Vec2,
    radius: f32,
    width: f32,
    color: Color
) !void {
    const CIRCLE_RESOLUTION = 128;
    self.fillShape(camera, .{
        .pos = center,
    }, try self.tmpShape(&(funk.mapWithCtx(
        Vec2.fromPolar,
        &funk.linspace(1, 0, CIRCLE_RESOLUTION, true),
        radius - width / 2,
    ) ++ funk.mapWithCtx(
        Vec2.fromPolar,
        &funk.linspace(0, 1, CIRCLE_RESOLUTION, true),
        radius + width / 2,
    ))), color);
}



/// Performs triangulation; consider caching the result.
pub fn tmpShape(
    self: *Canvas,
    local_points: []const Vec2,
) !PrecomputedShape {
    return try .fromPoints(self.frame_arena.allocator(), local_points);
}

pub fn startFrame(self: *Canvas, gl: Gl) void {
    self.gl = gl;
    _ = self.frame_arena.reset(.retain_capacity);
}

const std = @import("std");
const assert = std.debug.assert;
const kommon = @import("kommon");
const math = kommon.math;
const Color = math.Color;
const FColor = math.FColor;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const IVec2 = math.IVec2;
const funk = kommon.funktional;
const Triangulator = kommon.Triangulator;
const Gl = @import("Gl.zig");
const PrecomputedShape = @import("renderer.zig").PrecomputedShape;
const RenderableInfo = @import("renderer.zig").RenderableInfo;
