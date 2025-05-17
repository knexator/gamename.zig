//! Game-layer wrapper around Gl, with common drawing utilities

/// for stuff that lives only for this frame
frame_arena: std.heap.ArenaAllocator,
/// only valid for a frame!
gl: Gl,

fill_shape_renderable: Gl.Renderable,
text_renderers: []TextRenderer,

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

pub fn init(gl: Gl, gpa: std.mem.Allocator, comptime font_names: []const []const u8, font_atlases: []const *const anyopaque) !Canvas {
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

    assert(font_names.len == font_atlases.len);
    const text_renderers = try gpa.alloc(TextRenderer, font_names.len);
    inline for (font_names, font_atlases, text_renderers) |name, atlas, *dst| {
        dst.* = try .init(name, gpa, gl, atlas);
    }

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
        .text_renderers = text_renderers,
    };
}

pub fn deinit(self: *Canvas, gl: Gl, gpa: std.mem.Allocator) void {
    self.DEFAULT_SHAPES.deinit(gpa);
    for (self.text_renderers) |*t| t.deinit();
    gpa.free(self.text_renderers);
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

// TODO: small text looks worse on the web version!
pub const TextRenderer = struct {
    atlas_texture: Gl.Texture,
    renderable: Gl.Renderable,
    font_info: std.json.Parsed(FontJsonInfo),

    const RectSides = struct {
        left: f32,
        bottom: f32,
        right: f32,
        top: f32,
    };
    const FontJsonInfo = struct {
        atlas: struct {
            type: enum { msdf },
            distanceRange: f32,
            distanceRangeMiddle: f32,
            /// pixels per em
            size: f32,
            width: usize,
            height: usize,
            yOrigin: enum { top },
        },
        metrics: struct {
            emSize: f32,
            lineHeight: f32,
            /// ??
            ascender: f32,
            /// ??
            descender: f32,
            /// ??
            underlineY: f32,
            /// ??
            underlineThickness: f32,
        },
        glyphs: []struct {
            unicode: u8,
            advance: f32,
            planeBounds: ?RectSides = null,
            atlasBounds: ?RectSides = null,
        },
        kerning: []struct {
            unicode1: u8,
            unicode2: u8,
            advance: f32,
        },
    };

    pub fn init(comptime font_name: []const u8, gpa: std.mem.Allocator, gl: Gl, atlas_image: *const anyopaque) !TextRenderer {
        return .{
            .atlas_texture = gl.buildTexture2D(atlas_image),
            // TODO: parse the font data at comptime
            .font_info = try std.json.parseFromSlice(
                FontJsonInfo,
                gpa,
                @embedFile("./fonts/" ++ font_name ++ ".json"),
                .{},
            ),
            .renderable = try gl.buildRenderable(
                \\in vec2 a_position;
                \\in vec2 a_texcoord;
                \\
                \\out vec2 v_texcoord;
                \\
                \\// 0,0 => -1,1 (top left)
                \\// 1,0 => 1,1 (top right)
                \\vec2 clipFromCam(vec2 p) {
                \\  return (p * 2.0 - 1.0) * vec2(1, -1);
                \\}
                \\
                \\void main() {
                \\  v_texcoord = a_texcoord;
                \\  gl_Position = vec4(clipFromCam(a_position), 0, 1);
                \\}
            ,
                \\precision highp float;
                \\out vec4 out_color;
                \\
                \\// for some reason, on desktop, the fwidth value is half of what it should.
                \\#ifdef GL_ES // WebGL2
                \\  #define FWIDTH(x) fwidth(x)
                \\#else // Desktop
                \\  #define FWIDTH(x) (2.0 * fwidth(x))
                \\#endif
                \\
                \\in vec2 v_texcoord;
                \\uniform sampler2D u_texture;
                \\
                \\float median(float r, float g, float b) {
                \\  return max(min(r, g), min(max(r, g), b));
                \\}
                \\
                \\float inverseLerp(float a, float b, float t) {
                \\  return (t - a) / (b - a);
                \\}
                \\
                \\void main() {
                \\  // assume square texture
                \\  float sdf_texture_size = float(textureSize(u_texture, 0).x);
                \\  // the values in the sdf texture should be remapped to (-sdf_pxrange/2, +sdf_pxrange/2)
                // TODO: get sdf_pxrange from the font data
                \\  float sdf_pxrange = 2.0;
                \\  vec3 raw = texture(u_texture, v_texcoord).rgb;
                \\  float distance_in_texels = (median(raw.r, raw.g, raw.b) - 0.5) * sdf_pxrange;
                \\  // density of the texture on screen; assume uniform scaling.
                \\  float texels_per_pixel = FWIDTH(v_texcoord.x) * sdf_texture_size;
                \\  float distance_in_pixels = distance_in_texels / texels_per_pixel;
                \\  // over how many screen pixels do the transition
                \\  float transition_pixels = 1.0;
                \\  float alpha = clamp(inverseLerp(-transition_pixels / 2.0, transition_pixels / 2.0, distance_in_pixels), 0.0, 1.0);
                \\  // TODO: premultiply alpha?
                \\  out_color = vec4(vec3(1.0), alpha);
                \\}
            ,
                .{ .attribs = &.{
                    .{ .name = "a_position", .kind = .Vec2 },
                    .{ .name = "a_texcoord", .kind = .Vec2 },
                } },
                &.{},
            ),
        };
    }

    pub fn deinit(self: *TextRenderer) void {
        self.font_info.deinit();
        // TODO
        // gl.destroyRenderable(self.renderable);
        // TODO
        // gl.DeleteTextures(1, @ptrCast(&self.texture));
    }

    // TODO: kerning
    // TODO: single draw call, maybe
    pub fn drawText(self: TextRenderer, gl: Gl, camera: Rect, bottom_left: Vec2, text: []const u8, em: f32) void {
        var cursor: Vec2 = bottom_left;
        for (text) |char| {
            cursor = self.drawLetter(gl, camera, cursor, char, em);
        }
    }

    // TODO: use a map
    pub fn drawLetter(self: TextRenderer, gl: Gl, camera: Rect, bottom_left: Vec2, letter: u8, em: f32) Vec2 {
        const glyph_info = blk: {
            for (self.font_info.value.glyphs) |glyph| {
                if (glyph.unicode == letter) break :blk glyph;
            } else unreachable;
        };
        if (glyph_info.atlasBounds) |b| {
            const s = UVec2.new(
                self.font_info.value.atlas.width,
                self.font_info.value.atlas.height,
            ).tof32();
            const p = glyph_info.planeBounds orelse unreachable;

            // TODO: use a better api
            const VertexData = extern struct { position: Vec2, texcoord: Vec2 };
            gl.useRenderable(self.renderable, &[4]VertexData{
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.left, p.bottom).scale(em))),
                    .texcoord = Vec2.new(b.left, b.bottom).div(s),
                },
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.right, p.bottom).scale(em))),
                    .texcoord = Vec2.new(b.right, b.bottom).div(s),
                },
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.left, p.top).scale(em))),
                    .texcoord = Vec2.new(b.left, b.top).div(s),
                },
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.right, p.top).scale(em))),
                    .texcoord = Vec2.new(b.right, b.top).div(s),
                },
            }, 4 * @sizeOf(VertexData), &.{ .{ 0, 1, 2 }, .{ 3, 2, 1 } }, &.{}, self.atlas_texture);
        }

        return bottom_left.addX(em * glyph_info.advance);
    }
};


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
