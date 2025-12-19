//! Game-layer wrapper around Gl, with common drawing utilities

// TODO: have this outside
/// for stuff that lives only for this frame
frame_arena: std.heap.ArenaAllocator,
/// only valid for a frame!
gl: Gl,

fill_instanced_shapes_renderable: Gl.InstancedRenderable,
fill_instanced_circles_renderable: Gl.InstancedRenderable,
instanced_rounded_lines_renderable: Gl.InstancedRenderable,
instanced_colored_separated_rounded_lines_renderable: Gl.InstancedRenderable,
fill_shape_renderable: Gl.Renderable,
fill_shape_vertex_colors_renderable: Gl.Renderable,
/// takes world positions directly
fill_shapes_renderable: Gl.Renderable,
// TODO: instancing
sprite_renderable: Gl.Renderable,
text_renderers: []TextRenderer,

DEFAULT_SHAPES: struct {
    circle_128: PrecomputedShape,
    circle_32: PrecomputedShape,
    circle_8: PrecomputedShape,
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
            ), null),
            .circle_32 = try .fromPoints(gpa, &funk.map(
                Vec2.fromTurns,
                &funk.linspace01(
                    32,
                    false,
                ),
            ), null),
            .circle_8 = try .fromPoints(gpa, &funk.map(
                Vec2.fromTurns,
                &funk.linspace01(
                    8,
                    false,
                ),
            ), null),
            .square = .{
                .local_points = &.{ .zero, .e1, .e2, .one },
                .triangles = &.{ .{ 0, 1, 2 }, .{ 3, 2, 1 } },
                .fill_shape_renderable = null,
            },
        };
    }

    pub fn deinit(self: @This(), gpa: std.mem.Allocator) void {
        self.circle_128.deinit(gpa);
        self.circle_32.deinit(gpa);
        self.circle_8.deinit(gpa);
    }
},

pub const sprite_renderable_vertex_src =
    \\precision highp float;
    \\uniform vec4 u_camera; // as top_left, size
    \\
    \\in vec2 a_position;
    \\in vec2 a_texcoord;
    \\in vec4 a_color;
    \\out vec2 v_texcoord;
    \\out vec4 v_color;
    \\void main() {
    \\  vec2 camera_position = (a_position - u_camera.xy) / u_camera.zw;
    \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
    \\  v_texcoord = a_texcoord;
    \\  v_color = a_color;
    \\}
;

pub const sprite_renderable_frag_src =
    \\precision highp float;
    \\out vec4 out_color;
    \\in vec2 v_texcoord;
    \\in vec4 v_color;
    \\uniform sampler2D u_texture;
    \\void main() {
    \\  out_color = v_color * texture(u_texture, v_texcoord);
    \\}
;

const Canvas = @This();

pub const fill_shape_info: RenderableInfo = .{
    .VertexData = extern struct { position: Vec2 },
    .IndexType = u16,
    .UniformTypes = struct {
        color: FColor,
        rect: Rect,
        point: Point,
    },
    .vertex =
    \\precision highp float;
    \\uniform vec4 u_camera; // as top_left, size
    \\uniform vec4 u_point; // as pos, turns, scale
    \\
    \\in vec2 a_position;
    \\#define TAU 6.283185307179586
    \\void main() {
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
    \\uniform vec4 u_color;
    \\void main() {
    \\  out_color = u_color;
    \\}
    ,
};

pub fn init(gl: Gl, gpa: std.mem.Allocator, comptime font_jsons: []const []const u8, font_atlases: []const *const anyopaque) !Canvas {
    assert(font_jsons.len == font_atlases.len);
    const text_renderers = try gpa.alloc(TextRenderer, font_jsons.len);
    inline for (font_jsons, font_atlases, text_renderers) |json, atlas, *dst| {
        dst.* = try .init(json, gpa, gl, atlas);
    }

    return .{
        .gl = gl,
        .frame_arena = .init(gpa),
        .sprite_renderable = try gl.buildRenderable(
            sprite_renderable_vertex_src,
            sprite_renderable_frag_src,
            .{ .attribs = &.{
                .{ .name = "a_position", .kind = .Vec2 },
                .{ .name = "a_texcoord", .kind = .Vec2 },
                .{ .name = "a_color", .kind = .FColor },
            } },
            &.{
                .{ .name = "u_camera", .kind = .Rect },
            },
        ),
        .fill_shape_vertex_colors_renderable = try gl.buildRenderable(
            \\precision highp float;
            \\uniform vec4 u_camera; // as top_left, size
            \\uniform vec4 u_point; // as pos, turns, scale
            \\
            \\in vec2 a_position;
            \\in vec4 a_color;
            \\out vec4 v_color;
            \\#define TAU 6.283185307179586
            \\void main() {
            \\  float c = cos(u_point.z * TAU);
            \\  float s = sin(u_point.z * TAU);
            \\  vec2 world_position = u_point.xy + u_point.w * (mat2x2(c,s,-s,c) * a_position);
            \\  vec2 camera_position = (world_position - u_camera.xy) / u_camera.zw;
            \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
            \\  v_color = a_color;
            \\}
        ,
            \\precision highp float;
            \\out vec4 out_color;
            \\in vec4 v_color;
            \\void main() {
            \\  out_color = v_color;
            \\}
        ,
            .{ .attribs = &.{
                .{ .name = "a_position", .kind = .Vec2 },
                .{ .name = "a_color", .kind = .FColor },
            } },
            &.{
                .{ .name = "u_camera", .kind = .Rect },
                .{ .name = "u_point", .kind = .Point },
            },
        ),
        .fill_shape_renderable = try gl.buildRenderable(
            fill_shape_info.vertex,
            fill_shape_info.fragment,
            .{ .attribs = &.{
                .{ .name = "a_position", .kind = .Vec2 },
            } },
            &.{
                .{ .name = "u_camera", .kind = .Rect },
                .{ .name = "u_point", .kind = .Point },
                .{ .name = "u_color", .kind = .FColor },
            },
        ),
        .fill_instanced_shapes_renderable = try gl.buildInstancedRenderable(
            \\precision highp float;
            \\uniform vec4 u_camera; // as top_left, size
            \\#define TAU 6.283185307179586
            \\in vec2 a_vertex_position;
            \\in vec4 a_point;
            \\in vec4 a_color;
            \\out vec4 v_color;
            \\void main() {
            \\  float c = cos(a_point.z * TAU);
            \\  float s = sin(a_point.z * TAU);
            \\  vec2 world_position = a_point.xy + a_point.w * (mat2x2(c,s,-s,c) * a_vertex_position);
            \\  vec2 camera_position = (world_position - u_camera.xy) / u_camera.zw;
            \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
            \\  v_color = a_color;
            \\}
        ,
            \\precision highp float;
            \\out vec4 out_color;
            \\in vec4 v_color;
            \\void main() {
            \\  out_color = v_color;
            \\}
        ,
            .{ .attribs = &.{
                .{ .name = "a_vertex_position", .kind = .Vec2 },
            } },
            .{ .attribs = &.{
                .{ .name = "a_point", .kind = .Point },
                .{ .name = "a_color", .kind = .FColor },
            } },
            &.{
                .{ .name = "u_camera", .kind = .Rect },
            },
        ),
        .fill_instanced_circles_renderable = try gl.buildInstancedRenderable(
            \\precision highp float;
            \\uniform vec4 u_camera; // as top_left, size
            \\
            \\in vec2 a_vertex_position;
            \\in vec2 a_center;
            \\void main() {
            \\  vec2 camera_position = (a_center + a_vertex_position - u_camera.xy) / u_camera.zw;
            \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
            \\}
        ,
            \\precision highp float;
            \\out vec4 out_color;
            \\
            \\void main() {
            \\  out_color = vec4(1.0);
            \\}
        ,
            .{ .attribs = &.{
                .{ .name = "a_vertex_position", .kind = .Vec2 },
            } },
            .{ .attribs = &.{
                .{ .name = "a_center", .kind = .Vec2 },
            } },
            &.{
                .{ .name = "u_camera", .kind = .Rect },
            },
        ),
        .instanced_rounded_lines_renderable = try gl.buildInstancedRenderable(
            \\precision highp float;
            \\uniform vec4 u_camera; // as top_left, size
            \\uniform float u_width;
            \\in vec2 a_vertex_position;
            \\in float a_vertex_is_b_side;
            \\in vec2 a_line_a;
            \\in vec2 a_line_b;
            \\void main() {
            \\  vec2 basis_x = normalize(a_line_b - a_line_a);
            \\  vec2 basis_y = vec2(-basis_x.y, basis_x.x);
            \\  vec2 world_position = mix(a_line_a, a_line_b, a_vertex_is_b_side) + u_width * (a_vertex_position.x * basis_x + a_vertex_position.y * basis_y);
            \\  vec2 camera_position = (world_position - u_camera.xy) / u_camera.zw;
            \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
            \\}
        ,
            \\precision highp float;
            \\out vec4 out_color;
            \\uniform vec4 u_color;
            \\void main() {
            \\  out_color = u_color;
            \\}
        ,
            .{ .attribs = &.{
                .{ .name = "a_vertex_position", .kind = .Vec2 },
                .{ .name = "a_vertex_is_b_side", .kind = .f32 },
            } },
            .{ .attribs = &.{
                .{ .name = "a_line_a", .kind = .Vec2 },
                .{ .name = "a_line_b", .kind = .Vec2 },
            }, .stride = @sizeOf(Vec2) },
            &.{
                .{ .name = "u_camera", .kind = .Rect },
                .{ .name = "u_width", .kind = .f32 },
                .{ .name = "u_color", .kind = .FColor },
            },
        ),
        .instanced_colored_separated_rounded_lines_renderable = try gl.buildInstancedRenderable(
            \\precision highp float;
            \\uniform vec4 u_camera; // as top_left, size
            \\uniform float u_width;
            \\in vec2 a_vertex_position;
            \\in float a_vertex_is_b_side;
            \\in vec2 a_line_a;
            \\in vec2 a_line_b;
            \\in vec4 a_line_color;
            \\out vec4 v_line_color;
            \\void main() {
            \\  vec2 basis_x = normalize(a_line_b - a_line_a);
            \\  vec2 basis_y = vec2(-basis_x.y, basis_x.x);
            \\  vec2 world_position = mix(a_line_a, a_line_b, a_vertex_is_b_side) + u_width * (a_vertex_position.x * basis_x + a_vertex_position.y * basis_y);
            \\  vec2 camera_position = (world_position - u_camera.xy) / u_camera.zw;
            \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
            \\  v_line_color = a_line_color;
            \\}
        ,
            \\precision highp float;
            \\out vec4 out_color;
            \\in vec4 v_line_color;
            \\void main() {
            \\  out_color = v_line_color;
            \\}
        ,
            .{ .attribs = &.{
                .{ .name = "a_vertex_position", .kind = .Vec2 },
                .{ .name = "a_vertex_is_b_side", .kind = .f32 },
            } },
            .{ .attribs = &.{
                .{ .name = "a_line_a", .kind = .Vec2 },
                .{ .name = "a_line_b", .kind = .Vec2 },
                .{ .name = "a_line_color", .kind = .FColor },
            } },
            &.{
                .{ .name = "u_camera", .kind = .Rect },
                .{ .name = "u_width", .kind = .f32 },
            },
        ),
        .fill_shapes_renderable = try gl.buildRenderable(
            \\precision highp float;
            \\uniform vec4 u_camera; // as top_left, size
            \\
            \\in vec2 a_position;
            \\in vec4 a_color;
            \\out vec4 v_color;
            \\void main() {
            \\  vec2 world_position = a_position;
            \\  vec2 camera_position = (world_position - u_camera.xy) / u_camera.zw;
            \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
            \\  v_color = a_color;
            \\}
        ,
            \\precision highp float;
            \\out vec4 out_color;
            \\in vec4 v_color;
            \\void main() {
            \\  out_color = v_color;
            \\}
        ,
            .{ .attribs = &.{
                .{ .name = "a_position", .kind = .Vec2 },
                .{ .name = "a_color", .kind = .FColor },
            } },
            &.{
                .{ .name = "u_camera", .kind = .Rect },
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
    self.frame_arena.deinit();
    _ = gl;
}

pub fn drawDirty(
    self: *const Canvas,
    comptime renderable_info: struct {
        /// without preamble!
        vertex_src: [:0]const u8,
        /// without preamble!
        fragment_src: [:0]const u8,
        attributes: Gl.VertexInfo.Collection,
        uniforms: []const Gl.UniformInfo.In,
    },
    asdf: struct {
        vertices_ptr: *const anyopaque,
        vertices_len_bytes: usize,
        triangles: []const [3]Gl.IndexType,
        uniforms: []const Gl.UniformInfo.Runtime,
        texture: ?Gl.Texture,
    },
) !void {
    const S = struct {
        pub var renderable: ?Gl.Renderable = null;
    };
    if (S.renderable == null) {
        S.renderable = try self.gl.buildRenderable(
            renderable_info.vertex_src,
            renderable_info.fragment_src,
            renderable_info.attributes,
            renderable_info.uniforms,
        );
        std.log.warn("built a renderable", .{});
    }
    self.gl.useRenderable(
        S.renderable.?,
        asdf.vertices_ptr,
        asdf.vertices_len_bytes,
        asdf.triangles,
        asdf.uniforms,
        asdf.texture,
    );
}

pub const ShapesBatch = struct {
    const VertexData = extern struct {
        a_position: Vec2,
        a_color: FColor,
    };
    const IndexType = u32;

    canvas: *Canvas,
    vertices: std.ArrayListUnmanaged(VertexData) = .empty,
    triangles: std.ArrayListUnmanaged([3]IndexType) = .empty,

    pub fn add(self: *ShapesBatch, parent_world_point: Point, shape: PrecomputedShape, fill: FColor) !void {
        const base_index: IndexType = @intCast(self.vertices.items.len);
        try self.vertices.ensureUnusedCapacity(self.canvas.frame_arena.allocator(), shape.local_points.len);
        for (shape.local_points) |p| {
            self.vertices.appendAssumeCapacity(.{
                .a_color = fill,
                .a_position = parent_world_point.applyToLocalPosition(p),
            });
        }
        try self.triangles.ensureUnusedCapacity(self.canvas.frame_arena.allocator(), shape.triangles.len * 3);
        for (shape.triangles) |t| {
            self.triangles.appendAssumeCapacity(.{
                t[0] + base_index,
                t[1] + base_index,
                t[2] + base_index,
            });
        }
    }

    pub fn draw(self: *ShapesBatch, camera: Rect) void {
        self.canvas.gl.useRenderable(
            self.canvas.fill_shapes_renderable,
            self.vertices.items.ptr,
            self.vertices.items.len * @sizeOf(VertexData),
            self.triangles.items,
            &.{
                .{ .name = "u_camera", .value = .{ .Rect = camera } },
            },
            null,
        );
        self.triangles.clearRetainingCapacity();
        self.vertices.clearRetainingCapacity();
    }
};

pub fn fillShapeWithVertexColors(
    self: *Canvas,
    camera: Rect,
    parent: Point,
    shape: PrecomputedShape,
    colors: []const FColor,
) void {
    assert(colors.len == shape.local_points.len);
    const VertexData = extern struct {
        a_position: Vec2,
        a_color: FColor,
    };
    const actual_points: []VertexData = self.frame_arena.allocator().alloc(
        VertexData,
        shape.local_points.len,
    ) catch @panic("OoM");
    for (actual_points, shape.local_points, colors) |*dst, p, c| {
        dst.a_color = c;
        dst.a_position = p;
    }
    self.gl.useRenderable(
        self.fill_shape_vertex_colors_renderable,
        actual_points.ptr,
        actual_points.len * @sizeOf(VertexData),
        shape.triangles,
        &.{
            .{ .name = "u_point", .value = .{ .Point = parent } },
            .{ .name = "u_camera", .value = .{ .Rect = camera } },
        },
        null,
    );
}

// TODO: multiple shapes in one draw call
// maybe by having this hidden behind a batch API
pub fn fillShape(
    self: *const Canvas,
    camera: Rect,
    parent: Point,
    shape: PrecomputedShape,
    color: FColor,
) void {
    if (shape.fill_shape_renderable) |renderable| {
        self.gl.useRenderableWithExistingData(
            renderable,
            shape.triangles.len,
            &.{
                .{ .name = "u_color", .value = .{ .FColor = color } },
                .{ .name = "u_point", .value = .{ .Point = parent } },
                .{ .name = "u_camera", .value = .{ .Rect = camera } },
            },
            null,
        );
    } else {
        self.gl.useRenderable(
            self.fill_shape_renderable,
            shape.local_points.ptr,
            shape.local_points.len * @sizeOf(Vec2),
            shape.triangles,
            &.{
                .{ .name = "u_color", .value = .{ .FColor = color } },
                .{ .name = "u_point", .value = .{ .Point = parent } },
                .{ .name = "u_camera", .value = .{ .Rect = camera } },
            },
            null,
        );
    }
}

pub fn fillCircleV2(
    self: *const Canvas,
    camera: Rect,
    circle: math.Circle,
    color: FColor,
) void {
    return self.fillCircle(camera, circle.center, circle.radius, color);
}

pub fn fillCircle(
    self: *const Canvas,
    camera: Rect,
    center: Vec2,
    radius: f32,
    color: FColor,
) void {
    self.fillShape(
        camera,
        .{ .pos = center, .scale = radius },
        self.DEFAULT_SHAPES.circle_128,
        color,
    );
}

pub fn fillSquare(
    self: *const Canvas,
    camera: Rect,
    top_left: Vec2,
    side: f32,
    color: FColor,
) void {
    self.fillShape(
        camera,
        .{ .pos = top_left, .scale = side },
        self.DEFAULT_SHAPES.square,
        color,
    );
}

pub const SpriteSheet = struct {
    count: UVec2,
    margin_px: usize,
    resolution: UVec2,

    pub fn at(self: SpriteSheet, k: usize) Rect {
        return self.atV(.new(
            @mod(k, self.count.x),
            @divFloor(k, self.count.x),
        ));
    }

    pub fn atV(self: SpriteSheet, v: UVec2) Rect {
        return .fromSpriteSheet(v, self.count, Vec2.both(@floatFromInt(self.margin_px)).div(self.resolution.tof32()));
    }
};

pub const Sprite = struct {
    point: Point,
    pivot: Rect.MeasureKind = .top_left,
    // which axis should have length 1 in a non-square texture?
    unit_scale_is: enum { hor, ver } = .ver,
    texcoord: Rect,
    tint: FColor = .white,

    pub fn verticesFromSprite(sprite: Sprite) [4]SpriteVertex {
        var vertices: [4]SpriteVertex = undefined;
        for ([4]Vec2{ .zero, .e1, .e2, .one }, 0..4) |vertex, k| {
            if (sprite.pivot != .top_left) @panic("TODO: other pivots");
            vertices[k] = .{
                .a_position = sprite.point.applyToLocalPosition(vertex),
                .a_texcoord = sprite.texcoord.applyToLocalPosition(vertex),
                .a_color = sprite.tint,
            };
        }
        return vertices;
    }
};

pub const SpriteVertex = extern struct { a_position: Vec2, a_texcoord: Vec2, a_color: FColor };

pub const SpriteBatch = struct {
    canvas: *Canvas,
    sprites: std.ArrayListUnmanaged(Sprite),
    texture: Gl.Texture,

    pub fn add(self: *SpriteBatch, sprite: Sprite) void {
        self.sprites.append(self.canvas.frame_arena.allocator(), sprite) catch @panic("OoM");
    }

    pub fn draw(self: *SpriteBatch, camera: Rect) void {
        self.canvas.drawSpriteBatch(camera, self.sprites.items, self.texture);
        self.sprites.clearRetainingCapacity();
    }
};

pub fn spriteBatch(
    self: *Canvas,
    texture: Gl.Texture,
) SpriteBatch {
    return .{
        .canvas = self,
        .texture = texture,
        .sprites = .empty,
    };
}

// TODO: instancing?
pub fn drawSpriteBatch(
    self: *Canvas,
    camera: Rect,
    sprites: []const Sprite,
    texture: Gl.Texture,
) void {
    const VertexData = SpriteVertex;
    const vertices = self.frame_arena.allocator().alloc(VertexData, 4 * sprites.len) catch @panic("OoM");
    const triangles = self.frame_arena.allocator().alloc([3]Gl.IndexType, 2 * sprites.len) catch @panic("OoM");
    for (sprites, 0..) |sprite, i| {
        for ([4]Vec2{ .zero, .e1, .e2, .one }, 0..4) |vertex, k| {
            const ratio: f32 = texture.resolution.tof32().mul(sprite.texcoord.size).aspectRatio();
            if (sprite.unit_scale_is != .ver) @panic("TODO");
            const ratio_scaling: Vec2 = .new(ratio, 1.0);
            const top_left_point: Point = sprite.point.applyToLocalPoint(.{ .pos = sprite.pivot.asPivot().neg().mul(ratio_scaling) });
            vertices[i * 4 + k] = .{
                .a_position = top_left_point.applyToLocalPosition(vertex.mul(ratio_scaling)),
                .a_texcoord = sprite.texcoord.applyToLocalPosition(vertex),
                .a_color = sprite.tint,
            };
        }
        const k: Gl.IndexType = @intCast(4 * i);
        triangles[i * 2 + 0] = .{ k + 0, k + 1, k + 2 };
        triangles[i * 2 + 1] = .{ k + 3, k + 2, k + 1 };
    }
    self.gl.useRenderable(
        self.sprite_renderable,
        vertices.ptr,
        vertices.len * @sizeOf(VertexData),
        // .local_points = &.{ .zero, .e1, .e2, .one },
        triangles,
        &.{
            .{ .name = "u_camera", .value = .{ .Rect = camera } },
        },
        texture,
    );
}

/// assumes the cuts are at 1/3, 2/3
pub fn sliced3x3(
    rect: Rect,
    target_margin: f32,
) [9]TexturedRect {
    var res: [9]TexturedRect = undefined;
    for (&res, Rect.MeasureKind.all9) |*dst, keep| {
        dst.* = .{
            .rect = rect.with(
                // .{ .size = .both(target_margin) },
                // .{ .size = .new(rect.size.x - 2.0 * target_margin, rect.size.y - 2.0 * target_margin) },
                .{
                    .size = .new(
                        // if (keep.horizontal() =)
                        if (keep.asPivot().x == 0.5) rect.size.x - 2.0 * target_margin else target_margin,
                        if (keep.asPivot().y == 0.5) rect.size.y - 2.0 * target_margin else target_margin,
                    ),
                },
                keep,
            ),
            .texcoord = Rect.unit.with(
                .{ .size = .both(1.0 / 3.0) },
                keep,
            ),
        };
    }
    if (true) return res;

    return .{
        .{ .rect = rect.with(
            .{ .size = .both(target_margin) },
            .top_left,
        ), .texcoord = Rect.unit.with(
            .{ .size = .both(1.0 / 3.0) },
            .top_left,
        ) },
        .{ .rect = rect.with(
            .{ .size = .new(rect.size.x - 2.0 * target_margin, target_margin) },
            .top_center,
        ), .texcoord = Rect.unit.with(
            .{ .size = .both(1.0 / 3.0) },
            .top_center,
        ) },
        .{ .rect = rect.with(
            .{ .size = .new(rect.size.x - 2.0 * target_margin, rect.size.y - 2.0 * target_margin) },
            .center,
        ), .texcoord = Rect.unit.with(
            .{ .size = .both(1.0 / 3.0) },
            .center,
        ) },
    };
}

pub const TexturedRect = struct {
    rect: Rect,
    texcoord: Rect,
    tint: FColor = .white,
};

// TODO: instancing?
pub fn drawTexturedRectBatch(
    self: *Canvas,
    camera: Rect,
    sprites: []const TexturedRect,
    texture: Gl.Texture,
) void {
    const VertexData = SpriteVertex;
    const vertices = self.frame_arena.allocator().alloc(VertexData, 4 * sprites.len) catch @panic("OoM");
    const triangles = self.frame_arena.allocator().alloc([3]Gl.IndexType, 2 * sprites.len) catch @panic("OoM");
    for (sprites, 0..) |sprite, i| {
        for ([4]Vec2{ .zero, .e1, .e2, .one }, 0..4) |vertex, k| {
            vertices[i * 4 + k] = .{
                .a_position = sprite.rect.applyToLocalPosition(vertex),
                .a_texcoord = sprite.texcoord.applyToLocalPosition(vertex),
                .a_color = sprite.tint,
            };
        }
        const k: Gl.IndexType = @intCast(4 * i);
        triangles[i * 2 + 0] = .{ k + 0, k + 1, k + 2 };
        triangles[i * 2 + 1] = .{ k + 3, k + 2, k + 1 };
    }
    self.gl.useRenderable(
        self.sprite_renderable,
        vertices.ptr,
        vertices.len * @sizeOf(VertexData),
        triangles,
        &.{
            .{ .name = "u_camera", .value = .{ .Rect = camera } },
        },
        texture,
    );
}

pub const DrawableTriangulation = struct {
    vertices_per_sprite: usize,
    triangles_per_sprite: usize,
    sprite_triangles: []const [3]Gl.IndexType,

    // TODO: from triangles
};

pub const sprite_triangulation: DrawableTriangulation = .{
    .vertices_per_sprite = 4,
    .triangles_per_sprite = 2,
    .sprite_triangles = &.{
        .{ 0, 1, 2 },
        .{ 3, 2, 1 },
    },
};

pub fn Drawable(
    VertexData: type,
    SpriteData: type,
    UniformData: type,
    triangulation: DrawableTriangulation,
    verticesFromSprite: fn (sprite: SpriteData) [triangulation.vertices_per_sprite]VertexData,
    vertex_src: [:0]const u8,
    fragment_src: [:0]const u8,
) type {
    return struct {
        const Self = @This();

        pub const Batch = struct {
            drawable: *const Self,
            sprites: std.ArrayListUnmanaged(SpriteData),

            pub fn add(self: *Batch, sprite: SpriteData) void {
                self.sprites.append(self.drawable.canvas.frame_arena.allocator(), sprite) catch @panic("OoM");
            }

            pub fn draw(self: *Batch, uniforms: UniformData, texture: ?Gl.Texture) void {
                self.drawable.draw(self.sprites.items, uniforms, texture);
                self.sprites.clearRetainingCapacity();
            }
        };

        canvas: *Canvas,
        renderable: Gl.Renderable,

        pub fn batch(self: *const Self) Batch {
            return .{
                .drawable = self,
                .sprites = .empty,
            };
        }

        pub fn reload(self: *Self) !void {
            self.* = try .init(self.canvas);
        }

        pub fn init(canvas: *Canvas) !Self {
            const vertex_info = @typeInfo(VertexData).@"struct";
            assert(vertex_info.layout == .@"extern");
            var attributes: [vertex_info.fields.len]Gl.VertexInfo.In = undefined;
            inline for (&attributes, vertex_info.fields) |*dst, info| {
                dst.* = .{ .name = info.name, .kind = .fromType(info.type) };
            }

            const uniform_info = @typeInfo(UniformData).@"struct";
            var uniforms: [uniform_info.fields.len]Gl.UniformInfo.In = undefined;
            inline for (&uniforms, uniform_info.fields) |*dst, info| {
                dst.* = .{ .name = info.name, .kind = .fromType(info.type) };
            }

            return .{
                .canvas = canvas,
                .renderable = try canvas.gl.buildRenderable(
                    vertex_src,
                    fragment_src,
                    .{ .attribs = &attributes },
                    &uniforms,
                ),
            };
        }

        pub fn draw(self: Self, sprites: []const SpriteData, uniforms: UniformData, texture: ?Gl.Texture) void {
            const scratch = self.canvas.frame_arena.allocator();
            const vertices = scratch.alloc(VertexData, triangulation.vertices_per_sprite * sprites.len) catch @panic("OoM");
            const triangles = scratch.alloc([3]Gl.IndexType, triangulation.triangles_per_sprite * sprites.len) catch @panic("OoM");
            for (sprites, 0..) |sprite, i| {
                const local_vertices = verticesFromSprite(sprite);
                const N = triangulation.vertices_per_sprite;
                for (local_vertices, vertices[i * N ..][0..N]) |v, *dst| {
                    dst.* = v;
                }
                const base: Gl.IndexType = @intCast(i * N);
                const K = triangulation.triangles_per_sprite;
                for (triangulation.sprite_triangles, triangles[i * K ..][0..K]) |t, *dst| {
                    dst.* = .{ base + t[0], base + t[1], base + t[2] };
                }
            }

            const uniform_info = @typeInfo(UniformData).@"struct";
            var uniforms_gl: [uniform_info.fields.len]Gl.UniformInfo.Runtime = undefined;
            inline for (&uniforms_gl, uniform_info.fields) |*dst, info| {
                dst.* = .{ .name = info.name, .value = .fromValue(@field(uniforms, info.name)) };
            }

            self.canvas.gl.useRenderable(
                self.renderable,
                vertices.ptr,
                vertices.len * @sizeOf(VertexData),
                triangles,
                &uniforms_gl,
                texture,
            );
        }
    };
}

// pub fn sprite(
//     self: *const Canvas,
//     camera: Rect,
//     point: Point,
//     pivot: Rect.MeasureKind,
//     texcoord: Rect,
//     texture: Gl.Texture,
//     tint: ?FColor,
// ) void {
//     if (pivot != .top_left) @panic("TODO: other pivots");
//     const VertexData = extern struct { pos: Vec2, texcoord: Vec2, color: FColor };
//     const vertices: [4]VertexData = .{ .{
//         .pos = point.applyToLocalPosition(.zero),
//         .texcoord = texcoord.applyToLocalPosition(.zero),
//         .color = tint orelse .white,
//     }, .{
//         .pos = point.applyToLocalPosition(.e1),
//         .texcoord = texcoord.applyToLocalPosition(.e1),
//         .color = tint orelse .white,
//     }, .{
//         .pos = point.applyToLocalPosition(.e2),
//         .texcoord = texcoord.applyToLocalPosition(.e2),
//         .color = tint orelse .white,
//     }, .{
//         .pos = point.applyToLocalPosition(.one),
//         .texcoord = texcoord.applyToLocalPosition(.one),
//         .color = tint orelse .white,
//     } };
//     self.gl.useRenderable(
//         self.sprite_renderable,
//         &vertices,
//         4 * @sizeOf(VertexData),
//         // .local_points = &.{ .zero, .e1, .e2, .one },
//         &.{ .{ 0, 1, 2 }, .{ 3, 2, 1 } },
//         &.{
//             .{ .name = "u_camera", .value = .{ .Rect = camera } },
//         },
//         texture,
//     );
// }

pub fn fillRectWithRoundCorners(
    self: *const Canvas,
    camera: Rect,
    rect: Rect,
    circle_radius: f32,
    color: FColor,
) void {
    self.fillRect(camera, rect.plusMargin2(.new(0, -circle_radius)), color);
    self.fillRect(camera, rect.plusMargin2(.new(-circle_radius, 0)), color);
    inline for (&.{ .top_left, .top_right, .bottom_left, .bottom_right }) |e| {
        self.fillCircle(camera, rect.plusMargin(-circle_radius).get(e), circle_radius, color);
    }
}

pub fn fillRect(
    self: *const Canvas,
    camera: Rect,
    rect: Rect,
    color: FColor,
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
            .fill_shape_renderable = null,
        },
        color,
    );
}

pub fn borderRect(
    self: *const Canvas,
    camera: Rect,
    rect: Rect,
    width: f32,
    mode: enum { inner, middle, outer },
    color: FColor,
) void {
    const t: f32 = switch (mode) {
        .inner => 0,
        .middle => 0.5,
        .outer => 1,
    };

    inline for (
        [4]Vec2{ .new(-1, -1), .new(1, -1), .new(1, 1), .new(-1, 1) },
        0..,
        .{ rect.size.x, rect.size.y, rect.size.x, rect.size.y },
    ) |corner_dir, k, longitud| {
        const top_left = rect.worldFromCenterLocal(corner_dir).add(corner_dir.scale(t * width));
        const long_dim = longitud + width * std.math.lerp(-1, 1, t);
        const short_dim = width;

        self.fillRect(camera, .{ .top_left = top_left, .size = Vec2.new(long_dim, short_dim).rotQuarters(@intCast(k)) }, color);
    }
}

pub fn strokeRect(
    self: *const Canvas,
    camera: Rect,
    rect: Rect,
    width: f32,
    color: FColor,
) void {
    self.line(
        camera,
        &.{
            rect.top_left,
            rect.get(.top_right),
            rect.get(.bottom_right),
            rect.get(.bottom_left),
            rect.top_left,
        },
        width,
        color,
    );
}

pub fn strokeCircle(
    self: *const Canvas,
    comptime resolution: usize,
    camera: Rect,
    center: Vec2,
    radius: f32,
    width: f32,
    color: FColor,
) void {
    self.line(
        camera,
        &funk.mapWithCtx(
            struct {
                pub fn anon(ctx: struct { radius: f32, center: Vec2 }, t: f32) Vec2 {
                    return ctx.center.add(.fromPolar(ctx.radius, t));
                }
            }.anon,
            &funk.linspace01(resolution, true),
            .{ .radius = radius, .center = center },
        ),
        width,
        color,
    );
}

pub fn fillArc(self: *Canvas, camera: Rect, center: Vec2, radius: f32, turns_start: f32, turns_end: f32, color: FColor) !void {
    self.fillShape(camera, .{
        .pos = center,
        .scale = radius,
    }, try self.tmpShape(&(funk.mapWithCtx(
        struct {
            pub fn anon(ctx: struct { min: f32, max: f32 }, t: f32) Vec2 {
                return .fromTurns(std.math.lerp(ctx.min, ctx.max, t));
            }
        }.anon,
        &funk.linspace01(128, true),
        .{ .min = turns_start, .max = turns_end },
    ) ++ [1]Vec2{.zero})), color);
}

pub fn fillCrown(self: *Canvas, camera: Rect, center: Vec2, radius: f32, width: f32, color: FColor) !void {
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

pub const InstancedShapeInfo = extern struct {
    point: Point,
    color: FColor,
};

pub fn fillShapesInstanced(
    self: *const Canvas,
    camera: Rect,
    shape: PrecomputedShape,
    instances: []const InstancedShapeInfo,
) void {
    self.gl.useInstancedRenderable(
        self.fill_instanced_shapes_renderable,
        shape.local_points.ptr,
        shape.local_points.len * @sizeOf(Vec2),
        shape.triangles,
        instances.ptr,
        instances.len * @sizeOf(InstancedShapeInfo),
        instances.len,
        &.{
            .{ .name = "u_camera", .value = .{ .Rect = camera } },
        },
        null,
    );
}

pub fn fillInstancedCircles(
    self: *Canvas,
    camera: Rect,
    points: []const Vec2,
) void {
    self.gl.useInstancedRenderable(
        self.fill_instanced_circles_renderable,
        self.DEFAULT_SHAPES.circle_128.local_points.ptr,
        self.DEFAULT_SHAPES.circle_128.local_points.len * @sizeOf(Vec2),
        self.DEFAULT_SHAPES.circle_128.triangles,
        points.ptr,
        points.len * @sizeOf(Vec2),
        points.len,
        &.{
            .{ .name = "u_camera", .value = .{ .Rect = camera } },
        },
        null,
    );
}

// // https://wwwtyro.net/2019/11/18/instanced-lines.html
pub fn line(
    self: *const Canvas,
    camera: Rect,
    points: []const Vec2,
    world_width: f32,
    color: FColor,
) void {
    const VertexData = extern struct {
        a_vertex_position: Vec2,
        a_vertex_is_b_side: f32,
    };
    // TODO
    const rounded_line_data: []const VertexData = &.{
        // basic rect
        .{ .a_vertex_position = .new(0, -0.5), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(0, -0.5), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = .new(0, 0.5), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(0, 0.5), .a_vertex_is_b_side = 1 },

        // rounded cap for a
        .{ .a_vertex_position = .new(0, 0), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(0, -0.5), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = Vec2.new(-0.5, -0.5).scale(1.0 / @sqrt(2.0)), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(-0.5, 0), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = Vec2.new(-0.5, 0.5).scale(1.0 / @sqrt(2.0)), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(0, 0.5), .a_vertex_is_b_side = 0 },

        // rounded cap for b
        .{ .a_vertex_position = .new(0, 0), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = .new(0, -0.5), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = Vec2.new(0.5, -0.5).scale(1.0 / @sqrt(2.0)), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = .new(0.5, 0), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = Vec2.new(0.5, 0.5).scale(1.0 / @sqrt(2.0)), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = .new(0, 0.5), .a_vertex_is_b_side = 1 },
    };
    self.gl.useInstancedRenderable(
        self.instanced_rounded_lines_renderable,
        rounded_line_data.ptr,
        rounded_line_data.len * @sizeOf(VertexData),
        &.{
            // basic rect
            .{ 0, 1, 2 },
            .{ 3, 2, 1 },

            // rounded cap for a
            .{ 4, 5, 6 },
            .{ 4, 6, 7 },
            .{ 4, 7, 8 },
            .{ 4, 8, 9 },

            // rounded cap for b
            .{ 10, 11, 12 },
            .{ 10, 12, 13 },
            .{ 10, 13, 14 },
            .{ 10, 14, 15 },
        },
        points.ptr,
        points.len * @sizeOf(Vec2),
        points.len - 1,
        &.{
            .{ .name = "u_camera", .value = .{ .Rect = camera } },
            .{ .name = "u_width", .value = .{ .f32 = world_width } },
            .{ .name = "u_color", .value = .{ .FColor = color } },
        },
        null,
    );
}

pub const Segment = extern struct { a: Vec2, b: Vec2, color: FColor };
pub fn instancedSegments(
    self: *const Canvas,
    camera: Rect,
    segments: []const Segment,
    world_width: f32,
) void {
    const VertexData = extern struct {
        a_vertex_position: Vec2,
        a_vertex_is_b_side: f32,
    };
    // TODO
    const rounded_line_data: []const VertexData = &.{
        // basic rect
        .{ .a_vertex_position = .new(0, -0.5), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(0, -0.5), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = .new(0, 0.5), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(0, 0.5), .a_vertex_is_b_side = 1 },

        // rounded cap for a
        .{ .a_vertex_position = .new(0, 0), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(0, -0.5), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = Vec2.new(-0.5, -0.5).scale(1.0 / @sqrt(2.0)), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(-0.5, 0), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = Vec2.new(-0.5, 0.5).scale(1.0 / @sqrt(2.0)), .a_vertex_is_b_side = 0 },
        .{ .a_vertex_position = .new(0, 0.5), .a_vertex_is_b_side = 0 },

        // rounded cap for b
        .{ .a_vertex_position = .new(0, 0), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = .new(0, -0.5), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = Vec2.new(0.5, -0.5).scale(1.0 / @sqrt(2.0)), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = .new(0.5, 0), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = Vec2.new(0.5, 0.5).scale(1.0 / @sqrt(2.0)), .a_vertex_is_b_side = 1 },
        .{ .a_vertex_position = .new(0, 0.5), .a_vertex_is_b_side = 1 },
    };
    self.gl.useInstancedRenderable(
        self.instanced_colored_separated_rounded_lines_renderable,
        rounded_line_data.ptr,
        rounded_line_data.len * @sizeOf(VertexData),
        &.{
            // basic rect
            .{ 0, 1, 2 },
            .{ 3, 2, 1 },

            // rounded cap for a
            .{ 4, 5, 6 },
            .{ 4, 6, 7 },
            .{ 4, 7, 8 },
            .{ 4, 8, 9 },

            // rounded cap for b
            .{ 10, 11, 12 },
            .{ 10, 12, 13 },
            .{ 10, 13, 14 },
            .{ 10, 14, 15 },
        },
        segments.ptr,
        segments.len * @sizeOf(Segment),
        segments.len,
        &.{
            .{ .name = "u_camera", .value = .{ .Rect = camera } },
            .{ .name = "u_width", .value = .{ .f32 = world_width } },
        },
        null,
    );
}

pub fn rectGradient(
    self: *Canvas,
    camera: Rect,
    rect: Rect,
    bottom: FColor,
    top: FColor,
) void {
    self.fillShapeWithVertexColors(
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
            .fill_shape_renderable = null,
        },
        &.{ top, top, bottom, bottom },
    );
}

pub const FilledRect = extern struct { pos: Rect, color: FColor };
pub fn fillRects(
    self: *const Canvas,
    camera: Rect,
    rects: []const FilledRect,
) void {
    // TODO: instanced
    for (rects) |r| self.fillRect(camera, r.pos, r.color);
}

/// Performs triangulation; consider caching the result.
pub fn tmpShape(
    self: *Canvas,
    local_points: []const Vec2,
) !PrecomputedShape {
    return try .fromPoints(self.frame_arena.allocator(), local_points, null);
}

pub fn startFrame(self: *Canvas, gl: Gl) void {
    self.gl = gl;
    _ = self.frame_arena.reset(.retain_capacity);
}

// TODO: baselines, etc
pub const TextBatch = struct {
    quads: std.ArrayList(TextRenderer.Quad),
    canvas: *Canvas,
    text_renderer: *TextRenderer,

    pub fn addFmt(
        self: *TextBatch,
        comptime fmt: []const u8,
        fmt_args: anytype,
        pos: TextRenderer.TextPosition,
        em: f32,
        color: FColor,
    ) !void {
        const text = try std.fmt.allocPrint(self.canvas.frame_arena.allocator(), fmt, fmt_args);
        defer self.canvas.frame_arena.allocator().free(text);
        try self.addText(text, pos, em, color);
    }

    pub fn addText(
        self: *TextBatch,
        text: []const u8,
        pos: TextRenderer.TextPosition,
        em: f32,
        color: FColor,
    ) !void {
        std.log.debug("in addText, text.len is: {d}", .{text.len});
        if (std.mem.indexOf(u8, text, "\n") != null) std.debug.panic("unexpected line break in addText: {s}", .{text});
        const info = try self.text_renderer.quadsForLineV2(text, em, color, self.canvas.frame_arena.allocator());
        const quads = info.quads;
        defer self.canvas.frame_arena.allocator().free(quads);
        if (quads.len == 0) return;
        // const delta = bounds.deltaToAchieve(pos);
        const delta = self.text_renderer.deltaToAchieve(pos, info.total_advance, em);
        try self.quads.ensureUnusedCapacity(quads.len);
        for (quads) |q| self.quads.appendAssumeCapacity(q.translate(delta));
    }

    pub fn draw(self: *TextBatch, camera: Rect) void {
        self.text_renderer.drawQuads(self.canvas.gl, camera, self.quads.items, self.canvas.frame_arena.allocator());
        self.quads.clearAndFree();
    }
};

/// prefer .textBatch
pub fn drawText(self: *Canvas, font_index: usize, camera: Rect, text: []const u8, pos: TextRenderer.TextPosition, em: f32, color: FColor) !void {
    var batch = self.textBatch(font_index);
    try batch.addText(text, pos, em, color);
    batch.draw(camera);
}

pub fn textBatch(self: *Canvas, font_index: usize) TextBatch {
    return .{
        .canvas = self,
        .quads = .init(self.frame_arena.allocator()),
        .text_renderer = &self.text_renderers[font_index],
    };
}

pub fn drawTextLine(
    self: *Canvas,
    font_index: usize,
    camera: Rect,
    pos: Rect.Measure,
    text: []const u8,
    em: f32,
    color: FColor,
) !void {
    try self.text_renderers[font_index].drawLine(
        self.gl,
        camera,
        pos,
        text,
        em,
        color,
        self.frame_arena.allocator(),
    );
}

pub fn drawTextLines(
    self: *Canvas,
    font_index: usize,
    camera: Rect,
    text_align: TextRenderer.Align,
    pos: Rect.Measure,
    lines: []const []const u8,
    em: f32,
    line_spacing: f32,
    color: FColor,
) !void {
    try self.text_renderers[font_index].drawLines(
        self.gl,
        camera,
        text_align,
        pos,
        lines,
        em,
        line_spacing,
        color,
        self.frame_arena.allocator(),
    );
}

// TODO: small text looks worse on the web version!
pub const TextRenderer = struct {
    atlas_texture: Gl.Texture,
    renderable: Gl.Renderable,
    font_info: std.json.Parsed(FontJsonInfo),

    pub const TextPosition = struct {
        hor: enum { left, center, right },
        ver: enum { baseline, ascender, descender, median },
        pos: Vec2,

        pub fn centeredAt(p: Vec2) TextPosition {
            return .{
                .hor = .center,
                .ver = .median,
                .pos = p,
            };
        }
    };

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
            unicode: u21,
            advance: f32,
            planeBounds: ?RectSides = null,
            atlasBounds: ?RectSides = null,
        },
        kerning: []struct {
            unicode1: u21,
            unicode2: u21,
            advance: f32,
        },
    };

    pub fn init(font_json: []const u8, gpa: std.mem.Allocator, gl: Gl, atlas_image: *const anyopaque) !TextRenderer {
        return .{
            .atlas_texture = gl.buildTexture2D(atlas_image, false),
            // TODO: parse the font data at comptime
            .font_info = try std.json.parseFromSlice(
                FontJsonInfo,
                gpa,
                font_json,
                .{},
            ),
            .renderable = try gl.buildRenderable(
                sprite_renderable_vertex_src,
                \\precision highp float;
                \\out vec4 out_color;
                \\in vec2 v_texcoord;
                \\in vec4 v_color;
                \\uniform sampler2D u_texture;
                \\
                \\// for some reason, on desktop, the fwidth value is half of what it should.
                \\#ifdef GL_ES // WebGL2
                \\  #define FWIDTH(x) fwidth(x)
                \\#else // Desktop
                \\  #define FWIDTH(x) (2.0 * fwidth(x))
                \\#endif
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
                \\  out_color = mix(vec4(v_color.rgb, 0), v_color, alpha);
                \\}
            ,
                .{ .attribs = &.{
                    .{ .name = "a_position", .kind = .Vec2 },
                    .{ .name = "a_texcoord", .kind = .Vec2 },
                    .{ .name = "a_color", .kind = .FColor },
                } },
                &.{
                    .{ .name = "u_camera", .kind = .Rect },
                },
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

    fn kerningOf(self: TextRenderer, a: u21, b: u21) ?f32 {
        for (self.font_info.value.kerning) |entry| {
            if (entry.unicode1 == a and entry.unicode2 == b) return entry.advance;
        } else return null;
    }

    /// how to move quads that were placed with the cursor at .zero
    pub fn deltaToAchieve(
        self: TextRenderer,
        pos: TextRenderer.TextPosition,
        total_advance: f32,
        em: f32,
    ) Vec2 {
        return pos.pos.sub(.new(switch (pos.hor) {
            .left => 0,
            .right => total_advance,
            .center => total_advance / 2.0,
        }, switch (pos.ver) {
            .baseline => 0,
            .ascender => self.font_info.value.metrics.ascender * em,
            .descender => self.font_info.value.metrics.descender * em,
            .median => -0.36 * em,
        }));
    }

    pub fn quadsForLineV2(
        self: TextRenderer,
        text: []const u8,
        em: f32,
        color: FColor,
        target: std.mem.Allocator,
    ) !struct { quads: []Quad, total_advance: f32 } {
        var quads: std.ArrayList(Quad) = try .initCapacity(target, text.len);
        var cursor: Vec2 = .zero;

        var utf8 = (try std.unicode.Utf8View.init(text)).iterator();
        var prev: ?u21 = null;
        while (utf8.nextCodepoint()) |codepoint| {
            if (prev) |p| {
                cursor.x += self.kerningOf(p, codepoint) orelse 0;
            }
            cursor, const quad = self.addLetter(cursor, codepoint, em, color);
            if (quad) |q| quads.appendAssumeCapacity(q);
            prev = codepoint;
        }

        // for (text, 0..) |char, k| {
        //     if (k != 0) {
        //         cursor.x += self.kerningOf(text[k - 1], char) orelse 0;
        //     }
        //     cursor, const quad = self.addLetter(cursor, char, em, color);
        //     if (quad) |q| quads.appendAssumeCapacity(q);
        // }

        assert(cursor.y == 0);
        return .{ .quads = try quads.toOwnedSlice(), .total_advance = cursor.x };
    }

    pub fn quadsForLine(
        self: TextRenderer,
        text: []const u8,
        em: f32,
        color: FColor,
        target: std.mem.Allocator,
    ) ![]Quad {
        return (try self.quadsForLineV2(text, em, color, target)).quads;
    }

    // TODO: validate kerning
    pub fn drawLine(
        self: TextRenderer,
        gl: Gl,
        camera: Rect,
        pos: Rect.Measure,
        text: []const u8,
        em: f32,
        color: FColor,
        scratch: std.mem.Allocator,
    ) !void {
        const quads = try self.quadsForLine(text, em, color, scratch);
        defer scratch.free(quads);
        if (quads.len == 0) return;
        const bounds = Rect.boundingOOP(Quad, quads, "pos");
        const delta = bounds.deltaToAchieve(pos);
        self.drawQuads(gl, camera.move(delta.neg()), quads, scratch);
    }

    pub const Align = enum { left, right, center };

    // TODO: kerning
    // TODO: single draw call, maybe
    pub fn drawLines(
        self: TextRenderer,
        gl: Gl,
        camera: Rect,
        text_align: Align,
        pos: Rect.Measure,
        lines: []const []const u8,
        em: f32,
        line_spacing: f32,
        color: FColor,
        scratch: std.mem.Allocator,
    ) !void {
        var all_quads: std.ArrayList(Quad) = .init(scratch);
        for (lines, 0..) |text_line, k| {
            const quads = try self.quadsForLine(text_line, em, color, scratch);
            if (quads.len == 0) continue;
            const width = Rect.boundingOOP(Quad, quads, "pos").size.x;
            const x_off = switch (text_align) {
                .left => 0,
                .center => -width / 2.0,
                .right => -width,
            };
            for (quads) |*q| q.pos.top_left.addInPlace(.new(x_off, math.tof32(k) * em * line_spacing));
            try all_quads.appendSlice(quads);
        }

        const delta = Rect.boundingOOP(Quad, all_quads.items, "pos").deltaToAchieve(pos);
        self.drawQuads(gl, camera.move(delta.neg()), all_quads.items, scratch);
    }

    pub const Quad = struct {
        pos: Rect,
        tex: Rect,
        color: FColor,

        pub fn translate(self: Quad, delta: Vec2) Quad {
            var res = self;
            res.pos.top_left.addInPlace(delta);
            return res;
        }
    };

    pub fn drawQuads(self: TextRenderer, gl: Gl, camera: Rect, sprites: []const Quad, frame_arena: std.mem.Allocator) void {
        // TODO: use a better api
        const VertexData = SpriteVertex;
        const vertices = frame_arena.alloc(VertexData, 4 * sprites.len) catch @panic("OoM");
        const triangles = frame_arena.alloc([3]Gl.IndexType, 2 * sprites.len) catch @panic("OoM");
        for (sprites, 0..) |sprite, i| {
            for ([4]Vec2{ .zero, .e1, .e2, .one }, 0..4) |vertex, k| {
                vertices[i * 4 + k] = .{
                    .a_position = sprite.pos.applyToLocalPosition(vertex),
                    .a_texcoord = sprite.tex.applyToLocalPosition(vertex),
                    .a_color = sprite.color,
                };
            }
            const k: Gl.IndexType = @intCast(4 * i);
            triangles[i * 2 + 0] = .{ k + 0, k + 1, k + 2 };
            triangles[i * 2 + 1] = .{ k + 3, k + 2, k + 1 };
        }

        gl.useRenderable(
            self.renderable,
            vertices.ptr,
            vertices.len * @sizeOf(VertexData),
            triangles,
            &.{
                .{ .name = "u_camera", .value = .{ .Rect = camera } },
            },
            self.atlas_texture,
        );
    }

    pub fn addLetter(
        self: TextRenderer,
        bottom_left: Vec2,
        letter: u21,
        em: f32,
        color: FColor,
    ) std.meta.Tuple(&.{ Vec2, ?Quad }) {
        const default_glyph_info = blk: {
            for (self.font_info.value.glyphs) |glyph| {
                if (glyph.unicode == '?') break :blk glyph;
            } else unreachable;
        };
        // TODO: use a map
        const glyph_info = blk: {
            for (self.font_info.value.glyphs) |glyph| {
                if (glyph.unicode == letter) break :blk glyph;
            } else break :blk default_glyph_info;
        };
        const quad: ?Quad = if (glyph_info.atlasBounds) |b| blk: {
            const s = UVec2.new(
                self.font_info.value.atlas.width,
                self.font_info.value.atlas.height,
            ).tof32();
            const p = glyph_info.planeBounds orelse unreachable;

            const quad: Quad = .{
                .color = color,
                .pos = .from(.{
                    .{ .top_left = bottom_left.add(Vec2.new(p.left, p.top).scale(em)) },
                    .{ .bottom_right = bottom_left.add(Vec2.new(p.right, p.bottom).scale(em)) },
                }),
                .tex = .from(.{
                    .{ .top_left = Vec2.new(b.left, b.top).div(s) },
                    .{ .bottom_right = Vec2.new(b.right, b.bottom).div(s) },
                }),
            };

            break :blk quad;
        } else null;

        return .{ bottom_left.addX(em * glyph_info.advance), quad };
    }
};

// TODO
// pub const SpritesheetRenderer = struct {
//     texture: Gl.Texture,
//     renderable: Gl.InstancedRenderable,

//     pub fn init(gl: Gl, texture: Gl.Texture) SpritesheetRenderer {
//         return .{
//             .texture = texture,
//             .renderable = try gl.buildInstancedRenderable(
//                 \\precision highp float;
//                 \\uniform vec4 u_camera; // as top_left, size
//                 \\in vec2 a_quad_vertex; // (0,0) .. (1,1)
//                 \\in vec4 a_position; // as top_left, size
//                 \\in vec4 a_texcoord; // as top_left, size
//                 \\in vec4 a_color;
//                 \\out vec4 v_color;
//                 \\out vec2 v_texcoord;
//                 \\void main() {
//                 \\  vec2 world_position = a_position.xy + a_position.zw * a_quad_vertex;
//                 \\  vec2 camera_position = (world_position - u_camera.xy) / u_camera.zw;
//                 \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
//                 \\  v_color = a_color;
//                 \\  v_texcoord = a_texcoord.xy + a_quad_vertex * a_texcoord.zw;
//                 \\}
//             ,
//                 \\precision highp float;
//                 \\out vec4 out_color;
//                 \\in vec4 v_color;
//                 \\in vec2 v_texcoord;
//                 \\void main() {
//                 \\  out_color = v_color * vec4(v_texcoord, 1, 1);
//                 \\}
//             ,
//                 .{ .attribs = &.{
//                     .{ .name = "a_quad_vertex", .kind = .Vec2 },
//                 } },
//                 .{ .attribs = &.{
//                     .{ .name = "a_position", .kind = .Rect },
//                     .{ .name = "a_color", .kind = .FColor },
//                     .{ .name = "a_texcoord", .kind = .Rect },
//                 } },
//                 &.{
//                     .{ .name = "u_camera", .kind = .Rect },
//                 },
//             ),
//         };
//     }

//     // pub fn deinit()

//     // pub fn add(self: *SpritesheetRenderer, pos: Rect, tex: Rect) !void {}

//     // pub fn end(self: *SpritesheetRenderer, camera: Rect) void {}
// };

const std = @import("std");
const assert = std.debug.assert;
const math = @import("math.zig");
const UColor = math.UColor;
const FColor = math.FColor;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const IVec2 = math.IVec2;
const funk = @import("funktional.zig");
const Gl = @import("Gl.zig");
const renderer = @import("renderer.zig");
pub const PrecomputedShape = renderer.PrecomputedShape;
pub const RenderableInfo = renderer.RenderableInfo;
