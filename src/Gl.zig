clear: *const fn (color: FColor) void,

// buildProgram: *const fn (
//     vertex_src: [:0]const u8,
//     fragment_src: [:0]const u8,
// ) error{ ShaderCreationError, ProgramCreationError }!Program,

// pub const Program = struct {
//     id: enum(c_uint) { null = 0, _ },
// };

buildRenderable: *const fn (
    vertex_src: [:0]const u8,
    fragment_src: [:0]const u8,
    attributes: []const VertexInfo.In,
    uniforms: []const UniformInfo.In,
) error{
    ShaderCreationError,
    ProgramCreationError,
    AttributeLocationError,
    UniformLocationError,
    TooManyUniforms,
}!Renderable,

// TODO: destroyRenderable

useRenderable: *const fn (
    renderable: Renderable,
    // TODO
    // vertices: []anyopaque,
    vertices_ptr: *const anyopaque,
    vertices_len: usize,
    // TODO: make triangles optional, since they could be precomputed
    triangles: []const [3]IndexType,
    uniforms: []const UniformInfo.Runtime,
    // TODO: textures, multiple textures
) void,

pub const VertexInfo = struct {
    // TODO: kind
    pub const In = struct { name: [:0]const u8 };
};

pub const UniformInfo = struct {
    location: c_int,
    // TODO: change to a bounded thing? or maybe dont even store it?
    name: [:0]const u8,
    kind: Kind,

    pub const In = struct { name: [:0]const u8, kind: UniformInfo.Kind };

    // TODO: rename
    pub const Runtime = struct {
        // TODO: location instead of name
        // location: c_int,
        name: [:0]const u8,
        // TODO: comptime magic
        value: union(Kind) {
            FColor: FColor,
            Rect: Rect,
            Point: Point,
        },
    };

    pub const Kind = enum {
        FColor,
        Rect,
        Point,
    };
};

// TODO
pub const IndexType = u16;

pub const Renderable = struct {
    program: enum(c_uint) { null = 0, _ },
    vao: enum(c_uint) { null = 0, _ }, // vertex array object: the draw call state, roughly
    vbo: enum(c_uint) { null = 0, _ }, // vertex buffer object: the vertex data itself
    ebo: enum(c_uint) { null = 0, _ }, // element buffer object: the triangle indices
    uniforms: std.BoundedArray(UniformInfo, 8),

    pub fn sizeOfVertex(self: Renderable) usize {
        // TODO NOW
        _ = self;
        return @sizeOf(Vec2);
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
