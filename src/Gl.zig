clear: *const fn (color: FColor) void,

// buildProgram: *const fn (
//     vertex_src: [:0]const u8,
//     fragment_src: [:0]const u8,
// ) error{ ShaderCreationError, ProgramCreationError }!Program,

// pub const Program = struct {
//     id: enum(c_uint) { null = 0, _ },
// };

// TODO: wrap, optional mips, filter
buildTexture2D: *const fn (
    data: *const anyopaque,
) Texture,

buildRenderable: *const fn (
    vertex_src: [:0]const u8,
    fragment_src: [:0]const u8,
    attributes: VertexInfo.Collection,
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
    // TODO: multiple textures
    texture: ?Texture,
) void,

pub const VertexInfo = struct {
    // TODO: kind
    pub const In = struct {
        name: [:0]const u8,
        kind: Kind,
        offset: ?usize = null,
    };

    pub const Collection = struct {
        attribs: []const In,
        stride: ?usize = null,

        /// assuming packed
        pub fn getStride(collection: Collection) usize {
            if (collection.stride) |s| return s;
            var result: usize = 0;
            for (collection.attribs) |attr| {
                result += attr.kind.byteCount();
            }
            return result;
        }

        /// assuming packed
        pub fn getOffset(collection: Collection, k: usize) usize {
            if (collection.attribs[k].offset) |o| return o;
            var result: usize = 0;
            for (0..k) |i| {
                result += collection.attribs[i].kind.byteCount();
            }
            return result;
        }

        // TODO
        // pub fn fromType(attribs: type) Collection {}
    };

    pub const Kind = enum {
        Vec2,
        FColor,

        pub fn byteCount(kind: Kind) usize {
            return switch (kind) {
                .Vec2 => @sizeOf(Vec2),
                .FColor => @sizeOf(FColor),
            };
        }

        pub fn @"type"(kind: Kind) VertexDataType {
            return switch (kind) {
                .Vec2 => .FLOAT,
                .FColor => .FLOAT,
            };
        }

        pub fn count(kind: Kind) enum(c_int) {
            @"1" = 1,
            @"2" = 2,
            @"3" = 3,
            @"4" = 4,
        } {
            return switch (kind) {
                .Vec2 => .@"2",
                .FColor => .@"4",
            };
        }

        pub fn normalized(kind: Kind) bool {
            return switch (kind) {
                .Vec2 => false,
                .FColor => false,
            };
        }
    };
};

pub const VertexDataType = enum(c_uint) {
    BYTE = 0x1400,
    UNSIGNED_BYTE = 0x1401,
    SHORT = 0x1402,
    UNSIGNED_SHORT = 0x1403,
    INT = 0x1404,
    UNSIGNED_INT = 0x1405,
    FLOAT = 0x1406,
    HALF_FLOAT = 0x140B,
    INT_2_10_10_10_REV = 0x8D9F,
    UNSIGNED_INT_2_10_10_10_REV = 0x8368,
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

pub const Texture = struct {
    id: c_uint,
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
