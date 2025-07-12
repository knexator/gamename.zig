clear: *const fn (color: FColor) void,

// buildProgram: *const fn (
//     vertex_src: [:0]const u8,
//     fragment_src: [:0]const u8,
// ) error{ ShaderCreationError, ProgramCreationError }!Program,

// pub const Program = struct {
//     id: enum(c_uint) { null = 0, _ },
// };

loadTextureDataFromBase64: *const fn (
    base64: []const u8,
) *const anyopaque,

loadTextureDataFromFilename: *const fn (
    path: [:0]const u8,
) *const anyopaque,

prerenderText: *const fn (
    text: []const u8,
    size_px: f32,
) *const anyopaque,

// isTextureDataLoaded: *const fn (
//     data: *const anyopaque,
// ) bool,

// TODO: wrap, optional mips, filter
buildTexture2D: *const fn (
    data: *const anyopaque,
    pixelart: bool,
) Texture,

buildRenderable: *const fn (
    /// without preamble!
    vertex_src: [:0]const u8,
    /// without preamble!
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
    vertices_len_bytes: usize,
    // TODO: make triangles optional, since they could be precomputed
    triangles: []const [3]IndexType,
    uniforms: []const UniformInfo.Runtime,
    // TODO: multiple textures
    texture: ?Texture,
) void,

buildInstancedRenderable: *const fn (
    /// without preamble!
    vertex_src: [:0]const u8,
    /// without preamble!
    fragment_src: [:0]const u8,
    per_vertex_attributes: VertexInfo.Collection,
    per_instance_attributes: VertexInfo.Collection,
    uniforms: []const UniformInfo.In,
) error{
    ShaderCreationError,
    ProgramCreationError,
    AttributeLocationError,
    UniformLocationError,
    TooManyUniforms,
}!InstancedRenderable,

useInstancedRenderable: *const fn (
    renderable: InstancedRenderable,
    // TODO: make the vertex data optional, since it could be precomputed
    vertex_data_ptr: *const anyopaque,
    vertex_data_len_bytes: usize,
    // TODO: make triangles optional, since they could be precomputed
    triangles: []const [3]IndexType,
    instance_data_ptr: *const anyopaque,
    instance_data_len_bytes: usize,
    instance_count: usize,
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
        Point,
        f32,

        pub fn fromType(t: type) Kind {
            return switch (t) {
                Vec2 => .Vec2,
                FColor => .FColor,
                Point => .Point,
                f32 => .f32,
                else => @compileError("TODO"),
            };
        }

        pub fn byteCount(kind: Kind) usize {
            return switch (kind) {
                .Vec2 => @sizeOf(Vec2),
                .FColor => @sizeOf(FColor),
                .Point => @sizeOf(Point),
                .f32 => @sizeOf(f32),
            };
        }

        pub fn @"type"(kind: Kind) VertexDataType {
            return switch (kind) {
                .Vec2 => .FLOAT,
                .FColor => .FLOAT,
                .Point => .FLOAT,
                .f32 => .FLOAT,
            };
        }

        pub fn count(kind: Kind) enum(c_int) {
            @"1" = 1,
            @"2" = 2,
            @"3" = 3,
            @"4" = 4,
        } {
            return switch (kind) {
                .f32 => .@"1",
                .Vec2 => .@"2",
                .FColor => .@"4",
                .Point => .@"4",
            };
        }

        pub fn normalized(kind: Kind) bool {
            return switch (kind) {
                .f32 => false,
                .Vec2 => false,
                .FColor => false,
                .Point => false,
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
            f32: f32,
            FColor: FColor,
            Rect: Rect,
            Point: Point,

            pub fn fromValue(v: anytype) @This() {
                return switch (@TypeOf(v)) {
                    f32 => .{ .f32 = v },
                    FColor => .{ .FColor = v },
                    Rect => .{ .Rect = v },
                    Point => .{ .Point = v },
                    else => @compileError(std.fmt.comptimePrint("Unhandled type: {s}", .{@typeName(@TypeOf(v))})),
                };
            }
        },
    };

    pub const Kind = enum {
        f32,
        FColor,
        Rect,
        Point,

        pub fn fromType(t: type) Kind {
            return switch (t) {
                Rect => .Rect,
                FColor => .FColor,
                Point => .Point,
                f32 => .f32,
                else => @compileError(std.fmt.comptimePrint("Unhandled type: {s}", .{@typeName(t)})),
            };
        }
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
};

pub const InstancedRenderable = struct {
    program: enum(c_uint) { null = 0, _ },
    vao: enum(c_uint) { null = 0, _ }, // vertex array object: the draw call state, roughly
    vbo_vertices: enum(c_uint) { null = 0, _ }, // vertex buffer object: the vertex data itself
    vbo_instances: enum(c_uint) { null = 0, _ }, // vertex buffer object: the vertex data itself
    ebo: enum(c_uint) { null = 0, _ }, // element buffer object: the triangle indices
    uniforms: std.BoundedArray(UniformInfo, 8),
};

pub const Texture = struct {
    id: c_uint,
    resolution: UVec2,
};

const std = @import("std");
const math = @import("math.zig");
const Color = math.Color;
const FColor = math.FColor;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
