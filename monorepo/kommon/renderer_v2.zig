const std = @import("std");
const assert = std.debug.assert;

pub const Renderer = @This();

pub const Vertex = extern struct {
    relative_pos: [2]f32,
    uv: [2]f32,
};

pub const IndexType = u16;

pub const Drawable = extern struct {
    camera_center: [2]f32,
    camera_axis_x: [2]f32,
    camera_axis_y: [2]f32,
    _pad0: [2]u32 = undefined,
    color: [4]f32,
    texture_id: u32,
    material_id: u32,
    _pad1: [8]u8 = undefined,
};

pub const ModelInfo = struct {
    num_indices: u32,
    first_index: u32,
    vertex_offset: i32,
};

pub const Texture = struct {
    w: u32,
    h: u32,
    pixels: []const [4]u8,
};

// pub const TexturePtr = struct {
//     id: c_uint,
//     w: usize,
//     h: usize,
// };

pub const VTable = struct {
    addModel: *const fn (vertices: []const Vertex, indices: []const [3]IndexType) ModelInfo,
    addDrawable: *const fn (drawable: Drawable, model_info: ModelInfo) void,
    // setTextureFromPtr: *const fn (id: usize, ptr: *const anyopaque) void,
};

comptime {
    assert(hasNoImplicitPadding(Vertex));
    assert(hasNoImplicitPadding(Drawable));
    validateHlslStructuredBuffer(Drawable);
}

fn hasNoImplicitPadding(comptime T: type) bool {
    var sum_size = @as(usize, 0);

    inline for (@typeInfo(T).@"struct".fields) |field| {
        if (field.is_comptime) continue;
        sum_size += @sizeOf(field.type);
    }

    return @sizeOf(T) == sum_size;
}

test hasNoImplicitPadding {
    try std.testing.expect(!hasNoImplicitPadding(struct {
        foo: [3]u8,
        bar: f32,
    }));
}

// based on https://maraneshi.github.io/HLSL-ConstantBufferLayoutVisualizer
// For some reason, the structured buffers produced by shadercross follow the Constant Buffer packing rules
// so ignore the Addendum 6 of that article.
fn validateHlslStructuredBuffer(comptime T: type) void {
    // 1. All scalar types are self-aligned, i.e. their type alignment requirement is equal to their size.
    // Additionally, bool has a size of 4 bytes
    // 1a. Vectors and matrices are aligned according to their scalar component type.
    // 2. A member can't cross a 16-byte row.
    // 3 and 4. rules about HLSL arrays, matrices, and structs, don't allow them for now

    comptime assert(@typeInfo(T).@"struct".layout != .auto);
    inline for (@typeInfo(T).@"struct".fields) |field| {
        if (std.mem.startsWith(u8, field.name, "_pad")) continue;
        switch (@typeInfo(field.type)) {
            else => @compileError(std.fmt.comptimePrint("type {} not supported", .{field.type})),
            .@"struct" => @compileError("TODO: support structs"),
            .vector => @compileError("TODO: support vectors"),
            .bool => @compileError("bools are forbidden, use a u32 explicitly"),
            .array => |info| switch (@typeInfo(info.child)) {
                else => @compileError(std.fmt.comptimePrint("TODO: support arrays of child type {}", .{field.type})),
                .int, .float => {},
            },
            .int, .float => {},
        }

        const byte_start = @offsetOf(T, field.name);
        const byte_end = byte_start + @sizeOf(field.type);
        const row_start = @divFloor(byte_start, 16);
        if ((byte_end - 16 * row_start) > 16 and @mod(byte_start, 16) != 0) {
            @compileError(std.fmt.comptimePrint("field {s} of type {} crosses a 16-byte boundary while not being 16-byte aligned", .{ field.name, field.type }));
        }
    }

    if (@mod(@sizeOf(T), 16) != 0) {
        @compileError(std.fmt.comptimePrint("struct size should be a multiple of 16; missing {d} bytes", .{16 - @mod(@sizeOf(T), 16)}));
    }
}

test validateHlslStructuredBuffer {
    comptime validateHlslStructuredBuffer(extern struct { a: f32, b: [3]f32 });
}
