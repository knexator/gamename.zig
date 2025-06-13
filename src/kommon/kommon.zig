pub const math = @import("math.zig");
pub const funktional = @import("funktional.zig");
pub const hex = @import("hex.zig");
pub const sdl = @import("sdl.zig");
pub const Timekeeper = @import("Timekeeper.zig");
pub const Grid2D = @import("grid_2D.zig").Grid2D;
pub const itertools = @import("itertools.zig");
pub const input = @import("input.zig");
pub const BFS = @import("bfs.zig").BFS;
pub const meta = @import("meta.zig");
pub const CircularBuffer = @import("circular_buffer.zig").CircularBuffer;
pub const Triangulator = @import("triangulator.zig").Triangulator;
pub const Noise = @import("fastnoise.zig").Noise(f32);
pub const Gl = @import("Gl.zig");
pub const Canvas = @import("Canvas.zig");
pub const renderer = @import("renderer.zig");

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

pub fn safeAt(T: type, arr: []const T, index: usize) ?T {
    if (index >= arr.len) {
        return null;
    }
    return arr[index];
}

pub fn last(T: type, arr: []T) ?T {
    if (arr.len == 0) return null;
    return arr[arr.len - 1];
}

// pub fn last(arr: anytype) ?std.meta.Elem(@TypeOf(arr)) {
//     const T = @TypeOf(arr);
//     switch (@typeInfo(T)) {
//         .array => if (arr.len == 0) return null else return arr[arr.len - 1],
//         .vector => @compileError("TODO"),
//         .pointer => |info| switch (info.size) {
//             .one => switch (@typeInfo(info.child)) {
//                 .array => |array_info| return array_info.child,
//                 .vector => |vector_info| return vector_info.child,
//                 else => {},
//             },
//             .many, .c, .slice => return info.child,
//         },
//         .optional => |info| return Elem(info.child),
//         else => {},
//     }
//     @compileError("Expected pointer, slice, array or vector type, found '" ++ @typeName(T) ++ "'");
// }

const std = @import("std");
