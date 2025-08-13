pub const math = @import("math.zig");
pub const funktional = @import("funktional.zig");
pub const hex = @import("hex.zig");
pub const sdl = @import("sdl.zig");
pub const Timekeeper = @import("Timekeeper.zig");
pub const grid2D = @import("grid_2D.zig");
pub const Grid2D = grid2D.Grid2D_Deprecated;
pub const Grid2DEdges = grid2D.Grid2DEdges;
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
pub const engine = @import("engine.zig");
pub const Mem = @import("Mem.zig");
pub const LazyState = @import("lazystate.zig").LazyState;
pub const TopK = @import("topk.zig").TopK;
pub const Key = enum(u64) {
    _,

    pub fn fromString(str: []const u8) Key {
        return @enumFromInt(std.hash.Wyhash.hash(0, str));
    }

    pub fn fromFormat(comptime fmt: []const u8, args: anytype) Key {
        var buf: [0x1000]u8 = undefined;
        return fromString(std.fmt.bufPrint(&buf, fmt, args) catch std.debug.panic("Key fmt was too long", .{}));
    }
};

comptime {
    std.testing.refAllDeclsRecursive(@This());
}

pub fn moveIndex(original: usize, delta: isize, arr_size: usize, mode: enum { clamp, mod }) usize {
    std.debug.assert(arr_size > 0);
    const new_index: isize = @as(isize, @intCast(original)) + delta;
    return switch (mode) {
        .clamp => @intCast(std.math.clamp(new_index, 0, @as(isize, @intCast(arr_size - 1)))),
        .mod => @intCast(@mod(new_index, @as(isize, @intCast(arr_size)))),
    };
}

pub fn safeAt(T: type, arr: []const T, index: usize) ?T {
    if (index >= arr.len) {
        return null;
    }
    return arr[index];
}

pub fn last(arr: anytype) ?std.meta.Elem(@TypeOf(arr)) {
    switch (@typeInfo(@TypeOf(arr))) {
        .array => @compileError("TODO"),
        .vector => @compileError("TODO"),
        .pointer => |info| switch (info.size) {
            .one => @compileError("no"),
            .many, .c => @compileError("TODO"),
            .slice => return lastExplicit(info.child, arr),
        },
        else => {},
    }
    @compileError("Expected pointer, slice, array or vector type, found '" ++ @typeName(@TypeOf(arr)) ++ "'");
}

pub fn lastExplicit(T: type, arr: []const T) ?T {
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
