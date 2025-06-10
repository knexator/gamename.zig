const std = @import("std");

pub fn CircularBuffer(comptime T: type, comptime buffer_size: usize) type {
    return struct {
        const Self = @This();

        buffer: [buffer_size]T = undefined,
        primer_elemento: usize = 0,
        primer_hueco: usize = 0,

        pub const init: Self = .{};

        pub fn append(self: *Self, element: T) error{circular_buffer_oom}!void {
            if ((self.primer_hueco + 1) % buffer_size == self.primer_elemento) {
                return error.circular_buffer_oom;
            }
            self.buffer[self.primer_hueco] = element;
            self.primer_hueco = (self.primer_hueco + 1) % buffer_size;
        }

        pub fn popFirst(self: *Self) ?T {
            if (self.primer_elemento == self.primer_hueco) return null;
            defer self.primer_elemento = (self.primer_elemento + 1) % buffer_size;
            return self.buffer[self.primer_elemento];
        }

        pub fn peekFirst(self: *Self) ?T {
            if (self.primer_elemento == self.primer_hueco) return null;
            return self.buffer[self.primer_elemento];
        }

        pub fn clear(self: *Self) void {
            self.primer_hueco = self.primer_elemento;
        }

        // TODO: count()
    };
}

test "basic operations" {
    var my_buffer = CircularBuffer(u8, 10){};
    try my_buffer.append(1);
    try my_buffer.append(2);
    try std.testing.expectEqual(1, my_buffer.peekFirst().?);
    try std.testing.expectEqual(1, my_buffer.popFirst().?);
    try std.testing.expectEqual(2, my_buffer.popFirst().?);
    try std.testing.expectEqual(null, my_buffer.popFirst());
}

test "oom error" {
    var my_buffer = CircularBuffer(u8, 4){};
    for (0..my_buffer.buffer.len - 1) |_| {
        try my_buffer.append(1);
    }
    try std.testing.expectError(error.circular_buffer_oom, my_buffer.append(1));
}

test "circularity" {
    var my_buffer = CircularBuffer(u8, 5){};
    try my_buffer.append(0);
    try my_buffer.append(0);

    for (0..100) |_| {
        try my_buffer.append(0);
        _ = my_buffer.popFirst().?;
    }

    _ = my_buffer.popFirst().?;
    _ = my_buffer.popFirst().?;
    try std.testing.expectEqual(null, my_buffer.popFirst());
}
