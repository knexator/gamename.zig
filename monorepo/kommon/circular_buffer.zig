const std = @import("std");

// based on https://www.snellman.net/blog/archive/2016-12-13-ring-buffers/
// fails after maxInt(usize) insertions, but maintains that read < write
pub fn RingBuffer(comptime T: type) type {
    return struct {
        const Self = @This();

        data: []T,
        read: usize = 0,
        write: usize = 0,

        pub fn len(self: Self) usize {
            return self.write - self.read;
        }

        pub fn isEmpty(self: Self) bool {
            return self.len() == 0;
        }

        pub fn isFull(self: Self) bool {
            return self.len() == self.data.len;
        }

        pub fn push(self: *Self, element: T) error{ Full, Overflow }!void {
            const new_write = try std.math.add(usize, self.write, 1);
            if (self.isFull()) return error.Full;
            self.data[self.write % self.data.len] = element;
            self.write = new_write;
        }

        pub fn peekFirst(self: *Self) ?T {
            if (self.isEmpty()) return null;
            return self.data[self.read % self.data.len];
        }

        pub fn shift(self: *Self) ?T {
            if (self.isEmpty()) return null;
            const result = self.data[self.read % self.data.len];
            self.read = std.math.add(usize, self.read, 1) catch unreachable;
            return result;
        }

        pub fn pop(self: *Self) ?T {
            if (self.isEmpty()) return null;
            self.write -= 1;
            return self.data[self.write % self.data.len];
        }
    };
}

test "RingBuffer circularity" {
    var buffer: [5]u16 = undefined;
    var ring_buffer: RingBuffer(u16) = .{ .data = &buffer };

    try ring_buffer.push(0);
    try ring_buffer.push(0);

    for (0..100) |_| {
        try ring_buffer.push(0);
        try std.testing.expect(ring_buffer.shift() != null);
    }

    try std.testing.expect(ring_buffer.shift() != null);
    try std.testing.expect(ring_buffer.pop() != null);
    try std.testing.expect(ring_buffer.shift() == null);
    try std.testing.expect(ring_buffer.pop() == null);
}

pub fn CircularBuffer(comptime T: type, comptime buffer_size: usize) type {
    return struct {
        const Self = @This();

        buffer: [buffer_size]T = undefined,
        primer_elemento: usize = 0,
        primer_hueco: usize = 0,

        pub const init: Self = .{};
        pub const empty: Self = .{};

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
