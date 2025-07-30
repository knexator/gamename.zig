pub fn Iterator(comptime T: type) type {
    return struct {
        min_inclusive: T,
        max_inclusive: T,
        cur_value: T,
        done: bool,

        const Self = @This();

        pub fn init(min_inclusive: T, max_inclusive: T) Iterator(T) {
            std.debug.assert(min_inclusive <= max_inclusive);
            return .{
                .min_inclusive = min_inclusive,
                .max_inclusive = max_inclusive,
                .cur_value = min_inclusive,
                .done = false,
            };
        }

        pub fn next(self: *Self) ?T {
            if (self.done) return null;
            const v = self.cur_value;
            if (v < self.max_inclusive) {
                self.cur_value += 1;
            } else {
                self.done = true;
            }
            return v;
        }

        pub fn reset(self: *Self) void {
            self.done = false;
            self.cur_value = self.min_inclusive;
        }

        pub fn cur(self: Self) ?T {
            if (self.done) return null;
            return self.cur_value;
        }

        pub fn advance(self: *Self) void {
            _ = self.next();
        }
    };
}

pub fn iterator(comptime T: type, min_inclusive: T, max_inclusive: T) Iterator(T) {
    return .init(min_inclusive, max_inclusive);
}

test "iterator" {
    const it = iterator(u8, 0, 255);
    try std.testing.expectEqual(256, iteratorLen(it));

    try std.testing.expectEqual(6, iteratorLen(
        iterator(std.math.IntFittingRange(0, 5), 0, 5),
    ));
}

pub fn iteratorLen(it: anytype) usize {
    var result: usize = 0;
    var it_local = it;
    while (it_local.next()) |_| {
        result += 1;
    }
    return result;
}

pub fn ForEachIterator(comptime T: type) type {
    return struct {
        buffer: []const T,
        index: usize,

        const Self = @This();

        /// Returns the next element, or null if already returned all.
        pub fn next(self: *Self) ?T {
            if (self.index >= self.buffer.len) return null;
            const result = self.buffer[self.index];
            self.index += 1;
            return result;
        }

        /// Resets the iterator to the initial element.
        pub fn reset(self: *Self) void {
            self.index = 0;
        }
    };
}

pub fn ReverseIterator(comptime T: type) type {
    return struct {
        min_inclusive: T,
        max_inclusive: T,
        cur_value: T,
        done: bool,

        const Self = @This();

        pub fn init(min_inclusive: T, max_inclusive: T) ReverseIterator(T) {
            std.debug.assert(min_inclusive <= max_inclusive);
            return .{
                .min_inclusive = min_inclusive,
                .max_inclusive = max_inclusive,
                .cur_value = max_inclusive,
                .done = false,
            };
        }

        pub fn next(self: *Self) ?T {
            if (self.done) return null;
            const v = self.cur_value;
            if (v > self.min_inclusive) {
                self.cur_value -= 1;
            } else {
                self.done = true;
            }
            return v;
        }

        pub fn reset(self: *Self) void {
            self.done = false;
            self.cur_value = self.max_inclusive;
        }

        pub fn cur(self: Self) ?T {
            if (self.done) return null;
            return self.cur_value;
        }

        pub fn advance(self: *Self) void {
            _ = self.next();
        }
    };
}

pub fn reverseRange(comptime T: type, min_inclusive: T, max_inclusive: T) ReverseIterator(T) {
    return .init(min_inclusive, max_inclusive);
}

pub fn argMin(it: anytype) usize {
    var best_value = it.next().?;
    var best_index: usize = 0;

    var cur_index: usize = 0;
    while (it.next()) |v| {
        cur_index += 1;
        if (v < best_value) {
            best_value = v;
            best_index = cur_index;
        }
    }

    return best_index;
}

const std = @import("std");
