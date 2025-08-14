/// same lifetime as a frame
frame: std.heap.ArenaAllocator,

/// same lifetime as a function call
scratch: std.heap.ArenaAllocator,

/// same lifetime as a level
level: std.heap.ArenaAllocator,

/// same lifetime as the game
forever: std.heap.ArenaAllocator,

/// use sparingly!
gpa: std.mem.Allocator,

pub fn init(gpa: std.mem.Allocator) @This() {
    return .{
        .frame = .init(gpa),
        .scratch = .init(gpa),
        .level = .init(gpa),
        .forever = .init(gpa),
        .gpa = gpa,
    };
}

// pub fn initPreheated(dst: *@This(), gpa: std.mem.Allocator, size: usize) !void {}

pub fn deinit(self: *@This()) void {
    self.frame.deinit();
    self.scratch.deinit();
    self.level.deinit();
    self.forever.deinit();
}

pub fn get(self: *@This(), comptime lifetime: enum { frame, scratch, level, forever }) std.mem.Allocator {
    return switch (lifetime) {
        .frame => self.frame.allocator(),
        .scratch => self.scratch.allocator(),
        .level => self.level.allocator(),
        .forever => self.forever.allocator(),
    };
}

const std = @import("std");
