pub const LazyState = struct {
    f32s: std.AutoHashMap(Key, f32),
    fcolors: std.AutoHashMap(Key, FColor),
    rects: std.AutoHashMap(Key, Rect),
    last_delta_seconds: f32 = undefined,

    pub fn init(gpa: std.mem.Allocator) LazyState {
        return .{
            .f32s = .init(gpa),
            .fcolors = .init(gpa),
            .rects = .init(gpa),
        };
    }

    pub fn deinit(self: *LazyState) void {
        self.arena.deinit();
        self.f32s.deinit();
        self.fcolors.deinit();
        self.rects.deinit();
    }

    pub fn set(self: *LazyState, T: type, key: Key, v: T) !void {
        switch (T) {
            f32 => try self.f32s.put(key, v),
            else => @compileError("TODO"),
        }
    }

    pub fn float(self: *LazyState, key: Key, goal: f32) !f32 {
        const gop = try self.f32s.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* = std.math.lerp(gop.value_ptr.*, goal, 0.2);
        } else {
            gop.value_ptr.* = goal;
        }
        return gop.value_ptr.*;
    }

    pub fn floatCustomSpeed(self: *LazyState, key: Key, goal: f32, speed: f32) !f32 {
        const gop = try self.f32s.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* = std.math.lerp(gop.value_ptr.*, goal, speed);
        } else {
            gop.value_ptr.* = goal;
        }
        return gop.value_ptr.*;
    }

    pub fn floatLinear(self: *LazyState, key: Key, goal: f32, duration: f32) !f32 {
        const gop = try self.f32s.getOrPut(key);
        if (gop.found_existing) {
            math.towards(gop.value_ptr, goal, self.last_delta_seconds / duration);
        } else {
            gop.value_ptr.* = goal;
        }
        return gop.value_ptr.*;
    }

    pub fn fcolor(self: *LazyState, key: Key, goal: FColor) !FColor {
        const gop = try self.fcolors.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* = .lerp(gop.value_ptr.*, goal, 0.2);
        } else {
            gop.value_ptr.* = goal;
        }
        return gop.value_ptr.*;
    }

    pub fn rect(self: *LazyState, key: Key, goal: Rect) !Rect {
        const gop = try self.rects.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* = .lerp(gop.value_ptr.*, goal, 0.2);
        } else {
            gop.value_ptr.* = goal;
        }
        return gop.value_ptr.*;
    }
};

const std = @import("std");
const kommon = @import("kommon.zig");
const Key = kommon.Key;
const math = kommon.math;
const FColor = math.FColor;
const Rect = math.Rect;
