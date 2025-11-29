const std = @import("std");
const digits2 = std.fmt.digits2;
const builtin = @import("builtin");

const options = @import("tracy-options");

pub inline fn setThreadName(comptime _: [:0]const u8) void {}

pub inline fn startupProfiler() void {}

pub inline fn shutdownProfiler() void {}

pub inline fn profilerStarted() bool {}

pub inline fn isConnected() bool {}

pub inline fn frameMark() void {}

pub inline fn frameMarkNamed(comptime _: [:0]const u8) void {}

const DiscontinuousFrame = struct {
    name: [:0]const u8,

    pub inline fn deinit(_: *const DiscontinuousFrame) void {}
};

pub inline fn initDiscontinuousFrame(comptime name: [:0]const u8) DiscontinuousFrame {
    return .{ .name = name };
}

pub inline fn frameImage(_: *anyopaque, _: u16, _: u16, _: u8, _: bool) void {}

pub const ZoneOptions = struct {
    active: bool = true,
    name: ?[]const u8 = null,
    color: ?u32 = null,
};

const ZoneContext = struct {
    pub inline fn deinit(_: *const ZoneContext) void {}
    pub inline fn name(_: *const ZoneContext, _: []const u8) void {}
    pub inline fn text(_: *const ZoneContext, _: []const u8) void {}
    pub inline fn color(_: *const ZoneContext, _: u32) void {}
    pub inline fn value(_: *const ZoneContext, _: u64) void {}
};

pub inline fn initZone(comptime _: std.builtin.SourceLocation, comptime _: ZoneOptions) ZoneContext {
    return .{};
}

pub inline fn plot(comptime T: type, comptime _: [:0]const u8, _: T) void {
    return;
}

pub const PlotType = enum(c_int) {
    Number,
    Memory,
    Percentage,
    Watt,
};

pub const PlotConfig = struct {
    plot_type: PlotType,
    step: c_int,
    fill: c_int,
    color: u32,
};

pub inline fn plotConfig(comptime _: [:0]const u8, comptime _: PlotConfig) void {
    return;
}

pub inline fn message(comptime _: [:0]const u8) void {
    return;
}

pub inline fn messageColor(comptime _: [:0]const u8, _: u32) void {
    return;
}

const tracy_message_buffer_size = 0;
threadlocal var tracy_message_buffer: [tracy_message_buffer_size]u8 = undefined;

pub inline fn print(comptime _: []const u8, _: anytype) void {
    return;
}

pub inline fn printColor(comptime _: []const u8, _: anytype, _: u32) void {
    return;
}

pub inline fn printAppInfo(comptime _: []const u8, _: anytype) void {
    return;
}

pub const TracingAllocator = struct {
    parent_allocator: std.mem.Allocator,
    pool_name: ?[:0]const u8,

    const Self = @This();

    pub fn init(parent_allocator: std.mem.Allocator) Self {
        return .{
            .parent_allocator = parent_allocator,
            .pool_name = null,
        };
    }

    pub fn initNamed(comptime pool_name: [:0]const u8, parent_allocator: std.mem.Allocator) Self {
        return .{
            .parent_allocator = parent_allocator,
            .pool_name = pool_name,
        };
    }

    pub fn allocator(self: *Self) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn alloc(
        ctx: *anyopaque,
        len: usize,
        ptr_align: std.mem.Alignment,
        ret_addr: usize,
    ) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        const result = self.parent_allocator.rawAlloc(len, ptr_align, ret_addr);
        return result;
    }

    fn resize(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: std.mem.Alignment,
        new_len: usize,
        ret_addr: usize,
    ) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));
        const result = self.parent_allocator.rawResize(buf, buf_align, new_len, ret_addr);
        if (!result) return false;

        return true;
    }

    fn remap(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: std.mem.Alignment,
        new_len: usize,
        ret_addr: usize,
    ) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        const result = self.parent_allocator.rawRemap(buf, buf_align, new_len, ret_addr);
        const new_buf = result orelse return null;

        return new_buf;
    }

    fn free(
        ctx: *anyopaque,
        buf: []u8,
        buf_align: std.mem.Alignment,
        ret_addr: usize,
    ) void {
        const self: *Self = @ptrCast(@alignCast(ctx));

        self.parent_allocator.rawFree(buf, buf_align, ret_addr);
    }
};
