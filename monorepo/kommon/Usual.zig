//! Things useful for most games

const Usual = @This();

canvas: Canvas,
mem: Mem,
smooth: LazyState,
random: std.Random.DefaultPrng,
// lazy_state: LocalDecisions,

pub fn init(
    dst: *Usual,
    gpa: std.mem.Allocator,
    random_seed: u64,
    canvas: Canvas,
) void {
    dst.mem = .init(gpa);
    dst.canvas = canvas;
    dst.smooth = .init(dst.mem.forever.allocator());
    dst.random = .init(random_seed);
}

pub fn deinit(self: *Usual, gl: Gl) void {
    self.canvas.deinit(gl, self.mem.gpa);
    self.mem.deinit();
}

pub fn frameStarted(self: *Usual, platform: anytype) void {
    _ = self.mem.frame.reset(.retain_capacity);
    _ = self.mem.scratch.reset(.retain_capacity);
    self.smooth.last_delta_seconds = platform.delta_seconds;
    self.canvas.startFrame(platform.gl);
    // self.lazy_state.frameStart();
}

pub const Canvas = @import("Canvas.zig");
pub const Mem = @import("Mem.zig");
pub const LazyState = @import("lazystate.zig").LazyState;

const Gl = @import("Gl.zig");

const std = @import("std");
