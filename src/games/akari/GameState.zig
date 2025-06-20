pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "papuzo",
        .author = "knexator",
        .desired_aspect_ratio = 1.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

canvas: Canvas,
mem: struct {
    frame: std.heap.ArenaAllocator,

    pub fn init(gpa: std.mem.Allocator) @This() {
        return .{ .frame = .init(gpa) };
    }

    pub fn deinit(self: *@This()) void {
        self.frame.deinit();
    }

    pub fn onFrameBegin(self: *@This()) void {
        _ = self.frame.reset(.retain_capacity);
    }
},

focus: union(enum) {
    none,
    placing_light: Key,
    painting: enum { none, cross },
} = .none,
smooth: LazyState,

board: kommon.Grid2D(TileState),
const TileState = union(enum) {
    block: ?u8,
    gap: struct {
        mark: enum { none, lamp, cross } = .none,
        lighted: bool = false,
    },
};

const raw_puzzle =
    \\......0...1
    \\.0.X...X...
    \\...........
    \\...2.2.....
    \\......1....
    \\.1..2...2..
    \\...........
    \\....1.X....
    \\....X....2.
    \\...........
    \\.....0..1..
;

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.* = .{
        .canvas = try .init(gl, gpa, &.{@embedFile("../../fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}),
        .mem = .init(gpa),
        .smooth = .init(gpa),
        .board = try .fromAsciiAndMap(gpa, raw_puzzle, struct {
            pub fn anon(c: u8) TileState {
                return switch (c) {
                    '.' => .{ .gap = .{} },
                    '0'...'4' => |n| .{ .block = n - '0' },
                    'X' => .{ .block = null },
                    else => panic("bad char: {d}", .{c}),
                };
            }
        }.anon),
    };
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    self.board.deinit(gpa);
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    _ = self;
}

pub const LazyState = struct {
    f32s: std.AutoHashMap(Key, f32),
    fcolors: std.AutoHashMap(Key, FColor),
    rects: std.AutoHashMap(Key, Rect),

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

    pub fn float(self: *LazyState, key: Key, goal: f32) !f32 {
        const gop = try self.f32s.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* = std.math.lerp(gop.value_ptr.*, goal, 0.2);
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

const Magic = struct {
    // TODO: 2 cycling arenas
    arena: std.heap.ArenaAllocator,
    things: std.AutoHashMap(Key, *const anyopaque),

    // TODO: tree(s?)

    pub fn init(gpa: std.mem.Allocator) Magic {
        return .{
            .arena = .init(gpa),
            .things = .init(gpa),
        };
    }

    pub fn deinit(self: *Magic) void {
        self.arena.deinit();
        self.things.deinit();
    }

    // pub fn endFrame(self: *Magic) void {
    //     var it = self.magic_floats.valueIterator();
    //     while (it.next()) |v| {
    //         if (!v.seen_this_frame) v.cur = null;
    //         v.seen_this_frame = false;
    //     }
    // }

    pub fn getThing(
        self: Magic,
        key: Key,
        comptime T: type,
    ) ?T {
        if (self.things.get(key)) |ptr| {
            // TODO: validate type in debug mode, somehow
            const ptr_casted: *const T = @alignCast(@ptrCast(ptr));
            return ptr_casted.*;
        } else return null;
    }

    pub fn draw(self: Magic, canvas: Canvas, camera: Rect) void {
        var it = self.things.valueIterator();
        while (it.next()) |box| {
            var tile: *const Tile = @alignCast(@ptrCast(box.*));
            tile.draw(canvas, camera);
        }
    }
};

const Tile = struct {
    rect: Rect,
    hot_t: f32 = 0,
    active_t: f32 = 0,

    pub fn draw(box: @This(), canvas: Canvas, cam: Rect) void {
        canvas.fillRect(
            cam,
            box.rect,
            .gray(box.hot_t),
        );
    }
};

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.mem.onFrameBegin();
    const camera: Rect = .{ .top_left = .zero, .size = .both(4) };
    const mouse = platform.getMouse(camera);
    platform.gl.clear(.gray(0.5));

    var it = self.board.iterator();
    while (it.next()) |pos| {
        const key = Key.fromFormat("tile {d} {d}", .{ pos.x, pos.y });
        const rect = self.board.getTileRect(camera, pos);
        const tile: *TileState = self.board.getPtr(pos);
        switch (tile.*) {
            .block => |b| {
                self.canvas.fillRect(camera, rect, .black);
                if (b) |v|
                    try self.canvas.text_renderers[0].drawLine(
                        platform.gl,
                        camera,
                        .{ .center = rect.get(.center) },
                        switch (v) {
                            0 => "0",
                            1 => "1",
                            2 => "2",
                            3 => "3",
                            4 => "4",
                            else => unreachable,
                        },
                        rect.size.y,
                        .white,
                        self.mem.frame.allocator(),
                    );
            },
            .gap => |*g| {
                const mouse_over = rect.contains(mouse.cur.position);
                self.canvas.fillRect(camera, rect, try self.smooth.fcolor(
                    .fromFormat("bg {d}", .{key}),
                    if (g.lighted) .fromHex("#BBFF87") else .white,
                ));
                const is_active = std.meta.eql(self.focus, .{ .placing_light = key }) or (mouse_over and std.meta.activeTag(self.focus) == .painting);
                const hot_t = try self.smooth.float(
                    .fromFormat("hot {d}", .{key}),
                    if (mouse_over and (self.focus == .none or is_active)) 1.0 else 0.0,
                );
                const active_t = try self.smooth.float(
                    .fromFormat("active {d}", .{key}),
                    if (mouse_over and is_active) 1.0 else 0.0,
                );
                if (g.mark != .none) {
                    try self.canvas.text_renderers[0].drawLine(
                        platform.gl,
                        camera,
                        .{ .center = rect.get(.center) },
                        switch (g.mark) {
                            .none => unreachable,
                            .lamp => "o",
                            .cross => ".",
                        },
                        rect.size.y,
                        switch (g.mark) {
                            .none => unreachable,
                            .lamp => .fromHex("#007F61"),
                            .cross => .fromHex("#00A11B"),
                        },
                        self.mem.frame.allocator(),
                    );
                }
                self.canvas.fillRect(camera, rect, FColor.gray(0.5).withAlpha(hot_t * 0.4 - active_t * 0.2));

                switch (self.focus) {
                    .none => {
                        if (mouse_over and mouse.wasPressed(.left)) {
                            self.focus = .{ .placing_light = key };
                        }

                        if (mouse_over and mouse.wasPressed(.right)) {
                            g.mark = switch (g.mark) {
                                .cross => .none,
                                else => .cross,
                            };
                            self.focus = .{ .painting = switch (g.mark) {
                                .cross => .cross,
                                .none => .none,
                                else => unreachable,
                            } };
                        }
                    },
                    .painting => |painting| {
                        if (mouse_over) {
                            g.mark = switch (painting) {
                                .cross => .cross,
                                .none => .none,
                            };
                        }
                    },
                    .placing_light => |active_key| {
                        if (key == active_key and mouse_over) {
                            if (mouse.wasReleased(.left)) {
                                g.mark = switch (g.mark) {
                                    .lamp => .none,
                                    else => .lamp,
                                };
                                self.focus = .none;
                            }
                        } else {}
                    },
                }
            },
        }
    }

    if (self.focus != .none and (mouse.wasReleased(.left) or mouse.wasReleased(.right))) {
        self.focus = .none;
    }

    try self.updateLighted();

    return false;
}

fn updateLighted(self: *GameState) !void {
    var lamp_positions: std.ArrayList(UVec2) = try .initCapacity(self.mem.frame.allocator(), self.board.size.x * 4);

    // set all tiles to off, and store lamp positions
    {
        var it = self.board.iterator();
        while (it.next()) |pos| {
            const tile = self.board.getPtr(pos);
            switch (tile.*) {
                .block => continue,
                .gap => {
                    tile.gap.lighted = false;
                    if (tile.gap.mark == .lamp) {
                        try lamp_positions.append(pos);
                    }
                },
            }
        }
    }

    for (lamp_positions.items) |lamp_pos| {
        for (IVec2.cardinal_directions) |dir| {
            var it = self.board.rayIterator(lamp_pos, dir);
            while (it.next()) |pos| {
                const tile = self.board.getPtr(pos);
                switch (tile.*) {
                    .block => break,
                    .gap => tile.gap.lighted = true,
                }
            }
        }
    }
}

pub const Key = enum(u64) {
    _,

    pub fn fromString(str: []const u8) Key {
        return @enumFromInt(std.hash.Wyhash.hash(0, str));
    }

    pub fn fromFormat(comptime fmt: []const u8, args: anytype) Key {
        var buf: [0x1000]u8 = undefined;
        return fromString(std.fmt.bufPrint(&buf, fmt, args) catch panic("Key fmt was too long", .{}));
    }
};

const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;

const kommon = @import("kommon");
const Triangulator = kommon.Triangulator;
const math = kommon.math;
const tof32 = math.tof32;
const Color = math.UColor;
const FColor = math.FColor;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const IVec2 = math.IVec2;
const funk = kommon.funktional;
const maybeMirror = math.maybeMirror;
const Noise = kommon.Noise;
pub const Mouse = kommon.input.Mouse;
pub const Keyboard = kommon.input.Keyboard;
pub const KeyboardButton = kommon.input.KeyboardButton;
pub const PrecomputedShape = kommon.renderer.PrecomputedShape;
pub const RenderableInfo = kommon.renderer.RenderableInfo;
pub const Gl = kommon.Gl;
pub const Canvas = kommon.Canvas;
pub const TextRenderer = Canvas.TextRenderer;
