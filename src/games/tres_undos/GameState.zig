pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "tres_undos",
        .author = "knexator",
        .desired_aspect_ratio = 1152.0 / 648.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
        .player = "images/player.png",
        .tiles = "images/tiles.png",
        .walls = "images/walls.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const Mem = @FieldType(GameState, "mem");

const COLORS = struct {
    CRATES: [4]FColor = .{
        .fromHex("#CFCFCF"),
        .fromHex("#FF9500"),
        .fromHex("#E74059"),
        .fromHex("#9D15EC"),
    },
}{};

canvas: Canvas,
mem: struct {
    /// same lifetime as a frame
    frame: std.heap.ArenaAllocator,

    /// same lifetime as a function call
    scratch: std.heap.ArenaAllocator,

    /// same lifetime as a level
    level: std.heap.ArenaAllocator,

    /// same lifetime as the game
    forever: std.heap.ArenaAllocator,

    pub fn init(gpa: std.mem.Allocator) @This() {
        return .{
            .frame = .init(gpa),
            .scratch = .init(gpa),
            .level = .init(gpa),
            .forever = .init(gpa),
        };
    }

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
},

smooth: @import("../akari/GameState.zig").LazyState,

textures: struct {
    tiles: Gl.Texture,
    player: Gl.Texture,
},

input_queue: kommon.CircularBuffer(Input, 32) = .init,
cur_level: LevelState,

const Input = union(enum) {
    dir: IVec2,
    undo,
};

const levels_raw: []const []const u8 = &.{
    \\########.#####
    \\########.#####
    \\########.#..##
    \\########_..1.#
    \\########..1..#
    \\########.#._.#
    \\########.#####
    \\..O...1._#####
    \\##############
    ,
};

const LevelState = struct {
    const GeoKind = enum { wall, air, hole };
    geometry: kommon.Grid2D(GeoKind),
    player: Undoable(struct {
        pos: UVec2,
    }),
    crates: std.ArrayList(Undoable(struct {
        pos: UVec2,
    })),

    pub fn init(mem: *Mem, ascii: []const u8) !LevelState {
        defer _ = mem.scratch.reset(.retain_capacity);
        const chars: kommon.Grid2D(u8) = try .fromAscii(mem.scratch.allocator(), ascii);
        const initial_player_pos: UVec2 = blk: {
            var it = chars.iterator();
            while (it.next()) |pos| {
                if (chars.at2(pos) == 'O') break :blk pos;
            } else unreachable;
        };
        var crates: @FieldType(LevelState, "crates") = .init(mem.level.allocator());
        {
            var it = chars.iterator();
            while (it.next()) |pos| {
                if (chars.at2(pos) == '1') {
                    try crates.append(try .init(mem, .{ .pos = pos }));
                }
            }
        }

        return .{
            .geometry = try chars.map(mem.level.allocator(), GeoKind, struct {
                pub fn anon(c: u8) GeoKind {
                    return switch (c) {
                        '#' => .wall,
                        '_' => .hole,
                        else => .air,
                    };
                }
            }.anon),
            .player = try .init(mem, .{ .pos = initial_player_pos }),
            .crates = crates,
        };
    }

    pub fn draw(self: *LevelState, camera: Rect, canvas: *Canvas, textures: @FieldType(GameState, "textures")) void {
        var geo_batch = canvas.spriteBatch(camera, textures.tiles);
        var it = self.geometry.iterator();
        while (it.next()) |pos| {
            const tile = self.geometry.at2(pos);
            const sprite_index: ?UVec2 = switch (tile) {
                .air => .zero,
                .hole => .new(1, 0),
                .wall => null,
            };
            if (sprite_index) |s| {
                geo_batch.add(.{
                    .point = .{ .pos = pos.tof32() },
                    .texcoord = .fromSpriteSheet(s, .new(3, 4), Vec2.both(2.0).div(textures.tiles.resolution.tof32())),
                });
            }
        }
        for (self.crates.items) |crate| {
            geo_batch.add(.{
                .point = .{ .pos = crate.cur().pos.tof32() },
                .texcoord = .fromSpriteSheet(.new(0, 2), .new(3, 4), Vec2.both(2.0).div(textures.tiles.resolution.tof32())),
                .tint = COLORS.CRATES[0],
            });
        }
        geo_batch.draw();
        canvas.drawSpriteBatch(camera, &.{.{
            .point = .{ .pos = self.player.cur().pos.tof32() },
            .texcoord = .fromSpriteSheet(.new(3, 1), .new(4, 4), .zero),
        }}, textures.player);
    }
};

pub fn Undoable(T: type) type {
    return struct {
        const Self = @This();

        true_values: std.ArrayList(T),

        pub fn init(mem: *Mem, initial_value: T) !Self {
            var true_values: std.ArrayList(T) = try .initCapacity(mem.level.allocator(), 64);
            true_values.appendAssumeCapacity(initial_value);
            return .{ .true_values = true_values };
        }

        pub fn append(self: *Self, value: T) !void {
            try self.true_values.append(value);
        }

        pub fn cur(self: Self) T {
            return self.true_values.getLast();
        }
    };
}

pub fn init(
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !GameState {
    // TODO: revise
    var mem: *Mem = try gpa.create(Mem);
    mem.* = .init(gpa);

    return .{
        .textures = .{
            .tiles = gl.buildTexture2D(loaded_images.get(.tiles), true),
            .player = gl.buildTexture2D(loaded_images.get(.player), true),
        },
        .canvas = try .init(
            gl,
            gpa,
            &.{@embedFile("../../fonts/Arial.json")},
            &.{loaded_images.get(.arial_atlas)},
        ),
        .smooth = .init(mem.forever.allocator()),
        .cur_level = try .init(mem, levels_raw[0]),
        .mem = mem.*,
    };
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    self.mem.deinit();
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    _ = self.mem.frame.reset(.retain_capacity);

    // TODO: getKeyRetriggerTime
    for ([_]std.meta.Tuple(&.{ KeyboardButton, IVec2 }){
        .{ .KeyD, .new(1, 0) },  .{ .ArrowRight, .new(1, 0) },
        .{ .KeyA, .new(-1, 0) }, .{ .ArrowLeft, .new(-1, 0) },
        .{ .KeyW, .new(0, -1) }, .{ .ArrowUp, .new(0, -1) },
        .{ .KeyS, .new(0, 1) },  .{ .ArrowDown, .new(0, 1) },
    }) |binding| {
        const key, const dir = binding;
        if (platform.keyboard.wasPressed(key)) {
            try self.input_queue.append(.{ .dir = dir });
        }
    }

    if (self.input_queue.popFirst()) |input| {
        std.log.debug("foo", .{});
        switch (input) {
            .dir => |dir| {
                var player = self.cur_level.player.cur();
                player.pos = player.pos.addSigned(dir);
                try self.cur_level.player.append(player);
            },
            else => @panic("todo"),
        }
    }

    const camera: Rect = Rect.withAspectRatio(
        .{ .top_left = .zero, .size = self.cur_level.geometry.size.tof32() },
        platform.aspect_ratio,
        .grow,
        .center,
    );
    // const mouse = platform.getMouse(camera);
    platform.gl.clear(.gray(0.5));
    self.cur_level.draw(camera, &self.canvas, self.textures);
    return false;
}

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
