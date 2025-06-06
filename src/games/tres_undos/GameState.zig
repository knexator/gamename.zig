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

smooth: @import("../akari/GameState.zig").LazyState,

geometry: kommon.Grid2D(GeoKind),
textures: struct {
    tiles: Gl.Texture,
},

const levels: []const []const u8 = &.{
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

const GeoKind = enum { wall, air, hole };

pub fn init(
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !GameState {
    return .{
        .textures = .{
            .tiles = gl.buildTexture2D(loaded_images.get(.tiles), true),
        },
        .canvas = try .init(
            gl,
            gpa,
            &.{@embedFile("../../fonts/Arial.json")},
            &.{loaded_images.get(.arial_atlas)},
        ),
        .mem = .init(gpa),
        .smooth = .init(gpa),
        .geometry = try .fromAsciiAndMap(gpa, levels[0], struct {
            pub fn anon(c: u8) GeoKind {
                return switch (c) {
                    '#' => .wall,
                    '_' => .hole,
                    else => .air,
                };
            }
        }.anon),
    };
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    self.geometry.deinit(gpa);
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.mem.onFrameBegin();
    const camera: Rect = Rect.withAspectRatio(
        .{ .top_left = .zero, .size = self.geometry.size.tof32() },
        platform.aspect_ratio,
        .grow,
        .center,
    );
    // const mouse = platform.getMouse(camera);
    platform.gl.clear(.gray(0.5));

    var geo_batch = self.canvas.spriteBatch(camera, self.textures.tiles);
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
                .texcoord = .fromSpriteSheet(s, .new(3, 4), Vec2.both(2.0).div(self.textures.tiles.resolution.tof32())),
            });
        }
    }
    geo_batch.draw();
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
