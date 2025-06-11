pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "tres_undos",
        .author = "knexator",
        .desired_aspect_ratio = 1152.0 / 648.0,
    },

    .sounds = .{
        .crash = "sounds/crash.wav",
        .step = "sounds/step1.wav",
    },

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
    undo: usize,
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
    true_timeline_undos: std.ArrayList(usize),
    player: Undoable(struct {
        pos: IVec2,
        in_hole: bool,
        dir: IVec2,
        par: bool,

        pub fn spriteIndex(self: @This()) usize {
            var index: usize = blk: inline for (IVec2.cardinal_directions, &.{ 6, 2, 4, 0 }) |d, k| {
                if (self.dir.equals(d)) break :blk k;
            } else unreachable;
            // if (self.pushing) index += 8;
            if (self.par) index += 1;
            return index;
        }
    }),
    crates: std.ArrayList(Undoable(struct {
        pos: IVec2,
        in_hole: bool,
    })),

    pub fn init(dst: *LevelState, mem: *Mem, ascii: []const u8, enter_dir: IVec2) !void {
        defer _ = mem.scratch.reset(.retain_capacity);
        _ = mem.level.reset(.retain_capacity);

        dst.true_timeline_undos = .init(mem.level.allocator());

        const chars: kommon.Grid2D(u8) = try .fromAscii(mem.scratch.allocator(), ascii);

        const initial_player_pos: IVec2 = blk: {
            var it = chars.iterator();
            while (it.next()) |pos| {
                if (chars.at2(pos) == 'O') break :blk pos.cast(isize);
            } else unreachable;
        };
        dst.player = try .init(mem, &dst.true_timeline_undos, 0, .{ .pos = initial_player_pos, .in_hole = false, .dir = enter_dir, .par = true });

        dst.crates = .init(mem.level.allocator());
        var it = chars.iterator();
        while (it.next()) |pos| {
            const char = chars.at2(pos);
            if ('1' <= char and char <= '9') {
                try dst.crates.append(try .init(
                    mem,
                    &dst.true_timeline_undos,
                    char - '1',
                    .{ .pos = pos.cast(isize), .in_hole = false },
                ));
            }
        }

        dst.geometry = try chars.map(mem.level.allocator(), GeoKind, struct {
            pub fn anon(c: u8) GeoKind {
                return switch (c) {
                    '#' => .wall,
                    '_' => .hole,
                    else => .air,
                };
            }
        }.anon);
    }

    pub fn isWall(self: LevelState, pos: IVec2) bool {
        return !self.geometry.inBoundsSigned(pos) or self.geometry.atSigned(pos) == .wall;
    }

    pub fn anyCrateBlockingAt(self: LevelState, pos: IVec2) bool {
        for (self.crates.items) |crate| {
            if (crate.cur().pos.equals(pos) and !crate.cur().in_hole) return true;
        } else return false;
    }

    pub fn openHoleAt(self: LevelState, pos: IVec2) bool {
        if (self.geometry.atSigned(pos) == .hole) {
            for (self.crates.items) |crate| {
                if (crate.cur().pos.equals(pos) and crate.cur().in_hole)
                    return false;
            } else return true;
        } else return false;
    }

    pub fn doTurn(self: *LevelState, mem: *Mem, input: Input) !union(enum) { usual, wall_crash } {
        defer _ = mem.scratch.reset(.retain_capacity);
        try self.true_timeline_undos.append(switch (input) {
            .dir => 0,
            .undo => |k| k,
        });
        switch (input) {
            .dir => |dir| {
                if (self.player.cur().in_hole) return .wall_crash;

                const new_player_pos = self.player.cur().pos.cast(isize).add(dir);
                if (self.isWall(new_player_pos)) return .wall_crash;

                var pushing_crates: std.ArrayList(usize) = .init(mem.scratch.allocator());
                for (self.crates.items, 0..) |crate, k| {
                    if (crate.cur().pos.equals(new_player_pos) and !crate.cur().in_hole) try pushing_crates.append(k);
                }
                if (pushing_crates.items.len > 0) {
                    const new_crates_pos = new_player_pos.add(dir);
                    if (self.isWall(new_crates_pos) or self.anyCrateBlockingAt(new_crates_pos)) {
                        return .wall_crash;
                    }
                    const fell_on_hole = self.openHoleAt(new_crates_pos);
                    for (pushing_crates.items) |pushed_crate_index| {
                        var cur = self.crates.items[pushed_crate_index].cur();
                        cur.pos = new_crates_pos;
                        cur.in_hole = fell_on_hole;
                        try self.crates.items[pushed_crate_index].setCurrent(cur);
                    }
                }

                try self.player.setCurrent(.{
                    .pos = new_player_pos,
                    .dir = dir,
                    .par = !self.player.cur().par,
                    .in_hole = self.openHoleAt(new_player_pos),
                });

                return .usual;
            },
            .undo => |k| {
                try self.player.undo(k);
                for (self.crates.items) |*crate| {
                    try crate.undo(k);
                }
                return .usual;
            },
        }
    }

    pub fn draw(self: *LevelState, mem: *Mem, camera: Rect, canvas: *Canvas, textures: @FieldType(GameState, "textures")) void {
        const tiles: Canvas.SpriteSheet = .{
            .count = .new(3, 4),
            .margin_px = 2,
            .resolution = textures.tiles.resolution,
        };
        const players: Canvas.SpriteSheet = .{
            .count = .new(4, 4),
            .margin_px = 0,
            .resolution = textures.player.resolution,
        };
        _ = mem;
        var layers = .{
            .basement = canvas.spriteBatch(textures.tiles),
            .main = canvas.spriteBatch(textures.tiles),
        };
        var it = self.geometry.iterator();
        while (it.next()) |pos| {
            const tile = self.geometry.at2(pos);
            switch (tile) {
                .air => {
                    layers.main.add(.{
                        .point = .{ .pos = pos.tof32() },
                        .texcoord = tiles.at(0),
                    });
                },
                .hole => {
                    layers.main.add(.{
                        .point = .{ .pos = pos.tof32() },
                        .texcoord = tiles.at(1),
                    });
                    layers.basement.add(.{
                        .point = .{ .pos = pos.tof32() },
                        .texcoord = tiles.at(3),
                    });
                },
                .wall => {},
            }
        }
        for (self.crates.items) |crate| {
            const layer = if (crate.cur().in_hole) &layers.basement else &layers.main;
            const color = COLORS.CRATES[0];
            layer.add(.{
                .point = .{ .pos = crate.cur().pos.tof32() },
                .texcoord = tiles.at(6),
                .tint = if (crate.cur().in_hole) color.scaleRGB(0.4) else color,
            });
        }
        layers.basement.draw(camera);
        if (self.player.cur().in_hole) {
            canvas.drawSpriteBatch(camera, &.{.{
                .point = .{ .pos = self.player.cur().pos.tof32() },
                .texcoord = players.at(self.player.cur().spriteIndex()),
                .tint = FColor.white.scaleRGB(0.4),
            }}, textures.player);
        }
        layers.main.draw(camera);
        if (!self.player.cur().in_hole) {
            canvas.drawSpriteBatch(camera, &.{.{
                .point = .{ .pos = self.player.cur().pos.tof32() },
                .texcoord = players.at(self.player.cur().spriteIndex()),
            }}, textures.player);
        }
    }
};

pub fn Undoable(T: type) type {
    return struct {
        const Self = @This();

        true_timeline_undos: *const std.ArrayList(usize),
        true_values: std.ArrayList(T),
        immune_to: usize,

        pub fn init(mem: *Mem, true_timeline_undos: *const std.ArrayList(usize), immune_to: usize, initial_value: T) !Self {
            var true_values: std.ArrayList(T) = try .initCapacity(mem.level.allocator(), 64);
            true_values.appendAssumeCapacity(initial_value);
            return .{
                .true_values = true_values,
                .immune_to = immune_to,
                .true_timeline_undos = true_timeline_undos,
            };
        }

        pub fn setCurrent(self: *Self, value: T) !void {
            const prev = self.cur();
            const real_tick = self.true_timeline_undos.items.len;
            while (self.true_values.items.len < real_tick) {
                try self.true_values.append(prev);
            }
            try self.true_values.append(value);
        }

        pub fn cur(self: Self) T {
            return self.true_values.getLast();
        }

        pub fn at(self: Self, tick: usize) T {
            if (tick < self.true_values.items.len) {
                return self.true_values.items[tick];
            } else {
                return self.true_values.getLast();
            }
        }

        pub fn undo(self: *Self, undo_level: usize) !void {
            assert(undo_level > 0);
            assert(self.true_values.items.len > 0);
            if (self.immune_to >= undo_level) {
                try self.setCurrent(self.cur());
            } else {
                const tick = get_original_tick(
                    self.true_values.items.len,
                    self.immune_to,
                    self.true_timeline_undos.items,
                );
                try self.setCurrent(self.at(tick));
            }
        }

        fn get_original_tick(tick: usize, immune_to: usize, true_timeline_undos: []const usize) usize {
            assert(tick <= true_timeline_undos.len);
            if (true_timeline_undos[tick - 1] <= immune_to) {
                return tick;
            } else {
                const travel_depth = true_timeline_undos[tick - 1];
                var counter: usize = 1;
                var res = tick - 1;
                while (counter > 0 and res > 0) {
                    const cur_depth = true_timeline_undos[res - 1];
                    if (cur_depth == travel_depth) {
                        counter += 1;
                        res -= 1;
                    } else if (cur_depth < travel_depth) {
                        counter -= 1;
                        res -= 1;
                    } else {
                        res = get_original_tick(res, immune_to, true_timeline_undos);
                    }
                }
                return res;
            }
        }
    };
}

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.mem = .init(gpa);
    try dst.cur_level.init(&dst.mem, levels_raw[0], .e1);

    dst.textures = .{
        .tiles = gl.buildTexture2D(loaded_images.get(.tiles), true),
        .player = gl.buildTexture2D(loaded_images.get(.player), true),
    };
    dst.canvas = try .init(
        gl,
        gpa,
        &.{@embedFile("../../fonts/Arial.json")},
        &.{loaded_images.get(.arial_atlas)},
    );
    dst.smooth = .init(dst.mem.forever.allocator());
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

    for ([_]KeyboardButton{ .KeyZ, .KeyX, .KeyC, .KeyV }, 0..) |undo_key, undo_level| {
        if (platform.keyboard.wasPressed(undo_key)) {
            try self.input_queue.append(.{ .undo = undo_level + 1 });
        }
    }

    if (self.input_queue.popFirst()) |input| {
        switch (try self.cur_level.doTurn(&self.mem, input)) {
            .usual => platform.sound_queue.insert(.step),
            .wall_crash => platform.sound_queue.insert(.crash),
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
    self.cur_level.draw(&self.mem, camera, &self.canvas, self.textures);
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
