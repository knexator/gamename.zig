pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};
pub const tracy = @import("tracy");

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "tj6",
        .author = "knexator",
        .desired_aspect_ratio = 1.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
        .atlas_texture = "assets/spritesheet.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

usual: kommon.Usual,
levels: []LevelState,
cur_level: usize = 0,
started_grabbing_at: ?IVec2 = null,
atlas_texture: Gl.Texture,

const levels_ascii: []const []const u8 = &.{
    \\#####
    \\#...#
    \\@.F.#
    \\#F..#
    \\#K.F#
    \\#####
    ,
};

const LevelState = struct {
    states_history: std.ArrayListUnmanaged(Snapshot),
    info: Constant,

    const Constant = struct {
        size: IVec2,
        walls: Grid2D(bool),
        lock: IVec2,
    };

    const Snapshot = struct {
        animals: []const Animal,
        solved: bool,

        fn animalAt(self: Snapshot, pos: IVec2) ?usize {
            for (self.animals, 0..) |animal, k| {
                if (animal.pos.equals(pos)) return k;
            } else return null;
        }

        fn doMove(old_state: Snapshot, info: Constant, allocator: std.mem.Allocator, old_pos: IVec2, new_pos: IVec2) !struct {
            message: ?[]const []const u8,
            new_state: ?Snapshot,

            const empty: @This() = .{ .message = null, .new_state = null };
            fn badMove(msg: []const []const u8) @This() {
                return .{ .new_state = null, .message = msg };
            }
        } {
            if (old_pos.sub(new_pos).taxiMag() != 1) return .empty;
            if (old_state.animalAt(new_pos) != null) return .empty;
            if (info.walls.atSigned(new_pos)) return .empty;
            const grabbed = old_state.animalAt(old_pos) orelse return .empty;
            const animal_kind = old_state.animals[grabbed].kind;
            if (animal_kind == .firefly) {
                return .badMove(&.{ "Ouch! These fireflies are", "too hot to handle" });
            }
            const new_animals = try allocator.dupe(Animal, old_state.animals);
            new_animals[grabbed].pos = new_pos;
            return .{ .new_state = .{ .animals = new_animals, .solved = false }, .message = null };
        }
    };

    pub fn cur(self: LevelState) Snapshot {
        return self.states_history.getLast();
    }

    pub fn fromAscii(allocator: std.mem.Allocator, ascii: []const u8) !LevelState {
        const ascii_grid: Grid2D(u8) = try .fromAscii(allocator, ascii);
        defer ascii_grid.deinit(allocator);

        var states_history: std.ArrayListUnmanaged(Snapshot) = .empty;

        var initial_state: std.ArrayListUnmanaged(Animal) = .empty;
        var it = ascii_grid.iteratorSigned();
        while (it.next()) |pos| {
            const char = std.ascii.toLower(ascii_grid.atSigned(pos));
            const kind: Animal.Kind = switch (char) {
                'k' => .beetlekey,
                'f' => .firefly,
                else => continue,
            };
            try initial_state.append(allocator, .{
                .kind = kind,
                .pos = pos,
            });
        }
        try states_history.append(allocator, .{ .animals = try initial_state.toOwnedSlice(allocator), .solved = false });

        return .{
            .states_history = states_history,
            .info = .{
                .size = ascii_grid.size.cast(isize),
                .walls = try ascii_grid.map(allocator, bool, struct {
                    fn anon(c: u8) bool {
                        return c == '#';
                    }
                }.anon),
                .lock = (try ascii_grid.findSingle('@')).cast(isize),
            },
        };
    }
};

const Animal = struct {
    pos: IVec2,
    kind: Kind,

    const Kind = enum {
        beetlekey,
        firefly,
    };
};

pub fn init(
    dst: *GameState,
    runtime_params: kommon.engine.InitRuntimeParamsFor(GameState),
    comptime _: kommon.engine.InitComptimeParamsFor(GameState),
) !void {
    const gpa = runtime_params.gpa;
    const gl = runtime_params.gl;
    const loaded_images = runtime_params.loaded_images;
    const random_seed = runtime_params.random_seed;
    dst.usual.init(gpa, random_seed, try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}));

    dst.cur_level = 0;
    dst.levels = try gpa.alloc(LevelState, levels_ascii.len);
    for (levels_ascii, dst.levels) |src, *lvl| {
        lvl.* = try .fromAscii(gpa, src);
    }

    dst.atlas_texture = gl.buildTexture2D(loaded_images.get(.atlas_texture), true);
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    _ = gpa;
    self.usual.deinit(undefined);
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    _ = self;
}

const Drawer = struct {
    batch: Canvas.SpriteBatch,

    const sheet: Canvas.SpriteSheet = .{
        .count = .both(8),
        .margin_px = 1,
        .resolution = .both((16 + 2 * 1) * 8),
    };

    pub fn start(canvas: *Canvas, atlas: Gl.Texture) Drawer {
        return .{ .batch = .{
            .canvas = canvas,
            .texture = atlas,
            .sprites = .empty,
        } };
    }

    pub fn end(drawer: *Drawer, camera: Rect) void {
        drawer.batch.draw(camera);
    }

    fn sprite(self: *Drawer, pos: IVec2, index: usize) void {
        self.batch.add(.{
            .point = .{ .pos = pos.tof32() },
            .texcoord = sheet.at(index),
        });
    }

    pub fn wall(self: *Drawer, pos: UVec2) void {
        // self.canvas.fillRect(camera, .{ .top_left = pos.tof32(), .size = .one }, .fromHex("#885522"));
        self.sprite(pos.cast(isize), 7);
    }

    pub fn lock(self: *Drawer, pos: IVec2) void {
        // self.canvas.fillRect(camera, .{ .top_left = pos.tof32(), .size = .one }, .cyan);
        self.sprite(pos, 5);
    }

    pub fn animal(self: *Drawer, animal_: Animal) void {
        self.sprite(animal_.pos, switch (animal_.kind) {
            .beetlekey => 4,
            .firefly => 1,
        });
    }
};

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);
    const mem = &self.usual.mem;
    // const smooth = &self.usual.smooth;

    const level = &self.levels[self.cur_level];

    if (platform.wasKeyPressedOrRetriggered(.KeyZ, 0.1, 0.2) and
        level.states_history.items.len > 1)
    {
        _ = level.states_history.pop();
    }

    const camera: Rect = .withAspectRatio(.plusMargin3(.plusMargin(.{
        .top_left = .zero,
        .size = level.info.size.tof32(),
    }, 0.5), .bottom, 1.5), platform.aspect_ratio, .grow, .center);
    const mouse = platform.getMouse(camera);
    const active_tile = level.info.walls.tileSignedAt(mouse.cur.position, .{ .top_left = .zero, .size = level.info.size.tof32() });

    var message: ?[]const []const u8 = null;

    if (self.started_grabbing_at == null) {
        platform.setCursor(if (active_tile != null and level.cur().animalAt(active_tile.?) != null)
            .could_grab
        else
            .default);
        if (mouse.wasPressed(.left)) {
            self.started_grabbing_at = active_tile;
        }
    } else {
        platform.setCursor(.grabbing);
        if (!mouse.cur.isDown(.left)) {
            self.started_grabbing_at = null;
        } else if (active_tile != null) {
            const result = try level.cur().doMove(level.info, mem.gpa, self.started_grabbing_at.?, active_tile.?);
            message = result.message;
            if (result.new_state) |new_state| {
                try level.states_history.append(mem.gpa, new_state);
                self.started_grabbing_at = active_tile.?;
            }
        }
    }

    platform.gl.clear(.fromHex("#181818"));

    var drawer: Drawer = .start(&self.usual.canvas, self.atlas_texture);

    if (true) { // draw walls
        var it = level.info.walls.iterator();
        while (it.next()) |p| {
            if (level.info.walls.at2(p)) {
                drawer.wall(p);
            }
        }
    }
    if (!level.cur().solved) {
        drawer.lock(level.info.lock);
    }
    for (level.cur().animals) |animal| {
        drawer.animal(animal);
    }

    drawer.end(camera);

    if (message) |msg| {
        try self.usual.canvas.drawTextLines(
            0,
            camera,
            .center,
            .{ .center = level.info.size.tof32().mul(.new(0.5, 1)).addY(1) },
            msg,
            0.5,
            1.1,
            .white,
        );
    }

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
const Hex = kommon.hex.Hex;
const Grid2D = kommon.Grid2D;
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
