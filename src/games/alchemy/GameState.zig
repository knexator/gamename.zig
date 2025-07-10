const palette: []const FColor = &.{
    // .fromHex("#0b132b"),
    // .fromHex("#1c2541"),
    // .fromHex("#3a506b"),
    // .fromHex("#5bc0be"),
    // .fromHex("#6fffe9"),

    .fromHex("#1a659e"),
    .fromHex("#004e89"),
    .fromHex("#ff6b35"),
    .fromHex("#f7c59f"),
    .fromHex("#efefd0"),
};

const COLORS = struct {
    text: FColor = palette[4],
    title: FColor = palette[4],
    level_border: FColor = palette[4],
    machine_border: FColor = palette[4],
    bg_board: FColor = palette[1],
}{};

const html_bg = COLORS.bg_board;

pub const Combo = struct { new_prod: usize, old_prod: usize, old_res: usize };

const places: [3]Rect = .{
    Rect.unit.move(.new(1.5, 2)),
    Rect.unit.move(.new(3.5, 2)),
    Rect.unit.move(.new(5.5, 2)),
};

const InputState = struct {
    hovering: ?union(enum) {
        element: usize,
        machine: usize,
    },
    grabbing: ?usize,
};

const AlchemyData = struct {
    const images_base64: []const []const u8 = @import("images.zon");
};

pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "alchemy",
        .author = "knexator",
        .desired_aspect_ratio = 4.0 / 3.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

canvas: Canvas,
mem: Mem,
smooth: @import("../akari/GameState.zig").LazyState,

textures_data: [AlchemyData.images_base64.len]?*const anyopaque = @splat(null),
textures: [AlchemyData.images_base64.len]Gl.Texture = undefined,

input_state: InputState = .{ .grabbing = null, .hovering = null },

menu_state: struct {
    game_focus: f32 = 0,
    game_focus_target: f32 = 0,
    level: usize = 0,
} = .{},

level_states: [levels.len]LevelState = undefined,

const levels: []const LevelInfo = &.{
    .{
        .goal = .earth,
        .recipes = &.{
            .{ .earth, .rain, .plant },
            .{ .egg, .plant, .eggplant },
            .{ .rain, .ice, .hail },
        },
        .initial = &.{
            .egg,
            .eggplant,
            .ice,
            .hail,
        },
    },
    .{
        .goal = .fire,
        .recipes = &.{
            .{ .cow, .sea, .manatee },
            .{ .human, .cow, .minotaur },
            .{ .horse, .human, .centaur },
            .{ .bird, .horse, .pegasus },
            .{ .fire, .bird, .phoenix },
        },
        .initial = &.{
            .phoenix,
            .pegasus,
            .centaur,
            .minotaur,
            .manatee,
            .sea,
        },
    },
    .{
        .goal = .sea,
        .recipes = &.{
            .{ .sugar, .confetti, .sprinkles },
            .{ .campfire, .sugar, .marshmallows },
            .{ .fabric, .campfire, .@"smoke signal" },
            .{ .blood, .fabric, .bandage },
            .{ .sea, .blood, .shark },
        },
        .initial = &.{
            .sprinkles,
            .confetti,
            .marshmallows,
            .@"smoke signal",
            .bandage,
            .shark,
        },
    },
    .{
        .goal = .air,
        .recipes = &.{
            .{ .air, .port, .airport },
            .{ .pass, .port, .passport },
            .{ .pass, .word, .password },
            .{ .cross, .word, .crossword },
            .{ .cross, .bow, .crossbow },
            .{ .bow, .tie, .bowtie },
        },
        .initial = &.{
            .tie,
            .bowtie,
            .crossbow,
            .crossword,
            .password,
            .passport,
            .airport,
        },
    },
};

const Element = enum(usize) {
    bird = 46,
    cow = 76,
    phoenix = 47,
    human = 48,
    horse = 198,
    pegasus = 226,
    centaur = 283,
    minotaur = 472,
    manatee = 335,
    sea = 9,
    fire = 2,

    sprinkles = 589,
    confetti = 451,
    marshmallows = 409,
    @"smoke signal" = 591,
    bandage = 506,
    shark = 227,
    blood = 112,
    sugar = 263,
    campfire = 63,
    fabric = 433,

    air = 4,
    port = 1000,
    airport,
    pass,
    word,
    crossword,
    passport,
    password,
    cross,
    crossbow,
    bow,
    tie,
    bowtie,

    earth = 3,
    plant = 24,
    egg = 49,
    rain = 13,
    ice = 148,
    hail = 194,

    seed = 2000,
    eggplant,

    pub fn textureIndex(self: Element) ?usize {
        if (@intFromEnum(self) >= 1000) {
            return null;
        } else return @intFromEnum(self);
    }

    pub fn name(self: Element) []const u8 {
        return switch (self) {
            inline else => |x| @tagName(x),
        };
    }
};
const LevelInfo = struct {
    goal: Element,
    recipes: []const [3]Element,
    initial: []const Element,

    pub fn combinationOf(self: LevelInfo, ingredient: Element, result: Element) ?Element {
        for (self.recipes) |recipe| {
            if (recipe[2] != result) continue;
            if (recipe[0] == ingredient) return recipe[1];
            if (recipe[1] == ingredient) return recipe[0];
        } else return null;
    }
};
const LevelState = struct {
    info: LevelInfo,
    solved: bool = false,
    machines: [3]?usize = @splat(null),
    placed: std.ArrayList(struct {
        pos: Vec2,
        id: Element,
        deleted: bool = false,

        pub fn rect(self: @This()) Rect {
            assert(!self.deleted);
            return .{ .top_left = self.pos, .size = .one };
        }

        pub fn label(self: @This()) Canvas.TextRenderer.TextPosition {
            assert(!self.deleted);
            return .{
                .hor = .center,
                .ver = .median,
                .pos = self.pos.add(.half),
            };
        }
    }),

    pub fn fromInfo(dst: *LevelState, info: LevelInfo, gpa: std.mem.Allocator) !void {
        dst.placed = .init(gpa);
        dst.info = info;
        for (info.initial, 0..) |id, k| {
            try dst.placed.append(.{ .id = id, .pos = .new(tof32(k) * 1.3 + 0.2, 4 + tof32(std.math.sign(@mod(k, 2)))) });
        }
    }

    fn machineCombo(self: *LevelState, ingredient: ?usize, result: ?usize) !?usize {
        if (ingredient == null or result == null) return null;
        assert(ingredient.? != result.?);
        if (self.info.combinationOf(
            self.placed.items[ingredient.?].id,
            self.placed.items[result.?].id,
        )) |new_id| {
            try self.placed.append(.{ .id = new_id, .pos = places[0].top_left });
            return self.placed.items.len - 1;
        } else return null;
    }

    pub fn placeInMachine(self: *LevelState, machine_index: usize, element_index: usize) !void {
        assert(machine_index != 0);
        self.machines[machine_index] = element_index;
        self.placed.items[element_index].pos = places[machine_index].top_left;
        self.machines[0] = try self.machineCombo(
            self.machines[1],
            self.machines[2],
        );
    }

    pub fn pickFromMachine(self: *LevelState, machine_index: usize) ?usize {
        const index = self.machines[machine_index];
        if (machine_index == 0) {
            self.deleteElements(&.{ self.machines[1], self.machines[2] });
            self.machines[1] = null;
            self.machines[2] = null;
        } else {
            self.deleteElements(&.{self.machines[0]});
            self.machines[0] = null;
        }
        self.machines[machine_index] = null;
        return index;
    }

    // TODO: revise
    fn deleteElements(self: *LevelState, indices: []const ?usize) void {
        for (indices) |id| {
            if (id != null) {
                self.placed.items[id.?].deleted = true;
            }
        }
    }
};

pub fn preload(
    dst: *GameState,
    gl: Gl,
) !void {
    for (&dst.textures_data, 0..) |*data, k| {
        if (std.mem.eql(u8, "NULL", AlchemyData.images_base64[k])) continue;
        data.* = gl.loadTextureDataFromBase64(AlchemyData.images_base64[k]);
    }
}

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("../../fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.smooth = .init(dst.mem.forever.allocator());

    for (&dst.level_states, levels) |*level, info| {
        try level.fromInfo(info, dst.mem.forever.allocator());
    }

    // for (AlchemyData.names, AlchemyData.required_mixes) |name, k| {
    //     std.log.debug("{d} for {s}, via {s} + {s}", .{ k.mixes, name, AlchemyData.names[k.a], AlchemyData.names[k.b] });
    // }

    for (&dst.textures, dst.textures_data) |*texture, data| {
        if (data == null) continue;
        texture.* = gl.buildTexture2D(data.?, false);
    }
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    // self.mem.deinit();
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    _ = self;
}

pub fn grabbingId(self: GameState) ?Element {
    if (self.input_state.grabbing) |g| {
        return self.level_states[self.menu_state.level].placed.items[g].id;
    } else return null;
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    _ = self.mem.frame.reset(.retain_capacity);
    _ = self.mem.scratch.reset(.retain_capacity);
    self.smooth.last_delta_seconds = platform.delta_seconds;

    const game_camera = (Rect.from(.{
        .{ .top_center = .new(4, 0) },
        .{ .size = .both(7) },
    })).withAspectRatio(
        platform.aspect_ratio,
        .grow,
        .top_center,
    );
    const menu_camera = (Rect.from(.{
        .{ .bottom_center = .new(4, 0) },
        .{ .size = .both(7) },
    })).withAspectRatio(
        platform.aspect_ratio,
        .grow,
        .bottom_center,
    );

    const game_focus = math.easings.easeInOutCubic(self.menu_state.game_focus);
    const camera: Rect = .lerp(menu_camera, game_camera, game_focus);

    const mouse = platform.getMouse(camera);

    const canvas = &self.canvas;
    platform.gl.clear(COLORS.bg_board);

    var icons: std.ArrayList(struct { id: Element, rect: Rect }) = .init(canvas.frame_arena.allocator());
    var level_icons: std.ArrayList(struct { id: Element, rect: Rect, solved: f32 = 0.0 }) = .init(canvas.frame_arena.allocator());
    var fg_text = canvas.textBatch(0);
    var title_text = &fg_text;

    if (self.menu_state.game_focus < 1) {
        try title_text.addText("Reverse", .{
            .hor = .center,
            .ver = .baseline,
            .pos = .new(math.lerp(
                -15,
                4,
                math.easings.easeOutCubic(math.clamp01(platform.global_seconds - 0.4)),
            ), -5),
        }, 2, COLORS.title);
        try title_text.addText("Alchemy", .{
            .hor = .center,
            .ver = .baseline,
            .pos = .new(math.lerp(
                15,
                4,
                math.easings.easeOutCubic(math.clamp01(platform.global_seconds - 0.8)),
            ), -3),
        }, 2, COLORS.title);
    }

    for (levels, 0..) |info, k| {
        const rect: Rect = .{ .top_left = .new(
            tof32(k) * 1.5,
            if (k == self.menu_state.level)
                math.lerp(-1.6, 0.3, game_focus)
            else
                -1.6,
        ), .size = .one };

        const hovering = rect.plusMargin(0.1).contains(mouse.cur.position);
        const hovering_to_enter_level = hovering and self.menu_state.game_focus_target == 0 and !self.level_states[k].solved;
        const hovering_to_solve_level = hovering and self.grabbingId() == info.goal;
        const hovering_to_exit_level = hovering and self.input_state.grabbing == null and self.menu_state.game_focus_target == 1;

        try level_icons.append(.{
            .rect = rect,
            .id = info.goal,
            .solved = if (hovering_to_solve_level or self.level_states[k].solved) 1.0 else 0.0,
        });

        canvas.borderRect(camera, rect.plusMargin(0.1), try self.smooth.float(
            .fromFormat("menu {d}", .{k}),
            if (hovering_to_enter_level or hovering_to_exit_level or self.grabbingId() == info.goal) 0.1 else 0.01,
        ), .inner, COLORS.level_border);

        if (hovering_to_enter_level and mouse.wasPressed(.left)) {
            self.menu_state.level = k;
            self.menu_state.game_focus_target = 1.0;
        }
        if (hovering_to_exit_level and mouse.wasPressed(.left)) {
            self.menu_state.game_focus_target = 0.0;
        }

        if (hovering_to_solve_level and mouse.wasReleased(.left)) {
            self.level_states[self.menu_state.level].placed.items[self.input_state.grabbing.?].deleted = true;
            self.input_state.grabbing = null;
            self.menu_state.game_focus_target = 0;
            self.level_states[k].solved = true;
        }
    }

    math.towards(&self.menu_state.game_focus, self.menu_state.game_focus_target, platform.delta_seconds / 0.4);

    if (platform.keyboard.cur.isDown(.Digit1)) {
        self.menu_state.game_focus_target = 1;
    }
    if (platform.keyboard.cur.isDown(.Escape)) {
        self.menu_state.game_focus_target = 0;
    }

    for (self.level_states[self.menu_state.level].placed.items, 0..) |*element, k| {
        if (element.deleted) continue;
        if (self.level_states[self.menu_state.level].machines[0] != k and self.input_state.grabbing != k) {
            element.pos = element.pos.towardsPure(
                element.pos.add(.half).awayFrom(places[0].get(.center), 1.0).sub(.half),
                10 * platform.delta_seconds,
            );
        }
        if (!game_camera.plusMargin(-0.5).contains(element.pos.add(.half))) {
            element.pos = element.pos.add(.half).towardsPure(game_camera.get(.center), 10 * platform.delta_seconds).sub(.half);
        }
        if (self.input_state.grabbing != k and element.pos.y < 1) {
            element.pos.y = 1;
        }
    }

    self.input_state.hovering = null;
    for (self.level_states[self.menu_state.level].placed.items, 0..) |element, k| {
        if (element.deleted) continue;
        try icons.append(.{
            .rect = element.rect(),
            .id = element.id,
        });
        try fg_text.addText(
            element.id.name(),
            .{
                .hor = .center,
                .ver = .median,
                .pos = element.rect().get(.bottom_center).addY(0.1),
            },
            2.5 / tof32(element.id.name().len),
            COLORS.text,
        );
        if (k == self.input_state.grabbing) continue;
        if (self.input_state.grabbing == null and element.rect().contains(mouse.cur.position)) {
            self.input_state.hovering = .{ .element = k };
        }
    }

    inline for (places, self.level_states[self.menu_state.level].machines, 0..) |place, contents, k| {
        var hovered = false;
        if (place.contains(mouse.cur.position)) {
            if (self.input_state.grabbing == null) {
                if (contents != null) {
                    self.input_state.hovering = .{ .machine = k };
                    hovered = true;
                }
            } else {
                if (k > 0 and contents == null) {
                    self.input_state.hovering = .{ .machine = k };
                    hovered = true;
                }
            }
        }

        if (contents == null) {
            try fg_text.addText("?", .{
                .hor = .center,
                .ver = .median,
                .pos = place.get(.center),
            }, 1, if (k == 0) .black else COLORS.text.withAlpha(if (hovered) 0.2 else 1));
        }
    }
    try fg_text.addText("+", .{ .hor = .center, .ver = .median, .pos = .lerp(
        places[0].get(.center),
        places[1].get(.center),
        0.5,
    ) }, 1, COLORS.text);
    try fg_text.addText("=", .{ .hor = .center, .ver = .median, .pos = .lerp(
        places[1].get(.center),
        places[2].get(.center),
        0.5,
    ) }, 1, COLORS.text);

    if (mouse.wasPressed(.left)) {
        assert(self.input_state.grabbing == null);
        self.input_state.grabbing = if (self.input_state.hovering) |h| switch (h) {
            .element => |k| k,
            .machine => |k| self.level_states[self.menu_state.level].pickFromMachine(k),
        } else null;
        self.input_state.hovering = null;
    }
    if (self.input_state.grabbing) |grabbing_index| {
        self.level_states[self.menu_state.level].placed.items[grabbing_index].pos.lerpTowards(
            mouse.cur.position.sub(.half),
            0.6,
            platform.delta_seconds,
        );
    }
    if (mouse.wasReleased(.left)) {
        if (self.input_state.grabbing) |grabbing| {
            if (self.input_state.hovering) |h| switch (h) {
                .element => unreachable,
                .machine => |k| try self.level_states[self.menu_state.level].placeInMachine(k, grabbing),
            };
            self.input_state.grabbing = null;
        }
    }

    if (self.menu_state.level == 0) {
        try fg_text.addText(
            "Hint: rain + ice = hail",
            .{
                .hor = .center,
                .ver = .median,
                .pos = game_camera.getCenter().addY(-3),
            },
            0.5,
            COLORS.text,
        );
    }

    if (self.level_states[self.menu_state.level].machines[0] != null) {
        canvas.strokeRect(camera, places[0], 0.02, COLORS.machine_border);
    }
    fg_text.draw(camera);
    for (level_icons.items) |icon| {
        canvas.drawSpriteBatch(camera, &.{.{
            .point = .{ .pos = icon.rect.top_left, .scale = icon.rect.size.x },
            .texcoord = .unit,
            .tint = .lerp(.black, .white, icon.solved),
        }}, self.textures[icon.id.textureIndex() orelse 0]);
    }
    for (icons.items) |icon| {
        canvas.drawSpriteBatch(camera, &.{.{
            .point = .{ .pos = icon.rect.top_left, .scale = icon.rect.size.x },
            .texcoord = .unit,
        }}, self.textures[icon.id.textureIndex() orelse 0]);
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
const funk = kommon.funktional;
const maybeMirror = math.maybeMirror;
const Noise = kommon.Noise;
const last = kommon.last;
pub const Mouse = kommon.input.Mouse;
pub const Keyboard = kommon.input.Keyboard;
pub const KeyboardButton = kommon.input.KeyboardButton;
pub const PrecomputedShape = kommon.renderer.PrecomputedShape;
pub const RenderableInfo = kommon.renderer.RenderableInfo;
pub const Gl = kommon.Gl;
pub const Canvas = kommon.Canvas;
pub const TextRenderer = Canvas.TextRenderer;
pub const Mem = @import("../tres_undos/GameState.zig").Mem;
pub const Key = @import("../akari/GameState.zig").Key;
pub const LazyState = @import("../akari/GameState.zig").LazyState;
pub const EdgePos = kommon.grid2D.EdgePos;
