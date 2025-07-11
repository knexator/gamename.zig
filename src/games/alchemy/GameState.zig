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
    const images_base64: []const []const u8 = &.{
        "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",
    };
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

    .sounds = .{
        .enter = "sounds/alchemy/enter.wav",
        .win = "sounds/alchemy/win.wav",
    },

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

canvas: Canvas,
mem: Mem,
smooth: @import("../akari/GameState.zig").LazyState,

textures_data: [1]?*const anyopaque = @splat(null),
textures: [1]Gl.Texture = undefined,

textures_new_data: kommon.meta.StructFromEnum(Element, *const anyopaque, false),
textures_new: kommon.meta.StructFromEnum(Element, Gl.Texture, false),

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
            .sea,
            .centaur,
            .pegasus,
            .manatee,
            .minotaur,
            .phoenix,
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
            .shark,
            .marshmallows,
            .@"smoke signal",
            .confetti,
            .bandage,
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
            .password,
            .crossbow,
            .tie,
            .bowtie,
            .airport,
            .crossword,
            .passport,
        },
    },
    .{
        .goal = .life,
        .recipes = &.{
            .{ .life, .dough, .@"gingerbread pal" },
            .{ .dough, .fire, .bread },
            .{ .bread, .pensioner, .pigeon },
            .{ .fire, .work, .fireworks },
            .{ .work, .idea, .game },
            .{ .@"gingerbread pal", .house, .@"gingerbread house" },
            .{ .house, .green, .greenhouse },
            .{ .green, .blue, .turquoise },
        },
        .initial = &.{
            .game,
            .blue,
            .@"gingerbread house",
            .pensioner,
            .fireworks,
            .idea,
            .greenhouse,
            .pigeon,
            .turquoise,
        },
    },
};

const Element = enum {
    bird,
    cow,
    phoenix,
    human,
    horse,
    pegasus,
    centaur,
    minotaur,
    manatee,
    sea,
    fire,

    sprinkles,
    confetti,
    marshmallows,
    @"smoke signal",
    bandage,
    shark,
    blood,
    sugar,
    campfire,
    fabric,

    air,
    port,
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

    earth,
    plant,
    egg,
    rain,
    ice,
    hail,
    eggplant,

    life,
    dough,
    @"gingerbread pal",
    bread,
    pensioner,
    pigeon,
    work,
    fireworks,
    idea,
    game,
    house,
    @"gingerbread house",
    green,
    turquoise,
    greenhouse,
    blue,

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

    inline for (@typeInfo(Element).@"enum".fields) |f| {
        @field(dst.textures_new_data, f.name) = gl.loadTextureDataFromFilename("images/alchemy/" ++ comptime urlSafe(f.name) ++ ".png");
    }
}

pub fn urlSafe(comptime name: []const u8) []const u8 {
    @setEvalBranchQuota(10_000);
    var res: [name.len]u8 = undefined;
    if (std.mem.replace(u8, name, " ", "_", &res) == 0) {
        return name;
    } else return &res;
}

pub fn textureFor(self: GameState, id: Element) Gl.Texture {
    switch (id) {
        inline else => |x| {
            if (@hasField(@FieldType(GameState, "textures_new"), @tagName(x))) {
                return @field(self.textures_new, @tagName(x));
            } else {
                return self.textures[0];
            }
        },
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

    inline for (@typeInfo(Element).@"enum".fields) |f| {
        @field(dst.textures_new, f.name) = gl.buildTexture2D(@field(dst.textures_new_data, f.name), false);
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
            platform.sound_queue.insert(.enter);
        }
        if (hovering_to_exit_level and mouse.wasPressed(.left)) {
            self.menu_state.game_focus_target = 0.0;
            platform.sound_queue.insert(.enter);
        }

        if (hovering_to_solve_level and mouse.wasReleased(.left)) {
            self.level_states[self.menu_state.level].placed.items[self.input_state.grabbing.?].deleted = true;
            self.input_state.grabbing = null;
            self.menu_state.game_focus_target = 0;
            self.level_states[k].solved = true;
            platform.sound_queue.insert(.win);
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
        }}, self.textureFor(icon.id));
    }
    for (icons.items) |icon| {
        canvas.drawSpriteBatch(camera, &.{.{
            .point = .{ .pos = icon.rect.top_left, .scale = icon.rect.size.x },
            .texcoord = .unit,
        }}, self.textureFor(icon.id));
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
