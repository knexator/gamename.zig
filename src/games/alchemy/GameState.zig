const COLORS = struct {
    text: FColor = .black,
    bg_element: FColor = .gray(0.9),
    bg_board: FColor = .white,
}{};

pub const Combo = struct { new_prod: usize, old_prod: usize, old_res: usize };

const places: [3]Rect = .{
    Rect.unit.move(.new(2, 2)),
    Rect.unit.move(.new(4, 2)),
    Rect.unit.move(.new(6, 2)),
};

const InputState = struct {
    hovering: ?union(enum) {
        element: usize,
        machine: usize,
    },
    grabbing: ?usize,
};

const BoardState = struct {
    // current design: never remove items lol
    placed: std.ArrayList(struct {
        pos: Vec2,
        id: usize,
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

    pub fn init(gpa: std.mem.Allocator) BoardState {
        return .{
            .placed = .init(gpa),
        };
    }

    pub fn addInitialElements(self: *BoardState) !void {
        for (AlchemyData.initial, 0..) |k, p| {
            try self.placed.append(.{ .id = k, .pos = .new(tof32(p) * 1.3 + 0.2, 4) });
        }
    }

    pub fn machineCombo(self: *BoardState, ingredient: ?usize, result: ?usize) !?usize {
        if (ingredient == null or result == null) return null;
        assert(ingredient.? != result.?);
        if (AlchemyData.combinationOf(
            self.placed.items[ingredient.?].id,
            self.placed.items[result.?].id,
        )) |new_id| {
            try self.placed.append(.{ .id = new_id, .pos = places[0].top_left });
            return self.placed.items.len - 1;
        } else return null;
    }

    pub fn deleteElements(self: *BoardState, indices: []const ?usize) void {
        for (indices) |id| {
            if (id != null) {
                self.placed.items[id.?].deleted = true;
            }
        }

        // if (indices.len == 0) return;
        // if (indices.len == 1) {
        //     if (indices[0]) |i| _ = self.placed.swapRemove(i);
        //     return;
        // }
        // // hardcoded for now
        // assert(indices.len == 2);
        // if (indices[0] == null) {
        //     self.deleteElements(&.{indices[1]});
        // } else if (indices[1] == null) {
        //     self.deleteElements(&.{indices[0]});
        // } else {
        //     const in0 = indices[0].?;
        //     const in1 = indices[1].?;
        //     assert(in0 != in1);
        //     if (in0 < in1) {
        //         _ = self.placed.swapRemove(in1);
        //         _ = self.placed.swapRemove(in0);
        //     } else {
        //         _ = self.placed.swapRemove(in0);
        //         _ = self.placed.swapRemove(in1);
        //     }
        // }
    }
};

const AlchemyData = struct {
    const names: []const []const u8 = @import("names.zon");
    const images_base64: []const []const u8 = @import("images.zon");
    const recipes: []const []const [2]usize = @import("base.zon");
    const Asdf = struct { mixes: usize, a: usize, b: usize };
    const required_mixes: [names.len]Asdf = blk: {
        @setEvalBranchQuota(names.len * 100);
        var result: [names.len]Asdf = @splat(.{ .mixes = std.math.maxInt(u20), .a = 0, .b = 0 });
        for (.{ 1, 2, 3, 4 }) |initial_element| result[initial_element].mixes = 0;
        var any_changes = true;
        while (any_changes) {
            any_changes = false;
            for (recipes, 0..) |r, k| {
                for (r) |pair| {
                    const new_value = 1 + result[pair[0]].mixes + result[pair[1]].mixes;
                    if (new_value < result[k].mixes) {
                        result[k] = .{ .mixes = new_value, .a = pair[0], .b = pair[1] };
                        any_changes = true;
                    }
                }
            }
        }
        break :blk result;
    };
    // const initial: []const usize = &.{ 80, 24, 13, 1 };
    const initial_names: []const []const u8 = &.{
        // "iced tea",
        // "egg timer",
        // "alarm clock",
        // "sound",
        // "sundial",

        // // goal: ocean
        // "sprinkles",
        // "confetti",
        // "marshmallows",
        // "smoke signal",
        // "bandage",
        // "shark",

        // goal: fire
        "phoenix",
        "pegasus",
        "centaur",
        "minotaur",
        "manatee",
        "sea",
    };
    const initial: [initial_names.len]usize = blk: {
        @setEvalBranchQuota(names.len * initial_names.len * 10);
        var result: [initial_names.len]usize = undefined;
        for (initial_names, &result) |name, *dst| {
            for (names, 0..) |n, k| {
                if (std.mem.eql(u8, name, n)) {
                    dst.* = k;
                    break;
                }
            } else @compileError(name);
        }
        break :blk result;
    };

    pub fn combinationOf(active_id: usize, pasive_id: usize) ?usize {
        for (recipes[pasive_id]) |pair| {
            if (pair[0] == active_id) return pair[1];
            if (pair[1] == active_id) return pair[0];
        } else return null;
    }

    pub fn additionOf(active_id: usize, pasive_id: usize) ?usize {
        for (recipes, 0..) |r, k| {
            for (r) |pair| {
                if (pair[0] == active_id and pair[1] == pasive_id) return k;
                if (pair[1] == active_id and pair[0] == pasive_id) return k;
            }
        } else return null;
    }
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

board: BoardState,
input_state: InputState = .{ .grabbing = null, .hovering = null },

machine_state: [3]?usize = @splat(null),

menu_state: struct {
    game_focus: f32 = 0,
    game_focus_target: f32 = 0,
    level: usize = 0,
} = .{},

level_states: [levels.len]LevelState = @splat(.{}),

const levels: []const LevelInfo = &.{
    .{ .icon = 2 },
    .{ .icon = 4 },
};

const LevelInfo = struct { icon: usize };
const LevelState = struct { solved: bool = false };

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
    dst.board = .init(gpa);
    try dst.board.addInitialElements();
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

pub fn grabbingId(self: GameState) ?usize {
    if (self.input_state.grabbing) |g| {
        return self.board.placed.items[g].id;
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

    var icons: std.ArrayList(struct { id: usize, rect: Rect }) = .init(canvas.frame_arena.allocator());
    var level_icons: std.ArrayList(struct { id: usize, rect: Rect, solved: f32 = 0.0 }) = .init(canvas.frame_arena.allocator());
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
        }, 2, .black);
        try title_text.addText("Alchemy", .{
            .hor = .center,
            .ver = .baseline,
            .pos = .new(math.lerp(
                15,
                4,
                math.easings.easeOutCubic(math.clamp01(platform.global_seconds - 0.8)),
            ), -3),
        }, 2, .black);
    }

    for (levels, 0..) |info, k| {
        const rect: Rect = .{ .top_left = .new(
            tof32(k) * 1.5,
            if (k == self.menu_state.level)
                math.lerp(-1.7, 0.1, game_focus)
            else
                -1.7,
        ), .size = .one };

        const hovering = rect.plusMargin(0.1).contains(mouse.cur.position);
        const hovering_to_enter_level = hovering and self.menu_state.game_focus_target == 0;
        const hovering_to_solve_level = hovering and self.grabbingId() == info.icon;
        const hovering_to_exit_level = hovering and self.input_state.grabbing == null and self.menu_state.game_focus_target == 1;

        try level_icons.append(.{
            .rect = rect,
            .id = info.icon,
            .solved = if (hovering_to_solve_level or self.level_states[k].solved) 1.0 else 0.0,
        });

        canvas.borderRect(camera, rect.plusMargin(0.1), try self.smooth.float(
            .fromFormat("menu {d}", .{k}),
            if (hovering_to_enter_level or hovering_to_exit_level) 0.1 else 0.01,
        ), .inner, .black);

        if (hovering_to_enter_level and mouse.wasPressed(.left)) {
            self.menu_state.level = k;
            self.menu_state.game_focus_target = 1.0;
        }
        if (hovering_to_exit_level and mouse.wasPressed(.left)) {
            self.menu_state.game_focus_target = 0.0;
        }

        if (hovering_to_solve_level and mouse.wasReleased(.left)) {
            self.board.placed.items[self.input_state.grabbing.?].deleted = true;
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

    for (self.board.placed.items, 0..) |*element, k| {
        if (element.deleted) continue;
        if (self.machine_state[0] != k and self.input_state.grabbing == null) {
            element.pos = element.pos.towardsPure(
                element.pos.add(.half).awayFrom(places[0].get(.center), 1.0).sub(.half),
                10 * platform.delta_seconds,
            );
        }
    }

    self.input_state.hovering = null;
    for (self.board.placed.items, 0..) |element, k| {
        if (element.deleted) continue;
        try icons.append(.{
            .rect = element.rect(),
            .id = element.id,
        });
        try fg_text.addText(
            AlchemyData.names[element.id],
            .{
                .hor = .center,
                .ver = .median,
                .pos = element.rect().get(.bottom_center).addY(0.1),
            },
            2.5 / tof32(AlchemyData.names[element.id].len),
            COLORS.text,
        );
        if (k == self.input_state.grabbing) continue;
        if (self.input_state.grabbing == null and element.rect().contains(mouse.cur.position)) {
            self.input_state.hovering = .{ .element = k };
        }
    }

    inline for (places, self.machine_state, 0..) |place, contents, k| {
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
            }, 1, COLORS.text.withAlpha(if (hovered or k == 0) 0.2 else 1));
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
            .machine => |k| blk: {
                const index = self.machine_state[k];
                if (k == 0) {
                    self.board.deleteElements(&.{ self.machine_state[1], self.machine_state[2] });
                    self.machine_state[1] = null;
                    self.machine_state[2] = null;
                } else {
                    self.board.deleteElements(&.{self.machine_state[0]});
                    self.machine_state[0] = null;
                }
                self.machine_state[k] = null;
                break :blk index;
            },
        } else null;
        self.input_state.hovering = null;
    }
    if (self.input_state.grabbing) |grabbing_index| {
        self.board.placed.items[grabbing_index].pos.addInPlace(mouse.cur.position.sub(mouse.prev.position));
    }
    if (mouse.wasReleased(.left)) {
        if (self.input_state.grabbing) |grabbing| {
            if (self.input_state.hovering) |h| switch (h) {
                .element => unreachable,
                .machine => |k| {
                    assert(k != 0);
                    self.machine_state[k] = grabbing;
                    self.board.placed.items[grabbing].pos = places[k].top_left;
                    self.input_state.grabbing = null;
                    self.machine_state[0] = try self.board.machineCombo(
                        self.machine_state[1],
                        self.machine_state[2],
                    );
                },
            };
        }
        self.input_state.grabbing = null;
    }

    if (self.machine_state[0] != null) {
        canvas.strokeCircle(128, camera, places[0].get(.center).addY(0.2), 0.7, 0.02, .black);
    }
    fg_text.draw(camera);
    for (level_icons.items) |icon| {
        canvas.drawSpriteBatch(camera, &.{.{
            .point = .{ .pos = icon.rect.top_left, .scale = icon.rect.size.x },
            .texcoord = .unit,
            .tint = .lerp(.black, .white, icon.solved),
        }}, self.textures[icon.id]);
    }
    for (icons.items) |icon| {
        canvas.drawSpriteBatch(camera, &.{.{
            .point = .{ .pos = icon.rect.top_left, .scale = icon.rect.size.x },
            .texcoord = .unit,
        }}, self.textures[icon.id]);
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
