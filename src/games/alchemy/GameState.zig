const COLORS = struct {
    text: FColor = .black,
    bg_element: FColor = .gray(0.9),
    bg_board: FColor = .white,
    bg_sidepanel: FColor = .gray(0.8),
}{};

const InputState = struct {
    hovering: ?union(enum) { element: usize, sidepanel: usize },
    grabbing: ?usize,
};

const BoardState = struct {
    placed: std.ArrayList(struct {
        pos: Vec2,
        id: usize,

        pub fn rect(self: @This()) Rect {
            return .{ .top_left = self.pos, .size = .one };
        }

        pub fn label(self: @This()) Canvas.TextRenderer.TextPosition {
            return .{
                .hor = .center,
                .ver = .median,
                .pos = self.pos.add(.half),
            };
        }
    }),
    discovered: [AlchemyData.names.len]bool = @splat(false),

    pub fn init(gpa: std.mem.Allocator) BoardState {
        return .{
            .placed = .init(gpa),
        };
    }

    pub fn addInitialElements(self: *BoardState) !void {
        for (AlchemyData.initial) |k| self.discovered[k] = true;
    }

    pub fn combine(self: *BoardState, active: usize, pasive: usize) !void {
        assert(active != pasive);
        if (AlchemyData.combinationOf(
            self.placed.items[active].id,
            self.placed.items[pasive].id,
        )) |new_id| {
            self.discovered[new_id] = true;
            self.placed.items[pasive].id = new_id;
            self.placed.items[pasive].pos = .lerp(
                self.placed.items[pasive].pos,
                self.placed.items[active].pos,
                0.5,
            );
            _ = self.placed.swapRemove(active);
        }
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

        "sprinkles",
        "confetti",
        "marshmallows",
        "smoke signal",
        "bandage",
        "shark",
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

textures: [AlchemyData.images_base64.len]Gl.Texture = undefined,

board: BoardState,
input_state: InputState = .{ .grabbing = null, .hovering = null },
sidepanel_offset: f32 = 0,

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("../../fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.board = .init(gpa);
    try dst.board.addInitialElements();
    // for (AlchemyData.names, AlchemyData.required_mixes) |name, k| {
    //     std.log.debug("{d} for {s}, via {s} + {s}", .{ k.mixes, name, AlchemyData.names[k.a], AlchemyData.names[k.b] });
    // }

    for (&dst.textures, 0..) |*texture, k| {
        if (std.mem.eql(u8, "NULL", AlchemyData.images_base64[k])) continue;
        const data = gl.loadTextureDataFromBase64(AlchemyData.images_base64[k]);
        assert(gl.isTextureDataLoaded(data));
        texture.* = gl.buildTexture2D(data, false);
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

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    _ = self.mem.frame.reset(.retain_capacity);
    _ = self.mem.scratch.reset(.retain_capacity);

    const camera = (Rect{
        .top_left = .zero,
        .size = .both(7),
    }).withAspectRatio(
        platform.aspect_ratio,
        .grow,
        .top_left,
    );
    const mouse = platform.getMouse(camera);

    const canvas = &self.canvas;

    var icons: std.ArrayList(struct { id: usize, rect: Rect }) = .init(canvas.frame_arena.allocator());
    var fg_text = canvas.textBatch(0);

    // var side_panel = ui.scrollablePanel(.{ .top_left = .zero, .size = .new(1, camera.size.y) });
    // for (self.discovered, 0..) |discovered, k| {
    //     if (!discovered) continue;
    //     var asdf = side_panel.add(.one, struct {
    //         id: usize,
    //         const flags = .{
    //             .hovereable,
    //             .clickable,
    //         };
    //     }, .{ .id = k });
    //     if (asdf.clicked) {
    //         ui.setGrabbed(self.board.addElementAt(asdf.pos, k));
    //     }
    // }

    // for (self.board.placed.items, 0..) |element, k| {
    //     var asdf = ui.addAlchemyElement(BoardState.elementRect(e));
    //     ui.add(e.pos)
    // }

    // ui.draw(self.canvas, camera);

    self.input_state.hovering = null;

    {
        var y = -self.sidepanel_offset;
        for (self.board.discovered, 0..) |discovered, k| {
            if (!discovered) continue;
            const rect: Rect = .{ .top_left = .new(0, y), .size = .one };
            if (self.input_state.grabbing == null and rect.contains(mouse.cur.position)) {
                self.input_state.hovering = .{ .sidepanel = k };
            }
            try icons.append(.{
                .rect = rect,
                .id = k,
            });
            try fg_text.addText(
                AlchemyData.names[k],
                .{
                    .hor = .center,
                    .ver = .median,
                    .pos = rect.getCenter(),
                },
                1.9 / tof32(AlchemyData.names[k].len),
                COLORS.text,
            );
            y += 1;
        }
    }

    for (self.board.placed.items, 0..) |element, k| {
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
        if (element.rect().contains(mouse.cur.position)) {
            self.input_state.hovering = .{ .element = k };
        }
    }

    const place_1 = Rect.unit.move(.new(2, 0));
    const place_2 = Rect.unit.move(.new(4, 0));
    const place_3 = Rect.unit.move(.new(6, 0));

    if (self.input_state.grabbing) |grabbing_index| {
        const element = self.board.placed.items[grabbing_index];
        try icons.append(.{
            .rect = place_2,
            .id = element.id,
        });
        try fg_text.addText(
            AlchemyData.names[element.id],
            .{
                .hor = .center,
                .ver = .median,
                .pos = place_2.get(.bottom_center).addY(0.1),
            },
            2.5 / tof32(AlchemyData.names[element.id].len),
            COLORS.text,
        );
    } else {
        try fg_text.addText("?", .{ .hor = .center, .ver = .median, .pos = place_2.get(.center) }, 1, COLORS.text);
    }
    try fg_text.addText("?", .{ .hor = .center, .ver = .median, .pos = place_1.get(.center) }, 1, COLORS.text);
    try fg_text.addText("+", .{ .hor = .center, .ver = .median, .pos = .lerp(place_1.get(.center), place_2.get(.center), 0.5) }, 1, COLORS.text);
    try fg_text.addText("=", .{ .hor = .center, .ver = .median, .pos = .lerp(place_2.get(.center), place_3.get(.center), 0.5) }, 1, COLORS.text);
    if (self.input_state.grabbing != null and self.input_state.hovering != null and std.meta.activeTag(self.input_state.hovering.?) == .element) {
        const element = self.board.placed.items[self.input_state.hovering.?.element];
        try icons.append(.{
            .rect = place_3,
            .id = element.id,
        });
        try fg_text.addText(
            AlchemyData.names[element.id],
            .{
                .hor = .center,
                .ver = .median,
                .pos = place_3.get(.bottom_center).addY(0.1),
            },
            2.5 / tof32(AlchemyData.names[element.id].len),
            COLORS.text,
        );
    } else {
        try fg_text.addText("?", .{ .hor = .center, .ver = .median, .pos = place_3.get(.center) }, 1, COLORS.text);
    }

    if (mouse.cur.position.x <= 1) {
        self.sidepanel_offset -= mouse.cur.scrolled.toNumber();
    }
    if (mouse.wasPressed(.left)) {
        assert(self.input_state.grabbing == null);
        self.input_state.grabbing = if (self.input_state.hovering) |h| switch (h) {
            .element => |k| k,
            .sidepanel => |element_id| blk: {
                try self.board.placed.append(.{ .id = element_id, .pos = mouse.cur.position });
                break :blk self.board.placed.items.len - 1;
            },
        } else null;
        self.input_state.hovering = null;
    }
    if (self.input_state.grabbing) |grabbing_index| {
        self.board.placed.items[grabbing_index].pos.addInPlace(mouse.cur.position.sub(mouse.prev.position));
    }
    if (mouse.wasReleased(.left)) {
        if (self.input_state.grabbing) |grabbing| {
            if (self.input_state.hovering != null) {
                try self.board.combine(grabbing, self.input_state.hovering.?.element);
            } else if (self.board.placed.items[grabbing].pos.x <= 1) {
                _ = self.board.placed.swapRemove(grabbing);
            }
        }
        self.input_state.grabbing = null;
    }

    platform.gl.clear(COLORS.bg_board);
    for (icons.items) |icon| {
        canvas.drawSpriteBatch(camera, &.{.{
            .point = .{ .pos = icon.rect.top_left, .scale = icon.rect.size.x },
            .texcoord = .unit,
        }}, self.textures[icon.id]);
    }
    fg_text.draw(camera);

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
