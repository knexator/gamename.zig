pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "gol_editor",
        .author = "knexator",
        .desired_aspect_ratio = 4.0 / 3.0,
    },
    .sounds = .{},
    .loops = .{},
    .preloaded_images = .{
        // TODO: don't require this here
        .arial_atlas = "fonts/Arial.png",
    },
};
pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const COLORS = struct {
    bg: FColor = .gray(0.5),
}{};

const CellState = enum {
    black,
    gray,
    white,

    pub fn color(self: CellState) FColor {
        return switch (self) {
            .black => .black,
            .gray => .gray(0.5),
            .white => .white,
        };
    }
};

const CellType = enum {
    empty,
    @"+",
    @"*",
    O,
    @"=",

    pub fn text(self: CellType) []const u8 {
        return switch (self) {
            inline else => |x| @tagName(x),
        };
    }
};

camera: Rect = .{ .top_left = .zero, .size = Vec2.new(4, 3).scale(8) },
cells_states: std.AutoArrayHashMap(IVec2, CellState),
cells_types: std.AutoArrayHashMap(IVec2, CellType),

random: std.Random.DefaultPrng,
canvas: Canvas,
mem: Mem,
smooth: kommon.LazyState,
// lazy_state: LocalDecisions,

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
    random_seed: u64,
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.smooth = .init(dst.mem.forever.allocator());
    // dst.lazy_state = .init(gpa);
    dst.random = .init(random_seed);

    dst.cells_states = .init(gpa);
    dst.cells_types = .init(gpa);
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
    self.smooth.last_delta_seconds = platform.delta_seconds;
    // self.lazy_state.frameStart();

    const camera = self.camera.withAspectRatio(platform.aspect_ratio, .grow, .top_left);
    const mouse = platform.getMouse(camera);
    const cell_under_mouse = mouse.cur.position.toInt(isize);

    self.camera = self.camera.zoom(mouse.cur.position, switch (mouse.cur.scrolled) {
        .none => 1,
        .down => 1.1,
        .up => 0.9,
    });

    for (Vec2.cardinal_directions, &[4]KeyboardButton{ .KeyD, .KeyS, .KeyA, .KeyW }) |d, k| {
        if (platform.keyboard.cur.isDown(k)) {
            self.camera = self.camera.move(d.scale(platform.delta_seconds * 0.8 * self.camera.size.y));
        }
    }

    if (mouse.cur.isDown(.middle)) {
        // TODO
        // platform.setCursor(.dragging)
        self.camera = self.camera.move(mouse.deltaPos().neg());
    }

    if (platform.keyboard.cur.isDown(.Space)) {
        // TODO
        // platform.setCursor(.drag)
        if (mouse.cur.isDown(.left) or mouse.cur.isDown(.middle) or mouse.cur.isDown(.right)) {
            // platform.setCursor(.dragging)
            self.camera = self.camera.move(mouse.deltaPos().neg());
        }
    } else {
        if (mouse.wasPressed(.left)) {
            const old_type: CellState = self.cells_states.get(cell_under_mouse) orelse .black;
            try self.cells_states.put(cell_under_mouse, switch (old_type) {
                .black => .gray,
                .gray => .white,
                .white => .black,
            });
        }

        if (mouse.wasPressed(.right)) {
            const old_type: CellState = self.cells_states.get(cell_under_mouse) orelse .black;
            try self.cells_states.put(cell_under_mouse, switch (old_type) {
                .black => .white,
                .gray => .black,
                .white => .gray,
            });
        }
    }

    for (&[_]CellType{ .empty, .@"+", .@"*", .@"+", .O, .@"=" }, 0..) |t, k| {
        if (platform.keyboard.wasPressed(.digit(k + 1))) {
            try self.cells_types.put(cell_under_mouse, t);
        }
    }

    platform.gl.clear(CellState.black.color());

    if (true) {
        var cell_bgs: std.ArrayList(Canvas.InstancedShapeInfo) = .init(self.mem.frame.allocator());
        var it = self.cells_states.iterator();
        while (it.next()) |kv| {
            if (kv.value_ptr.* == .black) continue;
            try cell_bgs.append(.{
                .point = .{ .pos = kv.key_ptr.*.tof32() },
                .color = kv.value_ptr.*.color(),
            });
        }
        self.canvas.fillShapesInstanced(camera, self.canvas.DEFAULT_SHAPES.square, cell_bgs.items);
    }

    if (true) {
        var cell_texts = self.canvas.textBatch(0);
        var it = self.cells_types.iterator();
        while (it.next()) |kv| {
            if (kv.value_ptr.* == .empty) continue;
            try cell_texts.addText(kv.value_ptr.*.text(), .centeredAt(kv.key_ptr.*.tof32().add(.half)), 1.0, .cyan);
        }
        cell_texts.draw(camera);
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
pub const Mem = kommon.Mem;
pub const Key = kommon.Key;
pub const LazyState = kommon.LazyState;
pub const EdgePos = kommon.grid2D.EdgePos;
// pub const LocalDecisions = @import("../chesstory/GameState.zig").LocalDecisions;
