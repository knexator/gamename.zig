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
            .black => .fromHex("#444444"),
            .gray => .fromHex("#999999"),
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
    @"-",

    pub fn text(self: CellType) []const u8 {
        return switch (self) {
            .empty => unreachable,
            inline else => |x| @tagName(x),
        };
    }
};

const Toolbar = struct {
    active_state: CellState = .white,
    active_type: CellType = .@"+",

    active_tool: enum { paint_state, paint_type },
};

const BoardState = struct {
    pub fn toText(out: std.io.AnyWriter) !void {
        try out.writeAll("TODO");
    }
};

toolbar: Toolbar = .{ .active_tool = .paint_state },

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

    try dst.cells_states.put(.new(1, 1), .gray);
    try dst.cells_states.put(.new(1, 2), .white);
    try dst.cells_types.put(.new(1, 3), .@"+");
    try dst.cells_types.put(.new(1, 4), .@"*");
    try dst.cells_types.put(.new(1, 5), .O);
    try dst.cells_types.put(.new(1, 6), .@"=");
    try dst.cells_types.put(.new(1, 7), .@"-");
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

    for (Vec2.cardinal_directions, &[4][]const KeyboardButton{
        &.{ .KeyD, .ArrowRight },
        &.{ .KeyS, .ArrowDown },
        &.{ .KeyA, .ArrowLeft },
        &.{ .KeyW, .ArrowUp },
    }) |d, ks| {
        for (ks) |k| {
            if (platform.keyboard.cur.isDown(k)) {
                self.camera = self.camera.move(d.scale(platform.delta_seconds * 0.8 * self.camera.size.y));
            }
        }
    }

    if (mouse.cur.isDown(.middle) or platform.keyboard.cur.isDown(.KeyG)) {
        // TODO
        // platform.setCursor(.dragging)
        self.camera = self.camera.move(mouse.deltaPos().neg());
    }

    switch (self.toolbar.active_tool) {
        .paint_state => {
            if (mouse.cur.isDown(.left)) {
                try self.cells_states.put(cell_under_mouse, self.toolbar.active_state);
            }
            if (mouse.wasPressed(.right)) {
                self.toolbar.active_state = self.cells_states.get(cell_under_mouse) orelse .black;
            }
        },
        .paint_type => {
            if (mouse.cur.isDown(.left)) {
                try self.cells_types.put(cell_under_mouse, self.toolbar.active_type);
            }
            if (mouse.wasPressed(.right)) {
                self.toolbar.active_type = self.cells_types.get(cell_under_mouse) orelse .empty;
            }
        },
    }

    switch (self.toolbar.active_tool) {
        .paint_state => {
            for (&[_]CellState{ .black, .gray, .white }, 0..) |t, k| {
                if (platform.keyboard.wasPressed(.digit(k + 1))) {
                    self.toolbar.active_state = t;
                }
            }
            if (platform.keyboard.wasPressed(.Backquote) or platform.keyboard.wasPressed(.Space)) {
                self.toolbar.active_tool = .paint_type;
            }
        },
        .paint_type => {
            for (&[_]CellType{ .@"+", .@"*", .O, .@"=", .@"-", .empty }, 0..) |t, k| {
                if (platform.keyboard.wasPressed(.digit(k + 1))) {
                    self.toolbar.active_type = t;
                }
            }
            if (platform.keyboard.wasPressed(.Backquote) or platform.keyboard.wasPressed(.Space)) {
                self.toolbar.active_tool = .paint_state;
            }
        },
    }

    if (platform.keyboard.wasPressed(.KeyF)) {
        var buf: std.ArrayList(u8) = .init(self.mem.frame.allocator());
        defer buf.deinit();
        try BoardState.toText(buf.writer().any());
        platform.downloadAsFile("gol_level.txt", buf.items);
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
            try cell_texts.addText(
                kv.value_ptr.*.text(),
                .centeredAt(kv.key_ptr.*.tof32().add(.half)),
                1.0,
                if ((self.cells_states.get(kv.key_ptr.*) orelse .black) == .black)
                    .white
                else
                    .black,
            );
        }
        cell_texts.draw(camera);
    }

    if (true) {
        var segments: std.ArrayList(Canvas.Segment) = .init(self.mem.frame.allocator());
        {
            var x = @floor(camera.top_left.x);
            while (x <= camera.top_left.x + camera.size.x) : (x += 1) {
                try segments.append(.{ .a = .new(x, camera.top_left.y), .b = .new(x, camera.top_left.y + camera.size.y), .color = .black });
            }
        }
        {
            var y = @floor(camera.top_left.y);
            while (y <= camera.top_left.y + camera.size.y) : (y += 1) {
                try segments.append(.{ .a = .new(camera.top_left.x, y), .b = .new(camera.top_left.x + camera.size.x, y), .color = .black });
            }
        }

        self.canvas.instancedSegments(camera, segments.items, 0.05);
    }

    if (true) {
        const ui_cam = camera.with(.{ .top_left = .zero }, .size);
        self.canvas.fillRect(ui_cam, Rect.unit.plusMargin(0.1), .cyan);
        switch (self.toolbar.active_tool) {
            .paint_state => self.canvas.fillRect(ui_cam, .unit, self.toolbar.active_state.color()),
            .paint_type => if (self.toolbar.active_type != .empty) {
                var cell_texts = self.canvas.textBatch(0);
                try cell_texts.addText(self.toolbar.active_type.text(), .centeredAt(.half), 1.0, .black);
                cell_texts.draw(ui_cam);
            },
        }
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
