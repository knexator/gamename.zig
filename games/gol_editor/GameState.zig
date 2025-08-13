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
    o,
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
    cells_states: std.AutoArrayHashMap(IVec2, CellState),
    cells_types: std.AutoArrayHashMap(IVec2, CellType),

    pub fn init(dst: *BoardState, gpa: std.mem.Allocator) !void {
        dst.cells_states = .init(gpa);
        dst.cells_types = .init(gpa);

        try dst.cells_states.put(.new(1, 1), .gray);
        try dst.cells_states.put(.new(1, 2), .white);
        try dst.cells_types.put(.new(1, 3), .@"+");
        try dst.cells_types.put(.new(1, 4), .@"*");
        try dst.cells_types.put(.new(1, 5), .o);
        try dst.cells_types.put(.new(1, 6), .@"=");
        try dst.cells_types.put(.new(1, 7), .@"-");
    }

    pub fn boundingRect(self: BoardState) math.IBounds {
        var result: math.IBounds = .empty;

        var it_states = self.cells_states.iterator();
        while (it_states.next()) |kv| {
            result.plusTile(kv.key_ptr.*);
        }

        var it_types = self.cells_types.iterator();
        while (it_types.next()) |kv| {
            result.plusTile(kv.key_ptr.*);
        }

        return result;
    }

    pub fn stateAt(self: BoardState, pos: IVec2) CellState {
        return self.cells_states.get(pos) orelse .black;
    }

    pub fn typeAt(self: BoardState, pos: IVec2) CellType {
        return self.cells_types.get(pos) orelse .empty;
    }

    pub fn toText(self: BoardState, out: std.io.AnyWriter) !void {
        const bounds = self.boundingRect();
        try out.writeAll("V1\n");
        for (0..bounds.inner_size.y) |dj| {
            for (0..bounds.inner_size.x) |di| {
                const p = bounds.top_left.addUnsigned(.new(di, dj));
                try out.writeAll(switch (self.stateAt(p)) {
                    .black => " ",
                    .gray => ".",
                    .white => ":",
                });
                try out.writeAll(switch (self.typeAt(p)) {
                    .empty => ".",
                    else => |t| t.text(),
                });
            }
            try out.writeAll("\n");
        }
    }

    pub fn fromText(dst: *BoardState, scratch: std.mem.Allocator, in: std.io.AnyReader) !void {
        dst.cells_states.clearRetainingCapacity();
        dst.cells_types.clearRetainingCapacity();
        const contents = try in.readAllAlloc(scratch, std.math.maxInt(usize));
        assert(std.mem.startsWith(u8, contents, "V1\n"));
        const raw_ascii = try kommon.Grid2D([2]u8).fromAsciiWide(2, scratch, std.mem.trim(u8, contents["V1".len..], &std.ascii.whitespace));
        var it = raw_ascii.iteratorSigned();
        while (it.next()) |p| {
            const t = raw_ascii.atSigned(p);
            const cell_state: CellState = switch (t[0]) {
                ' ' => .black,
                '.' => .gray,
                ':' => .white,
                else => return error.BadText,
            };
            const cell_type: CellType = switch (t[1]) {
                '.' => .empty,
                '+' => .@"+",
                '*' => .@"*",
                'o' => .o,
                '=' => .@"=",
                '-' => .@"-",
                else => return error.BadText,
            };
            if (cell_state != .black) try dst.cells_states.put(p, cell_state);
            if (cell_type != .empty) try dst.cells_types.put(p, cell_type);
        }
    }
};

toolbar: Toolbar = .{ .active_tool = .paint_state },

camera: Rect = .{ .top_left = .zero, .size = Vec2.new(4, 3).scale(8) },
board: BoardState,

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

    try dst.board.init(gpa);
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

    if (platform.userUploadedFile()) |reader| {
        defer platform.forgetUserUploadedFile();
        try self.board.fromText(self.mem.frame.allocator(), reader);
    }

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
                try self.board.cells_states.put(cell_under_mouse, self.toolbar.active_state);
            }
            if (mouse.wasPressed(.right)) {
                self.toolbar.active_state = self.board.cells_states.get(cell_under_mouse) orelse .black;
            }
        },
        .paint_type => {
            if (mouse.cur.isDown(.left)) {
                try self.board.cells_types.put(cell_under_mouse, self.toolbar.active_type);
            }
            if (mouse.wasPressed(.right)) {
                self.toolbar.active_type = self.board.cells_types.get(cell_under_mouse) orelse .empty;
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
            for (&[_]CellType{ .@"+", .@"*", .o, .@"=", .@"-", .empty }, 0..) |t, k| {
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
        try self.board.toText(buf.writer().any());
        platform.downloadAsFile("gol_level.txt", buf.items);
    }

    if (platform.keyboard.wasPressed(.KeyR)) {
        platform.askUserForFile();
    }

    platform.gl.clear(CellState.black.color());

    if (true) {
        var cell_bgs: std.ArrayList(Canvas.InstancedShapeInfo) = .init(self.mem.frame.allocator());
        var it = self.board.cells_states.iterator();
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
        var it = self.board.cells_types.iterator();
        while (it.next()) |kv| {
            if (kv.value_ptr.* == .empty) continue;
            try cell_texts.addText(
                kv.value_ptr.*.text(),
                .centeredAt(kv.key_ptr.*.tof32().add(.half)),
                1.0,
                if ((self.board.cells_states.get(kv.key_ptr.*) orelse .black) == .black)
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
