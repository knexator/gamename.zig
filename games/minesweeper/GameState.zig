pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};
pub const tracy = @import("tracy");

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "minesweeper",
        .author = "knexator",
        .desired_aspect_ratio = 1.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

usual: kommon.Usual,
board: kommon.Grid2D(TileState),
prev_board: kommon.Grid2D(TileState),
prev_board_changed_at: kommon.Grid2D(f32),

const TileState = struct {
    has_bomb: bool,
    has_flag: bool,
    uncovered: bool,
    surrounding_bombs: u8,
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

    const random: math.Random = .init(dst.usual.random.random());

    const board_size: UVec2 = .new(12, 12);
    dst.board = try .initFill(gpa, board_size, .{
        .has_bomb = false,
        .has_flag = false,
        .uncovered = false,
        .surrounding_bombs = 0,
    });
    for (0..20) |_| {
        dst.board.getPtr(random.inURect(.{ .top_left = .zero, .inner_size = dst.board.size })).has_bomb = true;
    }
    fixSurroundingBombsCount(&dst.board);

    dst.prev_board = try dst.board.clone(gpa);

    dst.prev_board_changed_at = try .initFill(gpa, board_size, -std.math.inf(f32));
}

fn fixSurroundingBombsCount(board: *kommon.Grid2D(TileState)) void {
    var tiles_it = board.iteratorSigned();
    while (tiles_it.next()) |p| {
        var surrounding_bombs: u8 = 0;
        for (IVec2.eight_directions) |d| {
            if (board.atSignedSafe(p.add(d))) |x| {
                if (x.has_bomb) surrounding_bombs += 1;
            }
        }
        board.getPtrSigned(p).surrounding_bombs = surrounding_bombs;
    }
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.board.deinit(gpa);
    self.prev_board.deinit(gpa);
    self.prev_board_changed_at.deinit(gpa);
    self.usual.deinit(undefined);
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    _ = self;
}

const COLORS: struct {
    bg: FColor = .black,
    numbers: FColor = .white,
    covered: FColor = .fromHsv(0.2, 0.6, 0.6),
    flag_bg: FColor = .gray(0.5),
    flag_icon: FColor = .black,
    borders: FColor = .white,
} = .{};

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);
    const canvas = &self.usual.canvas;

    const board_rect: Rect = .{ .top_left = .zero, .size = self.board.size.tof32() };
    const camera: Rect = board_rect.withAspectRatio(platform.aspect_ratio, .grow, .center);
    const mouse = platform.getMouse(camera);

    platform.gl.clear(COLORS.bg);

    if (true) { // hacky juice: randomly reveal tiles next to already revealed tiles
        var it = self.board.iteratorSigned();
        while (it.next()) |pos| {
            const tile = self.board.atSigned(pos);
            if (tile.uncovered and tile.surrounding_bombs == 0) {
                for (IVec2.eight_directions) |d| {
                    if (self.board.atSignedSafe(pos.add(d))) |neighbor| {
                        if (!neighbor.uncovered and !neighbor.has_bomb) {
                            // should depend on delta time, but idc
                            if (self.usual.random.random().float(f32) < 0.1) {
                                self.board.getPtrSigned(pos.add(d)).uncovered = true;
                            }
                        }
                    }
                }
            }
        }
    }

    if (self.board.tileAt(mouse.cur.position, board_rect)) |pos_under_mouse| {
        const old_tile = self.board.at2(pos_under_mouse);
        if (mouse.wasPressed(.left) and !old_tile.uncovered) {
            self.prev_board.set(pos_under_mouse, old_tile);
            self.prev_board_changed_at.set(pos_under_mouse, platform.global_seconds);
            self.board.getPtr(pos_under_mouse).uncovered = true;
            self.board.getPtr(pos_under_mouse).has_flag = false;
        }
        if (mouse.wasPressed(.right) and !old_tile.uncovered) {
            self.prev_board.set(pos_under_mouse, old_tile);
            self.prev_board_changed_at.set(pos_under_mouse, platform.global_seconds);
            const tile = self.board.getPtr(pos_under_mouse);
            tile.has_flag = !tile.has_flag;
        }
    }

    if (true) { // draw all numbers at once, since they use another shader
        var batch = canvas.textBatch(0);
        var it = self.board.iterator();
        while (it.next()) |pos| {
            const tile = self.board.at2(pos);
            const rect = self.board.getTileRect(camera, pos);
            if (tile.has_bomb) {
                try batch.addText("X", .centeredAt(rect.get(.center)), 1.0, COLORS.numbers);
            } else if (tile.surrounding_bombs > 0) {
                try batch.addFmt("{d}", .{tile.surrounding_bombs}, .centeredAt(rect.get(.center)), 1.0, COLORS.numbers);
            }
        }
        batch.draw(camera);
    }

    // const mouse_over = rect.contains(mouse.cur.position);
    // canvas.fillRect(camera, rect, try smooth.fcolor(
    //     .fromFormat("bg {d}", .{key}),
    //     if (g.lighted) .fromHex("#BBFF87") else .white,
    // ));

    var it = self.board.iterator();
    while (it.next()) |pos| {
        const rect = self.board.getTileRect(camera, pos);
        const tile = self.board.at2(pos);
        // const tile_prev = self.prev_board.at2(pos);
        // const seconds_since_change = platform.global_seconds - self.prev_board_changed_at.at2(pos);

        if (tile.uncovered) {
            // nothing
        } else {
            if (tile.has_flag) {
                canvas.fillRect(camera, rect.resizeRel(0.8, .center), COLORS.flag_bg);
            } else {
                canvas.fillRect(camera, rect, COLORS.covered);
            }
        }
    }

    return false;
}

const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;

const kommon = @import("kommon");
pub const Key = kommon.Key;
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
