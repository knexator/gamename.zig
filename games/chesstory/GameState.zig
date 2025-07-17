const Main = struct {
    prev_scene_state: ?SceneState,
    scene_state: SceneState,
    last_delta: SceneDelta,
    next_changes: []const SceneDelta,
    anim_t: f32,

    pub fn init(deltas: []const SceneDelta) Main {
        return .{
            .prev_scene_state = null,
            .scene_state = .applyDelta(null, deltas[0]),
            .last_delta = deltas[0],
            .next_changes = deltas[1..],
            .anim_t = 0,
        };
    }

    pub fn advance(self: *Main, option_index: ?usize) void {
        self.anim_t = 0;
        self.prev_scene_state = self.scene_state;
        if (option_index) |index| {
            assert(std.meta.activeTag(self.last_delta) == .chess_choice);
            const option = self.last_delta.chess_choice[index];
            self.last_delta = .{ .chess_move = option.move };
            self.scene_state = .applyDelta(self.scene_state, self.last_delta);
            self.next_changes = option.next;
        } else {
            self.scene_state = .applyDelta(self.scene_state, self.next_changes[0]);
            self.last_delta = self.next_changes[0];
            self.next_changes = self.next_changes[1..];
        }
    }
};

const SceneState = union(enum) {
    const DialogState = struct {
        character: enum { padre, hijo },
        text: []const u8,
    };

    const Options = []const struct {
        move: SceneDelta.ChessMove,
        label: []const u8,
        next: []const SceneDelta,
    };

    dialog: DialogState,
    chess: struct {
        board: ChessBoardState,
        chaval: ChavalState,
        options: ?Options,
    },

    fn applyDelta(maybe_cur: ?SceneState, delta: SceneDelta) SceneState {
        if (maybe_cur) |cur| {
            switch (delta) {
                .dialog => |d| {
                    return .{ .dialog = d };
                },
                .new_chess_game => |x| {
                    assert(std.meta.activeTag(cur) == .dialog);
                    return .{ .chess = .{
                        .board = x.board,
                        .chaval = x.chaval,
                        .options = null,
                    } };
                },
                .chess_move => |move| {
                    assert(std.meta.activeTag(cur) == .chess);
                    var res = cur;
                    res.chess.options = null;
                    res.chess.board.tiles.set(move.to, res.chess.board.tiles.at2(move.from));
                    res.chess.board.tiles.set(move.from, null);
                    return res;
                },
                .chess_choice => |options| {
                    assert(std.meta.activeTag(cur) == .chess);
                    var res = cur;
                    res.chess.options = options;
                    return res;
                },
            }
        } else {
            assert(std.meta.activeTag(delta) == .dialog);
            return .{ .dialog = delta.dialog };
        }
    }
};

const SceneDelta = union(enum) {
    dialog: SceneState.DialogState,
    new_chess_game: struct {
        board: ChessBoardState,
        chaval: ChavalState,
    },
    chess_move: ChessMove,
    chess_choice: SceneState.Options,

    pub const ChessMove = struct {
        from: UVec2,
        to: UVec2,
    };

    pub fn turnDuration(self: SceneDelta) f32 {
        return switch (self) {
            .new_chess_game => 1.0,
            else => 0.25,
        };
    }
};

const ChavalState = struct {
    skill: f32,
    frustration: f32,
};

const ChessBoardState = struct {
    const Piece = struct {
        color: Piece.Color,
        kind: Kind,

        pub const Color = enum { white, black };
        pub const Kind = enum {
            pawn,
            rook,
            horse,
            bishop,
            queen,
            king,
        };
    };

    tiles: kommon.grid2D.Grid2D(?Piece, .both(8)),

    pub const initial_white: ChessBoardState = blk: {
        const raw_pieces: []const struct {
            color: Piece.Color,
            kind: Piece.Kind,
            pos: UVec2,
        } = &.{
            .{ .pos = .new(0, 0), .kind = .rook, .color = .black },
            .{ .pos = .new(7, 0), .kind = .rook, .color = .black },
            .{ .pos = .new(1, 0), .kind = .horse, .color = .black },
            .{ .pos = .new(6, 0), .kind = .horse, .color = .black },
            .{ .pos = .new(2, 0), .kind = .bishop, .color = .black },
            .{ .pos = .new(5, 0), .kind = .bishop, .color = .black },
            .{ .pos = .new(3, 0), .kind = .queen, .color = .black },
            .{ .pos = .new(4, 0), .kind = .king, .color = .black },

            .{ .pos = .new(0, 1), .kind = .pawn, .color = .black },
            .{ .pos = .new(1, 1), .kind = .pawn, .color = .black },
            .{ .pos = .new(2, 1), .kind = .pawn, .color = .black },
            .{ .pos = .new(3, 1), .kind = .pawn, .color = .black },
            .{ .pos = .new(4, 1), .kind = .pawn, .color = .black },
            .{ .pos = .new(5, 1), .kind = .pawn, .color = .black },
            .{ .pos = .new(6, 1), .kind = .pawn, .color = .black },
            .{ .pos = .new(7, 1), .kind = .pawn, .color = .black },

            .{ .pos = .new(0, 6), .kind = .pawn, .color = .white },
            .{ .pos = .new(1, 6), .kind = .pawn, .color = .white },
            .{ .pos = .new(2, 6), .kind = .pawn, .color = .white },
            .{ .pos = .new(3, 6), .kind = .pawn, .color = .white },
            .{ .pos = .new(4, 6), .kind = .pawn, .color = .white },
            .{ .pos = .new(5, 6), .kind = .pawn, .color = .white },
            .{ .pos = .new(6, 6), .kind = .pawn, .color = .white },
            .{ .pos = .new(7, 6), .kind = .pawn, .color = .white },

            .{ .pos = .new(0, 7), .kind = .rook, .color = .white },
            .{ .pos = .new(7, 7), .kind = .rook, .color = .white },
            .{ .pos = .new(1, 7), .kind = .horse, .color = .white },
            .{ .pos = .new(6, 7), .kind = .horse, .color = .white },
            .{ .pos = .new(2, 7), .kind = .bishop, .color = .white },
            .{ .pos = .new(5, 7), .kind = .bishop, .color = .white },
            .{ .pos = .new(3, 7), .kind = .queen, .color = .white },
            .{ .pos = .new(4, 7), .kind = .king, .color = .white },
        };

        var res: ChessBoardState = .{ .tiles = .initFillV2(.both(8), null) };
        for (raw_pieces) |piece| {
            res.tiles.set(piece.pos, .{
                .color = piece.color,
                .kind = piece.kind,
            });
        }
        break :blk res;
    };

    pub const initial_black: ChessBoardState = blk: {
        var result: ChessBoardState = .{ .tiles = .initFillV2(.both(8), null) };
        var it = initial_white.tiles.iterator();
        while (it.next()) |p| {
            result.tiles.set(p, initial_white.tiles.at2(.new(
                7 - p.x,
                7 - p.y,
            )));
        }
        break :blk result;
    };
};

pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "chesstory",
        .author = "knexator",
        .desired_aspect_ratio = 16.0 / 9.0,
    },
    .sounds = .{},
    .loops = .{},
    .preloaded_images = .{
        .arial_atlas = "assets/fonts/Arial.png",
        .chaval = "assets/chesstory/images/chaval.png",
        .padre = "assets/chesstory/images/padre.png",
    },
};
pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const COLORS = struct {
    bg: FColor = .gray(0.5),
}{};

canvas: Canvas,
mem: Mem,
smooth: kommon.LazyState,
textures: struct {
    chaval: Gl.Texture,
    padre: Gl.Texture,
},

main: Main = .init(day_1),

const day_1: []const SceneDelta = &.{
    .{ .dialog = .{
        .character = .padre,
        .text = "hola buenas",
    } },
    .{ .dialog = .{
        .character = .padre,
        .text = "enseñale ajedrez al chaval",
    } },
    .{ .dialog = .{
        .character = .hijo,
        .text = "hola soy el chaval",
    } },
    .{ .new_chess_game = .{
        .board = .initial_white,
        .chaval = .{ .skill = 0, .frustration = 0 },
    } },
    .{ .chess_move = .{
        .from = .new(1, 1),
        .to = .new(1, 2),
    } },
    .{ .chess_choice = &.{
        .{ .label = "go easy", .move = .{
            .from = .new(3, 6),
            .to = .new(3, 5),
        }, .next = &.{
            .{ .dialog = .{
                .character = .hijo,
                .text = "bah este juego es too easy",
            } },
            .{ .dialog = .{
                .character = .padre,
                .text = "pues nada, adios",
            } },
        } },
        .{ .label = "go hard", .move = .{
            .from = .new(1, 6),
            .to = .new(1, 5),
        }, .next = &.{
            .{ .dialog = .{
                .character = .hijo,
                .text = "uff este juego es too hard",
            } },
            .{ .dialog = .{
                .character = .padre,
                .text = "pues nada, adios",
            } },
        } },
    } },
};

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("assets/fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.smooth = .init(dst.mem.forever.allocator());
    dst.textures.chaval = gl.buildTexture2D(loaded_images.get(.chaval), false);
    dst.textures.padre = gl.buildTexture2D(loaded_images.get(.padre), false);
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

    const camera = (Rect.from(.{
        .{ .top_left = .zero },
        .{ .size = .both(12) },
    })).withAspectRatio(
        platform.aspect_ratio,
        .grow,
        .top_left,
    );
    const mouse = platform.getMouse(camera);
    const canvas = &self.canvas;
    var fg_text = canvas.textBatch(0);
    platform.gl.clear(COLORS.bg);

    var advance: bool = false;
    var option_index: ?usize = null;

    switch (self.main.scene_state) {
        .dialog => |d| {
            canvas.drawSpriteBatch(camera, &.{.{
                .point = .{ .pos = camera.get(.center), .scale = 5 },
                .pivot = .center,
                .texcoord = .unit,
            }}, switch (d.character) {
                .padre => self.textures.padre,
                .hijo => self.textures.chaval,
            });
            try fg_text.addText(d.text, .{ .hor = .center, .ver = .baseline, .pos = camera.get(.bottom_center).addY(-1) }, 1, .black);

            if (mouse.wasPressed(.left)) {
                advance = true;
            }
        },
        .chess => |c| {
            canvas.drawSpriteBatch(camera, &.{.{
                .point = .{ .pos = camera.worldFromCenterLocal(.new(0.5, 0)), .scale = 5 },
                .pivot = .center,
                .texcoord = .unit,
            }}, self.textures.chaval);
            var chess_rect: Rect = .from2(
                .{ .center = camera.worldFromCenterLocal(.new(-0.5, 0)) },
                .{ .size = .both(8) },
            );
            if (std.meta.activeTag(self.main.last_delta) == .new_chess_game) {
                chess_rect = chess_rect.move(.new(0, math.lerp(camera.size.y, 0, math.smoothstepEased(self.main.anim_t, 0, 0.7, .linear))));
                // chess_rect = chess_rect.move(.new(0, math.remapClamped(camera.size.y, 0, self.main.anim_t)));
            }
            const board = c.board.tiles;
            var it = board.iterator();
            while (it.next()) |p| {
                const r = board.getTileRect(chess_rect, p);
                canvas.fillRect(camera, r, if (p.isEven()) .white else .black);
            }
            it.reset();
            while (it.next()) |p| {
                if (board.at2(p)) |piece| {
                    var piece_center: Vec2 = board.getTileRect(chess_rect, p).get(.center);
                    if (std.meta.activeTag(self.main.last_delta) == .chess_move and self.main.last_delta.chess_move.to.equals(p)) {
                        piece_center = .lerp(
                            board.getTileRect(chess_rect, self.main.last_delta.chess_move.from).get(.center),
                            board.getTileRect(chess_rect, self.main.last_delta.chess_move.to).get(.center),
                            self.main.anim_t,
                        );
                    }
                    canvas.fillCircle(camera, piece_center, 0.4, .gray(switch (piece.color) {
                        .white => 0.9,
                        .black => 0.2,
                    }));
                    try fg_text.addText(@tagName(piece.kind)[0..1], .centeredAt(piece_center), 1.0, .gray(0.5));
                }
            }

            if (c.options) |options| {
                assert(options.len == 2);
                for (options, 0.., &[2]f32{ -0.5, 0.5 }) |option, k, x| {
                    const r: Rect = .fromCenterAndSize(camera.worldFromCenterLocal(.new(x, 0.8)), .new(4, 1));
                    const hovered = r.contains(mouse.cur.position);
                    canvas.fillRect(camera, r, try self.smooth.fcolor(.fromFormat("button {d}", .{x}), if (hovered) .fromHex("#14bf14") else .white));
                    try fg_text.addText(option.label, .{
                        .hor = .center,
                        .ver = .median,
                        .pos = r.get(.center),
                    }, 1, .black);
                    if (hovered) {
                        canvas.fillCircle(camera, board.getTileRect(
                            chess_rect,
                            option.move.from,
                        ).get(.center), 0.45, .red);
                        canvas.fillCircle(camera, board.getTileRect(
                            chess_rect,
                            option.move.to,
                        ).get(.center), 0.45, .red);
                    }
                    if (hovered and mouse.wasPressed(.left)) {
                        advance = true;
                        option_index = k;
                    }
                }
            } else {
                if (mouse.wasPressed(.left)) {
                    advance = true;
                }
            }
        },
    }

    math.towards(&self.main.anim_t, 1, platform.delta_seconds / self.main.last_delta.turnDuration());
    if (self.main.anim_t == 1) switch (self.main.scene_state) {
        .dialog => {},
        .chess => |c| if (c.options == null) {
            advance = true;
        },
    };

    if (advance) {
        if (self.main.next_changes.len > 0 or option_index != null) {
            self.main.advance(option_index);
        } else return true;
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
pub const Mem = kommon.Mem;
pub const Key = kommon.Key;
pub const LazyState = kommon.LazyState;
pub const EdgePos = kommon.grid2D.EdgePos;
