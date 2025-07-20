const Main = struct {
    prev_scene_state: SceneState,
    scene_state: SceneState,
    last_delta: SceneDelta,
    next_changes: []const SceneDelta,
    anim_t: f32,

    pub fn init(deltas: []const SceneDelta) Main {
        return .{
            .prev_scene_state = .init,
            .scene_state = .init,
            .last_delta = .started_game,
            .next_changes = deltas,
            .anim_t = 0,
        };
    }

    pub fn advance(self: *Main, option_index: ?usize) void {
        self.anim_t = 0;
        self.prev_scene_state = self.scene_state;
        if (option_index) |index| {
            assert(self.scene_state.options != null);
            const option = self.scene_state.options.?[index];
            if (option.move) |move| {
                self.last_delta = .{ .chess_move = move };
            } else {
                self.last_delta = .{ .dialog = .{ .character = .teacher, .text = option.label } };
            }
            var new_scene_state = self.scene_state;
            new_scene_state.chaval_state = new_scene_state.chaval_state.applyDelta(option.effect);
            self.scene_state = .applyDelta(new_scene_state, self.last_delta);
            self.next_changes = option.next;
        } else {
            assert(self.scene_state.options == null);
            switch (self.next_changes[0]) {
                .next_day => |next_day| {
                    self.last_delta = self.next_changes[0];
                    self.next_changes = next_day;
                    self.scene_state = .applyDelta(self.scene_state, self.last_delta);
                },
                .branch_dynamic => |b| {
                    const next_deltas = b(self.scene_state.chaval_state);
                    self.scene_state = .applyDelta(self.scene_state, next_deltas[0]);
                    self.last_delta = next_deltas[0];
                    self.next_changes = next_deltas[1..];
                },
                else => {
                    self.scene_state = .applyDelta(self.scene_state, self.next_changes[0]);
                    self.last_delta = self.next_changes[0];
                    self.next_changes = self.next_changes[1..];
                },
            }
        }
    }
};

const Character = enum {
    father,
    kid,
    teacher,
    pub fn name(c: Character) []const u8 {
        return switch (c) {
            .father => "Jack",
            .kid => "Charlie",
            .teacher => "Frank",
        };
    }
};

const SceneState = struct {
    chaval_state: ChavalState,
    dialog: ?DialogState,
    chess_board: ?ChessBoardState,
    options: ?Options,

    main_menu: bool,
    fadeout: bool,

    pub const init: SceneState = .{
        .chaval_state = .init,
        .dialog = null,
        .chess_board = null,
        .options = null,
        .main_menu = true,
        .fadeout = false,
    };

    const DialogState = struct {
        character: Character,
        text: []const u8,
    };

    const Options = []const struct {
        move: ?SceneDelta.ChessMove,
        label: []const u8,
        effect: ChavalState.Delta,
        next: []const SceneDelta,
    };

    fn applyDelta(cur: SceneState, delta: SceneDelta) SceneState {
        var res = cur;
        res.main_menu = false;
        res.fadeout = false;
        switch (delta) {
            .branch_dynamic => unreachable,
            .started_game => unreachable,
            .dialog => |d| {
                res.dialog = d;
                res.options = null;
            },
            .dialog_dynamic => |d| {
                res.dialog = .{
                    .character = d.character,
                    .text = d.callback(cur.chaval_state),
                };
                res.options = null;
            },
            .exit_chess_game => {
                res.chess_board = null;
                res.options = null;
                res.dialog = null;
            },
            .new_chess_game, .reset_chess_game => |x| {
                res.chess_board = x;
                res.options = null;
                res.dialog = null;
            },
            .chess_move => |move| {
                assert(res.chess_board != null);
                res.options = null;
                res.dialog = null;
                res.chess_board.?.tiles.set(move.to, res.chess_board.?.tiles.at2(move.from));
                res.chess_board.?.tiles.set(move.from, null);
                if (move.extra_from) |f| {
                    const t = move.extra_to.?;
                    res.chess_board.?.tiles.set(t, res.chess_board.?.tiles.at2(f));
                    res.chess_board.?.tiles.set(f, null);
                }
                if (move.extra_death) |d| {
                    res.chess_board.?.tiles.set(d, null);
                }
            },
            .choice => |options| {
                res.options = options;
                res.dialog = null;
            },
            .next_day => {
                res.options = null;
                res.dialog = null;
                res.chess_board = null;
                res.fadeout = true;
            },
        }
        return res;
    }
};

const SceneDelta = union(enum) {
    started_game,
    dialog: SceneState.DialogState,
    new_chess_game: ChessBoardState,
    reset_chess_game: ChessBoardState,
    exit_chess_game,
    chess_move: ChessMove,
    choice: SceneState.Options,
    next_day: []const SceneDelta,

    dialog_dynamic: struct {
        character: Character,
        callback: *const fn (chaval: ChavalState) []const u8,
    },
    branch_dynamic: *const fn (chaval: ChavalState) []const SceneDelta,

    pub const ChessMove = struct {
        from: UVec2,
        to: UVec2,
        extra_death: ?UVec2 = null,
        extra_from: ?UVec2 = null,
        extra_to: ?UVec2 = null,

        pub fn moves(comptime strs: [2][]const u8) ChessMove {
            var move_1: ChessMove = .move(strs[0]);
            if (strs[1][0] == 'x') {
                const str = strs[1][1..];
                assert(str.len == 2);
                const death_col = switch (str[0]) {
                    'a'...'h' => |x| x - 'a',
                    else => unreachable,
                };
                const death_row = switch (str[1]) {
                    '1'...'8' => |x| x - '1',
                    else => unreachable,
                };
                move_1.extra_death = .new(death_col, 7 - death_row);
            } else {
                const move_2: ChessMove = .move(strs[1]);
                move_1.extra_from = move_2.from;
                move_1.extra_to = move_2.to;
            }
            return move_1;
        }

        pub fn move(comptime str: []const u8) ChessMove {
            assert(str.len == "a1,a1".len);
            const source_col = switch (str[0]) {
                'a'...'h' => |x| x - 'a',
                else => unreachable,
            };
            const source_row = switch (str[1]) {
                '1'...'8' => |x| x - '1',
                else => unreachable,
            };
            const target_col = switch (str[3]) {
                'a'...'h' => |x| x - 'a',
                else => unreachable,
            };
            const target_row = switch (str[4]) {
                '1'...'8' => |x| x - '1',
                else => unreachable,
            };
            return .{
                .from = .new(source_col, 7 - source_row),
                .to = .new(target_col, 7 - target_row),
            };
        }
    };

    pub fn turnDuration(self: SceneDelta, prev: SceneState) f32 {
        if (prev.main_menu) return 1.0;
        return switch (self) {
            .new_chess_game => 1.0,
            .next_day => 1.0,
            else => 0.25,
        };
    }

    pub fn move(comptime str: []const u8) SceneDelta {
        return .{ .chess_move = .move(str) };
    }

    pub fn moves(comptime strs: [2][]const u8) SceneDelta {
        return .{ .chess_move = .moves(strs) };
    }

    pub fn say(character: Character, text: []const u8) SceneDelta {
        return .{ .dialog = .{ .character = character, .text = text } };
    }

    pub fn sayDynamic(character: Character, callback: fn (chaval: ChavalState) []const u8) SceneDelta {
        return .{ .dialog_dynamic = .{ .character = character, .callback = callback } };
    }

    pub fn branchDynamic(callback: fn (chaval: ChavalState) []const SceneDelta) SceneDelta {
        return .{ .branch_dynamic = callback };
    }

    pub fn nextDay(day: []const SceneDelta) SceneDelta {
        return .{ .next_day = day };
    }
};

const ChavalState = struct {
    skill: f32,
    frustration: f32,

    pub const Delta = struct {
        skill: f32,
        frustration: f32,
    };

    fn applyDelta(cur: ChavalState, delta: Delta) ChavalState {
        var res = cur;
        res.frustration += delta.frustration;
        res.skill += delta.skill;
        return res;
    }

    pub const init: ChavalState = .{ .skill = 0, .frustration = 0 };
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
    player: Piece.Color,

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

        var res: ChessBoardState = .{ .tiles = .initFillV2(.both(8), null), .player = .white };
        for (raw_pieces) |piece| {
            res.tiles.set(piece.pos, .{
                .color = piece.color,
                .kind = piece.kind,
            });
        }
        break :blk res;
    };

    pub const initial_black: ChessBoardState = blk: {
        var res = initial_white;
        res.player = .black;
        break :blk res;
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
    .sounds = .{
        .changeexpression = "assets/chesstory/sound/sfx_changeexpresion.wav",
        .check = "assets/chesstory/sound/sfx_check.wav",
        .click = "assets/chesstory/sound/sfx_click.wav",
        .dialogue = "assets/chesstory/sound/sfx_dialogue.wav",
        .endgame = "assets/chesstory/sound/sfx_endgame.wav",
        .putpiece = "assets/chesstory/sound/sfx_putpiece.wav",
        .select = "assets/chesstory/sound/sfx_select.wav",
        .startgame = "assets/chesstory/sound/sfx_startgame.wav",
    },
    .loops = .{},
    .preloaded_images = .{
        .consolas_atlas = "assets/fonts/Consolas.png",
        .reference = "assets/chesstory/images/ref.png",

        .logo = "assets/chesstory/images/logo.png",
        .padre = "assets/chesstory/images/dad.png",

        .chaval_neutral_close = "assets/chesstory/images/chaval/neutralclose.png",
        .chaval_neutral_open = "assets/chesstory/images/chaval/neutralopen.png",
        .chaval_sad_close = "assets/chesstory/images/chaval/sadclose.png",
        .chaval_sad_open = "assets/chesstory/images/chaval/sadopen.png",

        // TODO: better
        .textbox = "assets/chesstory/images/9slicebasico.png",
        .casilla_blanca = "assets/chesstory/images/casilla_blanca.png",
        .casilla_negra = "assets/chesstory/images/casilla_negra.png",
        .delante = "assets/chesstory/images/delante.png",
        .fondo = "assets/chesstory/images/fondo.png",
        .pieza_blanca_alfil = "assets/chesstory/images/pieza_blanca_alfil.png",
        .pieza_blanca_caballo = "assets/chesstory/images/pieza_blanca_caballo.png",
        .pieza_blanca_peon = "assets/chesstory/images/pieza_blanca_peon.png",
        .pieza_blanca_reina = "assets/chesstory/images/pieza_blanca_reina.png",
        .pieza_blanca_rey = "assets/chesstory/images/pieza_blanca_rey.png",
        .pieza_blanca_torre = "assets/chesstory/images/pieza_blanca_torre.png",
        .pieza_negra_alfil = "assets/chesstory/images/pieza_negra_alfil.png",
        .pieza_negra_caballo = "assets/chesstory/images/pieza_negra_caballo.png",
        .pieza_negra_peon = "assets/chesstory/images/pieza_negra_peon.png",
        .pieza_negra_reina = "assets/chesstory/images/pieza_negra_reina.png",
        .pieza_negra_rey = "assets/chesstory/images/pieza_negra_rey.png",
        .pieza_negra_torre = "assets/chesstory/images/pieza_negra_torre.png",
    },
};
pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const COLORS = struct {
    bg: FColor = .gray(0.5),
}{};

canvas: Canvas,
mem: Mem,
smooth: kommon.LazyState,
lazy_state: LocalDecisions,
textures: struct {
    padre: Gl.Texture,
    logo: Gl.Texture,

    chaval_neutral_close: Gl.Texture,
    chaval_neutral_open: Gl.Texture,
    chaval_sad_close: Gl.Texture,
    chaval_sad_open: Gl.Texture,

    reference: Gl.Texture,

    // TODO: better
    textbox: Gl.Texture,
    casilla_blanca: Gl.Texture,
    casilla_negra: Gl.Texture,
    delante: Gl.Texture,
    fondo: Gl.Texture,
    pieza_blanca_alfil: Gl.Texture,
    pieza_blanca_caballo: Gl.Texture,
    pieza_blanca_peon: Gl.Texture,
    pieza_blanca_reina: Gl.Texture,
    pieza_blanca_rey: Gl.Texture,
    pieza_blanca_torre: Gl.Texture,
    pieza_negra_alfil: Gl.Texture,
    pieza_negra_caballo: Gl.Texture,
    pieza_negra_peon: Gl.Texture,
    pieza_negra_reina: Gl.Texture,
    pieza_negra_rey: Gl.Texture,
    pieza_negra_torre: Gl.Texture,

    next_blink: f32 = 1,

    fn getChaval(t: *@This(), chaval_state: ChavalState, global_seconds: f32) Gl.Texture {
        const blink_duration = 0.2;
        const blinking = math.inRange(global_seconds, t.next_blink, t.next_blink + blink_duration);
        if (global_seconds > t.next_blink + 2 * blink_duration) {
            t.next_blink = global_seconds + 2;
        }

        if (chaval_state.frustration > 1.5) {
            return if (blinking) t.chaval_sad_close else t.chaval_sad_open;
        } else {
            return if (blinking) t.chaval_neutral_close else t.chaval_neutral_open;
        }
    }

    // TODO: better
    fn chessPiece(t: @This(), v: ChessBoardState.Piece) Gl.Texture {
        return switch (v.color) {
            .black => switch (v.kind) {
                .bishop => t.pieza_negra_alfil,
                .king => t.pieza_negra_rey,
                .horse => t.pieza_negra_caballo,
                .pawn => t.pieza_negra_peon,
                .queen => t.pieza_negra_reina,
                .rook => t.pieza_negra_torre,
            },
            .white => switch (v.kind) {
                .bishop => t.pieza_blanca_alfil,
                .king => t.pieza_blanca_rey,
                .horse => t.pieza_blanca_caballo,
                .pawn => t.pieza_blanca_peon,
                .queen => t.pieza_blanca_reina,
                .rook => t.pieza_blanca_torre,
            },
        };
    }

    fn boardTile(t: @This(), color: ChessBoardState.Piece.Color) Gl.Texture {
        return switch (color) {
            .black => t.casilla_negra,
            .white => t.casilla_blanca,
        };
    }
},

main: Main = .init(day_1),

const day_1: []const SceneDelta = &.{
    .say(.father, "Well, here we are"),
    .say(.father, "Brian, this is your new chess teacher"),
    .say(.kid, "..."),
    .say(.kid, "Hi"),
    .say(.teacher, "Hello, Brian. Pleased to meet you."),
    .say(.teacher, "I've heard good things from your father."),
    .say(.teacher, "Let's see what you know."),
    .say(.father, "Not much... He just got started."),
    .say(.father, "His brother taught him the basics"),
    .say(.father, "and a couple of tricks."),
    .say(.father, "Don't be too tough on him."),
    .{ .new_chess_game = .initial_white },
    .move("e2,e4"),
    .move("g8,f6"),
    .move("e4,e5"),
    .move("f6,e4"),
    .move("f1,c4"),
    .move("g7,g6"),
    // https://www.chess.com/analysis/game/pgn/3LpDa88Dy8/analysis
    .{
        .choice = &.{
            .{
                .label = "try to checkmate",
                .move = .move("d1,f3"),
                .effect = .{ .skill = 1, .frustration = 3 },
                .next = &.{
                    .move("e4,c5"),
                    .move("f3,f7"),
                    // TODO: 'check' sound
                    .say(.kid, "What??"),
                    .say(.teacher, "You must be more attentive."),
                    .say(.teacher, "Let's try again..."),
                    // TODO: fadeout
                    .exit_chess_game,
                    .say(.teacher, "That's all for today."),
                    .say(.teacher, "Keep studying."),
                    .say(.father, "Did you have fun?"),
                    .say(.kid, "I learned a lot, I guess..."),
                    .nextDay(day_2),
                },
            },
            .{
                .label = "take it easy",
                .move = .move("d2,d3"),
                .effect = .{ .skill = 0, .frustration = -1 },
                .next = &.{
                    // https://www.chess.com/analysis/game/pgn/4RVF7zhg7g/analysis
                    .move("e4,c5"),
                    .move("c1,f4"),
                    .move("f8,g7"),
                    .move("g1,f3"),
                    .moves(.{ "e8,g8", "h8,f8" }),
                    .move("d1,d2"),
                    .move("d7,d5"),
                    .moves(.{ "e5,d6", "xd5" }),
                    .{ .choice = &.{ .{
                        .label = "test his skills",
                        .move = .moves(.{ "e1,g1", "h1,f1" }),
                        .effect = .{ .skill = 1, .frustration = -1 },
                        .next = &.{
                            .move("g7,b2"),
                            .say(.teacher, "Very good."),
                            .say(.teacher, "Few novices would have seen the opportunity."),
                            .say(.kid, "Thanks!"),
                            .exit_chess_game,
                            .say(.teacher, "That's all for today"),
                            .say(.father, "Did you have fun?"),
                            .say(.kid, "yep!"),
                            .nextDay(day_2),
                        },
                    }, .{
                        .label = "play normally",
                        .move = .move("b1,c3"),
                        .effect = .{ .skill = 1, .frustration = 0 },
                        .next = &.{
                            .say(.teacher, "etc etc"),
                            .exit_chess_game,
                            .say(.teacher, "That's all for today"),
                            .say(.father, "Did you have fun?"),
                            .say(.kid, "yes"),
                            .nextDay(day_2),
                        },
                    } } },
                },
            },
        },
    },
};

const day_2: []const SceneDelta = &.{
    .say(.teacher, "...so remember, the rooks are important."),
    .say(.kid, "Makes sense. Let's try again!"),
    .{ .new_chess_game = .initial_black },
    // https://www.chess.com/analysis/game/pgn/3AH82B1dJe/analysis
    .move("g1,f3"),
    .move("d7,d5"),
    .move("g2,g3"),
    .move("c8,g4"),
    .move("f1,g2"),
    .move("g4,f3"),
    .move("g2,f3"),
    .move("g8,f6"),
    .moves(.{ "e1,g1", "h1,f1" }),
    .move("e7,e5"),
    .say(.teacher, "Pawns are essential for a strong frontline."),
    .say(.kid, "Must be hard to be a pawn"),
    .say(.kid, "They can't come back, right?"),
    .{ .choice = &.{
        .{
            .label = "You know they can't",
            .move = null,
            .effect = .{ .skill = 0, .frustration = 3 },
            .next = &.{
                .say(.teacher, "Didn't your brother teach you that?"),
                .exit_chess_game,
                .say(.father, "so the kid was distracted today?"),
                .nextDay(day_3),
            },
        },
        .{
            .label = "Well, by getting to the end",
            .move = null,
            .effect = .{ .skill = 0, .frustration = -1 },
            .next = &.{
                .exit_chess_game,
                .say(.father, "so the kid was distracted today?"),
                .nextDay(day_3),
            },
        },
    } },
};

const day_3: []const SceneDelta = &.{
    .say(.teacher, "Kid is late today."),
    .say(.teacher, "asdfasdfasdf"),
    .say(.kid, "hello"),
    .{ .new_chess_game = .initial_white },
    // https://www.chess.com/analysis/game/pgn/4CLXDD4i3Y/analysis
    .move("d2,d4"),
    .move("g8,f6"),
    .move("c1,f4"),
    .move("g7,g6"),
    .move("e2,e3"),
    .move("f8,g7"),
    .say(.teacher, "That opening, again"),
    .say(.teacher, "Not common for a novice"),
    .say(.teacher, "Why do you keep playing it?"),
    .say(.kid, "My brother taught it"),
    .move("g1,f3"),
    .moves(.{ "h8,f8", "e8,g8" }),
    .move("c2,c3"),
    .move("b7,b6"),
    .move("f1,d3"),
    .move("c8,b7"),
    .move("b1,d2"),
    .move("f8,e8"),
    .moves(.{ "h1,f1", "e1,g1" }),
    .move("d7,d6"),
    .move("d1,c2"),
    .move("c7,c5"),
    .move("f4,g5"),
    .move("b8,d7"),
    .move("f1,e1"),
    .move("c5,d4"),
    .move("e3,d4"),
    .move("d8,c7"),
    .move("d2,e4"),
    .move("f6,e4"),
    .move("d3,e4"),
    .move("b7,e4"),
    .move("c2,e4"),
    .move("e7,e5"),
    .move("d4,e5"),
    .move("d6,e5"),
    .{
        .choice = &.{
            .{
                .label = "Defend your structure",
                .move = .move("a1,d1"),
                .effect = .{ .skill = 1, .frustration = 1 },
                .next = &.{
                    .move("d7,c5"),
                    .move("e4,h4"),
                    .move("e5,e4"),
                    .move("f3,d4"),
                    .move("g7,d4"),
                    .move("c3,d4"),
                    .move("c5,d3"),
                    .move("e1,e4"),
                    .move("d3,b2"),
                    .move("g5,f6"),
                    .move("b2,d1"),
                    .move("h4,h6"),
                    .say(.teacher, "Mate in 2"),
                    .say(.teacher, "When your opponent does something weird,"),
                    .say(.teacher, "you should pay attention."),
                    .exit_chess_game,
                    .say(.father, "next day is the tournament"),
                    .nextDay(day_4),
                },
            },
            .{
                .label = "Attack his structure",
                .move = .move("g5,e3"),
                .effect = .{ .skill = 2, .frustration = 2 },
                .next = &.{
                    .say(.kid, "x"),
                    .move("d7,f6"),
                    .move("e4,h4"),
                    .move("f6,d5"),
                    .move("e3,h6"),
                    .move("g7,h6"),
                    .move("h4,h6"),
                    .move("e5,e4"),
                    .move("f3,g5"),
                    .move("d5,f6"),
                    .move("h6,h4"),
                    .move("a8,d8"),
                    .move("g5,e4"),
                    .move("f6,e4"),
                    .move("e1,e4"),
                    .move("e8,e4"),
                    .move("h4,e4"),
                    .move("d8,d2"),
                    .move("a1,b1"),
                    .move("c7,d6"),
                    .move("h2,h4"),
                    .move("d2,d1"),
                    .move("b1,d1"),
                    .move("d6,d1"),
                    .move("g1,h2"),
                    .move("d1,a1"),
                    .move("e4,e8"),
                    .move("g8,g7"),
                    .move("e8,e5"),
                    .move("g7,g8"),
                    .say(.teacher, "this will end in a draw"),
                    .move("e5,b8"),
                    .move("g8,g7"),
                    .move("b8,a7"),
                    .move("a1,b2"),
                    .move("f2,f3"),
                    .move("h7,h5"),
                    .move("a7,b8"),
                    .move("b2,c3"),
                    .move("b8,b6"),
                    .move("c3,e5"),
                    .move("h2,h3"),
                    .move("e5,f5"),
                    .move("h3,h2"),
                    .move("f5,e5"),
                    .move("h2,h3"),
                    .move("e5,f5"),
                    .move("h3,h2"),
                    .move("f5,e5"),
                    .say(.teacher, "I guess it's a draw, due to repetition."),
                    .exit_chess_game,
                    .say(.father, "next day is the tournament"),
                    .nextDay(day_4),
                },
            },
        },
    },
};

const day_4: []const SceneDelta = &.{
    .say(.teacher, "So, how did the tournament go?"),
    .branchDynamic(struct {
        pub fn anon(chaval: ChavalState) []const SceneDelta {
            if (chaval.skill < 0) {
                return &.{
                    .say(.kid, "i got last."),
                };
            } else if (chaval.skill > 2) {
                return &.{
                    .say(.kid, "i won!"),
                };
            } else {
                return &.{
                    .say(.kid, "not bad i guess"),
                };
            }
        }
    }.anon),
    // .sayDynamic(.kid, struct {
    //     pub fn anon(chaval: ChavalState) []const u8 {
    //         if (chaval.skill < 0) {
    //             return "i got last.";
    //         } else if (chaval.skill > 2) {
    //             return "i won!";
    //         } else {
    //             return "not bad i guess";
    //         }
    //     }
    // }.anon),
};

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("assets/fonts/Consolas.json")}, &.{loaded_images.get(.consolas_atlas)});
    dst.smooth = .init(dst.mem.forever.allocator());
    dst.lazy_state = .init(gpa);

    inline for (std.meta.fields(@FieldType(GameState, "textures"))) |field| {
        if (field.type == f32) continue;
        @field(dst.textures, field.name) = gl.buildTexture2D(
            loaded_images.get(std.enums.nameCast(Images, field.name)),
            false,
        );
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
    self.smooth.last_delta_seconds = platform.delta_seconds;
    self.lazy_state.frameStart();

    // TODO
    const camera = (Rect.from(.{
        .{ .top_left = .zero },
        .{ .size = Vec2.new(16.0 / 9.0, 1).scale(12.5) },
    })).withAspectRatio(
        platform.aspect_ratio,
        .grow,
        .top_center,
    );
    const mouse = platform.getMouse(camera);
    const canvas = &self.canvas;
    var fg_text = canvas.textBatch(0);
    const text_color: FColor = .fromHex("#9D999E");
    const text_size: f32 = 0.7;
    const option_text_color: FColor = .fromHex("#3d3d3d");
    var pieces: std.ArrayList(struct {
        center: Vec2,
        value: GameState.ChessBoardState.Piece,
    }) = .init(self.mem.frame.allocator());
    var board_tiles: ?kommon.grid2D.Grid2D(struct {
        center: Vec2,
        color: ChessBoardState.Piece.Color,
        highlighted: bool,
    }, .both(8)) = null;
    platform.gl.clear(COLORS.bg);
    canvas.drawSpriteBatch(.{
        .top_left = .zero,
        .size = .new(platform.aspect_ratio, 1),
    }, &.{
        .{
            .point = .{},
            .texcoord = .unit,
        },
    }, self.textures.fondo);

    defer if (false) canvas.drawTexturedRectBatch(.unit, &.{
        .{
            .rect = .unit,
            .texcoord = .unit,
            .tint = FColor.white.withAlpha(0.2),
        },
    }, self.textures.reference);

    var advance: bool = false;
    var option_index: ?usize = null;

    const face_center: Point = .{ .pos = camera.worldFromCenterLocal(.new(0.45, -0.075)), .scale = 9 };

    const scene_state = self.main.scene_state;
    assert(scene_state.dialog == null or scene_state.options == null);

    if (scene_state.chess_board) |chess_board| {
        board_tiles = .initUndefinedV2(.both(8));

        var chess_rect: Rect = .from2(
            .{ .center = .new(6.3, 5.45) },
            .{ .size = .new(8, 8.4) },
        );

        // TODO: mate sound

        const board = chess_board.tiles;

        switch (self.main.last_delta) {
            .started_game => unreachable,
            .exit_chess_game => {
                chess_rect = chess_rect.move(.new(0, math.lerp(0, camera.size.y, math.smoothstepEased(self.main.anim_t, 0, 0.7, .linear))));
                if (try self.lazy_state.doOnceUntilRest(.fromString("exit chess game"))) {
                    platform.sound_queue.insert(.startgame);
                }
            },
            .new_chess_game => {
                chess_rect = chess_rect.move(.new(0, math.lerp(camera.size.y, 0, math.smoothstepEased(self.main.anim_t, 0, 0.7, .linear))));
                if (try self.lazy_state.doOnceUntilRest(.fromString("start chess game"))) {
                    platform.sound_queue.insert(.startgame);
                }
            },
            .reset_chess_game => {
                if (try self.lazy_state.doOnceUntilRest(.fromString("reset chess game"))) {
                    platform.sound_queue.insert(.startgame);
                }
            },
            .chess_move => |m| {
                if (try self.lazy_state.doOnceUntilRest(.fromFormat("move {any}", .{m}))) {
                    platform.sound_queue.insert(.putpiece);
                }
            },
            .choice, .next_day, .dialog, .dialog_dynamic => {},
            .branch_dynamic => unreachable,
        }

        var it = board.iterator();
        while (it.next()) |p| {
            const r = board.getTileRect(chess_rect, p);
            board_tiles.?.set(p, .{
                .center = r.get(.center),
                .color = if (p.isEven()) .white else .black,
                .highlighted = false,
            });
        }
        it.reset();
        while (it.next()) |p| {
            if (board.at2(p)) |piece| {
                var piece_center: Vec2 = board.getTileRect(chess_rect, p).get(.center);
                if (std.meta.activeTag(self.main.last_delta) == .chess_move) {
                    if (self.main.last_delta.chess_move.to.equals(p)) {
                        if (self.main.prev_scene_state.chess_board.?.tiles.at2(p)) |eaten_piece| {
                            if (self.main.anim_t < 0.5) {
                                try pieces.append(.{
                                    .center = piece_center,
                                    .value = eaten_piece,
                                });
                            }
                        }
                        piece_center = .lerp(
                            board.getTileRect(chess_rect, self.main.last_delta.chess_move.from).get(.center),
                            board.getTileRect(chess_rect, self.main.last_delta.chess_move.to).get(.center),
                            self.main.anim_t,
                        );
                    }
                    if (self.main.last_delta.chess_move.extra_to) |extra_to| {
                        if (extra_to.equals(p)) {
                            piece_center = .lerp(
                                board.getTileRect(chess_rect, self.main.last_delta.chess_move.extra_from.?).get(.center),
                                board.getTileRect(chess_rect, self.main.last_delta.chess_move.extra_to.?).get(.center),
                                self.main.anim_t,
                            );
                        }
                    }
                }
                if (chess_board.player == .black) {
                    piece_center = piece_center.rotateAround(chess_rect.getCenter(), 0.5);
                }
                try pieces.append(.{
                    .center = piece_center,
                    .value = piece,
                });
            }
        }
        // if (std.meta.activeTag(self.main.last_delta) == .chess_move) {
        //     // const move = self.main.last_delta.chess_move;
        //     // if (move.extra_death) |d| {}
        // }
    }

    // face
    if (scene_state.chess_board != null or self.main.prev_scene_state.chess_board != null) {
        canvas.drawSpriteBatch(camera, &.{.{
            .point = face_center,
            .pivot = .center,
            .texcoord = .unit,
        }}, self.textures.getChaval(self.main.scene_state.chaval_state, platform.global_seconds));
    } else if (scene_state.dialog) |d| {
        if (switch (d.character) {
            .father => self.textures.padre,
            .kid => self.textures.getChaval(scene_state.chaval_state, platform.global_seconds),
            .teacher => null,
        }) |t| {
            canvas.drawSpriteBatch(camera, &.{.{
                .point = face_center,
                .pivot = .center,
                .texcoord = .unit,
            }}, t);
        }
    }

    canvas.drawSpriteBatch(.unit, &.{.{
        .point = .{},
        .texcoord = .unit,
    }}, self.textures.delante);

    if (true) {
        const gradient_opacity = try self.smooth.floatCustomSpeed(
            .fromString("grad"),
            if (scene_state.dialog != null or scene_state.options != null or scene_state.fadeout) 0.5 else 0.0,
            0.1,
        );
        const y = 0.15;
        const w = 0.2;
        canvas.fillRect(
            .unit,
            Rect.unit.with2(.size, .new(1, y), .bottom_center),
            FColor.black.withAlpha(gradient_opacity),
        );
        canvas.rectGradient(
            .unit,
            Rect.unit
                .with2(.size, .new(1, w), .bottom_center)
                .move(.new(0, -y)),
            FColor.black.withAlpha(gradient_opacity),
            FColor.black.withAlpha(0),
        );
    }

    if (scene_state.dialog) |d| {
        assert(scene_state.options == null);
        try fg_text.addText(
            d.character.name(),
            .centeredAt(camera.worldFromLocal(.new(0.15, 0.825))),
            text_size * 1.0,
            text_color,
        );
        try fg_text.addText(
            d.text,
            .{ .hor = .center, .ver = .baseline, .pos = camera.get(.bottom_center).addY(-1) },
            text_size,
            text_color,
        );
    }

    if (scene_state.options) |options| {
        assert(options.len == 2);
        assert(scene_state.dialog == null);
        for (options, 0.., &[2]f32{ -0.45, 0.45 }) |option, k, x| {
            const base_r: Rect = .fromCenterAndSize(
                camera.worldFromCenterLocal(.new(x, 0.775)),
                .new(1.0 + tof32(option.label.len) * 0.4, 1.5),
                // .new(8, 1.5),
            );
            const hovered = base_r.contains(mouse.cur.position);
            const pressed = hovered and mouse.cur.isDown(.left);
            const clicked = hovered and mouse.wasReleased(.left);

            const r = base_r.move(.new(0, try self.smooth.float(
                .fromFormat("option {d}", .{k}),
                if (pressed) 0.1 else if (hovered) -0.1 else 0,
            )));

            canvas.drawTexturedRectBatch(camera, &Canvas.sliced3x3(r, 0.4), self.textures.casilla_blanca);
            try fg_text.addText(
                option.label,
                .centeredAt(r.getCenter().addY(-0.05)),
                text_size,
                option_text_color,
            );
            if (hovered) {
                if (option.move) |move| {
                    assert(scene_state.chess_board != null);
                    board_tiles.?.getPtr(move.from).highlighted = true;
                    if (move.extra_from) |f| {
                        board_tiles.?.getPtr(f).highlighted = true;
                    } else {
                        board_tiles.?.getPtr(move.to).highlighted = true;
                    }
                }
                if (try self.lazy_state.doOnceUntilRest(.fromFormat("hovered {d}", .{k}))) {
                    platform.sound_queue.insert(.select);
                }
            }
            if (clicked) {
                advance = true;
                option_index = k;
                platform.sound_queue.insert(.click);
            }
        }
    } else {
        // TODO: should auto be done
        for (0..2) |k| {
            _ = try self.smooth.float(.fromFormat("option {d}", .{k}), 0);
        }
    }

    if (scene_state.main_menu or self.main.prev_scene_state.main_menu) {
        canvas.drawSpriteBatch(camera, &.{
            .{
                .point = .{ .pos = camera.getCenter().addY(if (scene_state.main_menu)
                    0
                else
                    -camera.size.y * math.easings.easeInQuad(self.main.anim_t)), .scale = 7 },
                .pivot = .center,
                .texcoord = .unit,
                .tint = FColor.white.withAlpha(if (scene_state.main_menu) 1.0 else 1.0 - self.main.anim_t),
            },
        }, self.textures.logo);
        // try fg_text.addText(
        //     "GAME TITLE",
        //     .centeredAt(camera.getCenter()),
        //     2,
        //     FColor.black.withAlpha(if (scene_state.main_menu) 1.0 else 1.0 - self.main.anim_t),
        // );
    }

    math.towards(&self.main.anim_t, 1, platform.delta_seconds / self.main.last_delta.turnDuration(self.main.prev_scene_state));
    if (mouse.wasPressed(.left) and scene_state.options == null) {
        advance = true;
    }
    if (self.main.anim_t == 1) {
        if (scene_state.dialog == null and
            scene_state.options == null and
            scene_state.main_menu == false)
        {
            advance = true;
        }
    }

    if (board_tiles) |tiles| {
        var it = tiles.iterator();
        while (it.next()) |p| {
            const tile = tiles.at2(p);
            if (tile.highlighted) {
                canvas.drawSpriteBatch(camera, &.{
                    .{
                        .point = .{ .pos = tile.center, .scale = 1.1 },
                        .pivot = .center,
                        .texcoord = .unit,
                        .tint = .black,
                    },
                }, self.textures.boardTile(tile.color));
            }
            canvas.drawSpriteBatch(camera, &.{
                .{
                    .point = .{ .pos = tile.center },
                    .pivot = .center,
                    .texcoord = .unit,
                },
            }, self.textures.boardTile(tile.color));
        }
        for (pieces.items) |piece| {
            canvas.drawSpriteBatch(camera, &.{
                .{
                    .point = .{ .pos = piece.center.addY(-0.07).addX(-0.02), .scale = 0.85 },
                    .pivot = .center,
                    .texcoord = .unit,
                },
            }, self.textures.chessPiece(piece.value));
        }
    } else {
        assert(pieces.items.len == 0);
    }

    fg_text.draw(camera);

    if (scene_state.fadeout) {
        canvas.fillRect(.unit, .unit, FColor.black.withAlpha(math.smoothstepEased(@abs(self.main.anim_t - 0.5), 0.5, 0.4, .linear)));
    }

    if (advance) {
        if (self.main.next_changes.len > 0 or option_index != null) {
            self.main.advance(option_index);
        } else return true;
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

// TODO: rename
pub const LocalDecisions = struct {
    keys_seen_last_frame: std.AutoHashMap(Key, void),
    keys_seen_this_frame: std.AutoHashMap(Key, void),

    // TODO: debug helper
    // frame_started: if (@import("builtin").mode == .Debug) bool else void,

    pub fn init(gpa: std.mem.Allocator) LocalDecisions {
        return .{
            .keys_seen_last_frame = .init(gpa),
            .keys_seen_this_frame = .init(gpa),
        };
    }

    pub fn frameStart(self: *LocalDecisions) void {
        const old = self.keys_seen_last_frame;
        self.keys_seen_last_frame = self.keys_seen_this_frame;
        self.keys_seen_this_frame = old;
        self.keys_seen_this_frame.clearRetainingCapacity();
    }

    pub fn doOnceUntilRest(self: *LocalDecisions, key: Key) !bool {
        try self.keys_seen_this_frame.putNoClobber(key, {});
        return self.keys_seen_last_frame.get(key) == null;
    }
};
