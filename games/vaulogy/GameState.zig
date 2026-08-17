pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// Causes of bugs:
// - functions that take a pointer and allocate memory might invalidate that pointer

// TODO(game): gradual computation of solutions, avoid freezing

// TODO(bug): max stack size in toOldCoreValue :((((

// TODO(polish): undo for text changes

// TODO(game): in *_viewer, highlight the corresponding 'other' element!

const Drawer = @import("Drawer.zig");

pub const tracy = @import("tracy");

// TODO(optim): launching a fnkbox execution increases the number of existing legos
// TODO(game): see diff between sexprs

const ENABLE_REUSE = true;
const SAVING_ENABLED = true;
const EXECUTOR_MOVES_LEFT = true;
const SEQUENTIAL_GOES_DOWN = true;
const CRANKS_ENABLED = true;
const OVERWRITING_TOPLEVEL_SEXPRS_ENABLED = false;
const INCLUDE_DEBUG_FIELDS = @import("builtin").mode == .Debug and !@import("builtin").target.cpu.arch.isWasm();

const Level = @import("levels_new.zig").Level;
const levels = @import("levels_new.zig").levels;
// const levels = if (@import("builtin").mode == .Debug) @import("levels_new.zig").levels[0..3] else @import("levels_new.zig").levels;

pub fn levelIndex(comptime name: []const u8) usize {
    inline for (levels, 0..) |level, k| {
        if (comptime std.mem.eql(u8, name, level.fnk_name)) return k;
    } else @compileError("couldn't find name: " ++ name);
}

pub const FuzzerContext = struct {
    var toybox_instance: Toybox = undefined;

    const TestPlatform = struct {
        global_seconds: f32 = 0,
        delta_seconds: f32 = 0,
        mouse: Mouse = .{ .cur = .init, .prev = .init, .cur_time = 0 },
        keyboard: Keyboard = .{ .cur = .init, .prev = .init, .cur_time = 0 },
        frame_arena: std.heap.ArenaAllocator,
        gpa: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) TestPlatform {
            return .{
                .frame_arena = .init(allocator),
                .gpa = allocator,
            };
        }

        pub fn deinit(test_platform: *TestPlatform) void {
            test_platform.frame_arena.deinit();
        }

        pub fn after(self: *TestPlatform) void {
            self.mouse.prev = self.mouse.cur;
            self.mouse.cur.scrolled = .none;
            self.keyboard.prev = self.keyboard.cur;
            _ = self.frame_arena.reset(.retain_capacity);
        }

        pub fn getGives(self: *TestPlatform, delta_seconds: f32) PlatformGives {
            self.keyboard.cur_time = self.global_seconds;
            self.mouse.cur_time = self.global_seconds;
            self.global_seconds += delta_seconds;

            return .{
                .mouse = self.mouse,
                .keyboard = self.keyboard,
                .gpa = self.gpa,
                .frame_arena = self.frame_arena.allocator(),

                .aspect_ratio = stuff.metadata.desired_aspect_ratio,
                .delta_seconds = delta_seconds,
                .global_seconds = self.global_seconds,
                .gl = .stub,
                .setCursor = struct {
                    fn anon(_: Mouse.Cursor) void {}
                }.anon,
                .recording_log = null,

                .startTextInput = struct {
                    fn anon(_: ?Rect) void {}
                }.anon,
                .stopTextInput = struct {
                    fn anon() void {}
                }.anon,
                .consumeTextInput = struct {
                    fn anon() ?std.BoundedArray(u8, 4) {
                        return null;
                    }
                }.anon,

                .getClipboardText = undefined,
                .setClipboardText = undefined,
                .downloadActiveFramebuffer = undefined,
                .setItem = undefined,
                .getItem = undefined,
                .askUserForFile = undefined,
                .setKeyChanged = undefined,
                .setButtonChanged = undefined,
                .sound_queue = undefined,
                .loop_volumes = undefined,
                .sample_rate = undefined,
                .enqueueSamples = undefined,
                .queuedSeconds = undefined,
                .downloadAsFile = undefined,
                .userUploadedFile = undefined,
                .forgetUserUploadedFile = undefined,
            };
        }
    };

    pub const FakeInput = struct {
        z_down: bool,
        mouse_left_down: bool,
        mouse_right_down: bool,
        mouse_pos: Vec2,
        delta_seconds: f32 = 1.0 / 60.0,

        pub const Extern = extern struct {
            z_down: bool,
            mouse_left_down: bool,
            mouse_right_down: bool,
            mouse_pos: Vec2,

            pub fn asFull(this: @This()) FakeInput {
                return .{
                    .z_down = this.z_down,
                    .mouse_left_down = this.mouse_left_down,
                    .mouse_right_down = this.mouse_right_down,
                    .mouse_pos = this.mouse_pos,
                };
            }
        };
    };

    const Player = struct {
        workspace: Workspace,
        test_platform: TestPlatform,

        pub fn init(allocator: std.mem.Allocator, random_seed: u64) !Player {
            toybox = &FuzzerContext.toybox_instance;
            try toybox.init(allocator);
            var workspace: Workspace = undefined;
            try workspace.init(allocator, random_seed);
            return .{ .workspace = workspace, .test_platform = .init(allocator) };
        }

        pub fn deinit(player: *Player) void {
            player.workspace.deinit();
            player.test_platform.deinit();
            toybox.deinit();
        }

        pub fn advance(player: *Player, input: FakeInput) !void {
            player.test_platform.keyboard.cur.keys.KeyZ = input.z_down;
            player.test_platform.mouse.cur.buttons.left = input.mouse_left_down;
            player.test_platform.mouse.cur.buttons.right = input.mouse_right_down;
            player.test_platform.mouse.cur.position = input.mouse_pos;
            try player.workspace.update(player.test_platform.getGives(input.delta_seconds), null, player.test_platform.frame_arena.allocator());
            player.test_platform.after();
        }
    };

    fn testOne(_: @This(), input: []const u8) anyerror!void {
        var player: Player = try .init(std.testing.allocator, std.testing.random_seed);
        defer player.deinit();

        var it = std.mem.window(u8, input, @sizeOf(FakeInput.Extern), @sizeOf(FakeInput.Extern));
        while (it.next()) |cur_input_raw| {
            if (cur_input_raw.len == @sizeOf(FakeInput.Extern)) {
                const cur_input = std.mem.bytesToValue(FakeInput.Extern, cur_input_raw);
                try player.advance(cur_input.asFull());
            }
        }
    }
};

test "fuzz example" {
    try std.testing.fuzz(FuzzerContext{}, FuzzerContext.testOne, .{});
}

test "custom replay" {
    var player: FuzzerContext.Player = try .init(std.testing.allocator, std.testing.random_seed);
    defer player.deinit();

    // const inputs = @import("buggy_recording.zig").inputs;
    // for (inputs) |input| try player.advance(input);
    try player.advance(.{
        .z_down = false,
        .mouse_left_down = false,
        .mouse_pos = .zero,
        .mouse_right_down = false,
    });
}

test "No leaks on Workspace and Drawer" {
    var toybox_instance: Toybox = undefined;
    toybox = &toybox_instance;
    try toybox.init(std.testing.allocator);
    defer toybox.deinit();
    var workspace: Workspace = undefined;
    try workspace.init(std.testing.allocator, std.testing.random_seed);
    defer workspace.deinit();
    var usual: kommon.Usual = undefined;
    usual.init(
        std.testing.allocator,
        @intCast(std.testing.random_seed),
        try Canvas.init(Gl.stub, std.testing.allocator, &.{}, &.{}),
    );
    defer usual.deinit(undefined);
    const drawer: Drawer = try .init(&usual, undefined);
    _ = drawer;
}

test "solutions" {
    const gpa = std.testing.allocator;
    var mem: core.VeryPermamentGameStuff = .init(gpa);
    defer mem.deinit();

    var scratch: std.heap.ArenaAllocator = .init(gpa);
    defer scratch.deinit();
    var pool: std.heap.MemoryPool(core.Sexpr) = .init(gpa);
    defer pool.deinit();

    var scoring: core.ScoringRun = try .init(@embedFile("./solutions.txt"), &mem);
    defer scoring.deinit(true);

    for (levels) |level| {
        // TODO(design): test all levels
        if (std.mem.startsWith(u8, level.fnk_name, "meta_")) continue;
        if (std.mem.eql(u8, level.fnk_name, "interpreter")) continue;

        defer _ = scratch.reset(.retain_capacity);
        var samples_it = level.samplesIterator();
        while (try samples_it.next(&pool, scratch.allocator(), scratch.allocator())) |item| {
            defer _ = pool.reset(.retain_capacity);

            var exec: core.ExecutionThread = try .init(item.input, &.doLit(level.fnk_name), &scoring, .new_very_long);
            defer exec.deinit();
            const actual = exec.getFinalResultBoundedV2(&scoring, .new_very_long) catch |err| switch (err) {
                else => {
                    if (std.testing.backend_can_print) {
                        std.debug.print(
                            "error {s} on fnk {s} with input {any}, expected {any}\n",
                            .{ @errorName(err), level.fnk_name, item.input, item.expected },
                        );
                    }
                    return err;
                },
            };
            if (!actual.equals(item.expected)) {
                if (std.testing.backend_can_print) {
                    std.debug.print(
                        "failed on fnk {s} with input {any}: expected {any}, got {any}\n",
                        .{ level.fnk_name, item.input, item.expected, actual },
                    );
                }
                try std.testing.expect(false);
            }
        }
    }
}

// TODO(platform): type
pub const stuff = .{
    .metadata = .{
        .name = "vaulogy",
        .author = "knexator",
        .desired_aspect_ratio = 16.0 / 9.0,
    },
    .sounds = .{},
    .loops = .{},
    .preloaded_images = .{
        // TODO(platform): don't require this here
        .arial_atlas = "fonts/Arial.png",
        .atom_testing = "assets/images/atom_testing.png",
    },
};
pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

var COLORS: struct {
    bg: FColor = .gray(0.5),
} = .{};

usual: kommon.Usual,
toybox_instance: Toybox,
drawer: Drawer,
workspace: Workspace,
menu: Menu = .{},

// used for hot reloading
backup_point: ?Point = null,

var toybox: *Toybox = undefined;

const nothing = Lego.Index.nothing;
/// Might be an Area, a Sexpr, a Case, etc
pub const Lego = struct {
    // TODO(optim-late): remove in release modes
    exists: bool = false,
    index: Index,
    free_next: Index = .nothing,
    /// respect to parent
    local_point: Point,
    /// computed each frame
    absolute_point: Point = undefined,
    /// local coordinates
    visual_offset: Point = .{},
    hot_t: f32 = 0,
    // 1 if there is an element being dropped on this one
    dropzone_t: f32 = 0,
    // 1 if being grabbed
    active_t: f32 = 0,
    /// 1 if this element is being dropped into another
    dropping_t: f32 = 0,
    /// for now, only used for sexprs/cases/garlands
    /// means that it can be duplicated, but not modified
    immutable: bool = false,

    tree: Tree = .empty,

    specific: Specific,

    created_at: CreationTag,

    pub const CreationTag = if (INCLUDE_DEBUG_FIELDS) struct {
        sources: std.BoundedArray(std.builtin.SourceLocation, 64),

        pub fn new(src: std.builtin.SourceLocation) CreationTag {
            var result: CreationTag = .{ .sources = .{} };
            result.sources.appendAssumeCapacity(src);
            return result;
        }

        pub fn plus(tag: CreationTag, src: std.builtin.SourceLocation) CreationTag {
            var result = tag;
            result.sources.append(src) catch {};
            return result;
        }

        pub fn format(
            self: CreationTag,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            comptime assert(fmt.len == 0);
            assert(std.meta.eql(options, .{}));
            for (self.sources.constSlice()) |src| {
                try writer.print("line {d}, ", .{src.line});
            }
        }
    } else struct {
        pub fn new(src: std.builtin.SourceLocation) CreationTag {
            _ = src;
            return .{};
        }

        pub fn plus(tag: CreationTag, src: std.builtin.SourceLocation) CreationTag {
            _ = tag;
            _ = src;
            return .{};
        }
    };

    pub const Specific = union(enum) {
        button: Button,
        scrollbar: Scrollbar,
        area: Area,
        sexpr: Sexpr,
        case: Case,
        garland: Garland,
        fnkname_holder: FnknameHolder,
        /// cable between cases, and the handle to create new ones
        newcase: NewCase,
        executor: Executor,
        pill: Pill,
        fnkbox: Fnkbox,
        fnkbox_box: FnkboxBox,
        testcase: Testcase,
        microscope: Microscope,
        lens: Lens,
        postit: Postit,
        list_viewer: ListViewer,
        meta_viewer: MetaViewer,
        bubble: Bubble,
        bubble_connection: BubbleConnection,
        scorer: Scorer,
        scorer_row: ScorerRow,
        // TODO(design): simplify this
        scorer_rows: void,

        // TODO(design): could this be something else?
        scrollable_list_inbetween: struct {
            kind: enum { listviewer_sexprs },
        },
        scrollable_list: ScrollableList,

        // TODO(design): try to simplify these
        garland_newcases: void,
        editable_textline: EditableTextline,
        fnkslist_element: FnkslistElement,
        postit_text: struct {
            text: []const u8,
            kind: enum { left, center } = .center,
        },
        postit_drawing: enum {
            arrow,
            long_arrow,
            launch_testcase_button,
            piece_center,
        },
        executor_controls: struct {
            pub const Children = struct {
                stop: Lego.Index,
                brake: Lego.Index,
                crank: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                const asdf = Toybox.getChildrenExact(3, index);
                return .{
                    .stop = asdf[0],
                    .brake = asdf[1],
                    .crank = asdf[2],
                };
            }
        },
        executor_brake: struct {
            /// in 0..1; 1 is braked, 0.5 is normal speed, 0 is speedup
            brake_t: f32 = 0.5,
            handle_pos: Vec2 = undefined,

            pub fn brakeBody(brake: @This(), line_t: f32) Vec2 {
                return Specific.Executor.Controls.brakeLineRaw(.{}, brake.brake_t, line_t);
            }

            pub fn brakeHandlePath(_: @This(), brake_t: f32) Vec2 {
                return Specific.Executor.Controls.brakeLineRaw(.{}, brake_t, 1.0);
                // return crank_center
                //     .applyToLocalPosition(.fromPolar(1.5, math.remapFrom01(t, 0.125, 0.375)))
                //     .rotateAround(crank_center.applyToLocalPosition(.new(0.4, 0.25)), 0.1)
                //     .addY(0.25);
            }
        },
        executor_crank: struct {
            value: f32 = 0,
            enabled: bool = false,
            handle_pos: Vec2 = undefined,
        },

        pub const Tag = std.meta.Tag(Specific);

        pub fn tag(specific: *const Specific) Tag {
            return std.meta.activeTag(specific.*);
        }

        pub fn Tagged(comptime specific_tag: Tag) type {
            inline for (@typeInfo(Specific).@"union".fields) |field| {
                if (std.mem.eql(u8, field.name, @tagName(specific_tag))) return field.type;
            } else comptime unreachable;
        }

        pub fn as(specific: *Specific, comptime specific_tag: Tag) ?*Tagged(specific_tag) {
            return switch (specific.*) {
                specific_tag => |*x| x,
                else => null,
            };
        }

        pub const Bubble = struct {
            locked: bool = true,
            fulfilled: bool = false,
            goal: FulfillCondition,
            prev_bubble: Lego.Index,
            blueprint: Lego.Index,
            hint_for: [2]Lego.Index = @splat(.nothing),
            requested_hints: bool = false,
            has_hints: bool = false,
            remaining_reset_anim_t: f32 = 0,

            pub const FulfillCondition = union(enum) {
                all_scorers_solved,
                has_sexpr: Lego.Index,
            };

            pub const Children = struct {
                instanced: Lego.Index,
                reset_bubble_button: Lego.Index,
                unlock_hint_button: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .bubble);
                const asdf = Toybox.getChildrenExact(3, index);
                assert(asdf[0].hasTag(.area));
                return .{
                    .instanced = asdf[0],
                    .reset_bubble_button = asdf[1],
                    .unlock_hint_button = asdf[2],
                };
            }

            pub fn visibleUnlockHints(self: *const Bubble) bool {
                return self.has_hints and !self.requested_hints;
            }
        };

        pub const BubbleConnection = struct {
            source: Lego.Index,
            target: Lego.Index,
        };

        pub const Scorer = struct {
            score: ?Score = null,
            score_computed_at: struct {
                all_fnks_hash: u32,
                all_fnkname_rows_hash: u32,
            } = .{ .all_fnks_hash = 0, .all_fnkname_rows_hash = 0 },

            pub const Score = struct {
                total_time: usize,
                max_stack: usize,
                code_size: usize,
                compile_time: usize,
            };

            pub const Children = struct {
                scorer_rows: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .scorer);
                const asdf = Toybox.getChildrenExact(1, index);
                return .{
                    .scorer_rows = asdf[0],
                };
            }

            pub fn updateStatus(workspace: *Workspace, scorer_index: Lego.Index, all_fnks: core.FnkCollection, all_fnks_hash: u32, scratch: std.mem.Allocator) !void {
                const zone = tracy.initZone(@src(), .{ .name = "update status for scorer" });
                defer zone.deinit();

                assert(scorer_index.hasTag(.scorer));
                const all_scorer_fnknames_hash: u32 = blk: {
                    var hasher = std.hash.Wyhash.init(0);
                    const scorer_rows = scorer_index.children(.scorer).scorer_rows;
                    var cur = scorer_rows.get().tree.first;
                    while (cur != nothing) : (cur = cur.get().tree.next) {
                        assert(cur.hasTag(.scorer_row));
                        hasher.update(std.mem.asBytes(&Sexpr.hash(cur.children(.scorer_row).fnkname)));
                    }
                    break :blk @truncate(hasher.final());
                };
                if (scorer_index.get().specific.scorer.score_computed_at.all_fnks_hash == all_fnks_hash and
                    scorer_index.get().specific.scorer.score_computed_at.all_fnkname_rows_hash == all_scorer_fnknames_hash) return;

                var mem: core.VeryPermamentGameStuff = .init(scratch);
                var scoring_run: core.ScoringRun = try .initFromFnks(all_fnks, &mem);
                var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);

                const rows = try Toybox.getChildrenUnknown(scratch, children(scorer_index).scorer_rows);
                var score: struct {
                    time: usize,
                    max_stack: usize,
                } = .{ .time = 0, .max_stack = 0 };
                var failed_any: bool = false;
                var depended_on_fnkbox: bool = false;
                for (rows) |row| {
                    const fnkname_index = row.children(.scorer_row).fnkname;

                    const maybe_fnkbox = try workspace.fnkboxWithName(fnkname_index, scratch);
                    if (maybe_fnkbox) |fnkbox_index| {
                        if (fnkbox_index.get().specific.fnkbox.status != .solved) {
                            failed_any = true;
                            depended_on_fnkbox = true;
                            break;
                        }
                    }

                    const fnkname = try fnkname_index.get().specific.sexpr.toOldCoreValue(scratch);

                    const level_index = row.get().specific.scorer_row.level_index;
                    const level = levels[level_index];
                    var samples_it = level.samplesIterator();
                    while (try samples_it.next(&pool, scratch, scratch)) |sample| {
                        defer _ = pool.reset(.retain_capacity);

                        var exec = core.ExecutionThread.init(sample.input, fnkname, &scoring_run, .new) catch |err| switch (err) {
                            // error.NoMatchingCase, error.InvalidMetaFnk, error.UsedUndefinedVariable, error.FnkNotFound, error.TookTooLong => {
                            //     failed_any = true;
                            //     break;
                            // },
                            error.NoMatchingCase,
                            error.InvalidMetaFnk,
                            error.UsedUndefinedVariable,
                            error.FnkNotFound,
                            error.TookTooLong,
                            error.BAD_INPUT,
                            => panic("unreachable? {s}", .{@errorName(err)}),
                            error.OutOfMemory => |x| return x,
                        };
                        defer exec.deinit();

                        const actual_output = exec.getFinalResultBoundedV2(&scoring_run, .new) catch |err| switch (err) {
                            error.TookTooLong => {
                                failed_any = true;
                                break;
                            },
                            // error.NoMatchingCase, error.InvalidMetaFnk, error.UsedUndefinedVariable, error.FnkNotFound, error.TookTooLong => {
                            //     failed_any = true;
                            //     break;
                            // },
                            error.NoMatchingCase,
                            error.InvalidMetaFnk,
                            error.UsedUndefinedVariable,
                            error.FnkNotFound,
                            error.BAD_INPUT,
                            => return err,
                            // => panic("unreachable? {s}", .{@errorName(err)}),
                            error.OutOfMemory => |x| return x,
                        };

                        if (!actual_output.equals(sample.expected)) {
                            failed_any = true;
                            break;
                        } else {
                            score.time += exec.score.successful_matches;
                            score.max_stack = @max(score.max_stack, exec.score.max_stack);
                        }
                    }

                    if (try workspace.fnkboxWithName(fnkname_index, scratch)) |fnkbox_index| {
                        if (fnkbox_index.get().specific.fnkbox.status != .solved) {
                            failed_any = true;
                            depended_on_fnkbox = true;
                            break;
                        }
                    }

                    if (maybe_fnkbox) |fnkbox| {
                        if (!failed_any) fnkbox.get().specific.fnkbox.require_manual_execution = false;
                    }

                    if (failed_any) break;
                }
                scorer_index.get().specific.scorer.score = if (failed_any)
                    null
                else
                    .{
                        .total_time = score.time,
                        .max_stack = score.max_stack,
                        .code_size = scoring_run.score.code_size,
                        .compile_time = scoring_run.score.compile_time,
                    };
                if (!depended_on_fnkbox) scorer_index.get().specific.scorer.score_computed_at = .{
                    .all_fnks_hash = all_fnks_hash,
                    .all_fnkname_rows_hash = all_scorer_fnknames_hash,
                };
            }
        };

        pub const ScorerRow = struct {
            level_index: usize,
            offset: ?Vec2,
            magic_id: u32,

            pub const Children = struct {
                create_fnkname_button: Lego.Index,
                fnkname: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .scorer_row);
                const asdf = Toybox.getChildrenExact(2, index);
                return .{
                    .create_fnkname_button = asdf[0],
                    .fnkname = asdf[1],
                };
            }
        };

        pub const EditableTextline = struct {
            inner_text: std.ArrayListUnmanaged(u8),
            /// resets each frame
            cursor_points: std.ArrayListUnmanaged(CursorPoint) = .empty,

            config: Config,
            const Config = struct {
                local_position: Canvas.TextRenderer.TextPosition,
                em: f32,
                text_if_empty: []const u8,

                pub const fnkbox_description: Config = .{
                    .local_position = .centeredAt(.new(0, 0.75 + Lego.Specific.FnkboxBox.text_height / 2.0)),
                    .em = 0.8,
                    .text_if_empty = "<no description>",
                };

                pub const searchbox: Config = .{
                    .local_position = .{
                        .hor = .left,
                        .ver = .baseline,
                        .pos = .new(2.1, 0.9),
                    },
                    .em = 0.5,
                    .text_if_empty = "Search...",
                };
            };

            pub fn new(unowned_text: []const u8, config: Config) !Lego.Index {
                const result = try Toybox.new(.{}, .{ .editable_textline = .{
                    .inner_text = .empty,
                    .config = config,
                } }, .new(@src()));
                const text_allocator = Toybox.getArenaFor(result);
                result.get().specific.editable_textline.inner_text = .fromOwnedSlice(try text_allocator.dupe(u8, unowned_text));
                return result;
            }

            pub fn text(this: @This()) ?[]const u8 {
                return if (this.inner_text.items.len > 0)
                    this.inner_text.items
                else
                    null;
            }
        };

        pub const FnkslistElement = struct {
            fnkbox: Lego.Index,

            pub const height: f32 = 2.0;

            const core = @import("core.zig");
            pub fn build(count: usize, fnkbox: Lego.Index) !Lego.Index {
                const fnkname = try Toybox.dupeIntoFloating(Fnkbox.children(fnkbox).fnkname, .new(@src()));
                fnkname.get().local_point = .{ .pos = .new(1.5, 0.65), .scale = 0.5, .turns = 0.25 };
                fnkname.get().specific.sexpr.is_pattern = false;
                return try Toybox.createWithChildren(.{ .pos = .new(0, tof32(count) * height) }, .{
                    .fnkslist_element = .{ .fnkbox = fnkbox },
                }, &.{fnkname});
            }

            pub fn text(this: @This()) []const u8 {
                return this.fnkbox.children(.fnkbox).box.children(.fnkbox_box).description.get().specific.editable_textline.text() orelse "<empty description>";
            }
        };

        pub const Button = struct {
            local_rect: Rect,
            action: enum {
                /// assumes that the testcase is the direct parent
                launch_testcase,
                see_failing_testcase,
                /// assumes that the scrollbar is the direct parent
                scroll_up,
                /// assumes that the scrollbar is the direct parent
                scroll_down,
                /// assumes that fnkname_holder is the direct parent
                toggle_skip_fnk,
                /// assumes that bubble is the direct parent
                reset_bubble,
                /// assumes that bubble is the direct parent
                unlock_hint,
                /// assumes that scorer_row is the direct parent
                create_fnkbox_for_row,
                /// assumes that the scrollable_list of testcases is the direct parent
                add_testcase,
                /// assumes that the testcase is the direct parent
                delete_testcase,
                /// assumes that executor controls is the direct parent
                stop_execution,
            },
            enabled: bool = true,
            /// only applies to toggle_skip_fnk
            latched: bool = false,
            extra_info: union(enum) {
                none,
                see_failing_testcase: Lego.Specific.Fnkbox.Status,
            } = .none,

            pub fn instant(button: Button) bool {
                return switch (button.action) {
                    .launch_testcase,
                    .delete_testcase,
                    .see_failing_testcase,
                    .scroll_up,
                    .scroll_down,
                    .reset_bubble,
                    .unlock_hint,
                    .create_fnkbox_for_row,
                    .add_testcase,
                    .stop_execution,
                    => false,
                    .toggle_skip_fnk,
                    => true,
                };
            }
        };

        pub const Scrollbar = struct {
            total_rect: Rect,
            total_length: f32,
            visible_length: f32,
            scroll_visual: f32,
            scroll_target: f32,
            prev_scroll_visual: f32 = 0,

            const min_handle_length: f32 = 0.25;
            const max_handle_length: f32 = 1;

            pub fn build(bounding_rect: Rect, total_length: f32, visible_length: f32) Lego.Index {
                const arrows_height = bounding_rect.size.x;
                return try Toybox.createWithChildren(.{}, .{
                    .scrollbar = .{
                        .total_rect = bounding_rect.withSize(.new(
                            bounding_rect.size.x,
                            bounding_rect.size.y - 2 * arrows_height,
                        ), .center),
                        .total_length = total_length,
                        .visible_length = visible_length,
                        .scroll_visual = 0,
                        .scroll_target = 0,
                    },
                }, &.{
                    try Toybox.new(.{}, .{ .button = .{
                        .local_rect = bounding_rect.withSize(.both(arrows_height), .top_left),
                        .action = .scroll_up,
                    } }, .new(@src())),
                    try Toybox.new(.{}, .{ .button = .{
                        .local_rect = bounding_rect.withSize(.both(arrows_height), .bottom_left),
                        .action = .scroll_down,
                    } }, .new(@src())),
                });
            }

            pub fn buildForTestcases(n_testcases: usize, scroll: f32) Scrollbar {
                const total_rect = Lego.Specific.FnkboxBox.testcases_box
                    .withSize(.new(0.5, FnkboxBox.testcases_height - 1.2), .top_left)
                    .move(.new(0.1, 0.6));
                return .{
                    // + 1.0 to give a bit of extra for adding at the end
                    .total_length = tof32(n_testcases) + 1.0,
                    .visible_length = FnkboxBox.visible_testcases,
                    .total_rect = total_rect,
                    .scroll_visual = scroll,
                    .scroll_target = scroll,
                };
            }

            pub fn scrollVisualDelta(scrollbar: *const Scrollbar) f32 {
                return scrollbar.prev_scroll_visual - scrollbar.scroll_visual;
            }

            pub fn handleRectVisual(scrollbar: *const Scrollbar) Rect {
                // assert(scrollbar.visible_length <= scrollbar.total_length);
                const handle_size: Vec2 = scrollbar.total_rect.size.mul(.new(
                    1,
                    math.clamp(
                        math.clamp01(scrollbar.visible_length / scrollbar.total_length),
                        min_handle_length,
                        max_handle_length,
                    ),
                ));
                return scrollbar.total_rect
                    .withSize(handle_size, .top_left)
                    .move(.new(
                    0,
                    // (scrollbar.scroll_visual / tof32(@max(1, scrollbar.total_length - scrollbar.visible_length))) *
                    math.clamp01(scrollbar.scroll_visual / (scrollbar.total_length - scrollbar.visible_length)) *
                        (scrollbar.total_rect.size.y - handle_size.y),
                ));
            }

            pub fn onMouseMoved(scrollbar: *Scrollbar, local_pos: Vec2) void {
                const rect = scrollbar.total_rect
                    .withSize(.new(
                    scrollbar.total_rect.size.x,
                    scrollbar.total_rect.size.y - scrollbar.handleRectVisual().size.y,
                ), .top_left);
                scrollbar.scroll_target = math.clamp01(rect.localFromWorldPosition(local_pos).y) *
                    @max(0, scrollbar.total_length - scrollbar.visible_length);
            }
        };

        pub const Area = struct {
            /// kind of a collider
            bg: Bg,
            style: enum { none, main_area, toolbar, bubble },
            non_interactable: bool = false,

            pub const Bg = union(enum) {
                none,
                all,
                local_rect: Rect,

                pub fn contains(bg_kind: Bg, area_absolute_point: Point, needle_absolute_pos: Vec2) bool {
                    return switch (bg_kind) {
                        .none => false,
                        .all => true,
                        .local_rect => |rect| rect.contains(area_absolute_point.inverseApplyGetLocalPosition(needle_absolute_pos)),
                    };
                }
            };
        };

        pub const Sexpr = struct {
            kind: Kind,
            is_pattern: bool,
            is_pattern_t: f32,
            is_fnkname: bool,
            is_fnkname_t: f32,
            atom_name: []const u8,
            jiggling_t: f32 = 0,

            /// for patterns, this means the "eating value"
            emerging_value: Lego.Index = .nothing,
            emerging_value_t: f32 = 0,

            /// another Lego with a connected 'hotness' to this one
            /// for example, two sexpr values in both sides of the list viewer
            hot_sibling: Index = .nothing,

            // reset each frame
            bindings_all: std.ArrayListUnmanaged([]const u8) = .empty,
            bindings_unbound: std.ArrayListUnmanaged([]const u8) = .empty,

            pub const Kind = enum { empty, atom_lit, atom_var, pair };

            // TODO(bug): undoing doesn't set this back
            pub fn setIsPattern(parent: Lego.Index, is_pattern: bool) void {
                // TODO(polish): slow-mo looks better with this, but not the normal flow
                if (false and parent.get().specific.sexpr.is_pattern != is_pattern) {
                    if (is_pattern) {
                        parent.get().local_point.turns += 0.5;
                    } else {
                        parent.get().local_point.turns -= 0.5;
                    }
                }
                var cur_sexpr = parent;
                while (cur_sexpr != nothing) : (cur_sexpr = Toybox.next_preordered(cur_sexpr, parent).next) {
                    Toybox.get(cur_sexpr).specific.sexpr.is_pattern = is_pattern;
                    var cur_child = Toybox.get(cur_sexpr).specific.sexpr.emerging_value;
                    while (cur_child != nothing) : (cur_child = Toybox.next_preordered(cur_child, cur_sexpr).next) {
                        Toybox.get(cur_child).specific.sexpr.is_pattern = is_pattern;
                    }
                }
                Lego.Specific.Sexpr.updateLocalPositionsAndOfChildren(parent);
            }

            pub fn contains(sexpr_point: Point, is_pattern: bool, kind: Kind, needle_pos: Vec2) bool {
                return ViewHelper.overlapsAtom(is_pattern, sexpr_point, needle_pos, switch (kind) {
                    .atom_var, .atom_lit, .empty => .atom,
                    .pair => .pair,
                });
            }

            pub fn equalValue(a_index: Lego.Index, b_index: Lego.Index) bool {
                const a = &Toybox.get(a_index).specific.sexpr;
                const b = &Toybox.get(b_index).specific.sexpr;
                return equalValueV2(a, b);
            }

            pub fn equalValueV2(a: *const Sexpr, b: *const Sexpr) bool {
                if (a.kind != b.kind) return false;
                switch (a.kind) {
                    .empty => return true,
                    .atom_var, .atom_lit => return std.mem.eql(u8, a.atom_name, b.atom_name),
                    .pair => return equalValueV2(a.left(), b.left()) and equalValueV2(a.right(), b.right()),
                }
            }

            pub fn generateBindings(value: Lego.Index, pattern: Lego.Index, bindings: *Bindings) !bool {
                const p = Toybox.get(pattern).specific.sexpr;
                const v = Toybox.get(value).specific.sexpr;
                switch (p.kind) {
                    .empty => return true,
                    .atom_var => {
                        try bindings.append(.{ .name = p.atom_name, .value = value });
                        return true;
                    },
                    .atom_lit => {
                        switch (v.kind) {
                            .empty => return true,
                            .pair => return false,
                            .atom_lit => return std.mem.eql(u8, v.atom_name, p.atom_name),
                            .atom_var => return true,
                        }
                    },
                    .pair => {
                        switch (v.kind) {
                            else => return false,
                            .pair => {
                                const pat_up, const pat_down = Toybox.getChildrenExact(2, pattern);
                                const val_up, const val_down = Toybox.getChildrenExact(2, value);
                                return try generateBindings(val_up, pat_up, bindings) and try generateBindings(val_down, pat_down, bindings);
                            },
                        }
                    },
                }
            }

            /// Should be called only when changing any of the _t values
            pub fn updateLocalPositionsAndOfChildren(index: Lego.Index) void {
                var cur = index;
                while (cur != nothing) : (cur = Toybox.next_preordered(cur, index).next) {
                    const lego = Toybox.get(cur);
                    const sexpr = &lego.specific.sexpr;

                    if (sexpr.kind == .pair) {
                        const child_up, const child_down = Toybox.getChildrenExact(2, cur);
                        Toybox.get(child_up).local_point = (Point{})
                            .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .up));
                        Toybox.get(child_down).local_point = (Point{})
                            .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .down));
                    }

                    if (sexpr.emerging_value != nothing) {
                        std.log.err("TODO(now)", .{});
                        updateLocalPositionsAndOfChildren(sexpr.emerging_value);
                    }
                }
            }

            pub fn hash(index: Lego.Index) u32 {
                assert(index.hasTag(.sexpr));
                const sexpr = index.get().specific.sexpr;
                return switch (sexpr.kind) {
                    .empty => 0,
                    .atom_lit => hashString(sexpr.atom_name),
                    .atom_var => std.hash.int(hashString(sexpr.atom_name)),
                    .pair => {
                        const l, const r = pairChildren(index);
                        var hasher = std.hash.Wyhash.init(0);
                        std.hash.autoHash(&hasher, struct {
                            left: u32,
                            right: u32,
                        }{ .left = hash(l), .right = hash(r) });
                        return @truncate(hasher.final());
                    },
                };
            }

            fn pairChildren(index: Lego.Index) [2]Lego.Index {
                assert(index.hasTag(.sexpr));
                assert(index.get().specific.sexpr.kind == .pair);
                assert(Toybox.childCount(index) == 2);
                assert(index.get().tree.first.hasTag(.sexpr));
                assert(index.get().tree.last.hasTag(.sexpr));
                return .{ index.get().tree.first, index.get().tree.last };
            }

            fn left(self: *const Sexpr) *const Sexpr {
                assert(self.kind == .pair);
                assert(Toybox.childCount(Lego.fromSpecificConst(.sexpr, self).index) == 2);
                return &Toybox.get(Lego.fromSpecificConst(.sexpr, self).tree.first).specific.sexpr;
            }

            fn right(self: *const Sexpr) *const Sexpr {
                assert(self.kind == .pair);
                assert(Toybox.childCount(Lego.fromSpecificConst(.sexpr, self).index) == 2);
                return &Toybox.get(Lego.fromSpecificConst(.sexpr, self).tree.last).specific.sexpr;
            }

            pub fn toOldCoreValue(sexpr: *const Sexpr, mem: std.mem.Allocator) !*core.Sexpr {
                const result = try mem.create(core.Sexpr);
                result.* = switch (sexpr.kind) {
                    .empty => .empty,
                    .atom_var => .{ .atom_var = .{ .value = sexpr.atom_name } },
                    .atom_lit => .{ .atom_lit = .{ .value = sexpr.atom_name } },
                    .pair => .{ .pair = .{
                        .left = try sexpr.left().toOldCoreValue(mem),
                        .right = try sexpr.right().toOldCoreValue(mem),
                    } },
                };
                return result;
            }

            pub fn toOldCoreValueResolving(sexpr: *const Sexpr, bindings: []const Binding, mem: std.mem.Allocator) !*core.Sexpr {
                const result = try mem.create(core.Sexpr);
                result.* = switch (sexpr.kind) {
                    .empty => .empty,
                    .atom_var => for (bindings) |binding| {
                        if (std.mem.eql(u8, binding.name, sexpr.atom_name)) {
                            mem.destroy(result);
                            return try toOldCoreValue(&binding.value.get().specific.sexpr, mem);
                        }
                    } else .{ .atom_var = .{ .value = sexpr.atom_name } },
                    .atom_lit => .{ .atom_lit = .{ .value = sexpr.atom_name } },
                    .pair => .{ .pair = .{
                        .left = try sexpr.left().toOldCoreValueResolving(bindings, mem),
                        .right = try sexpr.right().toOldCoreValueResolving(bindings, mem),
                    } },
                };
                return result;
            }

            pub fn buildFromOldCoreValue(point: Point, value: *const core.Sexpr, is_pattern: bool, is_fnkname: bool, creation_tag: Lego.CreationTag) !Lego.Index {
                return try Toybox.buildSexpr(point, switch (value.*) {
                    .empty => .empty,
                    .atom_lit => |s| .{ .atom_lit = s.value },
                    .atom_var => |s| .{ .atom_var = s.value },
                    .pair => |pair| .{ .pair = .{
                        .up = try buildFromOldCoreValue(point.applyToLocalPoint(ViewHelper.offsetFor(false, .up)), pair.left, is_pattern, is_fnkname, creation_tag.plus(@src())),
                        .down = try buildFromOldCoreValue(point.applyToLocalPoint(ViewHelper.offsetFor(false, .down)), pair.right, is_pattern, is_fnkname, creation_tag.plus(@src())),
                    } },
                }, is_pattern, is_fnkname, creation_tag.plus(@src()));
            }

            pub fn drawEatingPattern(parent: Lego.Index, var_name: []const u8, t: f32, camera: Rect, drawer: *Drawer, base_alpha: f32) !void {
                var cur = parent;
                const alpha = t * base_alpha;
                while (cur != nothing) : (cur = Toybox.next_preordered(cur, parent).next) {
                    const point = cur.get().absolute_point;
                    const sexpr = cur.get().specific.sexpr;
                    assert(sexpr.is_pattern);
                    switch (sexpr.kind) {
                        .empty => {
                            // TODO(game)
                        },
                        .atom_lit => try drawer.drawPatternAtomSolidColor(
                            camera,
                            point,
                            sexpr.atom_name,
                            var_name,
                            alpha,
                        ),
                        .pair => try drawer.drawPatterPairHolderSolidColor(camera, point, var_name, alpha),
                        .atom_var => {
                            // TODO(game)
                        },
                    }
                }
            }

            fn connectHots(a: Lego.Index, b: Lego.Index) void {
                toybox.undo_stack.storeAllData(a);
                toybox.undo_stack.storeAllData(b);
                a.get().specific.sexpr.hot_sibling = b;
                b.get().specific.sexpr.hot_sibling = a;
            }
        };

        pub const FnknameHolder = struct {
            /// refreshed each frame
            /// might be .nothing
            fnkbox: Lego.Index = .nothing,

            const Children = struct {
                fnkname: Lego.Index,
                toggle_skip: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .fnkname_holder);
                const asdf = Toybox.getChildrenExact(2, index);
                return .{
                    .fnkname = asdf[0],
                    .toggle_skip = asdf[1],
                };
            }

            pub fn build(fnkname: Lego.Index) !Lego.Index {
                assert(fnkname.hasTag(.sexpr));
                fnkname.get().local_point = Case.fnk_name_offset;
                return try Toybox.createWithChildren(.{}, .{ .fnkname_holder = .{} }, &.{
                    fnkname,
                    try Toybox.new(.{}, .{ .button = .{
                        .local_rect = .{ .top_left = .new(5, 0), .size = .one },
                        .action = .toggle_skip_fnk,
                    } }, .new(@src())),
                });
            }

            pub fn text(this: @This()) ?[]const u8 {
                if (this.fnkbox.getSafe() == null) return null;
                return this.fnkbox.children(.fnkbox).box.children(.fnkbox_box).description.get().specific.editable_textline.text();
            }
        };

        pub const Case = struct {
            /// offset for the next garland, used during animations
            next_point_extra: Point = .{},
            /// offset for the fnkname_holder, used during animations
            fnkname_holder_extra: Point = .{},

            const fnk_name_offset: Point = .{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) };
            const next_garland_offset: Vec2 = .new(8, if (SEQUENTIAL_GOES_DOWN) 1 else -1.5);

            const Children = struct {
                pattern: Lego.Index,
                template: Lego.Index,
                fnkname_holder: Lego.Index,
                next: Lego.Index,
            };

            pub fn destroyForParts(index: Lego.Index) Children {
                const result = children(index);
                Toybox.popWithUndoAndChangingCoords(result.pattern);
                Toybox.popWithUndoAndChangingCoords(result.template);
                Toybox.popWithUndoAndChangingCoords(result.fnkname_holder);
                Toybox.popWithUndoAndChangingCoords(result.next);

                if (index.get().tree.parent != nothing) {
                    Toybox.popWithUndo(index);
                }
                Toybox.destroyFloating(index);

                return result;
            }

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .case);
                const asdf = Toybox.getChildrenExact(4, index);
                return .{
                    .pattern = asdf[0],
                    .template = asdf[1],
                    .fnkname_holder = asdf[2],
                    .next = asdf[3],
                };
            }

            pub fn next(case: *const Case) *const Garland {
                return &children(Lego.fromSpecificConst(.case, case).index).next.get().specific.garland;
            }

            /// only call when next_point_extra or fnk_name_extra changes
            pub fn updateLocalPositions(index: Lego.Index) void {
                const case = index.get().specific.case;
                const offsets: [4]Point = .{
                    .{ .pos = .xneg },
                    .{ .pos = .xpos },
                    case.fnkname_holder_extra,
                    (Point{ .pos = .new(8, 1) }).applyToLocalPoint(case.next_point_extra),
                };
                for (Toybox.getChildrenExact(4, index), offsets) |i, offset| {
                    Toybox.get(i).local_point = offset;
                }
            }

            pub fn hash(index: Lego.Index) u32 {
                assert(index.hasTag(.case));
                const c = children(index);
                var hasher = std.hash.Wyhash.init(0);
                std.hash.autoHash(&hasher, struct {
                    pattern: u32,
                    template: u32,
                    fnkname: u32,
                    next: u32,
                }{
                    .pattern = Sexpr.hash(c.pattern),
                    .template = Sexpr.hash(c.template),
                    .fnkname = Sexpr.hash(c.fnkname_holder.children(.fnkname_holder).fnkname),
                    .next = Garland.hash(c.next),
                });
                return @truncate(hasher.final());
            }
        };

        pub const Garland = struct {
            visible: bool = undefined,
            computed_height: f32 = 0,
            /// valid only for garlands that are enqueued in an executor
            enqueued_parent_pill_index: usize = undefined,
            /// valid only for garlands that are enqueued in an executor
            next_enqueued: Lego.Index = .nothing,

            pub const case_drop_preview_dist: f32 = 0.5 * dist_between_cases_rest;
            pub const dist_between_cases_first: f32 = 1.5;
            pub const dist_between_cases_rest: f32 = 2.5;
            pub const relative_fnkname_point: Point = .{ .pos = .{ .x = -2, .y = 0 }, .turns = 0.25, .scale = 0.5 };

            pub fn stealFnkname(garland: Lego.Index, replacement: ?Lego.Index) !Lego.Index {
                const original_fnkname = garland.children(.garland).fnkname;
                if (replacement) |r| {
                    r.get().local_point = Lego.Specific.Garland.relative_fnkname_point;
                    Lego.Specific.Sexpr.setIsPattern(r, true);
                    Toybox.changeChild(original_fnkname, r);
                } else {
                    const new_fnkname = try Toybox.buildSexpr(undefined, .empty, true, true, .new(@src()));
                    Toybox.changeChild(original_fnkname, new_fnkname);
                }
                return original_fnkname;
            }

            pub fn popCase(case: Lego.Index) void {
                Toybox.refreshAbsolutePoints(&.{case});

                assert(case.hasTag(.case));
                const parent = case.get().tree.parent;
                assert(parent.hasTag(.newcase));

                Toybox.popWithUndoAndChangingCoords(case);
                const original_parent_tree = Toybox.get(parent).tree;
                const l_a = Toybox.get(original_parent_tree.next).specific.newcase.length();
                const l_b = Toybox.get(parent).specific.newcase.length();
                Toybox.get(original_parent_tree.next).specific.newcase.length_before = l_b;
                Toybox.get(original_parent_tree.next).specific.newcase.length_after = l_a;
                Toybox.get(original_parent_tree.next).dropzone_t = case.get().hot_t;
                Toybox.get(original_parent_tree.next).local_point = parent.get().local_point;
                Toybox.pop(parent);
                Toybox.destroyFloating(parent);

                Toybox.refreshAbsolutePoints(&.{ case, original_parent_tree.next });
            }

            pub const Children = struct {
                fnkname: Lego.Index,
                cases: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .garland);
                const asdf = Toybox.getChildrenExact(2, index);
                return .{
                    .fnkname = asdf[0],
                    .cases = asdf[1],
                };
            }

            pub fn hasChildCases(garland: *const Garland) bool {
                return Toybox.childCount(garland.casesHolder()) > 1;
            }

            pub fn casesHolder(garland: *const Garland) Lego.Index {
                return children(Lego.fromSpecificConst(.garland, garland).index).cases;
            }

            pub fn firstNewcase(garland: *const Garland) *Specific.NewCase {
                return &garland.casesHolder().get().tree.first.get().specific.newcase;
            }

            pub fn toOldCoreValue(garland: *const Garland, allocator: std.mem.Allocator) !core.FnkBody {
                const cable_segments = try Toybox.getChildrenUnknown(allocator, garland.casesHolder());
                defer allocator.free(cable_segments);

                var cases: core.MatchCases = try .initCapacity(allocator, cable_segments.len - 1);
                for (cable_segments[0 .. cable_segments.len - 1]) |i| {
                    const c = Case.children(i.get().tree.first);
                    const next: ?core.MatchCases = blk: {
                        const asdf = try c.next.get().specific.garland.toOldCoreValue(allocator);
                        if (asdf.cases.items.len == 0) {
                            break :blk null;
                        } else {
                            break :blk asdf.cases;
                        }
                    };
                    cases.appendAssumeCapacity(.{
                        .pattern = try c.pattern.get().specific.sexpr.toOldCoreValue(allocator),
                        .fnk_name = try c.fnkname_holder.children(.fnkname_holder).fnkname.get().specific.sexpr.toOldCoreValue(allocator),
                        .template = try c.template.get().specific.sexpr.toOldCoreValue(allocator),
                        .next = next,
                    });
                }
                return .{ .cases = cases };
            }

            pub fn buildFromOldCoreValue(point: Point, definition: core.FnkBodyV2, scratch: std.mem.Allocator, creation_tag: Lego.CreationTag) !Lego.Index {
                var cases: std.ArrayListUnmanaged(Lego.Index) = try .initCapacity(scratch, definition.cases.len);
                for (definition.cases) |case| {
                    cases.appendAssumeCapacity(try Toybox.buildCase(.{}, .{
                        .pattern = try Sexpr.buildFromOldCoreValue(.{}, case.pattern, true, false, creation_tag.plus(@src())),
                        .template = try Sexpr.buildFromOldCoreValue(.{}, case.template, false, false, creation_tag.plus(@src())),
                        .fnkname = try Sexpr.buildFromOldCoreValue(.{}, case.fnk_name, false, true, creation_tag.plus(@src())),
                        .next = if (case.next) |next|
                            try buildFromOldCoreValue(.{}, .{ .cases = next }, scratch, creation_tag.plus(@src()))
                        else
                            null,
                    }, creation_tag.plus(@src())));
                }
                return try Toybox.buildGarland(point, try cases.toOwnedSlice(scratch), .new(@src()));
            }

            pub fn buildFromOldCoreValueV0(point: Point, definition: core.FnkBody, scratch: std.mem.Allocator, creation_tag: Lego.CreationTag) !Lego.Index {
                var cases: std.ArrayListUnmanaged(Lego.Index) = try .initCapacity(scratch, definition.cases.items.len);
                for (definition.cases.items) |case| {
                    cases.appendAssumeCapacity(try Toybox.buildCase(.{}, .{
                        .pattern = try Sexpr.buildFromOldCoreValue(.{}, case.pattern, true, false, creation_tag.plus(@src())),
                        .template = try Sexpr.buildFromOldCoreValue(.{}, case.template, false, false, creation_tag.plus(@src())),
                        .fnkname = try Sexpr.buildFromOldCoreValue(.{}, case.fnk_name, false, true, creation_tag.plus(@src())),
                        .next = if (case.next) |next|
                            try buildFromOldCoreValueV0(.{}, .{ .cases = next }, scratch, creation_tag.plus(@src()))
                        else
                            null,
                    }, creation_tag.plus(@src())));
                }
                return try Toybox.buildGarland(point, try cases.toOwnedSlice(scratch), creation_tag.plus(@src()));
            }

            /// 0 -> default point
            /// 0...1 rotating
            /// 1 -> enqueued
            fn extraForDequeuingNext(enqueueing_t: f32) Point {
                return .{
                    .pos = .new(enqueueing_t * 6, -3 * math.smoothstepEased(enqueueing_t, 0, 1, .easeInOutCubic)),
                    .turns = math.lerp(0, -0.1, math.smoothstepEased(enqueueing_t, 0, 0.7, .easeInOutCubic)),
                };
            }

            /// same as extraForDequeuingNext but applied from a case (so it has to climb more, for example)
            fn extraForEnqueuingNext(enqueueing_t: f32) Point {
                assert(math.in01(enqueueing_t));
                return .{
                    .pos = .new(enqueueing_t * 4, (-3 - Case.next_garland_offset.y - Garland.dist_between_cases_first) *
                        math.smoothstepEased(enqueueing_t, 0, 0.35, .easeInOutCubic)),
                    .turns = math.lerp(0, -0.1, math.smoothstepEased(enqueueing_t, 0.0, 0.3, .easeInOutCubic)),
                };
            }

            pub fn hash(index: Lego.Index) u32 {
                assert(index.hasTag(.garland));
                var hasher = std.hash.Wyhash.init(0);
                var cur_newcase = children(index).cases.get().tree.first;
                while (cur_newcase != nothing) : (cur_newcase = cur_newcase.get().tree.next) {
                    assert(cur_newcase.hasTag(.newcase));
                    const case = cur_newcase.get().tree.first;
                    if (case != nothing) hasher.update(std.mem.asBytes(&Case.hash(case)));
                }
                return @truncate(hasher.final());
            }
        };

        pub const NewCase = struct {
            length_before: f32 = undefined,
            length_after: f32 = undefined,
            /// used when updating animation
            offset_t: f32 = 0,
            /// used when updating animation
            offset_ghost: Lego.Index = .nothing,

            pub fn length(newcase: *const NewCase) f32 {
                return newcase.length_before + newcase.length_after;
            }
        };

        pub const Pill = struct {
            next_pill: Lego.Index,

            remaining_lifetime: f32 = std.math.inf(f32),
            velocity: Vec2 = .zero,

            pub fn alpha(pill: *const Pill) f32 {
                return math.smoothstep(pill.remaining_lifetime, 0, 0.4);
            }

            pub fn build(
                pattern_point: Point,
                old_first: Lego.Index,
                data: struct {
                    pattern: Lego.Index,
                    input: Lego.Index,
                    fnkname_holder_call: Lego.Index,
                    fnkname_response: Lego.Index,
                    // TODO(game)
                    // bindings: []const Binding,
                },
            ) !Lego.Index {
                const result = try Toybox.new(pattern_point, .{
                    .pill = .{ .next_pill = old_first },
                }, .new(@src()));

                Toybox.addChildLastWithoutChangingAbsPoint(result, data.pattern);
                Toybox.addChildLastWithoutChangingAbsPoint(result, data.input);
                Toybox.addChildLastWithoutChangingAbsPoint(result, data.fnkname_holder_call);
                Toybox.addChildLastWithoutChangingAbsPoint(result, data.fnkname_response);

                return result;
            }
        };

        pub const Executor = struct {
            used_for_bg_computation: bool,
            controlled_by_parent_fnkbox: bool,
            animation: ?struct {
                t: f32 = 0,
                active_case: Lego.Index,
                matching: bool,
                invoked_fnk: Lego.Index,
                // parent_pill: ?usize,
                // TODO(design): rethink
                new_bindings: []const Binding,
                // original_point: Point,
                garland_fnkname: Lego.Index,
                // paused: bool = false,
            } = null,
            first_enqueued: Lego.Index = .nothing,
            first_pill: Lego.Index = .nothing,
            garland_appearing_t: f32 = 1,

            const relative_input_point: Point = .{ .pos = .new(-1, 1.5) };
            const relative_garland_point: Point = .{ .pos = .new(4, 0) };
            const relative_crank_center: Point = .{ .pos = .new(-1, 4) };
            const first_case_point: Point = relative_garland_point.applyToLocalPoint(.{ .pos = .new(0, 1.5) });

            // TODO(design): rethink
            pub fn bindingsActive(executor_index: Lego.Index) BindingsState {
                const executor = Toybox.get(executor_index).specific.executor;
                return if (executor.animation) |anim| .{
                    .anim_t = if (anim.t < 0.2) null else math.remapTo01Clamped(anim.t, 0.2, 0.8),
                    .old = &.{},
                    // TODO(game)
                    // .old = if (anim.parent_pill) |k| executor.prev_pills.items[k].bindings else &.{},
                    .new = anim.new_bindings,
                } else .{
                    .anim_t = null,
                    .old = &.{},
                    .new = &.{},
                };
            }

            pub const Children = struct {
                input: Lego.Index,
                garland: Lego.Index,
                controls: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .executor);
                const asdf = Toybox.getChildrenExact(3, index);
                return .{
                    .input = asdf[0],
                    .garland = asdf[1],
                    .controls = asdf[2],
                };
            }

            pub fn getBrakeT(executor_index: Lego.Index) f32 {
                return children(executor_index).controls.children(.executor_controls).brake.get().specific.executor_brake.brake_t;
            }

            pub fn shouldStartExecution(executor_index: Lego.Index) bool {
                const executor = Toybox.get(executor_index).specific.executor;
                const garland = children(executor_index).garland;
                const input = children(executor_index).input;
                return executor.animation == null and
                    // TODO(bug): empty functions stop execution, even if there are enqueued cases. For example, try 'x -> f: x { y -> y}' with f having no cases
                    //  This line is a small part of the solution, but there's too much more
                    // (garland.garland().hasChildCases() or executor.first_enqueued != nothing) and
                    garland.garland().hasChildCases() and
                    Toybox.get(input).specific.sexpr.kind != .empty;
            }

            pub const Controls = struct {
                pub fn brakeHandlePath(brake_t: f32) Vec2 {
                    return brakeLineRaw(.{}, brake_t, 1.0);
                }

                pub fn brakeLineRaw(crank_center: Point, brake_t: f32, line_t: f32) Vec2 {
                    const radius: f32 = std.math.exp(2 - brake_t) / 2.0;
                    // const radius: f32 = math.remapFrom01(std.math.exp(1 - brake_t) / std.math.e, 1.3, 5);
                    // const radius: f32 = math.remapFrom01(math.easings.linear(1 - brake_t), 1.3, 5);
                    // const radius: f32 = math.remapFrom01(math.easings.easeInQuad(1 - brake_t), 1.3, 5);
                    return crank_center
                        .applyToLocalPoint(.{ .turns = -0.25 })
                        .applyToLocalPoint(.{ .pos = .new(0, 1.2) })
                        .applyToLocalPoint(.{ .pos = .new(0, -radius) })
                        .applyToLocalPosition(.fromPolar(radius - (1 - line_t) * 0.2, 0.25 + 0.65 * line_t / radius));

                    // return crank_center
                    //     .applyToLocalPosition(.fromPolar(
                    //     math.remapFrom01(line_t, 0.5, 1.5 + 0.5 * (1 - brake_t)),
                    //     math.remapFrom01(brake_t, 0.125, 0.375),
                    // ));
                }

                fn speedScale(brake_t: f32) f32 {
                    // 1 -> 0
                    // 0.5 -> 1
                    // 0 -> mucho
                    return std.math.exp2((1 - brake_t) * 2) - 1;
                }

                test "speedScale" {
                    try std.testing.expectApproxEqAbs(0, speedScale(1), 0.0001);
                    try std.testing.expectApproxEqAbs(1, speedScale(0.5), 0.0001);
                    try std.testing.expectApproxEqAbs(3, speedScale(0), 0.0001);
                }
            };
        };

        pub const FnkboxBox = struct {
            const relative_box: Rect = .fromMeasureAndSizeV2(
                .top_center,
                .new(0, 0.75),
                .new(16, box_height),
            );
            const testcases_box: Rect = relative_box.plusMargin3(.top, -box_height + testcases_height);
            const relative_top_testcase_pos: Vec2 = .new(0, box_height - testcases_height);
            const text_height: f32 = 2.4;
            const status_bar_height: f32 = 1;
            const testcases_header_height: f32 = 0.85;
            const testcases_height: f32 = 2.5 * visible_testcases;
            const box_height = text_height + status_bar_height + testcases_header_height + testcases_height;
            const visible_testcases = 2;
            const status_bar_goal: Rect = .fromMeasureAndSizeV2(
                .top_center,
                Vec2.new(0, 0.75).addY(text_height),
                .new(16, status_bar_height),
            );
            const edit_description_button_rect: Rect = relative_box.withSize(.both(1), .top_right);

            pub const Children = struct {
                description: Lego.Index,
                status_bar: Lego.Index,
                testcases_scrollbar: Lego.Index,
                testcases_area: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .fnkbox_box);
                const asdf = Toybox.getChildrenExact(4, index);
                return .{
                    .description = asdf[0],
                    .status_bar = asdf[1],
                    .testcases_scrollbar = asdf[2],
                    .testcases_area = asdf[3],
                };
            }
        };

        pub const Fnkbox = struct {
            // TODO(bug): this should also make the garland non-editable
            editable: bool,
            execution: ?struct {
                original_garland: Lego.Index,
                source: union(enum) {
                    testcase: Lego.Index,
                    input,
                },
                /// only valid if source is testcase
                old_testcase_actual_value: Lego.Index,
                /// only valid if source is testcase and state is .starting or .ending
                original_or_final_input_point: Point,
                /// only present if source is testcase and state is .starting or .ending
                floating_input_or_output: Lego.Index = .nothing,
                /// if source is input, this is ignored
                state: enum { scrolling_towards_case, starting, executing, ending },
                state_t: f32,
            } = null,
            status: Status,
            require_manual_execution: bool = false,

            const relative_fnkname_point: Point = .{ .pos = .new(-1, 1), .scale = 0.5, .turns = 0.25 };
            const relative_executor_point: Point = .{ .pos = .new(-3, 1 + FnkboxBox.box_height) };

            pub const Status = union(enum) {
                /// first still-running testcase
                undetermined: Lego.Index,
                /// the failing testcase
                unsolved: Lego.Index,
                // TODO(game): score
                solved,

                fn bad(status: Status) Lego.Index {
                    return switch (status) {
                        .solved => unreachable,
                        .undetermined, .unsolved => |i| i,
                    };
                }
            };

            pub fn hasExecutionOverTestcase(fnkbox: Fnkbox, testcase: Lego.Index) bool {
                return if (fnkbox.execution) |e|
                    switch (e.source) {
                        .testcase => |x| x == testcase,
                        .input => false,
                    }
                else
                    false;
            }

            pub fn fnkname(fnkbox: *const Fnkbox) Lego.Index {
                return children(Lego.fromSpecificConst(.fnkbox, fnkbox).index).fnkname;
            }

            pub fn executor(fnkbox: *const Fnkbox) ApiFor.Executor {
                return .{ .index = children(Lego.fromSpecificConst(.fnkbox, fnkbox).index).executor };
            }

            pub const Children = struct {
                box: Lego.Index,
                fnkname: Lego.Index,
                executor: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .fnkbox);
                const asdf = Toybox.getChildrenExact(3, index);
                return .{
                    .box = asdf[0],
                    .fnkname = asdf[1],
                    .executor = asdf[2],
                };
            }

            pub fn updateStatus(fnkbox: *Fnkbox, workspace: *Workspace, scratch: std.mem.Allocator, gpa_for_atom_names: std.mem.Allocator, all_fnks: core.FnkCollection, all_fnks_hash: u32, remaining_budget_for_unloaded_testcases: *usize) !void {
                // TODO(clean): remove
                _ = all_fnks;
                // const fnkname_value = try Toybox.get(children(fnkbox_index).fnkname).specific.sexpr.toOldCoreValue(scratch);

                const zone = tracy.initZone(@src(), .{ .name = "update status for fnkbox" });
                defer zone.deinit();

                const fnkbox_index = Lego.fromSpecificConst(.fnkbox, fnkbox).index;

                // update state
                var cur_testcase = FnkboxBox.children(children(fnkbox_index).box).testcases_area.get().tree.first;
                while (cur_testcase != nothing) : (cur_testcase = cur_testcase.get().tree.next) {
                    switch (cur_testcase.get().specific) {
                        else => unreachable,
                        .button => |button| assert(button.action == .add_testcase),
                        .testcase => |*testcase| {
                            const input_hash = if (testcase.source != null) 0 else Sexpr.hash(cur_testcase.children(.testcase).input);
                            if (input_hash == testcase.actual_and_solved_computed_at.input_hash and
                                all_fnks_hash == testcase.actual_and_solved_computed_at.all_fnks_hash)
                            {
                                continue;
                            }

                            testcase.computed = false;

                            if (fnkbox.hasExecutionOverTestcase(cur_testcase)) continue;

                            const outdated = cur_testcase.children(.testcase).actual.hasTag(.sexpr) or
                                input_hash != testcase.started_computation_at.input_hash or
                                all_fnks_hash != testcase.started_computation_at.all_fnks_hash;

                            if (outdated) {
                                Toybox.changeChildAndDestroyOld(cur_testcase.children(.testcase).actual, try Toybox.buildSexpr(Lego.Specific.Testcase.relative_actual_point, .empty, false, false, .new(@src())));
                                testcase.solved = false;
                                testcase.computed = false;
                                testcase.started_computation_at = .never;
                                testcase.actual_and_solved_computed_at = .never;
                            }

                            if (fnkbox.require_manual_execution and fnkbox.execution == null and !testcase.just_manually_executed) continue;

                            if (!testcase.loaded and remaining_budget_for_unloaded_testcases.* == 0) continue;

                            if (outdated) {
                                const input = if (testcase.loaded)
                                    try Toybox.dupeIntoFloating(cur_testcase.children(.testcase).input, .new(@src()))
                                else blk: {
                                    var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);
                                    defer pool.deinit();
                                    const source = testcase.source.?;
                                    const sample = (try levels[source.level].generate_sample(source.sample, &pool, scratch, gpa_for_atom_names)).?;
                                    break :blk try Lego.Specific.Sexpr.buildFromOldCoreValue(
                                        Lego.Specific.Testcase.relative_input_point,
                                        sample.input,
                                        false,
                                        false,
                                        .new(@src()),
                                    );
                                };
                                Toybox.changeChildAndDestroyOld(cur_testcase.children(.testcase).actual, try Toybox.buildExecutor(
                                    .{},
                                    false,
                                    true,
                                    input,
                                    try workspace.getGarlandForFnk(fnkbox_index.children(.fnkbox).fnkname, .{}, scratch),
                                ));
                                testcase.solved = false;
                                testcase.computed = false;
                                testcase.started_computation_at = .{ .all_fnks_hash = all_fnks_hash, .input_hash = input_hash };
                            }

                            for (0..100) |_| {
                                const total_before = toybox.all_legos.len;
                                try workspace.advanceExecutorAnimation(cur_testcase.children(.testcase).actual, undefined, scratch);
                                if (!testcase.loaded) remaining_budget_for_unloaded_testcases.* -|= 1;
                                if (remaining_budget_for_unloaded_testcases.* == 0) break;
                                if (cur_testcase.children(.testcase).actual.get().specific.executor.animation == null) break;
                                const total_after = toybox.all_legos.len;
                                // TODO(bug): do a better job about limiting expensive executions
                                if (total_after > total_before + 1000) break;
                            }

                            if (cur_testcase.children(.testcase).actual.get().specific.executor.animation == null) {
                                const new_actual = cur_testcase.children(.testcase).actual.children(.executor).input;
                                Toybox.pop(new_actual);
                                Toybox.changeChildAndDestroyOld(
                                    cur_testcase.children(.testcase).actual,
                                    new_actual,
                                );
                                new_actual.get().local_point = Lego.Specific.Testcase.relative_actual_point;
                                Toybox.refreshAbsolutePoints(&.{cur_testcase});

                                testcase.just_manually_executed = false;
                                testcase.computed = true;
                                testcase.actual_and_solved_computed_at = .{ .input_hash = input_hash, .all_fnks_hash = all_fnks_hash };
                                if (testcase.loaded) {
                                    testcase.solved = Lego.Specific.Sexpr.equalValue(new_actual, cur_testcase.children(.testcase).expected);
                                } else {
                                    var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);
                                    defer pool.deinit();
                                    const source = testcase.source.?;
                                    const sample = (try levels[source.level].generate_sample(source.sample, &pool, scratch, gpa_for_atom_names)).?;
                                    const expected = try Lego.Specific.Sexpr.buildFromOldCoreValue(
                                        Lego.Specific.Testcase.relative_input_point,
                                        sample.expected,
                                        false,
                                        false,
                                        .new(@src()),
                                    );
                                    testcase.solved = Lego.Specific.Sexpr.equalValue(new_actual, expected);
                                    Toybox.destroyFloating(expected);
                                }
                            }
                        },
                    }
                }

                // Get the actual status and update testcases solved
                const box_index = children(fnkbox_index).box;
                cur_testcase = FnkboxBox.children(box_index).testcases_area.get().tree.first;
                var all_good = true;
                while (cur_testcase != nothing) : (cur_testcase = cur_testcase.get().tree.next) {
                    const correct, const computed = switch (cur_testcase.get().specific) {
                        else => unreachable,
                        .button => |button| {
                            assert(button.action == .add_testcase);
                            continue;
                        },
                        .testcase => |testcase| .{ testcase.solved, testcase.computed },
                    };
                    if (!computed) {
                        fnkbox.status = .{ .undetermined = cur_testcase };
                        all_good = false;
                        break;
                    }
                    if (!correct) {
                        fnkbox.status = .{ .unsolved = cur_testcase };
                        all_good = false;
                        break;
                    }
                }
                if (all_good) {
                    fnkbox.status = .solved;
                }
            }
        };

        pub const Testcase = struct {
            solved: bool = false,
            computed: bool = false,
            source: ?Source,
            loaded: bool,
            just_manually_executed: bool = false,

            started_computation_at: CodeState = .never,
            actual_and_solved_computed_at: CodeState = .never,

            pub const CodeState = struct {
                all_fnks_hash: u32,
                input_hash: u32,

                pub const never: CodeState = .{ .all_fnks_hash = 0, .input_hash = 0 };
            };

            pub const Source = struct {
                level: usize,
                sample: usize,
                input_hash: u32,
                expected_hash: u32,

                pub fn build(level: usize, sample: usize, sample_core: Sample) Source {
                    return .{
                        .level = level,
                        .sample = sample,
                        .input_hash = sample_core.input.hash(),
                        .expected_hash = sample_core.expected.hash(),
                    };
                }
            };

            pub const Children = struct {
                input: Lego.Index,
                expected: Lego.Index,
                actual: Lego.Index,
                play_button: Lego.Index,
            };

            pub const relative_actual_point: Point = .{ .pos = .new(4, 0) };
            pub const relative_expected_point: Point = .{ .pos = .new(0, 0) };
            pub const relative_input_point: Point = .{ .pos = .new(-4, 0) };
            pub const relative_bounding_box: Rect = .fromCenterAndSize(.zero, .new(FnkboxBox.relative_box.size.x, 2.5));
            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .testcase);
                const asdf = Toybox.getChildrenExact(4, index);
                return .{
                    .input = asdf[0],
                    .expected = asdf[1],
                    .actual = asdf[2],
                    .play_button = asdf[3],
                };
            }
        };

        pub const Microscope = struct {
            in_toolbar: bool,

            /// To understand this, think of the fixed point of the lenses zoom
            pub const Transform = struct {
                center: Vec2,
                scale: f32,

                pub const identity: Transform = .{ .center = .zero, .scale = 1 };

                pub fn inverse(transform: Transform) Transform {
                    return .{ .center = transform.center, .scale = 1.0 / transform.scale };
                }

                pub fn actOnPosition(transform: Transform, position: Vec2) Vec2 {
                    return transform.actOn(.{ .pos = position }).pos;
                }

                pub fn actOn(transform: Transform, point: Point) Point {
                    return .{
                        .pos = transform.center.add(
                            point.pos.sub(transform.center).scale(transform.scale),
                        ),
                        .scale = point.scale * transform.scale,
                        .turns = point.turns,
                    };
                }

                pub fn combine(first: Transform, second: Transform) Transform {
                    // center is the fixed point of applying first, then second
                    return .{
                        .center = Vec2.add(
                            first.center.scale((1.0 - first.scale) * second.scale),
                            second.center.scale(1.0 - second.scale),
                        ).scale(1.0 / (1.0 - first.scale * second.scale)),
                        .scale = first.scale * second.scale,
                    };
                }

                pub fn getCamera(transform: Transform, original_camera: Rect) Rect {
                    return .fromCorners(
                        transform.inverse().actOnPosition(original_camera.top_left),
                        transform.inverse().actOnPosition(original_camera.get(.bottom_right)),
                    );
                }

                test getCamera {
                    const original_camera: Rect = .unit;
                    const transform: Transform = fromLenses(.new(1.5, 0.5), 0.25, .new(0.5, 0.5), 0.5);
                    const expected_camera: Rect = .fromCenterAndSize(.new(1.5, 0.5), .both(0.5));
                    try Rect.expectApproxEqAbs(expected_camera, transform.getCamera(original_camera), 0.001);
                }

                fn fromLenses(source_pos: Vec2, source_radius: f32, target_pos: Vec2, target_radius: f32) Transform {
                    const scale = target_radius / source_radius;
                    const delta = target_pos.sub(source_pos);
                    return .{
                        .center = source_pos.sub(delta.scale(1.0 / (scale - 1.0))),
                        .scale = scale,
                    };
                }
            };
        };

        pub const Lens = struct {
            local_radius: f32,
            /// set by the parent each frame
            transform: Microscope.Transform = undefined,
            /// set by the parent each frame
            is_target: bool = undefined,
            /// set by the parent each frame
            roots_to_interact: []Lego.Index = undefined,
            /// set by the parent each frame
            roots_to_draw: []Lego.Index = undefined,

            pub const source: Lens = .{ .local_radius = 0.25 };
            pub const target: Lens = .{ .local_radius = 1 };
        };

        pub const Postit = struct {
            pub const local_rect: Rect = .fromCenterAndSize(.zero, .both(6));

            pub const Helper = struct {
                main_area: Lego.Index,

                const DrawingPart = struct {
                    point: Point,
                    part: union(enum) {
                        paragraph: []const []const u8,
                        left_paragraph: []const []const u8,
                        arrow,
                        long_arrow,
                        launch_testcase_button,
                        piece_center,
                        thing: Lego.Index,
                    },
                };

                pub fn addFromParts(this: @This(), pos: Vec2, parts: []const DrawingPart) void {
                    Toybox.addChildLast(this.main_area, blk: {
                        const postit = try Toybox.new(
                            .{ .pos = pos },
                            .{ .postit = .{} },
                            .new(@src()),
                        );

                        for (parts) |part| {
                            const top_left: Point = .{ .pos = Lego.Specific.Postit.local_rect.top_left };
                            const center = top_left.applyToLocalPoint(part.point);
                            switch (part.part) {
                                inline else => |_, part_tag| {
                                    Toybox.addChildLast(postit, try Toybox.new(
                                        center,
                                        .{ .postit_drawing = switch (part_tag) {
                                            .left_paragraph, .paragraph, .thing => comptime unreachable,
                                            .arrow => .arrow,
                                            .long_arrow => .long_arrow,
                                            .piece_center => .piece_center,
                                            .launch_testcase_button => .launch_testcase_button,
                                        } },
                                        .new(@src()),
                                    ));
                                },
                                .thing => |index| {
                                    Toybox.addChildLastV2(center, postit, index);
                                },
                                inline .paragraph, .left_paragraph => |lines, t| {
                                    for (lines, 0..) |line, k| {
                                        Toybox.addChildLast(postit, try Toybox.new(
                                            center.applyToLocalPoint(.{ .pos = .new(0, (tof32(k) - (tof32(lines.len) - 1) / 2.0)) }),
                                            .{ .postit_text = .{ .text = line, .kind = switch (t) {
                                                else => comptime unreachable,
                                                .left_paragraph => .left,
                                                .paragraph => .center,
                                            } } },
                                            .new(@src()),
                                        ));
                                    }
                                },
                            }
                        }

                        break :blk postit;
                    });
                }

                pub fn addFromText(this: @This(), pos: Vec2, lines: []const []const u8) void {
                    Toybox.addChildLast(this.main_area, blk: {
                        const postit = try Toybox.new(
                            .{ .pos = pos },
                            .{ .postit = .{} },
                            .new(@src()),
                        );

                        const max_line_len = 15;

                        for (lines, 0..) |line, k| {
                            Toybox.addChildLast(postit, try Toybox.new(
                                .{
                                    .pos = .new(0, (tof32(k) - (tof32(lines.len) - 1) / 2.0)),
                                    .scale = tof32(max_line_len) / @max(tof32(line.len), tof32(max_line_len)),
                                },
                                .{ .postit_text = .{ .text = line } },
                                .new(@src()),
                            ));
                        }

                        break :blk postit;
                    });
                }
            };
        };

        pub const ScrollableList = struct {
            kind: enum { listviewer_sexprs, fnkbox_testcases, fnkslist },

            pub fn insertElement(inbetween: Lego.Index, new_element: Lego.Index) !void {
                assert(Toybox.isFloating(new_element));
                assert(inbetween.hasTag(.scrollable_list_inbetween));

                Toybox.changeCoordinates(new_element, Toybox.parentAbsolutePoint(new_element), Toybox.parentAbsolutePoint(inbetween));
                Toybox.insertAfter(new_element, inbetween);
                Toybox.insertAfter(try Toybox.new(inbetween.get().local_point, inbetween.get().specific, .new(@src())), new_element);
            }

            pub fn popElement(element: Lego.Index) !void {
                Toybox.refreshAbsolutePoints(&.{element});

                const prev_between = element.get().tree.prev;
                const next_between = element.get().tree.next;
                assert(prev_between.hasTag(.scrollable_list_inbetween));
                assert(next_between.hasTag(.scrollable_list_inbetween));
                const kind = prev_between.get().specific.scrollable_list_inbetween.kind;

                Toybox.pop(prev_between);
                Toybox.pop(next_between);
                Toybox.destroyFloating(prev_between);
                Toybox.destroyFloating(next_between);

                const new_between = try Toybox.new(element.get().absolute_point, .{ .scrollable_list_inbetween = .{ .kind = kind } }, .new(@src()));
                Toybox.changeChildWithUndoAndAlsoCoords(element, new_between);

                // TODO(code): revise
                // const l_a = Toybox.get(original_parent_tree.next).specific.newcase.length();
                // const l_b = Toybox.get(parent).specific.newcase.length();
                // Toybox.get(original_parent_tree.next).specific.newcase.length_before = l_b;
                // Toybox.get(original_parent_tree.next).specific.newcase.length_after = l_a;
                // Toybox.get(original_parent_tree.next).dropzone_t = element.get().hot_t;
                // Toybox.get(original_parent_tree.next).local_point = parent.get().local_point;
                // Toybox.pop(parent);
                // Toybox.destroyFloating(parent);

                Toybox.refreshAbsolutePoints(&.{ element, new_between });
            }

            pub fn canPluckElements(this: @This()) bool {
                return switch (this.kind) {
                    .listviewer_sexprs => true,
                    .fnkbox_testcases, .fnkslist => false,
                };
            }

            pub fn instantUpdates(this: @This()) bool {
                return switch (this.kind) {
                    .listviewer_sexprs, .fnkbox_testcases => false,
                    // TODO(polish): this is true just because they get recreated each frame,
                    //  i would much rather this function always return false
                    .fnkslist => true,
                };
            }

            pub fn base(this: @This()) Vec2 {
                return switch (this.kind) {
                    .listviewer_sexprs => .new(5, -1.75 - 0.05),
                    // .listviewer_sexprs => .new(4.5, -1.25 - 0 * 0.125),
                    .fnkbox_testcases => Lego.Specific.FnkboxBox.relative_top_testcase_pos.addY(2),
                    .fnkslist => .new(0, Workspace.toolbar_fnks_searchbox_height),
                };
            }

            pub fn elementScale(this: @This()) f32 {
                return switch (this.kind) {
                    .listviewer_sexprs => 0.5,
                    .fnkbox_testcases, .fnkslist => 1,
                };
            }

            pub fn spacing(this: @This()) f32 {
                return switch (this.kind) {
                    .listviewer_sexprs => 1.25,
                    .fnkbox_testcases => 2.5,
                    .fnkslist => Lego.Specific.FnkslistElement.height,
                };
            }

            pub fn rect(this: @This()) Rect {
                return switch (this.kind) {
                    .listviewer_sexprs => .fromCenterAndSize(.new(5.75 - 0.125, 0), .new(2.25, 5)),
                    .fnkbox_testcases => Specific.FnkboxBox.testcases_box,
                    .fnkslist => Workspace.toolbar_fnks_rect,
                };
            }

            pub fn clip(this: @This()) bool {
                return switch (this.kind) {
                    .listviewer_sexprs => true,
                    .fnkbox_testcases => true,
                    .fnkslist => false,
                };
            }
        };

        // TODO(game): smooth anim when popping a list element
        pub const ListViewer = struct {
            // (almost) ensures that the hash starts out incorrect
            main_hash: u32 = 123,
            list_hash: u32 = 123,

            pub fn computeMainHash(index: Lego.Index) u32 {
                const main = children(index).main;
                return Sexpr.hash(main);
            }

            pub fn computeListHash(index: Lego.Index) u32 {
                const list = children(index).scrollable_list;
                var hasher = std.hash.Wyhash.init(0);
                var cur = list.get().tree.first;
                while (cur != nothing) : (cur = cur.get().tree.next) {
                    if (cur.hasTag(.scrollable_list_inbetween)) continue;
                    hasher.update(std.mem.asBytes(&Sexpr.hash(cur)));
                }
                hasher.update(std.mem.asBytes(&Sexpr.hash(children(index).sentinel)));
                return @truncate(hasher.final());
            }

            pub const Children = struct {
                main: Lego.Index,
                scrollbar: Lego.Index,
                scrollable_list: Lego.Index,
                sentinel: Lego.Index,
            };

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .list_viewer);
                const asdf = Toybox.getChildrenExact(4, index);
                return .{
                    .main = asdf[0],
                    .scrollbar = asdf[1],
                    .scrollable_list = asdf[2],
                    .sentinel = asdf[3],
                };
            }

            // TODO(game): avoid sudden jumps
            pub fn canonize(index: Lego.Index) !void {
                const lego = index.get();
                const new_main_hash = Lego.Specific.ListViewer.computeMainHash(index);
                const new_list_hash = Lego.Specific.ListViewer.computeListHash(index);

                const list_viewer = &lego.specific.list_viewer;
                if (list_viewer.main_hash != new_main_hash) {
                    list_viewer.main_hash = new_main_hash;
                    defer list_viewer.list_hash = Lego.Specific.ListViewer.computeListHash(lego.index);

                    const lego_children = lego.index.children(.list_viewer);
                    const main = lego_children.main;
                    assert(main.hasTag(.sexpr));

                    if (true) { // destroy all old children
                        var cur = Toybox.get(lego_children.scrollable_list).tree.first;
                        while (cur != nothing) {
                            const original_tree = Toybox.get(cur).tree;
                            Toybox.pop(cur);
                            Toybox.destroyFloating(cur);
                            cur = original_tree.next;
                        }
                    }

                    var count: f32 = 0;
                    if (true) { // create new children
                        var cur_parent = main;
                        while (cur_parent.get().specific.sexpr.kind == .pair) {
                            const left, cur_parent = Lego.Specific.Sexpr.pairChildren(cur_parent);
                            count += 1;
                            Toybox.addChildLast(
                                lego_children.scrollable_list,
                                try Toybox.new(.{}, .{ .scrollable_list_inbetween = .{ .kind = .listviewer_sexprs } }, .new(@src())),
                            );
                            const foo = try Toybox.dupeIntoFloating(left, .new(@src()));
                            Toybox.addChildLast(
                                lego_children.scrollable_list,
                                foo,
                            );
                            Lego.Specific.Sexpr.connectHots(foo, left);
                        }
                        Toybox.addChildLast(
                            lego_children.scrollable_list,
                            try Toybox.new(.{}, .{ .scrollable_list_inbetween = .{ .kind = .listviewer_sexprs } }, .new(@src())),
                        );
                        const new_sentinel = try Toybox.dupeIntoFloating(cur_parent, .new(@src()));
                        new_sentinel.get().local_point = lego_children.sentinel.get().local_point;
                        Lego.Specific.Sexpr.connectHots(new_sentinel, cur_parent);
                        Toybox.changeChildAndDestroyOld(lego_children.sentinel, new_sentinel);
                    }
                    // + 0.5 to give a bit of extra for adding at the end
                    lego_children.scrollbar.get().specific.scrollbar.total_length = count + 0.5;
                    Toybox.refreshAbsolutePoints(&.{index});
                } else if (list_viewer.list_hash != new_list_hash) {
                    list_viewer.list_hash = new_list_hash;

                    const lego_children = lego.index.children(.list_viewer);
                    assert(lego_children.main.hasTag(.sexpr));

                    const new_main = try Toybox.buildSexpr(lego_children.main.get().local_point, .empty, false, false, .new(@src()));
                    var cur_parent = new_main;

                    var cur_item = Toybox.get(lego_children.scrollable_list).tree.first;
                    var count: f32 = 0;
                    while (cur_item != nothing) : (cur_item = cur_item.get().tree.next) {
                        if (cur_item.hasTag(.scrollable_list_inbetween)) continue;
                        count += 1;
                        const next_parent = try Toybox.buildSexpr(undefined, .empty, false, false, .new(@src()));
                        // TODO(optim): avoid this by directly creating either a pair or an empty
                        toybox.undo_stack.storeAllData(cur_parent);
                        cur_parent.get().specific.sexpr.kind = .pair;
                        const foo = try Toybox.dupeIntoFloating(cur_item, .new(@src()));
                        Toybox.addChildLastV2(ViewHelper.offsetFor(false, .up), cur_parent, foo);
                        Toybox.addChildLastV2(ViewHelper.offsetFor(false, .down), cur_parent, next_parent);
                        cur_parent = next_parent;
                        Lego.Specific.Sexpr.connectHots(foo, cur_item);
                    }
                    // + 0.5 to give a bit of extra for adding at the end
                    lego_children.scrollbar.get().specific.scrollbar.total_length = count + 0.5;

                    const sentinel = try Toybox.dupeIntoFloating(lego_children.sentinel, .new(@src()));
                    sentinel.get().local_point = cur_parent.get().local_point;
                    const sentinel_is_wrong = sentinel.get().specific.sexpr.kind == .pair;

                    Toybox.changeChild(lego_children.main, new_main);
                    Toybox.destroyFloating(lego_children.main);

                    // TODO(optim): avoid this by directly creating either a pair or the sentinel
                    Toybox.changeChild(cur_parent, sentinel);
                    Toybox.destroyFloating(cur_parent);

                    if (sentinel_is_wrong) {
                        list_viewer.main_hash = 0;
                        try canonize(index);
                    } else {
                        list_viewer.main_hash = Lego.Specific.ListViewer.computeMainHash(lego.index);
                        Toybox.refreshAbsolutePoints(&.{index});
                    }
                }
            }
        };

        pub const MetaViewer = struct {
            // (almost) ensures that the hash starts out incorrect
            value_hash: u32 = 123,
            garland_hash: u32 = 123,

            pub fn computeValueHash(meta_viewer: Lego.Index) u32 {
                return Sexpr.hash(children(meta_viewer).value);
            }

            pub fn computeGarlandHash(meta_viewer: Lego.Index) u32 {
                return Garland.hash(children(meta_viewer).garland);
            }

            pub const Children = struct {
                value: Lego.Index,
                garland: Lego.Index,
            };

            pub fn children(meta_viewer: Lego.Index) Children {
                assert(Toybox.get(meta_viewer).specific.tag() == .meta_viewer);
                const asdf = Toybox.getChildrenExact(2, meta_viewer);
                return .{
                    .value = asdf[0],
                    .garland = asdf[1],
                };
            }

            pub fn canonize(meta_viewer_index: Lego.Index, scratch: std.mem.Allocator) !void {
                const lego = meta_viewer_index.get();
                const new_value_hash = Lego.Specific.MetaViewer.computeValueHash(meta_viewer_index);
                const new_garland_hash = Lego.Specific.MetaViewer.computeGarlandHash(meta_viewer_index);

                var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);
                defer pool.deinit();

                const meta_viewer = &lego.specific.meta_viewer;
                if (meta_viewer.value_hash != new_value_hash) {
                    meta_viewer.value_hash = new_value_hash;
                    defer meta_viewer.garland_hash = Lego.Specific.MetaViewer.computeGarlandHash(meta_viewer_index);

                    const old_garland = children(meta_viewer_index).garland;
                    const value = children(meta_viewer_index).value;

                    const core_value = try Sexpr.toOldCoreValue(&value.get().specific.sexpr, scratch);
                    const maybe_fnkbody: ?core.FnkBody = core.fnkFromSexpr(core_value, scratch, &pool) catch |err| switch (err) {
                        error.OutOfMemory => |x| return x,
                        else => null,
                    };

                    const new_garland = if (maybe_fnkbody) |fnkbody|
                        try Garland.buildFromOldCoreValueV0(old_garland.get().local_point, fnkbody, scratch, .new(@src()))
                    else
                        try Toybox.buildGarland(old_garland.get().local_point, &.{}, .new(@src()));

                    Toybox.changeChild(old_garland, new_garland);
                    Toybox.destroyFloating(old_garland);
                    Toybox.refreshAbsolutePoints(&.{meta_viewer_index});
                } else if (meta_viewer.garland_hash != new_garland_hash) {
                    meta_viewer.garland_hash = new_garland_hash;
                    defer meta_viewer.value_hash = Lego.Specific.MetaViewer.computeValueHash(meta_viewer_index);

                    const garland = children(meta_viewer_index).garland;
                    const old_value = children(meta_viewer_index).value;

                    // TODO(game): minimize the number of "lit" helpers added; (lit . (x . y)) instead of ((lit . x) . (lit . y))
                    const core_garland = try Garland.toOldCoreValue(&garland.get().specific.garland, scratch);
                    const core_value = try core.sexprFromCases(core_garland.cases.items, &pool);

                    const new_value = try Sexpr.buildFromOldCoreValue(old_value.get().local_point, core_value, false, false, .new(@src()));

                    Toybox.changeChild(old_value, new_value);
                    Toybox.destroyFloating(old_value);
                    Toybox.refreshAbsolutePoints(&.{meta_viewer_index});
                }
            }
        };
    };

    pub const Tree = struct {
        first: Index,
        last: Index,
        next: Index,
        prev: Index,
        parent: Index,

        pub const empty: Tree = .{
            .first = .nothing,
            .last = .nothing,
            .next = .nothing,
            .prev = .nothing,
            .parent = .nothing,
        };

        pub fn isFloating(tree: Tree) bool {
            if (tree.parent == nothing) {
                assert(tree.next == nothing);
                assert(tree.prev == nothing);
                return true;
            } else return false;
        }

        pub fn isChildless(tree: Tree) bool {
            if (tree.first == nothing) {
                assert(tree.last == nothing);
                return true;
            } else {
                assert(tree.last != nothing);
                return false;
            }
        }

        pub fn equals(a: Tree, b: Tree) bool {
            return std.meta.eql(a, b);
        }
    };

    const INCLUDE_GENERATION = INCLUDE_DEBUG_FIELDS;
    pub const Index = packed struct(if (INCLUDE_GENERATION) u64 else u32) {
        index: u32,
        generation: Generation,

        const Generation = if (INCLUDE_GENERATION) u32 else void;

        pub const nothing: Index = .{
            .index = std.math.maxInt(u32),
            .generation = if (INCLUDE_GENERATION) std.math.maxInt(u32) else {},
        };

        pub fn firstGen(index: u32) Index {
            return .{
                .index = index,
                .generation = if (INCLUDE_GENERATION) 0 else {},
            };
        }

        pub fn nextGen(original: Index) Index {
            var new = original;
            if (INCLUDE_GENERATION) new.generation += 1;
            return new;
        }

        pub fn asU32(index: Index) u32 {
            return index.index;
        }

        pub fn hasTag(index: Index, tag: Specific.Tag) bool {
            if (index == Index.nothing) return false;
            return Toybox.get(index).specific.tag() == tag;
        }

        pub fn exists(index: Lego.Index) bool {
            if (index == Lego.Index.nothing) return false;
            return Toybox.getUnsafe(index).exists;
        }

        pub fn get(index: Index) *Lego {
            return Toybox.get(index);
        }

        pub fn getSafe(index: Index) ?*Lego {
            return Toybox.safeGet(index);
        }

        pub fn case(index: Index) Lego.Specific.Case.Children {
            return index.children(.case);
        }

        pub fn garland(index: Index) struct {
            self: Lego.Index,

            pub fn cases(this: @This()) Index {
                return this.self.get().tree.last;
            }

            pub fn hasChildCases(this: @This()) bool {
                return Toybox.childCount(this.cases()) > 1;
            }
        } {
            assert(Toybox.get(index).specific.tag() == .garland);
            return .{ .self = index };
        }

        pub fn children(index: Index, comptime specific: Specific.Tag) Specific.Tagged(specific).Children {
            if (!index.hasTag(specific)) {
                std.debug.panic("bad lego: {any}", .{index.get().specific});
            }
            assert(index.hasTag(specific));
            return Specific.Tagged(specific).children(index);
        }

        pub fn scrollbar(index: Index, comptime specific: Specific.Tag) Lego.Index {
            assert(index.hasTag(specific));
            return switch (specific) {
                else => comptime unreachable,
                .scrollable_list => switch (index.get().specific.scrollable_list.kind) {
                    .listviewer_sexprs,
                    .fnkbox_testcases,
                    .fnkslist,
                    => index.get().tree.prev,
                },
            };
        }

        pub fn isTheSexprLit(index: Index, lit: []const u8) bool {
            if (!index.hasTag(.sexpr)) return false;
            const sexpr = index.get().specific.sexpr;
            return sexpr.kind == .atom_lit and std.mem.eql(u8, sexpr.atom_name, lit);
        }

        pub fn getTheSexprVar(index: Index) ?[]const u8 {
            if (!index.hasTag(.sexpr)) return null;
            const sexpr = index.get().specific.sexpr;
            return if (sexpr.kind == .atom_var) sexpr.atom_name else null;
        }

        pub fn isTheSexprVar(index: Index, maybe_varname: ?[]const u8) bool {
            if (maybe_varname) |varname| {
                if (!index.hasTag(.sexpr)) return false;
                const sexpr = index.get().specific.sexpr;
                return sexpr.kind == .atom_var and std.mem.eql(u8, sexpr.atom_name, varname);
            } else return false;
        }
    };

    pub fn handle(lego: *const Lego) ?Handle {
        const kind: Handle.Kind = switch (lego.specific) {
            .bubble,
            .bubble_connection,
            .scorer,
            .scorer_row,
            .scorer_rows,
            .sexpr,
            .area,
            .microscope,
            .button,
            .scrollbar,
            .fnkbox_box,
            .editable_textline,
            .scrollable_list,
            .fnkslist_element,
            .executor,
            .testcase,
            .pill,
            .postit,
            .postit_text,
            .postit_drawing,
            .executor_controls,
            .garland_newcases,
            .fnkname_holder,
            => return null,
            .executor_brake => .default_extrahitbox,
            .executor_crank => |crank| if (crank.enabled) .default_extrahitbox else return null,
            .case => .default,
            .newcase => .new_case,
            .scrollable_list_inbetween => .new_case,
            .garland => .garland,
            .lens => .lens,
            .list_viewer => .{ .circle = Handle.Kind.default.circle.scale(2) },
            .meta_viewer => .{ .circle = Handle.Kind.default.circle.scale(2) },
            // TODO(game): maybe fnkbox tabs
            .fnkbox => |fnkbox| if (fnkbox.editable) .default else return null,
            // .fnkbox => |fnkbox| if (fnkbox.editable) .fnkbox_tab else return null,
        };
        const enabled: bool = switch (lego.specific) {
            else => true,
            .garland => |garland| garland.visible,
        };
        return .{
            .point = lego.absolute_point.applyToLocalPoint(.{ .pos = lego.handleLocalOffset() }),
            .hot_t = lego.hot_t + lego.dropzone_t,
            .kind = kind,
            .enabled = enabled,
        };
    }

    fn handleLocalOffset(lego: *const Lego) Vec2 {
        return switch (lego.specific) {
            .lens => |lens| .fromPolar(lens.local_radius + 0.1, 1.0 / 8.0),
            .newcase => |newcase| .new(0, newcase.length_before),
            .executor_brake => |brake| brake.handle_pos,
            .executor_crank => |crank| crank.handle_pos,
            .list_viewer => Vec2.new(-1, -1).scale(@sqrt(2.0)),
            .meta_viewer => Vec2.new(-1, 1).scale(@sqrt(2.0)),
            .scrollable_list_inbetween => |t| switch (t.kind) {
                .listviewer_sexprs => .new(-0.5, math.lerp(-1, -0.5, lego.dropzone_t)),
            },
            else => .zero,
        };
    }

    fn draggable(lego: *const Lego) bool {
        _ = lego;
        return true;
    }

    pub fn fromSpecific(comptime tag: Specific.Tag, pointer: *Specific.Tagged(tag)) *Lego {
        return @fieldParentPtr("specific", @as(
            *Specific,
            @alignCast(@fieldParentPtr(@tagName(tag), pointer)),
        ));
    }

    pub fn fromSpecificConst(comptime tag: Specific.Tag, pointer: *const Specific.Tagged(tag)) *const Lego {
        return @fieldParentPtr("specific", @as(
            *const Specific,
            @alignCast(@fieldParentPtr(@tagName(tag), pointer)),
        ));
    }

    pub fn addScroll(lego: *Lego, amount: f32) void {
        switch (lego.specific) {
            else => unreachable,
            inline .scrollable_list => |_, t| {
                lego.index.scrollbar(t).get().specific.scrollbar.scroll_target += amount;
            },
            .scrollbar => |*scrollbar| {
                scrollbar.scroll_target += amount;
            },
        }
    }

    pub fn getGrabbedOffset(lego: *const Lego, absolute_needle: Vec2) Vec2 {
        return switch (lego.specific) {
            .postit, .microscope, .fnkbox => lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle),
            .scrollbar => |scrollbar| lego.absolute_point.applyToLocalPoint(.{ .pos = scrollbar.handleRectVisual().top_left }).inverseApplyGetLocalPosition(absolute_needle),
            else => .zero,
        };
    }

    pub fn canDuplicate(lego: *const Lego) enum { yes, no, fnkbox } {
        return switch (lego.specific) {
            .sexpr,
            .garland,
            .executor,
            .case,
            .postit,
            .list_viewer,
            .meta_viewer,
            => .yes,
            .scrollbar,
            .button,
            .executor_brake,
            .executor_crank,
            .editable_textline,
            => .no,
            .fnkbox => .fnkbox,
            .lens => blk: {
                std.log.err("TODO(game): handle better", .{});
                break :blk .no;
            },
            .bubble,
            .bubble_connection,
            .scorer,
            .scorer_row,
            .fnkname_holder,
            .garland_newcases,
            .scorer_rows,
            .executor_controls,
            .microscope,
            .fnkbox_box,
            .scrollable_list,
            .fnkslist_element,
            .newcase,
            .area,
            .scrollable_list_inbetween,
            .testcase,
            .pill,
            .postit_text,
            .postit_drawing,
            => unreachable,
        };
    }

    pub fn grabsWithoutPlucking(lego: *const Lego) bool {
        return switch (lego.specific) {
            .button,
            .lens,
            .executor_crank,
            .executor_brake,
            .scrollbar,
            => true,
            .fnkbox,
            .sexpr,
            .garland,
            .case,
            .postit,
            .executor,
            .list_viewer,
            .meta_viewer,
            => false,
            .bubble,
            .bubble_connection,
            .scorer,
            .scorer_row,
            .scorer_rows,
            .fnkname_holder,
            .garland_newcases,
            .executor_controls,
            .microscope,
            .fnkbox_box,
            .editable_textline,
            .scrollable_list,
            .scrollable_list_inbetween,
            .fnkslist_element,
            .newcase,
            .area,
            .testcase,
            .pill,
            .postit_text,
            .postit_drawing,
            => unreachable,
        };
    }

    pub fn localBoundingBoxThatContainsSelfAndAllChildren(lego: *const Lego) Bounds {
        return switch (lego.specific) {
            else => .infinite,
            .sexpr => .fromRect(.fromCenterAndSize(.zero, .new(5, 2.5))),
            .testcase => .fromRect(Specific.Testcase.relative_bounding_box),
            .scrollable_list => |scrollable_list| if (scrollable_list.clip()) .fromRect(scrollable_list.rect()) else .infinite,
            .fnkbox => Bounds.fromRect(Specific.FnkboxBox.relative_box)
                // for the garland
                .plusMargin3(.bottom, std.math.inf(f32))
                .plusMargin3(.right, std.math.inf(f32)),
        };
    }

    pub fn visualOffsetGoal(lego: *const Lego) Point {
        return switch (lego.specific) {
            .sexpr => |sexpr| blk: {
                const base_point: Point = if (!sexpr.is_pattern)
                    .{ .turns = math.remap(sexpr.is_pattern_t, 0.5, 0, -0.25, 0) }
                else
                    .{ .turns = math.remap(sexpr.is_pattern_t, 0.5, 1, 0.25, 0) };

                const hovered_point = base_point.applyToLocalPoint(.lerp(.{}, .lerp(
                    .{ .turns = -0.01, .pos = .new(0.25, 0) },
                    .{ .turns = 0.01, .pos = .new(-0.25, 0) },
                    sexpr.is_pattern_t,
                ), lego.hot_t + lego.dropping_t * 2));

                break :blk hovered_point.plusTurns(sexpr.jiggling_t);
            },
            else => .{},
        };
    }
};

pub const ApiFor = struct {
    pub const Executor = struct {
        index: Lego.Index,

        pub fn garland(this: @This()) Garland {
            return .{ .index = this.index.children(.executor).garland };
        }
    };

    pub const Garland = struct {
        index: Lego.Index,
    };
};

pub const Handle = struct {
    point: Point,
    kind: Kind,
    hot_t: f32,
    enabled: bool,

    pub const Kind = union(enum) {
        circle: Size,
        fnkbox_tab,

        pub const default: Kind = .{ .circle = .{ .base = 0.2, .hot = 0.24, .hitbox = 0.24 } };
        pub const default_extrahitbox: Kind = .{ .circle = .{ .base = 0.2, .hot = 0.24, .hitbox = 1.0 } };
        pub const new_case: Kind = .{ .circle = .{ .base = 0.1, .hot = 0.4, .hitbox = 1.75 } };
        pub const garland: Kind = .{ .circle = .{ .base = 0.3, .hot = 0.5, .hitbox = 1.0 } };
        pub const lens: Kind = .{ .circle = .{ .base = 0.1, .hot = 0.2, .hitbox = 0.2 } };

        // pub const fnkbox_tab_rect: Rect = .{ .top_left = .new(0, 0), .size = .new(Lego.Specific.FnkboxBox.relative_box.size.x / 2, 1) };
        pub const fnkbox_tab_rect: Rect = Lego.Specific.FnkboxBox.relative_box
            .moveRelative(.new(0, -1))
            .withSize1d(.height, 1, .bottom_center)
            .scaleNonUniform(.new(0.5, 1), .bottom_right);
    };

    pub const Size = extern struct {
        base: f32,
        hot: f32,
        hitbox: f32,

        pub fn scale(original: Size, a: f32) Size {
            return .{
                .base = original.base * a,
                .hot = original.hot * a,
                .hitbox = original.hitbox * a,
            };
        }
    };

    pub fn draw(handle: *const Handle, drawer: *Drawer, camera: Rect, alpha: f32) !void {
        if (handle.enabled) {
            switch (handle.kind) {
                .circle => |radius| {
                    const r = std.math.lerp(radius.base, radius.hot, handle.hot_t);
                    drawer.canvas.fillCircleV3(camera, .{ .center = handle.point.pos, .radius = handle.point.scale * r }, COLORS.bg.withAlpha(alpha), .low);
                    drawer.canvas.strokeCircle(9, camera, handle.point.pos, handle.point.scale * r, 0.05 * handle.point.scale, .blackAlpha(alpha));
                },
                // TODO(game): improve
                .fnkbox_tab => {
                    const rect = handle.point.applyToLocalRect(Kind.fnkbox_tab_rect.move(.new(-0.05 / 2.0, 0.05 / 2.0)));
                    // TODO(polish): remove this ugly line
                    drawer.canvas.line(camera, &.{
                        rect.get(.bottom_left),
                        rect.get(.bottom_right),
                    }, 0.05 * handle.point.scale, COLORS.bg.withAlpha(alpha));

                    const rl = Kind.fnkbox_tab_rect; // rectlocal
                    drawer.canvas.fillShape(camera, handle.point, .{
                        .fill_shape_renderable = null,
                        .fill_atom_renderable = null,
                        .local_points = &.{
                            rl.get(.bottom_left),
                            rl.get(.top_left).addX(rl.size.y),
                            rl.get(.top_right),
                            rl.get(.bottom_right),
                        },
                        .triangles = &.{
                            .{ 0, 1, 3 },
                            .{ 1, 2, 3 },
                        },
                    }, COLORS.bg.withAlpha(alpha * 0.65));
                    drawer.canvas.line(camera, &.{
                        rect.get(.bottom_left),
                        rect.get(.top_left).addX(rect.size.y),
                        rect.get(.top_right),
                        rect.get(.bottom_right),
                    }, 0.05 * handle.point.scale, .blackAlpha(alpha));
                },
            }
        }
    }

    pub fn overlapped(handle: *const Handle, pos: Vec2) bool {
        return handle.enabled and switch (handle.kind) {
            .circle => |radius| handle.point.inRange(pos, radius.hitbox),
            .fnkbox_tab => handle.point.applyToLocalRect(Kind.fnkbox_tab_rect).contains(pos),
        };
    }
};

const TextManipulation = struct {
    selection: *TextSelection,
    text: *std.ArrayListUnmanaged(u8),
    alloc_text: std.mem.Allocator,
    cursor_points: *std.ArrayListUnmanaged(CursorPoint),
    alloc_cursor_points: std.mem.Allocator,

    pub const Jump = enum { one, word, full };

    fn moved(edit: TextManipulation, start: usize, direction: enum { left, right }, amount: Jump) usize {
        switch (amount) {
            .one => return switch (direction) {
                .left => start -| 1,
                .right => @min(start + 1, edit.cursor_points.items.len -| 1),
            },
            .full => return switch (direction) {
                .left => 0,
                .right => edit.cursor_points.items.len -| 1,
            },
            .word => {
                // based on raddbg's ui_scanned_column_from_column
                var found_text = false;
                var found_non_space = false;
                var cur = start;
                while (true) {
                    if (direction == .left and cur == 0) return cur;
                    if (direction == .right and cur == edit.cursor_points.items.len -| 1) return cur;
                    const next = edit.moved(cur, direction, .one);
                    const codepoint = edit.textBetween(cur, next);
                    assert(codepoint.len > 0);
                    const is_non_space = !(codepoint.len == 1 and std.ascii.isWhitespace(codepoint[0]));
                    const is_name = (codepoint.len > 1 or std.ascii.isAlphanumeric(codepoint[0]));
                    if (found_non_space and !is_non_space) return cur;
                    if (found_text and !is_name) return cur;
                    if (is_name) found_text = true;
                    if (is_non_space) found_non_space = true;
                    cur = next;
                }
            },
        }
    }

    fn textBetween(edit: TextManipulation, a: usize, b: usize) []const u8 {
        const start = @min(a, b);
        const end = @max(a, b);
        const start_byte = edit.cursor_points.items[start].index;
        const end_byte = edit.cursor_points.items[end].index;
        return edit.text.items[start_byte..end_byte];
    }

    pub fn left(edit: TextManipulation, extend: bool, jump: Jump) void {
        edit.selection.cursor = edit.moved(edit.selection.cursor, .left, jump);
        if (!extend) edit.selection.anchor = edit.selection.cursor;
    }

    pub fn right(edit: TextManipulation, extend: bool, jump: Jump) void {
        edit.selection.cursor = edit.moved(edit.selection.cursor, .right, jump);
        if (!extend) edit.selection.anchor = edit.selection.cursor;
    }

    fn delete(edit: TextManipulation, start: usize, end: usize) void {
        const start_byte = edit.cursor_points.items[start].index;
        const end_byte = edit.cursor_points.items[end].index;
        edit.text.replaceRangeAssumeCapacity(start_byte, end_byte - start_byte, &.{});
        edit.cursor_points.replaceRangeAssumeCapacity(start, end - start, &.{});
        for (edit.cursor_points.items[start..]) |*dst| {
            dst.index -= end_byte - start_byte;
        }
        if (edit.selection.anchor >= end) edit.selection.anchor -= end - start;
        if (edit.selection.cursor >= end) edit.selection.cursor -= end - start;
    }

    pub fn backspace(edit: TextManipulation, jump: Jump) void {
        const start: usize, const end: usize = if (edit.selection.empty())
            .{ edit.moved(edit.selection.cursor, .left, jump), edit.selection.cursor }
        else
            .{ edit.selection.min(), edit.selection.max() };

        edit.delete(start, end);
    }

    pub fn supr(edit: TextManipulation, jump: Jump) void {
        const start: usize, const end: usize = if (edit.selection.empty())
            .{ edit.selection.cursor, edit.moved(edit.selection.cursor, .right, jump) }
        else
            .{ edit.selection.min(), edit.selection.max() };

        edit.delete(start, end);
    }

    pub fn insertCharacter(edit: TextManipulation, bytes: []const u8) !void {
        if (!edit.selection.empty()) {
            edit.delete(edit.selection.min(), edit.selection.max());
        }
        assert(edit.selection.empty());

        try edit.text.insertSlice(
            edit.alloc_text,
            edit.cursor_points.items[edit.selection.cursor].index,
            bytes,
        );
        edit.selection.* = .both(edit.selection.cursor + 1);

        // TODO(zig): compiler bug here, dup is undefined if we dont print it
        const dup = edit.cursor_points.items[edit.selection.cursor - 1];
        std.log.info("dup: {any}", .{dup});
        try edit.cursor_points.insert(edit.alloc_cursor_points, edit.selection.cursor, dup);
        for (edit.cursor_points.items[edit.selection.cursor..]) |*dst| {
            dst.index += bytes.len;
        }
    }

    const TestHelper = struct {
        text: std.ArrayListUnmanaged(u8) = .empty,
        selection: TextSelection = .both(0),
        cursor_points: std.ArrayListUnmanaged(CursorPoint) = .empty,
        allocator: std.mem.Allocator,

        /// in state, | means cursor and !
        fn init(gpa: std.mem.Allocator, state: []const u8) !TestHelper {
            try std.testing.expect(std.mem.count(u8, state, ".") == 1);
            try std.testing.expect(std.mem.count(u8, state, ",") == 1);

            var result: TestHelper = .{ .allocator = gpa };
            try result.cursor_points.ensureUnusedCapacity(gpa, state.len);
            try result.text.ensureUnusedCapacity(gpa, state.len);

            var utf8 = (try std.unicode.Utf8View.init(state)).iterator();

            var text_index: usize = 0;
            var points_index: usize = 0;

            result.cursor_points.appendAssumeCapacity(.{ .index = text_index, .relative_pos = .zero, .relative_height = 0 });
            while (utf8.nextCodepointSlice()) |codepoint| {
                if (std.mem.eql(u8, codepoint, ".")) {
                    result.selection.anchor = points_index;
                } else if (std.mem.eql(u8, codepoint, ",")) {
                    result.selection.cursor = points_index;
                } else {
                    text_index += codepoint.len;
                    points_index += 1;
                    result.cursor_points.appendAssumeCapacity(.{ .index = text_index, .relative_pos = .zero, .relative_height = 0 });
                    result.text.appendSliceAssumeCapacity(codepoint);
                }
            }

            return result;
        }

        fn deinit(helper: *TestHelper) void {
            helper.text.deinit(helper.allocator);
            helper.cursor_points.deinit(helper.allocator);
        }

        fn expectState(helper: TestHelper, state: []const u8) !void {
            var other: TestHelper = try .init(helper.allocator, state);
            defer other.deinit();

            try std.testing.expectEqualStrings(other.text.items, helper.text.items);
            try std.testing.expectEqualSlices(CursorPoint, other.cursor_points.items, helper.cursor_points.items);
            try std.testing.expectEqual(other.selection, helper.selection);
        }

        fn edit(helper: *TestHelper) TextManipulation {
            return .{
                .selection = &helper.selection,
                .text = &helper.text,
                .alloc_text = helper.allocator,
                .cursor_points = &helper.cursor_points,
                .alloc_cursor_points = helper.allocator,
            };
        }
    };

    test "basics" {
        var helper: TestHelper = try .init(std.testing.allocator, "he.,llo");
        defer helper.deinit();

        // validate the actual helper
        try helper.expectState("he.,llo");
        try helper.expectState("he,.llo");
        try std.testing.expectEqualStrings("hello", helper.text.items);
        try std.testing.expectEqualSlices(CursorPoint, &.{
            .{ .index = 0, .relative_pos = .zero, .relative_height = 0 },
            .{ .index = 1, .relative_pos = .zero, .relative_height = 0 },
            .{ .index = 2, .relative_pos = .zero, .relative_height = 0 },
            .{ .index = 3, .relative_pos = .zero, .relative_height = 0 },
            .{ .index = 4, .relative_pos = .zero, .relative_height = 0 },
            .{ .index = 5, .relative_pos = .zero, .relative_height = 0 },
        }, helper.cursor_points.items);

        helper.edit().supr(.one);
        try helper.expectState("he.,lo");

        helper.edit().backspace(.one);
        try helper.expectState("h.,lo");

        helper.edit().right(true, .one);
        try helper.expectState("h.l,o");

        helper.edit().backspace(.one);
        try helper.expectState("h.,o");

        helper.edit().right(true, .one);
        try helper.expectState("h.o,");

        helper.edit().right(true, .one);
        try helper.expectState("h.o,");

        helper.edit().left(true, .one);
        try helper.expectState("h.,o");

        helper.edit().right(false, .one);
        try helper.expectState("ho.,");

        helper.edit().left(true, .one);
        try helper.expectState("h,o.");

        helper.edit().left(true, .one);
        try helper.expectState(",ho.");

        helper.edit().backspace(.one);
        try helper.expectState(",.");

        helper.edit().backspace(.one);
        try helper.expectState(",.");

        helper.edit().supr(.one);
        try helper.expectState(",.");

        try helper.edit().insertCharacter("a");
        try helper.expectState("a.,");
    }

    test "jumps" {
        var helper: TestHelper = try .init(std.testing.allocator, "hello .,there you");
        defer helper.deinit();

        helper.edit().right(true, .word);
        try helper.expectState("hello .there, you");

        helper.edit().right(false, .one);
        try helper.expectState("hello there .,you");

        helper.edit().left(false, .word);
        try helper.expectState("hello ,.there you");
    }

    test "non-ascii" {
        var helper: TestHelper = try .init(std.testing.allocator, ".我,你");
        defer helper.deinit();

        // validate the actual helper
        try helper.expectState(".我,你");
        try std.testing.expectEqualStrings("我你", helper.text.items);
        try std.testing.expectEqual(1, helper.selection.cursor);
        try std.testing.expectEqualSlices(CursorPoint, &.{
            .{ .index = 0, .relative_pos = .zero, .relative_height = 0 },
            .{ .index = "我".len, .relative_pos = .zero, .relative_height = 0 },
            .{ .index = "我你".len, .relative_pos = .zero, .relative_height = 0 },
        }, helper.cursor_points.items);

        helper.edit().right(true, .one);
        try helper.expectState(".我你,");
    }
};

const CursorPoint = struct {
    /// the lowest corner of the cursor line
    relative_pos: Vec2,
    relative_height: f32,
    index: usize,
};

pub const Toybox = struct {
    // TODO(optim-late): maybe use a plain arraylist + a fancy arena thing
    all_legos: std.SegmentedList(Lego, 0),
    all_legos_arena: std.heap.ArenaAllocator,
    free_head: Lego.Index = .nothing,

    undo_stack: UndoStack,
    gpa_for_undo_stack: std.mem.Allocator,

    // TODO(optim-late): remove before release
    disable_creation: bool = false,

    gpa_for_private_arenas: std.mem.Allocator,
    private_arenas: std.AutoHashMapUnmanaged(Lego.Index, std.heap.ArenaAllocator) = .empty,

    pub fn init(dst: *Toybox, gpa: std.mem.Allocator) !void {
        // TODO(optim-late): tune this number
        const undo_stack_capacity = if (@import("builtin").is_test) 1_000 else 1_000_000;

        dst.* = .{
            .all_legos_arena = .init(gpa),
            .all_legos = .{},
            .gpa_for_private_arenas = gpa,
            .gpa_for_undo_stack = gpa,
            .undo_stack = try .init(gpa, undo_stack_capacity),
        };
        // TODO(optim-late): tweak this number
        try dst.all_legos.growCapacity(
            dst.all_legos_arena.allocator(),
            4096,
        );
    }

    pub fn deinit(self: *Toybox) void {
        self.private_arenas.deinit(self.gpa_for_private_arenas);
        self.all_legos.deinit(self.all_legos_arena.allocator());
        self.all_legos_arena.deinit();
        self.undo_stack.deinit(self.gpa_for_undo_stack);
    }

    pub fn OoM() noreturn {
        std.debug.panic("OoM", .{});
    }

    pub fn createWithChildren(local_point: Point, specific: Lego.Specific, children: []const Lego.Index) !Lego.Index {
        const lego = try new(local_point, specific, .new(@src()));
        for (children) |child| {
            addChildLast(lego, child);
        }
        return lego;
    }

    pub fn getArenaFor(index: Lego.Index) std.mem.Allocator {
        const gop = toybox.private_arenas.getOrPut(toybox.gpa_for_private_arenas, index) catch OoM();
        if (!gop.found_existing) gop.value_ptr.* = .init(toybox.gpa_for_private_arenas);
        return gop.value_ptr.allocator();
    }

    pub fn new(local_point: Point, specific: Lego.Specific, tag: Lego.CreationTag) !Lego.Index {
        if (toybox.disable_creation) std.debug.panic("nope", .{});
        const result: *Lego, const index: Lego.Index = if (ENABLE_REUSE and toybox.free_head != Lego.Index.nothing) blk: {
            const result_index = toybox.free_head;
            const result = Toybox.getUnsafe(result_index);
            assert(result.exists == false);
            toybox.free_head = result.free_next;
            assert(!toybox.free_head.exists());
            break :blk .{ result, result_index };
        } else blk: {
            if (toybox.all_legos.count() >= std.math.maxInt(i31)) OoM();
            const result = toybox.all_legos.addOne(toybox.all_legos_arena.allocator()) catch OoM();
            break :blk .{ result, .firstGen(@intCast(toybox.all_legos.count() - 1)) };
        };

        result.* = .{
            .index = index,
            .exists = true,
            .local_point = local_point,
            .absolute_point = local_point,
            .specific = specific,
            .created_at = tag,
        };
        toybox.undo_stack.append(.{ .destroy_floating = index });
        return index;
    }

    pub fn get(index: Lego.Index) *Lego {
        const result = getUnsafe(index);
        assert(result.exists);
        assert(result.index == index);
        return result;
    }

    pub fn getUnsafe(index: Lego.Index) *Lego {
        assert(index != Lego.Index.nothing);
        return toybox.all_legos.at(index.index);
    }

    pub fn safeGet(index: Lego.Index) ?*Lego {
        if (index == Lego.Index.nothing) return null;
        const result = getUnsafe(index);
        if (!result.exists) return null;
        assert(result.index == index);
        return result;
    }

    pub fn addChildLastWithoutChangingAbsPoint(parent: Lego.Index, new_child: Lego.Index) void {
        addChildLast(parent, new_child);
        changeCoordinates(new_child, .{}, parent.get().absolute_point);
    }

    pub fn addChildLastWithLocalPoint(local_point: Point, parent: Lego.Index, new_child: Lego.Index) void {
        new_child.get().local_point = local_point;
        addChildLast(parent, new_child);
    }

    // TODO(design): remove the old version
    pub fn addChildLastV2(local_point: ?Point, parent: Lego.Index, new_child: Lego.Index) void {
        if (local_point) |l| {
            addChildLast(parent, new_child);
            new_child.get().local_point = l;
        } else {
            addChildLastWithoutChangingAbsPoint(parent, new_child);
        }
    }

    pub fn addChildLast(parent: Lego.Index, new_child: Lego.Index) void {
        assert(parent != nothing);
        if (new_child == nothing) return;
        toybox.undo_stack.append(.{ .pop = new_child });
        const parent_tree = &Toybox.get(parent).tree;
        const child_tree = &Toybox.get(new_child).tree;
        assert(child_tree.isFloating());
        child_tree.parent = parent;
        child_tree.prev = parent_tree.last;
        child_tree.next = .nothing;
        if (parent_tree.last != nothing) {
            Toybox.get(parent_tree.last).tree.next = new_child;
        }
        parent_tree.last = new_child;
        if (parent_tree.first == nothing) {
            parent_tree.first = new_child;
        }
    }

    pub fn addChildFirst(parent: Lego.Index, new_child: Lego.Index) void {
        assert(parent != nothing);
        if (new_child == nothing) return;
        toybox.undo_stack.append(.{ .pop = new_child });
        const parent_tree = &Toybox.get(parent).tree;
        const child_tree = &Toybox.get(new_child).tree;
        assert(child_tree.isFloating());
        child_tree.parent = parent;
        child_tree.next = parent_tree.first;
        child_tree.prev = .nothing;
        if (parent_tree.first != nothing) {
            Toybox.get(parent_tree.first).tree.prev = new_child;
        }
        parent_tree.first = new_child;
        if (parent_tree.last == nothing) {
            parent_tree.last = new_child;
        }
    }

    pub fn isFloating(index: Lego.Index) bool {
        return Toybox.get(index).tree.isFloating();
    }

    pub fn destroyFloatingWithUndo(index: Lego.Index) void {
        Toybox.destroyFloating(index);
    }

    pub fn destroyFloating(index: Lego.Index) void {
        destroyFloatingInner(index, true);
    }

    pub fn destroyFloatingInner(index: Lego.Index, undo: bool) void {
        assert(Toybox.isFloating(index));
        // std.log.debug("destroying {d}", .{index.asI32()});
        // std.log.debug("old free head: {d}", .{toybox.free_head.asI32()});

        // TODO(optim): avoid recursion
        while (index.get().tree.first != nothing) {
            const child = index.get().tree.first;
            Toybox.popInner(child, undo);
            Toybox.destroyFloatingInner(child, undo);
        }

        if (undo) toybox.undo_stack.append(.{ .recreate_floating = Toybox.get(index).* });

        if (toybox.private_arenas.fetchRemove(index)) |kv| kv.value.deinit();

        const new_index: Lego.Index = .nextGen(index);
        const lego = Toybox.get(index);

        // special cases
        switch (lego.specific) {
            else => {},
            .sexpr => |*sexpr| {
                if (sexpr.emerging_value != nothing) {
                    destroyFloatingInner(sexpr.emerging_value, undo);
                }
            },
            .executor => |*executor| {
                if (executor.animation) |*animation| {
                    inline for (.{ animation.active_case, animation.garland_fnkname, animation.invoked_fnk }) |thing| {
                        if (thing != nothing) {
                            popInner(thing, undo);
                            destroyFloatingInner(thing, undo);
                        }
                    }
                }

                if (true) { // remove pills
                    var cur = executor.first_pill;
                    executor.first_pill = .nothing;
                    while (cur != nothing) {
                        const next = cur.get().specific.pill.next_pill;
                        Toybox.popInner(cur, undo);
                        destroyFloatingInner(cur, undo);
                        cur = next;
                    }
                }

                if (true) { // remove enqueued
                    var cur = executor.first_enqueued;
                    executor.first_enqueued = .nothing;
                    while (cur != nothing) {
                        const next = cur.get().specific.garland.next_enqueued;
                        Toybox.popInner(cur, undo);
                        Toybox.destroyFloatingInner(cur, undo);
                        cur = next;
                    }
                }
            },
        }

        lego.* = undefined;
        lego.index = new_index;
        lego.exists = false;
        lego.free_next = toybox.free_head;
        assert(!toybox.free_head.exists());
        toybox.free_head = new_index;
        assert(!toybox.free_head.exists());
    }

    pub fn recreateFloating(data: Lego) void {
        assert(data.tree.isFloating());
        const lego = Toybox.getUnsafe(data.index);
        assert(!lego.exists);
        if (toybox.free_head == data.index) {
            toybox.free_head = lego.free_next;
            assert(!toybox.free_head.exists());
        }
        lego.* = data;
    }

    pub fn dupeIntoFloatingWithoutChangingPos(original: Lego.Index, tag: Lego.CreationTag) !Lego.Index {
        const result = try Toybox.dupeIntoFloating(original, tag);
        Toybox.get(result).local_point = Toybox.get(original).absolute_point;
        return result;
    }

    pub fn dupeIntoFloating(original: Lego.Index, tag: Lego.CreationTag) !Lego.Index {
        assert(original.get().exists);

        if (original.hasTag(.fnkbox)) {
            assert(!original.get().specific.fnkbox.editable);
        }

        const result_index = try Toybox.new(undefined, undefined, tag.plus(@src()));
        const result = Toybox.get(result_index);
        result.* = Toybox.get(original).*;
        result.index = result_index;
        result.tree.parent = .nothing;
        result.tree.next = .nothing;
        result.tree.prev = .nothing;
        result.free_next = .nothing;
        result.created_at = tag.plus(@src());

        // dupe children
        if (true) {
            var cur = result.tree.first;
            result.tree.first = .nothing;
            result.tree.last = .nothing;
            while (cur != nothing) : (cur = Toybox.get(cur).tree.next) {
                const new_child_index = try Toybox.dupeIntoFloating(cur, tag);
                Toybox.addChildLast(result_index, new_child_index);
            }
        }

        // special cases
        switch (result_index.get().specific) {
            else => {},
            .sexpr => |*sexpr| {
                if (sexpr.emerging_value != nothing) {
                    sexpr.emerging_value = try dupeIntoFloating(sexpr.emerging_value, tag);
                }
                // sexpr.emerging_value = .nothing;
                // sexpr.emerging_value_t = 0;
            },
            // .scrollbar => |*scrollbar| {
            //     // std.log.err("TODO: dupe scrollbars", .{});
            // },
        }

        return result_index;
    }

    pub fn getChildrenExact(comptime expected_count: usize, parent: Lego.Index) [expected_count]Lego.Index {
        var cur = Toybox.get(parent).tree.first;
        var result: [expected_count]Lego.Index = undefined;
        for (&result) |*dst| {
            assert(cur != nothing);
            dst.* = cur;
            cur = Toybox.get(cur).tree.next;
        }
        assert(cur == nothing);
        return result;
    }

    pub fn getFirstNChildren(comptime expected_count: usize, parent: Lego.Index) [expected_count]Lego.Index {
        var cur = Toybox.get(parent).tree.first;
        var result: [expected_count]Lego.Index = undefined;
        for (&result) |*dst| {
            assert(cur != nothing);
            dst.* = cur;
            cur = Toybox.get(cur).tree.next;
        }
        return result;
    }

    pub fn childCount(index: Lego.Index) usize {
        var count: usize = 0;
        var cur = Toybox.get(index).tree.first;
        while (cur != nothing) {
            count += 1;
            cur = Toybox.get(cur).tree.next;
        }
        return count;
    }

    pub fn getChildrenUnknown(allocator: std.mem.Allocator, parent: Lego.Index) ![]Lego.Index {
        const children_count: usize = Toybox.childCount(parent);
        const result = try allocator.alloc(Lego.Index, children_count);
        var cur = Toybox.get(parent).tree.first;
        for (result) |*dst| {
            assert(cur != nothing);
            dst.* = cur;
            cur = Toybox.get(cur).tree.next;
        }
        assert(cur == nothing);
        return result;
    }

    pub fn pop(child: Lego.Index) void {
        popInner(child, true);
    }

    pub fn popInner(child: Lego.Index, undo: bool) void {
        assert(!Toybox.isFloating(child));
        changeChildInner(child, .nothing, undo);
    }

    pub fn popWithUndo(child: Lego.Index) void {
        Toybox.pop(child);
    }

    pub fn popWithUndoAndChangingCoords(child: Lego.Index) void {
        const old_parent_abs_point = Toybox.parentAbsolutePoint(child);
        Toybox.pop(child);
        Toybox.changeCoordinates(child, old_parent_abs_point, .{});
    }

    pub fn insertAfter(to_be_inserted: Lego.Index, reference_sibling: Lego.Index) void {
        const sibling_tree = reference_sibling.get().tree;
        const element_tree = to_be_inserted.get().tree;
        return insert(to_be_inserted, .{
            .first = element_tree.first,
            .last = element_tree.last,
            .parent = sibling_tree.parent,
            .prev = reference_sibling,
            .next = sibling_tree.next,
        });
    }

    // TODO(code): change this to just take parent/siblings, not children
    pub fn insert(child: Lego.Index, where: Lego.Tree) void {
        return insertInner(child, where, true);
    }

    pub fn insertInner(child: Lego.Index, where: Lego.Tree, undo: bool) void {
        assert(Toybox.isFloating(child));
        assert(!where.isFloating());
        defer assert(Toybox.get(child).tree.equals(where));

        if (undo) toybox.undo_stack.append(.{ .pop = child });

        if (where.prev != nothing) {
            assert(Toybox.get(where.prev).tree.next == where.next);
            Toybox.get(where.prev).tree.next = child;
        } else {
            Toybox.get(where.parent).tree.first = child;
        }

        if (where.next != nothing) {
            assert(Toybox.get(where.next).tree.prev == where.prev);
            Toybox.get(where.next).tree.prev = child;
        } else {
            Toybox.get(where.parent).tree.last = child;
        }

        Toybox.get(child).tree = where;
    }

    pub fn changeChildWithUndo(original_child: Lego.Index, new_child: Lego.Index) void {
        Toybox.changeChild(original_child, new_child);
    }

    // TODO(design): remove
    pub fn changeChildWithUndoAndAlsoCoords(original_child: Lego.Index, new_child: Lego.Index) void {
        const old_parent_abs_point = Toybox.parentAbsolutePoint(original_child);
        Toybox.changeChild(original_child, new_child);
        Toybox.changeCoordinates(original_child, old_parent_abs_point, .{});
        Toybox.changeCoordinates(new_child, .{}, old_parent_abs_point);
    }

    pub fn changeChildAndDestroyOld(original_child: Lego.Index, new_child: Lego.Index) void {
        changeChild(original_child, new_child);
        destroyFloating(original_child);
    }

    /// things that pointed to original, now will point to new
    /// original will be left floating
    pub fn changeChild(original_child: Lego.Index, new_child: Lego.Index) void {
        changeChildInner(original_child, new_child, true);
    }

    pub fn changeChildInner(original_child: Lego.Index, new_child: Lego.Index, undo: bool) void {
        assert(original_child != nothing);
        assert(new_child == nothing or isFloating(new_child));
        defer assert(isFloating(original_child));

        if (true) {
            if (new_child == nothing) {
                const original_parent_tree = Toybox.get(original_child).tree;
                if (undo) toybox.undo_stack.append(.{
                    .insert = .{
                        .what = original_child,
                        .where = original_parent_tree,
                    },
                });
            } else {
                if (undo) toybox.undo_stack.append(.{ .change_child = .{
                    .original = new_child,
                    .new = original_child,
                } });
            }
        }

        const original_tree: Lego.Tree = get(original_child).tree;
        assert(original_tree.parent != nothing);
        const parent_tree: *Lego.Tree = &get(original_tree.parent).tree;
        if (parent_tree.first == original_child) {
            parent_tree.first = if (new_child != nothing) new_child else original_tree.next;
        }
        if (@hasField(Lego.Tree, "last")) {
            if (parent_tree.last == original_child) {
                parent_tree.last = if (new_child != nothing) new_child else original_tree.prev;
            }
        }
        if (original_tree.prev != nothing) {
            get(original_tree.prev).tree.next = if (new_child != nothing) new_child else original_tree.next;
        }
        if (original_tree.next != nothing) {
            get(original_tree.next).tree.prev = if (new_child != nothing) new_child else original_tree.prev;
        }
        if (new_child != nothing) {
            const new_child_tree = &get(new_child).tree;
            assert(new_child_tree.parent == nothing and
                new_child_tree.prev == nothing and
                new_child_tree.next == nothing);
            new_child_tree.parent = original_tree.parent;
            new_child_tree.next = original_tree.next;
            new_child_tree.prev = original_tree.prev;
        }
        get(original_child).tree.parent = .nothing;
        get(original_child).tree.next = .nothing;
        get(original_child).tree.prev = .nothing;
    }

    pub const VisitStep = struct {
        next: Lego.Index,
        // push_count: i32,
        // pop_count: i32,
    };

    /// root to leaf, from first to last child
    pub fn next_preordered(current: Lego.Index, root: Lego.Index) VisitStep {
        assert(root != nothing and current != nothing);
        var result: VisitStep = .{ .next = .nothing };
        // var result: VisitStep = .{ .next = .nothing, .pop_count = 0, .push_count = 0 };
        const cur = Toybox.get(current);
        if (cur.tree.first != nothing) {
            result.next = cur.tree.first;
            // result.push_count = 1;
        } else {
            var p = current;
            while (p != nothing and p != root) : (p = Toybox.get(p).tree.parent) {
                const next = Toybox.get(p).tree.next;
                if (next != nothing) {
                    result.next = next;
                    break;
                } else {
                    // result.pop_count += 1;
                }
            }
        }
        return result;
    }

    /// root to leaf, from last to first child
    pub fn next_postordered(current: Lego.Index, root: Lego.Index) VisitStep {
        assert(root != nothing and current != nothing);
        var result: VisitStep = .{ .next = .nothing };
        // var result: VisitStep = .{ .next = .nothing, .pop_count = 0, .push_count = 0 };
        const cur = Toybox.get(current);
        if (cur.tree.last != nothing) {
            result.next = cur.tree.last;
            // result.push_count = 1;
        } else {
            var p = current;
            while (p != nothing and p != root) : (p = Toybox.get(p).tree.parent) {
                const next = Toybox.get(p).tree.prev;
                if (next != nothing) {
                    result.next = next;
                    break;
                } else {
                    // result.pop_count += 1;
                }
            }
        }
        return result;
    }

    pub fn treeIterator(root: Lego.Index, first_to_last: bool) TreeIterator {
        return .{
            .root = root,
            .cur = root,
            .first_to_last = first_to_last,
        };
    }

    pub const TreeIterator = struct {
        root: Lego.Index,
        cur: Lego.Index,
        going_up: bool = false,
        first_to_last: bool,

        pub const Step = struct {
            index: Lego.Index,
            children_already_visited: bool,
        };

        pub fn next(it: *TreeIterator) ?Step {
            if (it.cur == nothing) return null;
            const result: Step = .{
                .children_already_visited = it.going_up,
                .index = it.cur,
            };
            const tree = Toybox.get(it.cur).tree;
            const child = if (it.first_to_last) tree.first else tree.last;
            const sibling = if (it.first_to_last) tree.next else tree.prev;

            if (it.going_up) {
                if (it.cur == it.root) {
                    it.cur = .nothing;
                } else if (sibling != nothing) {
                    it.cur = sibling;
                    it.going_up = false;
                } else {
                    it.cur = tree.parent;
                }
            } else {
                if (child != nothing) {
                    it.cur = child;
                } else {
                    it.going_up = true;
                }
            }
            return result;
        }

        pub fn skipChildren(it: *TreeIterator) void {
            if (!it.going_up) {
                it.cur = Toybox.get(it.cur).tree.parent;
                it.going_up = true;
            }
        }
    };

    test "iteration order" {
        try toybox.init(std.testing.allocator);
        defer toybox.deinit();
        const root = try Toybox.new(undefined, undefined, .new(@src()));
        const child_1 = try Toybox.new(undefined, undefined, .new(@src()));
        const child_2 = try Toybox.new(undefined, undefined, .new(@src()));
        const grandchild_1_1 = try Toybox.new(undefined, undefined, .new(@src()));
        const grandchild_1_2 = try Toybox.new(undefined, undefined, .new(@src()));
        const grandchild_2_1 = try Toybox.new(undefined, undefined, .new(@src()));
        const grandchild_2_2 = try Toybox.new(undefined, undefined, .new(@src()));

        Toybox.addChildLast(root, child_1);
        Toybox.addChildLast(root, child_2);

        Toybox.addChildLast(child_1, grandchild_1_1);
        Toybox.addChildLast(child_1, grandchild_1_2);

        Toybox.addChildLast(child_2, grandchild_2_1);
        Toybox.addChildLast(child_2, grandchild_2_2);

        try std.testing.expectEqual(child_1, Toybox.get(grandchild_1_1).tree.parent);

        if (true) {
            const expected_order: [7]VisitStep = .{
                .{ .next = root },
                .{ .next = child_1 },
                .{ .next = grandchild_1_1 },
                .{ .next = grandchild_1_2 },
                .{ .next = child_2 },
                .{ .next = grandchild_2_1 },
                .{ .next = grandchild_2_2 },
            };

            var actual_order: std.ArrayListUnmanaged(VisitStep) = try .initCapacity(std.testing.allocator, expected_order.len);
            defer actual_order.deinit(std.testing.allocator);

            var cur: VisitStep = .{ .next = root };
            while (cur.next != nothing) : (cur = Toybox.next_preordered(cur.next, root)) {
                try actual_order.append(std.testing.allocator, cur);
            }

            try std.testing.expectEqualSlices(VisitStep, &expected_order, actual_order.items);
        }

        if (true) {
            const expected_order: [14]TreeIterator.Step = .{
                .{ .children_already_visited = false, .index = root },
                .{ .children_already_visited = false, .index = child_1 },
                .{ .children_already_visited = false, .index = grandchild_1_1 },
                .{ .children_already_visited = true, .index = grandchild_1_1 },
                .{ .children_already_visited = false, .index = grandchild_1_2 },
                .{ .children_already_visited = true, .index = grandchild_1_2 },
                .{ .children_already_visited = true, .index = child_1 },
                .{ .children_already_visited = false, .index = child_2 },
                .{ .children_already_visited = false, .index = grandchild_2_1 },
                .{ .children_already_visited = true, .index = grandchild_2_1 },
                .{ .children_already_visited = false, .index = grandchild_2_2 },
                .{ .children_already_visited = true, .index = grandchild_2_2 },
                .{ .children_already_visited = true, .index = child_2 },
                .{ .children_already_visited = true, .index = root },
            };

            var actual_order: std.ArrayListUnmanaged(TreeIterator.Step) = try .initCapacity(std.testing.allocator, expected_order.len);
            defer actual_order.deinit(std.testing.allocator);

            var it = Toybox.treeIterator(root, true);
            while (it.next()) |step| {
                try actual_order.append(std.testing.allocator, step);
            }

            try std.testing.expectEqualSlices(TreeIterator.Step, &expected_order, actual_order.items);
        }

        if (true) {
            const expected_order: [10]TreeIterator.Step = .{
                .{ .children_already_visited = false, .index = root },
                .{ .children_already_visited = false, .index = child_1 },
                .{ .children_already_visited = true, .index = child_1 },
                .{ .children_already_visited = false, .index = child_2 },
                .{ .children_already_visited = false, .index = grandchild_2_1 },
                .{ .children_already_visited = true, .index = grandchild_2_1 },
                .{ .children_already_visited = false, .index = grandchild_2_2 },
                .{ .children_already_visited = true, .index = grandchild_2_2 },
                .{ .children_already_visited = true, .index = child_2 },
                .{ .children_already_visited = true, .index = root },
            };

            var actual_order: std.ArrayListUnmanaged(TreeIterator.Step) = try .initCapacity(std.testing.allocator, expected_order.len);
            defer actual_order.deinit(std.testing.allocator);

            var it = Toybox.treeIterator(root, true);
            while (it.next()) |step| {
                try actual_order.append(std.testing.allocator, step);
                if (step.index == child_1 and !step.children_already_visited) {
                    it.skipChildren();
                }
            }

            try std.testing.expectEqualSlices(TreeIterator.Step, &expected_order, actual_order.items);
        }

        if (true) {
            const expected_order: [10]TreeIterator.Step = .{
                .{ .children_already_visited = false, .index = root },
                .{ .children_already_visited = false, .index = child_2 },
                .{ .children_already_visited = false, .index = grandchild_2_2 },
                .{ .children_already_visited = true, .index = grandchild_2_2 },
                .{ .children_already_visited = false, .index = grandchild_2_1 },
                .{ .children_already_visited = true, .index = grandchild_2_1 },
                .{ .children_already_visited = true, .index = child_2 },
                .{ .children_already_visited = false, .index = child_1 },
                .{ .children_already_visited = true, .index = child_1 },
                .{ .children_already_visited = true, .index = root },
            };

            var actual_order: std.ArrayListUnmanaged(TreeIterator.Step) = try .initCapacity(std.testing.allocator, expected_order.len);
            defer actual_order.deinit(std.testing.allocator);

            var it = Toybox.treeIterator(root, false);
            while (it.next()) |step| {
                try actual_order.append(std.testing.allocator, step);
                if (step.index == child_1 and !step.children_already_visited) {
                    it.skipChildren();
                }
            }

            try std.testing.expectEqualSlices(TreeIterator.Step, &expected_order, actual_order.items);
        }
    }

    pub fn oldestAncestor(index: Lego.Index) Lego.Index {
        assert(index != nothing);
        var cur = index;
        while (true) {
            const next = Toybox.get(cur).tree.parent;
            if (next == nothing) return cur;
            cur = next;
        }
    }

    pub fn isAncestor(parent: Lego.Index, child: Lego.Index) bool {
        var cur = child;
        while (cur != nothing) {
            if (cur == parent) return true;
            cur = cur.get().tree.parent;
        }
        return false;
    }

    pub fn findAncestor(index: Lego.Index, kind: Lego.Specific.Tag) Lego.Index {
        assert(index != nothing);
        var cur = index;
        while (cur != nothing) {
            if (Toybox.get(cur).specific.tag() == kind) return cur;
            cur = Toybox.get(cur).tree.parent;
        }
        return .nothing;
    }

    pub fn isInATopLevelSexpr(index: Lego.Index) bool {
        assert(index != nothing);
        var cur = index;
        while (cur != nothing) {
            if (Toybox.get(cur).specific.tag() == .area) return true;
            if (Toybox.get(cur).specific.tag() != .sexpr) return false;
            cur = Toybox.get(cur).tree.parent;
        }
        return true;
    }

    pub fn parentAbsolutePoint(index: Lego.Index) Point {
        assert(index != nothing);
        const parent = Toybox.get(index).tree.parent;
        if (parent == nothing) return .{};
        return Toybox.get(parent).absolute_point;
    }

    pub fn refreshAbsolutePoints(roots: []const Lego.Index) void {
        const zone = tracy.initZone(@src(), .{ .name = "refresh absolute points" });
        defer zone.deinit();

        for (roots) |root| {
            var cur: Lego.Index = root;
            while (cur != nothing) {
                const skip = switch (cur.get().specific) {
                    else => false,
                    .area => |area| area.non_interactable,
                    .executor => |executor| executor.used_for_bg_computation,
                };
                if (skip) {
                    cur = cur.get().tree.next;
                } else {
                    Toybox.get(cur).absolute_point = parentAbsolutePoint(cur)
                        .applyToLocalPoint(Toybox.get(cur).local_point)
                        .applyToLocalPoint(Toybox.get(cur).visual_offset);
                    cur = next_preordered(cur, root).next;
                }
            }
        }
    }

    pub fn changeCoordinates(index: Lego.Index, old_parent: Point, new_parent: Point) void {
        Toybox.get(index).local_point = new_parent.inverseApplyGetLocal(old_parent.applyToLocalPoint(Toybox.get(index).local_point));
        Toybox.refreshAbsolutePoints(&.{index});
    }

    pub fn setAbsolutePoint(index: Lego.Index, abs_point: Point) void {
        Toybox.get(index).local_point = Toybox.parentAbsolutePoint(index).inverseApplyGetLocal(abs_point);
        Toybox.refreshAbsolutePoints(&.{index});
    }

    pub fn buildSexprFromText(local_point: Point, text: []const u8, is_pattern: bool, is_fnkname: bool, tag: Lego.CreationTag) !Lego.Index {
        const remaining = std.mem.trim(u8, text, &std.ascii.whitespace);
        if (remaining[0] == '(') {
            const data = try parsing.extractFromPair(remaining);
            const up = try buildSexprFromText(.{}, data.up, is_pattern, is_fnkname, tag.plus(@src()));
            const down = try buildSexprFromText(.{}, data.down, is_pattern, is_fnkname, tag.plus(@src()));
            return try buildSexpr(local_point, .{ .pair = .{ .up = up, .down = down } }, is_pattern, is_fnkname, tag.plus(@src()));
        } else {
            if (std.mem.eql(u8, remaining, "<empty>")) {
                return try buildSexpr(local_point, .empty, is_pattern, is_fnkname, tag.plus(@src()));
            } else if (remaining[0] == '@') {
                return try buildSexpr(local_point, .{ .atom_var = remaining[1..] }, is_pattern, is_fnkname, tag.plus(@src()));
            } else {
                return try buildSexpr(local_point, .{ .atom_lit = remaining }, is_pattern, is_fnkname, tag.plus(@src()));
            }
        }
    }

    pub fn buildSexpr(local_point: Point, value: union(Lego.Specific.Sexpr.Kind) {
        empty,
        atom_lit: []const u8,
        atom_var: []const u8,
        pair: struct { up: Lego.Index, down: Lego.Index },
    }, is_pattern: bool, is_fnkname: bool, tag: Lego.CreationTag) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .sexpr = .{
            .is_pattern = is_pattern,
            .is_pattern_t = if (is_pattern) 1 else 0,
            .is_fnkname = is_fnkname,
            .is_fnkname_t = if (is_fnkname) 1 else 0,
            .atom_name = switch (value) {
                .atom_lit, .atom_var => |v| v,
                else => undefined,
            },
            .kind = value,
        } }, tag.plus(@src()));
        switch (value) {
            else => {},
            .pair => |pair| {
                Toybox.addChildLastV2(ViewHelper.offsetFor(is_pattern, .up), result, pair.up);
                Toybox.addChildLastV2(ViewHelper.offsetFor(is_pattern, .down), result, pair.down);
            },
        }
        return result;
    }

    pub fn buildCase(local_point: Point, data: struct {
        pattern: Lego.Index,
        template: Lego.Index,
        fnkname: ?Lego.Index,
        next: ?Lego.Index,
    }, tag: Lego.CreationTag) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .case = .{} }, tag.plus(@src()));
        if (data.fnkname) |f| assert(f.hasTag(.sexpr));
        Toybox.addChildLastV2(.{ .pos = .xneg }, result, data.pattern);
        Toybox.addChildLastV2(.{ .pos = .xpos }, result, data.template);
        Toybox.addChildLastV2(.{}, result, try Lego.Specific.FnknameHolder.build(
            data.fnkname orelse try Toybox.buildSexpr(.{}, .empty, false, true, tag.plus(@src())),
        ));
        Toybox.addChildLastV2(.{ .pos = .new(8, 1) }, result, data.next orelse try Toybox.buildGarland(local_point, &.{}, tag.plus(@src())));
        return result;
    }

    /// The garland's children are a linear list of newcase, all except the last one with a child case
    /// the newcase position is the very top of the segment
    pub fn buildGarland(local_point: Point, child_cases: []const Lego.Index, tag: Lego.CreationTag) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .garland = .{} }, tag.plus(@src()));
        Toybox.addChildLast(result, try buildSexpr(
            Lego.Specific.Garland.relative_fnkname_point,
            .empty,
            true,
            true,
            tag,
        ));

        const cases_holder = try Toybox.new(.{}, .garland_newcases, tag.plus(@src()));
        for (child_cases) |case| {
            const new_segment = try Toybox.new(.{}, .{ .newcase = .{} }, tag.plus(@src()));
            Toybox.addChildLast(new_segment, case);
            Toybox.addChildLast(cases_holder, new_segment);
        }
        Toybox.addChildLast(cases_holder, try Toybox.new(.{}, .{ .newcase = .{} }, tag.plus(@src())));

        Toybox.addChildLast(result, cases_holder);
        return result;
    }

    pub fn buildTestcase(kind: union(enum) {
        unloaded: Lego.Specific.Testcase.Source,
        existing: struct { input: Lego.Index, expected: Lego.Index, unloaded: ?Lego.Specific.Testcase.Source },
    }, tag: Lego.CreationTag) !Lego.Index {
        const testcase = try Toybox.new(.{}, .{ .testcase = .{ .source = switch (kind) {
            .unloaded => |s| s,
            .existing => |s| s.unloaded,
        }, .loaded = std.meta.activeTag(kind) != .unloaded } }, tag.plus(@src()));
        const Testcase = Lego.Specific.Testcase;
        Toybox.addChildLastV2(Testcase.relative_input_point, testcase, switch (kind) {
            .unloaded => try Toybox.buildSexpr(.{}, .empty, false, false, tag.plus(@src())),
            .existing => |e| e.input,
        });
        Toybox.addChildLastV2(Testcase.relative_expected_point, testcase, switch (kind) {
            .unloaded => try Toybox.buildSexpr(.{}, .empty, false, false, tag.plus(@src())),
            .existing => |e| e.expected,
        });
        Toybox.addChildLastV2(Testcase.relative_actual_point, testcase, try Toybox.buildSexpr(.{}, .empty, false, false, tag.plus(@src())));
        Toybox.addChildLast(testcase, try Toybox.new(.{}, .{ .button = .{
            .local_rect = .fromCenterAndSize(.new(-6, 0), .one),
            .action = .launch_testcase,
        } }, tag.plus(@src())));
        return testcase;
    }

    pub fn buildFnkboxFromLevel(
        local_point: Point,
        fnkname: Lego.Index,
        level_index: usize,
        editable: bool,
        scratch: std.mem.Allocator,
        atomnames_allocator: std.mem.Allocator,
    ) !Lego.Index {
        var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);
        defer pool.deinit();

        const level = levels[level_index];

        const samples: []const Lego.Index = blk: {
            var samples: std.ArrayListUnmanaged(Lego.Index) = .empty;
            try samples.ensureUnusedCapacity(scratch, 100);
            var sample_index: usize = 0;
            while (try level.generate_sample(sample_index, &pool, scratch, atomnames_allocator)) |sample| {
                try samples.append(scratch, try Toybox.buildTestcase(.{ .unloaded = .build(
                    level_index,
                    sample_index,
                    sample,
                ) }, .new(@src())));
                sample_index += 1;
                _ = pool.reset(.retain_capacity);
            }
            break :blk try samples.toOwnedSlice(scratch);
        };

        const result = try Toybox.buildFnkbox(
            local_point,
            fnkname,
            editable,
            level.description,
            samples,
            if (try level.initialDefinition(&pool, scratch)) |definition|
                try Lego.Specific.Garland.buildFromOldCoreValue(.{}, definition, scratch, .new(@src()))
            else
                null,
        );
        result.get().specific.fnkbox.require_manual_execution = level.require_manual_execution;

        return result;
    }

    /// Children are:
    /// - box with (description area, status bar, testcases scroll bar, testcases area)
    /// - fnkname
    /// - executor
    pub fn buildFnkbox(
        local_point: Point,
        // TODO(design): take *const Sepxr
        fnkname: Lego.Index,
        editable: bool,
        text: []const u8,
        /// must be Testcase or UnloadedTestcase
        testcases: []const Lego.Index,
        initial_definition: ?Lego.Index,
    ) !Lego.Index {
        const Fnkbox = Lego.Specific.Fnkbox;
        const FnkboxBox = Lego.Specific.FnkboxBox;

        const scrollbar =
            try createWithChildren(.{}, .{ .scrollbar = .buildForTestcases(testcases.len, 0) }, &.{
                try new(.{}, .{ .button = .{
                    .local_rect = FnkboxBox.testcases_box.withSize(.new(0.7, 0.7), .top_left).plusMargin(-0.1),
                    .action = .scroll_up,
                } }, .new(@src())),
                try new(.{}, .{ .button = .{
                    .local_rect = FnkboxBox.testcases_box.withSize(.new(0.7, 0.7), .bottom_left).plusMargin(-0.1),
                    .action = .scroll_down,
                } }, .new(@src())),
            });

        const box = try Toybox.createWithChildren(.{}, .{ .fnkbox_box = .{} }, &.{
            try Lego.Specific.EditableTextline.new(text, .fnkbox_description),
            try new(.{}, .{ .button = .{
                .local_rect = FnkboxBox.status_bar_goal,
                .action = .see_failing_testcase,
            } }, .new(@src())),
            scrollbar,
            blk: {
                // TODO(now)
                const fnkbox_testcases = try Toybox.new(.{}, .{ .scrollable_list = .{ .kind = .fnkbox_testcases } }, .new(@src()));
                for (testcases) |testcase| {
                    assert(isFloating(testcase));
                    Toybox.addChildLast(fnkbox_testcases, testcase);
                }
                Toybox.addChildLast(fnkbox_testcases, try Toybox.new(.{}, .{ .button = .{
                    .local_rect = .fromCenterAndSize(.zero, .one),
                    .action = .add_testcase,
                } }, .new(@src())));
                break :blk fnkbox_testcases;
            },
        });

        fnkname.get().local_point = Fnkbox.relative_fnkname_point;
        fnkname.get().immutable = true;
        fnkname.get().specific.sexpr.is_pattern = true;
        fnkname.get().specific.sexpr.is_fnkname = true;

        const executor = try buildExecutor(Fnkbox.relative_executor_point, true, false, null, initial_definition);

        if (fnkname.isTheSexprLit("swap")) {
            executor.children(.executor).controls.children(.executor_controls).brake.get().specific.executor_brake.brake_t = 0.9;
        }

        return try Toybox.createWithChildren(local_point, .{ .fnkbox = .{ .status = undefined, .editable = editable } }, &.{
            box,
            fnkname,
            executor,
        });
    }

    pub fn buildExecutor(
        point: Point,
        controlled_by_parent_fnkbox: bool,
        used_for_bg_computation: bool,
        initial_value: ?Lego.Index,
        initial_definition: ?Lego.Index,
    ) !Lego.Index {
        const Executor = Lego.Specific.Executor;
        if (initial_definition) |d| d.get().local_point = Executor.relative_garland_point;
        return try Toybox.createWithChildren(point, .{
            .executor = .{ .controlled_by_parent_fnkbox = controlled_by_parent_fnkbox, .used_for_bg_computation = used_for_bg_computation },
        }, &.{
            initial_value orelse try Toybox.buildSexpr(Executor.relative_input_point, .empty, false, false, .new(@src())),
            initial_definition orelse try Toybox.buildGarland(Executor.relative_garland_point, &.{}, .new(@src())),
            blk: {
                const controls = try Toybox.new(Executor.relative_crank_center, .executor_controls, .new(@src()));
                Toybox.addChildLast(controls, try Toybox.new(.{}, .{ .button = .{
                    .action = .stop_execution,
                    .local_rect = .fromCenterAndSize(.new(0, 2), .both(0.6)),
                } }, .new(@src())));
                Toybox.addChildLast(controls, try Toybox.new(.{}, .{ .executor_brake = .{ .brake_t = 0.5 } }, .new(@src())));
                Toybox.addChildLast(controls, try Toybox.new(.{}, .{ .executor_crank = .{ .value = 0.0 } }, .new(@src())));
                break :blk controls;
            },
        });
    }

    pub fn buildMicroscope(source: Vec2, target: Vec2, in_toolbar: bool) !Lego.Index {
        const lens_source = try Toybox.new(.{ .pos = source }, .{ .lens = .source }, .new(@src()));
        const lens_target = try Toybox.new(.{ .pos = target }, .{ .lens = .target }, .new(@src()));
        const result = try Toybox.new(.{}, .{ .microscope = .{ .in_toolbar = in_toolbar } }, .new(@src()));
        Toybox.addChildLast(result, lens_source);
        Toybox.addChildLast(result, lens_target);
        return result;
    }

    pub fn buildListViewer(point: Point, value: ?Lego.Index) !Lego.Index {
        const scrollbar = Lego.Specific.Scrollbar.build(
            .fromCenterAndSize(.new(6.5, 0), .new(0.5, 5)),
            0,
            4,
        );

        return try createWithChildren(point, .{ .list_viewer = .{} }, &.{
            value orelse try buildSexpr(.{ .scale = 2 }, .empty, false, false, .new(@src())),
            scrollbar,
            try Toybox.new(.{}, .{
                .scrollable_list = .{
                    .kind = .listviewer_sexprs,
                },
            }, .new(@src())),
            try buildSexpr(.{ .pos = .new(5.5, 3.25), .scale = 0.5 }, .empty, false, false, .new(@src())),
        });
    }

    pub fn buildMetaViewer(point: Point) !Lego.Index {
        return try createWithChildren(point, .{ .meta_viewer = .{} }, &.{
            try buildSexpr(.{ .scale = 2 }, .empty, false, false, .new(@src())),
            try buildGarland(.{ .pos = .new(2, 3) }, &.{}, .new(@src())),
        });
    }

    pub fn buildScorer(point: Point, levels_indices: []const usize, create_at_offsets: []const ?Vec2) !Lego.Index {
        const rows_holder = try Toybox.new(.{}, .scorer_rows, .new(@src()));
        var y: f32 = 0;
        const magic_id = if (levels_indices.len == 1)
            hashString(levels[levels_indices[0]].fnk_name)
        else
            @panic("TODO");
        for (levels_indices, create_at_offsets) |level_index, offset| {
            const new_row = try Toybox.createWithChildren(.{ .pos = .new(0, y) }, .{ .scorer_row = .{
                .level_index = level_index,
                .offset = offset,
                .magic_id = magic_id,
            } }, &.{
                try Toybox.new(.{}, .{ .button = .{ .local_rect = .fromCenterAndSize(.zero, .one), .action = .create_fnkbox_for_row } }, .new(@src())),
                try buildSexpr(.{ .pos = .new(0, -0.5), .scale = 0.5, .turns = 0.25 }, .empty, false, true, .new(@src())),
            });
            Toybox.addChildLast(rows_holder, new_row);
            y += 2;
        }
        return try createWithChildren(point, .{ .scorer = .{} }, &.{
            rows_holder,
        });
    }

    pub fn buildBubble(point: Point, prev: Lego.Index, goal: Lego.Specific.Bubble.FulfillCondition, blueprint: Lego.Index) !Lego.Index {
        assert(blueprint.hasTag(.area));
        const instanced = try Toybox.dupeIntoFloating(blueprint, .new(@src()));
        return try Toybox.createWithChildren(point, .{ .bubble = .{
            .blueprint = blueprint,
            .prev_bubble = prev,
            .goal = goal,
        } }, &.{
            instanced,
            try Toybox.new(.{ .pos = blueprint.get().specific.area.bg.local_rect.top_left }, .{ .button = .{
                .local_rect = .fromCenterAndSize(.zero, .one),
                .action = .reset_bubble,
            } }, .new(@src())),
            try Toybox.new(.{ .pos = blueprint.get().specific.area.bg.local_rect.get(.top_center) }, .{ .button = .{
                .local_rect = .fromCenterAndSize(.zero, .new(2.5, 1.5)),
                .action = .unlock_hint,
            } }, .new(@src())),
        });
    }

    pub fn buildBubbleConnection(source: Lego.Index, target: Lego.Index) !Lego.Index {
        return try Toybox.new(.{}, .{ .bubble_connection = .{
            .source = source,
            .target = target,
        } }, .new(@src()));
    }

    pub fn setLocalPointSmooth(index: Lego.Index, new_local_point: Point) void {
        const current = index.get().local_point.applyToLocalPoint(index.get().visual_offset);
        const new_visual_offset = Point.inverseApplyGetLocal(new_local_point, current);
        assert(current.equalsAbs(new_local_point.applyToLocalPoint(new_visual_offset), 0.001));
        index.get().local_point = new_local_point;
        index.get().visual_offset = new_visual_offset;
    }
};

pub const UndoStack = struct {
    // TODO(optim-late): use a fancy arena thing
    commands: kommon.RingBuffer(Workspace.UndoableCommand),
    last_frame_command_count: usize = 0,

    pub fn init(allocator: std.mem.Allocator, capacity: usize) !UndoStack {
        return .{ .commands = .{ .data = try allocator.alloc(Workspace.UndoableCommand, capacity) } };
    }

    pub fn deinit(self: *UndoStack, allocator: std.mem.Allocator) void {
        allocator.free(self.commands.data);
    }

    pub fn removeUntilNextFenceOrEmpty(self: *UndoStack) void {
        var popped_at_least_once = false;
        while (self.commands.peekFirst()) |c| {
            if (popped_at_least_once and c == .fence) return;
            _ = self.commands.shift().?;
            popped_at_least_once = true;
        }
    }

    pub fn append(self: *UndoStack, command: Workspace.UndoableCommand) void {
        self.commands.push(command) catch |err| switch (err) {
            error.Full => {
                self.removeUntilNextFenceOrEmpty();
                self.commands.push(command) catch unreachable;
            },
            else => Toybox.OoM(),
        };
    }

    pub fn storeAllData(self: *UndoStack, index: Lego.Index) void {
        self.append(.{ .set_data_except_tree = Toybox.get(index).* });
    }

    pub fn pop(self: *UndoStack) ?Workspace.UndoableCommand {
        return self.commands.pop();
    }

    pub fn startFrame(self: *UndoStack) void {
        self.last_frame_command_count = self.commands.len();
    }

    pub fn anyChangesThisFrame(self: *UndoStack) bool {
        return self.commands.len() != self.last_frame_command_count;
    }
};

const Workspace = struct {
    main_area: Lego.Index,
    toolbar_left: Lego.Index,
    toolbar_left_unfolded_t: f32 = 0,
    toolbar_fnks: Lego.Index,
    toolbar_fnks_unfolded_t: f32 = 0,
    lenses_layer: Lego.Index,
    floating_inputs_layer: Lego.Index,
    invisible_floating_inputs_layer: Lego.Index,
    hand_layer: Lego.Index = .nothing,

    grabbing: Grabbing = .nothing,
    active_text_input: Lego.Index = .nothing,
    active_text_selection: Canvas.TextSelection = undefined,

    did_first_frame: bool = false,

    random_instance: std.Random.DefaultPrng,
    arena_for_atom_names: std.heap.ArenaAllocator,
    /// resets after interaction but before springs and drawing
    arena_for_oneframe_data: std.heap.ArenaAllocator,

    // TODO(design): remove
    gpa_for_bindings: std.mem.Allocator,
    // TODO(design): remove
    gpa_for_atom_names: std.mem.Allocator,
    gpa_for_big_buffers: std.mem.Allocator,

    display_fps: bool = false,
    debug_nodraw: bool = false,
    debug_all_bubbles_unlocked: bool = false,

    /// toolbar element unlocks when the bubble is unlocked, not fulfilled
    toolbar_unlocks: struct {
        case_with_wildcards: Lego.Index = .nothing,
        list_viewer: Lego.Index = .nothing,
        meta_viewer: Lego.Index = .nothing,
        lenses: Lego.Index = .nothing,
    } = .{},

    pub const Grabbing = struct {
        index: Lego.Index,
        offset: Vec2,

        pub const nothing: Grabbing = .{ .index = .nothing, .offset = .zero };
    };

    pub const toolbar_left_rect: Rect = .{ .top_left = .zero, .size = .new(6, 15) };
    pub const toolbar_fnks_rect: Rect = .{ .top_left = .zero, .size = .new(12, 15) };
    pub const toolbar_fnks_searchbox_height = 1.0;

    const UndoableCommand = union(enum) {
        fence,
        set_data_except_tree: Lego,

        destroy_floating: Lego.Index,
        recreate_floating: Lego,

        change_child: struct {
            original: Lego.Index,
            new: Lego.Index,
        },

        insert: struct {
            where: Lego.Tree,
            what: Lego.Index,
        },
        pop: Lego.Index,

        set_grabbing: Grabbing,
        set_handlayer: Lego.Index,
    };

    /// in draw order
    fn roots(workspace: Workspace, config: struct {
        include_hand: bool,
        include_lenses: bool,
        include_toolbars: bool,
        include_floating_inputs: bool,

        pub const all: @This() = .{
            .include_hand = true,
            .include_lenses = true,
            .include_toolbars = true,
            .include_floating_inputs = true,
        };
        pub const interactable: @This() = .{
            .include_hand = false,
            .include_lenses = true,
            .include_toolbars = true,
            .include_floating_inputs = true,
        };
        pub const with_main_camera: @This() = .{
            .include_hand = false,
            .include_lenses = true,
            .include_toolbars = false,
            .include_floating_inputs = true,
        };
    }) std.BoundedArray(Lego.Index, 8) {
        var result: std.BoundedArray(Lego.Index, 8) = .{};
        result.appendAssumeCapacity(workspace.main_area);
        if (config.include_floating_inputs) result.appendAssumeCapacity(workspace.floating_inputs_layer);
        if (config.include_toolbars) result.appendAssumeCapacity(workspace.toolbar_left);
        if (config.include_toolbars) result.appendAssumeCapacity(workspace.toolbar_fnks);
        if (config.include_hand) result.appendAssumeCapacity(workspace.hand_layer);
        if (config.include_lenses) result.appendAssumeCapacity(workspace.lenses_layer);
        return result;
    }

    pub fn init(dst: *Workspace, gpa: std.mem.Allocator, random_seed: u64) !void {
        dst.* = kommon.meta.initDefaultFields(Workspace);
        dst.random_instance = .init(random_seed);
        dst.arena_for_atom_names = .init(gpa);
        dst.arena_for_oneframe_data = .init(gpa);
        dst.gpa_for_bindings = gpa;
        dst.gpa_for_atom_names = gpa;
        dst.gpa_for_big_buffers = gpa;

        var scratch: std.heap.ArenaAllocator = .init(gpa);
        defer scratch.deinit();

        dst.main_area = try Toybox.new(.{ .scale = 0.1 }, .{ .area = .{ .bg = .all, .style = .main_area } }, .new(@src()));
        dst.toolbar_left = try Toybox.new(.{}, .{
            .area = .{
                .bg = .{
                    // ensure that "mouse off-screen on the left" also overlaps the toolbar
                    .local_rect = toolbar_left_rect.plusMargin3(.left, 100),
                },
                .style = .toolbar,
            },
        }, .new(@src()));
        dst.toolbar_fnks = try Toybox.new(.{}, .{
            .area = .{
                .bg = .{
                    // ensure that "mouse off-screen on the right" also overlaps the toolbar
                    .local_rect = toolbar_fnks_rect.plusMargin3(.right, 100),
                },
                .style = .toolbar,
            },
        }, .new(@src()));
        dst.lenses_layer = try Toybox.new(undefined, .{ .area = .{ .bg = .none, .style = .none } }, .new(@src()));
        dst.floating_inputs_layer = try Toybox.new(undefined, .{ .area = .{ .bg = .none, .style = .none } }, .new(@src()));
        dst.floating_inputs_layer.get().immutable = true;
        dst.invisible_floating_inputs_layer = try Toybox.new(undefined, .{ .area = .{ .bg = .none, .style = .none, .non_interactable = true } }, .new(@src()));
        dst.invisible_floating_inputs_layer.get().immutable = true;

        if (true) {
            try dst.setupBubbles(scratch.allocator(), gpa);
        } else {
            dst.centerCameraAt(.{ .pos = .zero, .scale = 15 }, true);
            const executor = try Toybox.buildExecutor(.{ .pos = .zero }, false, try Toybox.buildGarland(.{}, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
            }, .new(@src())));
            Toybox.addChildLast(dst.main_area, executor);
        }

        assert(try dst.valid(scratch.allocator()));

        try updateNonInteractive(
            dst,
            Rect
                .fromCenterAndSize(.zero, .both(2))
                .withAspectRatio(stuff.metadata.desired_aspect_ratio, .grow, .center),
            0,
            .{ .over_background = dst.main_area },
            null,
            scratch.allocator(),
        );

        toybox.undo_stack.commands.clear();

        assert(try dst.valid(scratch.allocator()));
        assert(toybox.undo_stack.commands.isEmpty());
    }

    fn setupBubbles(dst: *Workspace, scratch: std.mem.Allocator, gpa: std.mem.Allocator) !void {
        dst.centerCameraAt(.{ .pos = .new(4, 0), .scale = 15 }, true);

        const path_next_close: Vec2 = .new(30, 0);
        const path_next: Vec2 = .new(50, 0);
        const path_up: Vec2 = .new(60, -30);
        const path_down: Vec2 = .new(60, 30);

        var bubble_pos: Vec2 = .zero;
        const welcome_to_the_lab = try Toybox.buildBubble(.{ .pos = bubble_pos }, .nothing, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -7.5);
            postit.addFromText(postit_pos, &.{ "Welcome", "to the lab!" });
            postit_pos.addInPlace(.new(7.75, -0.6));
            postit.addFromText(postit_pos, &.{ "First of all,", "here are some", "Vaus!" });
            postit_pos.addInPlace(.new(8.1, 1.2));
            postit.addFromText(postit_pos, &.{ "Experiment", "with them,", "get a feel!" });

            postit_pos = .new(-4, 1);
            Toybox.addChildLast(bp, try Toybox.buildSexpr(
                .{ .pos = postit_pos.add(.new(-3.4, -1.7)) },
                .{ .atom_lit = "a" },
                false,
                false,

                .new(@src()),
            ));
            Toybox.addChildLast(bp, try Toybox.buildSexpr(
                .{ .pos = postit_pos },
                .{ .atom_lit = "c" },
                false,
                false,

                .new(@src()),
            ));
            Toybox.addChildLast(bp, try Toybox.buildSexpr(
                .{ .pos = postit_pos.add(.new(1.8, -2.6)) },
                .{ .atom_lit = "b" },
                false,
                false,

                .new(@src()),
            ));
            Toybox.addChildLast(bp, try Toybox.buildSexprFromText(
                .{ .pos = postit_pos.add(.new(3.8, 0.7)) },
                "(a . b)",
                false,
                false,

                .new(@src()),
            ));
            Toybox.addChildLast(bp, try Toybox.buildSexprFromText(
                .{ .pos = postit_pos.add(.new(7.4, -0.4)) },
                "((b . c) . a)",
                false,
                false,

                .new(@src()),
            ));
            Toybox.addChildLast(bp, try Toybox.buildSexprFromText(
                .{ .pos = postit_pos.add(.new(12.4, 0.2)) },
                "((b . c) . a)",
                true,
                false,

                .new(@src()),
            ));

            postit_pos = .new(-7, 6.4);
            postit.addFromText(postit_pos, &.{ "Left click", "to grab them", "(and also", "these notes)" });
            postit_pos.addInPlace(.new(7.5, 1.1));
            postit.addFromText(postit_pos, &.{ "Don't worry about", "losing them.", "The top left", "button resets", "the whole slide." });
            postit_pos.addInPlace(.new(7.6, -0.8));
            postit.addFromText(postit_pos, &.{ "And you can", "always Z", "to undo.", "", "So experiment!" });

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, welcome_to_the_lab);

        bubble_pos.addInPlace(path_next_close);
        const simple_warmup = try Toybox.buildBubble(.{ .pos = bubble_pos }, welcome_to_the_lab, .{
            .has_sexpr = try Toybox.buildSexprFromText(.{}, "(a . (b . c))", false, false, .new(@src())),
        }, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-7.6, -4.2);
            postit.addFromText(postit_pos, &.{ "Move around", "with WASD,", "Arrow Keys,", "or middle click." });
            postit_pos.addInPlace(.new(0.8, 9.2));
            postit.addFromText(postit_pos, &.{ "Scrollwheel", "to zoom." });

            postit_pos = .new(1.2, -7.2);
            postit.addFromText(postit_pos, &.{ "So, what are", "Vaus?" });
            postit_pos.addInPlace(.new(6.7, 1.4));
            postit.addFromText(postit_pos.add(.new(0.4, 0.45)), &.{ "(the Theoretical", "Vaulogy lab", "is right next", "door; they do", "very much care)" });
            postit.addFromText(postit_pos, &.{ "Who cares!", "The cool part", "is, what can", "you do with them?" });
            postit_pos.addInPlace(.new(-6.2, 6.4));
            postit.addFromText(postit_pos, &.{ "Right now,", "not a lot" });
            postit_pos.addInPlace(.new(6.3, 1.1));
            postit.addFromText(postit_pos, &.{ "Their only", "power is", "matching", "with themselves." });
            postit_pos.addInPlace(.new(-3.2, 6.7));

            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 2) }, .part = .{ .paragraph = &.{ "As a warmup,", "make that Vau", "match this:" } } },
                .{ .point = .{ .pos = .new(3, 4.8) }, .part = .{ .thing = try Toybox.buildSexprFromText(
                    .{},
                    "(a . (b . c))",
                    true,
                    false,
                    .new(@src()),
                ) } },
            });

            Toybox.addChildLast(bp, try Toybox.buildSexprFromText(
                .{ .pos = postit_pos.add(.new(-5, 1.8)) },
                "(c . (b . a))",
                false,
                false,

                .new(@src()),
            ));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, simple_warmup);

        bubble_pos.addInPlace(path_next_close);
        const intro_to_strands = try Toybox.buildBubble(.{ .pos = bubble_pos }, simple_warmup, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-5.6, -8);
            postit.addFromText(postit_pos, &.{ "Ok, enough", "manual labor." });
            postit_pos.addInPlace(.new(7.9, 0.3));
            postit.addFromText(postit_pos, &.{ "The real", "magic of Vaus", "is manipulating", "them with", "Strands." });

            postit_pos = .new(-7.4, 0.2);
            postit.addFromText(postit_pos.add(.new(0.5, -0.5)), &.{ "Here is an", "example Strand:" });
            postit_pos.addInPlace(.new(8, 0));
            Toybox.addChildLast(bp, try Toybox.buildGarland(.{ .pos = postit_pos.add(.new(-1, -2.5)) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
            }, .new(@src())));
            postit_pos.addInPlace(.new(5.8, -0.4));
            postit.addFromText(postit_pos, &.{ "This one turns", "'a' into 'b',", "and", "'b' into 'c'" });

            postit_pos = .new(-7.4, 0.2);
            postit_pos.addInPlace(.new(1.2, 7.1));
            postit.addFromText(postit_pos, &.{ "You'll see it", "in action", "soon." });
            postit_pos.addInPlace(.new(7.4, 1.1));
            postit.addFromText(postit_pos, &.{ "Don't be", "afraid of", "breaking it!" });

            // postit_pos = .new(-7.4, 0.2);
            // postit_pos.addInPlace(.new(1.2, 7.9));
            // postit.addFromText(postit_pos, &.{ "You'll see them", "in action", "soon, but first", "add this case", "to the strand." });
            // // postit.addFromText(postit_pos, &.{ "You'll see it", "in action", "in the next", "slide, but first", "play around with it!" });

            // postit_pos.addInPlace(.new(8, 0));
            // postit_pos.addInPlace(.new(8.2, 1.1));
            // postit.addFromText(postit_pos, &.{ "And play around", "wit it! You", "can always undo,", "or reset the", "whole slide." });

            Toybox.addChildLast(bp, try Toybox.buildCase(.{ .pos = .new(7.8, 10.2) }, .{
                .pattern = try Toybox.buildSexpr(
                    .{},
                    .{ .atom_lit = "c" },
                    true,
                    false,

                    .new(@src()),
                ),
                .template = try Toybox.buildSexpr(
                    .{},
                    .{ .atom_lit = "a" },
                    false,
                    false,

                    .new(@src()),
                ),
                .fnkname = null,
                .next = null,
            }, .new(@src())));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, intro_to_strands);

        bubble_pos.addInPlace(path_next_close);
        const intro_to_executors = try Toybox.buildBubble(.{ .pos = bubble_pos }, intro_to_strands, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-6, -8);
            postit.addFromText(postit_pos, &.{ "Take one of", "these vaus,", "and place it", "next to", "the strand:" });
            postit_pos.addInPlace(.new(8, 1));
            Toybox.addChildLast(bp, try Toybox.buildSexpr(
                .{ .pos = postit_pos.add(.new(-3.4, -1.7)) },
                .{ .atom_lit = "a" },
                false,
                false,

                .new(@src()),
            ));
            Toybox.addChildLast(bp, try Toybox.buildSexpr(
                .{ .pos = postit_pos },
                .{ .atom_lit = "c" },
                false,
                false,

                .new(@src()),
            ));
            Toybox.addChildLast(bp, try Toybox.buildSexpr(
                .{ .pos = postit_pos.add(.new(1.8, -2.6)) },
                .{ .atom_lit = "b" },
                false,
                false,

                .new(@src()),
            ));

            postit_pos = .new(0, -3);
            const executor = try Toybox.buildExecutor(.{ .pos = postit_pos }, false, false, null, try Toybox.buildGarland(.{}, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
            }, .new(@src())));
            executor.children(.executor).controls.children(.executor_controls).brake.get().specific.executor_brake.brake_t = 0.9;
            Toybox.addChildLast(bp, executor);
            postit.addFromText(postit_pos.add(.new(-6.5, 6)), &.{ "Use the crank", "and brake", "to control", "execution speed" });
            postit_pos.addInPlace(.new(8.5, 11.5));
            postit.addFromText(postit_pos, &.{ "It's one-use only,", "so undo with Z", "to try with", "another vau." });

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, intro_to_executors);

        const scorer_pos: Vec2 = .new(-7, 0);

        bubble_pos.addInPlace(path_next_close);
        const intro_to_fnkboxes = try Toybox.buildBubble(.{ .pos = bubble_pos }, intro_to_executors, .all_scorers_solved, if (false) blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "Your main job", "will be designing", "new strands" });
            postit_pos.addInPlace(.new(7.4, 0.9));
            postit.addFromText(postit_pos, &.{ "I will give you", "assignments.", "You must make", "a new strand to", "solve each one." });
            postit_pos.addInPlace(.new(7.6, 0.8));
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 2.5) }, .part = .{ .paragraph = &.{ "This is the first", "assignment,", "already solved", "as an example." } } },
                .{ .point = .{ .pos = .new(3, 5), .turns = 0.25 }, .part = .arrow },
            });
            postit_pos = .new(-6.5, 0);
            postit.addFromText(postit_pos, &.{ "The box below", "is the solution", "to the assignment." });
            postit_pos.addInPlace(.new(6.6, 2.8));
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 2.5) }, .part = .{ .paragraph = &.{ "That green vau", "is the name", "of the strand" } } },
                .{ .point = .{ .pos = .new(3, 5), .turns = 0.25 }, .part = .arrow },
                .{ .point = .{ .pos = .new(5, 1) }, .part = .arrow },
            });

            postit_pos = .new(0, 6);
            const fnkbox = try Toybox.buildFnkboxFromLevel(
                .{ .pos = postit_pos },
                try dst.findFnkname(.{}, true, levels[0].fnk_name.atom_lit.value),
                levels[0],
                false,
                scratch,
                dst.gpa_for_atom_names,
            );
            fnkbox.children(.fnkbox).executor.children(.executor).controls.get().specific.executor_controls.brake().get().specific.executor_brake.brake_t = 0.9;
            Toybox.addChildLast(bp, fnkbox);

            postit_pos = .new(4, 0);
            const scorer = try Toybox.buildScorer(.{ .pos = postit_pos }, &.{levelIndex("changeLowercaseToNextCyclingOnC")});
            const old_fnkname = scorer.children(.scorer).scorer_rows.get().tree.first.children(.scorer_row).fnkname;
            const new_fnkname = try Toybox.dupeIntoFloating(fnkbox.children(.fnkbox).fnkname, .new(@src()));
            new_fnkname.get().local_point = old_fnkname.get().local_point;
            new_fnkname.get().specific.sexpr.is_pattern = false;
            new_fnkname.get().immutable = false;
            Toybox.changeChild(old_fnkname, new_fnkname);
            Toybox.destroyFloating(old_fnkname);
            Toybox.addChildLast(bp, scorer);

            break :blk bp;
        } else blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -7);
            postit.addFromText(postit_pos, &.{ "Your main job", "will be designing", "new strands" });
            postit_pos.addInPlace(.new(7.4, 0.9));
            postit.addFromText(postit_pos, &.{ "I will give you", "assignments.", "You must make", "a new strand to", "solve each one." });
            postit_pos.addInPlace(.new(7.6, 0.8));
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 2.5) }, .part = .{ .paragraph = &.{ "This is the first", "assignment." } } },
                .{ .point = .{ .pos = .new(3, 5), .turns = 0.25 }, .part = .arrow },
            });

            const strand_already_in_box = true;
            if (!strand_already_in_box) {
                postit_pos = .new(-7.8, 2.4);
                postit.addFromText(postit_pos, &.{ "This strand", "solves it:" });
                postit_pos.addInPlace(.new(1, 4.7));
                Toybox.addChildLast(bp, try Toybox.buildGarland(.{ .pos = postit_pos.add(.new(-1, -2.5)) }, &.{
                    try Toybox.buildCase(.{}, .{
                        .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, false, .new(@src())),
                        .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                        .fnkname = null,
                        .next = null,
                    }, .new(@src())),
                    try Toybox.buildCase(.{}, .{
                        .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, .new(@src())),
                        .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, .new(@src())),
                        .fnkname = null,
                        .next = null,
                    }, .new(@src())),
                    try Toybox.buildCase(.{}, .{
                        .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, true, false, .new(@src())),
                        .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, .new(@src())),
                        .fnkname = null,
                        .next = null,
                    }, .new(@src())),
                }, .new(@src())));
            }

            postit_pos = .new(0, 0);
            const scorer = try Toybox.buildScorer(.{ .pos = if (strand_already_in_box) scorer_pos else postit_pos }, &.{levelIndex("changeLowercaseToNextCyclingOnC")}, &.{.new(4.5, 8.5)});
            Toybox.addChildLast(bp, scorer);

            postit_pos.addInPlace(.new(if (strand_already_in_box) -7 else 0, 4.5));
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "Click the +", "button to create", "a new 'solution'" } } },
                .{ .point = .{ .pos = .new(3, 1), .turns = -0.25 }, .part = .arrow },
            });
            postit_pos.addInPlace(.new(6.8, 0.4));
            if (strand_already_in_box) {
                postit.addFromText(postit_pos, &.{ "And complete the", "strand, solving", "the assignment." });
            } else {
                postit.addFromText(postit_pos, &.{ "And move the", "strand to it,", "solving the", "assignment." });
            }

            if (strand_already_in_box) {
                postit_pos.addInPlace(.new(7, 0));
                Toybox.addChildLast(bp, try Toybox.buildCase(.{ .pos = postit_pos }, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())));
            }

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, intro_to_fnkboxes);

        bubble_pos.addInPlace(path_next_close);
        const player_creates_fnkbox = try Toybox.buildBubble(.{ .pos = bubble_pos }, intro_to_fnkboxes, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{"Try on your own!"});
            postit_pos = .new(7.3, 7.7);
            // postit.addFromText(postit_pos, &.{ "(I promise", "the assignments", "will get more", "interesting)" });
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "Click the     ", "buttons to", "check your", "solution" } } },
                .{ .point = .{ .pos = .new(4.7, 1.5) }, .part = .launch_testcase_button },
            });

            postit_pos = .new(0.2, -10.4);
            Toybox.addChildLast(bp, try Toybox.buildGarland(.{ .pos = postit_pos }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
            }, .new(@src())));
            postit_pos.addInPlace(.new(0.3, 3.9));
            Toybox.addChildLast(bp, try Toybox.buildCase(.{ .pos = postit_pos }, .{
                .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, true, false, .new(@src())),
                .template = try Toybox.buildSexpr(.{}, .empty, false, false, .new(@src())),
                .fnkname = null,
                .next = null,
            }, .new(@src())));
            postit_pos.addInPlace(.new(4.6, 1.2));
            Toybox.addChildLast(bp, try Toybox.buildSexpr(.{ .pos = postit_pos }, .{ .atom_lit = "b" }, false, false, .new(@src())));
            postit_pos = .new(-6, 4);
            Toybox.addChildLast(bp, try Toybox.buildCase(.{ .pos = postit_pos }, .{
                .pattern = try Toybox.buildSexpr(.{}, .empty, true, false, .new(@src())),
                .template = try Toybox.buildSexpr(.{}, .empty, false, false, .new(@src())),
                .fnkname = null,
                .next = null,
            }, .new(@src())));
            postit_pos.addInPlace(.new(-1.8, 0.9));
            Toybox.addChildLast(bp, try Toybox.buildSexpr(.{ .pos = postit_pos }, .{
                .atom_lit = "a",
            }, true, false, .new(@src())));
            postit_pos = .new(-6, 4);
            postit_pos.addInPlace(.new(1.4, -0.7));
            Toybox.addChildLast(bp, try Toybox.buildSexpr(.{ .pos = postit_pos }, .{
                .atom_lit = "c",
            }, false, false, .new(@src())));

            const scorer = try Toybox.buildScorer(.{ .pos = scorer_pos }, &.{levelIndex("changeLowercaseToPrevCyclingOnC")}, &.{.new(0, 8.5)});
            Toybox.addChildLast(bp, scorer);

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, player_creates_fnkbox);

        bubble_pos.addInPlace(path_next_close);
        const intro_to_wildcards = try Toybox.buildBubble(.{ .pos = bubble_pos }, player_creates_fnkbox, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "This next assignment", "has too many cases", "to be hardcoded." });
            postit_pos.addInPlace(.new(6.8, 0.4));
            postit.addFromText(postit_pos, &.{ "Fear not!", "We can solve it", "elegantly, with", "Wildcards." });

            postit_pos.addInPlace(.new(7.8, -1.4));
            Toybox.addChildLast(bp, try Toybox.buildGarland(.{ .pos = postit_pos }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexprFromText(.{}, "(@up . @down)", true, false, .new(@src())),
                    .template = try Toybox.buildSexprFromText(.{}, "(@down . @up)", false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
            }, .new(@src())));

            postit_pos = .new(-8, -8);
            postit_pos.addInPlace(.new(1.8, 7.8));
            postit_pos.addInPlace(.new(0, 6));
            postit.addFromText(postit_pos, &.{ "Wildcards match", "with any value,", "and can", "recreate it." });

            postit_pos = .new(8.1, 7.7);
            postit.addFromText(postit_pos, &.{ "By the way,", "right click", "to duplicate", "anything" });

            postit_pos = .new(0, 0);
            const scorer = try Toybox.buildScorer(.{ .pos = scorer_pos }, &.{levelIndex("swap")}, &.{.new(0, 8.5)});
            Toybox.addChildLast(bp, scorer);

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, intro_to_wildcards);

        bubble_pos.addInPlace(path_next_close);
        const intro_to_calling = try Toybox.buildBubble(.{ .pos = bubble_pos }, intro_to_wildcards, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{"Next concept!"});
            postit_pos.addInPlace(.new(7.2, 0.3));
            postit.addFromText(postit_pos, &.{ "You can use", "old solutions", "as part of", "new solutions" });
            postit_pos.addInPlace(.new(7.1, 0.2));
            postit.addFromText(postit_pos, &.{ "For example", "this assignment", "is almost already", "solved by the", "first assignment" });

            postit_pos = .new(-6.7, 4.25);
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 2.5), .scale = 0.95 }, .part = .{ .paragraph = &.{ "That's the 'name'", "of your solution", "to the first", "assignment" } } },
                .{ .point = .{ .pos = .new(4, 5), .turns = 0.18 }, .part = .arrow },
            });
            postit.addFromText(postit_pos.add(.new(7.6, 1.3)), &.{ "Control-click it", "to see its", "definition" });
            Toybox.addChildLast(bp, try Toybox.buildGarland(.{ .pos = postit_pos.add(.new(-1.5, 3.8)) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexprFromText(.{}, "(@f . <empty>)", true, false, .new(@src())),
                    .template = try Toybox.buildSexprFromText(.{}, "@f", false, false, .new(@src())),
                    .fnkname = try Toybox.buildSexprFromText(.{}, levels[0].fnk_name, false, false, .new(@src())),
                    .next = null,
                }, .new(@src())),
            }, .new(@src())));

            const scorer = try Toybox.buildScorer(.{ .pos = scorer_pos }, &.{levelIndex("shiftTopHalf")}, &.{.new(4, 9.5)});
            Toybox.addChildLast(bp, scorer);

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, intro_to_calling);

        bubble_pos.addInPlace(path_next_close);
        const calling_exercise = try Toybox.buildBubble(.{ .pos = bubble_pos }, intro_to_calling, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "Time to prove", "you're learning" });
            postit_pos.addInPlace(.new(7.2, 0.2));
            postit.addFromText(postit_pos, &.{ "In the", "left toolbar", "you have fresh", "pieces and", "wildcards" });
            postit_pos.addInPlace(.new(7.1, 0.1));
            postit.addFromText(postit_pos, &.{ "On the right one", "you have all your", "solutions so far" });

            postit_pos = .new(-6, 5);
            postit.addFromText(postit_pos, &.{ "Carefully", "study the", "examples", "to understand", "the assignment" });

            if (false) {
                postit_pos = .new(-6, 1);
                Toybox.addChildLast(bp, try Toybox.buildSexpr(.{ .pos = postit_pos, .scale = 0.5, .turns = 0.25 }, .{
                    .atom_lit = levels[0].fnk_name,
                }, false, true, .new(@src())));
                postit_pos = .new(-4, 1);
                Toybox.addChildLast(bp, try Toybox.buildSexpr(.{ .pos = postit_pos, .scale = 0.5, .turns = 0.25 }, .{
                    .atom_lit = levels[1].fnk_name,
                }, false, true, .new(@src())));
            }

            const scorer = try Toybox.buildScorer(.{ .pos = scorer_pos }, &.{levelIndex("shiftInUnknownDirection")}, &.{.new(0, 8.5)});
            Toybox.addChildLast(bp, scorer);

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, calling_exercise);

        dst.toolbar_unlocks.case_with_wildcards = calling_exercise;

        bubble_pos.addInPlace(path_next_close);
        const intro_to_nested_strands = try Toybox.buildBubble(.{ .pos = bubble_pos }, calling_exercise, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{"Final concept!"});
            postit_pos.addInPlace(.new(7.2, 0.3));
            postit.addFromText(postit_pos, &.{ "Strands can be", "nested inside", "other strands" });
            postit_pos.addInPlace(.new(7.2, 0.3));
            postit.addFromText(postit_pos, &.{ "It's easier to", "see than to", "explain, try", "it out" });

            const level_index = levelIndex("startWithB");
            postit_pos = .new(-5, 2);
            Toybox.addChildLast(bp, try Lego.Specific.Garland.buildFromOldCoreValue(
                .{ .pos = postit_pos },
                levels[level_index].bubble_definition.?,
                scratch,
                .new(@src()),
            ));

            const scorer = try Toybox.buildScorer(.{ .pos = scorer_pos }, &.{level_index}, &.{.new(0, 8.5)});
            Toybox.addChildLast(bp, scorer);

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, intro_to_nested_strands);

        bubble_pos.addInPlace(path_next_close);
        const intro_to_mixing_both_tricks = try Toybox.buildBubble(.{ .pos = bubble_pos }, intro_to_nested_strands, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },
                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "That's all!", "You're now", "an expert in", "vau manipulation" });

            postit_pos.addInPlace(.new(8.2, 0.3));
            postit.addFromText(postit_pos, &.{ "These last two", "'tricks' can be", "mixed:" });
            postit_pos.addInPlace(.new(7.1, 0.3));
            postit.addFromText(postit_pos, &.{ "call a previous", "solution,", "and then match", "on the result" });

            const level_index = levelIndex("withBottomShifted");
            postit_pos = .new(-7.6, 2);
            Toybox.addChildLast(bp, try Lego.Specific.Garland.buildFromOldCoreValue(
                .{ .pos = postit_pos },
                levels[level_index].bubble_definition.?,
                scratch,
                .new(@src()),
            ));

            const scorer = try Toybox.buildScorer(.{ .pos = scorer_pos }, &.{level_index}, &.{.new(0, 8.5)});
            Toybox.addChildLast(bp, scorer);

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, intro_to_mixing_both_tricks);

        bubble_pos.addInPlace(path_next_close);
        const final_tutorial = try Toybox.buildBubble(.{ .pos = bubble_pos }, intro_to_mixing_both_tricks, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },
                .new(@src()),
            );
            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            const postits: []const []const []const u8 = &.{
                &.{ "You now know", "everything!" },
                &.{ "Take some fresh", "cases from the", "left toolbar", "and build a", "solution" },
                &.{"Good luck!"},
            };
            const positions: []const Vec2 = &.{
                .new(-8, -8),
                .new(-0.3, -7.2),
                .new(7.7, -7.1),
            };

            for (postits, positions) |lines, pos| {
                postit.addFromText(pos, lines);
            }

            var postit_pos: Vec2 = .new(7.5, 6);
            postit.addFromText(postit_pos, &.{ "Nested strands", "can have", "nested strands" });
            postit_pos.addInPlace(.new(0.1, 0.2));
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 1.8) }, .part = .{ .paragraph = &.{"You will need"} } },
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{"to call    "} } },
                .{ .point = .{ .pos = .new(3, 4.2) }, .part = .{ .paragraph = &.{"twice"} } },
                .{ .point = .{ .pos = .new(4.6, 2.65), .scale = 0.5, .turns = 0.25 }, .part = .{ .thing = try Toybox.buildSexprFromText(
                    .{},
                    "changeLowercaseToNextCyclingOnC",
                    false,
                    true,
                    .new(@src()),
                ) } },
            });
            postit_pos.addInPlace(.new(0.1, 0.2));
            postit.addFromText(postit_pos, &.{ "Below this", "I've left", "some hints,", "just in case" });

            const k = 0;
            const level_index = levelIndex("shiftPair");
            const scorer = try Toybox.buildScorer(.{ .pos = scorer_pos.addY(5 * k) }, &.{level_index}, &.{.new(k * 4, 8.5 + tof32(k) * 2)});
            Toybox.addChildLast(bp, scorer);

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, final_tutorial);

        var old_bubble_pos = bubble_pos;
        bubble_pos.addInPlace(path_up);
        const first_recursion_cruel = try buildBubbleSimple(bubble_pos, final_tutorial, &.{ "shiftAll", "mirrorTree" }, &.{
            &.{ "From now on,", "I won't always", "tell you", "all you need", "to know" },
            &.{ "Instead of", "going from", "easy to hard", "assignments..." },
            &.{ "...I will give you", "unreasonable", "hard ones first and", "hide the tutorial", "behind that", "'hint?' button" },
            // &.{ "Some", "assignments", "will be", "unreasonably", "hard" },
            // &.{ "or require you", "to rediscover", "some genius", "idea"},
            // &.{ "Don't hesitate", "to use the", "'hint' button", "for extra", "explanations" },
        });
        Toybox.addChildLast(dst.main_area, first_recursion_cruel);

        const first_recursion_nicer = try buildBubbleSimple(bubble_pos.add(path_down.neg()), .nothing, &.{"hasSomeB"}, &.{
            &.{ "Many problems", "can be solved", "by a function", "that invokes", "itself" },
            &.{ "(these are", "'recursive'", "functions)" },
        });
        Toybox.addChildLast(dst.main_area, first_recursion_nicer);
        addHint(first_recursion_nicer, first_recursion_cruel);

        const optional = try Toybox.buildBubble(.{ .pos = old_bubble_pos.add(path_down) }, final_tutorial, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "These are", "even harder", "optional", "assignments" });
            postit_pos.addInPlace(.new(7.7, 0.1));
            postit.addFromText(postit_pos, &.{ "They will be a bit", "spoiled by later", "assignments,", "so it's more fun to", "try them now" });
            postit_pos.addInPlace(.new(7.7, 0.2));
            postit.addFromText(postit_pos, &.{ "But don't", "expect to", "succeed!" });
            postit_pos = .new(-7, 7);
            postit.addFromText(postit_pos, &.{ "You can", "right click", "functions to", "create your", "own helpers" });

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-8, -2) }, &.{
                levelIndex("biggestHalf"),
            }, &.{
                .new(-6, 8.5),
            }));

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-4, 2) }, &.{
                levelIndex("deepestHalf"),
            }, &.{
                .new(6, 8.5),
            }));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, optional);

        bubble_pos.addInPlace(path_next);
        const double_recursion = try buildBubbleSimple(bubble_pos, first_recursion_cruel, &.{"sameShape?"}, &.{
            &.{ "Now that you know", "how to recurse", "on a tree..." },
            &.{ "...let's see how", "you recurse on", "two trees at once" },
        });
        Toybox.addChildLast(dst.main_area, double_recursion);

        bubble_pos.addInPlace(path_next);
        const intro_to_lists = try Toybox.buildBubble(.{ .pos = bubble_pos }, double_recursion, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "All assignments", "take and return", "a single vau" });
            postit_pos.addInPlace(.new(7.3, 0.3));
            postit.addFromText(postit_pos, &.{ "It's easy to treat", "a single vau", "as a pair", "of two values" });
            postit_pos.addInPlace(.new(7.4, 0.2));
            // postit.addFromText(postit_pos, &.{ "But sometimes", "this isn't", "enough!" });
            postit.addFromText(postit_pos, &.{ "But sometimes", "you need an", "unknown", "amount", "of values!" });

            postit_pos = .new(-7, -1);
            postit.addFromText(postit_pos, &.{ "Luckily, we can", "encode any", "sequence of vaus", "into a single one" });
            postit_pos.addInPlace(.new(6.7, 0.2));
            postit.addFromText(postit_pos, &.{ "The top half is", "the first element,", "the bottom half", "is the rest" });
            postit_pos.addInPlace(.new(6.7, -0.1));
            postit.addFromText(postit_pos, &.{ "When there are", "no elements left", "we use the", "'empty list' vau:" });
            Toybox.addChildLast(bp, try Toybox.buildSexpr(.{ .pos = postit_pos.add(.new(2, 3)) }, .{ .atom_lit = "nil" }, false, false, .new(@src())));

            postit_pos = .new(-7.2, 6.2);
            postit.addFromText(postit_pos, &.{ "I've added this", "to your toolbar:" });
            postit.addFromText(postit_pos.add(.new(15.8, 1.1)), &.{ "It lets you", "easily edit", "a list of vaus" });
            postit_pos.addInPlace(.new(5.1, 0.1));
            Toybox.addChildLast(bp, try Toybox.buildListViewer(.{ .pos = postit_pos }, try Toybox.buildSexprFromText(.{ .scale = 2 }, "(a . (b . (c . nil)))", false, false, .new(@src()))));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, intro_to_lists);
        dst.toolbar_unlocks.list_viewer = first_recursion_cruel;

        bubble_pos.addInPlace(path_next);
        const lists_1 = try Toybox.buildBubble(.{ .pos = bubble_pos }, intro_to_lists, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-7, -8) }, &.{
                levelIndex("second"),
            }, &.{.new(8, -4.5)}));

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-7, -3) }, &.{
                levelIndex("pairToList"),
            }, &.{.new(-8, -4.5)}));

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            const postit_pos: Vec2 = .new(-3, 4);

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-7, postit_pos.y + 4.5) }, &.{
                levelIndex("prepend"),
            }, &.{.new(0, 10.5)}));

            postit.addFromText(postit_pos, &.{ "Top half is", "the list,", "bottom half is", "the element" });

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, lists_1);

        bubble_pos.addInPlace(path_next);
        const lists_1_5 = try Toybox.buildBubble(.{ .pos = bubble_pos }, lists_1, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            const postit_pos: Vec2 = .new(-5, -5);
            postit.addFromText(postit_pos, &.{ "Note that this list", "has three", "elements,", "none of them 'b'" });
            Toybox.addChildLast(bp, try Toybox.buildListViewer(.{ .pos = postit_pos.add(.new(5, 0)) }, try Toybox.buildSexprFromText(
                .{ .scale = 2 },
                "(a . ((b . b) . (c . nil)))",
                false,
                false,

                .new(@src()),
            )));

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-7, 3) }, &.{
                levelIndex("listHasSomeB"),
            }, &.{.new(0, 8.5)}));

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-7, 8) }, &.{
                levelIndex("last"),
            }, &.{.new(5, 10.5)}));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, lists_1_5);

        bubble_pos.addInPlace(path_up);
        const lists_remove_last_b = try buildBubbleSimple(bubble_pos, lists_1_5, &.{"removeLastB"}, &.{
            &.{ "I don't expect", "you to get", "this one", "without", "the hints" },
            &.{ "Remember you", "can right click", "functions to", "create your", "own helpers" },
        });
        Toybox.addChildLast(dst.main_area, lists_remove_last_b);

        old_bubble_pos = bubble_pos;
        old_bubble_pos.addInPlace(path_down.neg());
        const hints_for_lists_remove_last_b = try buildBubbleSimple(old_bubble_pos, .nothing, &.{ "reverse", "removeFirstB" }, &.{});
        Toybox.addChildLast(dst.main_area, hints_for_lists_remove_last_b);
        addHint(hints_for_lists_remove_last_b, lists_remove_last_b);

        old_bubble_pos.addInPlace(path_next.neg());
        const hint_for_reverse = try buildBubbleSimple(old_bubble_pos, .nothing, &.{"append"}, &.{});
        Toybox.addChildLast(dst.main_area, hint_for_reverse);
        addHint(hint_for_reverse, hints_for_lists_remove_last_b);

        old_bubble_pos = bubble_pos.add(path_up.neg()).add(path_down);
        const lists_middle_element = try buildBubbleSimple(old_bubble_pos, lists_1_5, &.{"middleElement"}, &.{
            &.{ "Not hard, but", "the best solution", "requires some", "ingenuity" },
        });
        Toybox.addChildLast(dst.main_area, lists_middle_element);

        old_bubble_pos.addInPlace(path_up.neg());
        const hint_for_lists_middle_element = try buildBubbleSimple(old_bubble_pos, .nothing, &.{"evenLength?"}, &.{
            &.{ "Two key ideas", "for the best", "solution:" },
            &.{ "Eating two", "elements on", "each step" },
            &.{ "Recursing", "on two lists", "at once" },
            &.{ "Here's an", "assignment", "to practice", "the first one" },
        });
        Toybox.addChildLast(dst.main_area, hint_for_lists_middle_element);
        addHint(hint_for_lists_middle_element, lists_middle_element);

        bubble_pos.addInPlace(path_up);
        const lists_final = try buildBubbleSimple(bubble_pos, lists_remove_last_b, &.{ "mostCommonBoolean", "findSecondLongest" }, &.{
            &.{ "Try to get", "these ones", "without hints!" },
        });
        Toybox.addChildLast(dst.main_area, lists_final);

        old_bubble_pos = bubble_pos;
        old_bubble_pos.addInPlace(path_down.neg());
        const hint_for_lists_final = try buildBubbleSimple(old_bubble_pos, .nothing, &.{ "separateBooleans", "findTopTwoLongest" }, &.{});
        Toybox.addChildLast(dst.main_area, hint_for_lists_final);
        addHint(hint_for_lists_final, lists_final);

        bubble_pos.addInPlace(path_next);
        const breather = try buildBubbleSimple(bubble_pos, lists_final, &.{}, &.{
            &.{ "You've come", "so far!" },
            &.{ "You're almost", "ready for", "your actual", "assignment" },
            &.{ "But first,", "one last", "tutorial" },
        });
        Toybox.addChildLast(dst.main_area, breather);

        bubble_pos.addInPlace(path_up);
        const calculator = try buildBubbleSimple(bubble_pos, breather, &.{"calculator"}, &.{
            &.{ "There are two", "lessons here" },
            &.{ "The hints will", "show you the", "details of", "the first one:", "numbers" },
            &.{ "The second", "lesson", "is more subtle", "and needs", "no hint" },
        });
        Toybox.addChildLast(dst.main_area, calculator);

        old_bubble_pos = bubble_pos;
        old_bubble_pos.addInPlace(path_down.neg());
        const calculator_hints_1 = try buildBubbleSimple(old_bubble_pos, .nothing, &.{ "calculator_sum", "calculator_mul", "calculator_sub" }, &.{
            &.{ "These default", "solutions", "are not", "enough!" },
            &.{ "You'll need to", "handle all", "numbers, not", "just 1 to 9" },
            &.{ "The hint can", "show you how" },
        });
        Toybox.addChildLast(dst.main_area, calculator_hints_1);
        addHint(calculator_hints_1, calculator);

        old_bubble_pos.addInPlace(path_next.neg());
        const calculator_hints_2 = try buildBubbleSimple(old_bubble_pos, .nothing, &.{ "unary_from_naive", "naive_from_unary", "sum_unary" }, &.{
            &.{ "These are", "the only", "hardcoded", "strands", "you need!" },
        });
        Toybox.addChildLast(dst.main_area, calculator_hints_2);
        addHint(calculator_hints_2, calculator_hints_1);

        bubble_pos.addInPlace(path_up);
        const explicit_second_lesson = try buildBubbleSimple(bubble_pos, calculator, &.{}, &.{
            &.{ "Assignments", "have you write", "code that", "manipulates", "data" },
            &.{ "But that line", "got blurred", "in the calculator" },
            &.{ "The data itself", "told you what", "code to run" },
            &.{ "In other words", "data as code." },
            &.{ "(for a much", "harder example,", "see the next", "optional", "assignment)" },
            &.{ "But what about", "code as data?" },
            &.{ "You must be", "tired of making", "and modifying", "strands manually" },
            &.{ "Wouldn't it", "be nice to", "have strands", "do that work", "for you?" },
            &.{ "By representing", "strands as vaus,", "we can write", "tools to create", "and modify them" },
        });
        Toybox.addChildLast(dst.main_area, explicit_second_lesson);

        const optional_brainfuck = try buildBubbleSimple(bubble_pos.add(path_down), explicit_second_lesson, &.{"brainfuck"}, &.{
            &.{ "This assignment", "is about", "Brainf*ck,", "a programming", "language" },
            &.{ "Search online", "how it works" },
            &.{ "The top half", "is the code,", "the lower half", "is the stdin;", "return the stdout" },
        });
        Toybox.addChildLast(dst.main_area, optional_brainfuck);

        bubble_pos.addInPlace(path_up);
        const meta_play = try Toybox.buildBubble(.{ .pos = bubble_pos }, explicit_second_lesson, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -7);
            postit.addFromText(postit_pos, &.{ "This gadget", "converts", "between vaus", "and strands:" });
            const meta_viewer = try Toybox.buildMetaViewer(.{ .pos = postit_pos.add(.new(5.5, -1.5)) });
            Toybox.addChildLast(bp, meta_viewer);
            const old_garland = meta_viewer.children(.meta_viewer).garland;
            const new_garland = try Toybox.buildGarland(.{ .pos = postit_pos.add(.new(-1, -2.5)) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
            }, .new(@src()));
            new_garland.get().local_point = old_garland.get().local_point;
            Toybox.changeChild(old_garland, new_garland);
            Toybox.destroyFloating(old_garland);
            meta_viewer.get().specific.meta_viewer.value_hash = Lego.Specific.MetaViewer.computeValueHash(meta_viewer);
            // meta_viewer.get().specific.meta_viewer.garland_hash = Lego.Specific.MetaViewer.computeGarlandHash(meta_viewer);

            postit_pos.addInPlace(.new(15, 1));
            postit.addFromText(postit_pos, &.{ "Try playing", "around with it,", "try out", "some strands" });

            postit_pos = .new(-7.5, 5);
            postit.addFromText(postit_pos, &.{ "The encoding", "is as simple", "as it can be:", "a list of cases." });
            postit_pos.addInPlace(.new(7.1, 0.7));
            postit.addFromText(postit_pos, &.{ "The encoding", "for a case is", "a bit tricky" });
            postit_pos.addInPlace(.new(7.1, 0.7));
            postit.addFromText(postit_pos, &.{ "Don't worry", "about it", "for now" });

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, meta_play);
        dst.toolbar_unlocks.meta_viewer = meta_play;

        bubble_pos.addInPlace(path_next);
        const meta_duplicate = try Toybox.buildBubble(.{ .pos = bubble_pos }, meta_play, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "Let's start with", "a useful tool:", "call duplicator" });

            postit_pos = .new(-1, -5);
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(1, 5), .turns = math.lerp(0.5, 0.25, 0.5) }, .part = .arrow },
                .{ .point = .{ .pos = .both(3) }, .part = .{ .paragraph = &.{ "It creates", "this from that" } } },
                .{ .point = .{ .pos = .new(5, 4.5) }, .part = .arrow },
            });

            postit.addFromText(.new(8, -8), &.{ "Next slide", "you'll learn", "how to use it" });
            postit.addFromText(.new(5, 12), &.{ "Don't worry", "about the", "implementation", "for now" });

            Toybox.addChildLast(bp, try Toybox.buildSexpr(
                .{ .pos = postit_pos.add(.new(3.75, 1.5)) },
                .{ .atom_lit = "changeLowercaseToNextCyclingOnC" },
                false,
                false,

                .new(@src()),
            ));

            Toybox.addChildLast(bp, try Toybox.buildGarland(.{ .pos = postit_pos.add(.new(-4, 4)) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_var = "first" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_var = "first" }, false, false, .new(@src())),
                    .fnkname = try Toybox.buildSexpr(.{}, .{ .atom_lit = "changeLowercaseToNextCyclingOnC" }, false, true, .new(@src())),
                    .next = try Toybox.buildGarland(.{}, &.{
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_var = "second" }, true, false, .new(@src())),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_var = "second" }, false, false, .new(@src())),
                            .fnkname = try Toybox.buildSexpr(.{}, .{ .atom_lit = "changeLowercaseToNextCyclingOnC" }, false, true, .new(@src())),
                            .next = null,
                        }, .new(@src())),
                    }, .new(@src())),
                }, .new(@src())),
            }, .new(@src())));

            const level_index = levelIndex("meta_duplicate");
            const level = levels[level_index];
            postit_pos = .new(-7, 8);
            Toybox.addChildLast(bp, try Toybox.buildFnkboxFromLevel(
                .{ .pos = postit_pos },
                try dst.findFnkname(.{}, true, level.fnk_name),
                level_index,
                false,
                scratch,
                dst.gpa_for_atom_names,
            ));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, meta_duplicate);

        bubble_pos.addInPlace(path_next_close);
        const meta_duplicate_usage = try Toybox.buildBubble(.{ .pos = bubble_pos }, meta_duplicate, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "You can use it", "manually:", "run it on a vau,", "then use the gadget", "to turn the result", "into a strand" });
            postit_pos.addInPlace(.new(7, 1));
            postit.addFromText(postit_pos, &.{ "That can be", "useful, but", "there's a better", "way: directly", "call the tool", "in your code" });
            postit_pos.addInPlace(.new(7.1, 0.2));
            postit.addFromText(postit_pos, &.{ "Right half is", "the tool name,", "left half is", "the argument" });

            postit.addFromText(.new(-5.5, 1.5), &.{ "The generated", "strand is", "directly", "invoked" });

            Toybox.addChildLast(bp, try Toybox.buildGarland(.{ .pos = postit_pos.add(.new(-4, 4)) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_var = "other" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_var = "other" }, false, false, .new(@src())),
                    .fnkname = try Toybox.buildSexpr(.{}, .{ .pair = .{
                        .up = try Toybox.buildSexpr(.{}, .{ .atom_lit = "meta_duplicate" }, false, true, .new(@src())),
                        .down = try Toybox.buildSexpr(.{}, .{ .atom_lit = "changeLowercaseToNextCyclingOnC" }, false, true, .new(@src())),
                    } }, false, true, .new(@src())),
                    .next = null,
                }, .new(@src())),
            }, .new(@src())));

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-7, 8) }, &.{
                levelIndex("shiftback_with_meta_duplicate"),
            }, &.{.new(0, 12)}));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, meta_duplicate_usage);

        bubble_pos.addInPlace(path_next_close);
        const meta_implementation_1 = try Toybox.buildBubble(.{ .pos = bubble_pos }, meta_duplicate_usage, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos.add(.both(0.3)), &.{"(you should be)"});
            postit.addFromText(postit_pos, &.{ "Are you", "buzzing with", "excitement?" });
            postit_pos.addInPlace(.new(7.4, 0.2));
            postit.addFromText(postit_pos, &.{ "With custom tools,", "you can reduce", "your code", "significantly" });
            postit_pos.addInPlace(.new(7.1, 0.1));
            postit.addFromText(postit_pos, &.{ "Let's learn", "how to", "create them" });

            postit_pos = .new(-8, 0);
            postit.addFromText(postit_pos, &.{ "It's not easy", "to explain,", "so be sure to", "experiment" });
            const meta_viewer = try Toybox.buildMetaViewer(.{ .pos = postit_pos.add(.new(5.5, -1.5)) });
            Toybox.addChildLast(bp, meta_viewer);
            const old_garland = meta_viewer.children(.meta_viewer).garland;
            const new_garland = try Toybox.buildGarland(.{ .pos = postit_pos.add(.new(-1, -2.5)) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                    .fnkname = try Toybox.buildSexpr(.{}, .{ .atom_lit = "changeLowercaseToNextCyclingOnC" }, false, true, .new(@src())),
                    .next = try Toybox.buildGarland(.{}, &.{
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, true, false, .new(@src())),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, .new(@src())),
                            .fnkname = null,
                            .next = null,
                        }, .new(@src())),
                    }, .new(@src())),
                }, .new(@src())),
            }, .new(@src()));
            new_garland.get().local_point = old_garland.get().local_point;
            Toybox.changeChild(old_garland, new_garland);
            Toybox.destroyFloating(old_garland);
            meta_viewer.get().specific.meta_viewer.value_hash = Lego.Specific.MetaViewer.computeValueHash(meta_viewer);
            // meta_viewer.get().specific.meta_viewer.garland_hash = Lego.Specific.MetaViewer.computeGarlandHash(meta_viewer);

            postit_pos = .new(8, 0);
            postit.addFromText(postit_pos, &.{ "As I said,", "a strand is", "encoded", "as a list", "of cases" });

            postit_pos = .new(-8, 8);
            postit.addFromText(postit_pos, &.{ "Each case has", "4 parts: pattern,", "template, call,", "and nested strand" });

            postit_pos = .new(1.3, 7.65);
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(1, 1), .turns = -0.25 - 0.25 * 0.3 }, .part = .arrow },
                .{ .point = .{ .pos = .new(5, 1), .turns = -0.25 + 0.02 }, .part = .arrow },
                .{ .point = .{ .pos = .new(3.5, 1.5), .turns = -0.25 }, .part = .long_arrow },
                .{ .point = .{ .pos = .new(5.25, 3.25), .turns = -0.25 * 0.5 }, .part = .arrow },

                .{ .point = .{ .pos = .new(1.5, 2) }, .part = .{ .paragraph = &.{"pattern"} } },
                .{ .point = .{ .pos = .new(2.5, 3) }, .part = .{ .paragraph = &.{"template"} } },
                .{ .point = .{ .pos = .new(4.75, 2) }, .part = .{ .paragraph = &.{"call"} } },
                .{ .point = .{ .pos = .new(4.25, 4) }, .part = .{ .paragraph = &.{"nested"} } },
                .{ .point = .{ .pos = .new(4.25, 4.75) }, .part = .{ .paragraph = &.{"strand"} } },
            });

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, meta_implementation_1);

        bubble_pos.addInPlace(path_next_close);
        const meta_implementation_2 = try Toybox.buildBubble(.{ .pos = bubble_pos }, meta_duplicate_usage, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 1) }, .part = .{ .paragraph = &.{"Reference:"} } },
                .{ .point = .{ .pos = .new(1, 4), .scale = 1.5 }, .part = .{ .thing = try Toybox.buildSexprFromText(.{ .pos = .new(0, 0) }, "((a . b) . (changeLowercaseToNextCyclingOnC . nil))", false, false, .new(@src())) } },
                .{ .point = .{ .pos = .new(3.15, 2.8), .scale = 0.65 }, .part = .{ .left_paragraph = &.{"template"} } },
                .{ .point = .{ .pos = .new(3.15, 3.6), .scale = 0.65 }, .part = .{ .left_paragraph = &.{"pattern"} } },
                .{ .point = .{ .pos = .new(3.15, 4.35), .scale = 0.65 }, .part = .{ .left_paragraph = &.{"call"} } },
                .{ .point = .{ .pos = .new(3.15, 5.1), .scale = 0.65 }, .part = .{ .left_paragraph = &.{"nested"} } },
            });

            postit_pos.addInPlace(.new(9, 1.0));
            postit.addFromText(postit_pos, &.{ "The template", "and pattern", "have an extra", "quirk:" });
            const other_pos = postit_pos.add(.new(6.8, 0.5));
            postit.addFromParts(other_pos, &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "", "will get", "encoded as", "" } } },
                .{ .point = .{ .pos = .new(2.5, 1.1), .scale = 0.75 }, .part = .{ .thing = try Toybox.buildSexprFromText(
                    .{},
                    "a",
                    false,
                    false,
                    .new(@src()),
                ) } },
                .{ .point = .{ .pos = .new(2.5, 4.9), .scale = 0.75 }, .part = .{ .thing = try Toybox.buildSexprFromText(
                    .{},
                    "(lit . a)",
                    false,
                    false,
                    .new(@src()),
                ) } },
            });
            postit_pos.addInPlace(.new(-0.8, 8.3));
            postit_pos.addInPlace(.new(-6.8, -0.3));
            postit.addFromText(postit_pos, &.{ "This is", "required", "so we can", "also encode", "wildcards:" });
            postit_pos.addInPlace(.new(6.8, 0.3));
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "", "will get", "encoded as", "" } } },
                .{ .point = .{ .pos = .new(2.5, 1.1), .scale = 0.75 }, .part = .{ .thing = try Toybox.buildSexprFromText(
                    .{},
                    "@asdf",
                    false,
                    false,
                    .new(@src()),
                ) } },
                .{ .point = .{ .pos = .new(2.5, 4.9), .scale = 0.75 }, .part = .{ .thing = try Toybox.buildSexprFromText(
                    .{},
                    "(var . asdf)",
                    false,
                    false,
                    .new(@src()),
                ) } },
            });
            postit_pos.addInPlace(.new(6.8, 0.3));
            postit.addFromText(postit_pos, &.{ "(with the", "bottom half", "being some", "random atom)" });

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-7, 8) }, &.{
                levelIndex("fillAllVariablesWithA"),
            }, &.{.new(0, 12)}));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, meta_implementation_2);

        if (false) _ = try Toybox.buildBubble(.{ .pos = bubble_pos }, meta_play, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            var postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "Let's start with ", "a useful tool:", "a map inverter" });

            postit_pos = .new(0, -7);
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(1, 5), .turns = math.lerp(0.5, 0.25, 0.5) }, .part = .arrow },
                .{ .point = .{ .pos = .both(3) }, .part = .{ .paragraph = &.{ "It turns", "this into that" } } },
                .{ .point = .{ .pos = .new(5, 5), .turns = math.lerp(0.0, 0.25, 0.5) }, .part = .arrow },
            });

            postit.addFromText(.new(8, -8), &.{ "For now, you", "can use it", "manually, thanks", "to the gadget" });

            Toybox.addChildLast(bp, try Toybox.buildGarland(.{ .pos = postit_pos.add(.new(-6.25, 3)) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "1" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "2" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "3" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
            }, .new(@src())));

            Toybox.addChildLast(bp, try Toybox.buildGarland(.{ .pos = postit_pos.add(.new(4.75, 3)) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "1" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "2" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "3" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }, .new(@src())),
            }, .new(@src())));

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-7, 8) }, &.{
                levelIndex("meta_invert_map"),
            }, &.{.new(0, 10)}));

            break :blk bp;
        });

        // const meta_encoding = try Toybox.buildBubble(.{ .pos = bubble_pos.add(path_down.neg()) }, .nothing, .all_scorers_solved, blk: {
        //     const bp = try Toybox.new(
        //         .{},
        //         .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },
        //
        //     .new(@src()),);

        //     const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

        //     var postit_pos: Vec2 = .new(-8, -8);
        //     postit.addFromText(postit_pos, &.{ "The encoding", "is as simple", "as it can be:", "a list of cases" });
        //     postit_pos.addInPlace(.new(7, 0.1));
        //     postit.addFromText(postit_pos, &.{ "Each case has", "4 parts:", "pattern,", "template,", "fnkname,", "and nested" });
        //     postit_pos.addInPlace(.new(7, 0.1));
        //     postit.addFromText(postit_pos, &.{ "Fnkname", "is just the", "name of the", "assignment", "called by the", "case, or 'nil'", "by default" });

        //     postit_pos = .new(-8, 0);
        //     postit.addFromText(postit_pos, &.{ "Pattern and", "template", "are encoded", "into the top half" });
        //     postit_pos.addInPlace(.new(7, 0.1));
        //     postit.addFromText(postit_pos, &.{ "They are not", "the raw value", "but 'quoted'" });

        //     postit_pos.addInPlace(.new(8.2, 0.2));
        //     postit.addFromText(postit_pos, &.{ "Nested is", "itself", "a list of cases" });

        //     Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-8, 8) }, &.{
        //         levelIndex("meta_invert_case"),
        //     }, &.{.new(8, 11.5)}));

        //     break :blk bp;
        // });
        // Toybox.addChildLast(dst.main_area, meta_encoding);
        // addHint(meta_encoding, meta_play);

        bubble_pos.addInPlace(path_next);
        const meta_2 = try Toybox.buildBubble(.{ .pos = bubble_pos }, meta_implementation_2, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            const postit_pos: Vec2 = .new(-8, -8);
            postit.addFromText(postit_pos, &.{ "Time to write", "code that", "generates", "code :)" });

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-8, 8) }, &.{
                levelIndex("meta_hardcoded_map"),
            }, &.{.new(8, 11.5)}));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, meta_2);

        bubble_pos.addInPlace(path_next);
        const meta_final = try Toybox.buildBubble(.{ .pos = bubble_pos }, meta_2, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            // const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };

            // var postit_pos: Vec2 = .new(-8, -8);
            // postit.addFromText(postit_pos, &.{""});
            // postit_pos.addInPlace(.new(7.7, 0.2));
            // postit.addFromText(postit_pos, &.{ "In other words,", "you can make strands", "that operate", "on strands!" });

            Toybox.addChildLast(bp, try Toybox.buildScorer(.{ .pos = .new(-8, 8) }, &.{
                levelIndex("interpreter"),
            }, &.{.new(8, 11.5)}));

            break :blk bp;
        });
        Toybox.addChildLast(dst.main_area, meta_final);

        if (false) {
            const bubble_1 = try Toybox.buildBubble(.{ .pos = .new(0, 40) }, .zero, false, try Toybox.createWithChildren(.{}, .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(10)) }, .style = .bubble } }, &.{
                try Toybox.buildSexpr(.{ .pos = .new(-3, 0) }, .{ .atom_lit = "true" }, false, false, .new(@src())),
                try Toybox.buildScorer(.{ .pos = .new(0, 5) }, &.{ 0, 1 }, &.{ null, null }),
            }));
            Toybox.addChildLast(dst.main_area, bubble_1);
            const bubble_2 = try Toybox.buildBubble(.{ .pos = .new(30, 40) }, .zero, true, try Toybox.createWithChildren(.{}, .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(10)) }, .style = .bubble } }, &.{
                try Toybox.buildSexpr(.{ .pos = .new(-3, 0) }, .{ .atom_lit = "false" }, true, false, .new(@src())),
            }));
            Toybox.addChildLast(dst.main_area, bubble_2);
            dst.unlock_connections.appendAssumeCapacity(.{ .source = .nothing, .target = bubble_1, .condition = .always });
            dst.unlock_connections.appendAssumeCapacity(.{
                .source = bubble_1,
                .target = bubble_2,
                .condition = .all_scorers_solved,
            });
        }

        if (false) {
            Toybox.addChildLast(dst.main_area, try Toybox.buildScorer(.{ .pos = .new(20, 0) }, &.{ 0, 1 }));
        }

        if (false) {
            Toybox.addChildLast(
                dst.fnkboxes_layer,
                try Toybox.buildFnkbox(
                    .{ .pos = .new(-4, -8) },
                    try Toybox.buildSexpr(
                        .{},
                        .{ .atom_lit = "true" },
                        true,
                        true,

                        .new(@src()),
                    ),
                    false,
                    "do lowercase",
                    &.{
                        .{
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "A" }, false, false, .new(@src())),
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, .new(@src())),
                        },
                        .{
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "B" }, false, false, .new(@src())),
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                        },
                        .{
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "C" }, false, false, .new(@src())),
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, .new(@src())),
                        },
                    },
                    try Toybox.buildGarland(.{}, &.{
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "A" }, true, false, .new(@src())),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "B" }, false, false, .new(@src())),
                            .fnkname = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, .new(@src())),
                            .next = try Toybox.buildGarland(.{}, &.{
                                try Toybox.buildCase(.{}, .{
                                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, .new(@src())),
                                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, .new(@src())),
                                    .fnkname = null,
                                    .next = null,
                                }, .new(@src())),
                            }),
                        }, .new(@src())),
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "B" }, true, false, .new(@src())),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "C" }, false, false, .new(@src())),
                            .fnkname = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, true, .new(@src())),
                            .next = try Toybox.buildGarland(.{}, &.{
                                try Toybox.buildCase(.{}, .{
                                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, true, false, .new(@src())),
                                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                                    .fnkname = null,
                                    .next = null,
                                }),
                            }, .new(@src())),
                        }, .new(@src())),
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "C" }, true, false, .new(@src())),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, .new(@src())),
                            .fnkname = null,
                            .next = null,
                        }),
                    }, .new(@src())),
                ),
            );

            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = .new(0, 0) },
                .{ .atom_lit = "true" },
                false,
                false,

                .new(@src()),
            ));

            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = .new(0, 1) },
                .{ .atom_lit = "false" },
                false,
                false,

                .new(@src()),
            ));

            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = .new(3, 0) },
                .{ .pair = .{
                    .up = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, false, false, .new(@src())),
                    .down = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, .new(@src())),
                } },
                false,
                false,

                .new(@src()),
            ));

            Toybox.addChildLast(dst.main_area, try Toybox.buildCase(
                .{ .pos = .new(0, 4) },
                .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                },
            ));

            const case_1 = try Toybox.buildCase(.{}, .{
                .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true, false, .new(@src())),
                .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, .new(@src())),
                .fnkname = null,
                .next = null,
            });
            const case_2 = try Toybox.buildCase(.{}, .{
                .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true, false, .new(@src())),
                .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, .new(@src())),
                .fnkname = null,
                .next = null,
            });
            Toybox.addChildLast(dst.main_area, try Toybox.buildGarland(.{ .pos = .new(7, 4) }, &.{ case_1, case_2 }, .new(@src())));

            Toybox.addChildLast(dst.main_area, blk: {
                const postit = try Toybox.new(
                    .{ .pos = .new(3, 5, .new(@src())) },
                    .{ .postit = .{} },

                    .new(@src()),
                );
                Toybox.addChildLast(postit.index, (try Toybox.new(
                    .{ .pos = .new(0, 0, .new(@src())) },
                    .{ .postit_text = .{ .text = "hi" } },

                    .new(@src()),
                )).index);

                break :blk postit.index;
            });
        }

        if (false) {
            Toybox.addChildLast(dst.lenses_layer, try Toybox.buildMicroscope(
                .new(2, 2),
                .new(4, 3),
            ));

            Toybox.addChildLast(dst.lenses_layer, try Toybox.buildMicroscope(
                .new(4, 3),
                .new(6, 2),
            ));
        }

        if (false) {
            Toybox.addChildLast(dst.main_area, try Toybox.buildListViewer(
                .{ .pos = .new(30, -3) },
            ));
        }

        if (false) {
            Toybox.addChildLast(dst.main_area, try Toybox.buildMetaViewer(
                .{ .pos = .new(30, -3) },
            ));
        }

        if (false) { // add levels
            var pool: std.heap.MemoryPool(core.Sexpr) = .init(gpa);
            defer pool.deinit();
            var x: f32 = 100;
            const Sexpr = Lego.Specific.Sexpr;
            for (levels, 0..) |level, k| {
                defer _ = scratch.reset(.retain_capacity);
                const samples: []const [2]Lego.Index = blk: {
                    var samples_it = level.samplesIterator();
                    var samples: std.ArrayListUnmanaged([2]Lego.Index) = .empty;
                    while (try samples_it.next(&pool, scratch.allocator())) |item| {
                        try samples.append(scratch.allocator(), .{
                            try Sexpr.buildFromOldCoreValue(.{}, item.input, false, false, .new(@src())),
                            try Sexpr.buildFromOldCoreValue(.{}, item.expected, false, false, .new(@src())),
                        });
                        _ = pool.reset(.retain_capacity);
                    }
                    break :blk try samples.toOwnedSlice(scratch.allocator());
                };

                const fnkbox =
                    try Toybox.buildFnkbox(
                        .{ .pos = .new(x, if (k % 2 == 0) -6 else -5) },
                        try dst.findFnkname(level.fnk_name.atom_lit.value),
                        false,
                        level.description,
                        samples,
                        if (try level.initialDefinition(&pool, scratch.allocator())) |definition|
                            try Lego.Specific.Garland.buildFromOldCoreValue(.{}, definition, scratch.allocator(), .new(@src()))
                        else
                            null,
                    );
                Toybox.addChildLast(
                    dst.main_area,
                    fnkbox,
                );

                if (k == 0) {
                    fnkbox.children(.fnkbox).executor.children(.executor).controls.get().specific.executor_controls.brake().get().specific.executor_brake.brake_t = 0.9;
                    fnkbox.children(.fnkbox).box.children(.fnkbox_box).testcases_scrollbar.get().specific.scrollbar.scroll_target = 2;
                    fnkbox.children(.fnkbox).box.children(.fnkbox_box).testcases_scrollbar.get().specific.scrollbar.scroll_visual = 2;
                }

                if (level.fnk_name.isTheLit("calculator")) {
                    Toybox.addChildLast(dst.main_area, try Sexpr.buildFromOldCoreValue(
                        .{ .pos = .new(x, -8) },
                        try core.parsing.parseSingleSexpr("s(1 2 3 4 5 6 7 8 9)", &pool),
                        false,
                        false,
                    ), .new(@src()));
                }

                x += if (k < 4) 25 else if (k < 6) 30 else 35;
            }
        }

        if (false) { // tutorial postits
            var postit_pos: Vec2 = .new(470, -3);
            // dst.centerCameraAt(.{ .pos = postit_pos.add(.new(13, 8)), .scale = 4.5 * 2.75 }, true);

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = dst.main_area };

            if (false) {
                postit.addFromText(postit_pos, &.{ "Welcome", "to the lab!" });
                postit_pos.addInPlace(.new(12, 4));
                postit.addFromText(postit_pos, &.{ "Move around", "with WASD", "or Arrow Keys" });
                postit_pos.addInPlace(.new(-15, 5));
                postit.addFromText(postit_pos, &.{ "Left click", "to pick up", "Atoms ->" });
                postit_pos.addInPlace(.new(4.5, 1.25));
                Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                    .{ .pos = postit_pos },
                    .{ .atom_lit = "a" },
                    false,
                    false,

                    .new(@src()),
                ));
                Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                    .{ .pos = postit_pos.add(.new(5, -1.5)) },
                    .{ .atom_lit = "b" },
                    true,
                    false,

                    .new(@src()),
                ));
                Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                    .{ .pos = postit_pos.add(.new(-2, 4)) },
                    .{ .atom_lit = "C" },
                    false,
                    false,

                    .new(@src()),
                ));
                postit_pos.addInPlace(.new(5.5, 5.5));
                postit.addFromText(postit_pos, &.{ "Right click to", "duplicate them" });
                postit.addFromText(postit_pos.add(.new(6.5, 0.7)), &.{"Z to undo"});
            }

            postit_pos.addInPlace(.new(19, -14));
            postit.addFromText(postit_pos, &.{ "Your job:", "make machines", "that transform", "Atoms into", "other Atoms" });
            if (false) {
                postit_pos.addInPlace(.new(7, 0));
                postit.addFromText(postit_pos, &.{ "The piece below", "(when active)", "will match with", "the atom 'a'", "and transform it", "into 'b'" });
                Toybox.addChildLast(dst.main_area, Toybox.buildCase(.{ .pos = postit_pos.addY(5) }, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }));
            }
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromText(postit_pos, &.{ "The machine", "below, made of", "two pieces,", "will turn", "'a' into 'b',", "and 'b' into 'a'" });
            Toybox.addChildLast(dst.main_area, try Toybox.buildGarland(.{ .pos = postit_pos.addY(5) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, .new(@src())),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, .new(@src())),
                    .fnkname = null,
                    .next = null,
                }),
            }, .new(@src())));
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromText(postit_pos, &.{ "I will give you", "assignments.", "You must make", "a new machine to", "solve each one." });
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromParts(postit_pos.addY(-2), &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "That box     ", "is the first", "assignment,", "already solved", "as an example." } } },
                .{ .point = .{ .pos = .new(5, 1) }, .part = .arrow },
            });
            postit.addFromParts(postit_pos.add(.new(0.5, 4.5)), &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "Click the     ", "buttons to", "see it in action!" } } },
                .{ .point = .{ .pos = .new(4.7, 2) }, .part = .launch_testcase_button },
            });
            // postit.addFromText(postit_pos.add(.new(0.5, 4.5)), &.{ "Click the '>'", "buttons to", "see it in action!" });
            postit.addFromText(postit_pos.add(.new(3.5, 11.5)), &.{ "Use the crank", "and brake", "to control", "execution speed" });
            postit_pos.addInPlace(.new(25, -2));
            postit.addFromText(postit_pos, &.{"Your turn!"});
            postit.addFromText(postit_pos.add(.new(0.5, 6.5)), &.{ "Click the", "'Unsolved!'", "button to see", "an example", "where the", "machine fails" });
            postit.addFromText(postit_pos.add(.new(0.5, 6.5 * 2)), &.{ "and modify", "the machine", "to fix it" });
            postit_pos.addInPlace(.new(25, 0));
            postit_pos.addInPlace(.new(7, -6.1));
            postit.addFromText(postit_pos, &.{ "You can create", "new pieces", "by duplicating", "existing ones" });
            postit.addFromParts(postit_pos.addX(7), &.{
                .{ .point = .{ .pos = .new(3, 2) }, .part = .{ .paragraph = &.{ "(right click", "on the piece's", "circular center)" } } },
                .{ .point = (Point{ .pos = .new(2, 3) }).rotateAround(.both(3), 0.35).moveAbs(.new(0, 1.5)), .part = .arrow },
                .{ .point = .{ .pos = .new(3, 4.5) }, .part = .piece_center },
            });
            postit.addFromText(postit_pos.addX(7.5).addY(30), &.{ "You only need", "5 pieces!" });
            postit.addFromParts(postit_pos.addX(1).addY(24), &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "That    ", "is a Wildcard,", "which matches", "with everything" } } },
                .{ .point = .{ .pos = .new(4.5, 1.5) }, .part = .arrow },
            });
            postit_pos.addInPlace(.new(25, 1));
            postit.addFromText(postit_pos, &.{ "Use Wildcards", "to match", "any value", "and use it later" });
            postit.addFromText(postit_pos.addX(7), &.{ "You can grab", "fresh wildcards", "from the toolbar", "at the left border" });
            postit.addFromText(postit_pos.addX(14), &.{ "Remember,", "right click", "to duplicate." });
            postit.addFromText(postit_pos.addX(7).addY(Lego.Specific.FnkboxBox.box_height + 12), &.{ "Solve all the", "examples with", "a single piece!" });
            postit_pos.addInPlace(.new(25, -1));
            postit.addFromText(postit_pos, &.{ "Machines can", "invoke other", "machines" });
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromParts(postit_pos.addY(0.6), &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "Each machine", "has its own", "\"name\"" } } },
                .{ .point = .{ .pos = .new(0.9, 5.25), .turns = 0.25 }, .part = .arrow },
            });
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromParts(postit_pos, &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "That's the       ", "name of the first", "machine, the one", "that transforms", "'a' into 'A'" } } },
                .{ .point = .{ .pos = .new(5, 1) }, .part = .arrow },
            });
            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = postit_pos.add(.new(4, -2.5)), .scale = 0.5, .turns = 0.25 },
                .{ .atom_lit = @import("levels_new.zig").levels[0].fnk_name },
                false,
                true,

                .new(@src()),
            ));
            // postit.addFromText(postit_pos.add(.new(3, 8)), &.{ "The toolbar on the right", "has the name for", "every machine" });
            postit.addFromText(postit_pos.add(.new(6.5, 2.5)), &.{ "You can also", "find it", "on the toolbar", "on the right" });
            postit.addFromParts(postit_pos.add(.new(1.5, 16.75)), &.{
                .{ .point = .{ .pos = .new(3, 3) }, .part = .{ .paragraph = &.{ "Placed there,", "it will invoke", "the machine", "with that name" } } },
                .{ .point = .{ .pos = .new(1, 0.75), .turns = 0.5 }, .part = .arrow },
            });
            postit_pos.addInPlace(.new(30 - 14, 0));
            postit.addFromText(postit_pos, &.{ "Pieces can", "match with", "the result", "of other pieces" });
            postit.addFromText(postit_pos.addX(7), &.{ "Try running", "the first two", "examples." });
            postit.addFromText(postit_pos.addX(14).add(.new(4, 13)), &.{ "These 'nested'", "machines are", "the same as", "regular machines" });
            postit_pos.addInPlace(.new(33, 0));
            postit.addFromText(postit_pos, &.{ "You can combine", "both tricks:", "invoke a machine", "and then match", "on its result" });
            postit.addFromText(postit_pos.addX(7), &.{ "Study this", "solved", "assignment", "in detail." });
            postit_pos.addInPlace(.new(35, 0));
            postit.addFromText(postit_pos, &.{ "You now know", "everything!" });
            postit.addFromText(postit_pos.addX(7), &.{"Good luck."});

            postit_pos.addInPlace(.new(35, 0));
            postit_pos.addInPlace(.new(35, 0));
            postit_pos.addInPlace(.new(31, 0));
            postit.addFromText(postit_pos, &.{ "a \"list\" is an", "ordered collection", "of many values,", "compressed into", "a single one." });
            postit.addFromText(postit_pos.addX(7), &.{ "The top half", "is the first", "element; the", "bottom half", "is the rest", "of the list." });
            postit.addFromText(postit_pos.addX(14), &.{ "We use a special", "gray atom", "to mean", "an empty list." });
            postit.addFromText(postit_pos.addY(Lego.Specific.FnkboxBox.box_height + 18).addX(4), &.{ "hint: the second", "element is just", "the first element", "of the rest", "of the list" });
            postit.addFromText(postit_pos.addY(Lego.Specific.FnkboxBox.box_height + 18).addX(11), &.{ "In other words,", "the top half", "of the", "bottom half" });

            postit_pos.addInPlace(.new(35, 0));
            postit_pos.addInPlace(.new(-7, -7));
            postit.addFromText(postit_pos.addX(7), &.{ "The first example", "is an empty list." });
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "The second one", "is a list with only", "one element, 'a'." });
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "The third example", "is the list ['a', 'b']" });
            postit_pos.addInPlace(.new(-14, 7));
            postit.addFromText(postit_pos.addX(7), &.{ "The next one", "is ['a', 'b', 'c']" });
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "Careful!", "The first element", "of the", "next example", "is a pair of 2 Bs,", "not a B." });
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "The list does", "not have a", "B element." });

            postit_pos.addInPlace(.new(35 * 3, 0));
            postit_pos.addInPlace(.new(-4, 0));
            postit.addFromText(postit_pos, &.{ "You can create", "custom", "assignments", "by duplicating", "an existing one." });
            postit.addFromText(postit_pos.addX(7), &.{ "Try to invent", "machines that", "will be useful", "for multiple", "assignments." });
        }

        if (true) { // arrows for bubbles
            var cur = dst.main_area.get().tree.first;
            while (cur != nothing) : (cur = cur.get().tree.next) {
                if (!cur.hasTag(.bubble)) continue;
                const bubble = cur.get().specific.bubble;
                if (bubble.prev_bubble != nothing) {
                    Toybox.addChildFirst(dst.main_area, try Toybox.buildBubbleConnection(
                        bubble.prev_bubble,
                        cur,
                    ));
                }
                for (bubble.hint_for) |next_bubble| {
                    if (next_bubble == nothing) continue;
                    Toybox.addChildFirst(dst.main_area, try Toybox.buildBubbleConnection(
                        cur,
                        next_bubble,
                    ));
                }
            }
        }
    }

    fn buildBubbleSimple(bubble_pos: Vec2, prev: Lego.Index, comptime level_names: []const []const u8, postits: []const []const []const u8) !Lego.Index {
        const scorer_pos: Vec2 = .new(-7, 0);
        const postit_pos: []const Vec2 = if (level_names.len == 0) &.{
            .new(-8, -8),
            .new(-0.3, -7.2),
            .new(7.7, -7.1),
            .new(-7.2, -0.3),
            .new(0.15, 0.0),
            .new(7.8, 0.3),
            .new(-6.9, 7.3),
            .new(0.5, 7.5),
            .new(7.8, 7.9),
        } else &.{
            .new(-8, -8),
            .new(-0.3, -7.2),
            .new(7.7, -7.1),
            .new(-6.9, 5.8),
            .new(0.5, 5.9),
            .new(7.8, 5.8),
        };
        return try Toybox.buildBubble(.{ .pos = bubble_pos }, prev, .all_scorers_solved, blk: {
            const bp = try Toybox.new(
                .{},
                .{ .area = .{ .bg = .{ .local_rect = .fromCenterAndSize(.zero, .both(24)) }, .style = .bubble } },

                .new(@src()),
            );

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = bp };
            for (postits, 0..) |lines, k| {
                postit.addFromText(postit_pos[k], lines);
            }

            inline for (level_names, 0..) |level_name, k| {
                const level_index = levelIndex(level_name);
                const scorer = try Toybox.buildScorer(.{ .pos = scorer_pos.addY(5 * k) }, &.{level_index}, &.{.new(k * 4, 8.5 + tof32(k) * 2)});
                Toybox.addChildLast(bp, scorer);
            }

            break :blk bp;
        });
    }

    pub fn canonizeAfterChanges(workspace: *Workspace, scratch: std.mem.Allocator) !void {
        // when scrolling a lot of cases and tracy is active, this crashes :(
        const zone = tracy.initZone(@src(), .{ .name = "canonize after changes" });
        defer zone.deinit();

        const all_fnks: core.FnkCollection, const all_fnks_hash = try workspace.getAllFnks(scratch);

        const debug_all_bubbles_unlocked = workspace.debug_all_bubbles_unlocked;
        // TODO(design): this wouldn't be needed if we stored sexprs in the level file
        const debug_skip_has_sexpr_unlocks = all_fnks.count() > 0;

        // TODO(design-late): revisit
        var remaining_budget_for_unloaded_testcases: usize = 500;

        // reverse order so that fnkboxes get updated before scorers, and those before bubbles
        var lego_it = toybox.all_legos.iterator(toybox.all_legos.len - 1);
        while (lego_it.prev()) |lego| {
            if (!lego.exists) continue;
            if (workspace.isFreefloating(lego.index)) continue;
            if (lego.specific.tag() == .fnkbox) {
                try lego.specific.fnkbox.updateStatus(workspace, scratch, workspace.gpa_for_atom_names, all_fnks, all_fnks_hash, &remaining_budget_for_unloaded_testcases);
            }
            // TODO(optim): move this to interaction?
            if (lego.specific.tag() == .list_viewer) {
                try Lego.Specific.ListViewer.canonize(lego.index);
            }
            if (lego.specific.tag() == .meta_viewer) {
                try Lego.Specific.MetaViewer.canonize(lego.index, scratch);
            }
            if (lego.specific.tag() == .scorer) {
                try Lego.Specific.Scorer.updateStatus(workspace, lego.index, all_fnks, all_fnks_hash, scratch);
            }
            if (lego.specific.tag() == .bubble) {
                if (debug_all_bubbles_unlocked) {
                    lego.specific.bubble.locked = false;
                } else {
                    lego.specific.bubble.locked = lego.specific.bubble.locked and
                        if (lego.specific.bubble.prev_bubble.getSafe()) |prev|
                            prev.specific.bubble.locked or !prev.specific.bubble.fulfilled
                        else for (lego.specific.bubble.hint_for) |harder| {
                            if (harder == nothing) continue;
                            if (harder.get().specific.bubble.requested_hints) break false;
                        } else for (lego.specific.bubble.hint_for) |harder| {
                            if (harder != nothing) break true;
                        } else false;
                }

                const area = lego.index.children(.bubble).instanced;
                lego.specific.bubble.fulfilled = switch (lego.specific.bubble.goal) {
                    .all_scorers_solved => blk: {
                        var cur = area;
                        while (cur != nothing) : (cur = Toybox.next_preordered(cur, area).next) {
                            if (cur.hasTag(.scorer)) {
                                if (cur.get().specific.scorer.score == null) break :blk false;
                            }
                        }
                        break :blk true;
                    },
                    .has_sexpr => |sexpr| debug_skip_has_sexpr_unlocks or blk: {
                        var it = Toybox.treeIterator(area, false);
                        while (it.next()) |step| {
                            if (step.children_already_visited) continue;
                            const cur = step.index;
                            if (cur.hasTag(.postit)) {
                                it.skipChildren();
                            }
                            if (cur.hasTag(.sexpr)) {
                                if (Lego.Specific.Sexpr.equalValue(cur, sexpr) and
                                    cur.get().specific.sexpr.is_pattern == sexpr.get().specific.sexpr.is_pattern)
                                {
                                    break :blk true;
                                } else {
                                    it.skipChildren();
                                }
                            }
                        }
                        break :blk false;
                    },
                };
            }
        }
    }

    pub fn deinit(workspace: *Workspace) void {
        if (true) { // deinit all private arenas
            var it = toybox.private_arenas.valueIterator();
            while (it.next()) |v| v.deinit();
            toybox.private_arenas.clearRetainingCapacity();
        }
        workspace.arena_for_atom_names.deinit();
        workspace.arena_for_oneframe_data.deinit();
    }

    const HotAndDropzone = struct {
        hot: Lego.Index = .nothing,
        dropzone: Lego.Index = .nothing,
        over_background: Lego.Index,

        /// used when 'hot' is a CursorPoint
        text_index: ?usize = null,

        pub fn empty(x: @This()) bool {
            return x.hot == nothing and x.dropzone == nothing;
        }
    };
    fn findHotAndDropzone(workspace: *Workspace, absolute_needle_pos: Vec2) HotAndDropzone {
        const zone = tracy.initZone(@src(), .{ .name = "find hot" });
        defer zone.deinit();

        return _findHotAndDropzone(
            workspace.roots(.interactable).constSlice(),
            absolute_needle_pos,
            workspace.grabbing.index,
        );
    }

    fn _findHotAndDropzone(roots_in_draw_order: []const Lego.Index, absolute_needle_pos: Vec2, grabbing: Lego.Index) HotAndDropzone {
        var roots_it = std.mem.reverseIterator(roots_in_draw_order);
        while (roots_it.next()) |root| {
            var it = Toybox.treeIterator(root, false);
            while (it.next()) |step| {
                const cur = step.index;
                const lego = Toybox.get(cur);
                assert(lego.exists);
                const absolute_point = Point.inverseApplyToLocalPoint(lego.absolute_point, lego.visual_offset); // lego.absolute_point;
                const relative_needle_pos = absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos);
                const unhoverable = switch (lego.specific) {
                    else => false,
                    .area => |area| area.non_interactable,
                    .executor => |executor| executor.used_for_bg_computation,
                    .bubble => |bubble| bubble.locked,
                };
                if (unhoverable and !step.children_already_visited) {
                    it.skipChildren();
                    _ = it.next();
                    continue;
                }
                assert(!unhoverable);

                // TODO(optim): add a test to confirm that this always works
                const local_bounds = lego.localBoundingBoxThatContainsSelfAndAllChildren();
                const absolute_bounds = absolute_point.applyToLocalBounds(local_bounds);
                if (!absolute_bounds.contains(absolute_needle_pos)) {
                    it.skipChildren();
                    _ = it.next();
                    continue;
                }

                switch (lego.specific) {
                    .sexpr => |sexpr| {
                        if (!step.children_already_visited and
                            Lego.Specific.Sexpr.contains(absolute_point, sexpr.is_pattern, sexpr.kind, absolute_needle_pos))
                        {
                            if (grabbing == nothing and sexpr.kind != .empty) {
                                return .{ .hot = cur, .over_background = root };
                            } else if (grabbing != nothing and !lego.immutable and Toybox.get(grabbing).specific.tag() == .sexpr and
                                (OVERWRITING_TOPLEVEL_SEXPRS_ENABLED or sexpr.kind == .empty or
                                    sexpr.kind == .atom_var or grabbing.get().specific.sexpr.kind == .atom_var or
                                    !Toybox.isInATopLevelSexpr(lego.index)))
                            {
                                return .{ .dropzone = cur, .over_background = root };
                            }
                        }
                    },
                    .lens => |lens| {
                        if (step.children_already_visited and
                            lens.is_target and
                            absolute_point.inRange(absolute_needle_pos, lens.local_radius))
                        {
                            const interaction_nested = _findHotAndDropzone(
                                lens.roots_to_interact,
                                lens.transform.inverse().actOnPosition(absolute_needle_pos),
                                grabbing,
                            );
                            if (!interaction_nested.empty()) {
                                return interaction_nested;
                            }

                            // Avoid interacting with things hidden by the lens
                            return .{ .over_background = root };
                        }
                    },
                    .area => |area| {
                        if (step.children_already_visited and
                            area.bg.contains(absolute_point, absolute_needle_pos) and
                            (grabbing == nothing or Toybox.get(grabbing).tree.parent == nothing or Toybox.isAncestor(cur, grabbing)))
                        {
                            return .{ .over_background = cur };
                        }
                    },
                    .button => |button| {
                        if (step.children_already_visited and
                            button.enabled and
                            (grabbing == nothing or grabbing == cur) and
                            button.local_rect.contains(absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos)))
                        {
                            return .{ .hot = cur, .over_background = root };
                        }
                    },
                    .scrollbar => |scrollbar| {
                        if (step.children_already_visited and
                            grabbing == nothing and
                            scrollbar.handleRectVisual().contains(absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos)))
                        {
                            return .{ .hot = cur, .over_background = root };
                        }
                    },
                    .postit => {
                        if (!step.children_already_visited and
                            grabbing == nothing and
                            Lego.Specific.Postit.local_rect.contains(relative_needle_pos))
                        {
                            return .{ .hot = cur, .over_background = root };
                        }
                        if (!step.children_already_visited) {
                            it.skipChildren();
                        }
                    },
                    .microscope => |microscope| {
                        if (microscope.in_toolbar) {
                            if (grabbing == nothing) {
                                const lenses = Toybox.getChildrenExact(2, cur);
                                for (lenses) |lens_index| {
                                    const lens = lens_index.get().specific.lens;
                                    const parent_point = lens_index.get().absolute_point;
                                    if (parent_point.inRange(absolute_needle_pos, lens.local_radius)) {
                                        return .{ .hot = cur, .over_background = root };
                                    }
                                }
                            }

                            if (!step.children_already_visited) {
                                it.skipChildren();
                            }
                        }
                    },
                    .editable_textline => |editable_textline| {
                        if (Toybox.findAncestor(cur, .fnkbox).getSafe()) |f| {
                            if (!f.specific.fnkbox.editable) continue;
                        }
                        var best_index: usize = 0;
                        var best_dist: f32 = 1.0; // don't go too far, horizontally
                        var found_something = false;
                        for (editable_textline.cursor_points.items) |cursor_point| {
                            const p = lego.absolute_point.applyToLocalPoint(.{ .pos = cursor_point.relative_pos });
                            const asdf = p.inverseApplyGetLocalPosition(absolute_needle_pos);
                            // TODO(polish): ignore y position for already selected text
                            if (asdf.y > 0 or asdf.y < -cursor_point.relative_height) continue;
                            const cur_dist = @abs(asdf.x);
                            if (cur_dist < best_dist) {
                                best_dist = cur_dist;
                                best_index = cursor_point.index;
                                found_something = true;
                            }
                        }
                        if (found_something) {
                            return .{ .hot = cur, .text_index = best_index, .over_background = root };
                        }
                    },
                    .testcase => |t| {
                        if (!t.loaded) {
                            // if we got here, it means that the testcase hasn't been expanded in time,
                            //  or that the culling isn't working
                            panic("unexpected unloaded testcase while interacting!", .{});
                        }
                    },
                    .executor => |executor| {
                        if (executor.used_for_bg_computation) {
                            it.skipChildren();
                            _ = it.next();
                            continue;
                        }
                    },
                    // TODO(optim): check that scrollable_list ignores clipped elements
                    .scrollable_list,
                    .scrollable_list_inbetween,
                    .case,
                    .newcase,
                    .garland,
                    .garland_newcases,
                    .fnkbox,
                    .fnkbox_box,
                    .fnkslist_element,
                    .pill,
                    .postit_text,
                    .postit_drawing,
                    .executor_controls,
                    .executor_brake,
                    .executor_crank,
                    .list_viewer,
                    .meta_viewer,
                    .fnkname_holder,
                    .scorer,
                    .scorer_row,
                    .scorer_rows,
                    .bubble,
                    .bubble_connection,
                    => {},
                }
                if (step.children_already_visited) {
                    if (lego.handle()) |handle| {
                        const overlappable: bool, const kind: enum { hot, drop } = switch (lego.specific) {
                            .bubble,
                            .bubble_connection,
                            .scorer,
                            .scorer_row,
                            .scorer_rows,
                            .sexpr,
                            .area,
                            .microscope,
                            .editable_textline,
                            .scrollable_list,
                            .fnkslist_element,
                            .button,
                            .scrollbar,
                            .executor,
                            .fnkbox_box,
                            .testcase,
                            .pill,
                            .postit,
                            .postit_text,
                            .postit_drawing,
                            .executor_controls,
                            .garland_newcases,
                            .fnkname_holder,
                            => unreachable,
                            .case, .lens, .fnkbox, .list_viewer, .meta_viewer, .executor_brake, .executor_crank => .{ grabbing == nothing, .hot },
                            .newcase => .{ grabbing != nothing and Toybox.get(grabbing).specific.tag() == .case and !lego.immutable, .drop },
                            .scrollable_list_inbetween => |t| .{ grabbing != nothing and switch (t.kind) {
                                .listviewer_sexprs => Toybox.get(grabbing).specific.tag() == .sexpr,
                            }, .drop },
                            .garland => if (grabbing == nothing)
                                .{ true, .hot }
                            else if (Toybox.get(grabbing).specific.tag() == .garland)
                                .{ !lego.immutable, .drop }
                            else
                                .{ false, undefined },
                        };

                        if (overlappable and handle.overlapped(absolute_needle_pos)) {
                            switch (kind) {
                                .hot => return .{ .hot = cur, .over_background = root },
                                .drop => return .{ .dropzone = cur, .over_background = root },
                            }
                        }
                    }
                }
            }
        }

        if (grabbing != nothing) {
            if (grabbing.get().tree.parent == nothing) {
                std.debug.panic("grabbing {d} has no parent!", .{grabbing.index});
            }
            return .{ .over_background = Toybox.oldestAncestor(grabbing) };
        } else {
            unreachable;
        }
    }

    fn dragGrabbing(grabbing: Grabbing, active_text_selection: *TextSelection, absolute_mouse_pos: Vec2, interaction: HotAndDropzone, delta_seconds: f32) void {
        if (grabbing.index == nothing) return;
        const cur = grabbing.index;
        const lego = Toybox.get(cur);
        if (lego.draggable()) {
            switch (lego.specific) {
                .sexpr => |*sexpr| {
                    const target: Point = if (Toybox.safeGet(interaction.dropzone)) |dropzone|
                        dropzone.absolute_point.applyToLocalPoint(.{ .pos = dropzone.handleLocalOffset() })
                    else
                        // i don't like the scale hack
                        (Point{
                            .pos = absolute_mouse_pos,
                            .scale = Toybox.get(interaction.over_background).absolute_point.scale * @as(f32, if (sexpr.is_fnkname) 0.5 else 1),
                            .turns = if (sexpr.is_fnkname) 0.25 else 0,
                        })
                            .applyToLocalPoint(.{ .pos = lego.handleLocalOffset().neg() })
                            .applyToLocalPoint(.{ .pos = grabbing.offset.neg() });

                    const local_target = Toybox.parentAbsolutePoint(cur).inverseApplyGetLocal(target);
                    // const new_local = lego.local_point.lerpTowardsPure(local_target, .old, delta_seconds);

                    // TODO(game): improve and simplify
                    const gravity_center: Vec2 = .xpos;
                    const turns = Vec2.getTurnsBetween(
                        lego.local_point.applyToLocalPoint(lego.visual_offset).applyToLocalPosition(gravity_center),
                        lego.local_point.pos,
                        local_target.pos,
                    );
                    sexpr.jiggling_t = math.clamp(math.maybeMirror(turns, sexpr.is_pattern), -0.1, 0.1);

                    // _ = turns;
                    Toybox.setLocalPointSmooth(lego.index, local_target);
                    // lego.local_point.lerp_towards(local_target, 0.2, delta_seconds);
                    // lego.local_point.lerp_towards(local_target.plusTurns(math.clamp(math.maybeMirror(turns, sexpr.is_pattern), -0.1, 0.1)), 0.6, delta_seconds);
                    // lego.local_point.lerp_towards(local_target.plusTurns(math.clamp(turns, -0.1, 0.1)), 0.6, delta_seconds);
                    // lego.local_point = local_target.plusTurns(turns);
                    // lego.local_point = local_target;

                    if (Toybox.safeGet(interaction.dropzone)) |dropzone| {
                        const dropzone_is_pattern = switch (dropzone.specific) {
                            .sexpr => |s| s.is_pattern,
                            .scrollable_list_inbetween => |t| switch (t.kind) {
                                .listviewer_sexprs => false,
                            },
                            else => unreachable,
                        };
                        if (dropzone_is_pattern != sexpr.is_pattern) {
                            Lego.Specific.Sexpr.setIsPattern(cur, dropzone_is_pattern);
                        }

                        const dropzone_is_fnkname = switch (dropzone.specific) {
                            .sexpr => |s| s.is_fnkname,
                            .scrollable_list_inbetween => |t| switch (t.kind) {
                                .listviewer_sexprs => false,
                            },
                            else => unreachable,
                        };
                        if (dropzone_is_pattern != sexpr.is_pattern) {
                            Lego.Specific.Sexpr.setIsPattern(cur, dropzone_is_pattern);
                        }
                        if (dropzone_is_fnkname != sexpr.is_fnkname) {
                            var cur_sexpr = cur;
                            while (cur_sexpr != nothing) : (cur_sexpr = Toybox.next_preordered(cur_sexpr, cur).next) {
                                Toybox.get(cur_sexpr).specific.sexpr.is_fnkname = dropzone_is_fnkname;
                                var cur_child = Toybox.get(cur_sexpr).specific.sexpr.emerging_value;
                                while (cur_child != nothing) : (cur_child = Toybox.next_preordered(cur_child, cur_sexpr).next) {
                                    Toybox.get(cur_child).specific.sexpr.is_fnkname = dropzone_is_fnkname;
                                }
                            }
                        }
                    }
                },
                else => {
                    const target: Point = if (Toybox.safeGet(interaction.dropzone)) |dropzone|
                        dropzone.absolute_point.applyToLocalPoint(.{ .pos = dropzone.handleLocalOffset() })
                    else
                        // i don't like the scale hack
                        (Point{
                            .pos = absolute_mouse_pos,
                            .scale = Toybox.get(interaction.over_background).absolute_point.scale,
                        })
                            .applyToLocalPoint(.{ .pos = lego.handleLocalOffset().neg() })
                            .applyToLocalPoint(.{ .pos = grabbing.offset.neg() });

                    Toybox.setLocalPointSmooth(lego.index, Toybox.parentAbsolutePoint(cur)
                        .inverseApplyGetLocal(target));
                    // lego.local_point.lerp_towards(Toybox.parentAbsolutePoint(cur)
                    //     .inverseApplyGetLocal(target), 0.6, delta_seconds);
                    // lego.local_point = Toybox.parentAbsolutePoint(cur)
                    //     .inverseApplyGetLocal(target);
                },
                .button => |button| switch (button.action) {
                    else => {},
                    .scroll_up => {
                        lego.tree.parent.get().specific.scrollbar.scroll_target -= delta_seconds / 0.2;
                    },
                    .scroll_down => {
                        lego.tree.parent.get().specific.scrollbar.scroll_target += delta_seconds / 0.2;
                    },
                },
                .scrollbar => |*scrollbar| {
                    const local_pos = lego.absolute_point
                        .inverseApplyGetLocalPosition(absolute_mouse_pos);
                    scrollbar.onMouseMoved(local_pos.sub(grabbing.offset));
                },
                .executor_brake => |*brake| {
                    assert(interaction.dropzone == nothing);
                    const local_pos = lego.absolute_point.inverseApplyGetLocalPosition(absolute_mouse_pos);
                    const S = struct {
                        p: Vec2,
                        pub fn score(ctx: @This(), t: f32) f32 {
                            return Lego.Specific.Executor.Controls.brakeHandlePath(t).sub(ctx.p).magSq();
                        }
                    };
                    const raw_t = kommon.funktional.findFunctionMin(
                        S,
                        .{ .p = local_pos },
                        0,
                        1,
                        10,
                        0.0001,
                    );
                    // math.lerp_towards(&brake.brake_t, raw_t, 0.6, delta_seconds);
                    math.towards(&brake.brake_t, raw_t, delta_seconds * 5);
                },
                .executor_crank => |*crank| {
                    assert(interaction.dropzone == nothing);
                    const local_pos = lego.absolute_point.inverseApplyGetLocalPosition(absolute_mouse_pos);
                    const raw_t = local_pos.getTurns();
                    const executor = &Toybox.findAncestor(cur, .executor).get().specific.executor;
                    const cur_t = executor.animation.?.t;
                    const target_t = math.clamp01(math.mod(raw_t, cur_t - 0.5, cur_t + 0.5));
                    // math.lerp_towards(&crank.t, @max(0, target_t), 0.6, delta_seconds);
                    math.towards(&crank.value, target_t, delta_seconds * 5);
                    executor.animation.?.t = crank.value;
                },
                .editable_textline => {
                    if (interaction.text_index) |i| {
                        active_text_selection.cursor = i;
                    }
                },
            }
            Toybox.refreshAbsolutePoints(&.{grabbing.index});
        }
    }

    fn updateSprings(workspace: *Workspace, roots_in_draw_order: []const Lego.Index, interaction: HotAndDropzone, delta_seconds: f32) void {
        const asdf = tracy.initZone(@src(), .{ .name = "updateSprings" });
        defer asdf.deinit();

        for (roots_in_draw_order) |root| {
            var cur: Lego.Index = root;
            var next: Lego.Index = undefined;
            while (cur != nothing) : (cur = next) {
                const lego = Toybox.get(cur);
                defer lego.absolute_point = Toybox.parentAbsolutePoint(cur).applyToLocalPoint(lego.local_point);

                const skip = switch (cur.get().specific) {
                    else => false,
                    .area => |area| area.non_interactable,
                    .executor => |executor| executor.used_for_bg_computation,
                };

                if (skip) {
                    next = cur.get().tree.next;
                } else {
                    next = Toybox.next_preordered(cur, root).next;
                }

                // inherit immutability from parent, sometimes
                switch (lego.specific) {
                    else => {},
                    .garland, .case, .newcase, .sexpr, .garland_newcases, .fnkname_holder => if (lego.tree.parent.getSafe()) |p| switch (p.specific.tag()) {
                        else => {},
                        .garland, .case, .sexpr, .newcase, .testcase, .garland_newcases, .area, .fnkname_holder => {
                            if (p.specific.tag() == .garland and lego.specific.tag() == .sexpr) {
                                // special case: ignore sexpr with garland parent (it's the fnkname)
                                assert(lego.immutable);
                            } else if (p.specific.tag() == .testcase and p.specific.testcase.source != null) {
                                // special case: for now, builtin cases are inmutable
                                lego.immutable = true;
                            } else if (p.specific.tag() == .testcase and p.index.children(.testcase).actual == cur) {
                                // special case: the 'actual' sexpr in a testcase is always immutable
                                lego.immutable = true;
                            } else {
                                lego.immutable = p.immutable;
                            }
                        },
                    },
                }

                switch (lego.specific) {
                    .sexpr => |*sexpr| {

                        // TODO(optim): skip children in most cases

                        if (sexpr.emerging_value != nothing) {
                            Toybox.refreshAbsolutePoints(&.{cur});
                            const t = sexpr.emerging_value_t;
                            const offset: Point = if (sexpr.is_pattern)
                                .{}
                            else
                                .{ .pos = .new(math.remap(
                                    t,
                                    0,
                                    1,
                                    -2.3,
                                    0,
                                ), 0) };
                            Toybox.get(sexpr.emerging_value).local_point = lego.absolute_point.applyToLocalPoint(offset);
                            updateSprings(workspace, &.{sexpr.emerging_value}, interaction, delta_seconds);
                        }
                    },
                    .case => {
                        // TODO(optim): this is needed since undoing a half-done anim doesn't properly restore all the local positions of the case parts
                        Lego.Specific.Case.updateLocalPositions(cur);
                    },
                    .garland_newcases => {
                        var a = Toybox.get(lego.tree.first);
                        var offset: f32 = 0;
                        while (true) {
                            assert(a.specific.tag() == .newcase);
                            a.local_point = .{ .pos = .new(0, offset) };
                            offset += a.specific.newcase.length();

                            if (a.tree.next == nothing) break;
                            a = Toybox.get(a.tree.next);
                        }
                        lego.tree.parent.get().specific.garland.computed_height = offset;
                    },
                    .newcase => |*newcase| {
                        const Garland = Lego.Specific.Garland;

                        const is_first = lego.tree.prev == nothing;
                        const is_last = lego.tree.next == nothing;

                        const extra_before_offset_for_anim: f32 = if (newcase.offset_ghost == nothing)
                            0
                        else
                            newcase.offset_t * (Lego.Specific.Garland.dist_between_cases_rest * 0.5 +
                                newcase.offset_ghost.get().specific.case.next().computed_height);

                        const extra_after_offset_for_anim: f32 = if (newcase.offset_ghost == nothing or !is_first)
                            0
                        else
                            0.5 * newcase.offset_t * (Garland.dist_between_cases_rest - Garland.dist_between_cases_first);

                        const maybe_child_case: Lego.Index = if (lego.tree.first != nothing) blk: {
                            assert(lego.tree.last == lego.tree.first);
                            assert(lego.tree.next != nothing);
                            assert(Toybox.get(lego.tree.first).specific.tag() == .case);
                            break :blk lego.tree.first;
                        } else blk: {
                            assert(lego.tree.next == nothing);
                            break :blk .nothing;
                        };

                        const base_len = if (is_first) Garland.dist_between_cases_first else Garland.dist_between_cases_rest;

                        const extra_prev_height: f32 = if (is_first) 0 else blk: {
                            const case_of_prev_segment = Toybox.get(lego.tree.prev).tree.first;
                            assert(case_of_prev_segment.get().specific.tag() == .case);
                            const garland_of_case_of_prev_segment = Toybox.get(case_of_prev_segment).tree.last;
                            assert(garland_of_case_of_prev_segment.get().specific.tag() == .garland);
                            const prev_height = if (garland_of_case_of_prev_segment == interaction.dropzone)
                                Toybox.get(workspace.grabbing.index).specific.garland.computed_height
                            else
                                Toybox.get(garland_of_case_of_prev_segment).specific.garland.computed_height;
                            break :blk prev_height - Garland.dist_between_cases_first * 0.5;
                        };

                        const height_of_case_hovered: f32 = if (interaction.dropzone != cur)
                            0
                        else
                            workspace.grabbing.index.get().specific.case.next().computed_height - Garland.dist_between_cases_first * 0.5;

                        const target_length_before: f32 = extra_before_offset_for_anim + base_len * 0.5 + extra_prev_height + base_len * 0.5 * lego.dropzone_t;
                        const target_length_after: f32 = if (is_last) 0.0 else (extra_after_offset_for_anim + base_len * 0.5 +
                            lego.dropzone_t * (height_of_case_hovered + 0.5 * (if (!is_first)
                                Garland.dist_between_cases_rest
                            else
                                Garland.dist_between_cases_rest + (Garland.dist_between_cases_rest - Garland.dist_between_cases_first))));

                        // const must_be_this_length = math.lerpTowardsPure(
                        //     newcase.length_before + newcase.length_after,
                        //     target_length_after + target_length_before,
                        //     .slow,
                        //     delta_seconds,
                        // );

                        math.lerpTowards(&newcase.length_before, target_length_before, .slow, delta_seconds);
                        math.lerpTowards(&newcase.length_after, target_length_after, .slow, delta_seconds);

                        // const error_length = newcase.length_before + newcase.length_after - must_be_this_length;
                        // if (@abs(error_length) > 0.001) std.log.debug("error {d}", .{error_length});
                        // newcase.length_before -= error_length / 6.0;
                        // newcase.length_after -= error_length * 5.0 / 6.0;
                        // assert(@abs(newcase.length_after + newcase.length_before - must_be_this_length) < 0.0001);

                        if (Toybox.safeGet(maybe_child_case)) |case| case.local_point = .{ .pos = .new(0, newcase.length()) };
                    },
                    .fnkbox_box => {},
                    .executor => |executor| {
                        const Executor = Lego.Specific.Executor;
                        const children = Executor.children(cur);

                        if (executor.animation) |animation| {
                            animation.active_case.get().immutable = true;
                            animation.garland_fnkname.get().immutable = true;
                            if (animation.invoked_fnk.getSafe()) |s| s.immutable = true;
                            children.input.get().immutable = true;
                            children.garland.get().immutable = true;
                        } else {
                            children.input.get().immutable = false;
                            children.garland.get().immutable = false;
                        }

                        var pill_offset: f32 = 0;
                        if (executor.animation) |animation| {
                            const anim_t = math.clamp01(animation.t);
                            if (!animation.matching) { // match failed, draw case being discarded and next ones coming up
                                const match_t = math.remapClamped(anim_t, 0, 0.2, 0, 1);
                                const flyaway_t = math.remapClamped(anim_t, 0.2, 0.8, 0, 1);
                                const offset_t = math.remapClamped(anim_t, 0.2, 0.8, 1, 0);

                                const case_floating_away = Executor.first_case_point
                                    .applyToLocalPoint(Point.lerp(
                                    .{ .pos = .new(-match_t, 0) },
                                    .{ .pos = .new(6, -2), .scale = 0, .turns = -0.2 },
                                    flyaway_t,
                                ));
                                Toybox.get(children.garland).local_point = Executor.relative_garland_point;
                                Toybox.get(children.garland).specific.garland.firstNewcase().offset_t = offset_t;
                                Toybox.get(children.garland).specific.garland.firstNewcase().offset_ghost = animation.active_case;
                                Toybox.setAbsolutePoint(animation.active_case, lego.absolute_point.applyToLocalPoint(case_floating_away));
                                Toybox.get(children.input).local_point = Executor.relative_input_point;
                                Toybox.setAbsolutePoint(animation.garland_fnkname, lego.absolute_point
                                    .applyToLocalPoint(Toybox.get(children.input).local_point)
                                    .applyToLocalPoint(.{ .pos = .new(3, -1.5), .turns = 0.25, .scale = 0.5 }));

                                if (true) { // update enqueued garlands
                                    var enqueued = executor.first_enqueued;
                                    var k: usize = 0;
                                    while (enqueued != nothing) : ({
                                        enqueued = enqueued.get().specific.garland.next_enqueued;
                                        k += 1;
                                    }) {
                                        Toybox.setAbsolutePoint(enqueued, lego.absolute_point.applyToLocalPoint(Executor.relative_garland_point.applyToLocalPoint(
                                            Lego.Specific.Garland.extraForDequeuingNext(tof32(k + 1)),
                                        )));
                                    }
                                }
                            } else { // match succeeded
                                const match_t = math.remapClamped(anim_t, 0, 0.2, 0, 1);
                                // const bindings_t: ?f32 = if (anim_t < 0.2) null else math.remapTo01Clamped(anim_t, 0.2, 0.8);
                                const invoking_t = math.remapClamped(anim_t, 0.0, 0.7, 0, 1);
                                const enqueueing_t = math.remapClamped(anim_t, 0.2, 1, 0, 1);
                                const discarded_t = math.remapClamped(anim_t, 0.2, 1, 0, 1);
                                pill_offset = enqueueing_t;

                                if (!EXECUTOR_MOVES_LEFT) {
                                    executor.handle.point.pos = animation.original_point.pos.addX(enqueueing_t * 5);
                                }

                                const case_point = Executor.first_case_point.applyToLocalPoint(
                                    .{ .pos = .new(-match_t - enqueueing_t * 5, 0) },
                                );
                                Toybox.get(children.garland).local_point = Executor.relative_garland_point
                                    .applyToLocalPoint(.lerp(.{}, .{ .turns = 0.2, .scale = 0, .pos = .new(-4, 8) }, discarded_t));
                                Toybox.get(children.garland).specific.garland.firstNewcase().offset_ghost = animation.active_case;
                                Toybox.get(children.garland).specific.garland.firstNewcase().offset_t = 1;

                                Toybox.get(animation.active_case).specific.case.fnkname_holder_extra = .{ .pos = .new(0, -invoking_t * 2) };
                                if (animation.invoked_fnk != nothing) {
                                    const offset = (1.0 - invoking_t) + 2.0 * math.smoothstepEased(invoking_t, 0.4, 0.0, .linear);
                                    const function_point = lego.absolute_point.applyToLocalPoint(Lego.Specific.Executor.relative_garland_point)
                                        .applyToLocalPoint(.{ .pos = .new(2 * offset + 6 - match_t - enqueueing_t * 5, 6 * offset) });

                                    Toybox.setAbsolutePoint(animation.invoked_fnk, function_point);

                                    Toybox.get(animation.active_case).specific.case.next_point_extra = Lego.Specific.Garland.extraForEnqueuingNext(enqueueing_t);
                                    Toybox.setAbsolutePoint(animation.active_case, lego.absolute_point.applyToLocalPoint(case_point));

                                    const enqueueing = animation.active_case.case().next.garland().hasChildCases();
                                    if (true) { // update enqueued garlands
                                        var enqueued = executor.first_enqueued;
                                        var k: usize = 0;
                                        while (enqueued != nothing) : ({
                                            enqueued = enqueued.get().specific.garland.next_enqueued;
                                            k += 1;
                                        }) {
                                            Toybox.setAbsolutePoint(enqueued, lego.absolute_point.applyToLocalPoint(Executor.relative_garland_point.applyToLocalPoint(
                                                Lego.Specific.Garland.extraForDequeuingNext(tof32(k + 1) + if (enqueueing) enqueueing_t else 0),
                                            )));
                                        }
                                    }
                                } else {
                                    Toybox.setAbsolutePoint(animation.active_case, lego.absolute_point.applyToLocalPoint(case_point));
                                    Toybox.get(animation.active_case).specific.case.next_point_extra = .{
                                        .pos = .new(-enqueueing_t * 2, -(Lego.Specific.Case.next_garland_offset.y + Lego.Specific.Garland.dist_between_cases_first) *
                                            math.smoothstep(enqueueing_t, 0, 0.6)),
                                    };

                                    const dequeueing = !animation.active_case.case().next.garland().hasChildCases();
                                    if (true) { // update enqueued garlands
                                        var enqueued = executor.first_enqueued;
                                        var k: usize = 0;
                                        while (enqueued != nothing) : ({
                                            enqueued = enqueued.get().specific.garland.next_enqueued;
                                            k += 1;
                                        }) {
                                            Toybox.setAbsolutePoint(enqueued, lego.absolute_point.applyToLocalPoint(Executor.relative_garland_point.applyToLocalPoint(
                                                Lego.Specific.Garland.extraForDequeuingNext(tof32(k + 1) - if (dequeueing) enqueueing_t else 0),
                                            )));
                                        }
                                    }
                                }
                                Toybox.get(children.input).local_point = Executor.relative_input_point.applyToLocalPoint(.{ .pos = .new(-enqueueing_t * 5, 0) });
                                Toybox.setAbsolutePoint(animation.garland_fnkname, lego.absolute_point
                                    .applyToLocalPoint(Toybox.get(children.input).local_point)
                                    .applyToLocalPoint(.{ .pos = .new(3, -1.5), .turns = 0.25, .scale = 0.5 }));
                            }
                        } else {
                            Toybox.get(children.input).local_point = Executor.relative_input_point;
                            Toybox.get(children.garland).local_point = Executor.relative_garland_point;
                        }

                        if (true) { // update pills
                            var pill = executor.first_pill;
                            var k: usize = 0;
                            while (pill != nothing) : ({
                                pill = pill.get().specific.pill.next_pill;
                                k += 1;
                            }) {
                                Toybox.setAbsolutePoint(pill, lego.absolute_point.applyToLocalPoint(
                                    Executor.relative_input_point.applyToLocalPoint(
                                        .{ .pos = .new(-5 * (tof32(k) + pill_offset) - 2, 0) },
                                    ),
                                ));
                            }
                        }
                    },
                    .executor_controls => {},
                    .executor_brake => |*brake| {
                        lego.local_point = .{};
                        brake.handle_pos = Lego.Specific.Executor.Controls.brakeHandlePath(brake.brake_t);
                    },
                    .executor_crank => |*crank| {
                        lego.local_point = .{};
                        crank.handle_pos = .fromPolar(0.75, crank.value);
                    },
                    .scrollable_list => |scrollable_list| {
                        const scroll_visual = cur.scrollbar(.scrollable_list).get().specific.scrollbar.scroll_visual;
                        const delta_scroll = cur.scrollbar(.scrollable_list).get().specific.scrollbar.scrollVisualDelta();

                        var height: f32 = 0;
                        var cur_element: Lego.Index = lego.tree.first;
                        var y: f32 = -scroll_visual;
                        while (cur_element != nothing) {
                            cur_element.get().local_point.pos.y += delta_scroll * scrollable_list.spacing();

                            if (scrollable_list.instantUpdates()) {
                                cur_element.get().local_point = .{ .pos = scrollable_list.base()
                                    .addY(scrollable_list.spacing() * y), .scale = scrollable_list.elementScale() };
                            } else {
                                Toybox.setLocalPointSmooth(cur_element, .{ .pos = scrollable_list.base()
                                    .addY(scrollable_list.spacing() * y), .scale = scrollable_list.elementScale() });
                            }
                            y += if (cur_element.hasTag(.scrollable_list_inbetween)) 0.5 * cur_element.get().dropzone_t else 1.0;
                            cur_element = Toybox.get(cur_element).tree.next;
                            height += 1;
                        }

                        if (scrollable_list.kind == .fnkbox_testcases) {
                            cur.scrollbar(.scrollable_list).get().specific.scrollbar.total_length = height;
                        }
                    },
                    .scrollbar => |*scrollbar| {
                        scrollbar.prev_scroll_visual = scrollbar.scroll_visual;
                        math.lerpTowardsRange(&scrollbar.scroll_target, 0, @max(0, scrollbar.total_length - scrollbar.visible_length), .slow, delta_seconds);
                        math.lerpTowards(&scrollbar.scroll_visual, scrollbar.scroll_target, .slow, delta_seconds);
                    },
                    .fnkbox => |fnkbox| {
                        cur.children(.fnkbox).fnkname.get().immutable = true;
                        if (fnkbox.execution) |execution| {
                            cur.children(.fnkbox).executor.children(.executor).garland.get().immutable = true;
                            if (execution.floating_input_or_output.getSafe()) |s| s.immutable = true;
                        }
                    },
                    .garland => {
                        if (lego.index.children(.garland).fnkname.getSafe()) |f| f.immutable = true;
                    },
                    .scrollable_list_inbetween,
                    .list_viewer,
                    .meta_viewer,
                    .fnkslist_element,
                    .testcase,
                    .pill,
                    .area,
                    .microscope,
                    .lens,
                    .button,
                    .editable_textline,
                    .postit,
                    .postit_text,
                    .postit_drawing,
                    .fnkname_holder,
                    .scorer,
                    .scorer_row,
                    .scorer_rows,
                    .bubble,
                    .bubble_connection,
                    => {},
                }
            }
        }
    }

    fn draw(workspace: *Workspace, platform: PlatformGives, drawer: *Drawer) !void {
        const zone = tracy.initZone(@src(), .{ .name = "draw" });
        defer zone.deinit();

        const camera = Rect
            .fromCenterAndSize(.zero, .both(2))
            .withAspectRatio(platform.aspect_ratio, .grow, .center);

        drawer.canvas.clipper.reset();
        drawer.canvas.clipper.use(drawer.canvas);

        if (!workspace.debug_nodraw) {
            try _draw(workspace.roots(.all).constSlice(), if (Toybox.safeGet(workspace.grabbing.index)) |lego|
                lego.specific.tag() == .sexpr
            else
                false, camera, drawer, workspace.active_text_input, workspace.active_text_selection);
        }

        if (workspace.display_fps) try drawer.canvas.drawText(
            0,
            camera,
            try std.fmt.allocPrint(drawer.canvas.frame_arena.allocator(), "fps: {d:.5}", .{1.0 / platform.delta_seconds}),
            .{
                .pos = camera.top_left,
                .hor = .left,
                .ver = .ascender,
            },
            camera.size.y * 0.05,
            .black,
        );
    }

    // TODO(game): emerging values seem 1-frame delayed, can easily be seen in the queuing anim for "@a -> x: b { c -> @a; }"
    fn _draw(
        roots_in_draw_order: []const Lego.Index,
        holding_a_sexpr: bool,
        camera: Rect,
        drawer: *Drawer,
        active_text_input: Lego.Index,
        active_text_selection: TextSelection,
    ) !void {
        for (roots_in_draw_order) |root| {
            var inside_postit = false;
            var it = Toybox.treeIterator(root, true);
            while (it.next()) |step| {
                const cur = step.index;
                const lego = Toybox.get(cur);
                const alpha: f32 = switch (lego.specific) {
                    .sexpr, .garland, .case, .newcase => if (Toybox.safeGet(Toybox.findAncestor(cur, .executor))) |g|
                        @max(0, g.specific.executor.garland_appearing_t)
                    else if (Toybox.safeGet(Toybox.findAncestor(cur, .pill))) |p|
                        p.specific.pill.alpha()
                    else
                        1,
                    else => 1,
                };

                // TODO(polish): improve
                const max_resolution = 2000;
                const local_bounds = lego.localBoundingBoxThatContainsSelfAndAllChildren();
                const absolute_bounds = lego.absolute_point.applyToLocalBounds(local_bounds);
                if (camera.asBounds().intersect(absolute_bounds) == null or
                    camera.size.div(absolute_bounds.size()).normLInf() > max_resolution)
                {
                    it.skipChildren();
                    _ = it.next();
                    continue;
                }

                // if (lego.specific.tag() == .pill) {
                //     // std.log.debug("abs pos: {any}", .{lego.absolute_point});
                //     std.log.debug("abs pos of first child: {any}", .{lego.tree.first.get().absolute_point});
                // }

                const camera_relative = camera.reparentCamera(lego.absolute_point);
                if (step.children_already_visited) {
                    const zone2 = tracy.initZone(@src(), .{ .name = "draw going up" });
                    defer zone2.deinit();

                    if (false and lego.specific.tag() == .sexpr) { // draw numbers
                        try drawer.canvas.drawText(
                            0,
                            camera,
                            try std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{@intFromEnum(cur)}),
                            .centeredAt(lego.absolute_point.pos),
                            1 * lego.absolute_point.scale,
                            .black,
                        );
                    }
                    if (lego.handle()) |handle| try handle.draw(drawer, camera, alpha);
                    switch (lego.specific) {
                        .scrollable_list => |scrollable_list| if (scrollable_list.clip()) {
                            drawer.canvas.clipper.pop();
                            drawer.canvas.clipper.use(drawer.canvas);
                        },
                        .fnkbox_box => {
                            drawer.canvas.borderRect(camera_relative, Lego.Specific.FnkboxBox.relative_box, 0.05, .inner, .black);
                        },
                        .postit => {
                            assert(inside_postit);
                            inside_postit = false;
                        },
                        .bubble => |bubble| {
                            if (bubble.remaining_reset_anim_t > 0) {
                                const area = bubble.blueprint.get().specific.area;
                                switch (area.bg) {
                                    .all, .none => unreachable,
                                    .local_rect => |rect| {
                                        drawer.canvas.fillRect(camera, lego.absolute_point.applyToLocalRect(rect).plusMargin(-0.25 * lego.absolute_point.scale), FColor.gray(0.5).withAlpha(bubble.remaining_reset_anim_t));
                                    },
                                }
                            }
                        },
                        else => {},
                    }
                } else {
                    const zone2 = switch (lego.specific) {
                        inline else => |_, t| tracy.initZone(@src(), .{ .name = "draw_" ++ @tagName(t) }),
                    };
                    defer zone2.deinit();

                    const point = lego.absolute_point;
                    switch (lego.specific) {
                        .case => {
                            // TODO(game): draw variables in the cable
                            drawer.canvas.line(camera, &.{
                                lego.absolute_point.applyToLocalPosition(.xneg),
                                lego.absolute_point.applyToLocalPosition(.xpos),
                            }, 0.05 * lego.absolute_point.scale, .blackAlpha(alpha));
                            const next_garland = cur.children(.case).next;
                            if (next_garland.get().specific.garland.visible) {
                                // TODO(game): draw variables in the cable
                                drawer.canvas.line(camera, &.{
                                    lego.absolute_point.applyToLocalPosition(.new(1.5, 1)),
                                    next_garland.get().absolute_point.pos,
                                }, 0.05 * lego.absolute_point.scale, .blackAlpha(alpha));
                            }
                        },
                        .sexpr => |sexpr| {
                            if (sexpr.emerging_value != nothing) {
                                if (sexpr.is_pattern) {
                                    assert(sexpr.kind == .atom_var);
                                    try Lego.Specific.Sexpr.drawEatingPattern(sexpr.emerging_value, sexpr.atom_name, sexpr.emerging_value_t, camera, drawer, alpha);
                                    // const t = math.smoothstep(sexpr.emerging_value_t, 0, 0.4);
                                    // try drawer.drawEatingPatternV2(camera, point, sexpr.atom_name, t, alpha);
                                    // try _draw(&.{sexpr.emerging_value}, holding_a_sexpr, camera, drawer);
                                } else {
                                    if (drawer.canvas.clipper.push(.{ .camera = camera, .shape = .{
                                        .custom = .{ .point = lego.absolute_point, .shape = Drawer.AtomVisuals.Geometry.template_mask },
                                    } })) {
                                        drawer.canvas.clipper.use(drawer.canvas);
                                        defer {
                                            drawer.canvas.clipper.pop();
                                            drawer.canvas.clipper.use(drawer.canvas);
                                        }
                                        try _draw(&.{sexpr.emerging_value}, holding_a_sexpr, camera, drawer, active_text_input, active_text_selection);
                                    } else |_| {
                                        std.log.err("reached max lens depth, TODO(polish): improve", .{});
                                    }
                                }
                            }

                            switch (sexpr.kind) {
                                // parent is nothing if it's an emerging sexpr
                                .empty => if (lego.tree.parent != nothing and
                                    lego.tree.parent.get().specific.tag() != .sexpr and
                                    (holding_a_sexpr or !sexpr.is_fnkname) and
                                    // Don't draw empty garland fnknames
                                    !(sexpr.is_fnkname and sexpr.is_pattern))
                                {
                                    try drawer.drawPlaceholder(camera, point, sexpr.is_pattern, alpha);
                                },
                                .atom_lit => try drawer.drawAtom(camera, point, sexpr.is_pattern, sexpr.atom_name, inside_postit, alpha),
                                .pair => try drawer.drawPairHolder(camera, point, sexpr.is_pattern, alpha),
                                .atom_var => {
                                    const extra_alpha = 1.0 - sexpr.emerging_value_t;
                                    try drawer.drawVariable(camera, point, sexpr.is_pattern, sexpr.atom_name, alpha * extra_alpha);
                                },
                            }

                            if (sexpr.kind == .pair) {
                                try if (sexpr.is_pattern)
                                    drawer.drawPatternWildcardLinesNonRecursiveV2(
                                        camera,
                                        lego.specific.sexpr.left().bindings_all.items,
                                        lego.specific.sexpr.right().bindings_all.items,
                                        point,
                                        alpha,
                                    )
                                else
                                    drawer.drawTemplateWildcardLinesNonRecursiveV3(
                                        camera,
                                        lego.specific.sexpr.left().bindings_unbound.items,
                                        lego.specific.sexpr.left().bindings_all.items,
                                        lego.specific.sexpr.right().bindings_unbound.items,
                                        lego.specific.sexpr.right().bindings_all.items,
                                        sexpr.emerging_value_t,
                                        point,
                                        alpha,
                                    );
                            }
                        },
                        .lens => |lens| {
                            // TODO(game): lens distortion effect, on source and target

                            if (lens.is_target and camera.plusMargin(lego.absolute_point.scale * (lens.local_radius + 1)).contains(lego.absolute_point.pos)) {
                                const lens_circle: math.Circle = .{ .center = .zero, .radius = lens.local_radius };
                                if (drawer.canvas.clipper.push(.{ .camera = camera_relative, .shape = .{ .circle = lens_circle } })) {
                                    drawer.canvas.clipper.use(drawer.canvas);
                                    defer {
                                        drawer.canvas.clipper.pop();
                                        drawer.canvas.clipper.use(drawer.canvas);
                                    }
                                    drawer.canvas.fillCircleV2(camera_relative, lens_circle, COLORS.bg);

                                    try _draw(lens.roots_to_draw, holding_a_sexpr, lens.transform.getCamera(camera), drawer, active_text_input, active_text_selection);
                                } else |_| {
                                    std.log.err("reached max lens depth, TODO(polish): improve", .{});
                                }
                            }

                            drawer.canvas.strokeCircle(
                                128,
                                camera,
                                lego.absolute_point.pos,
                                lego.absolute_point.scale * lens.local_radius,
                                lego.absolute_point.scale * 0.05,
                                .black,
                            );
                        },
                        .microscope => {
                            const t: f32 = lego.hot_t * 0.2;
                            // TODO(optim-late): check if this is more performant when hidden behind an "if (t > 0)"
                            const lenses = Toybox.getChildrenExact(2, cur);
                            for (lenses) |lens_index| {
                                const lens = lens_index.get().specific.lens;
                                const parent_point = lens_index.get().absolute_point;
                                drawer.canvas.fillCircle(camera, parent_point.pos, lens.local_radius * parent_point.scale, .whiteAlpha(t));
                            }
                        },
                        .area => |area| {
                            switch (area.style) {
                                // TODO(game): .all background
                                .main_area, .none => {},
                                .toolbar => switch (area.bg) {
                                    .all, .none => unreachable,
                                    .local_rect => |rect| {
                                        drawer.canvas.fillRect(camera, lego.absolute_point.applyToLocalRect(rect), .gray(0.4));
                                    },
                                },
                                .bubble => switch (area.bg) {
                                    .all, .none => unreachable,
                                    .local_rect => |rect| {
                                        drawer.canvas.borderRect(
                                            camera,
                                            lego.absolute_point.applyToLocalRect(rect),
                                            0.5 * lego.absolute_point.scale,
                                            .middle,
                                            if (lego.tree.parent.get().specific.bubble.fulfilled)
                                                .fromHex("#386c38")
                                            else
                                                .gray(0.4),
                                        );
                                    },
                                },
                            }
                        },
                        .postit => {
                            assert(!inside_postit);
                            inside_postit = true;
                            const t: f32 = 2.0 + lego.hot_t * 0.7 + lego.active_t * 1.2;
                            drawer.canvas.fillShape(camera_relative, .{ .pos = .zero, .scale = 6.0 / 2.0 }, try drawer.canvas.tmpShape(&.{
                                .new(-1, -1),
                                .new(1, -1),
                                .new(1, 1 - t * 0.1),
                                .new(1 - t * 0.25, 1),
                                .new(-1, 1),
                            }), .fromHex("#FFEBA1"));
                            drawer.canvas.fillShape(camera_relative, .{ .pos = .zero, .scale = 6.0 / 2.0 }, try drawer.canvas.tmpShape(&.{
                                .new(1, 1 - t * 0.1),
                                .new(1 - t * 0.25, 1),
                                Vec2.new(1, 1).mirrorAroundSegment(
                                    .new(1, 1 - t * 0.1),
                                    .new(1 - t * 0.25, 1),
                                ),
                            }), .fromHex("#d4bd68"));
                        },
                        .postit_text => |postit_text| {
                            try drawer.canvas.drawText(0, camera_relative, postit_text.text, switch (postit_text.kind) {
                                .center => .centeredAt(.zero),
                                .left => .leftCenterAt(.zero),
                            }, 0.8, .black);
                        },
                        .postit_drawing => |kind| {
                            switch (kind) {
                                .long_arrow => {
                                    const center: Point = lego.absolute_point;
                                    drawer.canvas.line(camera, &.{
                                        center.applyToLocalPosition(.new(-1.0, 0)),
                                        center.applyToLocalPosition(.new(1.0, 0)),
                                        center.applyToLocalPosition(.new(0.5, 0.25)),
                                        center.applyToLocalPosition(.new(1.0, 0)),
                                        center.applyToLocalPosition(.new(0.5, -0.25)),
                                    }, 0.1 * center.scale, .black);
                                },
                                .arrow => {
                                    const center: Point = lego.absolute_point;
                                    drawer.canvas.line(camera, &.{
                                        center.applyToLocalPosition(.new(-0.5, 0)),
                                        center.applyToLocalPosition(.new(0.5, 0)),
                                        center.applyToLocalPosition(.new(0.0, 0.25)),
                                        center.applyToLocalPosition(.new(0.5, 0)),
                                        center.applyToLocalPosition(.new(0.0, -0.25)),
                                    }, 0.1 * center.scale, .black);
                                },
                                .launch_testcase_button => {
                                    const center: Point = lego.absolute_point;
                                    const rect: Rect = .fromPoint(center, .center, .one);
                                    drawer.canvas.borderRect(camera, rect, 0.05 * center.scale, .inner, .black);
                                    const arrow_center = center.applyToLocalPoint(.{ .pos = .new(0.15, 0) });
                                    drawer.canvas.line(camera, &.{
                                        arrow_center.applyToLocalPosition(.new(-0.25, -0.25)),
                                        arrow_center.applyToLocalPosition(.new(0, 0)),
                                        arrow_center.applyToLocalPosition(.new(-0.25, 0.25)),
                                    }, 0.05 * center.scale, .black);
                                },
                                .piece_center => {
                                    const center: Point = lego.absolute_point;
                                    drawer.canvas.line(camera, &.{
                                        center.applyToLocalPosition(.new(-0.5, 0)),
                                        center.applyToLocalPosition(.new(-0.2, 0)),
                                    }, 0.05 * center.scale, .black);
                                    drawer.canvas.line(camera, &.{
                                        center.applyToLocalPosition(.new(0.2, 0)),
                                        center.applyToLocalPosition(.new(0.5, 0)),
                                    }, 0.05 * center.scale, .black);
                                    drawer.canvas.strokeCircle(128, camera, center.pos, center.scale * 0.2, 0.05 * center.scale, .black);
                                    const arc: [32]Vec2 = comptime funk.map(
                                        Vec2.fromTurns,
                                        &funk.linspace(-0.15, 0.15, 32, true),
                                    );
                                    drawer.canvas.line(camera, &funk.mapOOP(center.applyToLocalPoint(.{ .pos = .new(-1.5, 0) }), .applyToLocalPosition, &arc), 0.05 * center.scale, .black);
                                    drawer.canvas.line(camera, &funk.mapOOP(center.applyToLocalPoint(.{ .pos = .new(1.5, 0), .turns = 0.5 }), .applyToLocalPosition, &arc), 0.05 * center.scale, .black);
                                },
                            }
                        },
                        .button => |button| {
                            switch (button.action) {
                                .stop_execution => {
                                    // TODO(game): nicer
                                    drawer.canvas.fillRect(camera_relative, button.local_rect, .gray(0.4));
                                    drawer.canvas.borderRect(camera_relative, button.local_rect, math.lerp(0.05, 0.1, @max(lego.hot_t, lego.active_t)), .inner, .black);
                                    const center = button.local_rect.get(.center);
                                    const s = button.local_rect.size.min() * 0.3;
                                    drawer.canvas.line(camera_relative, &.{
                                        center.add(.new(-s, -s)),
                                        center.add(.new(s, s)),
                                    }, 0.05, .black);
                                    drawer.canvas.line(camera_relative, &.{
                                        center.add(.new(s, -s)),
                                        center.add(.new(-s, s)),
                                    }, 0.05, .black);
                                },
                                .launch_testcase => {
                                    drawer.canvas.fillRect(camera_relative, button.local_rect, .gray(0.4));
                                    const rect = button.local_rect.move(Vec2.new(-1, -1).scale((1 - lego.hot_t) * 0.05 + (1 - @min(lego.active_t, lego.hot_t)) * 0.1));
                                    drawer.canvas.fillRect(camera_relative, rect, COLORS.bg);
                                    drawer.canvas.borderRect(camera_relative, rect, 0.05, .inner, .black);
                                    drawer.canvas.line(camera_relative, &.{
                                        rect.getCenter().add(.new(-0.25, -0.25)).addX(0.15),
                                        rect.getCenter().add(.new(0, 0)).addX(0.15),
                                        rect.getCenter().add(.new(-0.25, 0.25)).addX(0.15),
                                    }, 0.05, .black);
                                },
                                .delete_testcase => {
                                    // TODO(game): nicer
                                    drawer.canvas.fillRect(camera_relative, button.local_rect, COLORS.bg);
                                    drawer.canvas.borderRect(camera_relative, button.local_rect, math.lerp(0.05, 0.1, @max(lego.hot_t, lego.active_t)), .inner, .black);
                                    const center = button.local_rect.get(.center);
                                    const s = button.local_rect.size.min() * 0.3;
                                    drawer.canvas.line(camera_relative, &.{
                                        center.add(.new(-s, -s)),
                                        center.add(.new(s, s)),
                                    }, 0.05, .black);
                                    drawer.canvas.line(camera_relative, &.{
                                        center.add(.new(s, -s)),
                                        center.add(.new(-s, s)),
                                    }, 0.05, .black);
                                },
                                .see_failing_testcase => {
                                    switch (button.extra_info.see_failing_testcase) {
                                        .solved => {
                                            drawer.canvas.fillRect(camera_relative, button.local_rect, .gray(0.7));
                                            try drawer.canvas.drawText(0, camera_relative, "Solved!", .centeredAt(button.local_rect.getCenter()), 0.8, .black);
                                        },
                                        .unsolved => {
                                            drawer.canvas.rectGradient(
                                                camera_relative,
                                                button.local_rect,
                                                .gray(0.75 + lego.hot_t * 0.2 - lego.active_t * 0.1),
                                                .gray(0.95 - lego.hot_t * 0.2 - lego.active_t * 0.1),
                                            );
                                            try drawer.canvas.drawText(0, camera_relative, "Unsolved!", .centeredAt(button.local_rect.getCenter()), 0.75, .black);
                                        },
                                        .undetermined => {
                                            drawer.canvas.rectGradient(
                                                camera_relative,
                                                button.local_rect,
                                                .gray(0.7),
                                                .gray(0.8),
                                            );
                                            try drawer.canvas.drawText(0, camera_relative, "Checking...", .centeredAt(button.local_rect.getCenter()), 0.75, .black);
                                        },
                                    }
                                },
                                .scroll_up, .scroll_down => {
                                    // TODO(game): draw a better icon
                                    drawer.canvas.fillRect(camera_relative, button.local_rect, COLORS.bg);
                                    drawer.canvas.borderRect(camera_relative, button.local_rect, math.lerp(0.05, 0.1, @max(lego.hot_t, lego.active_t)), .inner, .black);
                                },
                                .reset_bubble => {
                                    // TODO(game): nicer
                                    drawer.canvas.fillRect(camera_relative, button.local_rect, COLORS.bg);
                                    drawer.canvas.borderRect(camera_relative, button.local_rect, math.lerp(0.05, 0.1, @max(lego.hot_t, lego.active_t)), .inner, .black);
                                },
                                .unlock_hint => {
                                    if (button.enabled) {
                                        drawer.canvas.fillRect(camera_relative, button.local_rect, COLORS.bg);
                                        drawer.canvas.borderRect(camera_relative, button.local_rect, math.lerp(0.05, 0.1, @max(lego.hot_t, lego.active_t)), .inner, .black);
                                        try drawer.canvas.drawText(0, camera_relative, "Hint?", .centeredAt(button.local_rect.getCenter()), 0.75, .black);
                                    }
                                },
                                .create_fnkbox_for_row, .add_testcase => if (button.enabled) {
                                    // TODO(game): nicer
                                    drawer.canvas.fillRect(camera_relative, button.local_rect, COLORS.bg);
                                    drawer.canvas.borderRect(camera_relative, button.local_rect, math.lerp(0.05, 0.1, @max(lego.hot_t, lego.active_t)), .inner, .black);
                                    const s = 0.1;
                                    drawer.canvas.line(camera_relative, &.{
                                        .new(-s, 0),
                                        .new(s, 0),
                                    }, 0.05, .black);
                                    drawer.canvas.line(camera_relative, &.{
                                        .new(0, -s),
                                        .new(0, s),
                                    }, 0.05, .black);
                                },
                                .toggle_skip_fnk => if (button.enabled) {
                                    // TODO(game): nicer
                                    // TODO(platform): fails for rotated cameras
                                    drawer.canvas.fillRect(camera_relative, button.local_rect, if (button.latched) .blackAlpha(alpha) else COLORS.bg.withAlpha(alpha));
                                    drawer.canvas.borderRect(camera_relative, button.local_rect, 0.1, .inner, .blackAlpha(alpha));
                                },
                            }
                        },
                        .scrollbar => |scrollbar| {
                            drawer.canvas.fillRect(camera_relative, scrollbar.handleRectVisual(), COLORS.bg);
                            drawer.canvas.borderRect(camera_relative, scrollbar.handleRectVisual(), math.lerp(0.05, 0.1, @max(lego.hot_t, lego.active_t)), .inner, .black);
                        },
                        .scrollable_list => |scrollable_list| {
                            if (scrollable_list.kind == .fnkbox_testcases) {
                                const testcases_labels_center = Lego.Specific.FnkboxBox.testcases_box.get(.top_center).addY(-Lego.Specific.FnkboxBox.testcases_header_height * 0.5).addX(0.85);
                                try drawer.canvas.drawText(0, camera_relative, "Examples:", .centeredAt(testcases_labels_center.addX(-7.15)), 0.65, .black);
                                try drawer.canvas.drawText(0, camera_relative, "Input", .centeredAt(testcases_labels_center.addX(-4)), 0.65, .black);
                                try drawer.canvas.drawText(0, camera_relative, "Target", .centeredAt(testcases_labels_center.addX(0)), 0.65, .black);
                                try drawer.canvas.drawText(0, camera_relative, "Actual", .centeredAt(testcases_labels_center.addX(4)), 0.65, .black);
                            }
                            if (scrollable_list.clip()) {
                                drawer.canvas.clipper.push(.{
                                    .camera = camera_relative,
                                    .shape = .{
                                        .rect = scrollable_list.rect(),
                                    },
                                }) catch @panic("TOO DEEP");
                                drawer.canvas.clipper.use(drawer.canvas);
                            }
                        },
                        .newcase => |newcase| {
                            // TODO(design): camera_relative fails due to rotation
                            drawer.canvas.line(camera, &.{
                                lego.absolute_point.pos,
                                lego.absolute_point.applyToLocalPosition(.new(0, newcase.length())),
                            }, 0.05 * lego.absolute_point.scale, .blackAlpha(alpha));
                        },
                        .editable_textline => |editable_textline| {
                            if (try drawer.canvas.drawEditableText(
                                .{},
                                0,
                                camera_relative,
                                editable_textline.text() orelse editable_textline.config.text_if_empty,
                                if (cur == active_text_input) active_text_selection else null,
                                editable_textline.config.local_position,
                                editable_textline.config.em,
                                .black,
                                FColor.cyan.withAlpha(0.5),
                            )) |cursor_line| {
                                drawer.canvas.line(camera_relative, &.{
                                    cursor_line.a,
                                    cursor_line.b,
                                }, 0.02, .white);
                            }
                        },
                        .fnkslist_element => |fnkslist_element| {
                            try drawer.canvas.drawText(
                                0,
                                camera_relative,
                                fnkslist_element.text(),
                                .leftCenterAt(.new(2.1, Lego.Specific.FnkslistElement.height / 2.0)),
                                0.5,
                                .black,
                            );
                        },
                        .fnkname_holder => |fnkname_holder| {
                            if (fnkname_holder.text()) |text| {
                                const hovered = cur.children(.fnkname_holder).fnkname.get().hot_t;
                                if (hovered > 0) {
                                    try drawer.canvas.drawTextV2(
                                        lego.absolute_point,
                                        0,
                                        camera,
                                        text,
                                        .leftCenterAt(.new(4.75, -0.5)),
                                        0.5,
                                        .blackAlpha(hovered * alpha),
                                    );
                                }
                            }
                        },
                        .scorer => |scorer| {
                            const n_rows = Toybox.childCount(cur.children(.scorer).scorer_rows);
                            const rect: Rect = .{
                                .top_left = .new(-1, -1),
                                .size = .new(16, 2 * tof32(n_rows)),
                            };
                            drawer.canvas.fillRect(camera_relative, rect, COLORS.bg);
                            drawer.canvas.strokeRect(camera_relative, rect, 0.1, .black);
                            try drawer.canvas.drawText(
                                0,
                                camera_relative,
                                if (scorer.score) |score|
                                    try std.fmt.allocPrint(drawer.canvas.frame_arena.allocator(), "Size {d}, Time {d}", .{ score.code_size, score.total_time })
                                else
                                    "unsolved!",
                                .leftCenterAt(.new(1, -2)),
                                0.75,
                                .black,
                            );
                        },
                        .scorer_row => |scorer_row| {
                            try drawer.canvas.drawText(
                                0,
                                camera_relative,
                                levels[scorer_row.level_index].description,
                                .leftCenterAt(.new(1, 0)),
                                0.75,
                                .black,
                            );
                        },
                        .fnkbox_box => {
                            drawer.canvas.fillRect(camera_relative, Lego.Specific.FnkboxBox.relative_box, COLORS.bg.withAlpha(0.65));
                        },
                        .executor_brake => |brake| {
                            drawer.canvas.line(camera_relative, &kommon.funktional.mapOOP(
                                brake,
                                .brakeBody,
                                &kommon.funktional.linspace01(32, true),
                            ), 0.2, .gray(0.4));
                            drawer.canvas.line(camera_relative, &kommon.funktional.mapOOP(
                                brake,
                                .brakeHandlePath,
                                &kommon.funktional.linspace01(32, true),
                            ), Drawer.pixelWidth(camera_relative), FColor.gray(1));
                        },
                        .executor_crank => |crank| {
                            drawer.canvas.fillShape(camera_relative, .{ .turns = crank.value }, Drawer.AtomVisuals.Geometry.ridged_circle, .gray(0.6));
                        },
                        .testcase => |testcase| {
                            // Don't draw testcases that will get clipped outside the testbox
                            assert(lego.tree.parent.get().specific.scrollable_list.kind == .fnkbox_testcases);
                            if (lego.local_point.applyToLocalRect(Lego.Specific.Testcase.relative_bounding_box)
                                .intersect(Lego.Specific.FnkboxBox.testcases_box) == null)
                            {
                                it.skipChildren();
                                _ = it.next();
                                continue;
                            }

                            const symbol_pos = lego.absolute_point.applyToLocalPoint(.{ .pos = .new(7, 0.0), .scale = 0.4 });
                            if (testcase.solved) {
                                // drawer.canvas.line(camera, &.{
                                //     symbol_pos.applyToLocalPosition(.new(-1, 0)),
                                //     symbol_pos.applyToLocalPosition(.new(0, 1)),
                                //     symbol_pos.applyToLocalPosition(.new(1.5, -1.25)),
                                // }, 0.1 * lego.absolute_point.scale, .blackAlpha(alpha));
                            } else {
                                // TODO(visual): better icon, maybe a smiley face
                                drawer.canvas.line(camera, &.{
                                    symbol_pos.applyToLocalPosition(.new(1, -1)),
                                    symbol_pos.applyToLocalPosition(.new(-1, 1)),
                                }, 0.1 * lego.absolute_point.scale, .blackAlpha(alpha));
                                drawer.canvas.line(camera, &.{
                                    symbol_pos.applyToLocalPosition(.new(-1, -1)),
                                    symbol_pos.applyToLocalPosition(.new(1, 1)),
                                }, 0.1 * lego.absolute_point.scale, .blackAlpha(alpha));
                            }
                        },
                        .garland => |garland| {
                            if (!garland.visible) {
                                it.skipChildren();
                                _ = it.next();
                                continue;
                            }
                        },
                        .list_viewer, .meta_viewer => {
                            const scale = 2.15;
                            drawer.canvas.line(
                                camera_relative,
                                &([1]Vec2{Vec2.new(2, -1).scale(scale)} ++
                                    funk.fromCount(32, struct {
                                        pub fn anon(k: usize) Vec2 {
                                            return Vec2.fromPolar(scale, math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(1);
                                        }
                                    }.anon) ++ [1]Vec2{Vec2.new(2, 1).scale(scale)}),
                                0.05,
                                .blackAlpha(alpha),
                            );
                        },
                        .bubble_connection => |bubble_connection| {
                            const s = bubble_connection.source.get().absolute_point.scale;
                            drawer.canvas.arrow(camera, @as(math.Segment, .{
                                .a = bubble_connection.source.get().absolute_point.pos,
                                .b = bubble_connection.target.get().absolute_point.pos,
                            }).clipToBeOutsideRect(bubble_connection.source.get().absolute_point.applyToLocalRect(
                                bubble_connection.source.get().specific.bubble.blueprint.get().specific.area.bg.local_rect.plusMargin(0.5),
                            )).clipToBeOutsideRect(bubble_connection.target.get().absolute_point.applyToLocalRect(
                                bubble_connection.target.get().specific.bubble.blueprint.get().specific.area.bg.local_rect.plusMargin(0.5),
                            )), s * 0.1, s * 1, .black);
                        },
                        .bubble => |bubble| {
                            if (bubble.locked) {
                                it.skipChildren();
                                _ = it.next();

                                const area = bubble.blueprint.get().specific.area;
                                switch (area.bg) {
                                    .all, .none => unreachable,
                                    .local_rect => |rect| {
                                        drawer.canvas.fillRect(camera, lego.absolute_point.applyToLocalRect(rect), .gray(0.4));
                                    },
                                }
                                try drawer.canvas.drawText(
                                    0,
                                    camera_relative,
                                    "Locked",
                                    .centeredAt(.zero),
                                    2,
                                    .black,
                                );

                                continue;
                            }
                        },
                        .executor => |executor| {
                            if (executor.used_for_bg_computation) {
                                it.skipChildren();
                                _ = it.next();
                                continue;
                            }
                        },
                        .scrollable_list_inbetween,
                        .executor_controls,
                        .garland_newcases,
                        .fnkbox,
                        .pill,
                        .scorer_rows,
                        => {},
                    }
                }
            }
        }
    }

    pub fn valid(workspace: *const Workspace, scratch: std.mem.Allocator) !bool {
        const zone = tracy.initZone(@src(), .{ .name = "checking valid" });
        defer zone.deinit();

        if (toybox.free_head != nothing and Toybox.getUnsafe(toybox.free_head).exists) {
            std.debug.panic("Free head is {any}, but that element exists!", .{toybox.free_head});
        }
        if (true and @import("builtin").mode == .Debug) { // check that free_next pointers are unique
            var seen_indices: std.AutoHashMap(Lego.Index, void) = .init(scratch);
            defer seen_indices.deinit();
            var lego_it = toybox.all_legos.constIterator(0);
            var k: usize = 0;
            while (lego_it.next()) |lego| {
                defer k += 1;
                assert(lego.index.index == k);
                assert(!lego.free_next.exists());
                if (lego.exists) {
                    assert(lego.free_next == nothing);
                } else {
                    seen_indices.putNoClobber(lego.free_next, {}) catch std.debug.panic("OoM", .{});
                }
            }
        }
        // Disabled for now, since execution.old_testcase_actual_value and execution.original_garland are rootless
        if (false and @import("builtin").mode == .Debug) { // check that there are no unknown roots
            var valid_ancestors: std.ArrayListUnmanaged(Lego.Index) = .empty;
            try valid_ancestors.appendSlice(scratch, workspace.roots(.all).constSlice());
            if (true) {
                var lego_it = toybox.all_legos.constIterator(0);
                while (lego_it.next()) |lego| {
                    if (!lego.exists) continue;
                    switch (lego.specific) {
                        else => {},
                        .testcase => |testcase| try valid_ancestors.append(scratch, testcase.source),
                        .bubble => |bubble| switch (bubble.goal) {
                            else => {},
                            .has_sexpr => |index| try valid_ancestors.append(scratch, index),
                        },
                    }
                }
            }

            var lego_it = toybox.all_legos.constIterator(0);
            var k: usize = 0;
            while (lego_it.next()) |lego| {
                defer k += 1;
                assert(lego.index.index == k);
                if (!lego.exists) continue;
                const root = Toybox.oldestAncestor(lego.index);
                if (root.hasTag(.area) and root.get().specific.area.style == .bubble) continue;
                for (valid_ancestors.items) |known_root| {
                    if (known_root == root) break;
                } else {
                    std.log.err("valid ancestors: {any}", .{valid_ancestors.items});
                    std.log.err("main area: {any}", .{workspace.main_area});
                    var cur = lego.index;
                    while (cur != nothing) : (cur = cur.get().tree.parent) {
                        std.log.err("lego {d} with tag {s} created at {}, son of...", .{
                            lego.index.asU32(),
                            @tagName(lego.specific.tag()),
                            lego.created_at,
                        });
                    }
                    std.log.err("no one", .{});
                    std.debug.panic("lego {d} with tag {s} created at {any} has unknown root {d}", .{
                        lego.index.asU32(),
                        @tagName(lego.specific.tag()),
                        lego.created_at,
                        root.asU32(),
                    });
                }
            }
        }
        if (true and @import("builtin").mode == .Debug) { // check that there are no repeated magic_id among scorers
            var seen_magics: std.AutoHashMap(u32, void) = .init(scratch);
            defer seen_magics.deinit();
            var lego_it = toybox.all_legos.constIterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.tag() != .scorer_row) continue;
                // don't include stuff in a blueprint
                if (workspace.isFreefloating(lego.index)) continue;
                if (seen_magics.get(lego.specific.scorer_row.magic_id) != null) {
                    panic("unexpected repeated fnk {s} in scorer_rows", .{levels[lego.specific.scorer_row.level_index].fnk_name});
                }
                seen_magics.putNoClobber(lego.specific.scorer_row.magic_id, {}) catch std.debug.panic("OoM", .{});
            }
        }
        return true;
    }

    pub fn update(workspace: *Workspace, platform: PlatformGives, drawer: ?*Drawer, scratch: std.mem.Allocator) !void {
        assert(try workspace.valid(scratch));

        tracy.plot(u32, "undo stack size", @intCast(toybox.undo_stack.commands.len()));
        // tracy.plot(u32, "canvas frame arena capacity in mb", @intCast(@divFloor(drawer.?.canvas.frame_arena.queryCapacity(), 1024 * 1024)));

        var typing: bool = workspace.active_text_input != nothing;
        if (typing and if (workspace.active_text_input.getSafe()) |a| a.specific.tag() != .editable_textline else true) {
            typing = false;
            workspace.active_text_input = .nothing;
            platform.stopTextInput();
        }

        workspace.display_fps = !typing and platform.keyboard.cur.isDown(.KeyF);
        workspace.debug_nodraw = !typing and platform.keyboard.cur.isDown(.KeyV);
        if (!typing and platform.keyboard.wasPressed(.KeyU)) {
            workspace.debug_all_bubbles_unlocked = !workspace.debug_all_bubbles_unlocked;
        }

        if (false and platform.keyboard.wasPressed(.KeyQ)) {
            var lego_it = toybox.all_legos.constIterator(0);
            var k: usize = 0;
            while (lego_it.next()) |lego| {
                defer k += 1;
                assert(lego.index == @as(Lego.Index, @enumFromInt(k)));
                if (!lego.exists) continue;
                const root = Toybox.oldestAncestor(lego.index);
                for (workspace.roots(.all).constSlice()) |known_root| {
                    if (known_root == root) break;
                } else {
                    std.log.debug("lego {d} with tag {s} has unknown root {d}", .{
                        lego.index.asU32(),
                        @tagName(lego.specific.tag()),
                        root.asU32(),
                    });
                }
            }
        }

        if (false and platform.keyboard.wasPressed(.KeyQ)) {
            Workspace.debugLogState();
        }

        if (true and platform.keyboard.wasPressed(.KeyQ)) {
            var total: usize = 0;
            var counts: std.EnumArray(Lego.Specific.Tag, usize) = .initFill(0);
            var lego_it = toybox.all_legos.constIterator(0);
            var k: usize = 0;
            while (lego_it.next()) |lego| {
                defer k += 1;
                assert(lego.index.index == k);
                if (lego.exists) {
                    counts.getPtr(lego.specific.tag()).* += 1;
                    total += 1;
                }
            }
            var it = counts.iterator();
            std.log.debug("counts:", .{});
            while (it.next()) |entry| {
                std.log.debug("{s}:\t{d}", .{ @tagName(entry.key), entry.value.* });
            }
            std.log.debug("total: {d}", .{total});
        }

        if (false and platform.keyboard.wasPressed(.KeyQ)) {
            var seen: usize = 0;
            var k = toybox.all_legos.len - 1;
            while (seen < 100) : (k -= 1) {
                const lego = toybox.all_legos.at(k);
                if (lego.exists) {
                    std.log.debug("{d}-to-last lego created {d} was {s} by {}", .{ seen, lego.index.index, @tagName(lego.specific.tag()), lego.created_at });
                    seen += 1;
                }
            }
        }

        if (false and platform.keyboard.wasPressed(.KeyQ)) {
            const children = try Toybox.getChildrenUnknown(scratch, workspace.floating_inputs_layer);
            std.log.debug("floating children: {d}", .{children.len});
            for (children) |index| {
                const lego = index.get();
                assert(lego.exists);
                std.log.debug("{d} \t{s} \tparent: {d} \tnext: {d} \tprev: {d} \tfirst: {d}\tfree next: {d}", .{
                    index.index,
                    @tagName(lego.specific.tag()),
                    lego.tree.parent.asU32(),
                    lego.tree.next.asU32(),
                    lego.tree.prev.asU32(),
                    lego.tree.first.asU32(),
                    lego.free_next.asU32(),
                });
            }
        }

        if (true and platform.keyboard.wasPressed(.KeyQ)) {
            var alive_count: usize = 0;
            var lego_it = toybox.all_legos.constIterator(0);
            var k: usize = 0;
            while (lego_it.next()) |lego| {
                defer k += 1;
                assert(lego.index.index == k);
                if (lego.exists) {
                    alive_count += 1;
                }
            }
            std.log.debug("total alive legos: {d}", .{alive_count});
            std.log.debug("total legos: {d}", .{toybox.all_legos.count()});
        }

        if (false and platform.keyboard.wasPressed(.KeyQ)) {
            std.log.debug("-----", .{});
            var lego_it = toybox.all_legos.constIterator(0);
            var k: usize = 0;
            while (lego_it.next()) |lego| {
                defer k += 1;
                assert(lego.index == @as(Lego.Index, @enumFromInt(k)));
                if (!lego.exists) continue;
                if (lego.specific.tag() == .fnkbox) {
                    std.log.debug("{any} at pos {any}", .{
                        try lego.index.children(.fnkbox).fnkname.get().specific.sexpr.toOldCoreValue(scratch),
                        lego.local_point,
                    });
                }
            }
        }

        if (false and platform.keyboard.wasPressed(.KeyQ)) {
            std.log.debug("-----", .{});
            var lego_it = toybox.all_legos.constIterator(0);
            var k: usize = 0;
            while (lego_it.next()) |lego| {
                defer k += 1;
                assert(lego.index == @as(Lego.Index, @enumFromInt(k)));
                if (lego.exists) {
                    std.log.debug("{d} \t{s} \tparent: {d} \tnext: {d} \tprev: {d} \tfirst: {d} \t rel: {any} \tabs: {any}", .{
                        k,
                        @tagName(lego.specific.tag()),
                        lego.tree.parent.asU32(),
                        lego.tree.next.asU32(),
                        lego.tree.prev.asU32(),
                        lego.tree.first.asU32(),
                        lego.local_point,
                        lego.absolute_point,
                    });
                }
            }
            std.log.debug("-----", .{});
            for (toybox.undo_stack.commands.items, 0..) |cmd, k1| {
                std.log.debug("{d} \t{any}", .{ k1, cmd });
            }
        }

        const delta_seconds = @min(1.0 / 30.0, platform.delta_seconds * @as(f32, (if (platform.keyboard.cur.isDown(.Space)) 0.01 else 1.0)));

        const absolute_camera = Rect
            .fromCenterAndSize(.zero, .both(2))
            .withAspectRatio(platform.aspect_ratio, .grow, .center);

        const mouse = platform.getMouse(absolute_camera);

        if (platform.recording_log) |log| {
            // const S = struct {
            //     var prev_input: FuzzerContext.FakeInput = .{
            //         .mouse_left_down = false,
            //         .mouse_right_down = false,
            //         .z_down = false,
            //         .mouse_pos = .zero,
            //     };
            // };
            const cur_input: FuzzerContext.FakeInput = .{
                .mouse_left_down = mouse.cur.isDown(.left),
                .mouse_right_down = mouse.cur.isDown(.right),
                .mouse_pos = platform.getMouse(.unit).cur.position,
                .z_down = platform.keyboard.cur.isDown(.KeyZ),
                .delta_seconds = platform.delta_seconds,
            };
            // if (true or !std.meta.eql(cur_input, S.prev_input)) {
            try log.print("{any},\n", .{cur_input});
            // try log.writeStruct(cur_input);
            // S.prev_input = cur_input;
            // }
        }

        const undo_stack = &toybox.undo_stack;
        undo_stack.startFrame();

        if (platform.keyboard.wasPressed(.KeyF)) {
            std.log.debug("camera point: {any}", .{workspace.main_area.get().absolute_point});
            std.log.debug("camera center: {any}", .{workspace.cameraCenter()});
        }

        assert(try workspace.valid(scratch));

        if (typing) {
            const editable_textline = &workspace.active_text_input.get().specific.editable_textline;
            var textedit: TextManipulation = .{
                .selection = &workspace.active_text_selection,
                .text = &editable_textline.inner_text,
                .alloc_text = Toybox.getArenaFor(workspace.active_text_input),
                .cursor_points = &editable_textline.cursor_points,
                .alloc_cursor_points = workspace.arena_for_oneframe_data.allocator(),
            };

            const key_time_first = 0.40;
            const key_time_rest = 0.05;

            if (platform.wasKeyPressedOrRetriggered(.ArrowLeft, key_time_rest, key_time_first)) {
                textedit.left(platform.keyboard.cur.isShiftDown(), if (platform.keyboard.cur.isControlDown()) .word else .one);
            }
            if (platform.wasKeyPressedOrRetriggered(.ArrowRight, key_time_rest, key_time_first)) {
                textedit.right(platform.keyboard.cur.isShiftDown(), if (platform.keyboard.cur.isControlDown()) .word else .one);
            }
            if (platform.wasKeyPressedOrRetriggered(.Backspace, key_time_rest, key_time_first)) {
                textedit.backspace(if (platform.keyboard.cur.isControlDown()) .word else .one);
            }
            if (platform.wasKeyPressedOrRetriggered(.Delete, key_time_rest, key_time_first)) {
                textedit.supr(if (platform.keyboard.cur.isControlDown()) .word else .one);
            }

            while (platform.consumeTextInput()) |input| {
                try textedit.insertCharacter(input.constSlice());
            }
        } else {
            assert(platform.consumeTextInput() == null);
        }

        if (typing and (workspace.grabbing.index != nothing and
            (!(mouse.cur.isDown(.left) or mouse.cur.isDown(.right)) or
                workspace.grabbingSomethingIllegal())))
        {
            workspace.setGrabbing(.{ .index = .nothing, .offset = .zero });
            workspace.setHandLayer(.nothing);
        }

        if (typing and (platform.keyboard.wasPressed(.Escape) or
            mouse.wasPressed(.left) or
            mouse.wasPressed(.right)))
        {
            platform.stopTextInput();
            workspace.active_text_input = .nothing;
            typing = false;
        }

        if (!typing and platform.keyboard.wasPressed(.KeyZ)) {
            // std.log.debug("on undo, undo_stack len was: {d}", .{undo_stack.commands.items.len});
            while (undo_stack.pop()) |command| {
                switch (command) {
                    .fence => break,
                    .destroy_floating => |index| {
                        Toybox.destroyFloatingInner(index, false);
                    },
                    .recreate_floating => |data| {
                        Toybox.recreateFloating(data);
                    },
                    .insert => |insert| {
                        Toybox.insertInner(insert.what, insert.where, false);
                    },
                    .set_data_except_tree => |data| {
                        const original_tree = Toybox.get(data.index).tree;
                        Toybox.get(data.index).* = data;
                        Toybox.get(data.index).tree = original_tree;
                    },
                    .pop => |index| {
                        Toybox.popInner(index, false);
                    },
                    .set_grabbing => |grabbing| {
                        workspace.grabbing = grabbing;
                    },
                    .set_handlayer => |index| {
                        workspace.hand_layer = index;
                    },
                    .change_child => |change| {
                        Toybox.changeChildInner(change.original, change.new, false);
                    },
                }
            }
        } else if (!typing) { // INTERACTION
            const zone = tracy.initZone(@src(), .{ .name = "interaction" });
            defer zone.deinit();

            const hot_and_dropzone = workspace.findHotAndDropzone(mouse.cur.position);

            if (workspace.grabbing.index == nothing and
                hot_and_dropzone.hot != nothing and
                (mouse.wasPressed(.left) or mouse.wasPressed(.right)))
            {
                // Main case A: plucking/grabbing/clicking something
                undo_stack.append(.fence);

                const hot_index = hot_and_dropzone.hot;
                const original_hot_data = Toybox.get(hot_index).*;
                const hot_parent = original_hot_data.tree.parent;

                var grabbed_element_index: Lego.Index = undefined;
                var plucked: bool = true;

                if (platform.keyboard.cur.isControlDown() and hot_index.hasTag(.sexpr)) {
                    // Special case: control-click to "go to definition"
                    undo_stack.storeAllData(workspace.main_area);
                    grabbed_element_index = .nothing;
                    plucked = false;

                    if (try workspace.fnkboxWithName(hot_index, scratch)) |fnkbox_index| {
                        const p = workspace.main_area.get().absolute_point.inverseApplyGetLocal(fnkbox_index.get().absolute_point);
                        workspace.centerCameraAt(p.applyToLocalPoint(.{
                            .scale = 8,
                            .pos = .new(0, 6),
                        }), false);
                    } else {
                        // fnk not found, TODO(game): handle better
                    }
                } else if (mouse.wasPressed(.right) or original_hot_data.immutable) {
                    // Case A.0: duplicating
                    switch (hot_index.get().canDuplicate()) {
                        .yes => {
                            const new_element_index = try Toybox.dupeIntoFloatingWithoutChangingPos(hot_index, .new(@src()));
                            // std.log.debug("duplicated {d} into {d}", .{ hot_index.asI32(), new_element_index.asI32() });
                            grabbed_element_index = new_element_index;
                        },
                        .no => {
                            grabbed_element_index = .nothing;
                            plucked = undefined;
                        },
                        .fnkbox => {
                            // TODO(game): maybe improve
                            const fnkbox = try Toybox.buildFnkbox(
                                hot_index.get().absolute_point,
                                try workspace.findFnkname(.{}, true, null),
                                true,
                                "Custom machine",
                                &.{},
                                null,
                            );

                            grabbed_element_index = fnkbox;
                        },
                    }
                } else if (hot_index.hasTag(.microscope)) {
                    // Special case: reparent toolbar microscope
                    assert(hot_index.get().specific.microscope.in_toolbar);
                    undo_stack.storeAllData(hot_index);
                    hot_index.get().specific.microscope.in_toolbar = false;

                    Toybox.pop(hot_index);
                    Toybox.addChildLast(workspace.lenses_layer, hot_index);
                    Toybox.changeCoordinates(hot_index, workspace.toolbar_left.get().absolute_point, workspace.lenses_layer.get().absolute_point);

                    // continue with Case A.3
                    grabbed_element_index = hot_index;
                    plucked = false;
                } else if (hot_index.hasTag(.editable_textline)) {
                    // Special case: edit text
                    platform.startTextInput(null);
                    workspace.active_text_input = hot_index;
                    workspace.active_text_selection = .both(hot_and_dropzone.text_index.?);
                    plucked = false;
                    grabbed_element_index = hot_index;
                } else if (hot_index.get().grabsWithoutPlucking()) {
                    // Case A.3: grabbing rather than plucking, including buttons
                    undo_stack.storeAllData(hot_index);
                    grabbed_element_index = hot_index;
                    plucked = false;

                    if (Toybox.get(hot_index).specific.as(.button)) |b| {
                        if (b.instant()) {
                            grabbed_element_index = .nothing;
                            switch (b.action) {
                                inline else => unreachable,
                                .toggle_skip_fnk => {
                                    b.latched = !b.latched;
                                },
                            }
                        }
                    }
                } else if (hot_parent != nothing and hot_parent.hasTag(.area)) {
                    // Case A.1: plucking a top-level thing
                    undo_stack.storeAllData(hot_index);
                    Toybox.popWithUndoAndChangingCoords(hot_index);
                    grabbed_element_index = hot_index;
                } else if (hot_parent != nothing and hot_parent.hasTag(.scrollable_list) and hot_parent.get().specific.scrollable_list.canPluckElements()) {
                    // Case A.6: plucking from a list, similar to A.4
                    undo_stack.storeAllData(hot_index);
                    try Lego.Specific.ScrollableList.popElement(hot_index);
                    grabbed_element_index = hot_index;
                } else if (original_hot_data.specific.tag() == .sexpr) {
                    // Case A.2: plucking a nested sexpr
                    undo_stack.storeAllData(hot_index);

                    const new_empty_sexpr = try Toybox.buildSexpr(
                        original_hot_data.local_point,
                        .empty,
                        original_hot_data.specific.sexpr.is_pattern,
                        original_hot_data.specific.sexpr.is_fnkname,

                        .new(@src()),
                    );

                    Toybox.changeChild(hot_index, new_empty_sexpr);
                    Toybox.changeCoordinates(hot_index, hot_parent.get().absolute_point, .{});
                    Toybox.refreshAbsolutePoints(&.{new_empty_sexpr});

                    grabbed_element_index = hot_index;
                } else if (hot_parent != nothing and Toybox.get(hot_parent).specific.tag() == .newcase) {
                    // Case A.4: plucking a case from a garland
                    assert(original_hot_data.specific.tag() == .case);
                    undo_stack.storeAllData(hot_index);
                    Lego.Specific.Garland.popCase(hot_index);
                    grabbed_element_index = hot_index;
                } else if (Toybox.get(hot_index).specific.tag() == .garland) {
                    // Case A.5: plucking a garland, and replacing it with an empty one
                    undo_stack.storeAllData(hot_index);

                    const new_garland = try Toybox.buildGarland(original_hot_data.local_point, &.{}, .new(@src()));
                    new_garland.get().specific.garland.computed_height = hot_index.get().specific.garland.computed_height;
                    Toybox.changeChild(hot_index, new_garland);
                    Toybox.changeCoordinates(hot_index, hot_parent.get().absolute_point, .{});
                    Toybox.refreshAbsolutePoints(&.{new_garland});

                    grabbed_element_index = hot_index;
                } else unreachable;

                assert(workspace.grabbing.index == nothing and workspace.hand_layer == nothing);
                if (grabbed_element_index != nothing) {
                    grabbed_element_index.get().immutable = false;
                    workspace.setGrabbing(
                        .{ .index = grabbed_element_index, .offset = grabbed_element_index.get().getGrabbedOffset(mouse.cur.position) },
                    );
                    if (plucked) {
                        workspace.setHandLayer(grabbed_element_index);
                        Toybox.refreshAbsolutePoints(&.{grabbed_element_index});
                    }
                }
            } else if (workspace.grabbing.index != nothing and
                (!(mouse.cur.isDown(.left) or mouse.cur.isDown(.right)) or
                    workspace.grabbingSomethingIllegal()))
            {
                const dropzone_index = hot_and_dropzone.dropzone;

                if (dropzone_index != nothing) {
                    assert(Toybox.isFloating(workspace.grabbing.index));
                    if (dropzone_index.hasTag(.scrollable_list_inbetween)) {
                        try Lego.Specific.ScrollableList.insertElement(dropzone_index, workspace.grabbing.index);
                    } else if (Toybox.get(dropzone_index).specific.tag() == .newcase) {
                        const displaced_newcase = &Toybox.get(dropzone_index).specific.newcase;
                        assert(Toybox.get(workspace.grabbing.index).specific.tag() == .case);
                        const newcase = try Toybox.new(.{}, .{ .newcase = .{
                            .length_before = Toybox.get(dropzone_index).specific.newcase.length_before,
                            .length_after = 0,
                        } }, .new(@src()));
                        displaced_newcase.length_before = 0;
                        const original_tree = Toybox.get(dropzone_index).tree;
                        Toybox.insert(newcase, .{
                            .parent = original_tree.parent,
                            .prev = original_tree.prev,
                            .next = dropzone_index,
                            .first = .nothing,
                            .last = .nothing,
                        });
                        Toybox.changeCoordinates(workspace.grabbing.index, .{}, Toybox.parentAbsolutePoint(dropzone_index));
                        Toybox.addChildLast(newcase, workspace.grabbing.index);
                    } else {
                        Toybox.changeCoordinates(workspace.grabbing.index, .{}, Toybox.parentAbsolutePoint(dropzone_index));
                        Toybox.refreshAbsolutePoints(&.{workspace.grabbing.index});
                        Toybox.changeChild(dropzone_index, workspace.grabbing.index);

                        Toybox.destroyFloating(dropzone_index);
                    }
                } else if (!Toybox.isFloating(workspace.grabbing.index)) {
                    // Case B.2: releasing a grabbed thing, which might be a button
                    assert(dropzone_index == nothing);
                    if (Toybox.get(workspace.grabbing.index).specific.as(.button)) |button| {
                        if (hot_and_dropzone.hot == workspace.grabbing.index) {
                            switch (button.action) {
                                .toggle_skip_fnk => unreachable,
                                .stop_execution => try forcefullyStopExecutor(Toybox.findAncestor(hot_and_dropzone.hot, .executor)),
                                .create_fnkbox_for_row => {
                                    const row = workspace.grabbing.index.get().tree.parent;
                                    assert(row.hasTag(.scorer_row));
                                    const level_index = row.get().specific.scorer_row.level_index;
                                    const level = levels[level_index];
                                    const old_fnkname = row.children(.scorer_row).fnkname;
                                    assert(old_fnkname.get().specific.sexpr.kind == .empty);
                                    const new_fnkname = try workspace.findFnkname(old_fnkname.get().local_point, false, level.fnk_name);
                                    Toybox.changeChild(old_fnkname, new_fnkname);
                                    Toybox.destroyFloating(old_fnkname);

                                    var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);
                                    defer pool.deinit();
                                    const samples: []const Lego.Index = blk: {
                                        var samples: std.ArrayListUnmanaged(Lego.Index) = .empty;
                                        try samples.ensureUnusedCapacity(scratch, 100);
                                        var sample_index: usize = 0;
                                        while (try level.generate_sample(sample_index, &pool, scratch, workspace.gpa_for_atom_names)) |sample| {
                                            try samples.append(scratch, try Toybox.buildTestcase(.{ .unloaded = .build(
                                                level_index,
                                                sample_index,
                                                sample,
                                            ) }, .new(@src())));
                                            sample_index += 1;
                                            _ = pool.reset(.retain_capacity);
                                        }
                                        break :blk try samples.toOwnedSlice(scratch);
                                    };
                                    std.log.debug("samples len: {d}", .{samples.len});

                                    const p = workspace.main_area.get().absolute_point.inverseApplyGetLocal(row.get().absolute_point);

                                    // TODO(game): better fnkbox positioning when not hardcoded
                                    const fnkbox = try Toybox.buildFnkbox(
                                        if (row.get().specific.scorer_row.offset) |offset|
                                            .{ .pos = offset }
                                        else
                                            .{ .pos = p.applyToLocalPosition(.new(0, 5)) },
                                        // .{ .pos = row.get().absolute_point.applyToLocalPosition(offset orelse .new(0, 5)) },
                                        try Toybox.dupeIntoFloating(new_fnkname, .new(@src())),
                                        true,
                                        level.description,
                                        samples,
                                        if (try level.initialDefinition(&pool, scratch)) |definition|
                                            try Lego.Specific.Garland.buildFromOldCoreValue(.{}, definition, scratch, .new(@src()))
                                        else
                                            null,
                                    );
                                    fnkbox.get().specific.fnkbox.require_manual_execution = level.require_manual_execution;
                                    Toybox.addChildLast(if (row.get().specific.scorer_row.offset == null)
                                        workspace.main_area
                                    else
                                        Toybox.findAncestor(row, .area), fnkbox);
                                },
                                .reset_bubble => {
                                    workspace.grabbing.index.get().tree.parent.get().specific.bubble.remaining_reset_anim_t = 1;
                                    const original_instanced = workspace.grabbing.index.get().tree.parent.children(.bubble).instanced;
                                    const new_instanced = try Toybox.dupeIntoFloating(workspace.grabbing.index.get().tree.parent.get().specific.bubble.blueprint, .new(@src()));
                                    Toybox.changeChild(original_instanced, new_instanced);
                                    Toybox.destroyFloating(original_instanced);
                                },
                                .unlock_hint => {
                                    const bubble = &workspace.grabbing.index.get().tree.parent.get().specific.bubble;
                                    bubble.requested_hints = true;
                                },
                                .see_failing_testcase => {
                                    const fnkbox = Toybox.findAncestor(workspace.grabbing.index, .fnkbox);
                                    const testcase_index = fnkbox.get().specific.fnkbox.status.bad();
                                    try ensureLoadedTestcase(testcase_index, scratch, workspace.gpa_for_atom_names);
                                    try launchTestcase(testcase_index);
                                },
                                .launch_testcase => {
                                    const testcase_index = Toybox.get(workspace.grabbing.index).tree.parent;
                                    try launchTestcase(testcase_index);
                                },
                                .delete_testcase => {
                                    const testcase_index = Toybox.get(workspace.grabbing.index).tree.parent;
                                    assert(testcase_index.hasTag(.testcase));
                                    const fnkbox_testcases = testcase_index.get().tree.parent;
                                    assert(fnkbox_testcases.hasTag(.scrollable_list));
                                    Toybox.pop(testcase_index);
                                    Toybox.destroyFloating(testcase_index);
                                },
                                .add_testcase => {
                                    const index = workspace.grabbing.index;
                                    const fnkbox_testcases = index.get().tree.parent;
                                    assert(fnkbox_testcases.hasTag(.scrollable_list));
                                    Toybox.pop(index);
                                    Toybox.addChildLast(fnkbox_testcases, try Toybox.buildTestcase(.{ .existing = .{
                                        .input = try Toybox.buildSexpr(.{}, .empty, false, false, .new(@src())),
                                        .expected = try Toybox.buildSexpr(.{}, .empty, false, false, .new(@src())),
                                        .unloaded = null,
                                    } }, .new(@src())));
                                    Toybox.addChildLast(fnkbox_testcases, index);
                                },
                                .scroll_up, .scroll_down => {},
                            }
                        }
                        assert(try workspace.valid(scratch));
                    }
                } else {
                    // Case B.3: dropping a floating thing on fresh space
                    const target_area = hot_and_dropzone.over_background;
                    Toybox.changeCoordinates(workspace.grabbing.index, .{}, Toybox.get(target_area).absolute_point);
                    Toybox.addChildLast(target_area, workspace.grabbing.index);
                    Toybox.refreshAbsolutePoints(&.{workspace.grabbing.index});
                }

                workspace.setGrabbing(.{ .index = .nothing, .offset = .zero });
                workspace.setHandLayer(.nothing);
            }
        }

        // const hovering: Lego.Index = if (workspace.focus.grabbing == nothing) hovered_or_dropzone_thing.which else .nothing;
        // const dropzone: Lego.Index = if (workspace.focus.grabbing != nothing) hovered_or_dropzone_thing.which else .nothing;

        // std.log.debug("--- After interaction ---", .{});
        // workspace.debugLogState();

        assert(try workspace.valid(scratch));

        // TODO(optim): avoid computing this twice?
        const hot_and_dropzone = workspace.findHotAndDropzone(mouse.cur.position);

        // cursor
        platform.setCursor(
            if (workspace.grabbing.index != nothing)
                if (workspace.grabbing.index.hasTag(.button))
                    .pointer
                else if (workspace.grabbing.index.hasTag(.editable_textline))
                    // TODO(polish): should be .text, but that looks buggy with the 50% gray bg color
                    .pointer
                else
                    .grabbing
            else if (hot_and_dropzone.hot != nothing)
                if (hot_and_dropzone.hot.hasTag(.button))
                    .pointer
                else if (hot_and_dropzone.hot.hasTag(.editable_textline))
                    // TODO(polish): should be .text, but that looks buggy with the 50% gray bg color
                    .pointer
                else
                    .could_grab
            else
                .default,
        );

        assert(workspace.grabbing.index == nothing or
            workspace.active_text_input == nothing or
            workspace.active_text_input == workspace.grabbing.index);

        // Do this here, for .grabbingSomethingIllegal to work correctly
        dragGrabbing(
            workspace.grabbing,
            &workspace.active_text_selection,
            mouse.cur.position,
            hot_and_dropzone,
            delta_seconds,
        );

        if (true) { // move camera and scroll stuff
            const zone = tracy.initZone(@src(), .{ .name = "move camera" });
            defer zone.deinit();

            var lego_it = toybox.all_legos.constIterator(0);
            const over_scrollable_element: Lego.Index = while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.tag() == .scrollable_list and lego.specific.scrollable_list.rect().contains(
                    lego.absolute_point.inverseApplyGetLocalPosition(mouse.cur.position),
                )) {
                    break lego.index;
                }
            } else .nothing;

            const p = &Toybox.get(workspace.main_area).local_point;
            if (over_scrollable_element == nothing) {
                p.* = p.scaleAroundLocalPosition(p.inverseApplyGetLocalPosition(mouse.cur.position), switch (mouse.cur.scrolled) {
                    .none => 1.0,
                    .up => 1.1,
                    .down => 0.9,
                });
            } else {
                Toybox.get(over_scrollable_element).addScroll(mouse.cur.scrolled.toNumber() * delta_seconds * -20);
            }
            inline for (KeyboardButton.directional_keys) |kv| {
                for (kv.keys) |key| {
                    if (platform.keyboard.cur.isDown(key) and !typing) {
                        p.pos.addInPlace(kv.dir.tof32().scale(delta_seconds * -2));
                    }
                }
            }

            if (mouse.cur.isDown(.middle) and mouse.prev.isDown(.middle)) {
                p.pos.addInPlace(mouse.deltaPos());
            }

            workspace.refreshCamera();
        }

        try updateNonInteractive(workspace, absolute_camera, delta_seconds, hot_and_dropzone, drawer, scratch);

        if (drawer) |d| {
            try workspace.draw(platform, d);
        }

        assert(try workspace.valid(scratch));
    }

    fn updateNonInteractive(workspace: *Workspace, absolute_camera: Rect, delta_seconds: f32, hot_and_dropzone: HotAndDropzone, drawer: ?*Drawer, scratch: std.mem.Allocator) !void {
        defer workspace.did_first_frame = true;
        // TODO(design): improve/remove, by having this be the permanent list, and not iterating over all elements
        var things_actually_hot_etc: std.ArrayList(Lego.Index) = .init(scratch);

        const undo_stack = &toybox.undo_stack;

        const other_hot: Lego.Index = if (hot_and_dropzone.hot.hasTag(.sexpr))
            hot_and_dropzone.hot.get().specific.sexpr.hot_sibling
        else
            .nothing;
        const hot_variable: ?[]const u8 = hot_and_dropzone.hot.getTheSexprVar();

        if (true) { // update _t and other simple things that could be done in parallel
            const zone = tracy.initZone(@src(), .{ .name = "update _t" });
            defer zone.deinit();

            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;

                var done = true;

                const eps: f32 = 0.0001;
                done = math.lerpTowardsWithFinish(&lego.hot_t, if (lego.index == hot_and_dropzone.hot or lego.index == other_hot or lego.index.isTheSexprVar(hot_variable)) 1 else 0, .fast, delta_seconds, eps) and done;
                done = math.lerpTowardsWithFinish(&lego.active_t, if (lego.index == workspace.grabbing.index) 1 else 0, .fast, delta_seconds, eps) and done;
                done = math.lerpTowardsWithFinish(&lego.dropzone_t, if (lego.index == hot_and_dropzone.dropzone) 1 else 0, .fast, delta_seconds, eps) and done;
                done = math.lerpTowardsWithFinish(&lego.dropping_t, if (lego.index == workspace.grabbing.index and hot_and_dropzone.dropzone != nothing) 1 else 0, .fast, delta_seconds, eps) and done;
                if (true) { // reduce visual_offset
                    const target = lego.visualOffsetGoal();
                    done = done and Point.equalsAbs(lego.visual_offset, target, 0.001);
                    lego.visual_offset.lerpTowards(target, if (lego.tree.parent.hasTag(.scrollable_list))
                        .slow
                    else
                        .fast, delta_seconds);
                }

                switch (lego.specific) {
                    .sexpr => |*sexpr| {
                        done = math.lerpTowardsWithFinish(&sexpr.is_pattern_t, if (sexpr.is_pattern) 1 else 0, .fast, delta_seconds, eps) and done;
                        done = math.lerpTowardsWithFinish(&sexpr.is_fnkname_t, if (sexpr.is_fnkname) 1 else 0, .fast, delta_seconds, eps) and done;
                        done = math.lerpTowardsWithFinish(&sexpr.jiggling_t, 0, .fast, delta_seconds, eps) and done;
                    },
                    .executor => |*executor| {
                        done = math.towardsWithFinish(&executor.garland_appearing_t, 1, delta_seconds / 0.4) and done;
                    },
                    .bubble => |*bubble| {
                        done = math.towardsWithFinish(&bubble.remaining_reset_anim_t, 0, delta_seconds / 0.2) and done;
                    },
                    .scorer,
                    .scorer_row,
                    .scorer_rows,
                    .list_viewer,
                    .meta_viewer,
                    .pill,
                    .area,
                    .case,
                    .newcase,
                    .garland,
                    .garland_newcases,
                    .microscope,
                    .lens,
                    .fnkbox,
                    .editable_textline,
                    .fnkbox_box,
                    .scrollable_list,
                    .scrollable_list_inbetween,
                    .fnkslist_element,
                    .button,
                    .scrollbar,
                    .testcase,
                    .postit,
                    .postit_text,
                    .postit_drawing,
                    .executor_controls,
                    .executor_brake,
                    .executor_crank,
                    .fnkname_holder,
                    .bubble_connection,
                    => {},
                }

                if (!done) {
                    try things_actually_hot_etc.append(lego.index);
                }
            }
        }

        // TODO(design): actually use
        // for (things_actually_hot_etc.items) |index| {
        //     switch (index.get().specific) {
        //         else => {},
        //     }
        // }

        if (true) { // pills decay
            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                switch (lego.specific) {
                    .pill => |*pill| {
                        pill.remaining_lifetime -= delta_seconds;
                        lego.local_point = lego.local_point.applyToLocalPoint(.{ .pos = pill.velocity.scale(delta_seconds) });
                        if (pill.remaining_lifetime <= 0) {
                            Toybox.pop(lego.index);
                            Toybox.destroyFloating(lego.index);
                        }
                    },
                    else => {},
                }
            }
        }

        // TODO(design): a bit hacky
        if (true) { // set garlands visibility
            const zone = tracy.initZone(@src(), .{ .name = "set garlands visibility" });
            defer zone.deinit();

            const grabbing_garland_or_case: bool = if (workspace.grabbing.index == nothing)
                false
            else switch (Toybox.get(workspace.grabbing.index).specific) {
                .case, .garland => true,
                else => false,
            };
            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.garland)) |garland| {
                    garland.visible =
                        (grabbing_garland_or_case and !Toybox.isAncestor(workspace.grabbing.index, lego.index)) or
                        garland.hasChildCases() or
                        (if (lego.tree.parent.getSafe()) |p| switch (p.specific) {
                            else => panic("unexpected tag: {s}", .{@tagName(p.specific.tag())}),
                            .area => true,
                            .case => false,
                            .executor, .meta_viewer => true,
                        } else true);
                }
            }
        }

        if (true) { // open/close left toolbar, and regenerate its contents
            const zone = tracy.initZone(@src(), .{ .name = "toolbar" });
            defer zone.deinit();

            const old_t = workspace.toolbar_left_unfolded_t;
            math.lerpTowards(
                &workspace.toolbar_left_unfolded_t,
                if (hot_and_dropzone.over_background == workspace.toolbar_left) 1 else 0,
                .slow,
                delta_seconds,
            );
            const new_t = workspace.toolbar_left_unfolded_t;
            if (new_t <= 0.01) { // delete all current children
                var cur = Toybox.get(workspace.toolbar_left).tree.first;
                while (cur != nothing) {
                    const original_tree = Toybox.get(cur).tree;
                    Toybox.pop(cur);
                    Toybox.destroyFloating(cur);
                    cur = original_tree.next;
                }
            } else if (old_t <= 0.01) { // regenerate children
                const isUnlocked = struct {
                    pub fn anon(i: Lego.Index) bool {
                        return if (i.getSafe()) |l| !l.specific.bubble.locked else false;
                    }
                }.anon;

                if (isUnlocked(workspace.toolbar_unlocks.case_with_wildcards)) { // add a fresh case
                    const new_name_1 = try workspace.arena_for_atom_names.allocator().alloc(u8, 32);
                    const new_name_2 = try workspace.arena_for_atom_names.allocator().alloc(u8, 32);
                    math.Random.init(workspace.random_instance.random()).alphanumeric_bytes(new_name_1);
                    math.Random.init(workspace.random_instance.random()).alphanumeric_bytes(new_name_2);

                    const index = try Toybox.buildCase(.{ .pos = .new(2.75, 3) }, .{
                        .pattern = try Toybox.buildSexpr(.{}, .{ .pair = .{
                            .up = try Toybox.buildSexpr(.{}, .{ .atom_var = new_name_2 }, true, false, .new(@src())),
                            .down = try Toybox.buildSexpr(.{}, .{ .atom_var = new_name_1 }, true, false, .new(@src())),
                        } }, true, false, .new(@src())),
                        .template = try Toybox.buildSexpr(.{}, .{ .pair = .{
                            .up = try Toybox.buildSexpr(.{}, .{ .atom_var = new_name_1 }, false, false, .new(@src())),
                            .down = try Toybox.buildSexpr(.{}, .{ .atom_lit = "nil" }, false, false, .new(@src())),
                        } }, false, false, .new(@src())),
                        .fnkname = null,
                        .next = null,
                    }, .new(@src()));

                    Toybox.addChildLast(workspace.toolbar_left, index);
                }

                if (isUnlocked(workspace.toolbar_unlocks.lenses)) { // add a fresh lens
                    Toybox.addChildLast(workspace.toolbar_left, try Toybox.buildMicroscope(
                        .new(1, 13),
                        .new(2.5, 12.5),
                        true,
                    ));
                }

                if (isUnlocked(workspace.toolbar_unlocks.list_viewer)) { // add a listviewer
                    Toybox.addChildLast(workspace.toolbar_left, try Toybox.buildListViewer(
                        .{ .pos = .new(2, 6), .scale = 0.5 },
                        null,
                    ));
                }

                if (isUnlocked(workspace.toolbar_unlocks.meta_viewer)) { // add a metaviewer
                    Toybox.addChildLast(workspace.toolbar_left, try Toybox.buildMetaViewer(
                        .{ .pos = .new(1.5, 9), .scale = 0.5 },
                    ));
                }
            }

            const rect = toolbar_left_rect;
            const hot_t = workspace.toolbar_left_unfolded_t;
            const p = &Toybox.get(workspace.toolbar_left).local_point;
            p.* = .{
                .scale = absolute_camera.size.y / rect.size.y,
                .pos = absolute_camera.top_left,
            };
            p.* = p.applyToLocalPoint(.{ .pos = .new(-(rect.size.x - 1) * (1 - hot_t), 0) });

            Toybox.refreshAbsolutePoints(&.{workspace.toolbar_left});
        }

        // TODO(bug): crashes if a fnkbox is deleted while the fnks toolbar is open
        if (true) { // open/close fnks toolbar, and regenerate its contents
            const old_t = workspace.toolbar_fnks_unfolded_t;
            math.lerpTowards(
                &workspace.toolbar_fnks_unfolded_t,
                if (hot_and_dropzone.over_background == workspace.toolbar_fnks) 1 else 0,
                .slow,
                delta_seconds,
            );
            const new_t = workspace.toolbar_fnks_unfolded_t;

            if (new_t <= 0.01) { // delete all current children
                var cur = workspace.toolbar_fnks.get().tree.first;
                while (cur != nothing) {
                    const original_tree = Toybox.get(cur).tree;
                    Toybox.pop(cur);
                    Toybox.destroyFloating(cur);
                    cur = original_tree.next;
                }
            } else if (old_t <= 0.01) { // regenerate children
                const searchbox = try Toybox.new(.{}, .{ .editable_textline = .{
                    .inner_text = .empty,
                    .config = .searchbox,
                } }, .new(@src()));
                const scrollbar = Lego.Specific.Scrollbar.build(
                    toolbar_fnks_rect
                        .plusMargin3(.top, -toolbar_fnks_searchbox_height)
                        .withSize1d(.width, 0.5, .top_right),
                    0,
                    (toolbar_fnks_rect.size.y - toolbar_fnks_searchbox_height) / Lego.Specific.FnkslistElement.height,
                );
                const fnkslist = try Toybox.new(.{}, .{ .scrollable_list = .{ .kind = .fnkslist } }, .new(@src()));
                Toybox.addChildLast(workspace.toolbar_fnks, scrollbar);
                Toybox.addChildLast(workspace.toolbar_fnks, fnkslist);
                Toybox.addChildLast(workspace.toolbar_fnks, searchbox);
            }

            if (workspace.toolbar_fnks.get().tree.first != nothing) { // filter out functions by search
                // TODO(perf): avoid recomputing this when nothing has changed
                // TODO(perf): instead of destroying and rebuilding the list, just hide/show the elements
                const zone = tracy.initZone(@src(), .{ .name = "recomputing fnkslist" });
                defer zone.deinit();

                // not exactly 3 children since the player might have dropped stuff in the area
                const scrollbar, const fnkslist, const searchbox = Toybox.getFirstNChildren(3, workspace.toolbar_fnks);
                const filter_text = searchbox.get().specific.editable_textline.text();

                if (true) { // destroy old
                    var cur = fnkslist.get().tree.first;
                    while (cur != nothing) {
                        const original_tree = Toybox.get(cur).tree;
                        Toybox.pop(cur);
                        Toybox.destroyFloating(cur);
                        cur = original_tree.next;
                    }
                }

                var k: usize = 0;
                const fnkboxes = try workspace.allFnkboxes(false, scratch);
                for (fnkboxes) |index| {
                    // TODO(polish): highlight the matched parts
                    const passed = if (filter_text) |filter|
                        fuzzyFilter(
                            filter,
                            index.children(.fnkbox).box.children(.fnkbox_box).description.get().specific.editable_textline.text() orelse "<empty description>",
                        )
                    else
                        true;
                    if (passed) {
                        Toybox.addChildLast(fnkslist, try Lego.Specific.FnkslistElement.build(
                            k,
                            index,
                        ));
                        k += 1;
                    }
                }
                scrollbar.get().specific.scrollbar.total_length = tof32(k);
            }

            const rect = toolbar_fnks_rect;
            const hot_t = workspace.toolbar_fnks_unfolded_t;
            const p = &Toybox.get(workspace.toolbar_fnks).local_point;
            p.* = .{
                .scale = absolute_camera.size.y / rect.size.y,
                .pos = absolute_camera.get(.top_right),
            };
            p.* = p.applyToLocalPoint(.{ .pos = .new(-1 - (rect.size.x - 1) * hot_t, 0) });

            Toybox.refreshAbsolutePoints(&.{workspace.toolbar_fnks});
        }

        if (true) { // start and advance fnkboxes animations
            const zone = tracy.initZone(@src(), .{ .name = "fnkboxes animations" });
            defer zone.deinit();

            assert(try workspace.valid(scratch));

            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.fnkbox)) |fnkbox| {
                    if (fnkbox.execution) |*execution| {
                        const executor_index = lego.index.children(.fnkbox).executor;
                        switch (execution.source) {
                            .testcase => |testcase| switch (execution.state) {
                                .scrolling_towards_case => {
                                    const offset_from_top: f32 = (Toybox.get(testcase).local_point.pos.y - Lego.Specific.FnkboxBox.relative_top_testcase_pos.y - 2) / 2.5;
                                    const offset_error = offset_from_top - math.clamp(offset_from_top, 0, Lego.Specific.FnkboxBox.visible_testcases - 1);
                                    if (offset_error == 0) {
                                        const new_input = try Toybox.dupeIntoFloating(Lego.Specific.Testcase.children(testcase).input, .new(@src()));
                                        Toybox.changeCoordinates(new_input, Toybox.get(testcase).absolute_point, Toybox.get(workspace.floating_inputs_layer).absolute_point);
                                        Toybox.addChildLast(workspace.floating_inputs_layer, new_input);
                                        undo_stack.storeAllData(lego.index);
                                        execution.state = .starting;
                                        execution.state_t = 0;
                                        execution.original_or_final_input_point = Toybox.get(new_input).local_point;
                                        execution.floating_input_or_output = new_input;
                                    } else {
                                        const scroll = &Toybox.get(testcase).tree.parent.scrollbar(.scrollable_list).get().specific.scrollbar.scroll_target;
                                        const target_scroll = scroll.* + offset_error;
                                        math.lerpTowards(scroll, target_scroll, .{ .duration = 0.5, .precision = 0.05 }, delta_seconds);
                                        math.towards(scroll, target_scroll, 0.1 * delta_seconds);
                                    }
                                },
                                .starting => {
                                    execution.state_t += delta_seconds / 0.8;

                                    const input = execution.floating_input_or_output;
                                    Toybox.get(input).local_point = .lerp(
                                        execution.original_or_final_input_point,
                                        Toybox.get(workspace.floating_inputs_layer).absolute_point.inverseApplyGetLocal(
                                            Toybox.get(Lego.Specific.Executor.children(executor_index).input).absolute_point,
                                        ),
                                        execution.state_t,
                                    );
                                    if (execution.state_t >= 1) {
                                        execution.state = .executing;
                                        execution.state_t = 0;
                                        Toybox.pop(input);
                                        Toybox.changeChild(Lego.Specific.Executor.children(executor_index).input, input);
                                        execution.floating_input_or_output = .nothing;
                                    }
                                },
                                .executing => {
                                    try workspace.advanceExecutorAnimation(executor_index, delta_seconds, scratch);
                                    if (Toybox.get(executor_index).specific.executor.animation == null) {
                                        undo_stack.storeAllData(lego.index);
                                        execution.state = .ending;
                                        execution.state_t = 0;

                                        const result = try resetExecutorAndExtractResult(executor_index, execution.original_garland);
                                        Toybox.changeCoordinates(
                                            result,
                                            .{},
                                            Toybox.get(workspace.floating_inputs_layer).absolute_point,
                                        );
                                        Toybox.addChildLast(workspace.floating_inputs_layer, result);
                                        Toybox.refreshAbsolutePoints(&.{result});

                                        undo_stack.storeAllData(lego.index);
                                        execution.floating_input_or_output = result;
                                        execution.original_or_final_input_point = Toybox.get(workspace.floating_inputs_layer).absolute_point.inverseApplyGetLocal(
                                            Toybox.get(result).absolute_point,
                                        );
                                    }
                                },
                                .ending => {
                                    execution.state_t += delta_seconds / 0.8;

                                    const final_result = execution.floating_input_or_output;
                                    Toybox.get(final_result).local_point = .lerp(
                                        execution.original_or_final_input_point,
                                        Toybox.get(workspace.floating_inputs_layer).absolute_point.inverseApplyGetLocal(
                                            testcase.get().absolute_point.applyToLocalPoint(Lego.Specific.Testcase.relative_actual_point),
                                        ),
                                        execution.state_t,
                                    );

                                    if (true) { // focus on the testcase
                                        const offset_from_top: f32 = (Toybox.get(testcase).local_point.pos.y - Lego.Specific.FnkboxBox.relative_top_testcase_pos.y - 2) / 2.5;
                                        const offset_error = offset_from_top - math.clamp(offset_from_top, 0, Lego.Specific.FnkboxBox.visible_testcases - 1);
                                        const scroll = &Toybox.get(testcase).tree.parent.scrollbar(.scrollable_list).get().specific.scrollbar.scroll_target;
                                        const target_scroll = scroll.* + offset_error;
                                        math.lerpTowards(scroll, target_scroll, .{ .duration = 0.5, .precision = 0.05 }, delta_seconds);
                                        math.towards(scroll, target_scroll, 0.1 * delta_seconds);
                                    }

                                    if (execution.state_t >= 1) {
                                        const new_actual = final_result;
                                        Toybox.changeCoordinates(new_actual, Toybox.parentAbsolutePoint(final_result), Toybox.get(testcase).absolute_point);
                                        Toybox.pop(new_actual);

                                        const old_actual = Lego.Specific.Testcase.children(testcase).actual;
                                        assert(Toybox.get(old_actual).specific.sexpr.kind == .empty);
                                        Toybox.changeChild(old_actual, new_actual);
                                        undo_stack.storeAllData(lego.index);
                                        if (execution.old_testcase_actual_value != nothing) Toybox.destroyFloating(execution.old_testcase_actual_value);
                                        fnkbox.execution = null;
                                        Toybox.refreshAbsolutePoints(&.{new_actual});

                                        // TODO(bug): less hacky
                                        testcase.get().specific.testcase.just_manually_executed = true;
                                    }
                                },
                            },
                            .input => {
                                try workspace.advanceExecutorAnimation(executor_index, delta_seconds, scratch);
                                if (Toybox.get(executor_index).specific.executor.animation == null) {
                                    const result = try resetExecutorAndExtractResult(executor_index, execution.original_garland);
                                    undo_stack.storeAllData(lego.index);
                                    if (execution.old_testcase_actual_value != nothing) Toybox.destroyFloating(execution.old_testcase_actual_value);
                                    fnkbox.execution = null;
                                    Toybox.addChildLast(workspace.main_area, result);
                                    Toybox.changeCoordinates(result, .{}, workspace.main_area.get().absolute_point);
                                }
                            },
                        }
                        assert(try workspace.valid(scratch));
                    } else {
                        const executor_index = Lego.Specific.Fnkbox.children(lego.index).executor;
                        if (Lego.Specific.Executor.shouldStartExecution(executor_index)) {
                            const original_garland_index = Lego.Specific.Executor.children(executor_index).garland;
                            const backup_garland_index = try Toybox.dupeIntoFloating(original_garland_index, .new(@src()));

                            undo_stack.storeAllData(lego.index);
                            fnkbox.execution = .{
                                .source = .input,
                                .original_garland = backup_garland_index,
                                .original_or_final_input_point = undefined,
                                .state_t = undefined,
                                .old_testcase_actual_value = .nothing,
                                .state = .executing,
                            };

                            // TODO(game)
                            // assert(fnkbox.executor.garland.fnkname == null);
                            // if (fnkbox.folded) fnkbox.executor.garland.fnkname = try fnkbox.fnkname.clone(&workspace.hover_pool);
                            // try toybox.undo_stack.append(.{ .specific = .{ .started_execution_fnkbox_from_input = .{
                            //     .fnkbox = k,
                            //     .input = try fnkbox.executor.input.clone(&mem.hover_pool),
                            // } } });
                            // try workspace.canonizeAfterChanges(mem);
                        }
                        // assert(try workspace.valid(scratch));
                    }
                }
            }
        }

        if (true) { // start and advance executors animations
            const zone = tracy.initZone(@src(), .{ .name = "executors animations" });
            defer zone.deinit();

            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.executor)) |executor| {
                    assert(!(executor.controlled_by_parent_fnkbox and executor.used_for_bg_computation));
                    if (executor.controlled_by_parent_fnkbox) continue;
                    // if (executor.used_for_bg_computation) continue;
                    try workspace.advanceExecutorAnimation(lego.index, delta_seconds, scratch);
                }
            }
        }

        if (true) { // enable/disable buttons and other things
            const zone = tracy.initZone(@src(), .{ .name = "enable/disable buttons" });
            defer zone.deinit();

            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.button)) |button| {
                    button.enabled = switch (button.action) {
                        .launch_testcase, .delete_testcase => Toybox.get(Toybox.findAncestor(lego.index, .fnkbox)).specific.fnkbox.execution == null,
                        .see_failing_testcase => Toybox.get(Toybox.findAncestor(lego.index, .fnkbox)).specific.fnkbox.status != .solved,
                        .scroll_up, .scroll_down, .reset_bubble, .add_testcase => true,
                        .unlock_hint => lego.tree.parent.get().specific.bubble.visibleUnlockHints(),
                        // TODO(game): set this to true to use the toggle_skip ui
                        .toggle_skip_fnk => false,
                        .create_fnkbox_for_row => lego.tree.parent.children(.scorer_row).fnkname.get().specific.sexpr.kind == .empty,
                        .stop_execution => true,
                    };
                    button.extra_info = switch (button.action) {
                        else => .none,
                        .see_failing_testcase => .{ .see_failing_testcase = Toybox.get(Toybox.findAncestor(lego.index, .fnkbox)).specific.fnkbox.status },
                    };
                    button.latched = switch (button.action) {
                        .launch_testcase,
                        .delete_testcase,
                        .see_failing_testcase,
                        .scroll_up,
                        .scroll_down,
                        .reset_bubble,
                        .unlock_hint,
                        .create_fnkbox_for_row,
                        .add_testcase,
                        .stop_execution,
                        => false,
                        .toggle_skip_fnk => button.latched,
                    };
                }
                if (lego.specific.as(.executor_crank)) |crank| {
                    crank.enabled = Toybox.findAncestor(lego.index, .executor).get().specific.executor.animation != null;
                }
            }
        }

        if (true) { // set fnkboxes for fnkname_holders
            const zone = tracy.initZone(@src(), .{ .name = "set fnkboxes for fnkname_holders" });
            defer zone.deinit();

            const fnkboxes = try workspace.allFnkboxes(false, scratch);
            var map: std.HashMapUnmanaged(u32, Lego.Index, kommon.AutoContextForIntKeys(u32), std.hash_map.default_max_load_percentage) = .empty;
            try map.ensureUnusedCapacity(scratch, @intCast(fnkboxes.len));
            for (fnkboxes) |index| {
                const fnkname_hash = Lego.Specific.Sexpr.hash(index.children(.fnkbox).fnkname);
                map.putAssumeCapacityNoClobber(fnkname_hash, index);
            }

            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                switch (lego.specific) {
                    .fnkname_holder => |*fnkname_holder| {
                        fnkname_holder.fnkbox = map.get(Lego.Specific.Sexpr.hash(
                            lego.index.children(.fnkname_holder).fnkname,
                        )) orelse .nothing;
                    },
                    else => {},
                }
            }
        }

        if (true) { // reset per-frame variables
            const zone = tracy.initZone(@src(), .{ .name = "reset per-frame variables" });
            defer zone.deinit();

            // TODO(optim): there must be better ways to do this
            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.case)) |case| {
                    case.next_point_extra = .{};
                    case.fnkname_holder_extra = .{};
                }
                if (lego.specific.as(.newcase)) |newcase| {
                    newcase.offset_ghost = .nothing;
                }
                if (lego.specific.as(.sexpr)) |sexpr| {
                    sexpr.bindings_all = .empty;
                    sexpr.bindings_unbound = .empty;
                }
                if (lego.specific.as(.editable_textline)) |editable_textline| {
                    editable_textline.cursor_points = .empty;
                }
            }

            _ = workspace.arena_for_oneframe_data.reset(.retain_capacity);
        }

        if (true) { // update bindings names
            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.tag() == .sexpr and lego.specific.sexpr.kind == .atom_var) {
                    const name = lego.specific.sexpr.atom_name;
                    const unbound = lego.specific.sexpr.emerging_value == nothing;
                    var cur = lego.index;
                    while (cur.hasTag(.sexpr)) : (cur = cur.get().tree.parent) {
                        try cur.get().specific.sexpr.bindings_all.append(workspace.arena_for_oneframe_data.allocator(), name);
                        if (unbound) {
                            try cur.get().specific.sexpr.bindings_unbound.append(workspace.arena_for_oneframe_data.allocator(), name);
                        }
                    }
                }
            }
        }

        if (true) { // update bindings_t
            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.executor)) |executor_asdf| {
                    const executor: *Lego.Specific.Executor = executor_asdf;
                    const bindings = Lego.Specific.Executor.bindingsActive(lego.index);
                    const t: f32 = if (bindings.anim_t) |anim_t| math.smoothstep(anim_t, 0, 0.4) else 0;

                    if (executor.animation) |animation| {
                        const parent_pattern = animation.active_case.case().pattern;
                        var cur = animation.active_case;
                        while (cur != nothing) : (cur = Toybox.next_preordered(cur, animation.active_case).next) {
                            if (Toybox.get(cur).specific.as(.sexpr)) |sexpr| {
                                if (sexpr.kind == .atom_var and
                                    (!sexpr.is_pattern or Toybox.isAncestor(parent_pattern, cur)))
                                {
                                    for (animation.new_bindings) |binding| {
                                        if (std.mem.eql(u8, binding.name, sexpr.atom_name)) {
                                            sexpr.emerging_value_t = t;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // doesn't include dragging and snapping to dropzone, despite that being just the spring between the mouse cursor/dropzone and the grabbed thing
        workspace.updateSprings(workspace.roots(.all).constSlice(), hot_and_dropzone, delta_seconds);

        if (true) Toybox.refreshAbsolutePoints(workspace.roots(.all).constSlice());

        if (true) { // load/unload testcases depending on their visibility
            const zone = tracy.initZone(@src(), .{ .name = "load/unload testcases" });
            defer zone.deinit();

            const fnkboxes = try workspace.allFnkboxes(false, scratch);
            for (fnkboxes) |fnkbox_index| {
                const testcases_parent = fnkbox_index.children(.fnkbox).box.children(.fnkbox_box).testcases_area;
                assert(testcases_parent.hasTag(.scrollable_list));
                const parent_box: Bounds = testcases_parent.get().localBoundingBoxThatContainsSelfAndAllChildren();
                const child_box: Bounds = .fromRect(Lego.Specific.Testcase.relative_bounding_box);
                var cur = testcases_parent.get().tree.first;
                while (cur != nothing) : (cur = cur.get().tree.next) {
                    if (!cur.hasTag(.testcase)) {
                        assert(cur.get().specific.button.action == .add_testcase);
                        continue;
                    }
                    const is_visible = cur.get().local_point.applyToLocalBounds(child_box).intersect(parent_box) != null or
                        fnkbox_index.get().specific.fnkbox.hasExecutionOverTestcase(cur);
                    if (is_visible) {
                        try ensureLoadedTestcase(cur, scratch, workspace.gpa_for_atom_names);
                    } else {
                        tryToUnloadTestcase(cur);
                    }
                }
                Toybox.refreshAbsolutePoints(&.{testcases_parent});
            }
        }

        if (true) { // for all testcases, set the correct button (launch or delete)
            const zone = tracy.initZone(@src(), .{ .name = "update testcases button" });
            defer zone.deinit();

            const fnkboxes = try workspace.allFnkboxes(false, scratch);
            for (fnkboxes) |fnkbox_index| {
                const testcases_parent = fnkbox_index.children(.fnkbox).box.children(.fnkbox_box).testcases_area;
                assert(testcases_parent.hasTag(.scrollable_list));
                var cur = testcases_parent.get().tree.first;
                while (cur != nothing) : (cur = cur.get().tree.next) {
                    if (cur.hasTag(.testcase)) {
                        cur.children(.testcase).play_button.get().specific.button.action = if (cur.children(.testcase).input.get().specific.sexpr.kind == .empty)
                            .delete_testcase
                        else
                            .launch_testcase;
                    }
                }
            }
        }

        if (true) { // delete testcases to avoid accidental damage
            const zone = tracy.initZone(@src(), .{ .name = "delete mangled testcases" });
            defer zone.deinit();

            const fnkboxes = try workspace.allFnkboxes(false, scratch);
            for (fnkboxes) |fnkbox_index| {
                const testcases_parent = fnkbox_index.children(.fnkbox).box.children(.fnkbox_box).testcases_area;
                assert(testcases_parent.hasTag(.scrollable_list));
                var cur = testcases_parent.get().tree.first;
                var next: Lego.Index = undefined;
                while (cur != nothing) : (cur = next) {
                    next = cur.get().tree.next;
                    if (!cur.hasTag(.testcase)) continue;

                    if (!cur.get().specific.testcase.loaded) continue;
                    if (cur.get().specific.testcase.source) |source| {
                        const mangled = source.input_hash != Lego.Specific.Sexpr.hash(cur.children(.testcase).input) or
                            source.expected_hash != Lego.Specific.Sexpr.hash(cur.children(.testcase).expected);

                        if (mangled) {
                            Toybox.pop(cur);
                            Toybox.destroyFloating(cur);
                        }
                    }
                }
            }
        }

        const something_happened = undo_stack.anyChangesThisFrame();
        if (true or something_happened or !workspace.did_first_frame) {
            try workspace.canonizeAfterChanges(scratch);
        }

        assert(try workspace.valid(scratch));

        // There should be no further changes
        undo_stack.startFrame();
        defer assert(!undo_stack.anyChangesThisFrame());

        if (drawer) |drwr| { // recompute cursor_points
            var lego_it = toybox.all_legos.iterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.tag() == .editable_textline) {
                    const cursor_points = &lego.specific.editable_textline.cursor_points;
                    assert(cursor_points.items.len == 0);
                    const config = lego.specific.editable_textline.config;
                    const text = lego.specific.editable_textline.text();

                    const text_renderer = &drwr.canvas.text_renderers[0];
                    const metrics = text_renderer.font_info.value.metrics;
                    const em = config.em;
                    const pos: Canvas.TextRenderer.TextPosition = config.local_position;
                    const info = try text_renderer.quadsForLineV2(text orelse config.text_if_empty, em, undefined, scratch);
                    const delta = text_renderer.deltaToAchieve(pos, info.total_advance, em);

                    try cursor_points.ensureTotalCapacityPrecise(
                        workspace.arena_for_oneframe_data.allocator(),
                        info.cursor_offsets.len,
                    );
                    assert(metrics.ascender < 0 and metrics.descender > 0);
                    for (info.cursor_offsets) |asdf| {
                        cursor_points.appendAssumeCapacity(.{
                            .relative_pos = delta.addX(asdf.offset).addY(metrics.descender * em),
                            .index = if (text == null) 0 else asdf.index,
                            .relative_height = (metrics.descender - metrics.ascender) * em,
                        });
                    }
                }
            }
        }

        if (true) { // set lenses data
            const zone = tracy.initZone(@src(), .{ .name = "set lenses data" });
            defer zone.deinit();

            const allocator = workspace.arena_for_oneframe_data.allocator();
            const microscopes = try Toybox.getChildrenUnknown(scratch, workspace.lenses_layer);
            for (microscopes, 0..) |microscope, k| {
                const source, const target = Toybox.getChildrenExact(2, microscope);
                const source_pos = Toybox.get(source).absolute_point.pos;
                const target_pos = Toybox.get(target).absolute_point.pos;
                const source_lens = &Toybox.get(source).specific.lens;
                const source_radius = source_lens.local_radius * Toybox.get(source).absolute_point.scale;
                const target_lens = &Toybox.get(target).specific.lens;
                const target_radius = target_lens.local_radius * Toybox.get(target).absolute_point.scale;

                source_lens.transform = .identity;
                source_lens.is_target = false;
                target_lens.transform = .fromLenses(source_pos, source_radius, target_pos, target_radius);
                target_lens.is_target = true;

                var all_roots: std.ArrayListUnmanaged(Lego.Index) = .empty;
                try all_roots.appendSlice(allocator, workspace.roots(.{
                    .include_hand = true,
                    .include_toolbars = true,
                    .include_floating_inputs = true,
                    .include_lenses = false,
                }).constSlice());
                try all_roots.appendSlice(allocator, microscopes[0..k]);

                var all_roots_except_hand: std.ArrayListUnmanaged(Lego.Index) = .empty;
                try all_roots_except_hand.appendSlice(allocator, workspace.roots(.{
                    .include_hand = false,
                    .include_toolbars = true,
                    .include_floating_inputs = true,
                    .include_lenses = false,
                }).constSlice());
                try all_roots_except_hand.appendSlice(allocator, microscopes[0..k]);

                source_lens.roots_to_draw = all_roots.items;
                source_lens.roots_to_interact = &.{};
                target_lens.roots_to_draw = all_roots.items;
                target_lens.roots_to_interact = all_roots_except_hand.items;
            }
        }

        assert(try workspace.valid(scratch));
    }

    fn cameraCenter(workspace: *Workspace) Point {
        return workspace.main_area.get().local_point.inverseApplyGetLocal(.{});
    }

    fn centerCameraAt(workspace: *Workspace, point: Point, instant: bool) void {
        if (instant) {
            workspace.main_area.get().local_point = Point.inverseApplyGetLocal(point, .{});
        } else {
            Toybox.setLocalPointSmooth(workspace.main_area, Point.inverseApplyGetLocal(point, .{}));
        }
        workspace.refreshCamera();
    }

    fn refreshCamera(workspace: *Workspace) void {
        for (workspace.roots(.with_main_camera).constSlice()) |root| {
            if (root != workspace.main_area) {
                Toybox.get(root).local_point = Toybox.get(workspace.main_area).local_point.applyToLocalPoint(Toybox.get(workspace.main_area).visual_offset);
            }
        }
        Toybox.refreshAbsolutePoints(workspace.roots(.with_main_camera).constSlice());
    }

    fn setGrabbing(workspace: *Workspace, grabbing: Grabbing) void {
        toybox.undo_stack.append(.{ .set_grabbing = workspace.grabbing });
        workspace.grabbing = grabbing;
    }

    fn setHandLayer(workspace: *Workspace, index: Lego.Index) void {
        toybox.undo_stack.append(.{ .set_handlayer = workspace.hand_layer });
        workspace.hand_layer = index;
    }

    fn forcefullyStopExecutor(executor_index: Lego.Index) !void {
        const executor = &Toybox.get(executor_index).specific.executor;
        if (executor.animation) |*animation| {
            const old_current = executor_index.children(.executor).garland;
            Toybox.changeChildAndDestroyOld(old_current, try Toybox.buildGarland(
                old_current.get().local_point,
                &.{},
                .new(@src()),
            ));

            const old_invoked = animation.invoked_fnk;
            if (old_invoked != nothing) {
                const new_invoked = try Toybox.buildGarland(
                    old_invoked.get().local_point,
                    &.{},
                    .new(@src()),
                );
                Toybox.changeChildAndDestroyOld(old_invoked, new_invoked);
                animation.invoked_fnk = new_invoked;
            }

            if (true) { // remove enqueued
                var cur = executor.first_enqueued;
                executor.first_enqueued = .nothing;
                while (cur != nothing) {
                    const next = cur.get().specific.garland.next_enqueued;
                    Toybox.pop(cur);
                    Toybox.destroyFloating(cur);
                    cur = next;
                }
            }
        }
    }

    fn advanceExecutorAnimation(workspace: *Workspace, executor_index: Lego.Index, delta_seconds: f32, scratch: std.mem.Allocator) !void {
        const Executor = Lego.Specific.Executor;
        const executor = &Toybox.get(executor_index).specific.executor;
        const visible = !executor.used_for_bg_computation;
        // const floating_inputs_layer = workspace.floating_inputs_layer;
        const floating_inputs_layer = if (visible) workspace.floating_inputs_layer else workspace.invisible_floating_inputs_layer;
        if (executor.animation) |*animation| {
            toybox.undo_stack.storeAllData(executor_index);
            if (visible) {
                animation.t += delta_seconds * Executor.Controls.speedScale(Executor.getBrakeT(executor_index));
            } else {
                animation.t = 1;
            }
            if (animation.t >= 1) {
                Toybox.popWithUndoAndChangingCoords(animation.garland_fnkname);
                if (animation.matching) {
                    if (true) { // fill variables
                        var cur = animation.active_case;
                        while (cur != nothing) {
                            const next = Toybox.next_preordered(cur, animation.active_case).next;
                            defer cur = next;
                            if (Toybox.get(cur).specific.as(.sexpr)) |sexpr| {
                                if (sexpr.emerging_value != nothing) {
                                    if (!sexpr.is_pattern) {
                                        sexpr.emerging_value.get().local_point = cur.get().local_point;
                                        Toybox.changeChild(cur, sexpr.emerging_value);
                                        cur.get().specific.sexpr.emerging_value = .nothing;
                                        Toybox.destroyFloating(cur);
                                    }
                                }
                            }
                        }
                    }

                    const old_case_parts = Lego.Specific.Case.destroyForParts(animation.active_case);
                    const old_input = Executor.children(executor_index).input;
                    const old_garland = Executor.children(executor_index).garland;
                    const next_garland = old_case_parts.next;

                    Toybox.changeChildWithUndoAndAlsoCoords(
                        old_input,
                        old_case_parts.template,
                    );

                    const new_garland = blk: {
                        if (animation.invoked_fnk != nothing) {
                            Toybox.pop(animation.invoked_fnk);
                            if (next_garland.garland().hasChildCases()) {
                                // TODO(game)
                                // Toybox.get(next_garland).specific.garland.enqueued_parent_pill_index = ??;
                                toybox.undo_stack.storeAllData(next_garland);
                                Toybox.get(next_garland).specific.garland.next_enqueued = executor.first_enqueued;
                                toybox.undo_stack.storeAllData(executor_index);
                                executor.first_enqueued = next_garland;
                                Toybox.addChildLastWithoutChangingAbsPoint(floating_inputs_layer, next_garland);
                            } else {
                                Toybox.destroyFloating(next_garland);
                            }
                            break :blk animation.invoked_fnk;
                        } else if (next_garland.garland().hasChildCases()) {
                            // TODO(game)
                            // parent_pill_index = executor.prev_pills.items.len - 1;
                            break :blk next_garland;
                        } else if (executor.first_enqueued != nothing) {
                            Toybox.destroyFloating(next_garland);
                            const asdf = executor.first_enqueued;
                            toybox.undo_stack.storeAllData(executor_index);
                            executor.first_enqueued = Toybox.get(asdf).specific.garland.next_enqueued;
                            // TODO(game)
                            // parent_pill_index = Toybox.get(asdf).specific.garland.enqueued_parent_pill_index;
                            Toybox.pop(asdf);
                            break :blk asdf;
                        } else {
                            Toybox.destroyFloating(next_garland);
                            break :blk try Toybox.buildGarland(.{}, &.{}, .new(@src()));
                        }
                    };

                    Toybox.changeChildWithUndo(old_garland, new_garland);

                    Toybox.destroyFloating(old_garland);

                    toybox.undo_stack.storeAllData(executor_index);
                    if (visible) {
                        executor.first_pill = try Lego.Specific.Pill.build(old_case_parts.pattern.get().absolute_point, executor.first_pill, .{
                            .pattern = old_case_parts.pattern,
                            .input = old_input,
                            .fnkname_holder_call = old_case_parts.fnkname_holder,
                            .fnkname_response = animation.garland_fnkname,
                            // TODO(game)
                            // They don't include previous bindings, since they have now been merged
                            // .bindings = try mem.gpa.dupe(Binding, animation.new_bindings),
                        });
                        Toybox.addChildLastWithoutChangingAbsPoint(floating_inputs_layer, executor.first_pill);
                    } else {
                        Toybox.destroyFloating(old_case_parts.pattern);
                        Toybox.destroyFloating(old_input);
                        Toybox.destroyFloating(old_case_parts.fnkname_holder);
                        Toybox.destroyFloating(animation.garland_fnkname);
                    }
                } else {
                    assert(animation.new_bindings.len == 0);
                    Toybox.destroyFloating(try Lego.Specific.Garland.stealFnkname(
                        Lego.Specific.Executor.children(executor_index).garland,
                        animation.garland_fnkname,
                    ));
                    Toybox.pop(animation.active_case);
                    Toybox.destroyFloating(animation.active_case);
                }
                toybox.undo_stack.storeAllData(executor_index);
                executor.animation = null;
            }
        }

        if (Executor.shouldStartExecution(executor_index)) {
            const value = Lego.Specific.Executor.children(executor_index).input;

            // pop first case for execution
            const garland_index = Executor.children(executor_index).garland;
            const first_segment = Lego.Specific.Garland.children(garland_index).cases.get().tree.first;
            assert(first_segment.hasTag(.newcase));
            const first_case = Toybox.get(first_segment).tree.first;
            Lego.Specific.Garland.popCase(first_case);
            Toybox.addChildLast(floating_inputs_layer, first_case);

            const pattern = Lego.Specific.Case.children(first_case).pattern;

            // TODO(optim): memory management
            var new_bindings: std.ArrayList(Binding) = .init(workspace.gpa_for_bindings);
            const matching = try Lego.Specific.Sexpr.generateBindings(value, pattern, &new_bindings);
            if (!matching) {
                new_bindings.clearAndFree();
            }
            const new_bindings_slice = try new_bindings.toOwnedSlice();
            const invoked_fnk: Lego.Index = if (!matching)
                .nothing
            else blk: {
                const offset = 3.0;
                const function_point = Lego.Specific.Executor.relative_garland_point
                    .applyToLocalPoint(.{ .pos = .new(2 * offset + 6, 6 * offset) });
                if (try workspace.getSkippedExecution(first_case.case().template, new_bindings_slice, first_case.case().fnkname_holder, function_point, scratch)) |garland| {
                    Toybox.addChildLast(floating_inputs_layer, garland);
                    break :blk garland;
                } else if (try workspace.getGarlandForFnk(first_case.case().fnkname_holder.children(.fnkname_holder).fnkname, function_point, scratch)) |garland| {
                    Toybox.addChildLast(floating_inputs_layer, garland);
                    break :blk garland;
                } else {
                    break :blk .nothing;
                }
            };
            const garland_fnkname = try Lego.Specific.Garland.stealFnkname(garland_index, null);
            Toybox.addChildLast(floating_inputs_layer, garland_fnkname);
            toybox.undo_stack.storeAllData(executor_index);
            executor.animation = .{
                .matching = matching,
                .active_case = first_case,
                .invoked_fnk = invoked_fnk,
                .new_bindings = new_bindings_slice,
                .garland_fnkname = garland_fnkname,
            };

            if (matching) {
                var cur = first_case;
                while (cur != nothing) : (cur = Toybox.next_preordered(cur, first_case).next) {
                    if (Toybox.get(cur).specific.as(.sexpr)) |sexpr| {
                        if (sexpr.kind == .atom_var and !sexpr.is_pattern) {
                            for (new_bindings_slice) |binding| {
                                if (std.mem.eql(u8, binding.name, sexpr.atom_name)) {
                                    toybox.undo_stack.storeAllData(cur);
                                    sexpr.emerging_value = try Toybox.dupeIntoFloating(binding.value, .new(@src()));
                                    Toybox.setAbsolutePoint(sexpr.emerging_value, Toybox.get(cur).absolute_point);
                                    Toybox.refreshAbsolutePoints(&.{sexpr.emerging_value});
                                }
                            }
                        }
                    }
                }

                const new_pattern = Lego.Specific.Case.children(first_case).pattern;
                cur = new_pattern;
                while (cur != nothing) : (cur = Toybox.next_preordered(cur, new_pattern).next) {
                    const sexpr = &cur.get().specific.sexpr;
                    assert(sexpr.is_pattern);
                    if (sexpr.kind == .atom_var) {
                        for (new_bindings_slice) |binding| {
                            if (std.mem.eql(u8, binding.name, sexpr.atom_name)) {
                                toybox.undo_stack.storeAllData(cur);
                                sexpr.emerging_value = try Toybox.dupeIntoFloating(binding.value, .new(@src()));
                                Lego.Specific.Sexpr.setIsPattern(sexpr.emerging_value, true);
                                sexpr.emerging_value.get().specific.sexpr.is_pattern_t = 1;
                                Toybox.setAbsolutePoint(sexpr.emerging_value, Toybox.get(cur).absolute_point);
                                Lego.Specific.Sexpr.updateLocalPositionsAndOfChildren(sexpr.emerging_value);
                                Toybox.refreshAbsolutePoints(&.{sexpr.emerging_value});
                            }
                        }
                    }
                }
            }
        }

        if (executor.animation == null) { // remove pills
            var cur = executor.first_pill;
            executor.first_pill = .nothing;
            while (cur != nothing) : (cur = cur.get().specific.pill.next_pill) {
                toybox.undo_stack.storeAllData(cur);
                cur.get().specific.pill.remaining_lifetime = 1;
                cur.get().specific.pill.velocity = .new(-4, 0);
            }
        }

        if (executor.animation == null) { // remove enqueued
            // TODO(game): smooth anim
            var cur = executor.first_enqueued;
            executor.first_enqueued = .nothing;
            while (cur != nothing) {
                const next = cur.get().specific.garland.next_enqueued;
                Toybox.pop(cur);
                Toybox.destroyFloating(cur);
                cur = next;
            }
        }

        const crank = Executor.children(executor_index).controls.children(.executor_controls).crank;
        const new_crank_value = if (executor.animation) |anim| anim.t else 0;
        if (new_crank_value != crank.get().specific.executor_crank.value) {
            toybox.undo_stack.storeAllData(crank);
            crank.get().specific.executor_crank.value = new_crank_value;
        }
    }

    fn resetExecutorAndExtractResult(executor_index: Lego.Index, original_garland: Lego.Index) !Lego.Index {
        const result = Lego.Specific.Executor.children(executor_index).input;
        const undo_stack = &toybox.undo_stack;

        Toybox.changeChildWithUndoAndAlsoCoords(
            result,
            try Toybox.buildSexpr(.{}, .empty, false, false, .new(@src())),
        );

        const children = Lego.Specific.Executor.children(executor_index);
        // const executor = &Toybox.get(executor_index).specific.executor;
        Toybox.changeChildWithUndo(children.garland, original_garland);
        Toybox.changeChildWithUndoAndAlsoCoords(
            children.input,
            try Toybox.buildSexpr(
                Lego.Specific.Executor.relative_input_point,
                .empty,
                false,
                false,

                .new(@src()),
            ),
        );
        Toybox.destroyFloating(children.garland);
        Toybox.destroyFloating(children.input);
        undo_stack.storeAllData(executor_index);
        Toybox.get(executor_index).specific.executor.garland_appearing_t = -1;
        // TODO(game)
        // fnkbox.executor.prev_pills.clearRetainingCapacity();
        // fnkbox.executor.enqueued_stack.clearRetainingCapacity();

        return result;
    }

    fn fnkboxWithName(workspace: *Workspace, name: Lego.Index, scratch: std.mem.Allocator) !?Lego.Index {
        for (try workspace.allFnkboxes(false, scratch)) |fnkbox_index| {
            if (Lego.Specific.Sexpr.equalValue(fnkbox_index.children(.fnkbox).fnkname, name)) {
                return fnkbox_index;
            }
        } else return null;
    }

    fn allFnkboxes(workspace: *Workspace, include_locked: bool, allocator: std.mem.Allocator) ![]const Lego.Index {
        var result: std.ArrayListUnmanaged(Lego.Index) = .empty;

        var lego_it = toybox.all_legos.iterator(0);
        while (lego_it.next()) |lego| {
            if (!lego.exists) continue;
            if (lego.specific.tag() != .fnkbox) continue;
            // don't include fnkboxes that are part of a blueprint, or other strange situations
            if (workspace.isFreefloating(lego.index)) continue;
            if (!include_locked) {
                if (Toybox.findAncestor(lego.index, .bubble).getSafe()) |b| {
                    if (b.specific.bubble.locked) continue;
                }
            }
            try result.append(allocator, lego.index);
        }

        return try result.toOwnedSlice(allocator);
    }

    /// duplicates the garland and returns it
    fn getGarlandForFnk(
        workspace: *Workspace,
        fnkname: Lego.Index,
        new_point: Point,
        scratch: std.mem.Allocator,
    ) !?Lego.Index {
        const sexpr = Toybox.get(fnkname).specific.sexpr;
        if (sexpr.kind == .empty or
            (sexpr.kind == .atom_lit and
                std.mem.eql(u8, sexpr.atom_name, "identity")))
        {
            return null;
        }

        const all_fnks: core.FnkCollection, _ = try workspace.getAllFnks(scratch);
        const fnkname_value = try fnkname.get().specific.sexpr.toOldCoreValue(scratch);
        var temp_mem: core.VeryPermamentGameStuff = .init(scratch);
        defer temp_mem.deinit();
        var scoring_run: core.ScoringRun = try .initFromFnks(all_fnks, &temp_mem);
        defer scoring_run.deinit(false);

        const fnkbody = scoring_run.findFunktion(fnkname_value, .new) catch |err| switch (err) {
            error.OutOfMemory => |x| return x,
            error.BAD_INPUT,
            error.FnkNotFound,
            error.NoMatchingCase,
            error.UsedUndefinedVariable,
            error.TookTooLong,
            => panic("unreachable? {s}", .{@errorName(err)}),
            error.InvalidMetaFnk => return null,
        } orelse return null;
        const garland = try Lego.Specific.Garland.buildFromOldCoreValueV0(new_point, fnkbody.*, scratch, .new(@src()));
        const original_fnkname = try Lego.Specific.Garland.stealFnkname(
            garland,
            try Toybox.dupeIntoFloating(fnkname, .new(@src())),
        );
        assert(original_fnkname.get().specific.sexpr.kind == .empty);
        Toybox.destroyFloating(original_fnkname);
        return garland;
    }

    fn getSkippedExecution(
        workspace: *Workspace,
        input_unresolved: Lego.Index,
        bindings: []const Binding,
        fnkname_holder: Lego.Index,
        new_point: Point,
        scratch: std.mem.Allocator,
    ) !?Lego.Index {
        const fnkname = fnkname_holder.children(.fnkname_holder).fnkname;
        const force_skip = fnkname_holder.children(.fnkname_holder).toggle_skip.get().specific.button.latched;

        const fnkname_value = try fnkname.get().specific.sexpr.toOldCoreValue(scratch);

        if (!(force_skip or fnkname_value.isTheLit("eqAtoms?"))) {
            return null;
        }

        const input_value = try input_unresolved.get().specific.sexpr.toOldCoreValueResolving(bindings, scratch);

        const all_fnks: core.FnkCollection, _ = try workspace.getAllFnks(scratch);
        var temp_mem: core.VeryPermamentGameStuff = .init(scratch);
        defer temp_mem.deinit();
        var scoring_run: core.ScoringRun = try .initFromFnks(all_fnks, &temp_mem);
        defer scoring_run.deinit(false);

        var exec: core.ExecutionThread = try .init(input_value, fnkname_value, &scoring_run, .new);
        defer exec.deinit();
        const result_value = try exec.getFinalResultBoundedV2(&scoring_run, .new);

        const garland = try Toybox.buildGarland(new_point, &.{
            try Toybox.buildCase(undefined, .{
                .pattern = try Lego.Specific.Sexpr.buildFromOldCoreValue(.{}, input_value, true, false, .new(@src())),
                .template = try Lego.Specific.Sexpr.buildFromOldCoreValue(.{}, result_value, false, false, .new(@src())),
                .fnkname = null,
                .next = null,
            }, .new(@src())),
        }, .new(@src()));
        const original_fnkname = try Lego.Specific.Garland.stealFnkname(
            garland,
            try Toybox.dupeIntoFloating(fnkname, .new(@src())),
        );
        assert(original_fnkname.get().specific.sexpr.kind == .empty);
        Toybox.destroyFloating(original_fnkname);
        return garland;
    }

    fn ensureLoadedTestcase(testcase_index: Lego.Index, scratch: std.mem.Allocator, text_allocator: std.mem.Allocator) !void {
        if (testcase_index.get().specific.testcase.loaded) return;
        var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);
        defer pool.deinit();
        // TODO(optim-late): tune this number
        // try pool.preheat(1_000_000);
        const source = testcase_index.get().specific.testcase.source.?;
        const sample = (try levels[source.level].generate_sample(source.sample, &pool, scratch, text_allocator)).?;

        Toybox.changeChildAndDestroyOld(testcase_index.children(.testcase).input, try Lego.Specific.Sexpr.buildFromOldCoreValue(
            Lego.Specific.Testcase.relative_input_point,
            sample.input,
            false,
            false,
            .new(@src()),
        ));
        Toybox.changeChildAndDestroyOld(testcase_index.children(.testcase).expected, try Lego.Specific.Sexpr.buildFromOldCoreValue(
            Lego.Specific.Testcase.relative_expected_point,
            sample.expected,
            false,
            false,
            .new(@src()),
        ));
        testcase_index.get().specific.testcase.loaded = true;
    }

    fn tryToUnloadTestcase(testcase_index: Lego.Index) void {
        if (!testcase_index.get().specific.testcase.loaded) return;

        if (testcase_index.get().specific.testcase.source) |source| {
            if (source.input_hash != Lego.Specific.Sexpr.hash(testcase_index.children(.testcase).input) or
                source.expected_hash != Lego.Specific.Sexpr.hash(testcase_index.children(.testcase).expected))
            {
                testcase_index.get().specific.testcase.source = null;
                return;
            }

            Toybox.changeChildAndDestroyOld(testcase_index.children(.testcase).input, try Toybox.buildSexpr(
                Lego.Specific.Testcase.relative_input_point,
                .empty,
                false,
                false,
                .new(@src()),
            ));
            Toybox.changeChildAndDestroyOld(testcase_index.children(.testcase).expected, try Toybox.buildSexpr(
                Lego.Specific.Testcase.relative_expected_point,
                .empty,
                false,
                false,
                .new(@src()),
            ));
            testcase_index.get().specific.testcase.loaded = false;
        }
    }

    fn launchTestcase(testcase_index: Lego.Index) !void {
        assert(Toybox.get(testcase_index).specific.tag() == .testcase);
        const fnkbox_index = Toybox.findAncestor(testcase_index, .fnkbox);
        const fnkbox = &Toybox.get(fnkbox_index).specific.fnkbox;
        assert(fnkbox.execution == null);
        const executor_index = Lego.Specific.Fnkbox.children(fnkbox_index).executor;

        const old_actual = Lego.Specific.Testcase.children(testcase_index).actual;
        const new_actual = try Toybox.buildSexpr(Lego.Specific.Testcase.relative_actual_point, .empty, false, false, .new(@src()));
        Toybox.changeChild(old_actual, new_actual);

        const original_garland_index = Lego.Specific.Executor.children(executor_index).garland;
        const backup_garland_index = try Toybox.dupeIntoFloating(original_garland_index, .new(@src()));

        toybox.undo_stack.storeAllData(fnkbox_index);
        fnkbox.execution = .{
            .source = .{ .testcase = testcase_index },
            .old_testcase_actual_value = old_actual,
            .original_garland = backup_garland_index,
            .original_or_final_input_point = undefined,
            .state_t = undefined,
            .state = .scrolling_towards_case,
        };
    }

    fn grabbingSomethingIllegal(workspace: *const Workspace) bool {
        return switch (workspace.grabbing.index.get().specific) {
            else => false,
            .executor_crank => |crank| !crank.enabled,
        };
    }

    // /// saves each lego
    // pub fn save(workspace: *Workspace, out: std.io.AnyWriter, scratch: std.mem.Allocator) !void {
    //     const version: u32 = 0;
    //     try out.writeInt(u32, version, ENDIANNESS);

    //     writeLen(out, toybox.all_legos.items.len);
    //     for (toybox.)
    // }

    // pub fn load(dst: *Workspace, in: std.io.AnyReader, scratch: std.mem.Allocator) !void {
    //     const version = try in.readInt(u32, .little);
    //     if (version != 0) @panic("Unsupported file version");
    // }

    const SavedTestcaseTag = enum(u8) { default, custom };

    /// only saves fnkboxes
    pub fn save(workspace: *Workspace, out: std.io.AnyWriter, scratch: std.mem.Allocator) !void {
        const version: u32 = 5;
        try out.writeInt(u32, version, ENDIANNESS);

        try out.writeStructEndian(workspace.main_area.get().local_point, ENDIANNESS);

        // TODO(design): rethink non-editable fnkboxes, probably
        const fnkboxes = try workspace.allFnkboxes(false, scratch);
        const n_saved: usize = blk: {
            var r: usize = 0;
            for (fnkboxes) |cur| {
                if (!cur.get().specific.fnkbox.editable) continue;
                r += 1;
            }
            break :blk r;
        };
        try writeLen(out, n_saved);
        for (fnkboxes) |cur| {
            if (!cur.get().specific.fnkbox.editable) continue;
            const fnkname_value = try cur.children(.fnkbox).fnkname.get().specific.sexpr.toOldCoreValue(scratch);
            const garland = if (cur.get().specific.fnkbox.execution) |e|
                e.original_garland
            else
                cur.children(.fnkbox).executor.children(.executor).garland;
            const definition = try garland.get().specific.garland.toOldCoreValue(scratch);

            const local_point_from_mainarea = workspace.main_area.get().absolute_point.inverseApplyGetLocal(cur.get().absolute_point);
            if (local_point_from_mainarea.pos.max() > 100_000) {
                panic("point too big: {any}", .{local_point_from_mainarea});
            }
            try out.writeStructEndian(local_point_from_mainarea.pos, ENDIANNESS);
            std.log.info("wrote pos: {any}", .{local_point_from_mainarea.pos});

            // write description
            try writeString(out, cur.children(.fnkbox).box.children(.fnkbox_box).description.get().specific.editable_textline.inner_text.items);

            if (true) { // write testcases
                const testcases_area = cur.children(.fnkbox).box.children(.fnkbox_box).testcases_area;
                var testcases: std.ArrayListUnmanaged(union(enum) {
                    default: Lego.Specific.Testcase.Source,
                    custom: Lego.Specific.Testcase.Children,
                }) = try .initCapacity(scratch, Toybox.childCount(testcases_area));
                defer testcases.deinit(scratch);

                var cur_testcase = testcases_area.get().tree.first;
                while (cur_testcase != nothing) : (cur_testcase = cur_testcase.get().tree.next) {
                    switch (cur_testcase.get().specific) {
                        else => unreachable,
                        .button => |button| assert(button.action == .add_testcase),
                        .testcase => |testcase| {
                            if (testcase.source) |source| {
                                if (testcase.loaded and
                                    (source.input_hash != Lego.Specific.Sexpr.hash(cur_testcase.children(.testcase).input) or
                                        source.expected_hash != Lego.Specific.Sexpr.hash(cur_testcase.children(.testcase).expected)))
                                {
                                    testcases.appendAssumeCapacity(.{ .custom = cur_testcase.children(.testcase) });
                                } else {
                                    testcases.appendAssumeCapacity(.{ .default = source });
                                }
                            } else {
                                testcases.appendAssumeCapacity(.{ .custom = cur_testcase.children(.testcase) });
                            }
                        },
                    }
                }

                try writeLen(out, testcases.items.len);
                for (testcases.items) |testcase| {
                    switch (testcase) {
                        .default => |source| {
                            try writeEnum(out, SavedTestcaseTag, .default, ENDIANNESS);
                            try out.writeInt(u64, @intCast(source.level), ENDIANNESS);
                            try out.writeInt(u64, @intCast(source.sample), ENDIANNESS);
                        },
                        .custom => |children| {
                            try writeEnum(out, SavedTestcaseTag, .custom, ENDIANNESS);
                            const input_sexpr = try Lego.Specific.Sexpr.toOldCoreValue(&children.input.get().specific.sexpr, scratch);
                            const expected_sexpr = try Lego.Specific.Sexpr.toOldCoreValue(&children.expected.get().specific.sexpr, scratch);
                            try writeFmt(out, scratch, "{any}\n", .{input_sexpr});
                            try writeFmt(out, scratch, "{any}\n", .{expected_sexpr});
                        },
                    }
                }
            }

            const fnk = core.Fnk{ .name = fnkname_value, .body = definition };
            try writeFmt(out, scratch, "{any}\n", .{fnk});
        }

        if (true) { // store which fnk was used for each scorer
            var scorers: std.ArrayListUnmanaged(struct { id: u32, fnkname: *const core.Sexpr }) = .empty;
            defer scorers.deinit(scratch);
            var lego_it = toybox.all_legos.constIterator(0);
            while (lego_it.next()) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.tag() != .scorer_row) continue;
                // don't include stuff in a blueprint
                if (workspace.isFreefloating(lego.index)) continue;
                const magic_id = lego.specific.scorer_row.magic_id;
                const fnkname = try Lego.Specific.Sexpr.toOldCoreValue(&lego.index.children(.scorer_row).fnkname.get().specific.sexpr, scratch);
                try scorers.append(scratch, .{ .id = magic_id, .fnkname = fnkname });
            }
            try writeLen(out, scorers.items.len);
            for (scorers.items) |s| {
                try out.writeInt(u32, s.id, ENDIANNESS);
                try writeFmt(out, scratch, "{any}\n", .{s.fnkname});
            }
        }
    }

    pub fn load(dst: *Workspace, in: std.io.AnyReader, scratch: std.mem.Allocator) !void {
        const version = try in.readInt(u32, .little);
        const text_magic: u32 = @bitCast(@as([4]u8, "////".*));
        const Config = struct {
            has_description: bool,
            starts_with_camera_point: bool,
            knows_n_fnkboxes: bool,
            includes_testcases: bool,
            includes_scorers: bool,
            is_text_based: bool = false,
        };
        const config: Config = switch (version) {
            0 => .{ .has_description = false, .starts_with_camera_point = false, .includes_testcases = false, .knows_n_fnkboxes = false, .includes_scorers = false },
            1 => .{ .has_description = true, .starts_with_camera_point = false, .includes_testcases = false, .knows_n_fnkboxes = false, .includes_scorers = false },
            2 => .{ .has_description = true, .starts_with_camera_point = true, .includes_testcases = false, .knows_n_fnkboxes = false, .includes_scorers = false },
            3 => .{ .has_description = true, .starts_with_camera_point = true, .includes_testcases = true, .knows_n_fnkboxes = false, .includes_scorers = false },
            4 => .{ .has_description = true, .starts_with_camera_point = true, .includes_testcases = true, .knows_n_fnkboxes = true, .includes_scorers = false },
            5 => .{ .has_description = true, .starts_with_camera_point = true, .includes_testcases = true, .knows_n_fnkboxes = true, .includes_scorers = true },
            text_magic => .{ .is_text_based = true, .has_description = false, .starts_with_camera_point = false, .includes_testcases = false, .knows_n_fnkboxes = false, .includes_scorers = false },
            else => {
                std.log.err("Unsupported file version {d}, ignoring savefile", .{version});
                return;
            },
        };

        const gpa_toybox = toybox.all_legos_arena.child_allocator;
        const gpa_dst = dst.arena_for_atom_names.child_allocator;
        dst.deinit();
        toybox.deinit();
        try toybox.init(gpa_toybox);
        try dst.init(gpa_dst, dst.random_instance.next());

        if (config.starts_with_camera_point) {
            dst.main_area.get().local_point = try in.readStructEndian(Point, ENDIANNESS);
        }

        if (config.is_text_based) {
            var input: []const u8 = try in.readAllAlloc(dst.gpa_for_atom_names, std.math.maxInt(usize));
            var x: f32 = 0;
            while (input.len > 0) {
                defer x += 40;
                var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);
                defer pool.deinit();
                const fnk = try core.parsing.parseFnk(&input, &pool, scratch);
                const pos: Vec2 = .new(x, 20);
                const description: []const u8 = try std.fmt.allocPrint(scratch, "{any}", .{fnk.name});

                const garland = try Lego.Specific.Garland.buildFromOldCoreValueV0(.{}, fnk.body, scratch, .new(@src()));
                const fnkbox = try Toybox.buildFnkbox(
                    .{ .pos = pos },
                    try Lego.Specific.Sexpr.buildFromOldCoreValue(.{}, fnk.name, true, true, .new(@src())),
                    true,
                    description,
                    &.{},
                    garland,
                );
                Toybox.addChildLast(dst.main_area, fnkbox);
            }
        } else {
            const n_fnkboxes: usize = if (config.knows_n_fnkboxes) try readLen(in) else std.math.maxInt(usize);
            for (0..n_fnkboxes) |_| {
                const pos = if (config.knows_n_fnkboxes)
                    try in.readStructEndian(Vec2, ENDIANNESS)
                else
                    in.readStructEndian(Vec2, ENDIANNESS) catch |err| switch (err) {
                        error.EndOfStream => break,
                        else => return err,
                    };

                if (std.math.isNan(pos.x) or std.math.isNan(pos.y)) {
                    return error.BadSaveFile;
                }

                std.log.info("readed pos: {any}", .{pos});

                const description_raw: []const u8 = if (config.has_description)
                    try readString(in, scratch)
                else
                    "Custom Machine";

                const description = if (std.unicode.utf8ValidateSlice(description_raw)) description_raw else "Custom Machine";

                var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);

                var testcases: std.ArrayListUnmanaged(Lego.Index) = .empty;
                if (config.includes_testcases) {
                    const n_testcases = try readLen(in);
                    try testcases.ensureUnusedCapacity(scratch, n_testcases);
                    for (0..n_testcases) |_| {
                        const tag = try readEnum(in, SavedTestcaseTag, ENDIANNESS);
                        defer _ = pool.reset(.retain_capacity);
                        switch (tag) {
                            .default => {
                                const level_index: usize = @intCast(try in.readInt(u64, ENDIANNESS));
                                const sample_index: usize = @intCast(try in.readInt(u64, ENDIANNESS));
                                // 'orelse' only engaged if tests were deleted between versions
                                const sample = (try levels[level_index].generate_sample(sample_index, &pool, scratch, dst.gpa_for_atom_names)) orelse continue;

                                testcases.appendAssumeCapacity(try Toybox.buildTestcase(.{ .unloaded = .build(
                                    level_index,
                                    sample_index,
                                    sample,
                                ) }, .new(@src())));
                            },
                            .custom => {
                                const input_ascii = try readString(in, dst.arena_for_atom_names.allocator());
                                const expected_ascii = try readString(in, dst.arena_for_atom_names.allocator());
                                const input = try core.parsing.parseSingleSexpr(input_ascii, &pool);
                                const expected = try core.parsing.parseSingleSexpr(expected_ascii, &pool);

                                testcases.appendAssumeCapacity(try Toybox.buildTestcase(.{ .existing = .{
                                    .input = try Lego.Specific.Sexpr.buildFromOldCoreValue(.{}, input, false, false, .new(@src())),
                                    .expected = try Lego.Specific.Sexpr.buildFromOldCoreValue(.{}, expected, false, false, .new(@src())),
                                    .unloaded = null,
                                } }, .new(@src())));
                            },
                        }
                    }
                }

                const ascii = try readString(in, dst.arena_for_atom_names.allocator());
                const fnk = try core.parsing.parseSingleFnk(ascii, &pool, scratch);

                const garland = try Lego.Specific.Garland.buildFromOldCoreValueV0(.{}, fnk.body, scratch, .new(@src()));
                const fnkbox = try Toybox.buildFnkbox(
                    .{ .pos = pos },
                    try Lego.Specific.Sexpr.buildFromOldCoreValue(.{}, fnk.name, true, true, .new(@src())),
                    true,
                    description,
                    testcases.items,
                    garland,
                );
                Toybox.addChildLast(dst.main_area, fnkbox);
            }

            if (config.includes_scorers) {
                var fnkname_from_scorer_row_magic_id: std.AutoHashMapUnmanaged(u32, *const core.Sexpr) = .empty;
                var pool: std.heap.MemoryPool(core.Sexpr) = .init(scratch);
                defer pool.deinit();

                const n_scorer_rows = try readLen(in);
                try fnkname_from_scorer_row_magic_id.ensureUnusedCapacity(scratch, @intCast(n_scorer_rows));
                for (0..n_scorer_rows) |_| {
                    const scorer_row_magic_id = try in.readInt(u32, ENDIANNESS);
                    const fnkbox_name = try readString(in, dst.arena_for_atom_names.allocator());
                    const fnkbox_value = try core.parsing.parseSingleSexpr(fnkbox_name, &pool);
                    fnkname_from_scorer_row_magic_id.putAssumeCapacityNoClobber(scorer_row_magic_id, fnkbox_value);
                }

                var lego_it = toybox.all_legos.constIterator(0);
                while (lego_it.next()) |lego| {
                    if (!lego.exists) continue;
                    if (lego.specific.tag() != .scorer_row) continue;
                    const fnkname = fnkname_from_scorer_row_magic_id.get(lego.specific.scorer_row.magic_id) orelse continue;
                    const old_fnkname_element = lego.index.children(.scorer_row).fnkname;
                    assert(old_fnkname_element.hasTag(.sexpr) and old_fnkname_element.get().specific.sexpr.kind == .empty);
                    const new_fnkname_element = try Lego.Specific.Sexpr.buildFromOldCoreValue(
                        old_fnkname_element.get().local_point,
                        fnkname,
                        old_fnkname_element.get().specific.sexpr.is_pattern,
                        old_fnkname_element.get().specific.sexpr.is_fnkname,

                        .new(@src()),
                    );
                    Toybox.changeChild(old_fnkname_element, new_fnkname_element);
                    Toybox.destroyFloating(old_fnkname_element);
                }
            }
        }

        try updateNonInteractive(
            dst,
            Rect
                .fromCenterAndSize(.zero, .both(2))
                .withAspectRatio(stuff.metadata.desired_aspect_ratio, .grow, .center),
            0,
            .{ .over_background = dst.main_area },
            null,
            scratch,
        );

        assert(try dst.valid(scratch));
    }

    pub fn canAutosaveNow(workspace: *const Workspace) bool {
        if (workspace.grabbing.index != nothing) return false;
        // find any active animations at fnkboxes/executors
        var lego_it = toybox.all_legos.iterator(0);
        while (lego_it.next()) |lego| {
            if (!lego.exists) continue;
            if (lego.specific.as(.fnkbox)) |fnkbox| {
                if (fnkbox.execution != null) return false;
            }
            if (lego.specific.as(.executor)) |executor| {
                if (executor.animation != null) return false;
            }
        }
        return true;
    }

    pub fn debugLogState() void {
        var alive_count: usize = 0;
        std.log.debug("free head: {d}", .{toybox.free_head.asU32()});
        var lego_it = toybox.all_legos.constIterator(0);
        var k: usize = 0;
        while (lego_it.next()) |lego| {
            defer k += 1;
            assert(lego.index.index == k);
            if (lego.exists) {
                alive_count += 1;
                std.log.debug("{d} \t{s} \tparent: {d} \tnext: {d} \tprev: {d} \tfirst: {d}\tfree next: {d}", .{
                    k,
                    @tagName(lego.specific.tag()),
                    lego.tree.parent.asU32(),
                    lego.tree.next.asU32(),
                    lego.tree.prev.asU32(),
                    lego.tree.first.asU32(),
                    lego.free_next.asU32(),
                });
            } else {
                std.log.debug("{d} \tdead \tfree next: {d}", .{
                    k,
                    lego.free_next.asU32(),
                });
            }
        }
        // std.log.debug("-----", .{});
        // for (toybox.undo_stack.commands.items, 0..) |cmd, k2| {
        //     std.log.debug("{d} \t{any}", .{ k2, cmd });
        // }
        // std.log.debug("-----", .{});
        // std.log.debug("{d} alive legos, {d} undo stack len", .{ alive_count, toybox.undo_stack.commands.items.len });
        // std.log.debug("-----", .{});
    }

    pub fn getAllFnks(workspace: *Workspace, scratch: std.mem.Allocator) !std.meta.Tuple(&.{ core.FnkCollection, u32 }) {
        const fnkboxes = try workspace.allFnkboxes(false, scratch);

        var hasher = std.hash.Wyhash.init(0);
        var all_fnks: core.FnkCollection = .init(scratch);
        for (fnkboxes) |cur| {
            const fnkbox = &Toybox.get(cur).specific.fnkbox;
            const fnkname_value = try Toybox.get(cur.children(.fnkbox).fnkname).specific.sexpr.toOldCoreValue(scratch);
            const garland = if (fnkbox.execution) |e|
                e.original_garland
            else
                fnkbox.executor().garland().index;
            const definition = try garland.get().specific.garland.toOldCoreValue(scratch);
            try all_fnks.putNoClobber(fnkname_value, definition);

            std.hash.autoHash(&hasher, struct {
                fnkname_hash: u32,
                fnkbody_hash: u32,
            }{ .fnkname_hash = fnkname_value.hash(), .fnkbody_hash = definition.hash() });
        }

        return .{ all_fnks, @truncate(hasher.final()) };
    }

    pub fn isFnknameTaken(workspace: *Workspace, fnkname: Lego.Index) bool {
        var lego_it = toybox.all_legos.iterator(0);
        while (lego_it.next()) |lego| {
            if (!lego.exists) continue;
            if (lego.specific.tag() != .fnkbox) continue;
            if (workspace.isFreefloating(lego.index)) continue;
            const existing = lego.index.children(.fnkbox).fnkname;
            if (Lego.Specific.Sexpr.equalValue(fnkname, existing)) return true;
        } else return false;
    }

    pub fn findFnkname(workspace: *Workspace, point: Point, is_pattern: bool, suggestion: ?[]const u8) !Lego.Index {
        const fnkname = try Toybox.buildSexpr(point, .{ .atom_lit = suggestion orelse "" }, is_pattern, true, .new(@src()));
        if (suggestion != null and !isFnknameTaken(workspace, fnkname)) return fnkname;

        const new_name = try workspace.arena_for_atom_names.allocator().alloc(u8, 8);
        math.Random.init(workspace.random_instance.random()).alphanumeric_bytes(new_name);
        fnkname.get().specific.sexpr.atom_name = new_name;

        while (isFnknameTaken(workspace, fnkname)) {
            math.Random.init(workspace.random_instance.random()).alphanumeric_bytes(new_name);
        }

        return fnkname;
    }

    pub fn isFreefloating(workspace: *const Workspace, index: Lego.Index) bool {
        if (INCLUDE_DEBUG_FIELDS) {
            for (workspace.roots(.all).constSlice()) |root| {
                assert(root == nothing or root.get().tree.parent == nothing);
            }
        }
        const root = Toybox.oldestAncestor(index);
        for (workspace.roots(.all).constSlice()) |r| {
            if (r == root) return false;
        } else return true;
    }
};

test fuzzyFilter {
    try std.testing.expect(fuzzyFilter("foo", "foobar"));
    try std.testing.expect(!fuzzyFilter("foo", "xoobar"));
}

fn fuzzyFilter(query: []const u8, haystack: []const u8) bool {
    // TODO(polish): support unicode, main obstacle is not having a .tolowercase there
    var remaining_description = haystack;
    var remaining_filter = query;

    while (remaining_filter.len > 0) {
        const first = std.ascii.toLower(remaining_filter[0]);
        remaining_filter = remaining_filter[1..];

        while (remaining_description.len > 0) {
            const x = std.ascii.toLower(remaining_description[0]);
            remaining_description = remaining_description[1..];
            if (x == first) {
                break;
            }
        } else return false;
    }
    return true;
}

const Menu = struct {
    showing: bool = true,
    // showing_t: f32 = 1,

    save_slot: i32 = 0,

    buttons: [3]Button = .{
        .{ .rect = .fromCenterAndSize(.zero, .one), .action = .play },
        .{ .rect = .zero, .action = .prev_save_slot },
        .{ .rect = .zero, .action = .next_save_slot },
    },

    const Button = struct {
        hovered_t: f32 = 0,
        rect: Rect,
        action: Action,

        const Action = enum {
            play,
            next_save_slot,
            prev_save_slot,
        };
    };

    pub fn update(menu: *Menu, platform: PlatformGives, maybe_drawer: ?*Drawer, scratch: std.mem.Allocator) !void {
        _ = scratch;

        const camera = Rect
            .fromCenterAndSize(.new(0, -0.25), .both(2))
            .withAspectRatio(platform.aspect_ratio, .grow, .center);

        const mouse = platform.getMouse(camera);
        const delta_seconds = platform.delta_seconds;

        const slot_rect: Rect = camera.plusMargin(-0.1).withSize(.new(0.9, 0.2), .bottom_left);
        menu.buttons[1].rect = slot_rect.withAspectRatio(1, .shrink, .top_left);
        menu.buttons[2].rect = slot_rect.withAspectRatio(1, .shrink, .top_right);

        const hovered: ?usize = for (menu.buttons, 0..) |button, k| {
            if (button.rect.contains(mouse.cur.position)) break k;
        } else null;
        for (&menu.buttons, 0..) |*button, k| {
            math.lerpTowards(&button.hovered_t, if (k == hovered) 1 else 0, .slow, delta_seconds);
        }
        const maybe_action: ?Button.Action = if (mouse.wasPressed(.left))
            if (hovered) |h| menu.buttons[h].action else null
        else
            null;

        if (maybe_action) |action| switch (action) {
            .play => menu.showing = false,
            .prev_save_slot => menu.save_slot = @mod(menu.save_slot - 1, 100),
            .next_save_slot => menu.save_slot = @mod(menu.save_slot + 1, 100),
        };

        if (maybe_drawer) |drawer| {
            try drawer.canvas.drawText(0, camera, try std.fmt.allocPrint(
                drawer.canvas.frame_arena.allocator(),
                "save {d}",
                .{menu.save_slot},
            ), .centeredAt(slot_rect.getCenter().addY(-0.01)), 0.125, .black);

            for (menu.buttons) |button| {
                drawer.canvas.borderRect(camera, button.rect.plusMargin(button.hovered_t * 0.1 * button.rect.size.y), switch (button.action) {
                    .play => 0.05,
                    .prev_save_slot,
                    .next_save_slot,
                    => 0.01,
                }, .inner, .black);
                try drawer.canvas.drawText(0, camera, switch (button.action) {
                    .play => "play",
                    .prev_save_slot => "<",
                    .next_save_slot => ">",
                }, .centeredAt(button.rect.getCenter()), switch (button.action) {
                    .play => 0.25,
                    .prev_save_slot,
                    .next_save_slot,
                    => 0.15,
                }, .black);
            }

            try drawer.canvas.drawText(0, camera, "Vaulogy", .centeredAt(.new(0, -0.85)), 0.4, .black);
        }
    }
};

pub fn init(
    dst: *GameState,
    runtime_params: kommon.engine.InitRuntimeParamsFor(GameState),
    comptime comptime_params: kommon.engine.InitComptimeParamsFor(GameState),
) !void {
    const gpa = runtime_params.gpa;
    const gl = runtime_params.gl;
    const loaded_images = runtime_params.loaded_images;
    const random_seed = runtime_params.random_seed;
    const tweakable = comptime_params.tweakable;
    _ = tweakable;

    dst.* = kommon.meta.initDefaultFields(GameState);
    try dst.toybox_instance.init(gpa);
    toybox = &dst.toybox_instance;

    dst.usual.init(
        gpa,
        random_seed,
        try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}),
    );

    // tweakable.fcolor("bg", &COLORS.bg);

    dst.drawer = try .init(&dst.usual, loaded_images.get(.atom_testing));
    try dst.workspace.init(gpa, random_seed);

    if (false) {
        var player: FuzzerContext.Player = try .init(gpa, 0);
        defer player.deinit();

        const inputs = @import("buggy_recording.zig").inputs;
        for (inputs, 0..) |input, turn_index| {
            std.log.debug("--- Turn {d}, before ---", .{turn_index});
            Workspace.debugLogState();
            std.log.debug("Applying input: left {any}, right {any}, z {any}", .{ input.mouse_left_down, input.mouse_right_down, input.z_down });
            try player.advance(input);
            std.log.debug("--- Turn {d}, after ---", .{turn_index});
            Workspace.debugLogState();
        }
    }
}

// TODO(platform): take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.usual.deinit(undefined);
    self.workspace.deinit();
    _ = gpa;
    toybox.deinit();
}

pub fn beforeHotReload(self: *GameState) !void {
    self.backup_point = self.workspace.main_area.get().local_point;
}

pub fn afterHotReload(self: *GameState) !void {
    try Drawer.AtomVisuals.Geometry.initFixed(self.usual.mem.forever.allocator(), self.usual.canvas.gl);
    self.drawer.atom_visuals_cache = try .init(self.usual.mem.forever.allocator(), self.usual.canvas.gl);
    toybox = &self.toybox_instance;
    try self.workspace.init(self.usual.mem.gpa, 0);
    if (self.backup_point) |p| self.workspace.main_area.get().local_point = p;
}

var first_frame_done = false;
var seconds_since_last_save: f32 = 0;

fn getSave(platform: PlatformGives, slot: i32) ?std.io.AnyReader {
    var buf: [128]u8 = undefined;
    const name = std.fmt.bufPrint(&buf, "vaulogy_save_{d}", .{slot}) catch unreachable;
    return platform.getItem(name);
}

fn setSave(platform: PlatformGives, slot: i32, data: []const u8) void {
    var buf: [128]u8 = undefined;
    const name = std.fmt.bufPrint(&buf, "vaulogy_save_{d}", .{slot}) catch unreachable;
    return platform.setItem(name, data);
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);

    if (!first_frame_done) {
        first_frame_done = true;
        if (SAVING_ENABLED) {
            if (getSave(platform, self.menu.save_slot)) |reader| {
                // TODO(platform): debug why we can't directly use reader
                // try self.workspace.load(reader, &self.core_mem);

                std.log.debug("got reader: {any}", .{reader});
                const data = try reader.readAllAlloc(self.usual.mem.frame.allocator(), std.math.maxInt(usize));
                std.log.debug("data len: {d}", .{data.len});
                var fbs = std.io.fixedBufferStream(data);
                try self.workspace.load(fbs.reader().any(), self.usual.mem.frame.allocator());
            } else if (false) {
                var fbs = std.io.fixedBufferStream(@embedFile("solutions.txt"));
                try self.workspace.load(fbs.reader().any(), self.usual.mem.frame.allocator());
            }
        }
    }

    if (false and platform.keyboard.wasPressed(.KeyQ)) {
        var asdf: std.ArrayList(u8) = .init(platform.gpa);
        defer asdf.deinit();
        try self.workspace.save(asdf.writer().any(), self.usual.mem.frame.allocator());
        // std.log.debug("save size in bytes: {d}", .{asdf.items.len});
        // std.log.debug("{s}", .{asdf.items});
        var fbs = std.io.fixedBufferStream(asdf.items);
        try self.workspace.load(fbs.reader().any(), self.usual.mem.frame.allocator());
    }

    if (SAVING_ENABLED and seconds_since_last_save > 30 and self.workspace.canAutosaveNow()) {
        var asdf: std.ArrayList(u8) = .init(self.usual.mem.frame.allocator());
        defer asdf.deinit();
        try self.workspace.save(asdf.writer().any(), self.usual.mem.frame.allocator());
        setSave(platform, self.menu.save_slot, asdf.items);
        seconds_since_last_save = 0;
        std.log.debug("autosaved", .{});
    }
    seconds_since_last_save += platform.delta_seconds;

    if (platform.keyboard.wasPressed(.Escape)) {
        self.menu.showing = !self.menu.showing;
        if (self.menu.showing and self.workspace.canAutosaveNow()) {
            var asdf: std.ArrayList(u8) = .init(self.usual.mem.frame.allocator());
            defer asdf.deinit();
            try self.workspace.save(asdf.writer().any(), self.usual.mem.frame.allocator());
            setSave(platform, self.menu.save_slot, asdf.items);
            seconds_since_last_save = 0;
        }
    }

    platform.gl.clear(COLORS.bg);
    if (self.menu.showing) {
        const old_save_slot = self.menu.save_slot;
        try self.menu.update(platform, &self.drawer, self.usual.mem.frame.allocator());
        const new_save_slot = self.menu.save_slot;
        if (new_save_slot != old_save_slot) {
            if (self.workspace.canAutosaveNow()) {
                var asdf: std.ArrayList(u8) = .init(self.usual.mem.frame.allocator());
                defer asdf.deinit();
                try self.workspace.save(asdf.writer().any(), self.usual.mem.frame.allocator());
                setSave(platform, old_save_slot, asdf.items);
                seconds_since_last_save = 0;
            } else std.log.err("TODO: handle saving better", .{});
            seconds_since_last_save = 0;

            if (getSave(platform, new_save_slot)) |reader| {
                const data = try reader.readAllAlloc(self.usual.mem.frame.allocator(), std.math.maxInt(usize));
                var fbs = std.io.fixedBufferStream(data);
                try self.workspace.load(fbs.reader().any(), self.usual.mem.frame.allocator());
            } else {
                const dst = &self.workspace;
                dst.deinit();
                toybox.deinit();
                try toybox.init(toybox.all_legos_arena.child_allocator);
                try dst.init(dst.arena_for_atom_names.child_allocator, dst.random_instance.next());
            }
        }
    } else {
        try self.workspace.update(platform, &self.drawer, self.usual.mem.frame.allocator());
    }

    return false;
}

pub const ENDIANNESS: std.builtin.Endian = .little;
comptime {
    assert(@import("builtin").target.cpu.arch.endian() == ENDIANNESS);
}

pub fn writeEnum(out: std.io.AnyWriter, T: type, value: T, endian: std.builtin.Endian) !void {
    const type_info = @typeInfo(T).@"enum";
    try out.writeInt(type_info.tag_type, @intFromEnum(value), endian);
}

pub fn readEnum(in: std.io.AnyReader, T: type, endian: std.builtin.Endian) !T {
    const type_info = @typeInfo(T).@"enum";
    return @enumFromInt(try in.readInt(type_info.tag_type, endian));
}

pub fn writeF32(out: std.io.AnyWriter, value: f32) !void {
    comptime assert(@import("builtin").target.cpu.arch.endian() == .little);
    try out.writeAll(std.mem.asBytes(&value));
}

pub fn readF32(in: std.io.AnyReader) !f32 {
    comptime assert(@import("builtin").target.cpu.arch.endian() == .little);
    return @bitCast(try in.readInt(u32, .little));
}

pub fn writeBool(out: std.io.AnyWriter, value: bool) !void {
    try out.writeByte(if (value) 0xFF else 0x00);
}

pub fn readBool(in: std.io.AnyReader) !bool {
    return switch (try in.readByte()) {
        0x00 => false,
        0xFF => true,
        else => @panic("bad bool"),
    };
}

pub fn writeLen(out: std.io.AnyWriter, len: usize) !void {
    try out.writeInt(u32, @intCast(len), ENDIANNESS);
}

pub fn readLen(in: std.io.AnyReader) !usize {
    return @intCast(try in.readInt(u32, ENDIANNESS));
}

pub fn writeString(out: std.io.AnyWriter, value: []const u8) !void {
    try writeLen(out, value.len);
    try out.writeAll(value);
}

pub fn writeFmt(out: std.io.AnyWriter, scratch: std.mem.Allocator, comptime format: []const u8, args: anytype) !void {
    var tmp_out: std.ArrayList(u8) = .init(scratch);
    defer tmp_out.deinit();
    try tmp_out.writer().print(format, args);
    try writeString(out, tmp_out.items);
}

pub fn readString(in: std.io.AnyReader, allocator: std.mem.Allocator) ![]u8 {
    const len = try readLen(in);
    const result = try allocator.alloc(u8, len);
    const actual_len = try in.readAll(result);
    if (actual_len != len) @panic("bad string");
    return result;
}

const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;

const core = @import("core.zig");

const hashString = std.array_hash_map.hashString;
const kommon = @import("kommon");
const Triangulator = kommon.Triangulator;
const math = kommon.math;
const tof32 = math.tof32;
const UColor = math.UColor;
const FColor = math.FColor;
const Rect = math.Rect;
const Bounds = math.Bounds;
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
pub const TextSelection = Canvas.TextSelection;
pub const TextRenderer = Canvas.TextRenderer;
pub const Mem = kommon.Mem;
pub const Key = kommon.Key;
pub const LazyState = kommon.LazyState;
pub const EdgePos = kommon.grid2D.EdgePos;
// pub const LocalDecisions = @import("../chesstory/GameState.zig").LocalDecisions;

const parsing = @import("parsing.zig");

const PhysicalSexpr = @import("physical.zig").PhysicalSexpr;
const ViewHelper = @import("physical.zig").ViewHelper;
const Sample = @import("levels_new.zig").Sample;

/// Inserts the element at the specified index, and moves the element there to the end of the list.
/// Undoes swapRemove
/// This operation is O(1).
/// Asserts that the index is in bounds.
pub fn swapInsertAssumeCapacity(T: type, array: *std.ArrayListUnmanaged(T), i: usize, element: T) void {
    assert(array.items.len < array.capacity);
    assert(i <= array.items.len);
    array.items.len += 1;
    if (array.items.len - 1 == i) {
        array.items[i] = element;
    } else {
        const old_last = array.items[i];
        array.items[i] = element;
        array.items[array.items.len - 1] = old_last;
    }
}

// TODO(design): rethink
pub const Binding = struct {
    name: []const u8,
    value: Lego.Index,
};
pub const Bindings = std.ArrayList(Binding);

pub const BindingsState = struct {
    new: []const Binding,
    old: []const Binding,
    anim_t: ?f32,
    pub const none: BindingsState = .{ .anim_t = null, .new = &.{}, .old = &.{} };
};

pub fn drawTemplateWildcardLinesNonRecursiveV2(
    drawer: *Drawer,
    camera: Rect,
    left_names_raw: [][]const u8,
    right_names_raw: [][]const u8,
    point: Point,
    bindings: BindingsState,
    alpha: f32,
) !void {
    var left_names: std.ArrayListUnmanaged([]const u8) = .fromOwnedSlice(left_names_raw);
    var right_names: std.ArrayListUnmanaged([]const u8) = .fromOwnedSlice(right_names_raw);

    if (bindings.anim_t) |anim_t| if (anim_t >= 0.4) {
        try removeBoundNamesV10(&left_names, bindings.new);
    };
    try removeBoundNamesV10(&left_names, bindings.old);

    if (bindings.anim_t) |anim_t| if (anim_t >= 0.4) {
        try removeBoundNamesV10(&right_names, bindings.new);
    };
    try removeBoundNamesV10(&right_names, bindings.old);

    {
        // TODO(game): these numbers are not exact, issues when zooming in
        try drawer.drawWildcardsCable(camera, &([1]Vec2{
            point.applyToLocalPosition(.new(-0.5, 0)),
        } ++ funk.fromCountAndCtx(32, struct {
            pub fn anon(k: usize, p: Point) Vec2 {
                return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.5 + 0.25 / 2.0, 0.75, math.tof32(k) / 32)).scale(0.75).add(.new(0.25, 0.25)));
            }
        }.anon, point)), left_names.items, alpha);

        try drawer.drawWildcardsCable(camera, &([1]Vec2{
            point.applyToLocalPosition(.new(-0.5, 0)),
        } ++ funk.fromCountAndCtx(32, struct {
            pub fn anon(k: usize, p: Point) Vec2 {
                return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.5 - 0.25 / 2.0, 0.25, math.tof32(k) / 32)).scale(0.75).add(.new(0.25, -0.25)));
            }
        }.anon, point)), right_names.items, alpha);
    }
}

fn removeBoundNamesV10(list: *std.ArrayListUnmanaged([]const u8), bindings: []const Binding) !void {
    for (bindings) |binding| {
        const name_to_remove = binding.name;
        while (funk.indexOfString(list.items, name_to_remove)) |i| {
            std.debug.assert(std.mem.eql(u8, name_to_remove, list.swapRemove(i)));
        }
    }
}

fn printNextGarlands(first: Lego.Index) void {
    std.log.debug("printing garlands", .{});
    var cur = first;
    while (cur != nothing) : (cur = Toybox.get(cur).specific.garland.next_enqueued) {
        std.log.debug("index: {d}", .{cur.asU32()});
    }
    std.log.debug("done", .{});
}

fn addHint(easy: Lego.Index, hard: Lego.Index) void {
    hard.get().specific.bubble.has_hints = true;
    var bubble = &easy.get().specific.bubble;
    for (&bubble.hint_for) |*dst| {
        if (dst.* != nothing) continue;
        dst.* = hard;
        break;
    } else unreachable;
}
