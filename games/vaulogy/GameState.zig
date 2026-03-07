pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// Causes of bugs:
// - functions that take a pointer and allocate memory might invalidate that pointer

const Drawer = @import("Drawer.zig");

pub const tracy = @import("tracy");

pub const display_fps = true;

// TODO(optim): set to true
const ENABLE_REUSE = false;

const EXECUTOR_MOVES_LEFT = true;
const SEQUENTIAL_GOES_DOWN = true;
const CRANKS_ENABLED = true;

const FuzzerContext = struct {
    var toybox_instance: Toybox = undefined;

    const TestPlatform = struct {
        global_seconds: f32 = 0,
        delta_seconds: f32 = 0,
        mouse: Mouse = .{ .cur = .init, .prev = .init, .cur_time = 0 },
        keyboard: Keyboard = .{ .cur = .init, .prev = .init, .cur_time = 0 },
        frame_arena: std.heap.ArenaAllocator = .init(std.testing.allocator),

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
                .gpa = std.testing.allocator,
                .frame_arena = self.frame_arena.allocator(),

                .aspect_ratio = stuff.metadata.desired_aspect_ratio,
                .delta_seconds = delta_seconds,
                .global_seconds = self.global_seconds,
                .gl = .stub,
                .setCursor = struct {
                    fn anon(_: Mouse.Cursor) void {}
                }.anon,

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

    const FakeInput = extern struct {
        z_down: bool,
        mouse_left_down: bool,
        mouse_pos: Vec2,
    };

    const Player = struct {
        workspace: Workspace,
        test_platform: TestPlatform,

        pub fn init() !Player {
            toybox = &FuzzerContext.toybox_instance;
            try toybox.init(std.testing.allocator);
            var workspace: Workspace = undefined;
            try workspace.init(std.testing.allocator, std.testing.random_seed);
            return .{ .workspace = workspace, .test_platform = .{} };
        }

        pub fn deinit(player: *Player) void {
            player.workspace.deinit();
            player.test_platform.frame_arena.deinit();
            toybox.deinit();
        }

        pub fn advance(player: *Player, input: FakeInput) !void {
            player.test_platform.keyboard.cur.keys.KeyZ = input.z_down;
            player.test_platform.mouse.cur.buttons.left = input.mouse_left_down;
            player.test_platform.mouse.cur.position = input.mouse_pos;
            try player.workspace.update(player.test_platform.getGives(1.0 / 60.0), null, player.test_platform.frame_arena.allocator());
            player.test_platform.after();
        }
    };

    fn testOne(_: @This(), input: []const u8) anyerror!void {
        var player: Player = try .init();
        defer player.deinit();

        var it = std.mem.window(u8, input, @sizeOf(FakeInput), @sizeOf(FakeInput));
        while (it.next()) |cur_input_raw| {
            if (cur_input_raw.len == @sizeOf(FakeInput)) {
                const cur_input = std.mem.bytesToValue(FakeInput, cur_input_raw);
                try player.advance(cur_input);
            }
        }
    }
};

test "fuzz example" {
    try std.testing.fuzz(FuzzerContext{}, FuzzerContext.testOne, .{});
}

test "custom replay" {
    var player: FuzzerContext.Player = try .init();
    defer player.deinit();

    const inputs: []const FuzzerContext.FakeInput = &.{
        .{ .z_down = false, .mouse_left_down = false, .mouse_pos = .new(0, 0) },
    };
    for (inputs) |input| try player.advance(input);
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
    const drawer: Drawer = try .init(&usual);
    _ = drawer;
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

var toybox: *Toybox = undefined;

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
    unhoverable: bool = false,

    tree: Tree = .empty,

    specific: Specific,

    pub const Specific = union(enum) {
        button: Button,
        scrollbar: Scrollbar,
        area: Area,
        sexpr: Sexpr,
        case: Case,
        garland: Garland,
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

        // TODO(design): try to simplify these
        garland_newcases: void,
        fnkbox_description: struct {
            text: []const u8,
        },
        fnkbox_testcases: struct {
            scrollbar: Lego.Index,
        },
        fnkslist: struct {
            scrollbar: Lego.Index,
        },
        fnkslist_element: FnkslistElement,
        postit_text: struct {
            text: []const u8,
        },
        postit_drawing: enum {
            arrow,
            launch_testcase_button,
            piece_center,
        },
        executor_controls: struct {
            pub fn brake(executor_controls: *const @This()) Lego.Index {
                return Toybox.getChildrenExact(2, Lego.fromSpecificConst(.executor_controls, executor_controls).index)[0];
            }

            pub fn crank(executor_controls: *const @This()) Lego.Index {
                return Toybox.getChildrenExact(2, Lego.fromSpecificConst(.executor_controls, executor_controls).index)[1];
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

        pub const FnkslistElement = struct {
            text: []const u8,

            pub const height: f32 = 2.0;

            const core = @import("core.zig");
            pub fn build(count: usize, fnkname: *const core.Sexpr, text: []const u8, undo_stack: ?*UndoStack) !Lego.Index {
                return try Toybox.createWithChildren(.{ .pos = .new(0, tof32(count) * height) }, .{
                    .fnkslist_element = .{ .text = text },
                }, &.{
                    try Sexpr.buildFromOldCoreValue(.{ .pos = .new(1.5, 0.65), .scale = 0.5, .turns = 0.25 }, fnkname, false, true, undo_stack),
                }, undo_stack);
            }
        };

        pub const Button = struct {
            local_rect: Rect,
            action: enum {
                launch_testcase,
                see_failing_testcase,
                /// assumes that the scrollbar is the direct parent
                scroll_up,
                /// assumes that the scrollbar is the direct parent
                scroll_down,
            },
            enabled: bool = true,

            pub fn instant(button: Button) bool {
                return switch (button.action) {
                    .launch_testcase,
                    .see_failing_testcase,
                    .scroll_up,
                    .scroll_down,
                    => false,
                };
            }
        };

        pub const Scrollbar = struct {
            total_rect: Rect,
            total_length: f32,
            visible_length: f32,
            scroll_visual: f32,
            scroll_target: f32,

            const min_handle_length: f32 = 0.25;
            const max_handle_length: f32 = 1;

            pub fn build(bounding_rect: Rect, total_length: f32, visible_length: f32, undo_stack: ?*UndoStack) Lego.Index {
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
                    (try Toybox.new(.{}, .{ .button = .{
                        .local_rect = bounding_rect.withSize(.both(arrows_height), .top_left),
                        .action = .scroll_up,
                    } }, undo_stack)).index,
                    (try Toybox.new(.{}, .{ .button = .{
                        .local_rect = bounding_rect.withSize(.both(arrows_height), .bottom_left),
                        .action = .scroll_down,
                    } }, undo_stack)).index,
                }, undo_stack);
            }

            pub fn buildForTestcases(n_testcases: usize, scroll: f32) Scrollbar {
                const total_rect = Lego.Specific.FnkboxBox.testcases_box
                    .withSize(.new(0.5, FnkboxBox.testcases_height - 1.2), .top_left)
                    .move(.new(0.1, 0.6));
                return .{
                    .total_length = tof32(n_testcases),
                    .visible_length = FnkboxBox.visible_testcases,
                    .total_rect = total_rect,
                    .scroll_visual = scroll,
                    .scroll_target = scroll,
                };
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
            immutable: bool,

            // TODO(design): rethink
            executor_with_bindings: Lego.Index = .nothing,
            /// for patterns, this means the "eating value"
            emerging_value: Lego.Index = .nothing,
            emerging_value_t: f32 = 0,
            // TODO(design): this is very hacky
            /// set to true on patterns that have eaten their value
            emerging_value_ignore_updates_to_t: bool = false,

            pub const Kind = enum { empty, atom_lit, atom_var, pair };

            pub fn setEmergingValueT(parent: Lego.Index, t: f32) void {
                var cur_sexpr = parent;
                while (cur_sexpr != .nothing) : (cur_sexpr = Toybox.next_preordered(cur_sexpr, parent).next) {
                    Toybox.get(cur_sexpr).specific.sexpr.emerging_value_t = t;
                }
            }

            pub fn setIsPattern(parent: Lego.Index, is_pattern: bool) void {
                var cur_sexpr = parent;
                while (cur_sexpr != .nothing) : (cur_sexpr = Toybox.next_preordered(cur_sexpr, parent).next) {
                    Toybox.get(cur_sexpr).specific.sexpr.is_pattern = is_pattern;
                    var cur_child = Toybox.get(cur_sexpr).specific.sexpr.emerging_value;
                    while (cur_child != .nothing) : (cur_child = Toybox.next_preordered(cur_child, cur_sexpr).next) {
                        Toybox.get(cur_child).specific.sexpr.is_pattern = is_pattern;
                    }
                }
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
            pub fn updateLocalPositions(index: Lego.Index) void {
                const lego = Toybox.get(index);
                const sexpr = &lego.specific.sexpr;

                const base_point: Point = if (!sexpr.is_pattern)
                    .{ .turns = math.remap(sexpr.is_pattern_t, 0.5, 0, -0.25, 0) }
                else
                    .{ .turns = math.remap(sexpr.is_pattern_t, 0.5, 1, 0.25, 0) };

                const hovered_point = base_point.applyToLocalPoint(.lerp(.{}, .lerp(
                    .{ .turns = -0.01, .pos = .new(0.25, 0) },
                    .{ .turns = 0.01, .pos = .new(-0.25, 0) },
                    sexpr.is_pattern_t,
                ), lego.hot_t + lego.dropping_t * 2));

                lego.visual_offset = hovered_point;

                if (sexpr.kind == .pair) {
                    const child_up, const child_down = Toybox.getChildrenExact(2, index);
                    Toybox.get(child_up).local_point = (Point{})
                        .applyToLocalPoint(lego.visual_offset)
                        .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .up));
                    Toybox.get(child_down).local_point = (Point{})
                        .applyToLocalPoint(lego.visual_offset)
                        .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .down));
                }

                if (sexpr.emerging_value != .nothing) {
                    std.log.err("Shouldn't happen!", .{});
                    updateLocalPositions(sexpr.emerging_value);
                }
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

            fn getAllVarNames(self: *const Sexpr, res: *std.ArrayList([]const u8)) !void {
                const indexOfString = @import("kommon").funktional.indexOfString;
                switch (self.kind) {
                    .atom_lit, .empty => return,
                    .atom_var => if (indexOfString(res.items, self.atom_name) == null) try res.append(self.atom_name),
                    .pair => {
                        try self.left().getAllVarNames(res);
                        try self.right().getAllVarNames(res);
                    },
                }
            }

            pub fn getAllVarNamesHelper(pair: Lego.Index, allocator: std.mem.Allocator) !struct {
                left: [][]const u8,
                right: [][]const u8,
            } {
                var left_names: std.ArrayList([]const u8) = .init(allocator);
                var right_names: std.ArrayList([]const u8) = .init(allocator);
                const s = &Toybox.get(pair).specific.sexpr;
                try s.left().getAllVarNames(&left_names);
                try s.right().getAllVarNames(&right_names);
                return .{
                    .left = try left_names.toOwnedSlice(),
                    .right = try right_names.toOwnedSlice(),
                };
            }

            const core = @import("core.zig");
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

            pub fn buildFromOldCoreValue(point: Point, value: *const core.Sexpr, is_pattern: bool, is_fnkname: bool, undo_stack: ?*UndoStack) !Lego.Index {
                return try Toybox.buildSexpr(point, switch (value.*) {
                    .empty => .empty,
                    .atom_lit => |s| .{ .atom_lit = s.value },
                    .atom_var => |s| .{ .atom_var = s.value },
                    .pair => |pair| .{ .pair = .{
                        .up = try buildFromOldCoreValue(point.applyToLocalPoint(ViewHelper.offsetFor(false, .up)), pair.left, is_pattern, is_fnkname, undo_stack),
                        .down = try buildFromOldCoreValue(point.applyToLocalPoint(ViewHelper.offsetFor(false, .down)), pair.right, is_pattern, is_fnkname, undo_stack),
                    } },
                }, is_pattern, is_fnkname, undo_stack);
            }

            pub fn drawEatingPattern(parent: Lego.Index, var_name: []const u8, t: f32, camera: Rect, drawer: *Drawer, base_alpha: f32) !void {
                var cur = parent;
                const alpha = t * base_alpha;
                while (cur != .nothing) : (cur = Toybox.next_preordered(cur, parent).next) {
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
        };

        pub const Case = struct {
            /// offset for the next garland, used during animations
            next_point_extra: Point = .{},
            /// offset for the fnk name, used during animations
            fnk_name_extra: Point = .{},

            const fnk_name_offset: Point = .{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) };
            const next_garland_offset: Vec2 = .new(8, if (SEQUENTIAL_GOES_DOWN) 1 else -1.5);

            const Children = struct {
                pattern: Lego.Index,
                template: Lego.Index,
                fnkname: Lego.Index,
                next: Lego.Index,
            };

            pub fn destroyForParts(index: Lego.Index, undo_stack: *UndoStack) Children {
                const result = children(index);
                Toybox.popWithUndoAndChangingCoords(result.pattern, undo_stack);
                Toybox.popWithUndoAndChangingCoords(result.template, undo_stack);
                Toybox.popWithUndoAndChangingCoords(result.fnkname, undo_stack);
                Toybox.popWithUndoAndChangingCoords(result.next, undo_stack);

                if (index.get().tree.parent != .nothing) {
                    Toybox.popWithUndo(index, undo_stack);
                }
                Toybox.destroyFloatingWithUndo(index, undo_stack);

                return result;
            }

            pub fn children(index: Lego.Index) Children {
                assert(Toybox.get(index).specific.tag() == .case);
                const asdf = Toybox.getChildrenExact(4, index);
                return .{
                    .pattern = asdf[0],
                    .template = asdf[1],
                    .fnkname = asdf[2],
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
                    (Point{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) }).applyToLocalPoint(case.fnk_name_extra),
                    (Point{ .pos = .new(8, 1) }).applyToLocalPoint(case.next_point_extra),
                };
                for (Toybox.getChildrenExact(4, index), offsets) |i, offset| {
                    Toybox.get(i).local_point = offset;
                }
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

            pub fn stealFnkname(garland: Lego.Index, replacement: ?Lego.Index, undo_stack: ?*UndoStack) !Lego.Index {
                const original_fnkname = Lego.Specific.Garland.children(garland).fnkname;
                if (replacement) |r| {
                    r.get().local_point = Lego.Specific.Garland.relative_fnkname_point;
                    r.get().specific.sexpr.is_pattern = true;
                    Toybox.changeChild(original_fnkname, r, undo_stack);
                } else {
                    const new_fnkname = try Toybox.buildSexpr(undefined, .empty, true, true, undo_stack);
                    Toybox.changeChild(original_fnkname, new_fnkname, undo_stack);
                }
                return original_fnkname;
            }

            pub fn popCase(case: Lego.Index, undo_stack: ?*UndoStack) void {
                Toybox.refreshAbsolutePoints(&.{case});

                assert(case.hasTag(.case));
                const parent = case.get().tree.parent;
                assert(parent.hasTag(.newcase));

                Toybox.popWithUndoAndChangingCoords(case, undo_stack);
                const original_parent_tree = Toybox.get(parent).tree;
                const l_a = Toybox.get(original_parent_tree.next).specific.newcase.length();
                const l_b = Toybox.get(parent).specific.newcase.length();
                Toybox.get(original_parent_tree.next).specific.newcase.length_before = l_b;
                Toybox.get(original_parent_tree.next).specific.newcase.length_after = l_a;
                Toybox.get(original_parent_tree.next).dropzone_t = case.get().hot_t;
                Toybox.get(original_parent_tree.next).local_point = parent.get().local_point;
                Toybox.pop(parent, undo_stack);
                Toybox.destroyFloating(parent, undo_stack);

                Toybox.refreshAbsolutePoints(&.{ case, original_parent_tree.next });
            }

            pub fn children(index: Lego.Index) struct {
                fnkname: Lego.Index,
                cases: Lego.Index,
            } {
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

            const core = @import("core.zig");
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
                        .fnk_name = try c.fnkname.get().specific.sexpr.toOldCoreValue(allocator),
                        .template = try c.template.get().specific.sexpr.toOldCoreValue(allocator),
                        .next = next,
                    });
                }
                return .{ .cases = cases };
            }

            pub fn buildFromOldCoreValue(point: Point, definition: core.FnkBodyV2, scratch: std.mem.Allocator, undo_stack: ?*UndoStack) !Lego.Index {
                var cases: std.ArrayListUnmanaged(Lego.Index) = try .initCapacity(scratch, definition.cases.len);
                for (definition.cases) |case| {
                    cases.appendAssumeCapacity(try Toybox.buildCase(.{}, .{
                        .pattern = try Sexpr.buildFromOldCoreValue(.{}, case.pattern, true, false, undo_stack),
                        .template = try Sexpr.buildFromOldCoreValue(.{}, case.template, false, false, undo_stack),
                        .fnkname = try Sexpr.buildFromOldCoreValue(.{}, case.fnk_name, false, true, undo_stack),
                        .next = if (case.next) |next|
                            try buildFromOldCoreValue(.{}, .{ .cases = next }, scratch, undo_stack)
                        else
                            null,
                    }, undo_stack));
                }
                return try Toybox.buildGarland(point, try cases.toOwnedSlice(scratch), undo_stack);
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
                    fnkname_call: Lego.Index,
                    fnkname_response: Lego.Index,
                    // TODO(game)
                    // bindings: []const Binding,
                },
                undo_stack: ?*UndoStack,
            ) !Lego.Index {
                const result = try Toybox.new(pattern_point, .{
                    .pill = .{ .next_pill = old_first },
                }, undo_stack);

                Toybox.addChildLastWithoutChangingAbsPoint(result.index, data.pattern, undo_stack);
                Toybox.addChildLastWithoutChangingAbsPoint(result.index, data.input, undo_stack);
                Toybox.addChildLastWithoutChangingAbsPoint(result.index, data.fnkname_call, undo_stack);
                Toybox.addChildLastWithoutChangingAbsPoint(result.index, data.fnkname_response, undo_stack);

                return result.index;
            }
        };

        pub const Executor = struct {
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

            pub fn children(index: Lego.Index) struct {
                input: Lego.Index,
                garland: Lego.Index,
                controls: Lego.Index,
            } {
                assert(Toybox.get(index).specific.tag() == .executor);
                const asdf = Toybox.getChildrenExact(3, index);
                return .{
                    .input = asdf[0],
                    .garland = asdf[1],
                    .controls = asdf[2],
                };
            }

            pub fn getBrakeT(executor_index: Lego.Index) f32 {
                return children(executor_index).controls.get().specific.executor_controls.brake().get().specific.executor_brake.brake_t;
            }

            pub fn shouldStartExecution(executor_index: Lego.Index) bool {
                const executor = Toybox.get(executor_index).specific.executor;
                const garland = children(executor_index).garland;
                const input = children(executor_index).input;
                return executor.animation == null and
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

            pub fn children(index: Lego.Index) struct {
                description: Lego.Index,
                status_bar: Lego.Index,
                testcases_scrollbar: Lego.Index,
                testcases_area: Lego.Index,
            } {
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

            const relative_fnkname_point: Point = .{ .pos = .new(-1, 1), .scale = 0.5, .turns = 0.25 };
            const relative_executor_point: Point = .{ .pos = .new(-3, 1 + FnkboxBox.box_height) };

            pub const Status = union(enum) {
                /// the failing testcase
                unsolved: Lego.Index,
                // TODO(game): score
                solved,
            };

            pub fn fnkname(fnkbox: *const Fnkbox) Lego.Index {
                return children(Lego.fromSpecificConst(.fnkbox, fnkbox).index).fnkname;
            }

            pub fn executor(fnkbox: *const Fnkbox) ApiFor.Executor {
                return .{ .index = children(Lego.fromSpecificConst(.fnkbox, fnkbox).index).executor };
            }

            pub fn children(index: Lego.Index) struct {
                box: Lego.Index,
                fnkname: Lego.Index,
                executor: Lego.Index,
            } {
                assert(Toybox.get(index).specific.tag() == .fnkbox);
                const asdf = Toybox.getChildrenExact(3, index);
                return .{
                    .box = asdf[0],
                    .fnkname = asdf[1],
                    .executor = asdf[2],
                };
            }

            pub fn updateStatus(fnkbox: *Fnkbox, worspace: *Workspace, scratch: std.mem.Allocator) !void {
                // TODO(optim): improve somehow
                const core = @import("core.zig");
                const fnkbox_index = Lego.fromSpecificConst(.fnkbox, fnkbox).index;
                var all_fnks: core.FnkCollection = .init(scratch);
                if (true) {
                    var cur = Toybox.get(worspace.fnkboxes_layer).tree.first;
                    while (cur != .nothing) : (cur = Toybox.get(cur).tree.next) {
                        // const fnkbox = &Toybox.get(cur).specific.fnkbox;
                        const fnkname_value = try Toybox.get(children(cur).fnkname).specific.sexpr.toOldCoreValue(scratch);
                        const definition = try Toybox.get(Executor.children(children(cur).executor).garland).specific.garland.toOldCoreValue(scratch);
                        try all_fnks.putNoClobber(fnkname_value, definition);
                    }
                }
                const fnkname_value = try Toybox.get(children(fnkbox_index).fnkname).specific.sexpr.toOldCoreValue(scratch);
                var temp_mem: core.VeryPermamentGameStuff = .init(scratch);
                defer temp_mem.deinit();
                var scoring_run: core.ScoringRun = try .initFromFnks(all_fnks, &temp_mem);
                defer scoring_run.deinit(false);
                // Update 'actual' values
                var cur_testcase = FnkboxBox.children(children(fnkbox_index).box).testcases_area.get().tree.first;
                while (cur_testcase != .nothing) : (cur_testcase = cur_testcase.get().tree.next) {
                    const t = Testcase.children(cur_testcase);
                    const input_value = try t.input.get().specific.sexpr.toOldCoreValue(scratch);
                    const actual_value = try t.actual.get().specific.sexpr.toOldCoreValue(scratch);
                    var exec = try core.ExecutionThread.init(input_value, fnkname_value, &scoring_run);
                    defer exec.deinit();

                    const actual_output = exec.getFinalResultBoundedV2(&scoring_run, 10_000, true, true) catch |err| switch (err) {
                        error.FnkNotFound,
                        error.UsedUndefinedVariable,
                        error.InvalidMetaFnk,
                        error.TookTooLong,
                        => core.Sexpr.builtin.empty,
                        error.OutOfMemory => return err,
                        error.BAD_INPUT => @panic("panic"),
                        error.NoMatchingCase => unreachable,
                    };
                    if (!actual_output.equals(actual_value) and fnkbox.execution == null) {
                        Toybox.changeChild(t.actual, try Sexpr.buildFromOldCoreValue(
                            t.actual.get().local_point,
                            actual_output,
                            false,
                            false,
                            &worspace.undo_stack,
                        ), &worspace.undo_stack);
                    }
                }

                // Get the actual status and update testcases solved
                const box_index = children(fnkbox_index).box;
                cur_testcase = FnkboxBox.children(box_index).testcases_area.get().tree.first;
                var wrote_first_wrong = false;
                while (cur_testcase != .nothing) : (cur_testcase = cur_testcase.get().tree.next) {
                    const actual: Lego.Index = if (fnkbox.execution) |execution|
                        switch (execution.source) {
                            .testcase => |source| if (source == cur_testcase)
                                execution.old_testcase_actual_value
                            else
                                Testcase.children(cur_testcase).actual,
                            .input => Testcase.children(cur_testcase).actual,
                        }
                    else
                        Testcase.children(cur_testcase).actual;

                    const expected = Testcase.children(cur_testcase).expected;

                    const correct = Sexpr.equalValue(actual, expected);
                    cur_testcase.get().specific.testcase.solved = correct;

                    if (!correct and !wrote_first_wrong) {
                        fnkbox.status = .{ .unsolved = cur_testcase };
                        wrote_first_wrong = true;
                    }
                }
                fnkbox.status = .solved;
            }
        };

        pub const Testcase = struct {
            solved: bool = false,

            pub const relative_actual_point: Point = .{ .pos = .new(4, 0) };
            pub const relative_expected_point: Point = .{ .pos = .new(0, 0) };
            pub const relative_input_point: Point = .{ .pos = .new(-4, 0) };
            pub const relative_bounding_box: Rect = .fromCenterAndSize(.zero, .new(FnkboxBox.relative_box.size.x, 2.5));
            pub fn children(index: Lego.Index) struct {
                input: Lego.Index,
                expected: Lego.Index,
                actual: Lego.Index,
                play_button: Lego.Index,
            } {
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
                undo_stack: ?*UndoStack,

                const DrawingPart = struct {
                    point: Point,
                    part: union(enum) {
                        paragraph: []const []const u8,
                        arrow,
                        launch_testcase_button,
                        piece_center,
                    },
                };

                pub fn addFromParts(this: @This(), pos: Vec2, parts: []const DrawingPart) void {
                    Toybox.addChildLast(this.main_area, blk: {
                        const postit = try Toybox.new(
                            .{ .pos = pos },
                            .{ .postit = .{} },
                            this.undo_stack,
                        );

                        for (parts) |part| {
                            const top_left: Point = .{ .pos = Lego.Specific.Postit.local_rect.top_left };
                            const center = top_left.applyToLocalPoint(part.point);
                            switch (part.part) {
                                inline else => |_, part_tag| {
                                    Toybox.addChildLast(postit.index, (try Toybox.new(
                                        center,
                                        .{ .postit_drawing = switch (part_tag) {
                                            .paragraph => comptime unreachable,
                                            .arrow => .arrow,
                                            .piece_center => .piece_center,
                                            .launch_testcase_button => .launch_testcase_button,
                                        } },
                                        this.undo_stack,
                                    )).index, this.undo_stack);
                                },
                                .paragraph => |lines| {
                                    for (lines, 0..) |line, k| {
                                        Toybox.addChildLast(postit.index, (try Toybox.new(
                                            center.applyToLocalPoint(.{ .pos = .new(0, (tof32(k) - (tof32(lines.len) - 1) / 2.0)) }),
                                            .{ .postit_text = .{ .text = line } },
                                            this.undo_stack,
                                        )).index, this.undo_stack);
                                    }
                                },
                            }
                        }

                        break :blk postit.index;
                    }, this.undo_stack);
                }

                pub fn addFromText(this: @This(), pos: Vec2, lines: []const []const u8) void {
                    Toybox.addChildLast(this.main_area, blk: {
                        const postit = try Toybox.new(
                            .{ .pos = pos },
                            .{ .postit = .{} },
                            this.undo_stack,
                        );

                        for (lines, 0..) |line, k| {
                            Toybox.addChildLast(postit.index, (try Toybox.new(
                                .{ .pos = .new(0, (tof32(k) - (tof32(lines.len) - 1) / 2.0)) },
                                .{ .postit_text = .{ .text = line } },
                                this.undo_stack,
                            )).index, this.undo_stack);
                        }

                        break :blk postit.index;
                    }, this.undo_stack);
                }
            };
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
            if (tree.parent == .nothing) {
                assert(tree.next == .nothing);
                assert(tree.prev == .nothing);
                return true;
            } else return false;
        }

        pub fn isChildless(tree: Tree) bool {
            if (tree.first == .nothing) {
                assert(tree.last == .nothing);
                return true;
            } else {
                assert(tree.last != .nothing);
                return false;
            }
        }

        pub fn equals(a: Tree, b: Tree) bool {
            return std.meta.eql(a, b);
        }
    };

    pub const Index = enum(u32) {
        nothing = std.math.maxInt(u32),
        _,

        pub fn asI32(index: Index) i32 {
            return @bitCast(@intFromEnum(index));
        }

        pub fn hasTag(index: Index, tag: Specific.Tag) bool {
            return Toybox.get(index).specific.tag() == tag;
        }

        pub fn get(index: Index) *Lego {
            return Toybox.get(index);
        }

        pub fn getSafe(index: Index) ?*Lego {
            return Toybox.safeGet(index);
        }

        pub fn case(index: Index) struct {
            self: Lego.Index,
            pattern: Lego.Index,
            template: Lego.Index,
            fnkname: Lego.Index,
            next: Lego.Index,

            pub fn hasIdentityFnkname(this: @This()) bool {
                const sexpr = Toybox.get(this.fnkname).specific.sexpr;
                return sexpr.kind == .empty or
                    (sexpr.kind == .atom_lit and std.mem.eql(u8, sexpr.atom_name, "identity"));
            }
        } {
            const children = Specific.Case.children(index);
            return .{
                .self = index,
                .pattern = children.pattern,
                .template = children.template,
                .fnkname = children.fnkname,
                .next = children.next,
            };
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
    };

    pub fn handle(lego: *const Lego) ?Handle {
        const radius: Handle.Size = switch (lego.specific) {
            .sexpr,
            .area,
            .microscope,
            .button,
            .scrollbar,
            .fnkbox_box,
            .fnkbox_description,
            .fnkbox_testcases,
            .fnkslist,
            .fnkslist_element,
            .executor,
            .testcase,
            .pill,
            .postit,
            .postit_text,
            .postit_drawing,
            .executor_controls,
            .garland_newcases,
            => return null,
            .executor_brake => .default_extrahitbox,
            .executor_crank => |crank| if (crank.enabled) .default_extrahitbox else return null,
            .case => .default,
            .newcase => .new_case,
            .garland => .garland,
            .lens => .lens,
            .fnkbox => .default,
        };
        const enabled: bool = switch (lego.specific) {
            else => true,
            .garland => |garland| garland.visible,
        };
        return .{
            .point = lego.absolute_point.applyToLocalPoint(.{ .pos = lego.handleLocalOffset() }),
            .hot_t = lego.hot_t + lego.dropzone_t,
            .radius = radius,
            .enabled = enabled,
        };
    }

    fn handleLocalOffset(lego: *const Lego) Vec2 {
        return switch (lego.specific) {
            .lens => |lens| .fromPolar(lens.local_radius + 0.1, 1.0 / 8.0),
            .newcase => |newcase| .new(0, newcase.length_before),
            .executor_brake => |brake| brake.handle_pos,
            .executor_crank => |crank| crank.handle_pos,
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
            .scrollbar => |*scrollbar| {
                scrollbar.scroll_target += amount;
            },
            .fnkslist => |fnkslist| {
                fnkslist.scrollbar.get().specific.scrollbar.scroll_target += amount;
            },
            .fnkbox_testcases => |fnkbox_testcases| {
                fnkbox_testcases.scrollbar.get().specific.scrollbar.scroll_target += amount;
            },
        }
    }

    pub fn getGrabbedOffset(lego: *const Lego, absolute_needle: Vec2) Vec2 {
        return switch (lego.specific) {
            .postit => lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle),
            .scrollbar => |scrollbar| lego.absolute_point.applyToLocalPoint(.{ .pos = scrollbar.handleRectVisual().top_left }).inverseApplyGetLocalPosition(absolute_needle),
            else => .zero,
        };
    }

    pub fn canDuplicate(lego: *const Lego) bool {
        return switch (lego.specific) {
            .sexpr,
            .garland,
            .executor,
            .case,
            .postit,
            => true,
            .scrollbar,
            .button,
            .executor_brake,
            .executor_crank,
            => false,
            .fnkbox, .lens => blk: {
                std.log.err("TODO(game): handle better", .{});
                break :blk false;
            },
            .garland_newcases,
            .executor_controls,
            .microscope,
            .fnkbox_box,
            .fnkbox_description,
            .fnkbox_testcases,
            .fnkslist,
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

    pub fn grabsWithoutPlucking(lego: *const Lego) bool {
        return switch (lego.specific) {
            .button,
            .lens,
            .fnkbox,
            .executor_crank,
            .executor_brake,
            .scrollbar,
            => true,
            .sexpr,
            .garland,
            .case,
            .postit,
            .executor,
            => false,
            .garland_newcases,
            .executor_controls,
            .microscope,
            .fnkbox_box,
            .fnkbox_description,
            .fnkbox_testcases,
            .fnkslist,
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
            .fnkbox_testcases => .fromRect(Specific.FnkboxBox.testcases_box),
            .fnkbox => Bounds.fromRect(Specific.FnkboxBox.relative_box)
                // for the garland
                .plusMargin3(.bottom, std.math.inf(f32))
                .plusMargin3(.right, std.math.inf(f32)),
        };
    }
};

pub const ApiFor = struct {
    pub const Executor = struct {
        index: Lego.Index,

        pub fn garland(this: @This()) Garland {
            return .{ .index = Lego.Specific.Executor.children(this.index).garland };
        }
    };

    pub const Garland = struct {
        index: Lego.Index,
    };
};

pub const Handle = struct {
    point: Point,
    radius: Size,
    hot_t: f32,
    enabled: bool,

    pub const Size = extern struct {
        base: f32,
        hot: f32,
        hitbox: f32,

        pub const default: Size = .{ .base = 0.2, .hot = 0.24, .hitbox = 0.24 };
        pub const default_extrahitbox: Size = .{ .base = 0.2, .hot = 0.24, .hitbox = 1.0 };
        pub const new_case: Size = .{ .base = 0.1, .hot = 0.4, .hitbox = 1.75 };
        pub const garland: Size = .{ .base = 0.3, .hot = 0.5, .hitbox = 0.5 };
        pub const lens: Size = .{ .base = 0.1, .hot = 0.2, .hitbox = 0.2 };
    };

    pub fn draw(handle: *const Handle, drawer: *Drawer, camera: Rect, alpha: f32) !void {
        if (handle.enabled) {
            const r = std.math.lerp(handle.radius.base, handle.radius.hot, handle.hot_t);
            drawer.canvas.fillCircle(camera, handle.point.pos, handle.point.scale * r, COLORS.bg.withAlpha(alpha));
            drawer.canvas.strokeCircle(128, camera, handle.point.pos, handle.point.scale * r, 0.05 * handle.point.scale, .blackAlpha(alpha));
        }
    }

    pub fn overlapped(handle: *const Handle, pos: Vec2) bool {
        return handle.enabled and pos.distTo(handle.point.pos) < handle.radius.hitbox * handle.point.scale;
    }
};

pub const Toybox = struct {
    // TODO(optim-late): use a fancy arena thing
    all_legos: std.ArrayListUnmanaged(Lego),
    all_legos_arena: std.heap.ArenaAllocator,
    free_head: Lego.Index = .nothing,

    pub fn init(dst: *Toybox, gpa: std.mem.Allocator) !void {
        dst.* = .{
            .all_legos_arena = .init(gpa),
            .all_legos = .empty,
        };
        // TODO(optim-late): tweak this number
        try dst.all_legos.ensureUnusedCapacity(
            dst.all_legos_arena.allocator(),
            1024,
        );
    }

    pub fn deinit(self: *Toybox) void {
        self.all_legos.deinit(self.all_legos_arena.allocator());
        self.all_legos_arena.deinit();
    }

    pub fn OoM() noreturn {
        std.debug.panic("OoM", .{});
    }

    pub fn createWithChildren(local_point: Point, specific: Lego.Specific, children: []const Lego.Index, undo_stack: ?*UndoStack) !Lego.Index {
        const lego = try new(local_point, specific, undo_stack);
        for (children) |child| {
            addChildLast(lego.index, child, undo_stack);
        }
        return lego.index;
    }

    pub fn new(local_point: Point, specific: Lego.Specific, undo_stack: ?*UndoStack) !*Lego {
        const result: *Lego, const index: Lego.Index = if (ENABLE_REUSE and toybox.free_head != .nothing) blk: {
            const result_index = toybox.free_head;
            const result = Toybox.getUnsafe(result_index);
            toybox.free_head = result.free_next;
            break :blk .{ result, result_index };
        } else blk: {
            if (toybox.all_legos.items.len >= std.math.maxInt(i31)) OoM();
            const result = toybox.all_legos.addOne(toybox.all_legos_arena.allocator()) catch OoM();
            break :blk .{ result, @enumFromInt(toybox.all_legos.items.len - 1) };
        };

        result.* = .{
            .index = index,
            .exists = true,
            .local_point = local_point,
            .absolute_point = local_point,
            .specific = specific,
        };
        if (undo_stack) |stack| stack.append(.{ .destroy_floating = index });
        return result;
    }

    pub fn get(index: Lego.Index) *Lego {
        const result = getUnsafe(index);
        assert(result.exists);
        return result;
    }

    pub fn getUnsafe(index: Lego.Index) *Lego {
        assert(index != .nothing);
        return &toybox.all_legos.items[@intFromEnum(index)];
    }

    pub fn safeGet(index: Lego.Index) ?*Lego {
        if (index == .nothing) return null;
        const result = getUnsafe(index);
        if (!result.exists) return null;
        return result;
    }

    pub fn addChildLastWithoutChangingAbsPoint(parent: Lego.Index, new_child: Lego.Index, undo_stack: ?*UndoStack) void {
        addChildLast(parent, new_child, undo_stack);
        changeCoordinates(new_child, .{}, parent.get().absolute_point);
    }

    pub fn addChildLastWithLocalPoint(local_point: Point, parent: Lego.Index, new_child: Lego.Index, undo_stack: ?*UndoStack) void {
        new_child.get().local_point = local_point;
        addChildLast(parent, new_child, undo_stack);
    }

    // TODO(design): remove the old version
    pub fn addChildLastV2(local_point: ?Point, parent: Lego.Index, new_child: Lego.Index, undo_stack: ?*UndoStack) void {
        if (local_point) |l| {
            addChildLast(parent, new_child, undo_stack);
            new_child.get().local_point = l;
        } else {
            addChildLastWithoutChangingAbsPoint(parent, new_child, undo_stack);
        }
    }

    pub fn addChildLast(parent: Lego.Index, new_child: Lego.Index, undo_stack: ?*UndoStack) void {
        assert(parent != .nothing);
        if (new_child == .nothing) return;
        if (undo_stack) |stack| {
            stack.append(.{ .pop = new_child });
        }
        const parent_tree = &Toybox.get(parent).tree;
        const child_tree = &Toybox.get(new_child).tree;
        assert(child_tree.isFloating());
        child_tree.parent = parent;
        child_tree.prev = parent_tree.last;
        child_tree.next = .nothing;
        if (parent_tree.last != .nothing) {
            Toybox.get(parent_tree.last).tree.next = new_child;
        }
        parent_tree.last = new_child;
        if (parent_tree.first == .nothing) {
            parent_tree.first = new_child;
        }
    }

    pub fn isFloating(index: Lego.Index) bool {
        return Toybox.get(index).tree.isFloating();
    }

    pub fn destroyFloatingWithUndo(index: Lego.Index, undo_stack: *UndoStack) void {
        Toybox.destroyFloating(index, undo_stack);
    }

    pub fn destroyFloating(index: Lego.Index, undo_stack: ?*UndoStack) void {
        assert(Toybox.isFloating(index));

        // TODO(optim): avoid recursion
        while (index.get().tree.first != .nothing) {
            const child = index.get().tree.first;
            Toybox.pop(child, undo_stack);
            Toybox.destroyFloating(child, undo_stack);
        }

        if (undo_stack) |stack| {
            stack.append(.{ .recreate_floating = Toybox.get(index).* });
        }

        const lego = Toybox.get(index);
        lego.* = undefined;
        lego.index = index;
        lego.exists = false;
        lego.free_next = toybox.free_head;
        toybox.free_head = index;
    }

    pub fn recreateFloating(data: Lego) void {
        assert(data.tree.isFloating());
        const lego = Toybox.getUnsafe(data.index);
        assert(!lego.exists);
        lego.* = data;
    }

    pub fn dupeIntoFloatingWithoutChangingPos(original: Lego.Index, dupe_children: bool, undo_stack: ?*UndoStack) !Lego.Index {
        const result = try Toybox.dupeIntoFloating(original, dupe_children, undo_stack);
        Toybox.get(result).local_point = Toybox.get(original).absolute_point;
        return result;
    }

    pub fn dupeIntoFloating(original: Lego.Index, dupe_children: bool, undo_stack: ?*UndoStack) !Lego.Index {
        const result = try Toybox.new(undefined, undefined, undo_stack);
        const result_index = result.index;
        result.* = Toybox.get(original).*;
        result.index = result_index;
        result.tree.parent = .nothing;
        result.tree.next = .nothing;
        result.tree.prev = .nothing;

        if (dupe_children) {
            var cur = result.tree.first;
            result.tree.first = .nothing;
            result.tree.last = .nothing;
            while (cur != .nothing) : (cur = Toybox.get(cur).tree.next) {
                const new_child_index = try Toybox.dupeIntoFloating(cur, true, undo_stack);
                Toybox.addChildLast(result_index, new_child_index, undo_stack);
            }
        } else {
            result.tree.first = .nothing;
            result.tree.last = .nothing;
        }

        return result_index;
    }

    pub fn getChildrenExact(comptime expected_count: usize, parent: Lego.Index) [expected_count]Lego.Index {
        var cur = Toybox.get(parent).tree.first;
        var result: [expected_count]Lego.Index = undefined;
        for (&result) |*dst| {
            assert(cur != .nothing);
            dst.* = cur;
            cur = Toybox.get(cur).tree.next;
        }
        assert(cur == .nothing);
        return result;
    }

    pub fn childCount(index: Lego.Index) usize {
        var count: usize = 0;
        var cur = Toybox.get(index).tree.first;
        while (cur != .nothing) {
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
            assert(cur != .nothing);
            dst.* = cur;
            cur = Toybox.get(cur).tree.next;
        }
        assert(cur == .nothing);
        return result;
    }

    pub fn pop(child: Lego.Index, undo_stack: ?*UndoStack) void {
        assert(!Toybox.isFloating(child));
        changeChild(child, .nothing, undo_stack);
    }

    pub fn popWithUndo(child: Lego.Index, undo_stack: *UndoStack) void {
        Toybox.pop(child, undo_stack);
    }

    pub fn popWithUndoAndChangingCoords(child: Lego.Index, undo_stack: ?*UndoStack) void {
        const old_parent_abs_point = Toybox.parentAbsolutePoint(child);
        Toybox.pop(child, undo_stack);
        Toybox.changeCoordinates(child, old_parent_abs_point, .{});
    }

    pub fn insert(child: Lego.Index, where: Lego.Tree, undo_stack: ?*UndoStack) void {
        assert(Toybox.isFloating(child));
        assert(!where.isFloating());
        defer assert(Toybox.get(child).tree.equals(where));

        if (undo_stack) |stack| stack.append(.{ .pop = child });

        if (where.prev != .nothing) {
            assert(Toybox.get(where.prev).tree.next == where.next);
            Toybox.get(where.prev).tree.next = child;
        } else {
            Toybox.get(where.parent).tree.first = child;
        }

        if (where.next != .nothing) {
            assert(Toybox.get(where.next).tree.prev == where.prev);
            Toybox.get(where.next).tree.prev = child;
        } else {
            Toybox.get(where.parent).tree.last = child;
        }

        Toybox.get(child).tree = where;
    }

    pub fn changeChildWithUndo(original_child: Lego.Index, new_child: Lego.Index, undo_stack: *UndoStack) void {
        Toybox.changeChild(original_child, new_child, undo_stack);
    }

    // TODO(design): remove
    pub fn changeChildWithUndoAndAlsoCoords(original_child: Lego.Index, new_child: Lego.Index, undo_stack: *UndoStack) void {
        const old_parent_abs_point = Toybox.parentAbsolutePoint(original_child);
        Toybox.changeChild(original_child, new_child, undo_stack);
        Toybox.changeCoordinates(original_child, old_parent_abs_point, .{});
        Toybox.changeCoordinates(new_child, .{}, old_parent_abs_point);
    }

    /// things that pointed to original, now will point to new
    /// original will be left floating
    pub fn changeChild(original_child: Lego.Index, new_child: Lego.Index, undo_stack: ?*UndoStack) void {
        assert(original_child != .nothing);
        assert(new_child == .nothing or isFloating(new_child));
        defer assert(isFloating(original_child));

        if (undo_stack) |undo| {
            if (new_child == .nothing) {
                const original_parent_tree = Toybox.get(original_child).tree;
                undo.append(.{
                    .insert = .{
                        .what = original_child,
                        .where = original_parent_tree,
                    },
                });
            } else {
                undo.append(.{ .change_child = .{
                    .original = new_child,
                    .new = original_child,
                } });
            }
        }

        const original_tree: Lego.Tree = get(original_child).tree;
        assert(original_tree.parent != .nothing);
        const parent_tree: *Lego.Tree = &get(original_tree.parent).tree;
        if (parent_tree.first == original_child) {
            parent_tree.first = if (new_child != .nothing) new_child else original_tree.next;
        }
        if (@hasField(Lego.Tree, "last")) {
            if (parent_tree.last == original_child) {
                parent_tree.last = if (new_child != .nothing) new_child else original_tree.prev;
            }
        }
        if (original_tree.prev != .nothing) {
            get(original_tree.prev).tree.next = if (new_child != .nothing) new_child else original_tree.next;
        }
        if (original_tree.next != .nothing) {
            get(original_tree.next).tree.prev = if (new_child != .nothing) new_child else original_tree.prev;
        }
        if (new_child != .nothing) {
            const new_child_tree = &get(new_child).tree;
            assert(new_child_tree.parent == .nothing and
                new_child_tree.prev == .nothing and
                new_child_tree.next == .nothing);
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
        assert(root != .nothing and current != .nothing);
        var result: VisitStep = .{ .next = .nothing };
        // var result: VisitStep = .{ .next = .nothing, .pop_count = 0, .push_count = 0 };
        const cur = Toybox.get(current);
        if (cur.tree.first != .nothing) {
            result.next = cur.tree.first;
            // result.push_count = 1;
        } else {
            var p = current;
            while (p != .nothing and p != root) : (p = Toybox.get(p).tree.parent) {
                const next = Toybox.get(p).tree.next;
                if (next != .nothing) {
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
        assert(root != .nothing and current != .nothing);
        var result: VisitStep = .{ .next = .nothing };
        // var result: VisitStep = .{ .next = .nothing, .pop_count = 0, .push_count = 0 };
        const cur = Toybox.get(current);
        if (cur.tree.last != .nothing) {
            result.next = cur.tree.last;
            // result.push_count = 1;
        } else {
            var p = current;
            while (p != .nothing and p != root) : (p = Toybox.get(p).tree.parent) {
                const next = Toybox.get(p).tree.prev;
                if (next != .nothing) {
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
            if (it.cur == .nothing) return null;
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
                } else if (sibling != .nothing) {
                    it.cur = sibling;
                    it.going_up = false;
                } else {
                    it.cur = tree.parent;
                }
            } else {
                if (child != .nothing) {
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
        const undo_stack: ?*UndoStack = null;
        const root = try Toybox.new(undefined, undefined, undo_stack);
        const child_1 = try Toybox.new(undefined, undefined, undo_stack);
        const child_2 = try Toybox.new(undefined, undefined, undo_stack);
        const grandchild_1_1 = try Toybox.new(undefined, undefined, undo_stack);
        const grandchild_1_2 = try Toybox.new(undefined, undefined, undo_stack);
        const grandchild_2_1 = try Toybox.new(undefined, undefined, undo_stack);
        const grandchild_2_2 = try Toybox.new(undefined, undefined, undo_stack);

        Toybox.addChildLast(root.index, child_1.index, undo_stack);
        Toybox.addChildLast(root.index, child_2.index, undo_stack);

        Toybox.addChildLast(child_1.index, grandchild_1_1.index, undo_stack);
        Toybox.addChildLast(child_1.index, grandchild_1_2.index, undo_stack);

        Toybox.addChildLast(child_2.index, grandchild_2_1.index, undo_stack);
        Toybox.addChildLast(child_2.index, grandchild_2_2.index, undo_stack);

        try std.testing.expectEqual(child_1.index, Toybox.get(grandchild_1_1.index).tree.parent);

        if (true) {
            const expected_order: [7]VisitStep = .{
                .{ .next = root.index },
                .{ .next = child_1.index },
                .{ .next = grandchild_1_1.index },
                .{ .next = grandchild_1_2.index },
                .{ .next = child_2.index },
                .{ .next = grandchild_2_1.index },
                .{ .next = grandchild_2_2.index },
            };

            var actual_order: std.ArrayListUnmanaged(VisitStep) = try .initCapacity(std.testing.allocator, expected_order.len);
            defer actual_order.deinit(std.testing.allocator);

            var cur: VisitStep = .{ .next = root.index };
            while (cur.next != .nothing) : (cur = Toybox.next_preordered(cur.next, root.index)) {
                try actual_order.append(std.testing.allocator, cur);
            }

            try std.testing.expectEqualSlices(VisitStep, &expected_order, actual_order.items);
        }

        if (true) {
            const expected_order: [14]TreeIterator.Step = .{
                .{ .children_already_visited = false, .index = root.index },
                .{ .children_already_visited = false, .index = child_1.index },
                .{ .children_already_visited = false, .index = grandchild_1_1.index },
                .{ .children_already_visited = true, .index = grandchild_1_1.index },
                .{ .children_already_visited = false, .index = grandchild_1_2.index },
                .{ .children_already_visited = true, .index = grandchild_1_2.index },
                .{ .children_already_visited = true, .index = child_1.index },
                .{ .children_already_visited = false, .index = child_2.index },
                .{ .children_already_visited = false, .index = grandchild_2_1.index },
                .{ .children_already_visited = true, .index = grandchild_2_1.index },
                .{ .children_already_visited = false, .index = grandchild_2_2.index },
                .{ .children_already_visited = true, .index = grandchild_2_2.index },
                .{ .children_already_visited = true, .index = child_2.index },
                .{ .children_already_visited = true, .index = root.index },
            };

            var actual_order: std.ArrayListUnmanaged(TreeIterator.Step) = try .initCapacity(std.testing.allocator, expected_order.len);
            defer actual_order.deinit(std.testing.allocator);

            var it = Toybox.treeIterator(root.index, true);
            while (it.next()) |step| {
                try actual_order.append(std.testing.allocator, step);
            }

            try std.testing.expectEqualSlices(TreeIterator.Step, &expected_order, actual_order.items);
        }

        if (true) {
            const expected_order: [10]TreeIterator.Step = .{
                .{ .children_already_visited = false, .index = root.index },
                .{ .children_already_visited = false, .index = child_1.index },
                .{ .children_already_visited = true, .index = child_1.index },
                .{ .children_already_visited = false, .index = child_2.index },
                .{ .children_already_visited = false, .index = grandchild_2_1.index },
                .{ .children_already_visited = true, .index = grandchild_2_1.index },
                .{ .children_already_visited = false, .index = grandchild_2_2.index },
                .{ .children_already_visited = true, .index = grandchild_2_2.index },
                .{ .children_already_visited = true, .index = child_2.index },
                .{ .children_already_visited = true, .index = root.index },
            };

            var actual_order: std.ArrayListUnmanaged(TreeIterator.Step) = try .initCapacity(std.testing.allocator, expected_order.len);
            defer actual_order.deinit(std.testing.allocator);

            var it = Toybox.treeIterator(root.index, true);
            while (it.next()) |step| {
                try actual_order.append(std.testing.allocator, step);
                if (step.index == child_1.index and !step.children_already_visited) {
                    it.skipChildren();
                }
            }

            try std.testing.expectEqualSlices(TreeIterator.Step, &expected_order, actual_order.items);
        }

        if (true) {
            const expected_order: [10]TreeIterator.Step = .{
                .{ .children_already_visited = false, .index = root.index },
                .{ .children_already_visited = false, .index = child_2.index },
                .{ .children_already_visited = false, .index = grandchild_2_2.index },
                .{ .children_already_visited = true, .index = grandchild_2_2.index },
                .{ .children_already_visited = false, .index = grandchild_2_1.index },
                .{ .children_already_visited = true, .index = grandchild_2_1.index },
                .{ .children_already_visited = true, .index = child_2.index },
                .{ .children_already_visited = false, .index = child_1.index },
                .{ .children_already_visited = true, .index = child_1.index },
                .{ .children_already_visited = true, .index = root.index },
            };

            var actual_order: std.ArrayListUnmanaged(TreeIterator.Step) = try .initCapacity(std.testing.allocator, expected_order.len);
            defer actual_order.deinit(std.testing.allocator);

            var it = Toybox.treeIterator(root.index, false);
            while (it.next()) |step| {
                try actual_order.append(std.testing.allocator, step);
                if (step.index == child_1.index and !step.children_already_visited) {
                    it.skipChildren();
                }
            }

            try std.testing.expectEqualSlices(TreeIterator.Step, &expected_order, actual_order.items);
        }
    }

    pub fn oldestAncestor(index: Lego.Index) Lego.Index {
        assert(index != .nothing);
        var cur = index;
        while (true) {
            const next = Toybox.get(cur).tree.parent;
            if (next == .nothing) return cur;
            cur = next;
        }
    }

    pub fn isAncestor(parent: Lego.Index, child: Lego.Index) bool {
        var cur = child;
        while (cur != .nothing) {
            if (cur == parent) return true;
            cur = cur.get().tree.parent;
        }
        return false;
    }

    pub fn findAncestor(index: Lego.Index, kind: Lego.Specific.Tag) Lego.Index {
        assert(index != .nothing);
        var cur = index;
        while (cur != .nothing) {
            if (Toybox.get(cur).specific.tag() == kind) return cur;
            cur = Toybox.get(cur).tree.parent;
        }
        return .nothing;
    }

    pub fn parentAbsolutePoint(index: Lego.Index) Point {
        assert(index != .nothing);
        const parent = Toybox.get(index).tree.parent;
        if (parent == .nothing) return .{};
        return Toybox.get(parent).absolute_point;
    }

    pub fn refreshAbsolutePoints(roots: []const Lego.Index) void {
        const zone = tracy.initZone(@src(), .{ .name = "refresh absolute points" });
        defer zone.deinit();

        for (roots) |root| {
            var cur: Lego.Index = root;
            while (cur != .nothing) : (cur = next_preordered(cur, root).next) {
                Toybox.get(cur).absolute_point = parentAbsolutePoint(cur)
                    .applyToLocalPoint(Toybox.get(cur).local_point);
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

    pub fn buildSexpr(local_point: Point, value: union(Lego.Specific.Sexpr.Kind) {
        empty,
        atom_lit: []const u8,
        atom_var: []const u8,
        pair: struct { up: Lego.Index, down: Lego.Index },
    }, is_pattern: bool, is_fnkname: bool, undo_stack: ?*UndoStack) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .sexpr = .{
            .is_pattern = is_pattern,
            .is_pattern_t = if (is_pattern) 1 else 0,
            .is_fnkname = is_fnkname,
            .is_fnkname_t = if (is_fnkname) 1 else 0,
            .immutable = false,
            .atom_name = switch (value) {
                .atom_lit, .atom_var => |v| v,
                else => undefined,
            },
            .kind = value,
        } }, undo_stack);
        switch (value) {
            else => {},
            .pair => |pair| {
                Toybox.addChildLastV2(ViewHelper.offsetFor(is_pattern, .up), result.index, pair.up, undo_stack);
                Toybox.addChildLastV2(ViewHelper.offsetFor(is_pattern, .down), result.index, pair.down, undo_stack);
            },
        }
        return result.index;
    }

    pub fn buildCase(local_point: Point, data: struct {
        pattern: Lego.Index,
        template: Lego.Index,
        fnkname: ?Lego.Index,
        next: ?Lego.Index,
    }, undo_stack: ?*UndoStack) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .case = .{} }, undo_stack);
        Toybox.addChildLastV2(.{ .pos = .xneg }, result.index, data.pattern, undo_stack);
        Toybox.addChildLastV2(.{ .pos = .xpos }, result.index, data.template, undo_stack);
        Toybox.addChildLastV2(.{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) }, result.index, data.fnkname orelse try Toybox.buildSexpr(.{}, .empty, false, true, undo_stack), undo_stack);
        Toybox.addChildLastV2(.{ .pos = .new(8, 1) }, result.index, data.next orelse try Toybox.buildGarland(local_point, &.{}, undo_stack), undo_stack);
        return result.index;
    }

    /// The garland's children are a linear list of newcase, all except the last one with a child case
    /// the newcase position is the very top of the segment
    pub fn buildGarland(local_point: Point, child_cases: []const Lego.Index, undo_stack: ?*UndoStack) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .garland = .{} }, undo_stack);
        Toybox.addChildLast(result.index, try buildSexpr(
            Lego.Specific.Garland.relative_fnkname_point,
            .empty,
            true,
            true,
            undo_stack,
        ), undo_stack);

        const cases_holder = try Toybox.new(.{}, .garland_newcases, undo_stack);
        for (child_cases) |case| {
            const new_segment = try Toybox.new(.{}, .{ .newcase = .{} }, undo_stack);
            Toybox.addChildLast(new_segment.index, case, undo_stack);
            Toybox.addChildLast(cases_holder.index, new_segment.index, undo_stack);
        }
        Toybox.addChildLast(cases_holder.index, (try Toybox.new(.{}, .{ .newcase = .{} }, undo_stack)).index, undo_stack);

        Toybox.addChildLast(result.index, cases_holder.index, undo_stack);
        return result.index;
    }

    /// Children are:
    /// - box with (description area, status bar, testcases scroll bar, testcases area)
    /// - fnkname
    /// - executor
    pub fn buildFnkbox(
        local_point: Point,
        // TODO(design): take *const Sepxr
        fnkname: Lego.Index,
        text: []const u8,
        testcases: []const [2]Lego.Index,
        initial_definition: ?Lego.Index,
        undo_stack: ?*UndoStack,
    ) !Lego.Index {
        const Fnkbox = Lego.Specific.Fnkbox;
        const FnkboxBox = Lego.Specific.FnkboxBox;
        const Executor = Lego.Specific.Executor;

        const scrollbar =
            try createWithChildren(.{}, .{ .scrollbar = .buildForTestcases(testcases.len, 0) }, &.{
                (try new(.{}, .{ .button = .{
                    .local_rect = FnkboxBox.testcases_box.withSize(.new(0.7, 0.7), .top_left).plusMargin(-0.1),
                    .action = .scroll_up,
                } }, undo_stack)).index,
                (try new(.{}, .{ .button = .{
                    .local_rect = FnkboxBox.testcases_box.withSize(.new(0.7, 0.7), .bottom_left).plusMargin(-0.1),
                    .action = .scroll_down,
                } }, undo_stack)).index,
            }, undo_stack);

        const box = try Toybox.createWithChildren(.{}, .{ .fnkbox_box = .{} }, &.{
            (try new(.{}, .{ .fnkbox_description = .{
                .text = text,
            } }, undo_stack)).index,
            (try new(.{}, .{ .button = .{
                .local_rect = FnkboxBox.status_bar_goal,
                .action = .see_failing_testcase,
            } }, undo_stack)).index,
            scrollbar,
            blk: {
                const fnkbox_testcases = try Toybox.new(.{}, .{
                    .fnkbox_testcases = .{ .scrollbar = scrollbar },
                }, undo_stack);
                for (testcases) |values| {
                    const Testcase = Lego.Specific.Testcase;
                    const testcase = try Toybox.new(.{}, .{ .testcase = .{} }, undo_stack);
                    Toybox.addChildLastV2(Testcase.relative_input_point, testcase.index, values[0], undo_stack);
                    Toybox.addChildLastV2(Testcase.relative_expected_point, testcase.index, values[1], undo_stack);
                    Toybox.addChildLastV2(Testcase.relative_actual_point, testcase.index, try Toybox.buildSexpr(.{}, .empty, false, false, undo_stack), undo_stack);
                    Toybox.addChildLast(testcase.index, (try Toybox.new(.{}, .{ .button = .{
                        .local_rect = .fromCenterAndSize(.new(-6, 0), .one),
                        .action = .launch_testcase,
                    } }, undo_stack)).index, undo_stack);
                    Toybox.addChildLast(fnkbox_testcases.index, testcase.index, undo_stack);
                }
                break :blk fnkbox_testcases.index;
            },
        }, undo_stack);

        fnkname.get().local_point = Fnkbox.relative_fnkname_point;

        if (initial_definition) |d| d.get().local_point = Executor.relative_garland_point;

        const executor = try Toybox.createWithChildren(Fnkbox.relative_executor_point, .{
            .executor = .{ .controlled_by_parent_fnkbox = true },
        }, &.{
            try Toybox.buildSexpr(Executor.relative_input_point, .empty, false, false, undo_stack),
            initial_definition orelse try Toybox.buildGarland(Executor.relative_garland_point, &.{}, undo_stack),
            blk: {
                const controls = try Toybox.new(Executor.relative_crank_center, .executor_controls, undo_stack);
                Toybox.addChildLast(controls.index, (try Toybox.new(.{}, .{ .executor_brake = .{ .brake_t = 0.5 } }, undo_stack)).index, undo_stack);
                Toybox.addChildLast(controls.index, (try Toybox.new(.{}, .{ .executor_crank = .{ .value = 0.0 } }, undo_stack)).index, undo_stack);
                break :blk controls.index;
            },
        }, undo_stack);

        return try Toybox.createWithChildren(local_point, .{ .fnkbox = .{ .status = undefined } }, &.{
            box,
            fnkname,
            executor,
        }, undo_stack);
    }

    pub fn buildMicroscope(source: Vec2, target: Vec2, undo_stack: ?*UndoStack) !Lego.Index {
        const lens_source = try Toybox.new(.{ .pos = source }, .{ .lens = .source }, undo_stack);
        const lens_target = try Toybox.new(.{ .pos = target }, .{ .lens = .target }, undo_stack);
        const result = try Toybox.new(.{}, .microscope, undo_stack);
        Toybox.addChildLast(result.index, lens_source.index, undo_stack);
        Toybox.addChildLast(result.index, lens_target.index, undo_stack);
        return result.index;
    }
};

pub const UndoStack = struct {
    commands: std.ArrayList(Workspace.UndoableCommand),
    last_frame_command_count: usize = 0,

    pub fn init(gpa: std.mem.Allocator) UndoStack {
        return .{ .commands = .init(gpa) };
    }

    pub fn deinit(self: *UndoStack) void {
        self.commands.deinit();
    }

    pub fn append(self: *UndoStack, command: Workspace.UndoableCommand) void {
        self.commands.append(command) catch Toybox.OoM();
    }

    pub fn storeAllData(self: *UndoStack, index: Lego.Index) void {
        self.append(.{ .set_data_except_tree = Toybox.get(index).* });
    }

    pub fn pop(self: *UndoStack) ?Workspace.UndoableCommand {
        return self.commands.pop();
    }

    pub fn startFrame(self: *UndoStack) void {
        self.last_frame_command_count = self.commands.items.len;
    }

    pub fn anyChangesThisFrame(self: *UndoStack) bool {
        return self.commands.items.len != self.last_frame_command_count;
    }
};

const Workspace = struct {
    main_area: Lego.Index,
    fnkboxes_layer: Lego.Index,
    toolbar_left: Lego.Index,
    toolbar_left_unfolded_t: f32 = 0,
    toolbar_fnks: Lego.Index,
    toolbar_fnks_unfolded_t: f32 = 0,
    lenses_layer: Lego.Index,
    floating_inputs_layer: Lego.Index,
    hand_layer: Lego.Index = .nothing,

    grabbing: Grabbing = .nothing,

    undo_stack: UndoStack,
    random_instance: std.Random.DefaultPrng,
    arena_for_atom_names: std.heap.ArenaAllocator,
    arena_for_lenses_data: std.heap.ArenaAllocator,

    // TODO(design): remove
    gpa_for_bindings: std.mem.Allocator,

    pub const Grabbing = struct {
        index: Lego.Index,
        offset: Vec2,

        pub const nothing: Grabbing = .{ .index = .nothing, .offset = .zero };
    };

    pub const toolbar_left_rect: Rect = .{ .top_left = .zero, .size = .new(6, 15) };
    pub const toolbar_fnks_rect: Rect = .{ .top_left = .zero, .size = .new(12, 15) };

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
            .include_floating_inputs = false,
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
        result.appendAssumeCapacity(workspace.fnkboxes_layer);
        if (config.include_floating_inputs) result.appendAssumeCapacity(workspace.floating_inputs_layer);
        if (config.include_toolbars) result.appendAssumeCapacity(workspace.toolbar_left);
        if (config.include_toolbars) result.appendAssumeCapacity(workspace.toolbar_fnks);
        if (config.include_hand) result.appendAssumeCapacity(workspace.hand_layer);
        if (config.include_lenses) result.appendAssumeCapacity(workspace.lenses_layer);
        return result;
    }

    pub fn init(dst: *Workspace, gpa: std.mem.Allocator, random_seed: u64) !void {
        dst.* = kommon.meta.initDefaultFields(Workspace);
        dst.undo_stack = .init(gpa);
        dst.random_instance = .init(random_seed);
        dst.arena_for_atom_names = .init(gpa);
        dst.arena_for_lenses_data = .init(gpa);
        dst.gpa_for_bindings = gpa;

        const undo_stack: ?*UndoStack = null;

        dst.main_area = (try Toybox.new(.{ .scale = 0.1 }, .{ .area = .{ .bg = .all } }, undo_stack)).index;
        dst.toolbar_left = (try Toybox.new(.{}, .{
            .area = .{
                .bg = .{
                    // ensure that "mouse off-screen on the left" also overlaps the toolbar
                    .local_rect = toolbar_left_rect.plusMargin3(.left, 100),
                },
            },
        }, undo_stack)).index;
        dst.toolbar_fnks, const fnkslist: Lego.Index = blk: {
            const area = (try Toybox.new(.{}, .{
                .area = .{
                    .bg = .{
                        // ensure that "mouse off-screen on the right" also overlaps the toolbar
                        .local_rect = toolbar_fnks_rect.plusMargin3(.right, 100),
                    },
                },
            }, undo_stack)).index;

            const scrollbar = Lego.Specific.Scrollbar.build(
                toolbar_fnks_rect
                    .withSize(.new(0.5, toolbar_fnks_rect.size.y), .top_right),
                0,
                toolbar_fnks_rect.size.y / Lego.Specific.FnkslistElement.height,
                undo_stack,
            );

            const fnkslist = try Toybox.new(.{}, .{
                .fnkslist = .{ .scrollbar = scrollbar },
            }, undo_stack);

            Toybox.addChildLast(area, fnkslist.index, undo_stack);

            Toybox.addChildLast(area, scrollbar, undo_stack);

            break :blk .{ area, fnkslist.index };
        };
        dst.fnkboxes_layer = (try Toybox.new(undefined, .{ .area = .{ .bg = .none } }, undo_stack)).index;
        dst.lenses_layer = (try Toybox.new(undefined, .{ .area = .{ .bg = .none } }, undo_stack)).index;
        dst.floating_inputs_layer = (try Toybox.new(undefined, .{ .area = .{ .bg = .none } }, undo_stack)).index;

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
                        undo_stack,
                    ),
                    "do lowercase",
                    &.{
                        .{
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "A" }, false, false, undo_stack),
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, undo_stack),
                        },
                        .{
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "B" }, false, false, undo_stack),
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, undo_stack),
                        },
                        .{
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "C" }, false, false, undo_stack),
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, undo_stack),
                        },
                    },
                    try Toybox.buildGarland(.{}, &.{
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "A" }, true, false, undo_stack),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "B" }, false, false, undo_stack),
                            .fnkname = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, undo_stack),
                            .next = try Toybox.buildGarland(.{}, &.{
                                try Toybox.buildCase(.{}, .{
                                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, undo_stack),
                                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, undo_stack),
                                    .fnkname = null,
                                    .next = null,
                                }, undo_stack),
                            }, undo_stack),
                        }, undo_stack),
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "B" }, true, false, undo_stack),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "C" }, false, false, undo_stack),
                            .fnkname = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, true, undo_stack),
                            .next = try Toybox.buildGarland(.{}, &.{
                                try Toybox.buildCase(.{}, .{
                                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, true, false, undo_stack),
                                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, undo_stack),
                                    .fnkname = null,
                                    .next = null,
                                }, undo_stack),
                            }, undo_stack),
                        }, undo_stack),
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "C" }, true, false, undo_stack),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false, false, undo_stack),
                            .fnkname = null,
                            .next = null,
                        }, undo_stack),
                    }, undo_stack),
                    undo_stack,
                ),
                undo_stack,
            );

            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = .new(0, 0) },
                .{ .atom_lit = "true" },
                false,
                false,
                undo_stack,
            ), undo_stack);

            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = .new(0, 1) },
                .{ .atom_lit = "false" },
                false,
                false,
                undo_stack,
            ), undo_stack);

            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = .new(3, 0) },
                .{ .pair = .{
                    .up = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, false, false, undo_stack),
                    .down = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, undo_stack),
                } },
                false,
                false,
                undo_stack,
            ), undo_stack);

            Toybox.addChildLast(dst.main_area, try Toybox.buildCase(
                .{ .pos = .new(0, 4) },
                .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true, false, undo_stack),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, undo_stack),
                    .fnkname = null,
                    .next = null,
                },
                undo_stack,
            ), undo_stack);

            const case_1 = try Toybox.buildCase(.{}, .{
                .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true, false, undo_stack),
                .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, undo_stack),
                .fnkname = null,
                .next = null,
            }, undo_stack);
            const case_2 = try Toybox.buildCase(.{}, .{
                .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true, false, undo_stack),
                .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false, false, undo_stack),
                .fnkname = null,
                .next = null,
            }, undo_stack);
            Toybox.addChildLast(dst.main_area, try Toybox.buildGarland(
                .{ .pos = .new(7, 4) },
                &.{ case_1, case_2 },
                undo_stack,
            ), undo_stack);

            Toybox.addChildLast(dst.main_area, blk: {
                const postit = try Toybox.new(
                    .{ .pos = .new(3, 5) },
                    .{ .postit = .{} },
                    undo_stack,
                );
                Toybox.addChildLast(postit.index, (try Toybox.new(
                    .{ .pos = .new(0, 0) },
                    .{ .postit_text = .{ .text = "hi" } },
                    undo_stack,
                )).index, undo_stack);

                break :blk postit.index;
            }, undo_stack);
        }

        if (false) {
            Toybox.addChildLast(dst.lenses_layer, try Toybox.buildMicroscope(
                .new(2, 2),
                .new(4, 3),
                undo_stack,
            ), undo_stack);

            Toybox.addChildLast(dst.lenses_layer, try Toybox.buildMicroscope(
                .new(4, 3),
                .new(6, 2),
                undo_stack,
            ), undo_stack);
        }

        if (true) { // add levels
            const core = @import("core.zig");
            var pool: std.heap.MemoryPool(core.Sexpr) = .init(gpa);
            defer pool.deinit();
            var scratch: std.heap.ArenaAllocator = .init(gpa);
            defer scratch.deinit();
            const levels = @import("levels_new.zig").levels;
            var x: f32 = 100;
            const Sexpr = Lego.Specific.Sexpr;
            for (levels, 0..) |level, k| {
                defer _ = scratch.reset(.retain_capacity);
                const samples: []const [2]Lego.Index = blk: {
                    var samples_it = level.samplesIterator();
                    var samples: std.ArrayListUnmanaged([2]Lego.Index) = .empty;
                    while (try samples_it.next(&pool, scratch.allocator())) |item| {
                        try samples.append(scratch.allocator(), .{
                            try Sexpr.buildFromOldCoreValue(.{}, item.input, false, false, undo_stack),
                            try Sexpr.buildFromOldCoreValue(.{}, item.expected, false, false, undo_stack),
                        });
                        _ = pool.reset(.retain_capacity);
                    }
                    break :blk try samples.toOwnedSlice(scratch.allocator());
                };

                const fnkbox =
                    try Toybox.buildFnkbox(
                        .{ .pos = .new(x, if (k % 2 == 0) -6 else -5) },
                        try Sexpr.buildFromOldCoreValue(.{}, level.fnk_name, true, true, undo_stack),
                        level.description,
                        samples,
                        if (level.initial_definition) |definition|
                            try Lego.Specific.Garland.buildFromOldCoreValue(.{}, definition, scratch.allocator(), undo_stack)
                        else
                            null,
                        undo_stack,
                    );
                Toybox.addChildLast(
                    dst.fnkboxes_layer,
                    fnkbox,
                    undo_stack,
                );

                if (k == 0) {
                    Lego.Specific.Executor.children(Lego.Specific.Fnkbox.children(fnkbox).executor).controls.get().specific.executor_controls.brake().get().specific.executor_brake.brake_t = 0.9;
                    Lego.Specific.FnkboxBox.children(Lego.Specific.Fnkbox.children(fnkbox).box).testcases_scrollbar.get().specific.scrollbar.scroll_target = 2;
                    Lego.Specific.FnkboxBox.children(Lego.Specific.Fnkbox.children(fnkbox).box).testcases_scrollbar.get().specific.scrollbar.scroll_visual = 2;
                }
                x += if (k < 4) 25 else if (k < 6) 30 else 35;

                fnkslist.get().specific.fnkslist.scrollbar.get().specific.scrollbar.total_length += 1;
                Toybox.addChildLast(fnkslist, try Lego.Specific.FnkslistElement.build(
                    k,
                    level.fnk_name,
                    // level.fnk_name.atom_lit.value,
                    level.description,
                    undo_stack,
                ), undo_stack);
            }
        }

        if (true) { // tutorial postits
            var postit_pos: Vec2 = .new(40, -3);
            dst.main_area.get().local_point = Point.inverseApplyGetLocal(.{ .pos = postit_pos.add(.new(13, 8)), .scale = 4.5 * 2.75 }, .{});

            const postit: Lego.Specific.Postit.Helper = .{ .main_area = dst.main_area, .undo_stack = undo_stack };

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
                undo_stack,
            ), undo_stack);
            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = postit_pos.add(.new(5, -1.5)) },
                .{ .atom_lit = "b" },
                true,
                false,
                undo_stack,
            ), undo_stack);
            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = postit_pos.add(.new(-2, 4)) },
                .{ .atom_lit = "C" },
                false,
                false,
                undo_stack,
            ), undo_stack);
            postit_pos.addInPlace(.new(5.5, 5.5));
            postit.addFromText(postit_pos, &.{ "Right click to", "duplicate them" });
            postit.addFromText(postit_pos.add(.new(6.5, 0.7)), &.{"Z to undo"});

            postit_pos.addInPlace(.new(19, -14));
            postit.addFromText(postit_pos, &.{ "Your job:", "make machines", "that transform", "Atoms into", "other Atoms" });
            if (false) {
                postit_pos.addInPlace(.new(7, 0));
                postit.addFromText(postit_pos, &.{ "The piece below", "(when active)", "will match with", "the atom 'a'", "and transform it", "into 'b'" });
                Toybox.addChildLast(dst.main_area, Toybox.buildCase(.{ .pos = postit_pos.addY(5) }, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false),
                    .fnkname = null,
                    .next = null,
                }));
            }
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromText(postit_pos, &.{ "The machine", "below, made of", "two pieces,", "will turn", "'a' into 'b',", "and 'b' into 'a'" });
            Toybox.addChildLast(dst.main_area, try Toybox.buildGarland(.{ .pos = postit_pos.addY(5) }, &.{
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, true, false, undo_stack),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false, false, undo_stack),
                    .fnkname = null,
                    .next = null,
                }, undo_stack),
                try Toybox.buildCase(.{}, .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, true, false, undo_stack),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false, false, undo_stack),
                    .fnkname = null,
                    .next = null,
                }, undo_stack),
            }, undo_stack), undo_stack);
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
            Toybox.addChildLast(dst.main_area, try Lego.Specific.Sexpr.buildFromOldCoreValue(
                .{ .pos = postit_pos.add(.new(4, -2.5)), .scale = 0.5, .turns = 0.25 },
                @import("levels_new.zig").levels[0].fnk_name,
                false,
                true,
                undo_stack,
            ), undo_stack);
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
            postit_pos.addInPlace(.new(35, 0));
            postit.addFromText(postit_pos, &.{ "You can combine", "both tricks:", "invoke a machine", "and then match", "on its result" });
            postit.addFromText(postit_pos.addX(7), &.{ "Study this", "solved", "assignment", "in detail." });
            postit_pos.addInPlace(.new(35, 0));
            postit.addFromText(postit_pos, &.{ "You now know", "everything!" });
            postit.addFromText(postit_pos.addX(7), &.{"Good luck."});

            postit_pos.addInPlace(.new(35, 0));
            postit_pos.addInPlace(.new(35, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "DEBUG:", "skip this one" });
            postit_pos.addInPlace(.new(35, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "DEBUG:", "skip this one" });

            postit_pos.addInPlace(.new(35 * 6, 0));
            postit_pos.addInPlace(.new(-7, -7));
            postit.addFromText(postit_pos.addX(7), &.{ "The first example", "is an empty list." });
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "The second one", "is a list with only", "one element, 'a'." });
            postit_pos.addInPlace(.new(-7, 7));
            postit.addFromText(postit_pos.addX(7), &.{ "The third example", "is the list ['a', 'b']" });
            postit_pos.addInPlace(.new(7, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "The next one", "is ['a', 'b', 'c']" });

            postit_pos.addInPlace(.new(35 * 2, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "DEBUG:", "skip this one" });
            postit_pos.addInPlace(.new(35, 0));
            postit.addFromText(postit_pos.addX(7), &.{ "DEBUG:", "skip this one" });
        }

        var arena: std.heap.ArenaAllocator = .init(gpa);
        defer arena.deinit();
        try dst.canonizeAfterChanges(arena.allocator());
    }

    pub fn canonizeAfterChanges(workspace: *Workspace, scratch: std.mem.Allocator) !void {
        const zone = tracy.initZone(@src(), .{ .name = "canonize after changes" });
        defer zone.deinit();

        for (toybox.all_legos.items) |*lego| {
            if (!lego.exists) continue;
            if (lego.specific.tag() == .fnkbox) {
                try lego.specific.fnkbox.updateStatus(workspace, scratch);
            }
        }
    }

    pub fn deinit(workspace: *Workspace) void {
        workspace.undo_stack.deinit();
        workspace.arena_for_atom_names.deinit();
        workspace.arena_for_lenses_data.deinit();
    }

    const HotAndDropzone = struct {
        hot: Lego.Index = .nothing,
        dropzone: Lego.Index = .nothing,
        over_background: Lego.Index,

        pub fn empty(x: @This()) bool {
            return x.hot == .nothing and x.dropzone == .nothing;
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
                const relative_needle_pos = lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos);
                if (lego.unhoverable and !step.children_already_visited) {
                    it.skipChildren();
                    _ = it.next();
                    continue;
                }
                assert(!lego.unhoverable);

                // TODO(optim): check that this works
                const local_bounds = lego.localBoundingBoxThatContainsSelfAndAllChildren();
                const absolute_bounds = lego.absolute_point.applyToLocalBounds(local_bounds);
                if (!absolute_bounds.contains(absolute_needle_pos)) {
                    it.skipChildren();
                    _ = it.next();
                    continue;
                }

                switch (lego.specific) {
                    .sexpr => |sexpr| {
                        if (!step.children_already_visited and
                            Lego.Specific.Sexpr.contains(lego.absolute_point, sexpr.is_pattern, sexpr.kind, absolute_needle_pos))
                        {
                            if (grabbing == .nothing and sexpr.kind != .empty) {
                                return .{ .hot = cur, .over_background = root };
                            } else if (grabbing != .nothing and !sexpr.immutable and Toybox.get(grabbing).specific.tag() == .sexpr) {
                                return .{ .dropzone = cur, .over_background = root };
                            }
                        }
                    },
                    .lens => |lens| {
                        if (step.children_already_visited and
                            lens.is_target and
                            lego.absolute_point.inRange(absolute_needle_pos, lens.local_radius))
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
                            area.bg.contains(lego.absolute_point, absolute_needle_pos) and
                            (grabbing == .nothing or Toybox.get(grabbing).tree.parent == .nothing or Toybox.isAncestor(cur, grabbing)))
                        {
                            return .{ .over_background = cur };
                        }
                    },
                    .button => |button| {
                        if (step.children_already_visited and
                            button.enabled and
                            grabbing == .nothing and
                            button.local_rect.contains(lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos)))
                        {
                            return .{ .hot = cur, .over_background = root };
                        }
                    },
                    .scrollbar => |scrollbar| {
                        if (step.children_already_visited and
                            grabbing == .nothing and
                            scrollbar.handleRectVisual().contains(lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos)))
                        {
                            return .{ .hot = cur, .over_background = root };
                        }
                    },
                    .postit => {
                        if (!step.children_already_visited and
                            grabbing == .nothing and
                            Lego.Specific.Postit.local_rect.contains(relative_needle_pos))
                        {
                            return .{ .hot = cur, .over_background = root };
                        }
                        if (!step.children_already_visited) {
                            it.skipChildren();
                        }
                    },
                    .fnkbox_testcases => {
                        if (!step.children_already_visited and !Lego.Specific.FnkboxBox.testcases_box.contains(
                            lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos),
                        )) {
                            it.skipChildren();
                        }
                    },
                    .case,
                    .newcase,
                    .microscope,
                    .garland,
                    .garland_newcases,
                    .executor,
                    .fnkbox,
                    .fnkbox_box,
                    .fnkbox_description,
                    .fnkslist,
                    .fnkslist_element,
                    .testcase,
                    .pill,
                    .postit_text,
                    .postit_drawing,
                    .executor_controls,
                    .executor_brake,
                    .executor_crank,
                    => {},
                }
                if (step.children_already_visited) {
                    if (lego.handle()) |handle| {
                        const overlappable: bool, const kind: enum { hot, drop } = switch (lego.specific) {
                            .sexpr,
                            .area,
                            .microscope,
                            .fnkbox_description,
                            .fnkbox_testcases,
                            .fnkslist,
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
                            => unreachable,
                            .case, .lens, .fnkbox, .executor_brake, .executor_crank => .{ grabbing == .nothing, .hot },
                            .newcase => .{ grabbing != .nothing and Toybox.get(grabbing).specific.tag() == .case, .drop },
                            .garland => if (grabbing == .nothing)
                                .{ true, .hot }
                            else if (Toybox.get(grabbing).specific.tag() == .garland)
                                .{ true, .drop }
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

        if (grabbing != .nothing) {
            assert(grabbing.get().tree.parent != .nothing);
            return .{ .over_background = Toybox.oldestAncestor(grabbing) };
        } else {
            unreachable;
        }
    }

    fn dragGrabbing(grabbing: Grabbing, absolute_mouse_pos: Vec2, interaction: HotAndDropzone, delta_seconds: f32) void {
        if (grabbing.index == .nothing) return;
        const cur = grabbing.index;
        const lego = Toybox.get(cur);
        if (lego.draggable()) {
            switch (lego.specific) {
                .sexpr => |sexpr| {
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

                    lego.local_point.lerp_towards(Toybox.parentAbsolutePoint(cur)
                        .inverseApplyGetLocal(target), 0.6, delta_seconds);

                    if (Toybox.safeGet(interaction.dropzone)) |dropzone| {
                        const dropzone_is_pattern = dropzone.specific.sexpr.is_pattern;
                        if (dropzone_is_pattern != sexpr.is_pattern) {
                            Lego.Specific.Sexpr.setIsPattern(cur, dropzone_is_pattern);
                        }

                        const dropzone_is_fnkname = dropzone.specific.sexpr.is_fnkname;
                        if (dropzone_is_fnkname != sexpr.is_fnkname) {
                            var cur_sexpr = cur;
                            while (cur_sexpr != .nothing) : (cur_sexpr = Toybox.next_preordered(cur_sexpr, cur).next) {
                                Toybox.get(cur_sexpr).specific.sexpr.is_fnkname = dropzone_is_fnkname;
                                var cur_child = Toybox.get(cur_sexpr).specific.sexpr.emerging_value;
                                while (cur_child != .nothing) : (cur_child = Toybox.next_preordered(cur_child, cur_sexpr).next) {
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

                    lego.local_point.lerp_towards(Toybox.parentAbsolutePoint(cur)
                        .inverseApplyGetLocal(target), 0.6, delta_seconds);
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
                    assert(interaction.dropzone == .nothing);
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
                    assert(interaction.dropzone == .nothing);
                    const local_pos = lego.absolute_point.inverseApplyGetLocalPosition(absolute_mouse_pos);
                    const raw_t = local_pos.getTurns();
                    const executor = &Toybox.findAncestor(cur, .executor).get().specific.executor;
                    const cur_t = executor.animation.?.t;
                    const target_t = math.clamp01(math.mod(raw_t, cur_t - 0.5, cur_t + 0.5));
                    // math.lerp_towards(&crank.t, @max(0, target_t), 0.6, delta_seconds);
                    math.towards(&crank.value, target_t, delta_seconds * 5);
                    executor.animation.?.t = crank.value;
                },
            }
            Toybox.refreshAbsolutePoints(&.{grabbing.index});
        }
    }

    fn updateSprings(workspace: *Workspace, roots_in_draw_order: []const Lego.Index, absolute_mouse_pos: Vec2, interaction: HotAndDropzone, delta_seconds: f32) void {
        const asdf = tracy.initZone(@src(), .{ .name = "updateSprings" });
        defer asdf.deinit();

        for (roots_in_draw_order) |root| {
            var cur: Lego.Index = root;
            while (cur != .nothing) : (cur = Toybox.next_preordered(cur, root).next) {
                const lego = Toybox.get(cur);
                defer lego.absolute_point = Toybox.parentAbsolutePoint(cur).applyToLocalPoint(lego.local_point);

                // TODO(design): remove this from here
                lego.unhoverable = switch (lego.specific) {
                    .sexpr, .case, .garland => if (lego.tree.parent == .nothing) false else switch (Toybox.get(lego.tree.parent).specific) {
                        .executor => |executor| if (Toybox.safeGet(Toybox.findAncestor(cur, .fnkbox))) |fnkbox|
                            fnkbox.specific.fnkbox.execution != null
                        else
                            executor.animation != null,
                        else => false,
                    },
                    else => false,
                };

                switch (lego.specific) {
                    .sexpr => |*sexpr| {

                        // TODO(optim): skip children in most cases

                        const zone = tracy.initZone(@src(), .{ .name = "updateSprings - sexpr" });
                        defer zone.deinit();

                        sexpr.immutable = if (lego.tree.parent == .nothing) false else switch (Toybox.get(lego.tree.parent).specific) {
                            else => false,
                            .sexpr => |parent_sexpr| parent_sexpr.immutable,
                            .garland, .fnkbox, .testcase, .fnkslist_element => true,
                        };

                        if (sexpr.emerging_value != .nothing and sexpr.executor_with_bindings != .nothing) {
                            const bindings = Lego.Specific.Executor.bindingsActive(sexpr.executor_with_bindings);
                            const t: f32 = if (bindings.anim_t) |anim_t| math.smoothstep(anim_t, 0, 0.4) else 0;
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
                            if (!sexpr.emerging_value_ignore_updates_to_t) Lego.Specific.Sexpr.setEmergingValueT(cur, t);
                            Toybox.get(sexpr.emerging_value).local_point = lego.absolute_point.applyToLocalPoint(offset);
                            updateSprings(workspace, &.{sexpr.emerging_value}, absolute_mouse_pos, interaction, delta_seconds);
                        }
                    },
                    .case => {
                        // TODO(optim): this is needed since undoing a half-done anim doesn't properly restore all the local positions of the case parts
                        Lego.Specific.Case.updateLocalPositions(cur);
                    },
                    .garland_newcases => {
                        const zone = tracy.initZone(@src(), .{ .name = "updateSprings - garland" });
                        defer zone.deinit();

                        var a = Toybox.get(lego.tree.first);
                        var offset: f32 = 0;
                        while (true) {
                            assert(a.specific.tag() == .newcase);
                            a.local_point = .{ .pos = .new(0, offset) };
                            offset += a.specific.newcase.length();

                            if (a.tree.next == .nothing) break;
                            a = Toybox.get(a.tree.next);
                        }
                        lego.tree.parent.get().specific.garland.computed_height = offset;
                    },
                    .newcase => |*newcase| {
                        const zone = tracy.initZone(@src(), .{ .name = "updateSprings - newcase" });
                        defer zone.deinit();

                        const Garland = Lego.Specific.Garland;

                        const is_first = lego.tree.prev == .nothing;
                        const is_last = lego.tree.next == .nothing;

                        const extra_before_offset_for_anim: f32 = if (newcase.offset_ghost == .nothing)
                            0
                        else
                            newcase.offset_t * (Lego.Specific.Garland.dist_between_cases_rest * 0.5 +
                                newcase.offset_ghost.get().specific.case.next().computed_height);

                        const extra_after_offset_for_anim: f32 = if (newcase.offset_ghost == .nothing or !is_first)
                            0
                        else
                            0.5 * newcase.offset_t * (Garland.dist_between_cases_rest - Garland.dist_between_cases_first);

                        const maybe_child_case: Lego.Index = if (lego.tree.first != .nothing) blk: {
                            assert(lego.tree.last == lego.tree.first);
                            assert(lego.tree.next != .nothing);
                            assert(Toybox.get(lego.tree.first).specific.tag() == .case);
                            break :blk lego.tree.first;
                        } else blk: {
                            assert(lego.tree.next == .nothing);
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
                        const zone = tracy.initZone(@src(), .{ .name = "updateSprings - executor" });
                        defer zone.deinit();

                        const Executor = Lego.Specific.Executor;
                        const children = Executor.children(cur);

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
                                    while (enqueued != .nothing) : ({
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
                                const discarded_t = anim_t;
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

                                if (animation.invoked_fnk != .nothing) {
                                    const offset = (1.0 - invoking_t) + 2.0 * math.smoothstepEased(invoking_t, 0.4, 0.0, .linear);
                                    const function_point = lego.absolute_point.applyToLocalPoint(Lego.Specific.Executor.relative_garland_point)
                                        .applyToLocalPoint(.{ .pos = .new(2 * offset + 6 - match_t - enqueueing_t * 5, 6 * offset) });

                                    Toybox.setAbsolutePoint(animation.invoked_fnk, function_point);

                                    Toybox.get(animation.active_case).specific.case.next_point_extra = Lego.Specific.Garland.extraForEnqueuingNext(enqueueing_t);
                                    Toybox.get(animation.active_case).specific.case.fnk_name_extra = .{ .pos = .new(-invoking_t * 4, 0) };
                                    Toybox.setAbsolutePoint(animation.active_case, lego.absolute_point.applyToLocalPoint(case_point));

                                    const enqueueing = animation.active_case.case().next.garland().hasChildCases();
                                    if (true) { // update enqueued garlands
                                        var enqueued = executor.first_enqueued;
                                        var k: usize = 0;
                                        while (enqueued != .nothing) : ({
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
                                    Toybox.get(animation.active_case).specific.case.fnk_name_extra = .{ .pos = .new(-invoking_t * 4, 0) };

                                    const dequeueing = !animation.active_case.case().next.garland().hasChildCases();
                                    if (true) { // update enqueued garlands
                                        var enqueued = executor.first_enqueued;
                                        var k: usize = 0;
                                        while (enqueued != .nothing) : ({
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
                            while (pill != .nothing) : ({
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
                        const zone = tracy.initZone(@src(), .{ .name = "updateSprings - executor_brake" });
                        defer zone.deinit();

                        lego.local_point = .{};
                        brake.handle_pos = Lego.Specific.Executor.Controls.brakeHandlePath(brake.brake_t);
                    },
                    .executor_crank => |*crank| {
                        lego.local_point = .{};
                        crank.handle_pos = .fromPolar(0.75, crank.value);
                    },
                    .fnkbox_testcases => |fnkbox_testcases| {
                        const zone = tracy.initZone(@src(), .{ .name = "updateSprings - fnkbox_testcases" });
                        defer zone.deinit();

                        const scroll_visual = fnkbox_testcases.scrollbar.get().specific.scrollbar.scroll_visual;

                        var k: usize = 0;
                        var cur_case: Lego.Index = lego.tree.first;
                        while (cur_case != .nothing) {
                            Toybox.get(cur_case).local_point = .{ .pos = Lego.Specific.FnkboxBox.relative_top_testcase_pos
                                .addY(2 + 2.5 * (tof32(k) - scroll_visual)) };
                            k += 1;
                            cur_case = Toybox.get(cur_case).tree.next;
                        }
                    },
                    .fnkslist => |fnkslist| {
                        const scroll_visual = fnkslist.scrollbar.get().specific.scrollbar.scroll_visual;

                        var k: usize = 0;
                        var cur_element: Lego.Index = lego.tree.first;
                        while (cur_element != .nothing) {
                            Toybox.get(cur_element).local_point = .{ .pos = .new(0, Lego.Specific.FnkslistElement.height * (tof32(k) - scroll_visual)) };
                            k += 1;
                            cur_element = Toybox.get(cur_element).tree.next;
                        }
                    },
                    .scrollbar => |*scrollbar| {
                        const zone = tracy.initZone(@src(), .{ .name = "updateSprings - scrollbar" });
                        defer zone.deinit();

                        math.lerpTowardsRange(&scrollbar.scroll_target, 0, @max(0, scrollbar.total_length - scrollbar.visible_length), .slow, delta_seconds);
                        math.lerpTowards(&scrollbar.scroll_visual, scrollbar.scroll_target, .slow, delta_seconds);
                    },
                    .garland, .fnkslist_element, .testcase, .fnkbox, .pill, .area, .microscope, .lens, .button, .fnkbox_description, .postit, .postit_text, .postit_drawing => {},
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

        try _draw(workspace.roots(.all).constSlice(), if (Toybox.safeGet(workspace.grabbing.index)) |lego|
            lego.specific.tag() == .sexpr
        else
            false, camera, drawer);

        if (display_fps) try drawer.canvas.drawText(
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

    // TODO(game): emerging values seem 1-frame delayed, can easilty be seen in the queuing anim for "@a -> x: b { c -> @a; }"
    fn _draw(roots_in_draw_order: []const Lego.Index, holding_a_sexpr: bool, camera: Rect, drawer: *Drawer) !void {
        for (roots_in_draw_order) |root| {
            var it = Toybox.treeIterator(root, true);
            while (it.next()) |step| {
                const cur = step.index;
                const lego = Toybox.get(cur);
                const alpha: f32 = if (Toybox.safeGet(Toybox.findAncestor(cur, .executor))) |g|
                    @max(0, g.specific.executor.garland_appearing_t)
                else if (Toybox.safeGet(Toybox.findAncestor(cur, .pill))) |p|
                    p.specific.pill.alpha()
                else
                    1;

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
                        .fnkbox_testcases => {
                            drawer.canvas.clipper.pop();
                            drawer.canvas.clipper.use(drawer.canvas);
                        },
                        .fnkbox_box => {
                            drawer.canvas.borderRect(camera_relative, Lego.Specific.FnkboxBox.relative_box, 0.05, .inner, .black);
                        },
                        else => {},
                    }
                } else {
                    const point = lego.absolute_point.applyToLocalPoint(lego.visual_offset);
                    switch (lego.specific) {
                        .case => {
                            // TODO(game): draw variables in the cable
                            drawer.canvas.line(camera, &.{
                                lego.absolute_point.applyToLocalPosition(.xneg),
                                lego.absolute_point.applyToLocalPosition(.xpos),
                            }, 0.05 * lego.absolute_point.scale, .blackAlpha(alpha));
                            const next_garland = Lego.Specific.Case.children(cur).next;
                            if (next_garland.get().specific.garland.visible) {
                                // TODO(game): draw variables in the cable
                                drawer.canvas.line(camera, &.{
                                    lego.absolute_point.applyToLocalPosition(.new(1.5, 1)),
                                    next_garland.get().absolute_point.pos,
                                }, 0.05 * lego.absolute_point.scale, .blackAlpha(alpha));
                            }
                        },
                        .sexpr => |sexpr| {
                            if (sexpr.emerging_value != .nothing) {
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
                                        try _draw(&.{sexpr.emerging_value}, holding_a_sexpr, camera, drawer);
                                    } else |_| {
                                        std.log.err("reached max lens depth, TODO(polish): improve", .{});
                                    }
                                }
                            }

                            const maybe_bindings: ?BindingsState = if (sexpr.executor_with_bindings != .nothing)
                                Lego.Specific.Executor.bindingsActive(sexpr.executor_with_bindings)
                            else
                                null;

                            switch (sexpr.kind) {
                                .empty => if (lego.tree.parent.get().specific.tag() != .sexpr and
                                    (holding_a_sexpr or !sexpr.is_fnkname) and
                                    // Don't draw empty garland fnknames
                                    !(sexpr.is_fnkname and sexpr.is_pattern))
                                {
                                    try drawer.drawPlaceholder(camera, point, sexpr.is_pattern, alpha);
                                },
                                .atom_lit => try drawer.drawAtom(camera, point, sexpr.is_pattern, sexpr.atom_name, alpha),
                                .pair => try drawer.drawPairHolder(camera, point, sexpr.is_pattern, alpha),
                                .atom_var => if (!sexpr.emerging_value_ignore_updates_to_t) {
                                    const extra_alpha: f32 = if (maybe_bindings) |bindings| blk: {
                                        if (bindings.anim_t) |t| {
                                            break :blk for (bindings.new) |binding| {
                                                if (std.mem.eql(u8, binding.name, sexpr.atom_name)) break :blk (1.0 - t);
                                            } else 1;
                                        } else break :blk 1;
                                    } else 1;
                                    try drawer.drawVariable(camera, point, sexpr.is_pattern, sexpr.atom_name, alpha * extra_alpha);
                                },
                            }

                            if (sexpr.kind == .pair) {
                                const names = try Lego.Specific.Sexpr.getAllVarNamesHelper(lego.index, drawer.canvas.frame_arena.allocator());
                                try if (sexpr.is_pattern)
                                    drawer.drawPatternWildcardLinesNonRecursiveV2(camera, names.left, names.right, point, alpha)
                                else
                                    drawer.drawTemplateWildcardLinesNonRecursiveV2(camera, names.left, names.right, point, maybe_bindings orelse .{
                                        .anim_t = null,
                                        .old = &.{},
                                        .new = &.{},
                                    }, alpha);
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

                                    try _draw(lens.roots_to_draw, holding_a_sexpr, lens.transform.getCamera(camera), drawer);
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
                        .area => |area| {
                            switch (area.bg) {
                                // TODO(game): .all background
                                .all, .none => {},
                                .local_rect => |rect| {
                                    drawer.canvas.fillRect(camera, lego.absolute_point.applyToLocalRect(rect), .gray(0.4));
                                },
                            }
                        },
                        .postit => {
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
                            try drawer.canvas.drawText(0, camera_relative, postit_text.text, .centeredAt(.zero), 0.8, .black);
                        },
                        .postit_drawing => |kind| {
                            switch (kind) {
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
                                .see_failing_testcase => {
                                    if (button.enabled) {
                                        drawer.canvas.rectGradient(
                                            camera_relative,
                                            button.local_rect,
                                            .gray(0.75 + lego.hot_t * 0.2 - lego.active_t * 0.1),
                                            .gray(0.95 - lego.hot_t * 0.2 - lego.active_t * 0.1),
                                        );
                                        try drawer.canvas.drawText(0, camera_relative, "Unsolved!", .centeredAt(button.local_rect.getCenter()), 0.75, .black);
                                    } else {
                                        drawer.canvas.fillRect(camera_relative, button.local_rect, .gray(0.7));
                                        try drawer.canvas.drawText(0, camera_relative, "Solved!", .centeredAt(button.local_rect.getCenter()), 0.8, .black);
                                    }
                                },
                                .scroll_up, .scroll_down => {
                                    drawer.canvas.fillRect(camera_relative, button.local_rect, COLORS.bg);
                                    drawer.canvas.borderRect(camera_relative, button.local_rect, math.lerp(0.05, 0.1, @max(lego.hot_t, lego.active_t)), .inner, .black);
                                },
                            }
                        },
                        .scrollbar => |scrollbar| {
                            drawer.canvas.fillRect(camera_relative, scrollbar.handleRectVisual(), COLORS.bg);
                            drawer.canvas.borderRect(camera_relative, scrollbar.handleRectVisual(), math.lerp(0.05, 0.1, @max(lego.hot_t, lego.active_t)), .inner, .black);
                        },
                        .fnkbox_testcases => {
                            const testcases_labels_center = Lego.Specific.FnkboxBox.testcases_box.get(.top_center).addY(-Lego.Specific.FnkboxBox.testcases_header_height * 0.5).addX(0.85);
                            try drawer.canvas.drawText(0, camera_relative, "Examples:", .centeredAt(testcases_labels_center.addX(-7.15)), 0.65, .black);
                            try drawer.canvas.drawText(0, camera_relative, "Input", .centeredAt(testcases_labels_center.addX(-4)), 0.65, .black);
                            try drawer.canvas.drawText(0, camera_relative, "Target", .centeredAt(testcases_labels_center.addX(0)), 0.65, .black);
                            try drawer.canvas.drawText(0, camera_relative, "Actual", .centeredAt(testcases_labels_center.addX(4)), 0.65, .black);
                            drawer.canvas.clipper.push(.{
                                .camera = camera_relative,
                                .shape = .{
                                    .rect = Lego.Specific.FnkboxBox.testcases_box,
                                },
                            }) catch @panic("TOO DEEP");
                            drawer.canvas.clipper.use(drawer.canvas);
                        },
                        .newcase => |newcase| {
                            // TODO(design): camera_relative fails due to rotation
                            drawer.canvas.line(camera, &.{
                                lego.absolute_point.pos,
                                lego.absolute_point.applyToLocalPosition(.new(0, newcase.length())),
                            }, 0.05 * lego.absolute_point.scale, .blackAlpha(alpha));
                        },
                        .fnkbox_description => |fnkbox_description| {
                            try drawer.canvas.drawText(
                                0,
                                camera_relative,
                                fnkbox_description.text,
                                .centeredAt(.new(0, 0.75 + Lego.Specific.FnkboxBox.text_height / 2.0)),
                                0.8,
                                .black,
                            );
                        },
                        .fnkslist_element => |fnkslist_element| {
                            try drawer.canvas.drawText(
                                0,
                                camera_relative,
                                fnkslist_element.text,
                                .leftCenterAt(.new(2.1, Lego.Specific.FnkslistElement.height / 2.0)),
                                0.5,
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
                            assert(lego.tree.parent.get().specific.tag() == .fnkbox_testcases);
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
                        .fnkslist, .executor_controls, .garland_newcases, .microscope, .executor, .fnkbox, .pill => {},
                    }
                }
            }
        }
    }

    pub fn update(workspace: *Workspace, platform: PlatformGives, drawer: ?*Drawer, scratch: std.mem.Allocator) !void {
        if (platform.keyboard.wasPressed(.KeyQ)) {
            std.log.debug("-----", .{});
            for (toybox.all_legos.items, 0..) |lego, k| {
                assert(lego.index == @as(Lego.Index, @enumFromInt(k)));
                if (lego.exists) {
                    std.log.debug("{d} \t{s} \tparent: {d} \tnext: {d} \tprev: {d} \tfirst: {d} \t rel: {any} \tabs: {any}", .{
                        k,
                        @tagName(lego.specific.tag()),
                        lego.tree.parent.asI32(),
                        lego.tree.next.asI32(),
                        lego.tree.prev.asI32(),
                        lego.tree.first.asI32(),
                        lego.local_point,
                        lego.absolute_point,
                    });
                }
            }
            std.log.debug("-----", .{});
            for (workspace.undo_stack.commands.items, 0..) |cmd, k| {
                std.log.debug("{d} \t{any}", .{ k, cmd });
            }
        }

        const delta_seconds = @min(1.0 / 30.0, platform.delta_seconds * @as(f32, (if (platform.keyboard.cur.isDown(.Space)) 0.01 else 1.0)));

        const absolute_camera = Rect
            .fromCenterAndSize(.zero, .both(2))
            .withAspectRatio(platform.aspect_ratio, .grow, .center);

        const mouse = platform.getMouse(absolute_camera);

        const undo_stack = &workspace.undo_stack;
        undo_stack.startFrame();

        if (platform.keyboard.wasPressed(.KeyZ)) {
            while (undo_stack.pop()) |command| {
                switch (command) {
                    .fence => break,
                    .destroy_floating => |index| {
                        Toybox.destroyFloating(index, null);
                    },
                    .recreate_floating => |data| {
                        Toybox.recreateFloating(data);
                    },
                    .insert => |insert| {
                        Toybox.insert(insert.what, insert.where, null);
                    },
                    .set_data_except_tree => |data| {
                        const original_tree = Toybox.get(data.index).tree;
                        Toybox.get(data.index).* = data;
                        Toybox.get(data.index).tree = original_tree;
                    },
                    .pop => |index| {
                        Toybox.pop(index, null);
                    },
                    .set_grabbing => |grabbing| {
                        workspace.grabbing = grabbing;
                    },
                    .set_handlayer => |index| {
                        workspace.hand_layer = index;
                    },
                    .change_child => |change| {
                        Toybox.changeChild(change.original, change.new, null);
                    },
                }
            }
        } else { // INTERACTION
            const zone = tracy.initZone(@src(), .{ .name = "interaction" });
            defer zone.deinit();

            const hot_and_dropzone = workspace.findHotAndDropzone(mouse.cur.position);

            if (workspace.grabbing.index == .nothing and
                hot_and_dropzone.hot != .nothing and
                (mouse.wasPressed(.left) or mouse.wasPressed(.right)))
            {
                // Main case A: plucking/grabbing/clicking something
                undo_stack.append(.fence);

                const hot_index = hot_and_dropzone.hot;
                const original_hot_data = Toybox.get(hot_index).*;
                const hot_parent = original_hot_data.tree.parent;

                var grabbed_element_index: Lego.Index = undefined;
                var plucked: bool = true;

                if (mouse.wasPressed(.right) or (original_hot_data.specific.tag() == .sexpr and original_hot_data.specific.sexpr.immutable)) {
                    // Case A.0: duplicating
                    if (hot_index.get().canDuplicate()) {
                        const new_element_index = try Toybox.dupeIntoFloatingWithoutChangingPos(hot_index, true, undo_stack);
                        grabbed_element_index = new_element_index;
                    } else {
                        grabbed_element_index = .nothing;
                        plucked = undefined;
                    }
                } else if (hot_index.get().grabsWithoutPlucking()) {
                    // Case A.3: grabbing rather than plucking, including buttons
                    undo_stack.storeAllData(hot_index);
                    grabbed_element_index = hot_index;
                    plucked = false;

                    if (Toybox.get(hot_index).specific.as(.button)) |b| {
                        if (b.instant()) {
                            grabbed_element_index = .nothing;
                            @panic("unhandled instant button");
                        }
                    }
                } else if (hot_parent != .nothing and Toybox.get(hot_parent).specific.tag() == .area) {
                    // Case A.1: plucking a top-level thing
                    undo_stack.storeAllData(hot_index);
                    Toybox.popWithUndoAndChangingCoords(hot_index, undo_stack);
                    grabbed_element_index = hot_index;
                } else if (original_hot_data.specific.tag() == .sexpr) {
                    // Case A.2: plucking a nested sexpr
                    undo_stack.storeAllData(hot_index);

                    const new_empty_sexpr = try Toybox.buildSexpr(
                        original_hot_data.local_point,
                        .empty,
                        original_hot_data.specific.sexpr.is_pattern,
                        original_hot_data.specific.sexpr.is_fnkname,
                        undo_stack,
                    );

                    Toybox.changeChild(hot_index, new_empty_sexpr, undo_stack);
                    Toybox.changeCoordinates(hot_index, hot_parent.get().absolute_point, .{});
                    Toybox.refreshAbsolutePoints(&.{new_empty_sexpr});

                    grabbed_element_index = hot_index;
                } else if (hot_parent != .nothing and Toybox.get(hot_parent).specific.tag() == .newcase) {
                    // Case A.4: plucking a case from a garland
                    assert(original_hot_data.specific.tag() == .case);
                    undo_stack.storeAllData(hot_index);
                    Lego.Specific.Garland.popCase(hot_index, undo_stack);
                    grabbed_element_index = hot_index;
                } else if (Toybox.get(hot_index).specific.tag() == .garland) {
                    // Case A.5: plucking a garland, and replacing it with an empty one
                    undo_stack.storeAllData(hot_index);

                    const new_garland = try Toybox.buildGarland(original_hot_data.local_point, &.{}, undo_stack);
                    new_garland.get().specific.garland.computed_height = hot_index.get().specific.garland.computed_height;
                    Toybox.changeChild(hot_index, new_garland, undo_stack);
                    Toybox.changeCoordinates(hot_index, hot_parent.get().absolute_point, .{});
                    Toybox.refreshAbsolutePoints(&.{new_garland});

                    grabbed_element_index = hot_index;
                } else unreachable;

                assert(workspace.grabbing.index == .nothing and workspace.hand_layer == .nothing);
                if (grabbed_element_index != .nothing) {
                    workspace.setGrabbing(
                        .{ .index = grabbed_element_index, .offset = grabbed_element_index.get().getGrabbedOffset(mouse.cur.position) },
                        undo_stack,
                    );
                    if (plucked) {
                        workspace.setHandLayer(grabbed_element_index, undo_stack);
                        Toybox.refreshAbsolutePoints(&.{grabbed_element_index});
                    }
                }
            } else if (workspace.grabbing.index != .nothing and
                (!(mouse.cur.isDown(.left) or mouse.cur.isDown(.right)) or
                    workspace.grabbingSomethingIllegal()))
            {
                const dropzone_index = hot_and_dropzone.dropzone;

                if (dropzone_index != .nothing) {
                    assert(Toybox.isFloating(workspace.grabbing.index));
                    if (Toybox.get(dropzone_index).specific.tag() == .newcase) {
                        const displaced_newcase = &Toybox.get(dropzone_index).specific.newcase;
                        assert(Toybox.get(workspace.grabbing.index).specific.tag() == .case);
                        const newcase = try Toybox.new(.{}, .{ .newcase = .{
                            .length_before = Toybox.get(dropzone_index).specific.newcase.length_before,
                            .length_after = 0,
                        } }, undo_stack);
                        displaced_newcase.length_before = 0;
                        const original_tree = Toybox.get(dropzone_index).tree;
                        Toybox.insert(newcase.index, .{
                            .parent = original_tree.parent,
                            .prev = original_tree.prev,
                            .next = dropzone_index,
                            .first = .nothing,
                            .last = .nothing,
                        }, undo_stack);
                        Toybox.changeCoordinates(workspace.grabbing.index, .{}, Toybox.parentAbsolutePoint(dropzone_index));
                        Toybox.addChildLast(newcase.index, workspace.grabbing.index, undo_stack);
                    } else {
                        Toybox.changeCoordinates(workspace.grabbing.index, .{}, Toybox.parentAbsolutePoint(dropzone_index));
                        Toybox.refreshAbsolutePoints(&.{workspace.grabbing.index});
                        Toybox.changeChild(dropzone_index, workspace.grabbing.index, undo_stack);

                        Toybox.destroyFloating(dropzone_index, undo_stack);
                    }
                } else if (!Toybox.isFloating(workspace.grabbing.index)) {
                    // Case B.2: releasing a grabbed thing, which might be a button
                    assert(dropzone_index == .nothing);
                    if (Toybox.get(workspace.grabbing.index).specific.as(.button)) |button| {
                        switch (button.action) {
                            .see_failing_testcase => {
                                const fnkbox = Toybox.findAncestor(workspace.grabbing.index, .fnkbox);
                                try launchTestcase(fnkbox.get().specific.fnkbox.status.unsolved, undo_stack);
                            },
                            .launch_testcase => {
                                const testcase_index = Toybox.get(workspace.grabbing.index).tree.parent;
                                try launchTestcase(testcase_index, undo_stack);
                            },
                            .scroll_up, .scroll_down => {},
                        }
                    }
                } else {
                    // Case B.3: dropping a floating thing on fresh space
                    const target_area = hot_and_dropzone.over_background;
                    Toybox.changeCoordinates(workspace.grabbing.index, .{}, Toybox.get(target_area).absolute_point);
                    Toybox.addChildLast(target_area, workspace.grabbing.index, undo_stack);
                    Toybox.refreshAbsolutePoints(&.{workspace.grabbing.index});
                }

                workspace.setGrabbing(.{ .index = .nothing, .offset = .zero }, undo_stack);
                workspace.setHandLayer(.nothing, undo_stack);
            }
        }

        // const hovering: Lego.Index = if (workspace.focus.grabbing == .nothing) hovered_or_dropzone_thing.which else .nothing;
        // const dropzone: Lego.Index = if (workspace.focus.grabbing != .nothing) hovered_or_dropzone_thing.which else .nothing;

        // TODO(optim): avoid computing this twice?
        const hot_and_dropzone = workspace.findHotAndDropzone(mouse.cur.position);

        // cursor
        platform.setCursor(
            if (workspace.grabbing.index != .nothing)
                .grabbing // or maybe .pointer, if it's UI
            else if (hot_and_dropzone.hot != .nothing)
                .could_grab // or maybe .pointer, if it's UI
            else
                .default,
        );

        // Do this here, for .grabbingSomethingIllegal to work correctly
        dragGrabbing(workspace.grabbing, mouse.cur.position, hot_and_dropzone, delta_seconds);

        // TODO(design): improve/remove, by having this be the permanent list, and not iterating over all elements
        var things_actually_hot_etc: std.ArrayList(Lego.Index) = .init(scratch);

        if (true) { // update _t and other simple things that could be done in parallel
            const zone = tracy.initZone(@src(), .{ .name = "update _t" });
            defer zone.deinit();

            for (toybox.all_legos.items) |*lego| {
                if (!lego.exists) continue;

                var done = true;

                const eps: f32 = 0.0001;
                done = math.lerpTowardsWithFinish(&lego.hot_t, if (lego.index == hot_and_dropzone.hot) 1 else 0, .fast, delta_seconds, eps) and done;
                done = math.lerpTowardsWithFinish(&lego.active_t, if (lego.index == workspace.grabbing.index) 1 else 0, .fast, delta_seconds, eps) and done;
                done = math.lerpTowardsWithFinish(&lego.dropzone_t, if (lego.index == hot_and_dropzone.dropzone) 1 else 0, .fast, delta_seconds, eps) and done;
                done = math.lerpTowardsWithFinish(&lego.dropping_t, if (lego.index == workspace.grabbing.index and hot_and_dropzone.dropzone != .nothing) 1 else 0, .fast, delta_seconds, eps) and done;

                switch (lego.specific) {
                    .sexpr => |*sexpr| {
                        done = math.lerpTowardsWithFinish(&sexpr.is_pattern_t, if (sexpr.is_pattern) 1 else 0, .fast, delta_seconds, eps) and done;
                        done = math.lerpTowardsWithFinish(&sexpr.is_fnkname_t, if (sexpr.is_fnkname) 1 else 0, .fast, delta_seconds, eps) and done;
                    },
                    .executor => |*executor| {
                        math.towards(&executor.garland_appearing_t, 1, delta_seconds / 0.4);
                        done = done and (@abs(executor.garland_appearing_t - 1) < eps);
                    },
                    .pill,
                    .area,
                    .case,
                    .newcase,
                    .garland,
                    .garland_newcases,
                    .microscope,
                    .lens,
                    .fnkbox,
                    .fnkbox_description,
                    .fnkbox_box,
                    .fnkbox_testcases,
                    .fnkslist,
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
                    => {},
                }

                if (!done) {
                    try things_actually_hot_etc.append(lego.index);
                }
            }
        }

        // TODO(design): improve/remove
        for (things_actually_hot_etc.items) |index| {
            switch (index.get().specific) {
                else => {},
                .sexpr => Lego.Specific.Sexpr.updateLocalPositions(index),
            }
        }

        if (true) { // pills decay
            for (toybox.all_legos.items) |*lego| {
                if (!lego.exists) continue;
                switch (lego.specific) {
                    .pill => |*pill| {
                        pill.remaining_lifetime -= delta_seconds;
                        lego.local_point = lego.local_point.applyToLocalPoint(.{ .pos = pill.velocity.scale(delta_seconds) });
                        if (pill.remaining_lifetime <= 0) {
                            Toybox.pop(lego.index, undo_stack);
                            Toybox.destroyFloating(lego.index, undo_stack);
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

            const grabbing_garland_or_case: bool = if (workspace.grabbing.index == .nothing)
                false
            else switch (Toybox.get(workspace.grabbing.index).specific) {
                .case, .garland => true,
                else => false,
            };
            for (toybox.all_legos.items) |*lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.garland)) |garland| {
                    garland.visible =
                        grabbing_garland_or_case or
                        garland.hasChildCases() or
                        (if (lego.tree.parent.getSafe()) |p| p.specific.tag() == .executor else false);
                }
            }
        }

        if (true) { // move camera and scroll stuff
            const zone = tracy.initZone(@src(), .{ .name = "move camera" });
            defer zone.deinit();

            const over_scrollable_element: Lego.Index = for (toybox.all_legos.items) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.tag() == .fnkbox_testcases and Lego.Specific.FnkboxBox.testcases_box.contains(
                    lego.absolute_point.inverseApplyGetLocalPosition(mouse.cur.position),
                )) {
                    break lego.index;
                }
                if (lego.specific.tag() == .fnkslist and toolbar_fnks_rect.contains(
                    lego.absolute_point.inverseApplyGetLocalPosition(mouse.cur.position),
                )) {
                    break lego.index;
                }
            } else .nothing;

            const p = &Toybox.get(workspace.main_area).local_point;
            if (over_scrollable_element == .nothing) {
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
                    if (platform.keyboard.cur.isDown(key)) {
                        p.pos.addInPlace(kv.dir.scale(delta_seconds * -2));
                    }
                }
            }

            for (workspace.roots(.with_main_camera).constSlice()) |root| {
                if (root != workspace.main_area) {
                    Toybox.get(root).local_point = Toybox.get(workspace.main_area).local_point;
                }
            }
            Toybox.refreshAbsolutePoints(workspace.roots(.with_main_camera).constSlice());
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
                while (cur != .nothing) {
                    const original_tree = Toybox.get(cur).tree;
                    Toybox.pop(cur, undo_stack);
                    Toybox.destroyFloating(cur, undo_stack);
                    cur = original_tree.next;
                }
            } else if (old_t <= 0.01) { // regenerate children
                if (true) { // add a fresh case
                    const new_name_1 = try workspace.arena_for_atom_names.allocator().alloc(u8, 32);
                    const new_name_2 = try workspace.arena_for_atom_names.allocator().alloc(u8, 32);
                    math.Random.init(workspace.random_instance.random()).alphanumeric_bytes(new_name_1);
                    math.Random.init(workspace.random_instance.random()).alphanumeric_bytes(new_name_2);

                    const index = try Toybox.buildCase(.{ .pos = .new(2.75, 5) }, .{
                        .pattern = try Toybox.buildSexpr(.{}, .{ .pair = .{
                            .up = try Toybox.buildSexpr(.{}, .{ .atom_var = new_name_2 }, true, false, undo_stack),
                            .down = try Toybox.buildSexpr(.{}, .{ .atom_var = new_name_1 }, true, false, undo_stack),
                        } }, true, false, undo_stack),
                        .template = try Toybox.buildSexpr(.{}, .{ .pair = .{
                            .up = try Toybox.buildSexpr(.{}, .{ .atom_var = new_name_1 }, false, false, undo_stack),
                            .down = try Toybox.buildSexpr(.{}, .{ .atom_lit = "nil" }, false, false, undo_stack),
                        } }, false, false, undo_stack),
                        .fnkname = null,
                        .next = null,
                    }, undo_stack);

                    Toybox.addChildLast(workspace.toolbar_left, index, undo_stack);
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

        if (true) { // open/close fnks toolbar
            math.lerpTowards(
                &workspace.toolbar_fnks_unfolded_t,
                if (hot_and_dropzone.over_background == workspace.toolbar_fnks) 1 else 0,
                .slow,
                delta_seconds,
            );

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

            for (toybox.all_legos.items) |*lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.fnkbox)) |fnkbox| {
                    if (fnkbox.execution) |*execution| {
                        const executor_index = Lego.Specific.Fnkbox.children(lego.index).executor;
                        switch (execution.source) {
                            .testcase => |testcase| switch (execution.state) {
                                .scrolling_towards_case => {
                                    const offset_from_top: f32 = (Toybox.get(testcase).local_point.pos.y - Lego.Specific.FnkboxBox.relative_top_testcase_pos.y - 2) / 2.5;
                                    const offset_error = offset_from_top - math.clamp(offset_from_top, 0, Lego.Specific.FnkboxBox.visible_testcases - 1);
                                    if (offset_error == 0) {
                                        const new_input = try Toybox.dupeIntoFloating(Lego.Specific.Testcase.children(testcase).input, true, undo_stack);
                                        Toybox.changeCoordinates(new_input, Toybox.get(testcase).absolute_point, Toybox.get(workspace.floating_inputs_layer).absolute_point);
                                        Toybox.addChildLast(workspace.floating_inputs_layer, new_input, undo_stack);
                                        undo_stack.storeAllData(lego.index);
                                        execution.state = .starting;
                                        execution.state_t = 0;
                                        execution.original_or_final_input_point = Toybox.get(new_input).local_point;
                                        execution.floating_input_or_output = new_input;
                                    } else {
                                        const scroll = &Toybox.get(Toybox.get(testcase).tree.parent).specific.fnkbox_testcases.scrollbar.get().specific.scrollbar.scroll_target;
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
                                        Toybox.pop(input, undo_stack);
                                        Toybox.changeChild(Lego.Specific.Executor.children(executor_index).input, input, undo_stack);
                                        execution.floating_input_or_output = .nothing;
                                    }
                                },
                                .executing => {
                                    try advanceExecutorAnimation(executor_index, workspace, undo_stack, delta_seconds);
                                    if (Toybox.get(executor_index).specific.executor.animation == null) {
                                        undo_stack.storeAllData(lego.index);
                                        execution.state = .ending;
                                        execution.state_t = 0;

                                        const result = try workspace.resetExecutorAndExtractResult(executor_index, execution.original_garland);
                                        Toybox.changeCoordinates(
                                            result,
                                            .{},
                                            Toybox.get(workspace.floating_inputs_layer).absolute_point,
                                        );
                                        Toybox.addChildLast(workspace.floating_inputs_layer, result, undo_stack);
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
                                            Toybox.get(
                                                Lego.Specific.Testcase.children(testcase).actual,
                                            ).absolute_point,
                                        ),
                                        execution.state_t,
                                    );

                                    if (true) { // focus on the testcase
                                        const offset_from_top: f32 = (Toybox.get(testcase).local_point.pos.y - Lego.Specific.FnkboxBox.relative_top_testcase_pos.y - 2) / 2.5;
                                        const offset_error = offset_from_top - math.clamp(offset_from_top, 0, Lego.Specific.FnkboxBox.visible_testcases - 1);
                                        const scroll = &Toybox.get(Toybox.get(testcase).tree.parent).specific.fnkbox_testcases.scrollbar.get().specific.scrollbar.scroll_target;
                                        const target_scroll = scroll.* + offset_error;
                                        math.lerpTowards(scroll, target_scroll, .{ .duration = 0.5, .precision = 0.05 }, delta_seconds);
                                        math.towards(scroll, target_scroll, 0.1 * delta_seconds);
                                    }

                                    if (execution.state_t >= 1) {
                                        const new_actual = final_result;
                                        Toybox.changeCoordinates(new_actual, Toybox.parentAbsolutePoint(final_result), Toybox.get(testcase).absolute_point);
                                        Toybox.pop(new_actual, undo_stack);

                                        const old_actual = Lego.Specific.Testcase.children(testcase).actual;
                                        assert(Toybox.get(old_actual).specific.sexpr.kind == .empty);
                                        Toybox.changeChild(old_actual, new_actual, undo_stack);
                                        undo_stack.storeAllData(lego.index);
                                        fnkbox.execution = null;
                                        Toybox.refreshAbsolutePoints(&.{new_actual});

                                        // TODO(optim): call this somewhere else
                                        try fnkbox.updateStatus(workspace, scratch);
                                    }
                                },
                            },
                            .input => {
                                try advanceExecutorAnimation(executor_index, workspace, &workspace.undo_stack, delta_seconds);
                                if (Toybox.get(executor_index).specific.executor.animation == null) {
                                    const result = try workspace.resetExecutorAndExtractResult(executor_index, execution.original_garland);
                                    undo_stack.storeAllData(lego.index);
                                    fnkbox.execution = null;
                                    Toybox.addChildLast(workspace.main_area, result, undo_stack);
                                    Toybox.changeCoordinates(result, .{}, workspace.main_area.get().absolute_point);
                                }
                            },
                        }
                    } else {
                        const executor_index = Lego.Specific.Fnkbox.children(lego.index).executor;
                        if (Lego.Specific.Executor.shouldStartExecution(executor_index)) {
                            const original_garland_index = Lego.Specific.Executor.children(executor_index).garland;
                            const backup_garland_index = try Toybox.dupeIntoFloating(original_garland_index, true, undo_stack);

                            undo_stack.storeAllData(lego.index);
                            fnkbox.execution = .{
                                .source = .input,
                                .original_garland = backup_garland_index,
                                .original_or_final_input_point = undefined,
                                .state_t = undefined,
                                .old_testcase_actual_value = undefined,
                                .state = .executing,
                            };

                            // TODO(game)
                            // assert(fnkbox.executor.garland.fnkname == null);
                            // if (fnkbox.folded) fnkbox.executor.garland.fnkname = try fnkbox.fnkname.clone(&workspace.hover_pool);
                            // try workspace.undo_stack.append(.{ .specific = .{ .started_execution_fnkbox_from_input = .{
                            //     .fnkbox = k,
                            //     .input = try fnkbox.executor.input.clone(&mem.hover_pool),
                            // } } });
                            // try workspace.canonizeAfterChanges(mem);
                        }
                    }
                }
            }
        }

        if (true) { // start and advance executors animations
            const zone = tracy.initZone(@src(), .{ .name = "executors animations" });
            defer zone.deinit();

            for (toybox.all_legos.items) |*lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.executor)) |executor| {
                    if (executor.controlled_by_parent_fnkbox) continue;

                    if (executor.animation) |*animation| {
                        _ = animation;
                        @panic("TODO(game)");
                    }

                    if (Lego.Specific.Executor.shouldStartExecution(lego.index)) {
                        @panic("TODO(game)");
                    }
                }
            }
        }

        if (true) { // enable/disable buttons and other things
            const zone = tracy.initZone(@src(), .{ .name = "enable/disable buttons" });
            defer zone.deinit();

            for (toybox.all_legos.items) |*lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.button)) |button| {
                    button.enabled = switch (button.action) {
                        .launch_testcase => Toybox.get(Toybox.findAncestor(lego.index, .fnkbox)).specific.fnkbox.execution == null,
                        .see_failing_testcase => Toybox.get(Toybox.findAncestor(lego.index, .fnkbox)).specific.fnkbox.status == .unsolved,
                        .scroll_up, .scroll_down => true,
                    };
                }
                if (lego.specific.as(.executor_crank)) |crank| {
                    crank.enabled = Toybox.findAncestor(lego.index, .executor).get().specific.executor.animation != null;
                }
            }
        }

        const something_happened = undo_stack.anyChangesThisFrame();
        if (something_happened) {
            try workspace.canonizeAfterChanges(scratch);
        }

        // There should be no further changes
        undo_stack.startFrame();
        defer assert(!undo_stack.anyChangesThisFrame());

        if (true) { // reset per-frame variables
            const zone = tracy.initZone(@src(), .{ .name = "reset per-frame variables" });
            defer zone.deinit();

            // TODO(optim): there must be better ways to do this
            for (toybox.all_legos.items) |*lego| {
                if (!lego.exists) continue;
                if (lego.specific.as(.case)) |case| {
                    case.next_point_extra = .{};
                    case.fnk_name_extra = .{};
                }
                if (lego.specific.as(.newcase)) |newcase| {
                    newcase.offset_ghost = .nothing;
                }
            }
        }

        // doesn't include dragging and snapping to dropzone, despite that being just the spring between the mouse cursor/dropzone and the grabbed thing
        workspace.updateSprings(workspace.roots(.all).constSlice(), mouse.cur.position, hot_and_dropzone, delta_seconds);

        if (true) Toybox.refreshAbsolutePoints(workspace.roots(.all).constSlice());

        if (true) { // set lenses data
            const zone = tracy.initZone(@src(), .{ .name = "set lenses data" });
            defer zone.deinit();

            _ = workspace.arena_for_lenses_data.reset(.retain_capacity);
            const allocator = workspace.arena_for_lenses_data.allocator();
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

        if (drawer) |d| {
            try workspace.draw(platform, d);
        }
    }

    fn setGrabbing(workspace: *Workspace, grabbing: Grabbing, undo_stack: ?*UndoStack) void {
        if (undo_stack) |s| {
            s.append(.{ .set_grabbing = workspace.grabbing });
        }
        workspace.grabbing = grabbing;
    }

    fn setHandLayer(workspace: *Workspace, index: Lego.Index, undo_stack: ?*UndoStack) void {
        if (undo_stack) |s| {
            s.append(.{ .set_handlayer = workspace.hand_layer });
        }
        workspace.hand_layer = index;
    }

    fn advanceExecutorAnimation(executor_index: Lego.Index, workspace: *Workspace, undo_stack: *UndoStack, delta_seconds: f32) !void {
        const floating_inputs_layer = workspace.floating_inputs_layer;
        const Executor = Lego.Specific.Executor;
        const executor = &Toybox.get(executor_index).specific.executor;
        if (executor.animation) |*animation| {
            undo_stack.storeAllData(executor_index);
            animation.t += delta_seconds * Executor.Controls.speedScale(Executor.getBrakeT(executor_index));
            if (animation.t >= 1) {
                Toybox.popWithUndoAndChangingCoords(animation.garland_fnkname, undo_stack);
                if (animation.matching) {
                    if (true) { // fill variables
                        var cur = animation.active_case;
                        while (cur != .nothing) {
                            const next = Toybox.next_preordered(cur, animation.active_case).next;
                            defer cur = next;
                            if (Toybox.get(cur).specific.as(.sexpr)) |sexpr| {
                                if (sexpr.emerging_value != .nothing) {
                                    if (sexpr.is_pattern) {
                                        sexpr.emerging_value_ignore_updates_to_t = true;
                                    } else {
                                        Toybox.changeChildWithUndoAndAlsoCoords(cur, sexpr.emerging_value, &workspace.undo_stack);
                                    }
                                }
                            }
                        }
                    }

                    const old_case_parts = Lego.Specific.Case.destroyForParts(animation.active_case, undo_stack);
                    const old_input = Executor.children(executor_index).input;
                    const old_garland = Executor.children(executor_index).garland;
                    const next_garland = old_case_parts.next;

                    Toybox.changeChildWithUndoAndAlsoCoords(
                        old_input,
                        old_case_parts.template,
                        undo_stack,
                    );

                    const new_garland = blk: {
                        if (animation.invoked_fnk != .nothing) {
                            Toybox.pop(animation.invoked_fnk, undo_stack);
                            if (next_garland != .nothing) {
                                // TODO(game)
                                // Toybox.get(next_garland).specific.garland.enqueued_parent_pill_index = ??;
                                undo_stack.storeAllData(next_garland);
                                Toybox.get(next_garland).specific.garland.next_enqueued = executor.first_enqueued;
                                undo_stack.storeAllData(executor_index);
                                executor.first_enqueued = next_garland;
                                Toybox.addChildLastWithoutChangingAbsPoint(floating_inputs_layer, next_garland, undo_stack);
                            }
                            break :blk animation.invoked_fnk;
                        } else if (next_garland.garland().hasChildCases()) {
                            // TODO(game)
                            // parent_pill_index = executor.prev_pills.items.len - 1;
                            break :blk next_garland;
                        } else if (executor.first_enqueued != .nothing) {
                            const asdf = executor.first_enqueued;
                            undo_stack.storeAllData(executor_index);
                            executor.first_enqueued = Toybox.get(asdf).specific.garland.next_enqueued;
                            // TODO(game)
                            // parent_pill_index = Toybox.get(asdf).specific.garland.enqueued_parent_pill_index;
                            Toybox.pop(asdf, undo_stack);
                            break :blk asdf;
                        } else {
                            break :blk try Toybox.buildGarland(.{}, &.{}, undo_stack);
                        }
                    };

                    Toybox.changeChildWithUndo(old_garland, new_garland, undo_stack);

                    const fnkname = Lego.Specific.Garland.children(old_garland).fnkname;
                    Toybox.popWithUndoAndChangingCoords(fnkname, undo_stack);
                    Toybox.destroyFloating(old_garland, undo_stack);

                    undo_stack.storeAllData(executor_index);
                    executor.first_pill = try Lego.Specific.Pill.build(old_case_parts.pattern.get().absolute_point, executor.first_pill, .{
                        .pattern = old_case_parts.pattern,
                        .input = old_input,
                        .fnkname_call = old_case_parts.fnkname,
                        .fnkname_response = animation.garland_fnkname,
                        // TODO(game)
                        // They don't include previous bindings, since they have now been merged
                        // .bindings = try mem.gpa.dupe(Binding, animation.new_bindings),
                    }, undo_stack);
                    Toybox.addChildLastWithoutChangingAbsPoint(floating_inputs_layer, executor.first_pill, undo_stack);
                } else {
                    assert(animation.new_bindings.len == 0);
                    Toybox.destroyFloating(try Lego.Specific.Garland.stealFnkname(
                        Lego.Specific.Executor.children(executor_index).garland,
                        animation.garland_fnkname,
                        undo_stack,
                    ), undo_stack);
                }
                undo_stack.storeAllData(executor_index);
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
            Lego.Specific.Garland.popCase(first_case, undo_stack);
            Toybox.addChildLast(workspace.floating_inputs_layer, first_case, undo_stack);

            const pattern = Lego.Specific.Case.children(first_case).pattern;

            // TODO(optim): memory management
            var new_bindings: std.ArrayList(Binding) = .init(workspace.gpa_for_bindings);
            const matching = try Lego.Specific.Sexpr.generateBindings(value, pattern, &new_bindings);
            const invoked_fnk: Lego.Index = if (!matching)
                .nothing
            else if (first_case.case().hasIdentityFnkname())
                .nothing
            else blk: {
                const offset = 3.0;
                const function_point = Lego.Specific.Executor.relative_garland_point
                    .applyToLocalPoint(.{ .pos = .new(2 * offset + 6, 6 * offset) });
                if (try workspace.getGarlandForFnk(first_case.case().fnkname, function_point)) |garland| {
                    Toybox.addChildLast(workspace.floating_inputs_layer, garland, undo_stack);
                    break :blk garland;
                } else @panic("TODO(game): handle this");
            };
            const garland_fnkname = try Lego.Specific.Garland.stealFnkname(garland_index, null, undo_stack);
            Toybox.addChildLast(floating_inputs_layer, garland_fnkname, undo_stack);
            const new_bindings_slice = try new_bindings.toOwnedSlice();
            undo_stack.storeAllData(executor_index);
            executor.animation = .{
                .matching = matching,
                .active_case = first_case,
                .invoked_fnk = invoked_fnk,
                .new_bindings = new_bindings_slice,
                .garland_fnkname = garland_fnkname,
            };

            if (matching) {
                var cur = first_case;
                while (cur != .nothing) : (cur = Toybox.next_preordered(cur, first_case).next) {
                    if (Toybox.get(cur).specific.as(.sexpr)) |sexpr| {
                        sexpr.executor_with_bindings = executor_index;
                        if (sexpr.kind == .atom_var and !sexpr.is_pattern) {
                            for (new_bindings_slice) |binding| {
                                if (std.mem.eql(u8, binding.name, sexpr.atom_name)) {
                                    undo_stack.storeAllData(cur);
                                    sexpr.emerging_value = try Toybox.dupeIntoFloating(binding.value, true, undo_stack);
                                    Toybox.setAbsolutePoint(sexpr.emerging_value, Toybox.get(cur).absolute_point);
                                    Toybox.refreshAbsolutePoints(&.{sexpr.emerging_value});
                                }
                            }
                        }
                    }
                }

                const new_pattern = Lego.Specific.Case.children(first_case).pattern;
                cur = new_pattern;
                while (cur != .nothing) : (cur = Toybox.next_preordered(cur, new_pattern).next) {
                    const sexpr = &cur.get().specific.sexpr;
                    assert(sexpr.is_pattern);
                    if (sexpr.kind == .atom_var) {
                        for (new_bindings_slice) |binding| {
                            if (std.mem.eql(u8, binding.name, sexpr.atom_name)) {
                                undo_stack.storeAllData(cur);
                                sexpr.emerging_value = try Toybox.dupeIntoFloating(binding.value, true, undo_stack);
                                Lego.Specific.Sexpr.setIsPattern(sexpr.emerging_value, true);
                                sexpr.emerging_value.get().specific.sexpr.is_pattern_t = 1;
                                Toybox.setAbsolutePoint(sexpr.emerging_value, Toybox.get(cur).absolute_point);
                                Lego.Specific.Sexpr.updateLocalPositions(sexpr.emerging_value);
                                Toybox.refreshAbsolutePoints(&.{sexpr.emerging_value});
                            }
                        }
                    }
                }
            }
        }

        if (executor.animation == null) {
            var cur = executor.first_pill;
            executor.first_pill = .nothing;
            while (cur != .nothing) : (cur = cur.get().specific.pill.next_pill) {
                undo_stack.storeAllData(cur);
                cur.get().specific.pill.remaining_lifetime = 1;
                cur.get().specific.pill.velocity = .new(-4, 0);
            }
        }

        undo_stack.storeAllData(Executor.children(executor_index).controls.get().specific.executor_controls.crank());
        Executor.children(executor_index).controls.get().specific.executor_controls
            .crank().get().specific.executor_crank.value = if (executor.animation) |anim| anim.t else 0;
    }

    fn resetExecutorAndExtractResult(workspace: *Workspace, executor_index: Lego.Index, original_garland: Lego.Index) !Lego.Index {
        const result = Lego.Specific.Executor.children(executor_index).input;
        const undo_stack = &workspace.undo_stack;

        Toybox.changeChildWithUndoAndAlsoCoords(
            result,
            try Toybox.buildSexpr(.{}, .empty, false, false, undo_stack),
            undo_stack,
        );

        const children = Lego.Specific.Executor.children(executor_index);
        // const executor = &Toybox.get(executor_index).specific.executor;
        Toybox.changeChildWithUndo(children.garland, original_garland, undo_stack);
        Toybox.changeChildWithUndoAndAlsoCoords(
            children.input,
            try Toybox.buildSexpr(
                Lego.Specific.Executor.relative_input_point,
                .empty,
                false,
                false,
                undo_stack,
            ),
            undo_stack,
        );
        undo_stack.storeAllData(executor_index);
        Toybox.get(executor_index).specific.executor.garland_appearing_t = -1;
        // TODO(game)
        // fnkbox.executor.prev_pills.clearRetainingCapacity();
        // fnkbox.executor.enqueued_stack.clearRetainingCapacity();

        return result;
    }

    fn getGarlandForFnk(
        workspace: *Workspace,
        fnkname: Lego.Index,
        new_point: Point,
    ) !?Lego.Index {
        _ = new_point;
        const undo_stack = &workspace.undo_stack;
        for (toybox.all_legos.items) |*lego| {
            if (!lego.exists) continue;
            if (lego.specific.as(.fnkbox)) |fnkbox| {
                if (Lego.Specific.Sexpr.equalValue(fnkbox.fnkname(), fnkname)) {
                    const garland = try Toybox.dupeIntoFloating(if (fnkbox.execution) |e|
                        e.original_garland
                    else
                        fnkbox.executor().garland().index, true, undo_stack);
                    const original_fnkname = try Lego.Specific.Garland.stealFnkname(
                        garland,
                        try Toybox.dupeIntoFloating(fnkname, true, undo_stack),
                        undo_stack,
                    );
                    assert(original_fnkname.get().specific.sexpr.kind == .empty);
                    Toybox.destroyFloating(original_fnkname, undo_stack);
                    return garland;
                }
            }
        } else return null;
    }

    fn launchTestcase(testcase_index: Lego.Index, undo_stack: *UndoStack) !void {
        assert(Toybox.get(testcase_index).specific.tag() == .testcase);
        const fnkbox_index = Toybox.findAncestor(testcase_index, .fnkbox);
        const fnkbox = &Toybox.get(fnkbox_index).specific.fnkbox;
        assert(fnkbox.execution == null);
        const executor_index = Lego.Specific.Fnkbox.children(fnkbox_index).executor;

        const old_actual = Lego.Specific.Testcase.children(testcase_index).actual;
        const new_actual = try Toybox.buildSexpr(Toybox.get(old_actual).local_point, .empty, false, false, undo_stack);
        Toybox.changeChild(old_actual, new_actual, undo_stack);

        const original_garland_index = Lego.Specific.Executor.children(executor_index).garland;
        const backup_garland_index = try Toybox.dupeIntoFloating(original_garland_index, true, undo_stack);

        undo_stack.storeAllData(fnkbox_index);
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
};

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
    random_seed: u64,
    // tweakable: type,
    // tweakable: struct {
    //     fcolor: fn (name: []const u8, value: *FColor) void,
    // },
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);
    try dst.toybox_instance.init(gpa);
    toybox = &dst.toybox_instance;

    dst.usual.init(
        gpa,
        random_seed,
        try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}),
    );

    // tweakable.fcolor("bg", &COLORS.bg);

    dst.drawer = try .init(&dst.usual);
    try dst.workspace.init(gpa, random_seed);
}

// TODO(platform): take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.usual.deinit(undefined);
    self.workspace.deinit();
    _ = gpa;
    toybox.deinit();
}

pub fn beforeHotReload(_: *GameState) !void {}

pub fn afterHotReload(self: *GameState) !void {
    try Drawer.AtomVisuals.Geometry.initFixed(self.usual.mem.forever.allocator(), self.usual.canvas.gl);
    self.drawer.atom_visuals_cache = try .init(self.usual.mem.forever.allocator(), self.usual.canvas.gl);
    // try self.workspace.init(&self.core_mem, 0);
    toybox = &self.toybox_instance;
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);

    if (false and platform.keyboard.wasPressed(.KeyQ)) {
        var asdf: std.ArrayList(u8) = .init(platform.gpa);
        defer asdf.deinit();
        try self.workspace.save(asdf.writer().any(), self.usual.mem.frame.allocator());
        std.log.debug("save size in bytes: {d}", .{asdf.items.len});
        var fbs = std.io.fixedBufferStream(asdf.items);
        try self.workspace.load(fbs.reader().any(), &self.core_mem);
    }

    platform.gl.clear(COLORS.bg);
    try self.workspace.update(platform, &self.drawer, self.usual.mem.frame.allocator());

    return false;
}

fn moveCamera(camera: Rect, delta_seconds: f32, keyboard: Keyboard, mouse: Mouse, allow_zoom: bool) Rect {
    var result = camera;
    const mouse_pos = mouse.cur.position;

    if (allow_zoom) {
        result = result.zoom(mouse_pos, switch (mouse.cur.scrolled) {
            .none => 1.0,
            .down => 1.1,
            .up => 0.9,
        });
    }

    inline for (KeyboardButton.directional_keys) |kv| {
        for (kv.keys) |key| {
            if (keyboard.cur.isDown(key)) {
                result.top_left.addInPlace(kv.dir.scale(delta_seconds * camera.size.y));
            }
        }
    }

    if (mouse.cur.isDown(.middle) and mouse.prev.isDown(.middle)) {
        result.top_left.addInPlace(mouse.deltaPos().neg());
    }

    return result;
}

pub fn writeEnum(out: std.io.AnyWriter, T: type, value: T, endian: std.builtin.Endian) !void {
    const type_info = @typeInfo(T).@"enum";
    try out.writeInt(type_info.tag_type, @intFromEnum(value), endian);
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

pub fn writeString(out: std.io.AnyWriter, value: []const u8) !void {
    try out.writeInt(u32, @intCast(value.len), .little);
    try out.writeAll(value);
}

pub fn readString(in: std.io.AnyReader, allocator: std.mem.Allocator) ![]u8 {
    const len: usize = @intCast(try in.readInt(u32, .little));
    const result = try allocator.alloc(u8, len);
    const actual_len = try in.readAll(result);
    if (actual_len != len) @panic("bad string");
    return result;
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
