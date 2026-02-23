pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// Causes of bugs:
// - functions that take a pointer and allocate memory might invalidate that pointer

const Drawer = @import("Drawer.zig");

pub const tracy = @import("tracy");

pub const display_fps = true;

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

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "vaulogy",
        .author = "knexator",
        .desired_aspect_ratio = 16.0 / 9.0,
    },
    .sounds = .{},
    .loops = .{},
    .preloaded_images = .{
        // TODO: don't require this here
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
    // TODO: remove in release modes
    exists: bool = false,
    index: Index,
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
        area: Area,
        sexpr: Sexpr,
        case: Case,
        garland: Garland,
        /// cable between cases, and the handle to create new ones
        newcase: NewCase,
        executor: Executor,
        fnkbox: Fnkbox,
        fnkbox_box: FnkboxBox,
        testcase: Testcase,
        microscope: Microscope,
        lens: Lens,
        postit: Postit,

        // TODO: try to simplify these
        fnkbox_description: struct {
            text: []const u8,
        },
        fnkbox_testcases: struct {
            scroll_visual: f32 = 0,
            scroll_target: f32 = 0,
        },
        postit_text: struct {
            text: []const u8,
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

        pub const Button = struct {
            local_rect: Rect,
            action: enum { launch_testcase, see_failing_testcase },
            enabled: bool = true,

            pub fn instant(button: Button) bool {
                return switch (button.action) {
                    .launch_testcase, .see_failing_testcase => false,
                };
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
            atom_name: []const u8,
            immutable: bool,

            // TODO: rethink
            executor_with_bindings: Lego.Index = .nothing,
            emerging_value: Lego.Index = .nothing,

            pub const Kind = enum { empty, atom_lit, atom_var, pair };

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
                                return try generateBindings(pat_up, val_up, bindings) and try generateBindings(pat_down, val_down, bindings);
                            },
                        }
                    },
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

            pub fn buildFromOldCoreValue(point: Point, value: *const core.Sexpr, is_pattern: bool) !Lego.Index {
                return try Toybox.buildSexpr(point, switch (value.*) {
                    .empty => .empty,
                    .atom_lit => |s| .{ .atom_lit = s.value },
                    .atom_var => |s| .{ .atom_var = s.value },
                    .pair => |pair| .{ .pair = .{
                        .up = try buildFromOldCoreValue(point.applyToLocalPoint(ViewHelper.offsetFor(false, .up)), pair.left, is_pattern),
                        .down = try buildFromOldCoreValue(point.applyToLocalPoint(ViewHelper.offsetFor(false, .down)), pair.right, is_pattern),
                    } },
                }, is_pattern);
            }
        };

        pub const Case = struct {
            /// offset for the next garland, used during animations
            next_point_extra: Point = .{},
            /// offset for the fnk name, used during animations
            fnk_name_extra: Point = .{},

            const fnk_name_offset: Point = .{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) };
            const next_garland_offset: Vec2 = .new(8, if (SEQUENTIAL_GOES_DOWN) 1 else -1.5);

            pub fn children(index: Lego.Index) struct {
                pattern: Lego.Index,
                template: Lego.Index,
                fnkname: Lego.Index,
                next: Lego.Index,
            } {
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

            pub fn firstNewcase(garland: *const Garland) *Specific.NewCase {
                return &Lego.fromSpecificConst(.garland, garland).tree.first.get().specific.newcase;
            }

            const core = @import("core.zig");
            pub fn toOldCoreValue(garland: *const Garland, allocator: std.mem.Allocator) !core.FnkBody {
                const cable_segments = try Toybox.getChildrenUnknown(allocator, Lego.fromSpecificConst(.garland, garland).index);
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

            pub fn buildFromOldCoreValue(point: Point, definition: core.FnkBodyV2, scratch: std.mem.Allocator) !Lego.Index {
                var cases: std.ArrayListUnmanaged(Lego.Index) = try .initCapacity(scratch, definition.cases.len);
                for (definition.cases) |case| {
                    cases.appendAssumeCapacity(try Toybox.buildCase(.{}, .{
                        .pattern = try Sexpr.buildFromOldCoreValue(.{}, case.pattern, true),
                        .template = try Sexpr.buildFromOldCoreValue(.{}, case.template, false),
                        .fnkname = try Sexpr.buildFromOldCoreValue(.{}, case.fnk_name, false),
                        .next = if (case.next) |next|
                            try buildFromOldCoreValue(.{}, .{ .cases = next }, scratch)
                        else
                            null,
                    }));
                }
                return try Toybox.buildGarland(point, try cases.toOwnedSlice(scratch));
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

        pub const Executor = struct {
            controlled_by_parent_fnkbox: bool,
            animation: ?struct {
                t: f32 = 0,
                active_case: Lego.Index,
                matching: bool,
                invoked_fnk: Lego.Index,
                // parent_pill: ?usize,
                // TODO: rethink
                new_bindings: []const Binding,
                // original_point: Point,
                // garland_fnkname: ?Lego.Index,
                // paused: bool = false,
            } = null,
            first_enqueued: Lego.Index = .nothing,
            garland_appearing_t: f32 = 1,

            const relative_input_point: Point = .{ .pos = .new(-1, 1.5) };
            const relative_garland_point: Point = .{ .pos = .new(4, 0) };
            const relative_crank_center: Point = .{ .pos = .new(-1, 4) };
            const first_case_point: Point = relative_garland_point.applyToLocalPoint(.{ .pos = .new(0, 1.5) });

            // TODO: rethink
            pub fn bindingsActive(executor_index: Lego.Index) BindingsState {
                const executor = Toybox.get(executor_index).specific.executor;
                return if (executor.animation) |anim| .{
                    .anim_t = if (anim.t < 0.2) null else math.remapTo01Clamped(anim.t, 0.2, 0.8),
                    .old = &.{},
                    // TODO
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
                    Toybox.childCount(garland) > 1 and
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
                testcases_area: Lego.Index,
            } {
                assert(Toybox.get(index).specific.tag() == .fnkbox_box);
                const asdf = Toybox.getChildrenExact(3, index);
                return .{
                    .description = asdf[0],
                    .status_bar = asdf[1],
                    .testcases_area = asdf[2],
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
                // TODO: score
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
                // TODO: improve somehow
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
                        // TODO: "NoMatchingCase" is no longer an error
                        error.FnkNotFound,
                        error.NoMatchingCase,
                        error.UsedUndefinedVariable,
                        error.InvalidMetaFnk,
                        error.TookTooLong,
                        => core.Sexpr.builtin.empty,
                        error.OutOfMemory => return err,
                        error.BAD_INPUT => @panic("panic"),
                    };
                    if (!actual_output.equals(actual_value) and fnkbox.execution == null) {
                        Toybox.changeChild(t.actual, try Sexpr.buildFromOldCoreValue(t.actual.get().local_point, actual_output, false));
                    }
                }

                // Get the actual status
                const box_index = children(fnkbox_index).box;
                cur_testcase = FnkboxBox.children(box_index).testcases_area.get().tree.first;
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

                    if (!Sexpr.equalValue(actual, expected)) {
                        fnkbox.status = .{ .unsolved = cur_testcase };
                        return;
                    }
                }
                fnkbox.status = .solved;
            }
        };

        pub const Testcase = struct {
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

        pub fn get(index: Index) *Lego {
            return Toybox.get(index);
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

            pub fn hasChildCases(this: @This()) bool {
                return Toybox.childCount(this.self) > 1;
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
            .fnkbox_box,
            .fnkbox_description,
            .fnkbox_testcases,
            .executor,
            .testcase,
            .postit,
            .postit_text,
            .executor_controls,
            => return null,
            .executor_brake => .default,
            .executor_crank => |crank| if (crank.enabled) .default else return null,
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
        return switch (lego.specific) {
            .button => false,
            else => true,
        };
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
            .fnkbox_testcases => |*fnkbox_testcases| {
                fnkbox_testcases.scroll_target += amount;
            },
        }
    }

    pub fn ignoresGrabOffset(lego: *const Lego) bool {
        return switch (lego.specific) {
            .postit => false,
            else => true,
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
            .button,
            .executor_brake,
            .executor_crank,
            => false,
            .fnkbox, .lens => blk: {
                std.log.err("TODO: handle better", .{});
                break :blk false;
            },
            .executor_controls,
            .microscope,
            .fnkbox_box,
            .fnkbox_description,
            .fnkbox_testcases,
            .newcase,
            .area,
            .testcase,
            .postit_text,
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
            => true,
            .sexpr,
            .garland,
            .case,
            .postit,
            .executor,
            => false,
            .executor_controls,
            .microscope,
            .fnkbox_box,
            .fnkbox_description,
            .fnkbox_testcases,
            .newcase,
            .area,
            .testcase,
            .postit_text,
            => unreachable,
        };
    }

    pub fn localBoundingBoxThatContainsSelfAndAllChildren(lego: *const Lego) Rect {
        return switch (lego.specific.tag()) {
            else => .infinite,
            .sexpr => .fromCenterAndSize(.zero, .new(5, 2.5)),
            .testcase => Specific.Testcase.relative_bounding_box,
            .fnkbox_testcases => Specific.FnkboxBox.testcases_box,
            .fnkbox => Specific.FnkboxBox.relative_box
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
        pub const new_case: Size = .{ .base = 0.1, .hot = 0.4, .hitbox = 1.5 };
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
    // TODO: use a fancy arena thing
    all_legos: std.ArrayListUnmanaged(Lego),
    all_legos_arena: std.heap.ArenaAllocator,

    pub fn init(dst: *Toybox, gpa: std.mem.Allocator) !void {
        dst.* = .{
            .all_legos_arena = .init(gpa),
            .all_legos = .empty,
        };
        // TODO: tweak this number
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

    pub fn new(local_point: Point, specific: Lego.Specific) !*Lego {
        if (toybox.all_legos.items.len >= std.math.maxInt(i31)) OoM();
        const result = toybox.all_legos.addOne(toybox.all_legos_arena.allocator()) catch OoM();
        result.* = .{
            .index = @enumFromInt(toybox.all_legos.items.len - 1),
            .exists = true,
            .local_point = local_point,
            .specific = specific,
        };
        return result;
    }

    pub fn get(index: Lego.Index) *Lego {
        return &toybox.all_legos.items[@intFromEnum(index)];
    }

    pub fn safeGet(index: Lego.Index) ?*Lego {
        if (index == .nothing) return null;
        return get(index);
    }

    pub fn addChildLast(parent: Lego.Index, new_child: Lego.Index) void {
        // TODO: call insert?
        assert(parent != .nothing);
        if (new_child == .nothing) return;
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

    pub fn destroyFloatingWithUndo(index: Lego.Index, undo_stack: *Workspace.UndoStack) void {
        undo_stack.appendAssumeCapacity(.{ .recreate_floating = Toybox.get(index).* });
        Toybox.destroyFloating(index);
    }

    pub fn destroyFloating(index: Lego.Index) void {
        assert(Toybox.isFloating(index));

        var cur: Lego.Index = index;
        while (cur != .nothing) {
            const next = Toybox.next_preordered(cur, index).next;
            // TODO: set undefined to catch bugs, can't do it now since it would mess the iteration
            // Toybox.get(cur).* = undefined;
            // Toybox.get(cur).index = index;
            Toybox.get(cur).exists = false;
            cur = next;
        }

        // Toybox.get(lego).free_next = toybox.free_head;
        // TODO: free the memory
        // @panic("TODO");
    }

    pub fn recreateFloating(data: Lego) void {
        assert(data.tree.isFloating());
        assert(!Toybox.get(data.index).exists);
        Toybox.get(data.index).* = data;
    }

    pub fn dupeIntoFloating(original: Lego.Index, dupe_children: bool) !Lego.Index {
        const result = try Toybox.new(undefined, undefined);
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
                const new_child_index = try Toybox.dupeIntoFloating(cur, true);
                Toybox.addChildLast(result_index, new_child_index);
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

    pub fn pop(child: Lego.Index) void {
        assert(!Toybox.isFloating(child));
        changeChild(child, .nothing);
    }

    pub fn popWithUndo(child: Lego.Index, undo_stack: *Workspace.UndoStack) void {
        undo_stack.appendAssumeCapacity(.{ .insert = .{
            .what = child,
            .where = Toybox.get(child).tree,
        } });
        Toybox.pop(child);
    }

    pub fn popWithUndoAndChangingCoords(child: Lego.Index, undo_stack: *Workspace.UndoStack) void {
        undo_stack.appendAssumeCapacity(.{ .insert = .{
            .what = child,
            .where = Toybox.get(child).tree,
        } });
        const old_parent_abs_point = Toybox.parentAbsolutePoint(child);
        Toybox.pop(child);
        Toybox.changeCoordinates(child, old_parent_abs_point, .{});
    }

    pub fn insert(child: Lego.Index, where: Lego.Tree) void {
        assert(Toybox.isFloating(child));
        assert(!where.isFloating());
        defer assert(Toybox.get(child).tree.equals(where));

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

    pub fn changeChildWithUndo(original_child: Lego.Index, new_child: Lego.Index, undo_stack: *Workspace.UndoStack) void {
        Toybox.changeChild(original_child, new_child);
        undo_stack.appendAssumeCapacity(.{ .change_child = .{
            .original = new_child,
            .new = original_child,
        } });
    }

    pub fn changeChildWithUndoAndAlsoCoords(original_child: Lego.Index, new_child: Lego.Index, undo_stack: *Workspace.UndoStack) void {
        const old_parent_abs_point = Toybox.parentAbsolutePoint(original_child);
        Toybox.changeChild(original_child, new_child);
        undo_stack.appendAssumeCapacity(.{ .change_child = .{
            .original = new_child,
            .new = original_child,
        } });
        Toybox.changeCoordinates(original_child, old_parent_abs_point, .{});
        Toybox.changeCoordinates(new_child, .{}, old_parent_abs_point);
    }

    /// things that pointed to original, now will point to new
    /// original will be left floating
    pub fn changeChild(original_child: Lego.Index, new_child: Lego.Index) void {
        assert(original_child != .nothing);
        assert(new_child == .nothing or isFloating(new_child));
        defer assert(isFloating(original_child));
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
        const root = try Toybox.new(undefined, undefined);
        const child_1 = try Toybox.new(undefined, undefined);
        const child_2 = try Toybox.new(undefined, undefined);
        const grandchild_1_1 = try Toybox.new(undefined, undefined);
        const grandchild_1_2 = try Toybox.new(undefined, undefined);
        const grandchild_2_1 = try Toybox.new(undefined, undefined);
        const grandchild_2_2 = try Toybox.new(undefined, undefined);

        Toybox.addChildLast(root.index, child_1.index);
        Toybox.addChildLast(root.index, child_2.index);

        Toybox.addChildLast(child_1.index, grandchild_1_1.index);
        Toybox.addChildLast(child_1.index, grandchild_1_2.index);

        Toybox.addChildLast(child_2.index, grandchild_2_1.index);
        Toybox.addChildLast(child_2.index, grandchild_2_2.index);

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
    }

    pub fn setAbsolutePoint(index: Lego.Index, abs_point: Point) void {
        Toybox.get(index).local_point = Toybox.parentAbsolutePoint(index).inverseApplyGetLocal(abs_point);
    }

    pub fn buildSexpr(local_point: Point, value: union(Lego.Specific.Sexpr.Kind) {
        empty,
        atom_lit: []const u8,
        atom_var: []const u8,
        pair: struct { up: Lego.Index, down: Lego.Index },
    }, is_pattern: bool) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .sexpr = .{
            .is_pattern = is_pattern,
            .is_pattern_t = if (is_pattern) 1 else 0,
            .immutable = false,
            .atom_name = switch (value) {
                .atom_lit, .atom_var => |v| v,
                else => undefined,
            },
            .kind = value,
        } });
        switch (value) {
            else => {},
            .pair => |pair| {
                Toybox.addChildLast(result.index, pair.up);
                Toybox.addChildLast(result.index, pair.down);
            },
        }
        return result.index;
    }

    pub fn buildCase(local_point: Point, data: struct {
        pattern: Lego.Index,
        template: Lego.Index,
        fnkname: Lego.Index,
        next: ?Lego.Index,
    }) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .case = .{} });
        Toybox.addChildLast(result.index, data.pattern);
        Toybox.addChildLast(result.index, data.template);
        Toybox.addChildLast(result.index, data.fnkname);
        Toybox.addChildLast(result.index, data.next orelse try Toybox.buildGarland(local_point, &.{}));
        return result.index;
    }

    /// The garland's children are a linear list of newcase, all except the last one with a child case
    /// the newcase position is the very top of the segment
    pub fn buildGarland(local_point: Point, child_cases: []const Lego.Index) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .garland = .{} });
        for (child_cases) |case| {
            const new_segment = try Toybox.new(.{}, .{ .newcase = .{} });
            Toybox.addChildLast(new_segment.index, case);
            Toybox.addChildLast(result.index, new_segment.index);
        }
        Toybox.addChildLast(result.index, (try Toybox.new(.{}, .{ .newcase = .{} })).index);
        return result.index;
    }

    /// Children are:
    /// - box with (description area, status bar, testcases area)
    /// - fnkname
    /// - executor
    pub fn buildFnkbox(
        local_point: Point,
        // TODO: take *const Sepxr
        fnkname: Lego.Index,
        text: []const u8,
        testcases: []const [2]Lego.Index,
        initial_definition: ?Lego.Index,
    ) !Lego.Index {
        const result = try Toybox.new(local_point, .{ .fnkbox = .{ .status = undefined } });
        const box = try Toybox.new(local_point, .{ .fnkbox_box = .{} });
        Toybox.addChildLast(box.index, (try Toybox.new(.{}, .{
            .fnkbox_description = .{
                .text = text,
            },
        })).index);
        Toybox.addChildLast(box.index, (try Toybox.new(.{}, .{
            .button = .{
                .local_rect = .fromMeasureAndSizeV2(.top_center, .zero, .new(16, 1)),
                .action = .see_failing_testcase,
            },
        })).index);
        const fnkbox_testcases = try Toybox.new(.{}, .{
            .fnkbox_testcases = .{},
        });
        for (testcases) |values| {
            const testcase = try Toybox.new(.{}, .{ .testcase = .{} });
            Toybox.addChildLast(testcase.index, values[0]);
            Toybox.addChildLast(testcase.index, values[1]);
            Toybox.addChildLast(testcase.index, try Toybox.buildSexpr(.{}, .empty, false));
            Toybox.addChildLast(testcase.index, (try Toybox.new(.{}, .{ .button = .{
                .local_rect = .unit,
                .action = .launch_testcase,
            } })).index);

            Toybox.addChildLast(fnkbox_testcases.index, testcase.index);
        }
        Toybox.addChildLast(box.index, fnkbox_testcases.index);
        Toybox.addChildLast(result.index, box.index);
        Toybox.addChildLast(result.index, fnkname);
        const executor = try Toybox.new(.{}, .{ .executor = .{ .controlled_by_parent_fnkbox = true } });
        Toybox.addChildLast(executor.index, try Toybox.buildSexpr(.{}, .empty, false));
        Toybox.addChildLast(executor.index, initial_definition orelse try Toybox.buildGarland(.{}, &.{}));
        Toybox.addChildLast(executor.index, blk: {
            const controls = try Toybox.new(.{}, .executor_controls);
            Toybox.addChildLast(controls.index, (try Toybox.new(.{}, .{ .executor_brake = .{ .brake_t = 0.5 } })).index);
            Toybox.addChildLast(controls.index, (try Toybox.new(.{}, .{ .executor_crank = .{ .value = 0.0 } })).index);
            break :blk controls.index;
        });
        Toybox.addChildLast(result.index, executor.index);
        return result.index;
    }

    pub fn buildMicroscope(source: Vec2, target: Vec2) !Lego.Index {
        const lens_source = try Toybox.new(.{ .pos = source }, .{ .lens = .source });
        const lens_target = try Toybox.new(.{ .pos = target }, .{ .lens = .target });
        const result = try Toybox.new(.{}, .microscope);
        Toybox.addChildLast(result.index, lens_source.index);
        Toybox.addChildLast(result.index, lens_target.index);
        return result.index;
    }
};

const Workspace = struct {
    main_area: Lego.Index,
    fnkboxes_layer: Lego.Index,
    toolbar_left: Lego.Index,
    toolbar_left_unfolded_t: f32 = 0,
    lenses_layer: Lego.Index,
    floating_inputs_layer: Lego.Index,
    hand_layer: Lego.Index = .nothing,

    grabbing: Grabbing = .nothing,

    undo_stack: UndoStack,
    random_instance: std.Random.DefaultPrng,
    arena_for_atom_names: std.heap.ArenaAllocator,
    arena_for_lenses_data: std.heap.ArenaAllocator,

    // TODO: remove
    gpa_for_bindings: std.mem.Allocator,

    pub const Grabbing = struct {
        index: Lego.Index,
        offset: Vec2,

        pub const nothing: Grabbing = .{ .index = .nothing, .offset = .zero };
    };

    pub const UndoStack = std.ArrayList(UndoableCommand);

    pub const toolbar_left_rect: Rect = .{ .top_left = .zero, .size = .new(6, 15) };

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
        include_toolbar: bool,
        include_floating_inputs: bool,

        pub const all: @This() = .{
            .include_hand = true,
            .include_lenses = true,
            .include_toolbar = true,
            .include_floating_inputs = true,
        };
        pub const interactable: @This() = .{
            .include_hand = false,
            .include_lenses = true,
            .include_toolbar = true,
            .include_floating_inputs = false,
        };
        pub const with_main_camera: @This() = .{
            .include_hand = false,
            .include_lenses = true,
            .include_toolbar = false,
            .include_floating_inputs = true,
        };
    }) std.BoundedArray(Lego.Index, 6) {
        var result: std.BoundedArray(Lego.Index, 6) = .{};
        result.appendAssumeCapacity(workspace.main_area);
        result.appendAssumeCapacity(workspace.fnkboxes_layer);
        if (config.include_floating_inputs) result.appendAssumeCapacity(workspace.floating_inputs_layer);
        if (config.include_toolbar) result.appendAssumeCapacity(workspace.toolbar_left);
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

        dst.main_area = (try Toybox.new(.{ .scale = 0.1 }, .{ .area = .{ .bg = .all } })).index;
        dst.toolbar_left = (try Toybox.new(.{}, .{
            .area = .{
                .bg = .{
                    // ensure that "mouse off-screen on the left" also overlaps the toolbar
                    .local_rect = toolbar_left_rect.plusMargin3(.left, 100),
                },
            },
        })).index;
        dst.fnkboxes_layer = (try Toybox.new(undefined, .{ .area = .{ .bg = .none } })).index;
        dst.lenses_layer = (try Toybox.new(undefined, .{ .area = .{ .bg = .none } })).index;
        dst.floating_inputs_layer = (try Toybox.new(undefined, .{ .area = .{ .bg = .none } })).index;

        try dst.regenerateToolbarLeft();

        if (true) {
            Toybox.addChildLast(
                dst.fnkboxes_layer,
                try Toybox.buildFnkbox(
                    .{ .pos = .new(-4, -8) },
                    try Toybox.buildSexpr(
                        .{},
                        .{ .atom_lit = "true" },
                        true,
                    ),
                    "do lowercase",
                    &.{
                        .{
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "A" }, false),
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false),
                        },
                        .{
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "B" }, false),
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false),
                        },
                        .{
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "C" }, false),
                            try Toybox.buildSexpr(.{}, .{ .atom_lit = "c" }, false),
                        },
                    },
                    try Toybox.buildGarland(.{}, &.{
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "A" }, true),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "a" }, false),
                            .fnkname = try Toybox.buildSexpr(.{}, .empty, false),
                            .next = null,
                        }),
                        try Toybox.buildCase(.{}, .{
                            .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "B" }, true),
                            .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "b" }, false),
                            .fnkname = try Toybox.buildSexpr(.{}, .empty, false),
                            .next = null,
                        }),
                    }),
                ),
            );

            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = .new(0, 0) },
                .{ .atom_lit = "true" },
                false,
            ));

            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = .new(0, 1) },
                .{ .atom_lit = "false" },
                false,
            ));

            Toybox.addChildLast(dst.main_area, try Toybox.buildSexpr(
                .{ .pos = .new(3, 0) },
                .{ .pair = .{
                    .up = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, false),
                    .down = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                } },
                false,
            ));

            Toybox.addChildLast(dst.main_area, try Toybox.buildCase(
                .{ .pos = .new(0, 4) },
                .{
                    .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true),
                    .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                    .fnkname = try Toybox.buildSexpr(.{}, .empty, false),
                    .next = null,
                },
            ));

            const case_1 = try Toybox.buildCase(.{}, .{
                .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true),
                .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                .fnkname = try Toybox.buildSexpr(.{}, .empty, false),
                .next = null,
            });
            const case_2 = try Toybox.buildCase(.{}, .{
                .pattern = try Toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true),
                .template = try Toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                .fnkname = try Toybox.buildSexpr(.{}, .empty, false),
                .next = null,
            });
            Toybox.addChildLast(dst.main_area, try Toybox.buildGarland(
                .{ .pos = .new(7, 4) },
                &.{ case_1, case_2 },
            ));

            Toybox.addChildLast(dst.main_area, blk: {
                const postit = try Toybox.new(
                    .{ .pos = .new(3, 5) },
                    .{ .postit = .{} },
                );
                Toybox.addChildLast(postit.index, (try Toybox.new(
                    .{ .pos = .new(0, 0) },
                    .{ .postit_text = .{ .text = "TODO" } },
                )).index);

                break :blk postit.index;
            });

            Toybox.addChildLast(dst.lenses_layer, try Toybox.buildMicroscope(
                .new(2, 2),
                .new(4, 3),
            ));

            Toybox.addChildLast(dst.lenses_layer, try Toybox.buildMicroscope(
                .new(4, 3),
                .new(6, 2),
            ));
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
                            try Sexpr.buildFromOldCoreValue(.{}, item.input, false),
                            try Sexpr.buildFromOldCoreValue(.{}, item.expected, false),
                        });
                        _ = pool.reset(.retain_capacity);
                    }
                    break :blk try samples.toOwnedSlice(scratch.allocator());
                };

                Toybox.addChildLast(
                    dst.fnkboxes_layer,
                    try Toybox.buildFnkbox(
                        .{ .pos = .new(x, if (k % 2 == 0) -6 else -5) },
                        try Sexpr.buildFromOldCoreValue(.{}, level.fnk_name, true),
                        level.description,
                        samples,
                        if (level.initial_definition) |definition|
                            try Lego.Specific.Garland.buildFromOldCoreValue(.{}, definition, scratch.allocator())
                        else
                            null,
                    ),
                );

                // TODO
                // if (k == 0) {
                //     dst.main_area.fnkboxes.items[dst.main_area.fnkboxes.items.len - 1].executor.brake_t = 0.9;
                //     dst.main_area.fnkboxes.items[dst.main_area.fnkboxes.items.len - 1].scroll_testcases = 3;
                // }
                x += if (k < 4) 25 else if (k == 4) 30 else 35;
            }
        }

        var arena: std.heap.ArenaAllocator = .init(gpa);
        defer arena.deinit();
        try dst.canonizeAfterChanges(arena.allocator());
    }

    pub fn canonizeAfterChanges(workspace: *Workspace, scratch: std.mem.Allocator) !void {
        for (toybox.all_legos.items) |*lego| {
            if (lego.specific.as(.fnkbox)) |fnkbox| {
                try fnkbox.updateStatus(workspace, scratch);
            }
            // if (lego.specific.tag() == .fnkbox) {
            //     try lego.specific.fnkbox.updateStatus(workspace, scratch);
            // }
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
                const relative_needle_pos = lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos);
                if (lego.unhoverable and !step.children_already_visited) it.skipChildren();
                if (lego.unhoverable) continue;
                switch (lego.specific) {
                    .sexpr => |sexpr| {
                        // TODO: skip children if needle is far
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
                            (grabbing == .nothing or Toybox.get(grabbing).tree.parent == .nothing or cur == roots_in_draw_order[0]))
                        {
                            return .{ .over_background = cur };
                        }
                    },
                    .button => |button| {
                        if (step.children_already_visited and
                            button.enabled and
                            button.local_rect.contains(lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos)))
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
                    .executor,
                    .fnkbox,
                    .fnkbox_box,
                    .fnkbox_description,
                    .testcase,
                    .postit_text,
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
                            .button,
                            .executor,
                            .fnkbox_box,
                            .testcase,
                            .postit,
                            .postit_text,
                            .executor_controls,
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

        unreachable;
    }

    fn updateSprings(workspace: *Workspace, roots_in_draw_order: []const Lego.Index, absolute_mouse_pos: Vec2, interaction: HotAndDropzone, delta_seconds: f32) void {
        for (roots_in_draw_order) |root| {
            var cur: Lego.Index = root;
            while (cur != .nothing) : (cur = Toybox.next_preordered(cur, root).next) {
                const lego = Toybox.get(cur);
                defer lego.absolute_point = Toybox.parentAbsolutePoint(cur).applyToLocalPoint(lego.local_point);
                if (cur == workspace.grabbing.index and lego.draggable()) {
                    switch (lego.specific) {
                        else => {
                            const target: Point = if (interaction.dropzone == .nothing)
                                // TODO: i don't like the scale hack
                                (Point{
                                    .pos = absolute_mouse_pos,
                                    .scale = Toybox.get(interaction.over_background).absolute_point.scale,
                                })
                                    .applyToLocalPoint(.{ .pos = lego.handleLocalOffset().neg() })
                                    .applyToLocalPoint(.{ .pos = workspace.grabbing.offset.neg() })
                            else
                                Toybox.get(interaction.dropzone).absolute_point.applyToLocalPoint(.{ .pos = Toybox.get(interaction.dropzone).handleLocalOffset() });

                            lego.local_point.lerp_towards(Toybox.parentAbsolutePoint(cur)
                                .inverseApplyGetLocal(target), 0.6, delta_seconds);
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
                }

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
                        sexpr.immutable = if (lego.tree.parent == .nothing) false else switch (Toybox.get(lego.tree.parent).specific.tag()) {
                            else => false,
                            .testcase, .fnkbox => true,
                        };

                        if (cur == workspace.grabbing.index) {
                            sexpr.is_pattern = if (interaction.dropzone == .nothing) sexpr.is_pattern else Toybox.get(interaction.dropzone).specific.sexpr.is_pattern;
                        }

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
                            const child_up, const child_down = Toybox.getChildrenExact(2, cur);
                            Toybox.get(child_up).local_point = (Point{})
                                .applyToLocalPoint(lego.visual_offset)
                                .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .up));
                            Toybox.get(child_down).local_point = (Point{})
                                .applyToLocalPoint(lego.visual_offset)
                                .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .down));

                            Toybox.get(child_up).specific.sexpr.is_pattern = sexpr.is_pattern;
                            Toybox.get(child_up).specific.sexpr.is_pattern_t = if (sexpr.is_pattern) 1 else 0;
                            Toybox.get(child_down).specific.sexpr.is_pattern = sexpr.is_pattern;
                            Toybox.get(child_down).specific.sexpr.is_pattern_t = if (sexpr.is_pattern) 1 else 0;
                        }

                        if (sexpr.emerging_value != .nothing) {
                            const bindings = Lego.Specific.Executor.bindingsActive(sexpr.executor_with_bindings);
                            const offset: Point = if (bindings.anim_t) |anim_t|
                                if (sexpr.is_pattern)
                                    .{}
                                else
                                    .{ .pos = .new(math.remap(
                                        math.smoothstep(anim_t, 0, 0.4),
                                        0,
                                        1,
                                        -2.3,
                                        0,
                                    ), 0) }
                            else
                                .{ .pos = .new(-2.3, 0) };
                            Toybox.get(sexpr.emerging_value).local_point = lego.absolute_point.applyToLocalPoint(offset);
                            updateSprings(workspace, &.{sexpr.emerging_value}, absolute_mouse_pos, interaction, delta_seconds);
                        }
                    },
                    .case => |case| {
                        const offsets: [4]Point = .{
                            .{ .pos = .xneg },
                            .{ .pos = .xpos },
                            (Point{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) }).applyToLocalPoint(case.fnk_name_extra),
                            (Point{ .pos = .new(8, 1) }).applyToLocalPoint(case.next_point_extra),
                        };
                        // const pattern, const template, const fnkname = Toybox.getChildrenExact(3, cur);
                        // for (.{ pattern, template, fnkname }, offsets) |i, offset| {
                        for (Toybox.getChildrenExact(4, cur), offsets) |i, offset| {
                            Toybox.get(i).local_point = offset;
                        }
                    },
                    .garland => |*garland| {
                        var a = Toybox.get(lego.tree.first);
                        var offset: f32 = 0;
                        while (true) {
                            assert(a.specific.tag() == .newcase);
                            a.local_point = .{ .pos = .new(0, offset) };
                            offset += a.specific.newcase.length();

                            if (a.tree.next == .nothing) break;
                            a = Toybox.get(a.tree.next);
                        }
                        garland.computed_height = offset;
                    },
                    .newcase => |*newcase| {
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

                        math.lerpTowards(&newcase.length_before, target_length_before, .slow, delta_seconds);
                        math.lerpTowards(&newcase.length_after, target_length_after, .slow, delta_seconds);

                        if (Toybox.safeGet(maybe_child_case)) |case| case.local_point = .{ .pos = .new(0, newcase.length()) };
                    },
                    .fnkbox => {
                        const Fnkbox = Lego.Specific.Fnkbox;
                        const children = Fnkbox.children(cur);
                        Toybox.get(children.fnkname).local_point = Fnkbox.relative_fnkname_point;
                        Toybox.get(children.executor).local_point = Fnkbox.relative_executor_point;
                        Toybox.get(children.box).local_point = .{};
                    },
                    .fnkbox_box => {
                        // FIXME
                        const FnkboxBox = Lego.Specific.FnkboxBox;
                        const children = FnkboxBox.children(cur);
                        Toybox.get(children.status_bar).local_point = .{};
                        Toybox.get(children.status_bar).specific.button.local_rect = FnkboxBox.status_bar_goal;
                        Toybox.get(children.description).local_point = .{};
                        Toybox.get(children.testcases_area).local_point = .{};
                    },
                    .executor => |executor| {
                        const Executor = Lego.Specific.Executor;
                        const children = Executor.children(cur);
                        Toybox.get(children.garland).local_point = Executor.relative_garland_point;
                        Toybox.get(children.controls).local_point = Executor.relative_crank_center;

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
                                Toybox.get(children.garland).specific.garland.firstNewcase().offset_t = offset_t;
                                Toybox.get(children.garland).specific.garland.firstNewcase().offset_ghost = animation.active_case;
                                Toybox.setAbsolutePoint(animation.active_case, lego.absolute_point.applyToLocalPoint(case_floating_away));
                                Toybox.get(children.input).local_point = Executor.relative_input_point;
                                // TODO
                                // if (animation.garland_fnkname) |*f| f.point.lerp_towards(executor.inputPoint().applyToLocalPoint(.{ .pos = .new(3, -1.5), .turns = 0.25, .scale = 0.5 }), 0.6, delta_seconds);
                                // FIXME NOW
                                // for (executor.enqueued_stack.items, 0..) |*x, k| {
                                //     x.garland.kinematicUpdate(executor.garlandPoint().applyToLocalPoint(
                                //         extraForDequeuingNext(tof32(executor.enqueued_stack.items.len - k - 1) + 1),
                                //     ), null, delta_seconds);
                                // }
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

                                // TODO
                                if (animation.invoked_fnk != .nothing) {
                                    const offset = (1.0 - invoking_t) + 2.0 * math.smoothstepEased(invoking_t, 0.4, 0.0, .linear);
                                    const function_point = lego.absolute_point.applyToLocalPoint(Lego.Specific.Executor.relative_garland_point)
                                        .applyToLocalPoint(.{ .pos = .new(2 * offset + 6 - match_t - enqueueing_t * 5, 6 * offset) });

                                    Toybox.setAbsolutePoint(animation.invoked_fnk, function_point);

                                    // TODO
                                    // animation.active_case.kinematicUpdate(
                                    //     case_point,
                                    //     extraForEnqueuingNext(enqueueing_t),
                                    //     .{ .pos = .new(-invoking_t * 4, 0) },
                                    //     delta_seconds,
                                    // );
                                    // Toybox.get(animation.active_case).specific.case.next_point_extra = extraForEnqueuingNext(enqueueing_t);
                                    Toybox.get(animation.active_case).specific.case.fnk_name_extra = .{ .pos = .new(-invoking_t * 4, 0) };
                                    Toybox.setAbsolutePoint(animation.active_case, lego.absolute_point.applyToLocalPoint(case_point));

                                    // TODO
                                    // const enqueueing = animation.active_case.next.cases.items.len > 0;
                                    // for (executor.enqueued_stack.items, 0..) |*x, k| {
                                    //     x.garland.kinematicUpdate(executor.garlandPoint()
                                    //         .applyToLocalPoint(extraForDequeuingNext(1 + tof32(executor.enqueued_stack.items.len - k - 1) + if (enqueueing) enqueueing_t else 0)), null, delta_seconds);
                                    // }
                                } else {
                                    Toybox.setAbsolutePoint(animation.active_case, lego.absolute_point.applyToLocalPoint(case_point));
                                    Toybox.get(animation.active_case).specific.case.next_point_extra = .{
                                        .pos = .new(-enqueueing_t * 2, -(Lego.Specific.Case.next_garland_offset.y + Lego.Specific.Garland.dist_between_cases_first) *
                                            math.smoothstep(enqueueing_t, 0, 0.6)),
                                    };
                                }
                                Toybox.get(children.input).local_point = Executor.relative_input_point.applyToLocalPoint(.{ .pos = .new(-enqueueing_t * 5, 0) });
                                // TODO
                                // if (animation.garland_fnkname) |*f| f.point = executor.input.point.applyToLocalPoint(.{ .pos = .new(3, -1.5), .turns = 0.25, .scale = 0.5 });
                            }
                        } else {
                            Toybox.get(children.input).local_point = Executor.relative_input_point;
                        }

                        // TODO: prev_pills
                    },
                    .executor_controls => {},
                    // FIXME NOW: review
                    .executor_brake => |*brake| {
                        lego.local_point = .{};
                        brake.handle_pos = Lego.Specific.Executor.Controls.brakeHandlePath(brake.brake_t);
                    },
                    .executor_crank => |*crank| {
                        lego.local_point = .{};
                        crank.handle_pos = .fromPolar(0.75, crank.value);
                    },
                    .fnkbox_testcases => |*fnkbox_testcases| {
                        var k: usize = 0;
                        var cur_case: Lego.Index = lego.tree.first;
                        while (cur_case != .nothing) {
                            Toybox.get(cur_case).local_point = .{ .pos = Lego.Specific.FnkboxBox.relative_top_testcase_pos
                                .addY(2 + 2.5 * (tof32(k) - fnkbox_testcases.scroll_visual)) };
                            k += 1;
                            cur_case = Toybox.get(cur_case).tree.next;
                        }
                        math.lerpTowardsRange(&fnkbox_testcases.scroll_target, 0, @max(0, tof32(k) - Lego.Specific.FnkboxBox.visible_testcases), .slow, delta_seconds);
                        math.lerpTowards(&fnkbox_testcases.scroll_visual, fnkbox_testcases.scroll_target, .slow, delta_seconds);
                    },
                    .testcase => {
                        const Testcase = Lego.Specific.Testcase;
                        const children = Testcase.children(cur);
                        Toybox.get(children.input).local_point = Testcase.relative_input_point;
                        Toybox.get(children.expected).local_point = Testcase.relative_expected_point;
                        Toybox.get(children.actual).local_point = Testcase.relative_actual_point;
                        Toybox.get(children.play_button).local_point = .{};
                        Toybox.get(children.play_button).specific.button.local_rect = .fromCenterAndSize(.new(-6, 0), .one);
                    },
                    .area, .microscope, .lens, .button, .fnkbox_description, .postit, .postit_text => {},
                }
            }
        }
    }

    fn draw(workspace: *Workspace, platform: PlatformGives, drawer: *Drawer) !void {
        const asdf = tracy.initZone(@src(), .{ .name = "draw" });
        defer asdf.deinit();

        const camera = Rect
            .fromCenterAndSize(.zero, .both(2))
            .withAspectRatio(platform.aspect_ratio, .grow, .center);

        drawer.canvas.clipper.reset();
        drawer.canvas.clipper.use(drawer.canvas);

        try _draw(workspace.roots(.all).constSlice(), camera, drawer);

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

    fn _draw(roots_in_draw_order: []const Lego.Index, camera: Rect, drawer: *Drawer) !void {
        for (roots_in_draw_order) |root| {
            var it = Toybox.treeIterator(root, true);
            while (it.next()) |step| {
                const cur = step.index;
                const lego = Toybox.get(cur);
                const alpha: f32 = if (Toybox.safeGet(Toybox.findAncestor(cur, .executor))) |g| @max(0, g.specific.executor.garland_appearing_t) else 1;

                const camera_relative = camera.reparentCamera(lego.absolute_point);
                // TODO: improve
                const max_resolution = 2000;
                if (camera_relative.intersect(lego.localBoundingBoxThatContainsSelfAndAllChildren()) == null or
                    camera_relative.size.div(lego.localBoundingBoxThatContainsSelfAndAllChildren().size).normLInf() > max_resolution)
                {
                    it.skipChildren();
                    _ = it.next();
                    continue;
                }

                // TODO: don't draw if small or far from camera
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
                        .sexpr => |sexpr| {
                            if (sexpr.emerging_value != .nothing) {
                                if (drawer.canvas.clipper.push(.{ .camera = camera_relative, .shape = .{
                                    .custom = .{ .point = .{}, .shape = Drawer.AtomVisuals.Geometry.template_mask },
                                } })) {
                                    drawer.canvas.clipper.use(drawer.canvas);
                                    defer {
                                        drawer.canvas.clipper.pop();
                                        drawer.canvas.clipper.use(drawer.canvas);
                                    }
                                    try _draw(&.{sexpr.emerging_value}, camera, drawer);
                                } else |_| {
                                    std.log.err("reached max lens depth, TODO: improve", .{});
                                }
                            }

                            switch (sexpr.kind) {
                                .empty => {},
                                .atom_lit => try drawer.drawAtom(camera, point, sexpr.is_pattern, sexpr.atom_name, alpha),
                                .pair => try drawer.drawPairHolder(camera, point, sexpr.is_pattern, alpha),
                                .atom_var => try drawer.drawVariable(camera, point, sexpr.is_pattern, sexpr.atom_name, alpha),
                            }

                            const maybe_bindings: ?BindingsState = if (sexpr.executor_with_bindings != .nothing)
                                Lego.Specific.Executor.bindingsActive(sexpr.executor_with_bindings)
                            else
                                null;

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

                            if (maybe_bindings) |bindings| {
                                switch (sexpr.kind) {
                                    // TODO: cables?
                                    else => {},
                                    .atom_var => {
                                        // TODO: check that compiler skips the loop if anim_t is null
                                        for (bindings.new) |binding| {
                                            if (bindings.anim_t) |anim_t| {
                                                if (std.mem.eql(u8, binding.name, sexpr.atom_name)) {
                                                    if (sexpr.is_pattern) {
                                                        const t = math.smoothstep(anim_t, 0, 0.1);
                                                        try drawer.drawEatingPattern(camera, point, binding, t, alpha);
                                                    } else {
                                                        // FIXME NOW
                                                        // try drawer.clipAtomRegion(camera, point);
                                                        // const t = math.smoothstep(anim_t, 0, 0.4);
                                                        // try drawer.drawSexpr(camera, .{
                                                        //     .is_pattern = sexpr.is_pattern_t,
                                                        //     .value = binding.value,
                                                        //     .pos = point.applyToLocalPoint(.{ .pos = .new(math.remap(t, 0, 1, -2.3, 0), 0) }),
                                                        // }, alpha);
                                                        // drawer.endClip();

                                                        // try drawer.drawSexpr(camera, .{
                                                        //     .is_pattern = sexpr.is_pattern_t,
                                                        //     .value = sexpr.atom_name,
                                                        //     .pos = point,
                                                        // }, alpha * (1 - anim_t));

                                                        // TODO: uncomment
                                                        // if (anim_t < 0.5) {
                                                        //     try out_particles.append(.{ .point = actual_point, .t = t, .name = binding.name });
                                                        // }
                                                    }
                                                    break;
                                                }
                                            }
                                        } else for (bindings.old) |binding| {
                                            if (std.mem.eql(u8, binding.name, sexpr.atom_name)) {
                                                if (sexpr.is_pattern) {
                                                    const t = 1;
                                                    try drawer.drawEatingPattern(camera, point, binding, t, alpha);
                                                } else {
                                                    // FIXME NOW
                                                    // try drawer.drawSexpr(camera, .{
                                                    //     .is_pattern = sexpr.is_pattern_t,
                                                    //     .value = binding.value,
                                                    //     .pos = point,
                                                    // }, alpha);
                                                }
                                                break;
                                            }
                                        } else {
                                            // FIXME NOW
                                            // try drawer.drawSexpr(camera, .{
                                            //     .is_pattern = sexpr.is_pattern_t,
                                            //     .value = sexpr.atom_name,
                                            //     .pos = point,
                                            // }, alpha);
                                        }
                                    },
                                }
                            }
                        },
                        .lens => |lens| {
                            // TODO: lens distortion effect, on source and target

                            if (lens.is_target and camera.plusMargin(lego.absolute_point.scale * (lens.local_radius + 1)).contains(lego.absolute_point.pos)) {
                                const lens_circle: math.Circle = .{ .center = .zero, .radius = lens.local_radius };
                                if (drawer.canvas.clipper.push(.{ .camera = camera_relative, .shape = .{ .circle = lens_circle } })) {
                                    drawer.canvas.clipper.use(drawer.canvas);
                                    defer {
                                        drawer.canvas.clipper.pop();
                                        drawer.canvas.clipper.use(drawer.canvas);
                                    }
                                    drawer.canvas.fillCircleV2(camera_relative, lens_circle, COLORS.bg);

                                    try _draw(lens.roots_to_draw, lens.transform.getCamera(camera), drawer);
                                } else |_| {
                                    std.log.err("reached max lens depth, TODO: improve", .{});
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
                                // TODO: .all background
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
                            }
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
                            // TODO: camera_relative fails due to rotation
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
                        .testcase => {
                            // Don't draw testcases that will get clipped outside the testbox
                            assert(lego.tree.parent.get().specific.tag() == .fnkbox_testcases);
                            if (lego.local_point.applyToLocalRect(Lego.Specific.Testcase.relative_bounding_box)
                                .intersect(Lego.Specific.FnkboxBox.testcases_box) == null)
                            {
                                it.skipChildren();
                                _ = it.next();
                                continue;
                            }
                        },
                        .executor_controls, .case, .garland, .microscope, .executor, .fnkbox => {},
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
        }

        const delta_seconds = @min(1.0 / 30.0, platform.delta_seconds * @as(f32, (if (platform.keyboard.cur.isDown(.Space)) 0.1 else 1.0)));

        const absolute_camera = Rect
            .fromCenterAndSize(.zero, .both(2))
            .withAspectRatio(platform.aspect_ratio, .grow, .center);

        const mouse = platform.getMouse(absolute_camera);

        const old_undo_count = workspace.undo_stack.items.len;
        if (platform.keyboard.wasPressed(.KeyZ)) {
            while (workspace.undo_stack.pop()) |command| {
                switch (command) {
                    .fence => break,
                    .destroy_floating => |index| {
                        Toybox.destroyFloating(index);
                    },
                    .recreate_floating => |data| {
                        // TODO: recreate children too!
                        Toybox.recreateFloating(data);
                    },
                    .insert => |insert| {
                        Toybox.insert(insert.what, insert.where);
                    },
                    .set_data_except_tree => |data| {
                        // TODO: set the children data too!
                        const original_tree = Toybox.get(data.index).tree;
                        Toybox.get(data.index).* = data;
                        Toybox.get(data.index).tree = original_tree;
                    },
                    .pop => |index| {
                        Toybox.pop(index);
                    },
                    .set_grabbing => |grabbing| {
                        workspace.grabbing = grabbing;
                    },
                    .set_handlayer => |index| {
                        workspace.hand_layer = index;
                    },
                    .change_child => |change| {
                        Toybox.changeChild(change.original, change.new);
                    },
                }
            }
        } else { // INTERACTION
            const hot_and_dropzone = workspace.findHotAndDropzone(mouse.cur.position);

            try workspace.undo_stack.ensureUnusedCapacity(32);
            if (workspace.grabbing.index == .nothing and
                hot_and_dropzone.hot != .nothing and
                (mouse.wasPressed(.left) or mouse.wasPressed(.right)))
            {
                // Main case A: plucking/grabbing/clicking something
                try workspace.undo_stack.append(.fence);

                const hot_index = hot_and_dropzone.hot;
                const original_hot_data = Toybox.get(hot_index).*;
                const hot_parent = original_hot_data.tree.parent;
                const original_parent_absolute_point = Toybox.parentAbsolutePoint(hot_index);

                var grabbed_element_index: Lego.Index = undefined;
                var plucked: bool = true;

                if (mouse.wasPressed(.right) or (original_hot_data.specific.tag() == .sexpr and original_hot_data.specific.sexpr.immutable)) {
                    // Case A.0: duplicating
                    if (hot_index.get().canDuplicate()) {
                        const new_element_index = try Toybox.dupeIntoFloating(hot_index, true);
                        workspace.undo_stack.appendAssumeCapacity(.{
                            .destroy_floating = new_element_index,
                        });
                        grabbed_element_index = new_element_index;
                    } else {
                        grabbed_element_index = .nothing;
                        plucked = undefined;
                    }
                } else if (hot_index.get().grabsWithoutPlucking()) {
                    // Case A.3: grabbing rather than plucking, including buttons
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .set_data_except_tree = original_hot_data,
                    });
                    grabbed_element_index = hot_index;
                    plucked = false;

                    if (Toybox.get(hot_index).specific.as(.button)) |b| {
                        if (b.instant()) {
                            grabbed_element_index = .nothing;
                            @panic("TODO");
                        }
                    }
                } else if (hot_parent != .nothing and Toybox.get(hot_parent).specific.tag() == .area) {
                    // Case A.1: plucking a top-level thing
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .set_data_except_tree = original_hot_data,
                    });

                    Toybox.pop(hot_index);
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .insert = .{
                            .what = hot_index,
                            .where = original_hot_data.tree,
                        },
                    });

                    grabbed_element_index = hot_index;
                } else if (original_hot_data.specific.tag() == .sexpr) {
                    // Case A.2: plucking a nested sexpr
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .set_data_except_tree = original_hot_data,
                    });

                    const new_empty_sexpr = try Toybox.dupeIntoFloating(hot_index, false);
                    Toybox.get(new_empty_sexpr).specific.sexpr.kind = .empty;
                    workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = new_empty_sexpr });

                    Toybox.changeChild(hot_index, new_empty_sexpr);
                    workspace.undo_stack.appendAssumeCapacity(.{ .change_child = .{
                        .new = hot_index,
                        .original = new_empty_sexpr,
                    } });

                    grabbed_element_index = hot_index;
                } else if (hot_parent != .nothing and Toybox.get(hot_parent).specific.tag() == .newcase) {
                    // Case A.4: plucking a case from a garland
                    assert(original_hot_data.specific.tag() == .case);
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .set_data_except_tree = original_hot_data,
                    });

                    Toybox.pop(hot_index);
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .insert = .{
                            .what = hot_index,
                            .where = original_hot_data.tree,
                        },
                    });

                    const original_parent_tree = Toybox.get(hot_parent).tree;
                    Toybox.get(original_parent_tree.next).specific.newcase.length_before += Toybox.get(hot_parent).specific.newcase.length();
                    Toybox.pop(hot_parent);
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .insert = .{
                            .what = hot_parent,
                            .where = original_parent_tree,
                        },
                    });

                    grabbed_element_index = hot_index;
                } else if (Toybox.get(hot_index).specific.tag() == .garland) {
                    // Case A.5: plucking a garland, and replacing it with an empty one
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .set_data_except_tree = original_hot_data,
                    });

                    const new_garland = try Toybox.buildGarland(original_hot_data.local_point, &.{});
                    workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = new_garland });

                    Toybox.changeChild(hot_index, new_garland);
                    workspace.undo_stack.appendAssumeCapacity(.{ .change_child = .{
                        .new = hot_index,
                        .original = new_garland,
                    } });

                    grabbed_element_index = hot_index;
                } else unreachable;

                assert(workspace.grabbing.index == .nothing and workspace.hand_layer == .nothing);
                if (grabbed_element_index != .nothing) {
                    workspace.grabbing = .{ .index = grabbed_element_index, .offset = if (grabbed_element_index.get().ignoresGrabOffset())
                        .zero
                    else
                        grabbed_element_index.get().absolute_point.inverseApplyGetLocalPosition(mouse.cur.position) };
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .set_grabbing = .nothing,
                    });
                    if (plucked) {
                        workspace.hand_layer = grabbed_element_index;
                        workspace.undo_stack.appendAssumeCapacity(.{
                            .set_handlayer = .nothing,
                        });
                        Toybox.changeCoordinates(grabbed_element_index, original_parent_absolute_point, .{});
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
                        } });
                        displaced_newcase.length_before = 0;
                        workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = newcase.index });
                        const original_tree = Toybox.get(dropzone_index).tree;
                        Toybox.insert(newcase.index, .{
                            .parent = original_tree.parent,
                            .prev = original_tree.prev,
                            .next = dropzone_index,
                            .first = .nothing,
                            .last = .nothing,
                        });
                        workspace.undo_stack.appendAssumeCapacity(.{ .pop = newcase.index });
                        Toybox.changeCoordinates(workspace.grabbing.index, .{}, Toybox.parentAbsolutePoint(dropzone_index));
                        Toybox.addChildLast(newcase.index, workspace.grabbing.index);
                        workspace.undo_stack.appendAssumeCapacity(.{ .pop = workspace.grabbing.index });
                    } else {
                        Toybox.changeCoordinates(workspace.grabbing.index, .{}, Toybox.parentAbsolutePoint(dropzone_index));
                        Toybox.refreshAbsolutePoints(&.{workspace.grabbing.index});
                        Toybox.changeChild(dropzone_index, workspace.grabbing.index);
                        workspace.undo_stack.appendAssumeCapacity(.{
                            .change_child = .{
                                .original = workspace.grabbing.index,
                                .new = dropzone_index,
                            },
                        });

                        const overwritten_data = Toybox.get(dropzone_index).*;
                        Toybox.destroyFloating(dropzone_index);
                        workspace.undo_stack.appendAssumeCapacity(.{
                            .recreate_floating = overwritten_data,
                        });
                    }
                } else if (!Toybox.isFloating(workspace.grabbing.index)) {
                    // Case B.2: releasing a grabbed thing, which might be a button
                    assert(dropzone_index == .nothing);
                    if (Toybox.get(workspace.grabbing.index).specific.as(.button)) |button| {
                        switch (button.action) {
                            .see_failing_testcase => {
                                const fnkbox = Toybox.findAncestor(workspace.grabbing.index, .fnkbox);
                                try workspace.launchTestcase(fnkbox.get().specific.fnkbox.status.unsolved);
                            },
                            .launch_testcase => {
                                // TODO: proper interaction with undo
                                const testcase_index = Toybox.get(workspace.grabbing.index).tree.parent;
                                try workspace.launchTestcase(testcase_index);
                            },
                        }
                    }
                } else {
                    // Case B.3: dropping a floating thing on fresh space
                    const target_area = hot_and_dropzone.over_background;
                    Toybox.changeCoordinates(workspace.grabbing.index, .{}, Toybox.get(target_area).absolute_point);
                    Toybox.addChildLast(target_area, workspace.grabbing.index);
                    Toybox.refreshAbsolutePoints(&.{workspace.grabbing.index});
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .pop = workspace.grabbing.index,
                    });
                }

                workspace.undo_stack.appendAssumeCapacity(.{ .set_grabbing = workspace.grabbing });
                workspace.undo_stack.appendAssumeCapacity(.{ .set_handlayer = workspace.hand_layer });
                workspace.grabbing.index = .nothing;
                workspace.hand_layer = .nothing;
            }
        }

        // const hovering: Lego.Index = if (workspace.focus.grabbing == .nothing) hovered_or_dropzone_thing.which else .nothing;
        // const dropzone: Lego.Index = if (workspace.focus.grabbing != .nothing) hovered_or_dropzone_thing.which else .nothing;

        // TODO: avoid computing this twice?
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

        // update _t
        // Should technically be inside updateSprings,
        // but I suspect this is faster (and simpler).
        for (toybox.all_legos.items) |*lego| {
            math.lerp_towards(&lego.hot_t, if (lego.index == hot_and_dropzone.hot) 1 else 0, 0.6, delta_seconds);
            math.lerp_towards(&lego.active_t, if (lego.index == workspace.grabbing.index) 1 else 0, 0.6, delta_seconds);
            math.lerp_towards(&lego.dropzone_t, if (lego.index == hot_and_dropzone.dropzone) 1 else 0, 0.6, delta_seconds);
            math.lerp_towards(&lego.dropping_t, if (lego.index == workspace.grabbing.index and hot_and_dropzone.dropzone != .nothing) 1 else 0, 0.6, delta_seconds);

            switch (lego.specific) {
                .sexpr => |*sexpr| {
                    math.lerp_towards(&sexpr.is_pattern_t, if (sexpr.is_pattern) 1 else 0, 0.6, delta_seconds);
                },
                .executor => |*executor| {
                    math.towards(&executor.garland_appearing_t, 1, delta_seconds / 0.4);
                },
                .area,
                .case,
                .newcase,
                .garland,
                .microscope,
                .lens,
                .fnkbox,
                .fnkbox_description,
                .fnkbox_box,
                .fnkbox_testcases,
                .button,
                .testcase,
                .postit,
                .postit_text,
                .executor_controls,
                .executor_brake,
                .executor_crank,
                => {},
            }
        }

        // TODO: a bit hacky
        if (true) { // set garlands visibility
            const grabbing_garland_or_case: bool = if (workspace.grabbing.index == .nothing)
                false
            else switch (Toybox.get(workspace.grabbing.index).specific) {
                .case, .garland => true,
                else => false,
            };
            for (toybox.all_legos.items) |*lego| {
                const in_toolbar = Toybox.oldestAncestor(lego.index) == workspace.toolbar_left;
                if (lego.specific.as(.garland)) |garland| {
                    garland.visible = !in_toolbar and (grabbing_garland_or_case or lego.tree.first != .nothing);
                }
            }
        }

        if (true) { // move camera and scroll stuff
            const over_scrollable_element: Lego.Index = for (toybox.all_legos.items) |lego| {
                if (!lego.exists) continue;
                if (lego.specific.tag() == .fnkbox_testcases and Lego.Specific.FnkboxBox.testcases_box.contains(
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
            const old_t = workspace.toolbar_left_unfolded_t;
            math.lerpTowards(
                &workspace.toolbar_left_unfolded_t,
                if (hot_and_dropzone.over_background == workspace.toolbar_left) 1 else 0,
                .slow,
                delta_seconds,
            );
            const new_t = workspace.toolbar_left_unfolded_t;
            if (old_t <= 0.01 and new_t > 0.01) {
                try workspace.regenerateToolbarLeft();
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

        // includes dragging and snapping to dropzone, since that's just the spring between the mouse cursor/dropzone and the grabbed thing
        workspace.updateSprings(workspace.roots(.all).constSlice(), mouse.cur.position, hot_and_dropzone, delta_seconds);

        if (true) { // start and advance fnkboxes animations
            try workspace.undo_stack.ensureUnusedCapacity(32);
            for (toybox.all_legos.items) |*lego| {
                if (lego.specific.as(.fnkbox)) |fnkbox| {
                    if (fnkbox.execution) |*execution| {
                        const executor_index = Lego.Specific.Fnkbox.children(lego.index).executor;
                        switch (execution.source) {
                            .testcase => |testcase| switch (execution.state) {
                                .scrolling_towards_case => {
                                    const offset_from_top: f32 = (Toybox.get(testcase).local_point.pos.y - Lego.Specific.FnkboxBox.relative_top_testcase_pos.y - 2) / 2.5;
                                    const offset_error = offset_from_top - math.clamp(offset_from_top, 0, Lego.Specific.FnkboxBox.visible_testcases - 1);
                                    if (offset_error == 0) {
                                        const new_input = try Toybox.dupeIntoFloating(Lego.Specific.Testcase.children(testcase).input, true);
                                        workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = new_input });
                                        Toybox.changeCoordinates(new_input, Toybox.get(testcase).absolute_point, Toybox.get(workspace.floating_inputs_layer).absolute_point);
                                        Toybox.addChildLast(workspace.floating_inputs_layer, new_input);
                                        execution.state = .starting;
                                        execution.state_t = 0;
                                        execution.original_or_final_input_point = Toybox.get(new_input).local_point;
                                        execution.floating_input_or_output = new_input;
                                    } else {
                                        const scroll = &Toybox.get(Toybox.get(testcase).tree.parent).specific.fnkbox_testcases.scroll_target;
                                        const target_scroll = scroll.* + offset_error;
                                        math.lerpTowards(scroll, target_scroll, .{ .duration = 0.5, .precision = 0.05 }, delta_seconds);
                                        math.towards(scroll, target_scroll, 0.1 * delta_seconds);
                                    }
                                },
                                .starting => {
                                    const input = execution.floating_input_or_output;
                                    Toybox.get(input).local_point = .lerp(
                                        execution.original_or_final_input_point,
                                        Toybox.get(workspace.floating_inputs_layer).absolute_point.inverseApplyGetLocal(
                                            Toybox.get(Lego.Specific.Executor.children(executor_index).input).absolute_point,
                                        ),
                                        execution.state_t,
                                    );
                                    execution.state_t += delta_seconds / 0.8;
                                    if (execution.state_t >= 1) {
                                        execution.state = .executing;
                                        execution.state_t = 0;
                                        Toybox.pop(input);
                                        Toybox.changeChild(Lego.Specific.Executor.children(executor_index).input, input);
                                        execution.floating_input_or_output = .nothing;
                                    }
                                },
                                .executing => {
                                    try advanceExecutorAnimation(executor_index, workspace, &workspace.undo_stack, delta_seconds);
                                    if (Toybox.get(executor_index).specific.executor.animation == null) {
                                        execution.state = .ending;
                                        execution.state_t = 0;

                                        const result = try workspace.resetExecutorAndExtractResult(executor_index, execution.original_garland);
                                        Toybox.changeCoordinates(
                                            result,
                                            .{},
                                            Toybox.get(workspace.floating_inputs_layer).absolute_point,
                                        );
                                        Toybox.addChildLast(workspace.floating_inputs_layer, result);
                                        Toybox.refreshAbsolutePoints(&.{result});
                                        // TODO: execution traces?
                                        // trace = try .fromExecutor(executor.prev_pills.items, null, .new(0, 0), 0.75, mem, hover_pool);

                                        execution.floating_input_or_output = result;
                                        execution.original_or_final_input_point = Toybox.get(workspace.floating_inputs_layer).absolute_point.inverseApplyGetLocal(
                                            Toybox.get(result).absolute_point,
                                        );
                                    }
                                },
                                .ending => {
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
                                    // TODO: also lerp the testcases scroll
                                    execution.state_t += delta_seconds / 0.8;
                                    if (execution.state_t >= 1) {
                                        // fnkbox.testcases.items[testcase_index].actual = execution.final_result;

                                        const new_actual = final_result;
                                        Toybox.changeCoordinates(new_actual, Toybox.get(executor_index).absolute_point, Toybox.get(testcase).absolute_point);
                                        Toybox.pop(new_actual);

                                        const old_actual = Lego.Specific.Testcase.children(testcase).actual;
                                        assert(Toybox.get(old_actual).specific.sexpr.kind == .empty);
                                        Toybox.changeChild(old_actual, new_actual);
                                        workspace.undo_stack.appendAssumeCapacity(.{ .change_child = .{
                                            .new = old_actual,
                                            .original = new_actual,
                                        } });
                                        fnkbox.execution = null;

                                        // TODO: call this somewhere else
                                        try fnkbox.updateStatus(workspace, scratch);
                                    }
                                },
                            },
                            .input => {
                                try advanceExecutorAnimation(executor_index, workspace, &workspace.undo_stack, delta_seconds);
                                if (Toybox.get(executor_index).specific.executor.animation == null) {
                                    const result = try workspace.resetExecutorAndExtractResult(executor_index, execution.original_garland);
                                    fnkbox.execution = null;
                                    Toybox.addChildLast(workspace.main_area, result);
                                    Toybox.changeCoordinates(result, .{}, workspace.main_area.get().absolute_point);
                                }
                            },
                        }
                    } else {
                        const executor_index = Lego.Specific.Fnkbox.children(lego.index).executor;
                        if (Lego.Specific.Executor.shouldStartExecution(executor_index)) {
                            const original_garland_index = Lego.Specific.Executor.children(executor_index).garland;
                            const backup_garland_index = try Toybox.dupeIntoFloating(original_garland_index, true);
                            workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = backup_garland_index });

                            fnkbox.execution = .{
                                .source = .input,
                                .original_garland = backup_garland_index,
                                .original_or_final_input_point = undefined,
                                .state_t = undefined,
                                .old_testcase_actual_value = undefined,
                                .state = .executing,
                            };

                            // TODO
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
            try workspace.undo_stack.ensureUnusedCapacity(32);
            for (toybox.all_legos.items) |*lego| {
                if (lego.specific.as(.executor)) |executor| {
                    if (executor.controlled_by_parent_fnkbox) continue;

                    if (executor.animation) |*animation| {
                        _ = animation;
                        @panic("TODO");
                    }

                    if (Lego.Specific.Executor.shouldStartExecution(lego.index)) {
                        @panic("TODO");
                    }
                }
            }
        }

        if (true) { // enable/disable buttons and other things
            for (toybox.all_legos.items) |*lego| {
                if (lego.specific.as(.button)) |button| {
                    button.enabled = switch (button.action) {
                        .launch_testcase => Toybox.get(Toybox.findAncestor(lego.index, .fnkbox)).specific.fnkbox.execution == null,
                        .see_failing_testcase => Toybox.get(Toybox.findAncestor(lego.index, .fnkbox)).specific.fnkbox.status == .unsolved,
                    };
                }
                if (lego.specific.as(.executor_crank)) |crank| {
                    crank.enabled = Toybox.findAncestor(lego.index, .executor).get().specific.executor.animation != null;
                }
            }
        }

        const something_happened = (workspace.undo_stack.items.len != old_undo_count);
        if (something_happened) {
            try workspace.canonizeAfterChanges(scratch);
        }

        if (true) { // set lenses data
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
                    .include_toolbar = true,
                    .include_floating_inputs = true,
                    .include_lenses = false,
                }).constSlice());
                try all_roots.appendSlice(allocator, microscopes[0..k]);

                var all_roots_except_hand: std.ArrayListUnmanaged(Lego.Index) = .empty;
                try all_roots_except_hand.appendSlice(allocator, workspace.roots(.{
                    .include_hand = false,
                    .include_toolbar = true,
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

    fn regenerateToolbarLeft(workspace: *Workspace) !void {
        try workspace.undo_stack.ensureUnusedCapacity(32);

        if (true) { // delete all current children
            var cur = Toybox.get(workspace.toolbar_left).tree.first;
            while (cur != .nothing) {
                const original_tree = Toybox.get(cur).tree;
                Toybox.pop(cur);
                workspace.undo_stack.appendAssumeCapacity(.{
                    .insert = .{
                        .what = cur,
                        .where = original_tree,
                    },
                });
                cur = original_tree.next;
            }
        }

        if (true) { // add a fresh case
            const new_name = try workspace.arena_for_atom_names.allocator().alloc(u8, 32);
            math.Random.init(workspace.random_instance.random()).alphanumeric_bytes(new_name);

            const index = try Toybox.buildCase(.{ .pos = .new(2.5, 5) }, .{
                .pattern = try Toybox.buildSexpr(.{}, .{ .atom_var = new_name }, true),
                .template = try Toybox.buildSexpr(.{}, .{ .pair = .{
                    .up = try Toybox.buildSexpr(.{}, .{ .atom_var = new_name }, false),
                    .down = try Toybox.buildSexpr(.{}, .{ .atom_lit = "nil" }, false),
                } }, false),
                .fnkname = try Toybox.buildSexpr(.{}, .empty, false),
                .next = null,
            });

            workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = index });
            Toybox.addChildLast(workspace.toolbar_left, index);
            workspace.undo_stack.appendAssumeCapacity(.{ .pop = index });
        }
    }

    fn advanceExecutorAnimation(executor_index: Lego.Index, workspace: *Workspace, undo_stack: *UndoStack, delta_seconds: f32) !void {
        const floating_inputs_layer = workspace.floating_inputs_layer;
        const Executor = Lego.Specific.Executor;
        const executor = &Toybox.get(executor_index).specific.executor;
        if (executor.animation) |*animation| {
            animation.t += delta_seconds * Executor.Controls.speedScale(Executor.getBrakeT(executor_index));
            if (animation.t >= 1) {
                if (animation.matching) {
                    // TODO
                    // try executor.prev_pills.append(mem.gpa, .{
                    //     .pattern = animation.active_case.pattern,
                    //     .input = executor.input,
                    //     .fnkname_call = animation.active_case.fnk_name,
                    //     .fnkname_response = animation.garland_fnkname,
                    //     // TODO: should include previous bindings? not really, since they have now been merged
                    //     .bindings = try mem.gpa.dupe(Binding, animation.new_bindings),
                    // });

                    // TODO
                    if (true) { // fill variables
                        var cur = animation.active_case;
                        while (cur != .nothing) : (cur = Toybox.next_preordered(cur, animation.active_case).next) {
                            if (Toybox.get(cur).specific.as(.sexpr)) |sexpr| {
                                if (sexpr.emerging_value != .nothing) {
                                    Toybox.changeChildWithUndoAndAlsoCoords(cur, sexpr.emerging_value, &workspace.undo_stack);
                                }
                            }
                        }
                    }
                    // try animation.active_case.fillVariables(animation.new_bindings, mem);

                    const next_garland = animation.active_case.case().next;
                    const old_active_case = animation.active_case;
                    const old_input = Executor.children(executor_index).input;
                    const old_garland = Executor.children(executor_index).garland;

                    const new_input = Lego.Specific.Case.children(old_active_case).template;
                    Toybox.popWithUndoAndChangingCoords(new_input, undo_stack);

                    Toybox.changeChildWithUndoAndAlsoCoords(
                        old_input,
                        new_input,
                        undo_stack,
                    );

                    Toybox.popWithUndo(old_active_case, undo_stack);
                    Toybox.destroyFloatingWithUndo(old_active_case, undo_stack);

                    const new_garland = blk: {
                        // TODO
                        if (animation.invoked_fnk != .nothing) {
                            Toybox.pop(animation.invoked_fnk);
                            if (next_garland != .nothing) {
                                // TODO
                                // Toybox.get(next_garland).specific.garland.enqueued_parent_pill_index = ??;
                                Toybox.get(next_garland).specific.garland.next_enqueued = executor.first_enqueued;
                                executor.first_enqueued = next_garland;
                            }
                            break :blk animation.invoked_fnk;
                        } else if (next_garland.garland().hasChildCases()) {
                            Toybox.pop(next_garland);
                            // TODO
                            // parent_pill_index = executor.prev_pills.items.len - 1;
                            break :blk next_garland;
                        } else if (executor.first_enqueued != .nothing) {
                            const asdf = executor.first_enqueued;
                            executor.first_enqueued = Toybox.get(asdf).specific.garland.next_enqueued;
                            // TODO
                            // parent_pill_index = Toybox.get(asdf).specific.garland.enqueued_parent_pill_index;
                            Toybox.pop(asdf);
                            break :blk asdf;
                        } else {
                            break :blk try Toybox.buildGarland(.{}, &.{});
                        }
                    };

                    Toybox.changeChildWithUndo(old_garland, new_garland, undo_stack);
                } else {
                    // TODO
                    // assert(animation.new_bindings.len == 0);
                    // executor.garland.fnkname = animation.garland_fnkname;
                }
                executor.animation = null;
            }
        }

        if (Executor.shouldStartExecution(executor_index)) {
            const value = Lego.Specific.Executor.children(executor_index).input;

            // pop first case for execution
            const garland_index = Executor.children(executor_index).garland;
            const first_segment = Toybox.get(garland_index).tree.first;
            undo_stack.appendAssumeCapacity(.{ .insert = .{
                .where = Toybox.get(first_segment).tree,
                .what = first_segment,
            } });
            Toybox.pop(first_segment);
            const first_case = Toybox.get(first_segment).tree.first;
            undo_stack.appendAssumeCapacity(.{ .insert = .{
                .where = Toybox.get(first_case).tree,
                .what = first_case,
            } });
            Toybox.pop(first_case);
            Toybox.addChildLast(floating_inputs_layer, first_case);
            undo_stack.appendAssumeCapacity(.{ .pop = first_case });
            Toybox.get(Toybox.get(garland_index).tree.first).specific.newcase.length_before += Toybox.get(first_segment).specific.newcase.length();

            const pattern = Lego.Specific.Case.children(first_case).pattern;

            // TODO: memory management
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
                    Toybox.addChildLast(workspace.floating_inputs_layer, garland);
                    break :blk garland;
                } else @panic("TODO: handle this");
            };
            // const garland_fnkname: Lego.Index = .nothing; // TODO
            // executor.garland.fnkname = null; // TODO
            const new_bindings_slice = try new_bindings.toOwnedSlice();
            executor.animation = .{
                .matching = matching,
                .active_case = first_case,
                .invoked_fnk = invoked_fnk,
                .new_bindings = new_bindings_slice,
            };

            if (matching) {
                var cur = first_case;
                while (cur != .nothing) : (cur = Toybox.next_preordered(cur, first_case).next) {
                    if (Toybox.get(cur).specific.as(.sexpr)) |sexpr| {
                        sexpr.executor_with_bindings = executor_index;
                        if (sexpr.kind == .atom_var and !sexpr.is_pattern) {
                            for (new_bindings_slice) |binding| {
                                if (std.mem.eql(u8, binding.name, sexpr.atom_name)) {
                                    sexpr.emerging_value = try Toybox.dupeIntoFloating(binding.value, true);
                                    Toybox.setAbsolutePoint(sexpr.emerging_value, Toybox.get(cur).absolute_point);
                                    Toybox.refreshAbsolutePoints(&.{sexpr.emerging_value});
                                }
                            }
                        }
                    }
                }
            }
        }

        Executor.children(executor_index).controls.get().specific.executor_controls
            .crank().get().specific.executor_crank.value = if (executor.animation) |anim| anim.t else 0;
    }

    fn resetExecutorAndExtractResult(workspace: *Workspace, executor_index: Lego.Index, original_garland: Lego.Index) !Lego.Index {
        const result = Lego.Specific.Executor.children(executor_index).input;

        Toybox.changeChildWithUndoAndAlsoCoords(
            result,
            try Toybox.buildSexpr(.{}, .empty, false),
            &workspace.undo_stack,
        );

        const children = Lego.Specific.Executor.children(executor_index);
        // const executor = &Toybox.get(executor_index).specific.executor;
        Toybox.changeChildWithUndo(children.garland, original_garland, &workspace.undo_stack);
        Toybox.changeChildWithUndoAndAlsoCoords(
            children.input,
            try Toybox.buildSexpr(
                Lego.Specific.Executor.relative_input_point,
                .empty,
                false,
            ),
            &workspace.undo_stack,
        );
        Toybox.get(executor_index).specific.executor.garland_appearing_t = -1;
        // TODO
        // fnkbox.executor.prev_pills.clearRetainingCapacity();
        // fnkbox.executor.enqueued_stack.clearRetainingCapacity();

        return result;
    }

    fn getGarlandForFnk(
        workspace: *Workspace,
        fnkname: Lego.Index,
        new_point: Point,
    ) !?Lego.Index {
        _ = workspace;
        _ = new_point;
        for (toybox.all_legos.items) |*lego| {
            if (!lego.exists) continue;
            if (lego.specific.as(.fnkbox)) |fnkbox| {
                if (Lego.Specific.Sexpr.equalValue(fnkbox.fnkname(), fnkname)) {
                    const garland = try Toybox.dupeIntoFloating(if (fnkbox.execution) |e|
                        e.original_garland
                    else
                        fnkbox.executor().garland().index, true);
                    // TODO
                    // garland.fnkname = try .fromSexpr(hover_pool, fnkname, .{}, true);
                    // garland.kinematicUpdate(new_point, null, undefined);
                    return garland;
                }
            }
        } else return null;
    }

    fn launchTestcase(workspace: *Workspace, testcase_index: Lego.Index) !void {
        assert(Toybox.get(testcase_index).specific.tag() == .testcase);
        const fnkbox_index = Toybox.findAncestor(testcase_index, .fnkbox);
        const fnkbox = &Toybox.get(fnkbox_index).specific.fnkbox;
        assert(fnkbox.execution == null);
        const executor_index = Lego.Specific.Fnkbox.children(fnkbox_index).executor;

        const old_actual = Lego.Specific.Testcase.children(testcase_index).actual;
        const new_actual = try Toybox.buildSexpr(Toybox.get(old_actual).local_point, .empty, false);
        Toybox.changeChild(old_actual, new_actual);
        workspace.undo_stack.appendAssumeCapacity(.{ .change_child = .{
            .original = new_actual,
            .new = old_actual,
        } });

        const original_garland_index = Lego.Specific.Executor.children(executor_index).garland;
        const backup_garland_index = try Toybox.dupeIntoFloating(original_garland_index, true);
        workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = backup_garland_index });

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

// TODO: take gl parameter
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

// TODO: rethink
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
        // TODO: these numbers are not exact, issues when zooming in
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
