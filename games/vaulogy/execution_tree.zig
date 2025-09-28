pub const ExecutionTree = struct {
    input_raw: *const Sexpr,
    input_filled: *const Sexpr,

    old_bindings: []const core.Binding,
    new_bindings: []const core.Binding,
    all_bindings: []const core.Binding,

    current_fn_name: *const Sexpr,

    cases: []const core.MatchCaseDefinition,
    matched_index: usize,

    matched: struct {
        pattern: *const Sexpr,
        raw_template: *const Sexpr,
        filled_template: *const Sexpr,
        funk_tangent: ?struct {
            fn_name: *const Sexpr,
            tree: *const ExecutionTree,
        },
        next: ?*const ExecutionTree,
    },

    test "v1" {
        var sut: TestHelper = try .init(std.testing.allocator,
            \\ not {
            \\     true -> false;
            \\     false -> true;
            \\ }
        , "not", "true");
        defer sut.deinit();
        for ([_]f32{ 0, 0.5, 1 }) |t| {
            const actual_displacement = try sut.displacementAt(t);
            try std.testing.expectEqual(t, actual_displacement);
        }
    }

    test "v2" {
        var sut: TestHelper = try .init(std.testing.allocator,
            \\ not {
            \\     true -> false;
            \\     false -> true;
            \\ }
            \\ notnot {
            \\     @a -> not: @a {
            \\         @b -> not: @b;
            \\     }
            \\ }
        , "notnot", "true");
        defer sut.deinit();
        for ([_]f32{ 0, 0.5, 1, 1.5, 2 }) |t| {
            const actual_displacement = try sut.displacementAt(t);
            try std.testing.expectEqual(t, actual_displacement);
        }
    }

    test "v3" {
        var sut: TestHelper = try .init(std.testing.allocator,
            \\ not {
            \\     true -> false;
            \\     false -> true;
            \\ }
            \\ doublenot {
            \\     @a -> not: @a {
            \\         @b -> (@b . @b);
            \\     }
            \\ }
        , "doublenot", "true");
        defer sut.deinit();
        for ([_]f32{ 0, 0.5, 1, 1.5, 2 }) |t| {
            const actual_displacement = try sut.displacementAt(t);
            try std.testing.expectEqual(t, actual_displacement);
        }
    }

    pub fn buildNewStack(scoring_run: *core.ScoringRun, fn_name: *const Sexpr, input: *const Sexpr) error{
        OutOfMemory,
        BAD_INPUT,
        FnkNotFound,
        NoMatchingCase,
        InvalidMetaFnk,
        UsedUndefinedVariable,
    }!ExecutionTree {
        const func = try scoring_run.findFunktion(fn_name);
        const result: ExecutionTree = try .buildExtending(scoring_run, func.cases.items, input, &.{}, fn_name);
        return result;
    }

    fn getLast(self: ExecutionTree) *const Sexpr {
        const matched = self.matched;
        if (matched.next) |next| {
            return next.getLast();
        } else if (matched.funk_tangent) |fnk| {
            return fnk.tree.getLast();
        } else return matched.filled_template;
    }

    pub fn buildExtending(
        scoring_run: *core.ScoringRun,
        cases: []const core.MatchCaseDefinition,
        input: *const Sexpr,
        incoming_bindings: []const core.Binding,
        current_fn_name: *const Sexpr,
    ) !ExecutionTree {
        for (cases, 0..) |case, case_index| {
            var new_bindings: std.ArrayList(core.Binding) = .init(scoring_run.mem.gpa);
            if (try core.generateBindings(case.pattern, input, &new_bindings)) {
                const bindings = try scoring_run.mem.gpa.alloc(core.Binding, incoming_bindings.len + new_bindings.items.len);
                @memcpy(bindings[0..incoming_bindings.len], incoming_bindings);
                @memcpy(bindings[incoming_bindings.len..], new_bindings.items);
                const argument = try core.fillTemplateV2(case.template, bindings, &scoring_run.mem.pool_for_sexprs);

                const funk_tangent: ?ExecutionTree = if (case.fnk_name.equals(Sexpr.builtin.identity))
                    null
                else
                    try .buildNewStack(scoring_run, case.fnk_name, argument);

                const next_input = if (funk_tangent) |t| t.getLast() else argument;

                const next_tree: ?ExecutionTree = if (case.next) |next|
                    try .buildExtending(scoring_run, next.items, next_input, bindings, current_fn_name)
                else
                    null;

                return .{
                    .current_fn_name = current_fn_name,
                    .all_bindings = bindings,
                    .old_bindings = incoming_bindings,
                    .new_bindings = try new_bindings.toOwnedSlice(),
                    .input_raw = input,
                    .input_filled = input,
                    .cases = cases,
                    .matched_index = case_index,
                    .matched = .{
                        .pattern = case.pattern,
                        .raw_template = case.template,
                        .filled_template = argument,
                        .funk_tangent = if (funk_tangent) |t| blk: {
                            const ptr = try scoring_run.mem.gpa.create(ExecutionTree);
                            ptr.* = t;
                            break :blk .{
                                .fn_name = case.fnk_name,
                                .tree = ptr,
                            };
                        } else null,
                        .next = if (next_tree) |t| blk: {
                            const ptr = try scoring_run.mem.gpa.create(ExecutionTree);
                            ptr.* = t;
                            break :blk ptr;
                        } else null,
                    },
                };
            } else {
                new_bindings.deinit();
            }
        } else @panic("nope");
    }

    pub fn buildFromText(scoring_run: *core.ScoringRun, fn_name_raw: []const u8, input_raw: []const u8) !ExecutionTree {
        var permanent_stuff = scoring_run.mem;
        const fn_name = try parsing.parseSingleSexpr(fn_name_raw, &permanent_stuff.pool_for_sexprs);
        const input = try parsing.parseSingleSexpr(input_raw, &permanent_stuff.pool_for_sexprs);
        return try .buildNewStack(scoring_run, fn_name, input);
    }

    const DrawList = std.ArrayList(union(enum) {
        physical: PhysicalSexpr,
        case: struct {
            case: core.MatchCaseDefinition,
            pattern_point: Point,
            bindings: BindingsState,
        },
        templated: struct {
            point: Point,
            bindings: BindingsState,
            template: *const Sexpr,
        },
        fnk_name: struct {
            point: Point,
            value: *const Sexpr,
        },

        pub fn draw(self: @This(), drawer: *Drawer, camera: Rect) !void {
            switch (self) {
                .physical => |p| try drawer.drawSexpr(camera, p),
                .case => |c| try drawCase(drawer, camera, c.pattern_point, c.case, c.bindings, 1.0, null),
                .templated => |t| try drawer.drawTemplateSexprWithBindings(camera, t.point, t.template, t.bindings),
                .fnk_name => |f| try drawer.drawHoldedFnk(camera, f.point, 1.0, f.value),
            }
        }
    });

    pub fn drawAsExecutingThread(self: ExecutionTree, drawer: *Drawer, camera: Rect, input_point: Point, t: f32) !void {
        var shapes: DrawList = .init(drawer.canvas.frame_arena.allocator());
        const asdf = try self.drawAsExecutingThreadInternal(input_point, t, &shapes);
        for (shapes.items) |s| {
            try s.draw(drawer, camera.move(.new(asdf.displacement * 5, 0)));
        }
    }

    const TestHelper = struct {
        tree: ExecutionTree,
        shapes: DrawList,
        core_mem: core.VeryPermamentGameStuff,
        scoring_run: core.ScoringRun,
        // TODO: fix mem of scoring_run
        arena: std.heap.ArenaAllocator,

        pub fn init(
            gpa: std.mem.Allocator,
            all_fnks: []const u8,
            fn_name: []const u8,
            input: []const u8,
        ) !TestHelper {
            var arena: std.heap.ArenaAllocator = .init(gpa);
            const shapes: DrawList = .init(arena.allocator());
            var core_mem: core.VeryPermamentGameStuff = .init(arena.allocator());
            var scoring_run: core.ScoringRun = try .init(all_fnks, &core_mem);
            const tree: ExecutionTree = try .buildFromText(&scoring_run, fn_name, input);
            return .{
                .arena = arena,
                .tree = tree,
                .shapes = shapes,
                .core_mem = core_mem,
                .scoring_run = scoring_run,
            };
        }

        pub fn deinit(self: *TestHelper) void {
            // self.shapes.deinit();
            // self.scoring_run.deinit(true);
            // self.core_mem.deinit();
            self.arena.deinit();
        }

        pub fn displacementAt(test_helper: *TestHelper, t: f32) !f32 {
            test_helper.shapes.clearAndFree();
            const asdf = try test_helper.tree.drawAsExecutingThreadInternal(.{}, t, &test_helper.shapes, test_helper.arena.allocator());
            return asdf.displacement;
        }
    };

    const DrawInfo = struct {
        queued_nexts: f32,
        displacement: f32,
        state: union(enum) {
            fully_consumed,
            right_before_exiting: f32,
            exited: struct {
                remaining_t: f32,
            },
        },
    };

    fn drawAsExecutingThreadInternal(self: ExecutionTree, original_input_point: Point, original_t: f32, out: *DrawList, scratch: std.mem.Allocator) !DrawInfo {
        _ = original_input_point;
        var active = self;
        var queued: std.ArrayList(*const ExecutionTree) = .init(scratch);
        // var queued: std.BoundedArray(*const ExecutionTree, 10) = .{};
        _ = out;

        var displacement: f32 = 0;
        var remaining_t = original_t;
        // std.log.err("remaining t: {d}", .{remaining_t});
        while (@floor(remaining_t) > tof32(active.matched_index)) {
            remaining_t -= tof32(active.matched_index + 1);
            // std.log.err("remaining t now: {d}", .{remaining_t});
            if (active.matched.funk_tangent) |funk_tangent| {
                if (active.matched.next) |next| {
                    try queued.append(next);
                }
                active = funk_tangent.tree.*;
            } else if (active.matched.next) |next| {
                active = next.*;
            } else if (queued.pop()) |q| {
                active = q.*;
            } else return .{
                .queued_nexts = tof32(queued.items.len),
                .displacement = 1 + displacement,
                .state = .fully_consumed,
            };
            displacement += 1;
        }
        // std.debug.panic("hola", .{});
        if (@floor(remaining_t) < tof32(active.matched_index)) {
            // pass
        } else {
            assert(@floor(remaining_t) == tof32(active.matched_index));
            displacement += @mod(remaining_t, 1);
        }

        return .{
            .queued_nexts = tof32(queued.items.len),
            .displacement = displacement,
            .state = .fully_consumed,
        };
    }

    fn drawCase(
        drawer: *Drawer,
        camera: Rect,
        pattern_point: Point,
        case: core.MatchCaseDefinition,
        bindings: BindingsState,
        unfolded: f32,
        invoking_next: ?struct {
            t: f32,
            cases: []const core.MatchCaseDefinition,
            next_cases_pattern_point_ptr: *Point,
        },
    ) !void {
        assert(math.in01(unfolded));

        try drawer.drawSexpr(camera, .{
            .is_pattern = 1,
            .value = case.pattern,
            .pos = pattern_point,
        });

        if (unfolded > 0.1) {
            try drawer.drawTemplateSexprWithBindings(
                camera,
                pattern_point.applyToLocalPoint(.{ .pos = .new(2, 0) }),
                case.template,
                bindings,
            );

            if (invoking_next) |invoking| {
                try drawer.drawHoldedFnk(camera, pattern_point.applyToLocalPoint(.{ .pos = .new(0, -3 * invoking.t) }).applyToLocalPoint(.{
                    .pos = .new(5, 0),
                    .turns = -0.25,
                    .scale = 0.5,
                }).applyToLocalPoint(.{ .scale = 1.0 - math.remapTo01Clamped(invoking.t, 0.5, 1.0) }), 0, case.fnk_name);

                const offset = (1.0 - invoking.t) + 2.0 * math.smoothstepEased(invoking.t, 0.4, 0.0, .linear);
                for (invoking.cases, 0..) |next_case, k| {
                    _ = try drawCase(drawer, camera, pattern_point.applyToLocalPoint(
                        .{ .pos = .new(6, 3 * (offset + tof32(k))) },
                    ), next_case, .none, if (k == 0) 1 else 0, null);
                }

                if (case.next) |next| {
                    assert(next.items.len > 0);
                    try drawer.drawSexpr(camera, .{
                        .is_pattern = 1,
                        .value = next.items[0].pattern,
                        .pos = pattern_point.applyToLocalPoint(
                            .{ .pos = .new(8 + 10 * math.smoothstepEased(invoking.t, 0.0, 0.5, .easeInOutCubic), 0) },
                        ),
                    });

                    invoking.next_cases_pattern_point_ptr.* = invoking.next_cases_pattern_point_ptr.*.applyToLocalPoint(
                        .{ .pos = .new(10 * math.smoothstepEased(invoking.t, 0.0, 0.5, .easeInOutCubic), 0) },
                    );

                    // if (next.items.len > 0) {
                    //     try drawer.drawSexpr(camera, .{
                    //         .is_pattern = 1,
                    //         .value = next.items[0].pattern,
                    //         .pos = invoking.next_cases_pattern_point_ptr.*,
                    //     });
                    // }
                } else {
                    invoking.next_cases_pattern_point_ptr.* = invoking.next_cases_pattern_point_ptr.*.applyToLocalPoint(
                        .{ .pos = .new(5 * math.smoothstepEased(invoking.t, 0.0, 0.5, .easeInOutCubic), 0) },
                    );
                }

                // return pattern_point.applyToLocalPoint(
                //     .{ .pos = .new(8 + 5 * math.smoothstepEased(invoking.t, 0.0, 0.5, .easeInOutCubic), 0) },
                // );

                // if (case.next) |next| {
                //     for (next.items, 0..) |next_case, k| {
                //         try drawCase(drawer, camera, pattern_point.applyToLocalPoint(
                //             .{ .pos = .new(8 + 5 * math.smoothstepEased(invoking.t, 0.0, 0.5, .easeInOutCubic), 3 * tof32(k)) },
                //         ), next_case, bindings, if (k == 0) 1 else 0, null);
                //     }
                // }
            } else {
                try drawer.drawHoldedFnk(camera, pattern_point.applyToLocalPoint(.{
                    .pos = .new(5, 0),
                    .turns = -0.25,
                    .scale = 0.5,
                }), 0, case.fnk_name);

                if (case.next) |next| {
                    if (next.items.len > 0) {
                        try drawer.drawSexpr(camera, .{
                            .is_pattern = 1,
                            .value = next.items[0].pattern,
                            .pos = pattern_point.applyToLocalPoint(.{ .pos = .new(8, 0) }),
                        });
                    }
                    // for (next.items, 0..) |next_case, k| {
                    //     try drawCase(drawer, camera, pattern_point.applyToLocalPoint(
                    //         .{ .pos = .new(8, 3 * tof32(k)) },
                    //     ), next_case, bindings, if (k == 0) 1 else 0, null);
                    // }
                }
            }
        }
    }
};

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

const core = @import("core.zig");
const Drawer = @import("Drawer.zig");
const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const PhysicalSexpr = @import("physical.zig").PhysicalSexpr;
const ViewHelper = @import("physical.zig").ViewHelper;
const BindingsState = @import("physical.zig").BindingsState;
