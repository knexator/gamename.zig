pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

const core = @import("core.zig");
const Drawer = @import("Drawer.zig");

pub const tracy = @import("tracy");

pub const display_fps = true;

const EXECUTOR_MOVES_LEFT = true;
const SEQUENTIAL_GOES_DOWN = true;
const CRANKS_ENABLED = true;

const FuzzerContext = struct {
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
            var workspace: Workspace = undefined;
            try workspace.init(std.testing.allocator, std.testing.random_seed);
            return .{ .workspace = workspace, .test_platform = .{} };
        }

        pub fn deinit(player: *Player) void {
            player.workspace.deinit();
            player.test_platform.frame_arena.deinit();
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

drawer: Drawer,
workspace: Workspace,

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
    // active_t: f32 = 0,
    /// 1 if this element is being dropped into another
    dropping_t: f32 = 0,

    /// only valid for Legos in a collection
    ll_parent: Lego.Index = .nothing,
    /// only valid for Legos in a collection
    ll_prev: Lego.Index = .nothing,
    /// only valid for Legos in a collection
    ll_next: Lego.Index = .nothing,

    /// only valid for Legos containing a collection
    ll_first: Lego.Index = .nothing,
    /// only valid for Legos containing a collection
    ll_last: Lego.Index = .nothing,

    specific: Specific,

    const UndoStack = Workspace.UndoStack;

    pub const Specific = union(enum) {
        button: Button,
        area: Area,
        sexpr: Sexpr,
        case: Case,
        garland: Garland,
        /// cable between cases, and the handle to create new ones
        newcase: NewCase,
        microscope: Microscope,
        lens: Lens,

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

            pub fn children(_: *const Button, _: *Toybox, _: *std.ArrayListUnmanaged(Lego.Index), _: Allocator) !void {}
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

            pub fn children(area: *const Area, toybox: *Toybox, dst: *std.ArrayListUnmanaged(Lego.Index), allocator: Allocator) !void {
                var cur = Lego.fromSpecificConst(.area, area).ll_first;
                while (cur != .nothing) {
                    try dst.append(allocator, cur);
                    cur = toybox.get(cur).ll_next;
                }
            }
        };

        pub const Sexpr = struct {
            kind: Kind,
            is_pattern: bool,
            is_pattern_t: f32,

            pub const Kind = union(enum) {
                empty,
                atom_lit: []const u8,
                atom_var: []const u8,
                pair: struct { up: Lego.Index, down: Lego.Index },
            };

            pub fn contains(sexpr_point: Point, is_pattern: bool, kind: Kind, needle_pos: Vec2) bool {
                return ViewHelper.overlapsAtom(is_pattern, sexpr_point, needle_pos, switch (kind) {
                    .atom_var, .atom_lit, .empty => .atom,
                    .pair => .pair,
                });
            }

            pub fn children(sexpr: *const Sexpr, _: *Toybox, dst: *std.ArrayListUnmanaged(Lego.Index), allocator: Allocator) !void {
                switch (sexpr.kind) {
                    else => {},
                    .pair => |pair| {
                        try dst.append(allocator, pair.up);
                        try dst.append(allocator, pair.down);
                    },
                }
            }

            pub fn emptyAndClone(sexpr: *Sexpr, toybox: *Toybox, undo_stack: ?*UndoStack) !Lego.Index {
                const new = try toybox.add(undefined, undefined);
                new.overwriteMostData(Lego.fromSpecific(.sexpr, sexpr), null);
                if (undo_stack) |s| {
                    s.appendAssumeCapacity(.{ .destroy_floating = new.index });
                }

                Lego.fromSpecific(.sexpr, sexpr).overwriteSpecificData(.{ .sexpr = .{
                    .kind = .empty,
                    .is_pattern = sexpr.is_pattern,
                    .is_pattern_t = sexpr.is_pattern_t,
                } }, undo_stack);
                return new.index;
            }

            // fn overwriteKind(sexpr: *Sexpr, kind: Kind, undo_stack: ?*UndoStack) void {
            //     if (undo_stack) |s| {
            //         s.appendAssumeCapacity(.{ .restore_sexpr_kind = .{
            //             .index = Lego.fromSpecific(.sexpr, sexpr).index,
            //             .kind = sexpr.kind,
            //         } });
            //     }
            //     sexpr.kind = kind;
            // }
        };

        pub const Case = struct {
            pattern: Lego.Index,
            template: Lego.Index,
            fnkname: Lego.Index,
            next: Lego.Index,

            pub fn children(case: *const Case, _: *Toybox, dst: *std.ArrayListUnmanaged(Lego.Index), allocator: Allocator) !void {
                try dst.appendSlice(allocator, &.{
                    case.pattern,
                    case.template,
                    case.fnkname,
                    case.next,
                });
            }
        };

        pub const Garland = struct {
            visible: bool = undefined,
            computed_height: f32 = 0,
            /// used when updating animation
            offset_t: f32 = 0,
            /// used when updating animation
            offset_ghost: Lego.Index = .nothing,

            pub fn children(garland: *const Garland, toybox: *Toybox, dst: *std.ArrayListUnmanaged(Lego.Index), allocator: Allocator) !void {
                var cur = Lego.fromSpecificConst(.garland, garland).ll_first;
                while (cur != .nothing) {
                    try dst.append(allocator, cur);
                    cur = toybox.get(cur).ll_next;
                }
            }

            pub fn emptyAndClone(garland: *Garland, toybox: *Toybox, undo_stack: ?*UndoStack) !Lego.Index {
                const old = Lego.fromSpecific(.garland, garland);
                const new = try toybox.add(undefined, undefined);
                // FIXME: this API could be much better
                new.stealMostData(old, toybox, null);

                if (undo_stack) |s| {
                    s.appendAssumeCapacity(.{ .destroy_floating = new.index });
                }

                try garland.setCases(&.{}, toybox, undo_stack);

                return new.index;
            }

            pub fn setCases(garland: *Garland, child_cases: []const Lego.Index, toybox: *Toybox, undo_stack: ?*UndoStack) !void {
                if (undo_stack) |_| {
                    // FIXME
                }

                const lego = Lego.fromSpecific(.garland, garland);
                assert(!lego.hasCollection());
                for (child_cases) |case| {
                    const new_segment = try toybox.add(.{}, .{ .newcase = .{
                        .case = case,
                    } });

                    lego.addChildLast(toybox, new_segment.index, null);
                }
                lego.addChildLast(toybox, (try toybox.add(.{}, .{ .newcase = .{
                    .case = .nothing,
                } })).index, null);
            }
        };

        pub const NewCase = struct {
            length: f32 = undefined,
            /// if 1, the newcase handle is right at the end of the cable
            handle_t: f32 = undefined,
            case: Lego.Index,

            pub fn children(newcase: *const NewCase, _: *Toybox, dst: *std.ArrayListUnmanaged(Lego.Index), allocator: Allocator) !void {
                if (newcase.case != .nothing) {
                    try dst.append(allocator, newcase.case);
                }
            }
        };

        pub const Microscope = struct {
            source: Lego.Index,
            target: Lego.Index,

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

            pub fn children(microscope: *const Microscope, _: *Toybox, dst: *std.ArrayListUnmanaged(Lego.Index), allocator: Allocator) !void {
                try dst.append(allocator, microscope.source);
                try dst.append(allocator, microscope.target);
            }
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

            pub fn children(_: *const Lens, _: *Toybox, _: *std.ArrayListUnmanaged(Lego.Index), _: Allocator) !void {}
        };
    };

    pub const Index = enum(u32) {
        nothing = std.math.maxInt(u32),
        _,

        pub fn asI32(index: Index) i32 {
            return @bitCast(@intFromEnum(index));
        }

        pub fn format(
            self: @This(),
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (fmt.len != 0) std.debug.panic("unexpected fmt: {s}", .{fmt});
            assert(fmt.len == 0);
            assert(std.meta.eql(options, .{}));
            if (self == .nothing) {
                try writer.writeAll("X");
            } else {
                try writer.print("{d}", .{@intFromEnum(self)});
            }
        }
    };

    pub fn overwriteableByDropping(lego: *const Lego) bool {
        return switch (lego.specific) {
            .sexpr, .garland => true,
            else => false,
        };
    }

    pub fn overwrite(lego: *Lego, new: Lego.Index, toybox: *Toybox, undo_stack: ?*UndoStack) void {
        assert(lego.overwriteableByDropping());
        lego.overwriteMostData(toybox.get(new), undo_stack);
        toybox.free(new, undo_stack);
    }

    pub fn pop(lego: *Lego, toybox: *Toybox, child: Lego.Index, undo_stack: ?*UndoStack) void {
        assert(switch (lego.specific) {
            .area => true,
            .garland => toybox.get(child).specific.tag() == .newcase,
            else => false,
        });

        if (undo_stack) |s| {
            s.appendAssumeCapacity(.{ .insert = .{
                .ll_prev = toybox.get(child).ll_prev,
                .ll_next = toybox.get(child).ll_next,
                .ll_parent = lego.index,
                .self = child,
            } });
        }

        assert(lego.ll_first != .nothing and lego.ll_last != .nothing);
        const old_prev = toybox.get(child).ll_prev;
        const old_next = toybox.get(child).ll_next;
        toybox.get(child).ll_next = .nothing;
        toybox.get(child).ll_prev = .nothing;
        toybox.get(child).ll_parent = .nothing;

        if (old_prev != .nothing) {
            assert(lego.ll_first != child);
            toybox.get(old_prev).ll_next = old_next;
        } else {
            assert(lego.ll_first == child);
            lego.ll_first = old_next;
        }

        if (old_next != .nothing) {
            assert(lego.ll_last != child);
            toybox.get(old_next).ll_prev = old_prev;
        } else {
            assert(lego.ll_last == child);
            lego.ll_last = old_prev;
        }
    }

    pub fn insert(lego: *Lego, toybox: *Toybox, child_index: Lego.Index, ll_prev: Lego.Index, ll_next: Lego.Index, undo_stack: ?*UndoStack) void {
        assert(switch (lego.specific) {
            .area => true,
            .garland => toybox.get(child_index).specific.tag() == .newcase,
            else => false,
        });

        if (undo_stack) |s| s.appendAssumeCapacity(.{ .pop = .{ .child = child_index, .parent = lego.index } });

        const child = toybox.get(child_index);
        assert(!child.inCollection());

        if (ll_prev != .nothing) {
            assert(toybox.get(ll_prev).ll_next == ll_next);
            toybox.get(ll_prev).ll_next = child_index;
        } else {
            lego.ll_first = child_index;
        }

        if (ll_next != .nothing) {
            assert(toybox.get(ll_next).ll_prev == ll_prev);
            toybox.get(ll_next).ll_prev = child_index;
        } else {
            lego.ll_last = child_index;
        }

        child.ll_next = ll_next;
        child.ll_prev = ll_prev;
        child.ll_parent = lego.index;
    }

    pub fn addChildLast(lego: *Lego, toybox: *Toybox, child_index: Lego.Index, undo_stack: ?*UndoStack) void {
        lego.insert(toybox, child_index, lego.ll_last, .nothing, undo_stack);
    }

    pub fn hasCollection(lego: *const Lego) bool {
        if (lego.ll_first == .nothing) {
            assert(lego.ll_last == .nothing);
            return false;
        } else {
            assert(lego.ll_last != .nothing);
            return true;
        }
    }

    pub fn inCollection(lego: *const Lego) bool {
        const has_siblings = lego.ll_prev != .nothing or lego.ll_next != .nothing;
        if (lego.ll_parent == .nothing) {
            assert(!has_siblings);
            return false;
        } else {
            return true;
        }
    }

    pub fn childrenSlice(lego: *const Lego, toybox: *Toybox, alloc: Allocator) ![]const Lego.Index {
        var dst: std.ArrayListUnmanaged(Lego.Index) = .empty;
        try lego.children(toybox, &dst, alloc);
        return dst.toOwnedSlice(alloc);
    }

    pub fn children(lego: *const Lego, toybox: *Toybox, dst: *std.ArrayListUnmanaged(Lego.Index), allocator: Allocator) !void {
        return switch (lego.specific) {
            inline else => |*x| try x.children(toybox, dst, allocator),
        };
    }

    pub fn clone(lego: *const Lego, toybox: *Toybox) !Lego.Index {
        const S = struct {
            pub fn cloneChildren(dst: *Lego, src: *const Lego, t: *Toybox) error{OutOfMemory}!void {
                if (src.ll_first == .nothing) {
                    assert(src.ll_last == .nothing);
                    dst.ll_first = .nothing;
                    dst.ll_last = .nothing;
                } else {
                    dst.ll_first = try t.get(src.ll_first).clone(t);
                    dst.ll_last = dst.ll_first;
                    t.get(dst.ll_last).ll_parent = dst.index;
                    var original_last = src.ll_first;
                    while (t.get(original_last).ll_next != .nothing) {
                        const new = try t.get(t.get(original_last).ll_next).clone(t);
                        original_last = t.get(original_last).ll_next;
                        t.get(dst.ll_last).ll_next = new;
                        t.get(new).ll_prev = dst.ll_last;
                        t.get(new).ll_parent = dst.index;
                        dst.ll_last = new;
                    }
                }
            }
        };

        const result = try toybox.add(undefined, undefined);
        result.overwriteMostData(lego, null);
        switch (lego.specific) {
            .sexpr => |sexpr| switch (sexpr.kind) {
                else => {},
                .pair => |pair| {
                    result.specific.sexpr.kind = .{ .pair = .{
                        .up = try toybox.get(pair.up).clone(toybox),
                        .down = try toybox.get(pair.down).clone(toybox),
                    } };
                },
            },
            .case => |case| {
                result.specific.case = .{
                    .pattern = try toybox.get(case.pattern).clone(toybox),
                    .template = try toybox.get(case.template).clone(toybox),
                    .fnkname = try toybox.get(case.fnkname).clone(toybox),
                    .next = try toybox.get(case.next).clone(toybox),
                };
            },
            .garland => {
                try S.cloneChildren(result, lego, toybox);
            },
            .newcase => |newcase| {
                if (newcase.case != .nothing) {
                    result.specific.newcase.case = try clone(toybox.get(newcase.case), toybox);
                }
            },
            else => std.debug.panic("TODO: {s}", .{@tagName(lego.specific)}),
        }
        return result.index;
    }

    /// dst takes ownership of src's children, but not src's place in a collection
    pub fn stealMostData(dst: *Lego, src: *Lego, toybox: *Toybox, undo_stack: ?*UndoStack) void {
        if (undo_stack) |s| {
            // FIXME
            _ = s;
            // s.appendAssumeCapacity(.{ .steal_most_data = .{
            //     .data = dst.*,
            //     .index = dst.index,
            // } });
        }
        const old_ll_prev = dst.ll_prev;
        const old_ll_next = dst.ll_next;
        const old_ll_parent = dst.ll_parent;
        const old_index = dst.index;
        dst.* = src.*;
        dst.index = old_index;
        dst.ll_prev = old_ll_prev;
        dst.ll_next = old_ll_next;
        dst.ll_parent = old_ll_parent;

        src.ll_first = .nothing;
        src.ll_last = .nothing;

        var cur = dst.ll_first;
        while (cur != .nothing) {
            toybox.get(cur).ll_parent = dst.index;
            cur = toybox.get(cur).ll_next;
        }
    }

    /// overwrites everything except index and sibling relations
    pub fn overwriteMostData(dst: *Lego, src: *const Lego, undo_stack: ?*UndoStack) void {
        if (undo_stack) |s| {
            s.appendAssumeCapacity(.{ .overwrite_most_data = .{
                .data = dst.*,
                .index = dst.index,
            } });
        }
        const old_ll_prev = dst.ll_prev;
        const old_ll_next = dst.ll_next;
        const old_ll_parent = dst.ll_parent;
        const old_index = dst.index;
        dst.* = src.*;
        dst.index = old_index;
        dst.ll_prev = old_ll_prev;
        dst.ll_next = old_ll_next;
        dst.ll_parent = old_ll_parent;
    }

    pub fn overwriteVisualData(dst: *Lego, src: *const Lego, undo_stack: ?*UndoStack) void {
        if (undo_stack) |s| dst.storeVisualData(s);
        const old_ll_prev = dst.ll_prev;
        const old_ll_next = dst.ll_next;
        const old_ll_parent = dst.ll_parent;
        const old_specific = dst.specific;
        const old_index = dst.index;
        dst.* = src.*;
        dst.index = old_index;
        dst.specific = old_specific;
        dst.ll_prev = old_ll_prev;
        dst.ll_next = old_ll_next;
        dst.ll_parent = old_ll_parent;
    }

    pub fn overwriteSpecificData(dst: *Lego, data: Specific, undo_stack: ?*UndoStack) void {
        if (undo_stack) |s| {
            s.appendAssumeCapacity(.{ .overwrite_specific_data = .{
                .index = dst.index,
                .data = dst.specific,
            } });
        }
        dst.specific = data;
    }

    pub fn storeVisualData(lego: *const Lego, undo_stack: *UndoStack) void {
        undo_stack.appendAssumeCapacity(.{ .overwrite_visual_data = lego.* });
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

    pub fn handle(lego: *const Lego) ?Handle {
        const radius: Handle.Size = switch (lego.specific) {
            .sexpr,
            .area,
            .microscope,
            .button,
            => return null,
            .case => .default,
            .newcase => .new_case,
            .garland => .garland,
            .lens => .lens,
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
            .newcase => |newcase| .new(0, newcase.length * newcase.handle_t),
            else => .zero,
        };
    }

    fn draggable(lego: *const Lego) bool {
        return switch (lego.specific) {
            .button => false,
            else => true,
        };
    }
};

pub const Handle = struct {
    point: Point,
    radius: Size,
    hot_t: f32,
    // TODO: remove default value
    alpha: f32 = 1,
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

    pub fn draw(handle: *const Handle, drawer: *Drawer, camera: Rect) !void {
        if (handle.enabled) {
            const r = std.math.lerp(handle.radius.base, handle.radius.hot, handle.hot_t);
            drawer.canvas.fillCircle(camera, handle.point.pos, handle.point.scale * r, COLORS.bg.withAlpha(handle.alpha));
            drawer.canvas.strokeCircle(128, camera, handle.point.pos, handle.point.scale * r, 0.05 * handle.point.scale, .blackAlpha(handle.alpha));
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
    arena_for_iterators: std.heap.ArenaAllocator,

    pub fn init(dst: *Toybox, gpa: std.mem.Allocator) !void {
        dst.* = .{
            .all_legos_arena = .init(gpa),
            .all_legos = .empty,
            .arena_for_iterators = .init(gpa),
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
        self.arena_for_iterators.deinit();
    }

    pub fn add(toybox: *Toybox, local_point: Point, specific: Lego.Specific) !*Lego {
        const result = try toybox.all_legos.addOne(toybox.all_legos_arena.allocator());
        result.* = .{
            .index = @enumFromInt(toybox.all_legos.items.len - 1),
            .exists = true,
            .local_point = local_point,
            .specific = specific,
        };
        return result;
    }

    pub fn free(toybox: *Toybox, index: Lego.Index, undo_stack: ?*Workspace.UndoStack) void {
        assert(!toybox.get(index).inCollection());
        if (undo_stack) |s| {
            s.appendAssumeCapacity(.{ .recreate_floating = toybox.get(index).* });
        }

        // TODO: actually free
        toybox.get(index).* = undefined;
        toybox.get(index).index = index;
        toybox.get(index).exists = false;
    }

    pub fn get(toybox: *Toybox, index: Lego.Index) *Lego {
        return &toybox.all_legos.items[@intFromEnum(index)];
    }

    pub fn safeGet(toybox: *Toybox, index: Lego.Index) ?*Lego {
        return if (index == .nothing) null else &toybox.all_legos.items[@intFromEnum(index)];
    }

    pub fn destroyFloating(toybox: *Toybox, index: Lego.Index, undo_stack: ?*Workspace.UndoStack) void {
        assert(!toybox.get(index).inCollection());
        assert(toybox.get(index).exists);

        if (undo_stack) |s| {
            s.appendAssumeCapacity(.{ .recreate_floating = toybox.get(index).* });
        }

        toybox.get(index).exists = false;
        // TODO: destroy/free children

        toybox.free(index, null);
    }

    pub fn recreateFloating(toybox: *Toybox, data: Lego) void {
        assert(!data.inCollection());
        assert(!toybox.get(data.index).exists);
        toybox.get(data.index).* = data;
    }

    pub fn OoM() noreturn {
        std.debug.panic("Out of Memory!", .{});
    }

    pub fn asdfIterator(toybox: *Toybox, root: Lego.Index) AsdfIterator {
        var result: AsdfIterator = .{
            .toybox = toybox,
            .pending = .empty,
        };
        if (root != .nothing) {
            result.pending.append(toybox.arena_for_iterators.allocator(), root) catch OoM();
        }
        return result;
    }

    pub const AsdfIterator = struct {
        toybox: *Toybox,
        pending: std.ArrayListUnmanaged(Lego.Index),
        last_child_count: usize = 0,

        pub const Step = struct {
            index: Lego.Index,
            children: []const Lego.Index,
        };

        pub fn next(it: *AsdfIterator) ?Step {
            if (it.pending.pop()) |index| {
                const old_count = it.pending.items.len;
                it.toybox.get(index).children(it.toybox, &it.pending, it.toybox.arena_for_iterators.allocator()) catch OoM();
                it.last_child_count = it.pending.items.len - old_count;
                return .{
                    .index = index,
                    .children = it.pending.items[old_count..],
                };
            } else {
                it.pending.deinit(it.toybox.arena_for_iterators.allocator());
                return null;
            }
        }

        pub fn skipChildren(it: *AsdfIterator) void {
            it.pending.items.len -= it.last_child_count;
            it.last_child_count = undefined;
        }
    };

    pub fn doublePassIterator(toybox: *Toybox, root: Lego.Index, first_to_last: bool) DoublePassIterator {
        var result: DoublePassIterator = .{
            .toybox = toybox,
            .pending = .empty,
            .first_to_last = first_to_last,
        };
        if (root != .nothing) {
            result.pending.append(
                toybox.arena_for_iterators.allocator(),
                .{ .index = root, .visited_children = false, .parent = .nothing },
            ) catch OoM();
        }
        return result;
    }

    pub const DoublePassIterator = struct {
        toybox: *Toybox,
        first_to_last: bool,
        pending: std.ArrayListUnmanaged(struct { index: Lego.Index, visited_children: bool = false, parent: Lego.Index }),
        tmp: std.ArrayListUnmanaged(Lego.Index) = .empty,
        last_child_count: usize = 0,

        pub const Step = struct {
            index: Lego.Index,
            // children: []const Lego.Index,
            children_already_visited: bool,
            parent: Lego.Index,
        };

        pub fn next(it: *DoublePassIterator) ?Step {
            if (it.pending.pop()) |foo| {
                if (!foo.visited_children) {
                    it.pending.appendAssumeCapacity(.{ .index = foo.index, .visited_children = true, .parent = foo.parent });
                    const old_count = it.pending.items.len;
                    it.toybox.get(foo.index).children(it.toybox, &it.tmp, it.toybox.arena_for_iterators.allocator()) catch OoM();
                    if (it.first_to_last) {
                        std.mem.reverse(Lego.Index, it.tmp.items);
                    }
                    it.pending.ensureUnusedCapacity(it.toybox.arena_for_iterators.allocator(), it.tmp.items.len) catch OoM();
                    for (it.tmp.items) |x| {
                        it.pending.appendAssumeCapacity(.{ .index = x, .visited_children = false, .parent = foo.index });
                    }
                    it.tmp.clearRetainingCapacity();
                    it.last_child_count = it.pending.items.len - old_count;
                } else {
                    it.last_child_count = 0;
                }

                return .{
                    .index = foo.index,
                    // .children = it.pending.items[old_count..],
                    .children_already_visited = foo.visited_children,
                    .parent = foo.parent,
                };
            } else {
                it.pending.deinit(it.toybox.arena_for_iterators.allocator());
                it.tmp.deinit(it.toybox.arena_for_iterators.allocator());
                return null;
            }
        }

        pub fn skipChildren(it: *AsdfIterator) void {
            it.pending.items.len -= it.last_child_count;
            it.last_child_count = undefined;
        }
    };

    pub fn refreshAbsolutePoints(toybox: *Toybox, roots: []const Lego.Index) void {
        for (roots) |root| {
            if (root == .nothing) continue;
            toybox.get(root).absolute_point = toybox.get(root).local_point;

            var it = toybox.asdfIterator(root);
            while (it.next()) |step| {
                const parent_point = toybox.get(step.index).absolute_point;
                for (step.children) |child_index| {
                    toybox.get(child_index).absolute_point = parent_point.applyToLocalPoint(toybox.get(child_index).local_point);
                }
            }

            // var pending: std.ArrayListUnmanaged(Lego.Index) = .empty;
            // try pending.append(alloc, root);
            // while (pending.pop()) |index| {
            //     const parent_point = toybox.get(index).absolute_point;
            //     const children = try toybox.get(index).children(alloc);
            //     for (children) |child_index| {
            //         toybox.get(child_index).absolute_point = parent_point.applyToLocalPoint(toybox.get(child_index).local_point);
            //     }
            //     // defer alloc.free(children);
            // }

            // var it = toybox.treeIterator(root, .any, .single);
            // while (it.next()) |step| {
            //     const parent_point = toybox.get(step.index).absolute_point;
            //     for (toybox.get(step.index).children()) |child_index| {
            //         toybox.get(child_index).absolute_point = parent_point.applyToLocalPoint(toybox.get(child_index).local_point);
            //     }
            // }
        }
    }

    pub fn changeCoordinates(toybox: *Toybox, index: Lego.Index, old_parent: Point, new_parent: Point) void {
        toybox.get(index).local_point = new_parent.inverseApplyGetLocal(old_parent.applyToLocalPoint(toybox.get(index).local_point));
    }

    pub fn buildSexpr(toybox: *Toybox, local_point: Point, value: Lego.Specific.Sexpr.Kind, is_pattern: bool) !Lego.Index {
        const result = try toybox.add(local_point, .{ .sexpr = .{
            .is_pattern = is_pattern,
            .is_pattern_t = if (is_pattern) 1 else 0,
            .kind = value,
        } });
        return result.index;
    }

    pub fn buildCase(toybox: *Toybox, local_point: Point, data: struct {
        pattern: Lego.Index,
        template: Lego.Index,
        fnkname: Lego.Index,
        next: ?Lego.Index = null,
    }) !Lego.Index {
        const result = try toybox.add(local_point, .{ .case = .{
            .pattern = data.pattern,
            .template = data.template,
            .fnkname = data.fnkname,
            .next = data.next orelse try toybox.buildGarland(local_point, &.{}),
        } });
        return result.index;
    }

    /// The garland's children are a linear list of newcase, all except the last one with a child case
    /// the newcase position is the very top of the segment
    pub fn buildGarland(toybox: *Toybox, local_point: Point, child_cases: []const Lego.Index) !Lego.Index {
        const result = try toybox.add(local_point, .{ .garland = .{} });
        try result.specific.garland.setCases(child_cases, toybox, null);
        return result.index;
    }

    pub fn buildMicroscope(toybox: *Toybox, source: Vec2, target: Vec2) !Lego.Index {
        const lens_source = try toybox.add(.{ .pos = source }, .{ .lens = .source });
        const lens_target = try toybox.add(.{ .pos = target }, .{ .lens = .target });
        const result = try toybox.add(.{}, .{ .microscope = .{
            .source = lens_source.index,
            .target = lens_target.index,
        } });
        return result.index;
    }
};

const Workspace = struct {
    toybox: Toybox,

    main_area: Lego.Index,
    toolbar_left: Lego.Index,
    toolbar_left_unfolded_t: f32 = 0,
    lenses_layer: Lego.Index,
    hand_layer: Lego.Index = .nothing,

    grabbing: Lego.Index = .nothing,

    undo_stack: UndoStack,
    random_instance: std.Random.DefaultPrng,
    arena_for_atom_names: std.heap.ArenaAllocator,

    pub const UndoStack = std.ArrayList(UndoableCommand);

    pub const toolbar_left_rect: Rect = .{ .top_left = .zero, .size = .new(6, 15) };

    const UndoableCommand = union(enum) {
        fence,
        // set_data_except_tree: Lego,

        // TODO: could use less space by only storing relevant fields
        overwrite_visual_data: Lego,

        destroy_floating: Lego.Index,
        recreate_floating: Lego,

        // restore_sexpr_kind: struct {
        //     index: Lego.Index,
        //     kind: Lego.Specific.Sexpr.Kind,
        // },
        overwrite_most_data: struct {
            index: Lego.Index,
            data: Lego,
        },

        overwrite_specific_data: struct {
            index: Lego.Index,
            data: Lego.Specific,
        },

        // change_child: struct {
        //     original: Lego.Index,
        //     new: Lego.Index,
        // },

        insert: struct {
            ll_prev: Lego.Index,
            ll_next: Lego.Index,
            ll_parent: Lego.Index,
            self: Lego.Index,
        },
        pop: struct {
            child: Lego.Index,
            parent: Lego.Index,
        },

        set_grabbing: Lego.Index,
        set_handlayer: Lego.Index,
    };

    /// in draw order
    fn roots(workspace: Workspace, config: struct {
        include_hand: bool,
        include_lenses: bool,

        pub const all: @This() = .{ .include_hand = true, .include_lenses = true };
        pub const interactable: @This() = .{ .include_hand = false, .include_lenses = true };
    }) std.BoundedArray(Lego.Index, 4) {
        var result: std.BoundedArray(Lego.Index, 4) = .{};
        result.appendAssumeCapacity(workspace.main_area);
        result.appendAssumeCapacity(workspace.toolbar_left);
        if (config.include_hand) result.appendAssumeCapacity(workspace.hand_layer);
        if (config.include_lenses) result.appendAssumeCapacity(workspace.lenses_layer);
        return result;
    }

    pub fn init(dst: *Workspace, gpa: std.mem.Allocator, random_seed: u64) !void {
        dst.* = kommon.meta.initDefaultFields(Workspace);
        dst.undo_stack = .init(gpa);
        dst.random_instance = .init(random_seed);
        dst.arena_for_atom_names = .init(gpa);
        try dst.toybox.init(gpa);
        const toybox = &dst.toybox;

        const main_area = try dst.toybox.add(.{ .scale = 0.1 }, .{ .area = .{ .bg = .all } });
        dst.main_area = main_area.index;
        dst.toolbar_left = (try dst.toybox.add(.{}, .{
            .area = .{
                .bg = .{
                    // ensure that "mouse off-screen on the left" also overlaps the toolbar
                    .local_rect = toolbar_left_rect.plusMargin3(.left, 100),
                },
            },
        })).index;
        const lenses_layer = try dst.toybox.add(main_area.local_point, .{ .area = .{ .bg = .none } });
        dst.lenses_layer = lenses_layer.index;

        try dst.regenerateToolbarLeft();

        if (true) {
            main_area.addChildLast(toybox, try dst.toybox.buildSexpr(
                .{ .pos = .new(0, 0) },
                .{ .atom_lit = "true" },
                false,
            ), null);
            main_area.addChildLast(toybox, try dst.toybox.buildSexpr(
                .{ .pos = .new(0, 1) },
                .{ .atom_lit = "false" },
                false,
            ), null);

            main_area.addChildLast(toybox, try dst.toybox.buildSexpr(
                .{ .pos = .new(3, 0) },
                .{ .pair = .{
                    .up = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, false),
                    .down = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                } },
                false,
            ), null);

            main_area.addChildLast(toybox, try dst.toybox.buildCase(
                .{ .pos = .new(0, 4) },
                .{
                    .pattern = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true),
                    .template = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                    .fnkname = try dst.toybox.buildSexpr(.{}, .empty, false),
                },
            ), null);

            const case_1 = try dst.toybox.buildCase(.{}, .{
                .pattern = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true),
                .template = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                .fnkname = try dst.toybox.buildSexpr(.{}, .empty, false),
            });
            const case_2 = try dst.toybox.buildCase(.{}, .{
                .pattern = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true),
                .template = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                .fnkname = try dst.toybox.buildSexpr(.{}, .empty, false),
            });
            main_area.addChildLast(toybox, try dst.toybox.buildGarland(
                .{ .pos = .new(7, 4) },
                &.{ case_1, case_2 },
            ), null);

            lenses_layer.addChildLast(toybox, try dst.toybox.buildMicroscope(
                .new(2, 2),
                .new(4, 3),
            ), null);

            lenses_layer.addChildLast(toybox, try dst.toybox.buildMicroscope(
                .new(4, 3),
                .new(6, 2),
            ), null);
        }
    }

    pub fn deinit(workspace: *Workspace) void {
        workspace.toybox.deinit();
        workspace.undo_stack.deinit();
        workspace.arena_for_atom_names.deinit();
    }

    const HotAndDropzone = struct {
        hot: Lego.Index = .nothing,
        dropzone: Lego.Index = .nothing,
        parent: Lego.Index,
        over_background: Lego.Index,

        pub fn empty(x: @This()) bool {
            return x.hot == .nothing and x.dropzone == .nothing;
        }
    };
    fn findHotAndDropzone(workspace: *Workspace, absolute_needle_pos: Vec2) HotAndDropzone {
        return _findHotAndDropzone(
            &workspace.toybox,
            workspace.roots(.interactable).constSlice(),
            absolute_needle_pos,
            workspace.grabbing,
        );
    }

    fn _findHotAndDropzone(toybox: *Toybox, roots_in_draw_order: []const Lego.Index, absolute_needle_pos: Vec2, grabbing: Lego.Index) HotAndDropzone {
        var roots_it = std.mem.reverseIterator(roots_in_draw_order);
        while (roots_it.next()) |root| {
            var it = toybox.doublePassIterator(root, false);
            while (it.next()) |step| {
                const cur = step.index;
                const lego = toybox.get(cur);
                switch (lego.specific) {
                    .sexpr => |sexpr| {
                        // TODO: skip children if needle is far
                        if (!step.children_already_visited and
                            Lego.Specific.Sexpr.contains(lego.absolute_point, sexpr.is_pattern, sexpr.kind, absolute_needle_pos))
                        {
                            if (grabbing == .nothing and sexpr.kind != .empty) {
                                return .{ .hot = cur, .over_background = root, .parent = step.parent };
                            } else if (grabbing != .nothing and toybox.get(grabbing).specific.tag() == .sexpr) {
                                return .{ .dropzone = cur, .over_background = root, .parent = step.parent };
                            }
                        }
                    },
                    .lens => |lens| {
                        if (step.children_already_visited and
                            lens.is_target and
                            lego.absolute_point.inRange(absolute_needle_pos, lens.local_radius))
                        {
                            const interaction_nested = _findHotAndDropzone(
                                toybox,
                                lens.roots_to_interact,
                                lens.transform.inverse().actOnPosition(absolute_needle_pos),
                                grabbing,
                            );
                            if (!interaction_nested.empty()) {
                                return interaction_nested;
                            }

                            // TODO: find the actual background
                            // Avoid interacting with things hidden by the lens
                            return .{ .over_background = roots_in_draw_order[0], .parent = .nothing };
                        }
                    },
                    .area => |area| {
                        if (step.children_already_visited and
                            area.bg.contains(lego.absolute_point, absolute_needle_pos))
                        {
                            return .{ .over_background = cur, .parent = .nothing };
                        }
                    },
                    .button => |button| {
                        if (step.children_already_visited and
                            button.enabled and
                            button.local_rect.contains(lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos)))
                        {
                            return .{ .hot = cur, .over_background = root, .parent = .nothing };
                        }
                    },
                    .case,
                    .newcase,
                    .microscope,
                    .garland,
                    => {},
                }
                if (step.children_already_visited) {
                    if (lego.handle()) |handle| {
                        const overlappable: bool, const kind: enum { hot, drop } = switch (lego.specific) {
                            .sexpr, .area, .microscope, .button => unreachable,
                            .case, .lens => .{ grabbing == .nothing, .hot },
                            .newcase => .{ grabbing != .nothing and toybox.get(grabbing).specific.tag() == .case, .drop },
                            .garland => if (grabbing == .nothing)
                                .{ true, .hot }
                            else if (toybox.get(grabbing).specific.tag() == .garland)
                                .{ true, .drop }
                            else
                                .{ false, undefined },
                        };

                        if (overlappable and handle.overlapped(absolute_needle_pos)) {
                            switch (kind) {
                                .hot => return .{ .hot = cur, .over_background = root, .parent = step.parent },
                                .drop => return .{ .dropzone = cur, .over_background = root, .parent = step.parent },
                            }
                        }
                    }
                }
            }
        }

        unreachable;
    }

    fn updateSprings(workspace: *Workspace, absolute_mouse_pos: Vec2, interaction: HotAndDropzone, delta_seconds: f32) void {
        const toybox = &workspace.toybox;

        defer toybox.refreshAbsolutePoints(workspace.roots(.all).constSlice());
        for (workspace.roots(.all).constSlice()) |root| {
            // TODO: use single-pass iterator, we just need the parent
            var it = toybox.doublePassIterator(root, false);
            while (it.next()) |step| {
                const cur = step.index;
                const lego = toybox.get(cur);
                if (cur == workspace.grabbing and lego.draggable()) {
                    const target: Point = if (interaction.dropzone == .nothing)
                        // TODO: i don't like the scale hack
                        (Point{
                            .pos = absolute_mouse_pos,
                            .scale = toybox.get(interaction.over_background).absolute_point.scale,
                        }).applyToLocalPoint(.{ .pos = lego.handleLocalOffset().neg() })
                    else
                        toybox.get(interaction.dropzone).absolute_point.applyToLocalPoint(.{ .pos = toybox.get(interaction.dropzone).handleLocalOffset() });

                    const parent_point: Point = if (step.parent == .nothing) .{} else toybox.get(step.parent).absolute_point;

                    lego.local_point.lerp_towards(parent_point
                        .inverseApplyGetLocal(target), 0.6, delta_seconds);
                }

                switch (lego.specific) {
                    .sexpr => |*sexpr| {
                        if (cur == workspace.grabbing) {
                            sexpr.is_pattern = if (interaction.dropzone == .nothing) sexpr.is_pattern else toybox.get(interaction.dropzone).specific.sexpr.is_pattern;
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

                        switch (sexpr.kind) {
                            else => {},
                            .pair => |pair| {
                                toybox.get(pair.up).local_point = (Point{})
                                    .applyToLocalPoint(lego.visual_offset)
                                    .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .up));
                                toybox.get(pair.down).local_point = (Point{})
                                    .applyToLocalPoint(lego.visual_offset)
                                    .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .down));

                                toybox.get(pair.up).specific.sexpr.is_pattern = sexpr.is_pattern;
                                toybox.get(pair.up).specific.sexpr.is_pattern_t = if (sexpr.is_pattern) 1 else 0;
                                toybox.get(pair.down).specific.sexpr.is_pattern = sexpr.is_pattern;
                                toybox.get(pair.down).specific.sexpr.is_pattern_t = if (sexpr.is_pattern) 1 else 0;
                            },
                        }
                    },
                    .case => |case| {
                        for ([4]Lego.Index{
                            case.pattern,
                            case.template,
                            case.fnkname,
                            case.next,
                        }, [4]Point{
                            .{ .pos = .xneg },
                            .{ .pos = .xpos },
                            .{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) },
                            .{ .pos = .new(8, 1) },
                        }) |index, offset| {
                            toybox.get(index).local_point = offset;
                        }
                    },
                    .garland => |*garland| {
                        var a = toybox.get(lego.ll_first);
                        var offset: Vec2 = .zero;
                        while (true) {
                            assert(a.specific.tag() == .newcase);
                            a.local_point = .{ .pos = offset };
                            offset.addInPlace(.new(0, a.specific.newcase.length));

                            a = toybox.safeGet(a.ll_next) orelse break;
                        }
                        garland.computed_height = offset.y;
                    },
                    .newcase => |*newcase| {
                        const target_length: f32, const target_handle_t: f32 = blk: {
                            const prev_height: f32 = if (lego.ll_prev == .nothing)
                                0
                            else blk2: {
                                const case_of_prev_segment = toybox.get(lego.ll_prev).specific.newcase.case;
                                const garland_of_case_of_prev_segment = toybox.get(case_of_prev_segment).specific.case.next;
                                if (garland_of_case_of_prev_segment == interaction.dropzone) {
                                    break :blk2 toybox.get(workspace.grabbing).specific.garland.computed_height;
                                } else {
                                    break :blk2 toybox.get(garland_of_case_of_prev_segment).specific.garland.computed_height;
                                }
                            };
                            if (newcase.case != .nothing) {
                                assert(lego.ll_next != .nothing);
                                assert(toybox.get(newcase.case).specific.tag() == .case);
                                toybox.get(newcase.case).local_point = .{ .pos = .new(0, newcase.length) };
                                break :blk .{ 1.5 + prev_height, 0.5 };
                            } else {
                                assert(lego.ll_next == .nothing);
                                break :blk .{ 1 + prev_height, 1 };
                            }
                        };
                        math.lerpTowards(&newcase.handle_t, target_handle_t, .fast, delta_seconds);
                        math.lerpTowards(&newcase.length, target_length + 2.5 * 0.5 * lego.dropzone_t, .slow, delta_seconds);
                    },
                    .area, .microscope, .lens, .button => {},
                }
            }
        }
    }

    fn draw(workspace: *Workspace, platform: PlatformGives, drawer: *Drawer) !void {
        // std.log.debug("main frame", .{});
        const asdf = tracy.initZone(@src(), .{ .name = "draw" });
        defer asdf.deinit();

        const camera = Rect
            .fromCenterAndSize(.zero, .both(2))
            .withAspectRatio(platform.aspect_ratio, .grow, .center);
        const toybox = &workspace.toybox;

        drawer.canvas.clipper.reset();
        drawer.canvas.clipper.use(drawer.canvas);

        try _draw(toybox, workspace.roots(.all).constSlice(), camera, drawer);

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

    fn _draw(toybox: *Toybox, roots_in_draw_order: []const Lego.Index, camera: Rect, drawer: *Drawer) !void {
        // std.log.debug("calling _draw", .{});
        for (roots_in_draw_order) |root| {
            var it = toybox.doublePassIterator(root, true);
            while (it.next()) |step| {
                const cur = step.index;
                const lego = toybox.get(cur);
                // TODO: don't draw if small or far from camera
                if (step.children_already_visited) {
                    if (lego.handle()) |handle| try handle.draw(drawer, camera);
                } else {
                    const point = lego.absolute_point.applyToLocalPoint(lego.visual_offset);
                    switch (lego.specific) {
                        .sexpr => |sexpr| {
                            switch (sexpr.kind) {
                                .empty => {},
                                .atom_lit => |atom_name| try drawer.drawAtom(camera, point, sexpr.is_pattern, atom_name, 1),
                                .atom_var => |atom_name| try drawer.drawVariable(camera, point, sexpr.is_pattern, atom_name, 1),
                                .pair => try drawer.drawPairHolder(camera, point, sexpr.is_pattern, 1),
                            }
                        },
                        .lens => |lens| {
                            // TODO: lens distortion effect, on source and target

                            if (lens.is_target and camera.plusMargin(lego.absolute_point.scale * (lens.local_radius + 1)).contains(lego.absolute_point.pos)) {
                                const lens_circle: math.Circle = .{ .center = lego.absolute_point.pos, .radius = lens.local_radius * lego.absolute_point.scale };
                                if (drawer.canvas.clipper.push(.{ .camera = camera, .shape = .{ .circle = lens_circle } })) {
                                    drawer.canvas.clipper.use(drawer.canvas);
                                    defer {
                                        drawer.canvas.clipper.pop();
                                        drawer.canvas.clipper.use(drawer.canvas);
                                    }
                                    drawer.canvas.fillCircleV2(camera, lens_circle, COLORS.bg);

                                    // std.log.debug("for cur {any}, recursively drawing roots: {any}", .{ cur, lens.roots_to_draw });
                                    try _draw(toybox, lens.roots_to_draw, lens.transform.getCamera(camera), drawer);
                                    // std.log.debug("for cur {any}, finished recursively drawing roots: {any}", .{ cur, lens.roots_to_draw });
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
                        .button => |button| {
                            drawer.canvas.fillRect(camera, lego.absolute_point.applyToLocalRect(button.local_rect), .gray(0.4));
                        },
                        .newcase => |newcase| {
                            // TODO: use .reparentCamera
                            drawer.canvas.line(camera, &.{
                                lego.absolute_point.pos,
                                lego.absolute_point.pos.addY(newcase.length * lego.absolute_point.scale),
                            }, 0.05 * lego.absolute_point.scale, .black);
                        },
                        .case, .garland, .microscope => {},
                    }
                }
            }
        }
    }

    pub fn update(workspace: *Workspace, platform: PlatformGives, drawer: ?*Drawer, scratch: std.mem.Allocator) !void {
        const toybox = &workspace.toybox;

        if (platform.keyboard.wasPressed(.KeyQ)) {
            std.log.debug("-----", .{});
            for (toybox.all_legos.items, 0..) |lego, k| {
                assert(lego.index == @as(Lego.Index, @enumFromInt(k)));
                if (lego.exists) {
                    // std.log.debug("{d} \t{s} \trel: {any} \tabs: {any}", .{
                    //     k,
                    //     @tagName(lego.specific.tag()),
                    //     lego.local_point,
                    //     lego.absolute_point,
                    // });
                    std.log.debug("{d} \t{s} \ttree: parent {any}; prev {any}; next {any}; first {any}; last {any}", .{
                        k,
                        @tagName(lego.specific.tag()),
                        lego.ll_parent,
                        lego.ll_prev,
                        lego.ll_next,
                        lego.ll_first,
                        lego.ll_last,
                    });
                }
            }
            std.log.debug("-----", .{});
            for (workspace.undo_stack.items) |cmd| {
                std.log.debug("{any}", .{cmd});
            }
        }

        if (platform.keyboard.wasPressed(.KeyZ)) {
            while (workspace.undo_stack.pop()) |command| {
                switch (command) {
                    .fence => break,
                    .destroy_floating => |index| {
                        toybox.destroyFloating(index, null);
                    },
                    .overwrite_most_data => |info| {
                        toybox.get(info.index).overwriteMostData(&info.data, null);
                    },
                    .overwrite_specific_data => |info| {
                        toybox.get(info.index).overwriteSpecificData(info.data, null);
                    },
                    // .restore_sexpr_kind => |data| {
                    //     toybox.get(data.index).specific.sexpr.kind = data.kind;
                    // },
                    .recreate_floating => |data| {
                        // TODO: recreate children too!
                        toybox.recreateFloating(data);
                    },
                    .insert => |where| {
                        toybox.get(where.ll_parent).insert(toybox, where.self, where.ll_prev, where.ll_next, null);
                    },
                    .overwrite_visual_data => |data| {
                        toybox.get(data.index).overwriteVisualData(&data, null);
                    },
                    .pop => |data| {
                        toybox.get(data.parent).pop(toybox, data.child, null);
                    },
                    // .change_child => |change| {
                    //     toybox.changeChild(change.original, change.new);
                    // },
                    .set_grabbing => |index| {
                        workspace.grabbing = index;
                    },
                    .set_handlayer => |index| {
                        workspace.hand_layer = index;
                    },
                }
            }
        }

        const delta_seconds = platform.delta_seconds;

        // TODO: fix some anims
        // const delta_seconds = platform.delta_seconds * 0.01;

        const absolute_camera = Rect
            .fromCenterAndSize(.zero, .both(2))
            .withAspectRatio(platform.aspect_ratio, .grow, .center);
        const mouse = platform.getMouse(absolute_camera);

        const hot_and_dropzone = workspace.findHotAndDropzone(mouse.cur.position);

        // const hovering: Lego.Index = if (workspace.focus.grabbing == .nothing) hovered_or_dropzone_thing.which else .nothing;
        // const dropzone: Lego.Index = if (workspace.focus.grabbing != .nothing) hovered_or_dropzone_thing.which else .nothing;

        // cursor
        platform.setCursor(
            if (workspace.grabbing != .nothing)
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
            comptime assert(!@hasField(Lego, "active_t"));
            math.lerp_towards(&lego.dropzone_t, if (lego.index == hot_and_dropzone.dropzone) 1 else 0, 0.6, delta_seconds);
            math.lerp_towards(&lego.dropping_t, if (lego.index == workspace.grabbing and hot_and_dropzone.dropzone != .nothing) 1 else 0, 0.6, delta_seconds);

            switch (lego.specific) {
                .sexpr => |*sexpr| {
                    math.lerp_towards(&sexpr.is_pattern_t, if (sexpr.is_pattern) 1 else 0, 0.6, delta_seconds);
                },
                .area,
                .case,
                .newcase,
                .garland,
                .microscope,
                .lens,
                .button,
                => {},
            }
        }

        // TODO: a bit hacky
        if (true) { // set garlands visibility
            // const grabbing_garland_or_case: bool = if (workspace.grabbing == .nothing)
            //     false
            // else switch (toybox.get(workspace.grabbing).specific) {
            //     .case, .garland => true,
            //     else => false,
            // };
            for (toybox.all_legos.items) |*lego| {
                // const in_toolbar = toybox.oldestAncestor(lego.index) == workspace.toolbar_left;
                if (lego.specific.as(.garland)) |garland| {
                    // garland.visible = !in_toolbar and (grabbing_garland_or_case or lego.tree.first != .nothing);
                    // TODO
                    garland.visible = true;
                }
            }
        }

        if (true) { // move camera
            const p = &toybox.get(workspace.main_area).local_point;
            p.* = p.scaleAroundLocalPosition(p.inverseApplyGetLocalPosition(mouse.cur.position), switch (mouse.cur.scrolled) {
                .none => 1.0,
                .up => 1.1,
                .down => 0.9,
            });
            inline for (KeyboardButton.directional_keys) |kv| {
                for (kv.keys) |key| {
                    if (platform.keyboard.cur.isDown(key)) {
                        p.pos.addInPlace(kv.dir.scale(delta_seconds * -2));
                    }
                }
            }

            toybox.get(workspace.lenses_layer).local_point = toybox.get(workspace.main_area).local_point;

            toybox.refreshAbsolutePoints(&.{ workspace.main_area, workspace.lenses_layer });
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
            const p = &toybox.get(workspace.toolbar_left).local_point;
            p.* = .{
                .scale = absolute_camera.size.y / rect.size.y,
                .pos = absolute_camera.top_left,
            };
            p.* = p.applyToLocalPoint(.{ .pos = .new(-(rect.size.x - 1) * (1 - hot_t), 0) });

            toybox.refreshAbsolutePoints(&.{workspace.toolbar_left});
        }

        // TODO: buttons
        if (false) { // enable/disable buttons
            for (toybox.all_legos.items) |*lego| {
                if (lego.specific.as(.button)) |button| {
                    button.enabled = switch (button.action) {
                        else => @panic("TODO"),
                    };
                }
            }
        }

        // includes dragging and snapping to dropzone, since that's just the spring between the mouse cursor/dropzone and the grabbed thing
        workspace.updateSprings(mouse.cur.position, hot_and_dropzone, delta_seconds);

        if (true) { // set lenses data
            const microscopes = try toybox.get(workspace.lenses_layer).childrenSlice(toybox, scratch);
            for (microscopes, 0..) |microscope, k| {
                const source = toybox.get(microscope).specific.microscope.source;
                const target = toybox.get(microscope).specific.microscope.target;
                const source_pos = toybox.get(source).absolute_point.pos;
                const target_pos = toybox.get(target).absolute_point.pos;
                const source_lens = &toybox.get(source).specific.lens;
                const source_radius = source_lens.local_radius * toybox.get(source).absolute_point.scale;
                const target_lens = &toybox.get(target).specific.lens;
                const target_radius = target_lens.local_radius * toybox.get(target).absolute_point.scale;

                source_lens.transform = .identity;
                source_lens.is_target = false;
                target_lens.transform = .fromLenses(source_pos, source_radius, target_pos, target_radius);
                target_lens.is_target = true;

                var all_roots: std.ArrayListUnmanaged(Lego.Index) = .empty;
                try all_roots.appendSlice(scratch, workspace.roots(.{
                    .include_hand = true,
                    .include_lenses = false,
                }).constSlice());
                try all_roots.appendSlice(scratch, microscopes[0..k]);

                var all_roots_except_hand: std.ArrayListUnmanaged(Lego.Index) = .empty;
                try all_roots_except_hand.appendSlice(scratch, workspace.roots(.{
                    .include_hand = false,
                    .include_lenses = false,
                }).constSlice());
                try all_roots_except_hand.appendSlice(scratch, microscopes[0..k]);

                source_lens.roots_to_draw = all_roots.items;
                source_lens.roots_to_interact = &.{};
                target_lens.roots_to_draw = all_roots.items;
                target_lens.roots_to_interact = all_roots_except_hand.items;
            }
        }

        if (drawer) |d| {
            try workspace.draw(platform, d);
        }

        if (true) { // INTERACTION
            const undo_stack = &workspace.undo_stack;
            try undo_stack.ensureUnusedCapacity(32);
            if (workspace.grabbing == .nothing and
                hot_and_dropzone.hot != .nothing and
                (mouse.wasPressed(.left) or mouse.wasPressed(.right)))
            {
                // Main case A: plucking/grabbing/clicking something
                try workspace.undo_stack.append(.fence);

                const hot_index = hot_and_dropzone.hot;
                const original_hot_data = toybox.get(hot_index).*;
                const hot_parent = hot_and_dropzone.parent;
                const original_parent_absolute_point: Point = if (toybox.safeGet(hot_parent)) |p| p.absolute_point else .{};

                var grabbed_element_index: Lego.Index = undefined;
                var plucked: bool = true;

                if (mouse.wasPressed(.right)) {
                    // Case A.0: duplicating
                    // TODO
                    // if (original_hot_data.canDuplicate()) {
                    const new_element_index = try toybox.get(hot_index).clone(toybox);
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .destroy_floating = new_element_index,
                    });
                    grabbed_element_index = new_element_index;
                    // }
                } else if (if (toybox.safeGet(hot_parent)) |p| p.specific.tag() == .area else false) {
                    // Case A.1: plucking a top-level thing
                    toybox.get(hot_index).storeVisualData(undo_stack);
                    // workspace.undo_stack.appendAssumeCapacity(.{
                    //     .set_data_except_tree = original_hot_data,
                    // });

                    toybox.get(hot_parent).pop(toybox, hot_index, undo_stack);
                    grabbed_element_index = hot_index;
                } else if (toybox.get(hot_index).specific.as(.sexpr)) |sexpr| {
                    // Case A.2: plucking a nested sexpr
                    grabbed_element_index = try sexpr.emptyAndClone(toybox, &workspace.undo_stack);
                } else if (original_hot_data.specific.tag() == .lens) {
                    // Case A.3: grabbing rather than plucking
                    toybox.get(hot_index).storeVisualData(undo_stack);
                    grabbed_element_index = hot_index;
                    plucked = false;
                } else if (hot_parent != .nothing and toybox.get(hot_parent).specific.tag() == .newcase) {
                    // Case A.4: plucking a case from a garland
                    assert(original_hot_data.specific.tag() == .case);
                    var asdf = toybox.get(hot_parent).specific.newcase;
                    asdf.case = .nothing;
                    toybox.get(hot_parent).overwriteSpecificData(.{ .newcase = asdf }, undo_stack);

                    const garland = toybox.get(toybox.get(hot_parent).ll_parent); // .specific.garland;
                    // TODO: less jumpyness
                    toybox.get(toybox.get(hot_parent).ll_next).specific.newcase.length += toybox.get(hot_parent).specific.newcase.length;
                    garland.pop(toybox, hot_parent, undo_stack);
                    grabbed_element_index = hot_index;
                } else if (toybox.get(hot_index).specific.as(.garland)) |garland| {
                    // Case A.5: plucking a garland, and replacing it with an empty one
                    grabbed_element_index = try garland.emptyAndClone(toybox, &workspace.undo_stack);
                } else if (false and toybox.get(hot_index).specific.tag() == .button) {
                    // TODO
                    // Case A.6: pressing a button
                    plucked = false;
                    if (toybox.get(hot_index).specific.button.instant()) {
                        grabbed_element_index = .nothing;
                        @panic("TODO");
                    } else {
                        grabbed_element_index = hot_index;
                    }
                } else unreachable;

                assert(workspace.grabbing == .nothing and workspace.hand_layer == .nothing);
                workspace.setGrabbing(grabbed_element_index, undo_stack);
                if (plucked) {
                    workspace.setHandLayer(grabbed_element_index, undo_stack);
                    toybox.changeCoordinates(grabbed_element_index, original_parent_absolute_point, .{});
                    toybox.refreshAbsolutePoints(&.{grabbed_element_index});
                }
            } else if (workspace.grabbing != .nothing and
                !(mouse.cur.isDown(.left) or mouse.cur.isDown(.right)))
            {
                const dropzone_index = hot_and_dropzone.dropzone;
                const original_parent_absolute_point: Point = if (toybox.safeGet(hot_and_dropzone.parent)) |p| p.absolute_point else .{};

                if (dropzone_index != .nothing) {
                    assert(!toybox.get(workspace.grabbing).inCollection());
                    if (toybox.get(dropzone_index).overwriteableByDropping()) {
                        toybox.changeCoordinates(workspace.grabbing, .{}, original_parent_absolute_point);
                        toybox.refreshAbsolutePoints(&.{workspace.grabbing});
                        toybox.get(dropzone_index).overwrite(workspace.grabbing, toybox, &workspace.undo_stack);
                    } else if (toybox.get(dropzone_index).specific.tag() == .newcase) {
                        // TODO: avoid jumpyness
                        assert(toybox.get(workspace.grabbing).specific.tag() == .case);
                        const newcase = try toybox.add(.{}, .{ .newcase = .{
                            .case = workspace.grabbing,
                        } });
                        workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = newcase.index });
                        toybox.get(toybox.get(dropzone_index).ll_parent).insert(
                            toybox,
                            newcase.index,
                            toybox.get(dropzone_index).ll_prev,
                            dropzone_index,
                            undo_stack,
                        );
                        // TODO: maybe?
                        // toybox.changeCoordinates(workspace.grabbing, .{}, toybox.parentAbsolutePoint(dropzone_index));
                    } else unreachable;
                } else if (workspace.hand_layer == .nothing) { // equivalent to toybox.isFloating(workspace.grabbing), hopefully
                    // Case B.2: releasing a grabbed thing, which might be a button
                    assert(dropzone_index == .nothing);
                    if (toybox.get(workspace.grabbing).specific.as(.button)) |button| {
                        switch (button.action) {
                            else => @panic("TODO"),
                        }
                    }
                } else if (toybox.get(hot_and_dropzone.over_background).specific.tag() == .area) {
                    // Case B.3: dropping a floating thing on fresh space
                    const target_area = hot_and_dropzone.over_background;
                    toybox.changeCoordinates(workspace.grabbing, .{}, toybox.get(target_area).absolute_point);
                    toybox.refreshAbsolutePoints(&.{workspace.grabbing});
                    toybox.get(hot_and_dropzone.over_background).addChildLast(toybox, workspace.grabbing, &workspace.undo_stack);
                } else unreachable;

                workspace.undo_stack.appendAssumeCapacity(.{ .set_grabbing = workspace.grabbing });
                workspace.undo_stack.appendAssumeCapacity(.{ .set_handlayer = workspace.hand_layer });
                workspace.grabbing = .nothing;
                workspace.hand_layer = .nothing;
            }
        }
    }

    fn setGrabbing(workspace: *Workspace, index: Lego.Index, undo_stack: ?*UndoStack) void {
        if (undo_stack) |s| {
            s.appendAssumeCapacity(.{
                .set_grabbing = workspace.grabbing,
            });
        }
        workspace.grabbing = index;
    }

    fn setHandLayer(workspace: *Workspace, index: Lego.Index, undo_stack: ?*UndoStack) void {
        if (undo_stack) |s| {
            s.appendAssumeCapacity(.{
                .set_handlayer = workspace.hand_layer,
            });
        }
        workspace.hand_layer = index;
    }

    fn regenerateToolbarLeft(workspace: *Workspace) !void {
        const toybox = &workspace.toybox;
        const undo_stack = &workspace.undo_stack;
        try undo_stack.ensureUnusedCapacity(32);

        if (true) { // delete all current children
            while (toybox.get(workspace.toolbar_left).ll_last != .nothing) {
                toybox.get(workspace.toolbar_left).pop(toybox, toybox.get(workspace.toolbar_left).ll_last, undo_stack);
            }
        }

        if (true) { // add a fresh case
            const new_name = try workspace.arena_for_atom_names.allocator().alloc(u8, 32);
            math.Random.init(workspace.random_instance.random()).alphanumeric_bytes(new_name);

            const index = try toybox.buildCase(.{ .pos = .new(2.5, 5) }, .{
                .pattern = try toybox.buildSexpr(.{}, .{ .atom_var = new_name }, true),
                .template = try toybox.buildSexpr(.{}, .{ .pair = .{
                    .up = try toybox.buildSexpr(.{}, .{ .atom_var = new_name }, false),
                    .down = try toybox.buildSexpr(.{}, .{ .atom_lit = "nil" }, false),
                } }, false),
                .fnkname = try toybox.buildSexpr(.{}, .empty, false),
            });

            workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = index });
            toybox.get(workspace.toolbar_left).addChildLast(toybox, index, undo_stack);
        }
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
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    try Drawer.AtomVisuals.Geometry.initFixed(self.usual.mem.forever.allocator(), self.usual.canvas.gl);
    self.drawer.atom_visuals_cache = try .init(self.usual.mem.forever.allocator(), self.usual.canvas.gl);
    // try self.workspace.init(&self.core_mem, 0);
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

const Atom = core.Atom;
// const Pair = core.Pair;
// const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const PhysicalSexpr = @import("physical.zig").PhysicalSexpr;
const ViewHelper = @import("physical.zig").ViewHelper;
const BindingsState = @import("physical.zig").BindingsState;
const Sample = @import("levels_new.zig").Sample;

const Allocator = std.mem.Allocator;

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
