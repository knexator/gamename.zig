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

    tree: Tree = .empty,

    specific: Specific,

    pub const Specific = union(enum) {
        button: Button,
        area: Area,
        sexpr: Sexpr,
        case,
        garland: Garland,
        /// cable between cases, and the handle to create new ones
        newcase: NewCase,
        executor: Executor,
        fnkbox: Fnkbox,
        microscope: Microscope,
        lens: Lens,

        // TODO: try to simplify these
        fnkbox_description: struct {
            text: []const u8,
        },
        fnkbox_testcases: struct {},

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

            pub const Kind = enum { empty, atom_lit, atom_var, pair };

            pub fn contains(sexpr_point: Point, is_pattern: bool, kind: Kind, needle_pos: Vec2) bool {
                return ViewHelper.overlapsAtom(is_pattern, sexpr_point, needle_pos, switch (kind) {
                    .atom_var, .atom_lit, .empty => .atom,
                    .pair => .pair,
                });
            }
        };

        pub const Garland = struct {
            visible: bool = undefined,
            computed_height: f32 = 0,
        };

        pub const NewCase = struct {
            length: f32 = undefined,
            /// if 1, the newcase handle is right at the end of the cable
            handle_t: f32 = undefined,
        };

        pub const Executor = struct {
            const relative_input_point: Point = .{ .pos = .new(-1, 1.5) };
            const relative_garland_point: Point = .{ .pos = .new(4, 0) };
            const relative_crank_center: Point = .{ .pos = .new(-1, 4) };

            pub fn children(toybox: *Toybox, index: Lego.Index) struct {
                input: Lego.Index,
                garland: Lego.Index,
            } {
                assert(toybox.get(index).specific.tag() == .executor);
                const asdf = toybox.getChildrenExact(2, index);
                return .{
                    .input = asdf[0],
                    .garland = asdf[1],
                };
            }
        };

        pub const Fnkbox = struct {
            relative_box: Rect = .fromMeasureAndSizeV2(
                .top_center,
                .new(0, 0.75),
                .new(16, box_height),
            ),

            const relative_fnkname_point: Point = .{ .pos = .new(-1, 1), .scale = 0.5, .turns = 0.25 };
            const relative_top_testcase_point: Point = .{ .pos = .new(0, box_height - testcases_height) };
            const text_height: f32 = 2.4;
            const status_bar_height: f32 = 1;
            const testcases_header_height: f32 = 0.85;
            const testcases_height: f32 = 2.5 * visible_testcases;
            const box_height = text_height + status_bar_height + testcases_header_height + testcases_height;
            const visible_testcases = 2;
            const relative_executor_point: Point = .{ .pos = .new(-3, 1 + box_height) };
            const status_bar_goal: Rect = .fromMeasureAndSizeV2(
                .top_center,
                Vec2.new(0, 0.75).addY(text_height),
                .new(16, status_bar_height),
            );

            pub fn children(toybox: *Toybox, index: Lego.Index) struct {
                description: Lego.Index,
                status_bar: Lego.Index,
                testcases_area: Lego.Index,
                fnkname: Lego.Index,
                executor: Lego.Index,
            } {
                assert(toybox.get(index).specific.tag() == .fnkbox);
                const asdf = toybox.getChildrenExact(5, index);
                return .{
                    .description = asdf[0],
                    .status_bar = asdf[1],
                    .testcases_area = asdf[2],
                    .fnkname = asdf[3],
                    .executor = asdf[4],
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
    };

    pub fn handle(lego: *const Lego) ?Handle {
        const radius: Handle.Size = switch (lego.specific) {
            .sexpr,
            .area,
            .microscope,
            .button,
            .fnkbox_description,
            .fnkbox_testcases,
            .executor,
            => return null,
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
            .newcase => |newcase| .new(0, newcase.length * newcase.handle_t),
            else => .zero,
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

    pub fn get(toybox: *Toybox, index: Lego.Index) *Lego {
        return &toybox.all_legos.items[@intFromEnum(index)];
    }

    pub fn addChildLast(toybox: *Toybox, parent: Lego.Index, new_child: Lego.Index) void {
        // TODO: call insert?
        assert(parent != .nothing);
        if (new_child == .nothing) return;
        const parent_tree = &toybox.get(parent).tree;
        const child_tree = &toybox.get(new_child).tree;
        assert(child_tree.isFloating());
        child_tree.parent = parent;
        child_tree.prev = parent_tree.last;
        child_tree.next = .nothing;
        if (parent_tree.last != .nothing) {
            toybox.get(parent_tree.last).tree.next = new_child;
        }
        parent_tree.last = new_child;
        if (parent_tree.first == .nothing) {
            parent_tree.first = new_child;
        }
    }

    pub fn isFloating(toybox: *Toybox, index: Lego.Index) bool {
        return toybox.get(index).tree.isFloating();
    }

    pub fn destroyFloating(toybox: *Toybox, index: Lego.Index) void {
        assert(toybox.isFloating(index));

        var cur: Lego.Index = index;
        while (cur != .nothing) {
            const next = toybox.next_preordered(cur, index).next;
            // TODO: set undefined to catch bugs, can't do it now since it would mess the iteration
            // toybox.get(cur).* = undefined;
            // toybox.get(cur).index = index;
            toybox.get(cur).exists = false;
            cur = next;
        }

        // toybox.get(lego).free_next = toybox.free_head;
        // TODO: free the memory
        // @panic("TODO");
    }

    pub fn recreateFloating(toybox: *Toybox, data: Lego) void {
        assert(data.tree.isFloating());
        assert(!toybox.get(data.index).exists);
        toybox.get(data.index).* = data;
    }

    pub fn dupeIntoFloating(toybox: *Toybox, original: Lego.Index, dupe_children: bool) !Lego.Index {
        const result = try toybox.add(undefined, undefined);
        const result_index = result.index;
        result.* = toybox.get(original).*;
        result.index = result_index;
        result.tree.parent = .nothing;
        result.tree.next = .nothing;
        result.tree.prev = .nothing;

        if (dupe_children) {
            var cur = result.tree.first;
            result.tree.first = .nothing;
            result.tree.last = .nothing;
            while (cur != .nothing) : (cur = toybox.get(cur).tree.next) {
                const new_child_index = try toybox.dupeIntoFloating(cur, true);
                toybox.addChildLast(result_index, new_child_index);
            }
        } else {
            result.tree.first = .nothing;
            result.tree.last = .nothing;
        }

        return result_index;
    }

    pub fn getChildrenExact(toybox: *Toybox, comptime expected_count: usize, parent: Lego.Index) [expected_count]Lego.Index {
        var cur = toybox.get(parent).tree.first;
        var result: [expected_count]Lego.Index = undefined;
        for (&result) |*dst| {
            assert(cur != .nothing);
            dst.* = cur;
            cur = toybox.get(cur).tree.next;
        }
        assert(cur == .nothing);
        return result;
    }

    pub fn childCount(toybox: *Toybox, index: Lego.Index) usize {
        var count: usize = 0;
        var cur = toybox.get(index).tree.first;
        while (cur != .nothing) {
            count += 1;
            cur = toybox.get(cur).tree.next;
        }
        return count;
    }

    pub fn getChildrenUnknown(toybox: *Toybox, allocator: std.mem.Allocator, parent: Lego.Index) ![]Lego.Index {
        const children_count: usize = toybox.childCount(parent);
        const result = try allocator.alloc(Lego.Index, children_count);
        var cur = toybox.get(parent).tree.first;
        for (result) |*dst| {
            assert(cur != .nothing);
            dst.* = cur;
            cur = toybox.get(cur).tree.next;
        }
        assert(cur == .nothing);
        return result;
    }

    pub fn pop(toybox: *Toybox, child: Lego.Index) void {
        assert(!toybox.isFloating(child));
        changeChild(toybox, child, .nothing);
    }

    pub fn insert(toybox: *Toybox, child: Lego.Index, where: Lego.Tree) void {
        assert(toybox.isFloating(child));
        assert(!where.isFloating());
        defer assert(toybox.get(child).tree.equals(where));

        if (where.prev != .nothing) {
            assert(toybox.get(where.prev).tree.next == where.next);
            toybox.get(where.prev).tree.next = child;
        } else {
            toybox.get(where.parent).tree.first = child;
        }

        if (where.next != .nothing) {
            assert(toybox.get(where.next).tree.prev == where.prev);
            toybox.get(where.next).tree.prev = child;
        } else {
            toybox.get(where.parent).tree.last = child;
        }

        toybox.get(child).tree = where;
    }

    /// things that pointed to original, now will point to new
    /// original will be left floating
    pub fn changeChild(toybox: *Toybox, original_child: Lego.Index, new_child: Lego.Index) void {
        assert(original_child != .nothing);
        assert(new_child == .nothing or toybox.isFloating(new_child));
        defer assert(toybox.isFloating(original_child));
        const original_tree: Lego.Tree = toybox.get(original_child).tree;
        assert(original_tree.parent != .nothing);
        const parent_tree: *Lego.Tree = &toybox.get(original_tree.parent).tree;
        if (parent_tree.first == original_child) {
            parent_tree.first = if (new_child != .nothing) new_child else original_tree.next;
        }
        if (@hasField(Lego.Tree, "last")) {
            if (parent_tree.last == original_child) {
                parent_tree.last = if (new_child != .nothing) new_child else original_tree.prev;
            }
        }
        if (original_tree.prev != .nothing) {
            toybox.get(original_tree.prev).tree.next = if (new_child != .nothing) new_child else original_tree.next;
        }
        if (original_tree.next != .nothing) {
            toybox.get(original_tree.next).tree.prev = if (new_child != .nothing) new_child else original_tree.prev;
        }
        if (new_child != .nothing) {
            const new_child_tree = &toybox.get(new_child).tree;
            assert(new_child_tree.parent == .nothing and
                new_child_tree.prev == .nothing and
                new_child_tree.next == .nothing);
            new_child_tree.parent = original_tree.parent;
            new_child_tree.next = original_tree.next;
            new_child_tree.prev = original_tree.prev;
        }
        toybox.get(original_child).tree.parent = .nothing;
        toybox.get(original_child).tree.next = .nothing;
        toybox.get(original_child).tree.prev = .nothing;
    }

    pub const VisitStep = struct {
        next: Lego.Index,
        // push_count: i32,
        // pop_count: i32,
    };

    /// root to leaf, from first to last child
    pub fn next_preordered(toybox: *Toybox, current: Lego.Index, root: Lego.Index) VisitStep {
        assert(root != .nothing and current != .nothing);
        var result: VisitStep = .{ .next = .nothing };
        // var result: VisitStep = .{ .next = .nothing, .pop_count = 0, .push_count = 0 };
        const cur = toybox.get(current);
        if (cur.tree.first != .nothing) {
            result.next = cur.tree.first;
            // result.push_count = 1;
        } else {
            var p = current;
            while (p != .nothing and p != root) : (p = toybox.get(p).tree.parent) {
                const next = toybox.get(p).tree.next;
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
    pub fn next_postordered(toybox: *Toybox, current: Lego.Index, root: Lego.Index) VisitStep {
        assert(root != .nothing and current != .nothing);
        var result: VisitStep = .{ .next = .nothing };
        // var result: VisitStep = .{ .next = .nothing, .pop_count = 0, .push_count = 0 };
        const cur = toybox.get(current);
        if (cur.tree.last != .nothing) {
            result.next = cur.tree.last;
            // result.push_count = 1;
        } else {
            var p = current;
            while (p != .nothing and p != root) : (p = toybox.get(p).tree.parent) {
                const next = toybox.get(p).tree.prev;
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

    pub fn treeIterator(toybox: *Toybox, root: Lego.Index, first_to_last: bool) TreeIterator {
        return .{
            .toybox = toybox,
            .root = root,
            .cur = root,
            .first_to_last = first_to_last,
        };
    }

    pub const TreeIterator = struct {
        toybox: *Toybox,
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
            const tree = it.toybox.get(it.cur).tree;
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
    };

    test "iteration order" {
        var toybox: Toybox = undefined;
        try toybox.init(std.testing.allocator);
        defer toybox.deinit();
        const root = try toybox.add(undefined, undefined);
        const child_1 = try toybox.add(undefined, undefined);
        const child_2 = try toybox.add(undefined, undefined);
        const grandchild_1_1 = try toybox.add(undefined, undefined);
        const grandchild_1_2 = try toybox.add(undefined, undefined);
        const grandchild_2_1 = try toybox.add(undefined, undefined);
        const grandchild_2_2 = try toybox.add(undefined, undefined);

        toybox.addChildLast(root.index, child_1.index);
        toybox.addChildLast(root.index, child_2.index);

        toybox.addChildLast(child_1.index, grandchild_1_1.index);
        toybox.addChildLast(child_1.index, grandchild_1_2.index);

        toybox.addChildLast(child_2.index, grandchild_2_1.index);
        toybox.addChildLast(child_2.index, grandchild_2_2.index);

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
            while (cur.next != .nothing) : (cur = toybox.next_preordered(cur.next, root.index)) {
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

            var it = toybox.treeIterator(root.index, true);
            while (it.next()) |step| {
                try actual_order.append(std.testing.allocator, step);
            }

            try std.testing.expectEqualSlices(TreeIterator.Step, &expected_order, actual_order.items);
        }
    }

    pub fn oldestAncestor(toybox: *Toybox, index: Lego.Index) Lego.Index {
        assert(index != .nothing);
        var cur = index;
        while (true) {
            const next = toybox.get(cur).tree.parent;
            if (next == .nothing) return cur;
            cur = next;
        }
    }

    pub fn parentAbsolutePoint(toybox: *Toybox, index: Lego.Index) Point {
        assert(index != .nothing);
        const parent = toybox.get(index).tree.parent;
        if (parent == .nothing) return .{};
        return toybox.get(parent).absolute_point;
    }

    pub fn refreshAbsolutePoints(toybox: *Toybox, roots: []const Lego.Index) void {
        for (roots) |root| {
            var cur: Lego.Index = root;
            while (cur != .nothing) : (cur = toybox.next_preordered(cur, root).next) {
                toybox.get(cur).absolute_point = toybox
                    .parentAbsolutePoint(cur)
                    .applyToLocalPoint(toybox.get(cur).local_point);
            }
        }
    }

    pub fn changeCoordinates(toybox: *Toybox, index: Lego.Index, old_parent: Point, new_parent: Point) void {
        toybox.get(index).local_point = new_parent.inverseApplyGetLocal(old_parent.applyToLocalPoint(toybox.get(index).local_point));
    }

    pub fn buildSexpr(toybox: *Toybox, local_point: Point, value: union(Lego.Specific.Sexpr.Kind) {
        empty,
        atom_lit: []const u8,
        atom_var: []const u8,
        pair: struct { up: Lego.Index, down: Lego.Index },
    }, is_pattern: bool) !Lego.Index {
        const result = try toybox.add(local_point, .{ .sexpr = .{
            .is_pattern = is_pattern,
            .is_pattern_t = if (is_pattern) 1 else 0,
            .atom_name = switch (value) {
                .atom_lit, .atom_var => |v| v,
                else => undefined,
            },
            .kind = value,
        } });
        switch (value) {
            else => {},
            .pair => |pair| {
                toybox.addChildLast(result.index, pair.up);
                toybox.addChildLast(result.index, pair.down);
            },
        }
        return result.index;
    }

    pub fn buildCase(toybox: *Toybox, local_point: Point, data: struct {
        pattern: Lego.Index,
        template: Lego.Index,
        fnkname: Lego.Index,
        next: ?Lego.Index = null,
    }) !Lego.Index {
        const result = try toybox.add(local_point, .case);
        toybox.addChildLast(result.index, data.pattern);
        toybox.addChildLast(result.index, data.template);
        toybox.addChildLast(result.index, data.fnkname);
        toybox.addChildLast(result.index, data.next orelse try toybox.buildGarland(local_point, &.{}));
        return result.index;
    }

    /// The garland's children are a linear list of newcase, all except the last one with a child case
    /// the newcase position is the very top of the segment
    pub fn buildGarland(toybox: *Toybox, local_point: Point, child_cases: []const Lego.Index) !Lego.Index {
        const result = try toybox.add(local_point, .{ .garland = .{} });
        for (child_cases) |case| {
            const new_segment = try toybox.add(.{}, .{ .newcase = .{} });
            toybox.addChildLast(new_segment.index, case);
            toybox.addChildLast(result.index, new_segment.index);
        }
        toybox.addChildLast(result.index, (try toybox.add(.{}, .{ .newcase = .{} })).index);
        return result.index;
    }

    /// Children are:
    /// - description area
    /// - status bar
    /// - testcases area
    /// - fnkname
    /// - executor
    pub fn buildFnkbox(
        toybox: *Toybox,
        local_point: Point,
        // TODO: take *const Sepxr
        fnkname: Lego.Index,
        text: []const u8,
        // testcases_values
        initial_definition: ?Lego.Index,
    ) !Lego.Index {
        const result = try toybox.add(local_point, .{ .fnkbox = .{} });
        toybox.addChildLast(result.index, (try toybox.add(.{}, .{
            .fnkbox_description = .{
                .text = text,
            },
        })).index);
        toybox.addChildLast(result.index, (try toybox.add(.{}, .{
            .button = .{
                .local_rect = .fromMeasureAndSizeV2(.top_center, .zero, .new(16, 1)),
            },
        })).index);
        toybox.addChildLast(result.index, (try toybox.add(.{}, .{
            .fnkbox_testcases = .{},
        })).index);
        toybox.addChildLast(result.index, fnkname);
        const executor = try toybox.add(.{}, .{ .executor = .{} });
        toybox.addChildLast(executor.index, try toybox.buildSexpr(.{}, .empty, false));
        toybox.addChildLast(executor.index, initial_definition orelse try toybox.buildGarland(.{}, &.{}));
        toybox.addChildLast(result.index, executor.index);
        return result.index;
    }

    pub fn buildMicroscope(toybox: *Toybox, source: Vec2, target: Vec2) !Lego.Index {
        const lens_source = try toybox.add(.{ .pos = source }, .{ .lens = .source });
        const lens_target = try toybox.add(.{ .pos = target }, .{ .lens = .target });
        const result = try toybox.add(.{}, .microscope);
        toybox.addChildLast(result.index, lens_source.index);
        toybox.addChildLast(result.index, lens_target.index);
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

    undo_stack: std.ArrayList(UndoableCommand),
    random_instance: std.Random.DefaultPrng,
    arena_for_atom_names: std.heap.ArenaAllocator,

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

        dst.main_area = (try dst.toybox.add(.{ .scale = 0.1 }, .{ .area = .{ .bg = .all } })).index;
        dst.toolbar_left = (try dst.toybox.add(.{}, .{
            .area = .{
                .bg = .{
                    // ensure that "mouse off-screen on the left" also overlaps the toolbar
                    .local_rect = toolbar_left_rect.plusMargin3(.left, 100),
                },
            },
        })).index;
        dst.lenses_layer = (try dst.toybox.add(undefined, .{ .area = .{ .bg = .none } })).index;

        try dst.regenerateToolbarLeft();

        if (true) {
            dst.toybox.addChildLast(
                dst.main_area,
                try dst.toybox.buildFnkbox(
                    .{},
                    try dst.toybox.buildSexpr(
                        .{},
                        .{ .atom_lit = "true" },
                        true,
                    ),
                    "do lowercase",
                    null,
                ),
            );
        }

        if (false) {
            dst.toybox.addChildLast(dst.main_area, try dst.toybox.buildSexpr(
                .{ .pos = .new(0, 0) },
                .{ .atom_lit = "true" },
                false,
            ));

            dst.toybox.addChildLast(dst.main_area, try dst.toybox.buildSexpr(
                .{ .pos = .new(0, 1) },
                .{ .atom_lit = "false" },
                false,
            ));

            dst.toybox.addChildLast(dst.main_area, try dst.toybox.buildSexpr(
                .{ .pos = .new(3, 0) },
                .{ .pair = .{
                    .up = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, false),
                    .down = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                } },
                false,
            ));

            dst.toybox.addChildLast(dst.main_area, try dst.toybox.buildCase(
                .{ .pos = .new(0, 4) },
                .{
                    .pattern = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true),
                    .template = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                    .fnkname = try dst.toybox.buildSexpr(.{}, .empty, false),
                },
            ));

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
            dst.toybox.addChildLast(dst.main_area, try dst.toybox.buildGarland(
                .{ .pos = .new(7, 4) },
                &.{ case_1, case_2 },
            ));

            dst.toybox.addChildLast(dst.lenses_layer, try dst.toybox.buildMicroscope(
                .new(2, 2),
                .new(4, 3),
            ));

            dst.toybox.addChildLast(dst.lenses_layer, try dst.toybox.buildMicroscope(
                .new(4, 3),
                .new(6, 2),
            ));
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
            var it = toybox.treeIterator(root, false);
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
                                return .{ .hot = cur, .over_background = root };
                            } else if (grabbing != .nothing and toybox.get(grabbing).specific.tag() == .sexpr) {
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
                                toybox,
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
                            area.bg.contains(lego.absolute_point, absolute_needle_pos))
                        {
                            return .{ .over_background = cur };
                        }
                    },
                    .button => |button| {
                        if (step.children_already_visited and
                            button.local_rect.contains(lego.absolute_point.inverseApplyGetLocalPosition(absolute_needle_pos)))
                        {
                            return .{ .hot = cur, .over_background = root };
                        }
                    },
                    .case,
                    .newcase,
                    .microscope,
                    .garland,
                    .executor,
                    .fnkbox,
                    .fnkbox_description,
                    .fnkbox_testcases,
                    => {},
                }
                if (step.children_already_visited) {
                    if (lego.handle()) |handle| {
                        const overlappable: bool, const kind: enum { hot, drop } = switch (lego.specific) {
                            .sexpr, .area, .microscope, .fnkbox_description, .fnkbox_testcases, .button, .executor => unreachable,
                            .case, .lens, .fnkbox => .{ grabbing == .nothing, .hot },
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

    fn updateSprings(workspace: *Workspace, absolute_mouse_pos: Vec2, interaction: HotAndDropzone, delta_seconds: f32) void {
        const toybox = &workspace.toybox;

        defer toybox.refreshAbsolutePoints(workspace.roots(.all).constSlice());
        for (workspace.roots(.all).constSlice()) |root| {
            var cur: Lego.Index = root;
            while (cur != .nothing) : (cur = toybox.next_preordered(cur, root).next) {
                const lego = toybox.get(cur);
                if (cur == workspace.grabbing) {
                    const target: Point = if (interaction.dropzone == .nothing)
                        // TODO: i don't like the scale hack
                        (Point{
                            .pos = absolute_mouse_pos,
                            .scale = toybox.get(interaction.over_background).absolute_point.scale,
                        }).applyToLocalPoint(.{ .pos = lego.handleLocalOffset().neg() })
                    else
                        toybox.get(interaction.dropzone).absolute_point.applyToLocalPoint(.{ .pos = toybox.get(interaction.dropzone).handleLocalOffset() });

                    lego.local_point.lerp_towards(toybox.parentAbsolutePoint(cur)
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

                        if (sexpr.kind == .pair) {
                            const child_up, const child_down = toybox.getChildrenExact(2, cur);
                            toybox.get(child_up).local_point = (Point{})
                                .applyToLocalPoint(lego.visual_offset)
                                .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .up));
                            toybox.get(child_down).local_point = (Point{})
                                .applyToLocalPoint(lego.visual_offset)
                                .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .down));

                            toybox.get(child_up).specific.sexpr.is_pattern = sexpr.is_pattern;
                            toybox.get(child_up).specific.sexpr.is_pattern_t = if (sexpr.is_pattern) 1 else 0;
                            toybox.get(child_down).specific.sexpr.is_pattern = sexpr.is_pattern;
                            toybox.get(child_down).specific.sexpr.is_pattern_t = if (sexpr.is_pattern) 1 else 0;
                        }
                    },
                    .case => {
                        const offsets: [4]Point = .{
                            .{ .pos = .xneg },
                            .{ .pos = .xpos },
                            .{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) },
                            .{ .pos = .new(8, 1) },
                        };
                        // const pattern, const template, const fnkname = toybox.getChildrenExact(3, cur);
                        // for (.{ pattern, template, fnkname }, offsets) |i, offset| {
                        for (toybox.getChildrenExact(4, cur), offsets) |i, offset| {
                            toybox.get(i).local_point = offset;
                        }
                    },
                    .garland => |*garland| {
                        var a = toybox.get(lego.tree.first);
                        var offset: Vec2 = .zero;
                        while (true) {
                            assert(a.specific.tag() == .newcase);
                            a.local_point = .{ .pos = offset };
                            offset.addInPlace(.new(0, a.specific.newcase.length));

                            if (a.tree.next == .nothing) break;
                            a = toybox.get(a.tree.next);
                        }
                        garland.computed_height = offset.y;
                    },
                    .newcase => |*newcase| {
                        const target_length: f32, const target_handle_t: f32 = blk: {
                            const prev_height: f32 = if (lego.tree.prev == .nothing)
                                0
                            else blk2: {
                                const case_of_prev_segment = toybox.get(lego.tree.prev).tree.first;
                                const garland_of_case_of_prev_segment = toybox.get(case_of_prev_segment).tree.last;
                                if (garland_of_case_of_prev_segment == interaction.dropzone) {
                                    break :blk2 toybox.get(workspace.grabbing).specific.garland.computed_height;
                                } else {
                                    break :blk2 toybox.get(garland_of_case_of_prev_segment).specific.garland.computed_height;
                                }
                            };
                            if (lego.tree.first != .nothing) {
                                assert(lego.tree.last == lego.tree.first);
                                assert(lego.tree.next != .nothing);
                                assert(toybox.get(lego.tree.first).specific.tag() == .case);
                                toybox.get(lego.tree.first).local_point = .{ .pos = .new(0, newcase.length) };
                                break :blk .{ 1.5 + prev_height, 0.5 };
                            } else {
                                assert(lego.tree.next == .nothing);
                                break :blk .{ 1 + prev_height, 1 };
                            }
                        };
                        math.lerpTowards(&newcase.handle_t, target_handle_t, .fast, delta_seconds);
                        math.lerpTowards(&newcase.length, target_length + 2.5 * 0.5 * lego.dropzone_t, .slow, delta_seconds);
                    },
                    .fnkbox => |*fnkbox| {
                        // FIXME
                        _ = fnkbox;
                        const Fnkbox = Lego.Specific.Fnkbox;
                        const children = Fnkbox.children(toybox, cur);
                        toybox.get(children.fnkname).local_point = Fnkbox.relative_fnkname_point;
                        toybox.get(children.status_bar).local_point = .{};
                        toybox.get(children.status_bar).specific.button.local_rect = Fnkbox.status_bar_goal;
                        toybox.get(children.executor).local_point = Fnkbox.relative_executor_point;
                    },
                    .executor => {
                        const Executor = Lego.Specific.Executor;
                        const children = Executor.children(toybox, cur);
                        toybox.get(children.garland).local_point = Executor.relative_garland_point;
                        toybox.get(children.input).local_point = Executor.relative_input_point;
                    },
                    .fnkbox_description, .fnkbox_testcases => {
                        // FIXME
                    },
                    .area, .microscope, .lens, .button => {},
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
        const toybox = &workspace.toybox;

        drawer.canvas.clipper.reset();
        drawer.canvas.clipper.use(drawer.canvas);

        for (workspace.roots(.all).constSlice()) |root| {
            try _draw(toybox, root, camera, drawer);
        }

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

    fn _draw(toybox: *Toybox, root: Lego.Index, camera: Rect, drawer: *Drawer) !void {
        var it = toybox.treeIterator(root, true);
        while (it.next()) |step| {
            const cur = step.index;
            const lego = toybox.get(cur);
            // TODO: don't draw if small or far from camera
            if (step.children_already_visited) {
                if (lego.handle()) |handle| try handle.draw(drawer, camera);
                if (lego.specific.tag() == .fnkbox_testcases) {
                    drawer.canvas.clipper.pop();
                }
            } else {
                const point = lego.absolute_point.applyToLocalPoint(lego.visual_offset);
                switch (lego.specific) {
                    .sexpr => |sexpr| {
                        switch (sexpr.kind) {
                            .empty => {},
                            .atom_lit => try drawer.drawAtom(camera, point, sexpr.is_pattern, sexpr.atom_name, 1),
                            .pair => try drawer.drawPairHolder(camera, point, sexpr.is_pattern, 1),
                            .atom_var => try drawer.drawVariable(camera, point, sexpr.is_pattern, sexpr.atom_name, 1),
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

                                for (lens.roots_to_draw) |asdf| {
                                    try _draw(toybox, asdf, lens.transform.getCamera(camera), drawer);
                                }
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
                    .fnkbox_testcases => {
                        drawer.canvas.clipper.push(.{
                            .camera = camera,
                            .shape = .{
                                // FIXME
                                .rect = .unit,
                            },
                        }) catch @panic("TOO DEEP");
                    },
                    .newcase => |newcase| {
                        // TODO: use .reparentCamera
                        drawer.canvas.line(camera, &.{
                            lego.absolute_point.pos,
                            lego.absolute_point.pos.addY(newcase.length * lego.absolute_point.scale),
                        }, 0.05 * lego.absolute_point.scale, .black);
                    },
                    .fnkbox => |fnkbox| {
                        const relative_camera = camera.reparentCamera(lego.absolute_point);
                        drawer.canvas.borderRect(relative_camera, fnkbox.relative_box, 0.05, .inner, .black);
                    },
                    .case, .garland, .microscope, .fnkbox_description, .executor => {},
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

        if (platform.keyboard.wasPressed(.KeyZ)) {
            while (workspace.undo_stack.pop()) |command| {
                switch (command) {
                    .fence => break,
                    .destroy_floating => |index| {
                        toybox.destroyFloating(index);
                    },
                    .recreate_floating => |data| {
                        // TODO: recreate children too!
                        toybox.recreateFloating(data);
                    },
                    .insert => |insert| {
                        toybox.insert(insert.what, insert.where);
                    },
                    .set_data_except_tree => |data| {
                        // TODO: set the children data too!
                        const original_tree = toybox.get(data.index).tree;
                        toybox.get(data.index).* = data;
                        toybox.get(data.index).tree = original_tree;
                    },
                    .pop => |index| {
                        toybox.pop(index);
                    },
                    .set_grabbing => |index| {
                        workspace.grabbing = index;
                    },
                    .set_handlayer => |index| {
                        workspace.hand_layer = index;
                    },
                    .change_child => |change| {
                        toybox.changeChild(change.original, change.new);
                    },
                }
            }
        }

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
            math.lerp_towards(&lego.hot_t, if (lego.index == hot_and_dropzone.hot) 1 else 0, 0.6, platform.delta_seconds);
            comptime assert(!@hasField(Lego, "active_t"));
            math.lerp_towards(&lego.dropzone_t, if (lego.index == hot_and_dropzone.dropzone) 1 else 0, 0.6, platform.delta_seconds);
            math.lerp_towards(&lego.dropping_t, if (lego.index == workspace.grabbing and hot_and_dropzone.dropzone != .nothing) 1 else 0, 0.6, platform.delta_seconds);

            switch (lego.specific) {
                .sexpr => |*sexpr| {
                    math.lerp_towards(&sexpr.is_pattern_t, if (sexpr.is_pattern) 1 else 0, 0.6, platform.delta_seconds);
                },
                .area,
                .case,
                .newcase,
                .garland,
                .microscope,
                .lens,
                .executor,
                .fnkbox,
                .fnkbox_description,
                .fnkbox_testcases,
                .button,
                => {},
            }
        }

        // TODO: a bit hacky
        if (true) { // set garlands visibility
            const grabbing_garland_or_case: bool = if (workspace.grabbing == .nothing)
                false
            else switch (toybox.get(workspace.grabbing).specific) {
                .case, .garland => true,
                else => false,
            };
            for (toybox.all_legos.items) |*lego| {
                const in_toolbar = toybox.oldestAncestor(lego.index) == workspace.toolbar_left;
                if (lego.specific.as(.garland)) |garland| {
                    garland.visible = !in_toolbar and (grabbing_garland_or_case or lego.tree.first != .nothing);
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
                        p.pos.addInPlace(kv.dir.scale(platform.delta_seconds * -2));
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
                platform.delta_seconds,
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

        // includes dragging and snapping to dropzone, since that's just the spring between the mouse cursor/dropzone and the grabbed thing
        workspace.updateSprings(mouse.cur.position, hot_and_dropzone, platform.delta_seconds);

        if (true) { // set lenses data
            const microscopes = try toybox.getChildrenUnknown(scratch, workspace.lenses_layer);
            for (microscopes, 0..) |microscope, k| {
                const source, const target = workspace.toybox.getChildrenExact(2, microscope);
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

        // INTERACTION
        try workspace.undo_stack.ensureUnusedCapacity(32);
        if (workspace.grabbing == .nothing and
            hot_and_dropzone.hot != .nothing and
            (mouse.wasPressed(.left) or mouse.wasPressed(.right)))
        {
            // Main case A: plucking/grabbing/clicking something
            try workspace.undo_stack.append(.fence);

            const hot_index = hot_and_dropzone.hot;
            const original_hot_data = toybox.get(hot_index).*;
            const hot_parent = original_hot_data.tree.parent;
            const original_parent_absolute_point = toybox.parentAbsolutePoint(hot_index);

            var grabbed_element_index: Lego.Index = undefined;
            var plucked: bool = true;

            if (mouse.wasPressed(.right)) {
                // Case A.0: duplicating
                // TODO
                // if (original_hot_data.canDuplicate()) {
                const new_element_index = try toybox.dupeIntoFloating(hot_index, true);
                workspace.undo_stack.appendAssumeCapacity(.{
                    .destroy_floating = new_element_index,
                });
                grabbed_element_index = new_element_index;
                // }
            } else if (hot_parent != .nothing and toybox.get(hot_parent).specific.tag() == .area) {
                // Case A.1: plucking a top-level thing
                workspace.undo_stack.appendAssumeCapacity(.{
                    .set_data_except_tree = original_hot_data,
                });

                toybox.pop(hot_index);
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

                const new_empty_sexpr = try toybox.dupeIntoFloating(hot_index, false);
                toybox.get(new_empty_sexpr).specific.sexpr.kind = .empty;
                workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = new_empty_sexpr });

                toybox.changeChild(hot_index, new_empty_sexpr);
                workspace.undo_stack.appendAssumeCapacity(.{ .change_child = .{
                    .new = hot_index,
                    .original = new_empty_sexpr,
                } });

                grabbed_element_index = hot_index;
            } else if (original_hot_data.specific.tag() == .lens) {
                // Case A.3: grabbing rather than plucking
                workspace.undo_stack.appendAssumeCapacity(.{
                    .set_data_except_tree = original_hot_data,
                });
                grabbed_element_index = hot_index;
                plucked = false;
            } else if (hot_parent != .nothing and toybox.get(hot_parent).specific.tag() == .newcase) {
                // Case A.4: plucking a case from a garland
                assert(original_hot_data.specific.tag() == .case);
                workspace.undo_stack.appendAssumeCapacity(.{
                    .set_data_except_tree = original_hot_data,
                });

                toybox.pop(hot_index);
                workspace.undo_stack.appendAssumeCapacity(.{
                    .insert = .{
                        .what = hot_index,
                        .where = original_hot_data.tree,
                    },
                });

                const original_parent_tree = toybox.get(hot_parent).tree;
                toybox.get(original_parent_tree.next).specific.newcase.length += toybox.get(hot_parent).specific.newcase.length;
                toybox.pop(hot_parent);
                workspace.undo_stack.appendAssumeCapacity(.{
                    .insert = .{
                        .what = hot_parent,
                        .where = original_parent_tree,
                    },
                });

                grabbed_element_index = hot_index;
            } else if (toybox.get(hot_index).specific.tag() == .garland) {
                // Case A.5: plucking a garland, and replacing it with an empty one
                workspace.undo_stack.appendAssumeCapacity(.{
                    .set_data_except_tree = original_hot_data,
                });

                const new_garland = try toybox.buildGarland(original_hot_data.local_point, &.{});
                workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = new_garland });

                toybox.changeChild(hot_index, new_garland);
                workspace.undo_stack.appendAssumeCapacity(.{ .change_child = .{
                    .new = hot_index,
                    .original = new_garland,
                } });

                grabbed_element_index = hot_index;
            } else unreachable;

            assert(workspace.grabbing == .nothing and workspace.hand_layer == .nothing);
            workspace.grabbing = grabbed_element_index;
            workspace.undo_stack.appendAssumeCapacity(.{
                .set_grabbing = .nothing,
            });
            if (plucked) {
                workspace.hand_layer = grabbed_element_index;
                workspace.undo_stack.appendAssumeCapacity(.{
                    .set_handlayer = .nothing,
                });
                toybox.changeCoordinates(grabbed_element_index, original_parent_absolute_point, .{});
                toybox.refreshAbsolutePoints(&.{grabbed_element_index});
            }
        } else if (workspace.grabbing != .nothing and
            !(mouse.cur.isDown(.left) or mouse.cur.isDown(.right)))
        {
            const dropzone_index = hot_and_dropzone.dropzone;

            if (dropzone_index != .nothing) {
                assert(toybox.isFloating(workspace.grabbing));
                if (toybox.get(dropzone_index).specific.tag() == .newcase) {
                    // TODO: avoid jumpyness
                    assert(toybox.get(workspace.grabbing).specific.tag() == .case);
                    const newcase = try toybox.add(.{}, .{ .newcase = .{} });
                    workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = newcase.index });
                    const original_tree = toybox.get(dropzone_index).tree;
                    toybox.insert(newcase.index, .{
                        .parent = original_tree.parent,
                        .prev = original_tree.prev,
                        .next = dropzone_index,
                        .first = .nothing,
                        .last = .nothing,
                    });
                    workspace.undo_stack.appendAssumeCapacity(.{ .pop = newcase.index });
                    toybox.changeCoordinates(workspace.grabbing, .{}, toybox.parentAbsolutePoint(dropzone_index));
                    toybox.addChildLast(newcase.index, workspace.grabbing);
                    workspace.undo_stack.appendAssumeCapacity(.{ .pop = workspace.grabbing });
                } else {
                    toybox.changeCoordinates(workspace.grabbing, .{}, toybox.parentAbsolutePoint(dropzone_index));
                    toybox.refreshAbsolutePoints(&.{workspace.grabbing});
                    toybox.changeChild(dropzone_index, workspace.grabbing);
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .change_child = .{
                            .original = workspace.grabbing,
                            .new = dropzone_index,
                        },
                    });

                    const overwritten_data = toybox.get(dropzone_index).*;
                    toybox.destroyFloating(dropzone_index);
                    workspace.undo_stack.appendAssumeCapacity(.{
                        .recreate_floating = overwritten_data,
                    });
                }
            } else if (!toybox.isFloating(workspace.grabbing)) {
                // Case B.2: releasing a grabbed thing
                assert(dropzone_index == .nothing);
            } else {
                // Case B.3: dropping a floating thing on fresh space
                const target_area = hot_and_dropzone.over_background;
                toybox.changeCoordinates(workspace.grabbing, .{}, toybox.get(target_area).absolute_point);
                toybox.refreshAbsolutePoints(&.{workspace.grabbing});
                toybox.addChildLast(target_area, workspace.grabbing);
                workspace.undo_stack.appendAssumeCapacity(.{
                    .pop = workspace.grabbing,
                });
            }

            workspace.undo_stack.appendAssumeCapacity(.{ .set_grabbing = workspace.grabbing });
            workspace.undo_stack.appendAssumeCapacity(.{ .set_handlayer = workspace.hand_layer });
            workspace.grabbing = .nothing;
            workspace.hand_layer = .nothing;
        }
    }

    fn regenerateToolbarLeft(workspace: *Workspace) !void {
        const toybox = &workspace.toybox;
        try workspace.undo_stack.ensureUnusedCapacity(32);

        if (true) { // delete all current children
            var cur = toybox.get(workspace.toolbar_left).tree.first;
            while (cur != .nothing) {
                const original_tree = toybox.get(cur).tree;
                toybox.pop(cur);
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

            const index = try toybox.buildCase(.{ .pos = .new(2.5, 5) }, .{
                .pattern = try toybox.buildSexpr(.{}, .{ .atom_var = new_name }, true),
                .template = try toybox.buildSexpr(.{}, .{ .pair = .{
                    .up = try toybox.buildSexpr(.{}, .{ .atom_var = new_name }, false),
                    .down = try toybox.buildSexpr(.{}, .{ .atom_lit = "nil" }, false),
                } }, false),
                .fnkname = try toybox.buildSexpr(.{}, .empty, false),
            });

            workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = index });
            toybox.addChildLast(workspace.toolbar_left, index);
            workspace.undo_stack.appendAssumeCapacity(.{ .pop = index });
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
