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

// TODO: draw order should be the reverse of hitbox testing order

test "fuzz example" {
    const TestPlatform = struct {
        global_seconds: f32 = 0,
        delta_seconds: f32 = 0,
        mouse: Mouse = .{ .cur = .init, .prev = .init, .cur_time = 0 },
        keyboard: Keyboard = .{ .cur = .init, .prev = .init, .cur_time = 0 },

        const TestPlatform = @This();

        pub fn after(self: *TestPlatform) void {
            self.mouse.prev = self.mouse.cur;
            self.mouse.cur.scrolled = .none;
            self.keyboard.prev = self.keyboard.cur;
        }

        pub fn getGives(self: *TestPlatform, delta_seconds: f32) PlatformGives {
            self.keyboard.cur_time = self.global_seconds;
            self.mouse.cur_time = self.global_seconds;
            self.global_seconds += delta_seconds;

            return .{
                .mouse = self.mouse,
                .keyboard = self.keyboard,
                .gpa = std.testing.allocator,

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

    const Context = struct {
        fn testOne(_: @This(), input: []const u8) anyerror!void {
            var workspace: Workspace = undefined;
            try workspace.init(std.testing.allocator);
            defer workspace.deinit();

            var test_platform: TestPlatform = .{};

            var it = std.mem.window(u8, input, @sizeOf(FakeInput), @sizeOf(FakeInput));
            while (it.next()) |cur_input_raw| {
                if (cur_input_raw.len == @sizeOf(FakeInput)) {
                    const cur_input = std.mem.bytesToValue(FakeInput, cur_input_raw);
                    test_platform.keyboard.cur.keys.KeyZ = cur_input.z_down;
                    test_platform.mouse.cur.buttons.left = cur_input.mouse_left_down;
                    test_platform.mouse.cur.position = cur_input.mouse_pos;
                    try workspace.update(test_platform.getGives(1.0 / 60.0), null);
                }
            }
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}

test "No leaks on Workspace and Drawer" {
    var workspace: Workspace = undefined;
    try workspace.init(std.testing.allocator);
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

pub const Sexpr = struct {
    pub const Kind = enum { empty, atom_lit, atom_var, pair };

    pub fn contains(sexpr_point: Point, is_pattern: bool, kind: Kind, needle_pos: Vec2) bool {
        return ViewHelper.overlapsAtom(is_pattern, sexpr_point, needle_pos, switch (kind) {
            .atom_var, .atom_lit, .empty => .atom,
            .pair => .pair,
        });
    }
};

/// Might be an Area, a Sexpr, a Case, etc
pub const Lego = struct {
    exists: bool = false,
    index: Index,
    /// absolute coordinates
    point: Point = .{},
    /// local coordinates
    visual_offset: Point = .{},
    hot_t: f32 = 0,
    // 1 if there is an element being dropped on this one
    // dropzone_t: f32 = 0,
    // active_t: f32 = 0,
    /// 1 if this element is being dropped into another
    dropping_t: f32 = 0,
    immutable: bool = false,
    sexpr: ?struct {
        kind: Sexpr.Kind,
        atom_name: []const u8,
        is_pattern: bool,
        is_pattern_t: f32,
    } = null,
    case: bool = false,
    handle_kind: enum {
        none,
        existing_case,

        pub fn getSize(kind: @This()) Handle.Size {
            return switch (kind) {
                .none => unreachable,
                .existing_case => .default,
            };
        }
    } = .none,

    tree: Tree = .{
        .first = .nothing,
        .last = .nothing,
        .next = .nothing,
        .prev = .nothing,
        .parent = .nothing,
    },

    pub const Tree = struct {
        first: Index,
        last: Index,
        next: Index,
        prev: Index,
        parent: Index,

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
        if (lego.handle_kind == .none) return null;
        return .{
            .point = lego.point.applyToLocalPoint(.{ .pos = lego.handleOffset() }),
            .hot_t = lego.hot_t,
            .radius = lego.handle_kind.getSize(),
        };
    }

    fn handleOffset(lego: *const Lego) Vec2 {
        _ = lego;
        return .zero;
    }
};

pub const Handle = struct {
    point: Point,
    radius: Size,
    hot_t: f32,
    // TODO: remove default value
    alpha: f32 = 1,
    // TODO: remove
    comptime enabled: bool = true,

    pub const Size = extern struct {
        base: f32,
        hot: f32,
        hitbox: f32,

        pub const default: Size = .{ .base = 0.2, .hot = 0.24, .hitbox = 0.24 };
        pub const zero: Size = .{ .base = 0, .hot = 0, .hitbox = 0 };
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
        dst.all_legos_arena = .init(gpa);
        dst.all_legos = .empty;
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

    pub fn add(toybox: *Toybox) !*Lego {
        const result = try toybox.all_legos.addOne(toybox.all_legos_arena.allocator());
        result.* = kommon.meta.initDefaultFields(Lego);
        result.index = @enumFromInt(toybox.all_legos.items.len - 1);
        result.exists = true;
        return result;
    }

    pub fn get(toybox: *Toybox, index: Lego.Index) *Lego {
        return &toybox.all_legos.items[@intFromEnum(index)];
    }

    pub fn addChildFirst(toybox: *Toybox, parent: Lego.Index, new_child: Lego.Index) void {
        // TODO: call insert?
        assert(parent != .nothing);
        if (new_child == .nothing) return;
        const parent_tree = &toybox.get(parent).tree;
        const child_tree = &toybox.get(new_child).tree;
        assert(child_tree.parent == .nothing and
            child_tree.next == .nothing and
            child_tree.prev == .nothing);
        child_tree.parent = parent;
        child_tree.next = parent_tree.first;
        child_tree.prev = .nothing;
        if (parent_tree.first != .nothing) {
            toybox.get(parent_tree.first).tree.prev = new_child;
        }
        parent_tree.first = new_child;
        if (parent_tree.last == .nothing) {
            parent_tree.last = new_child;
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
        // FIXME: free the memory
        // @panic("TODO");
    }

    pub fn recreateFloating(toybox: *Toybox, data: Lego) void {
        assert(data.tree.isFloating());
        assert(!toybox.get(data.index).exists);
        toybox.get(data.index).* = data;
    }

    pub fn dupeIntoFloating(toybox: *Toybox, original: Lego.Index, dupe_children: bool) !Lego.Index {
        const result = try toybox.add();
        const result_index = result.index;
        result.* = toybox.get(original).*;
        result.index = result_index;
        result.tree.parent = .nothing;
        result.tree.next = .nothing;
        result.tree.prev = .nothing;

        if (dupe_children) {
            var cur = result.tree.last;
            result.tree.first = .nothing;
            result.tree.last = .nothing;
            while (cur != .nothing) : (cur = toybox.get(cur).tree.prev) {
                const new_child_index = try toybox.dupeIntoFloating(cur, true);
                toybox.addChildFirst(result_index, new_child_index);
            }
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

    test "iteration order" {
        var toybox: Toybox = undefined;
        try toybox.init(std.testing.allocator);
        defer toybox.deinit();
        const root = try toybox.add();
        const child_1 = try toybox.add();
        const child_2 = try toybox.add();
        const grandchild_1_1 = try toybox.add();
        const grandchild_1_2 = try toybox.add();
        const grandchild_2_1 = try toybox.add();
        const grandchild_2_2 = try toybox.add();

        toybox.addChildFirst(root.index, child_2.index);
        toybox.addChildFirst(root.index, child_1.index);

        toybox.addChildFirst(child_1.index, grandchild_1_2.index);
        toybox.addChildFirst(child_1.index, grandchild_1_1.index);

        toybox.addChildFirst(child_2.index, grandchild_2_2.index);
        toybox.addChildFirst(child_2.index, grandchild_2_1.index);

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

    pub fn buildSexpr(toybox: *Toybox, point: Point, value: union(Sexpr.Kind) {
        empty,
        atom_lit: []const u8,
        atom_var: []const u8,
        pair: struct { up: Lego.Index, down: Lego.Index },
    }, is_pattern: bool) !Lego.Index {
        const result = try toybox.add();
        result.point = point;
        result.sexpr = .{
            .is_pattern = is_pattern,
            .is_pattern_t = if (is_pattern) 1 else 0,
            .atom_name = switch (value) {
                .atom_lit, .atom_var => |v| v,
                else => undefined,
            },
            .kind = value,
        };
        switch (value) {
            else => {},
            .pair => |pair| {
                toybox.addChildFirst(result.index, pair.down);
                toybox.addChildFirst(result.index, pair.up);
            },
        }
        return result.index;
    }

    pub fn buildCase(toybox: *Toybox, point: Point, data: struct {
        pattern: Lego.Index,
        template: Lego.Index,
        fnkname: Lego.Index,
    }) !Lego.Index {
        const result = try toybox.add();
        result.point = point;
        result.case = true;
        result.handle_kind = .existing_case;
        toybox.addChildFirst(result.index, data.fnkname);
        toybox.addChildFirst(result.index, data.template);
        toybox.addChildFirst(result.index, data.pattern);
        return result.index;
    }
};

const Workspace = struct {
    toybox: Toybox,

    main_area: Lego.Index,
    hand_layer: Lego.Index = .nothing,

    grabbing: Lego.Index = .nothing,

    camera: Rect = .fromCenterAndSize(.zero, Vec2.new(16, 9).scale(2.75)),
    undo_stack: std.ArrayList(UndoableCommand),

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

    pub fn init(dst: *Workspace, gpa: std.mem.Allocator) !void {
        dst.* = kommon.meta.initDefaultFields(Workspace);
        dst.undo_stack = .init(gpa);
        try dst.toybox.init(gpa);

        dst.main_area = (try dst.toybox.add()).index;

        if (true) {
            const sample_sexpr = try dst.toybox.add();
            sample_sexpr.sexpr = .{
                .atom_name = "true",
                .kind = .atom_lit,
                .is_pattern = false,
                .is_pattern_t = 0,
            };
            dst.toybox.addChildFirst(dst.main_area, sample_sexpr.index);
        }

        if (true) {
            dst.toybox.addChildFirst(dst.main_area, try dst.toybox.buildSexpr(
                .{ .pos = .new(0, 1) },
                .{ .atom_lit = "false" },
                false,
            ));
        }

        if (true) {
            dst.toybox.addChildFirst(dst.main_area, try dst.toybox.buildSexpr(
                .{ .pos = .new(3, 0) },
                .{ .pair = .{
                    .up = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, false),
                    .down = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                } },
                false,
            ));
        }

        if (true) {
            dst.toybox.addChildFirst(dst.main_area, try dst.toybox.buildCase(
                .{ .pos = .new(0, 4) },
                .{
                    .pattern = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "false" }, true),
                    .template = try dst.toybox.buildSexpr(.{}, .{ .atom_lit = "true" }, false),
                    .fnkname = try dst.toybox.buildSexpr(.{}, .empty, false),
                },
            ));
        }
    }

    pub fn deinit(workspace: *Workspace) void {
        workspace.toybox.deinit();
        workspace.undo_stack.deinit();
    }

    const HotAndDropzone = struct { hot: Lego.Index = .nothing, dropzone: Lego.Index = .nothing };
    fn findHotAndDropzone(workspace: *Workspace, needle_pos: Vec2) HotAndDropzone {
        const toybox = &workspace.toybox;
        const root = workspace.main_area;
        const grabbing = workspace.grabbing;

        var cur: Lego.Index = root;
        while (cur != .nothing) : (cur = toybox.next_preordered(cur, root).next) {
            const lego = toybox.get(cur);
            if (lego.sexpr) |sexpr| {
                if (Sexpr.contains(lego.point, sexpr.is_pattern, sexpr.kind, needle_pos)) {
                    if (grabbing == .nothing) {
                        return .{ .hot = cur };
                    } else if (toybox.get(grabbing).sexpr != null) {
                        return .{ .dropzone = cur };
                    }
                }
            }
            if (lego.handle()) |handle| {
                if (grabbing == .nothing and handle.overlapped(needle_pos)) {
                    return .{ .hot = cur };
                }
            }
        }

        return .{};
    }

    fn updateSprings(workspace: *Workspace, mouse_pos: Vec2, dropzone: Lego.Index, delta_seconds: f32) void {
        const toybox = &workspace.toybox;

        const roots: [2]Lego.Index = .{ workspace.main_area, workspace.hand_layer };
        for (&roots) |root| {
            var cur: Lego.Index = root;
            while (cur != .nothing) : (cur = toybox.next_preordered(cur, root).next) {
                const lego = toybox.get(cur);
                if (cur == workspace.grabbing) {
                    const target: Point = if (dropzone == .nothing) .{ .pos = mouse_pos } else toybox.get(dropzone).point;
                    lego.point.lerp_towards(target, 0.6, delta_seconds);
                }
                if (lego.sexpr) |sexpr| {
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
                        toybox.get(child_up).point = lego.point
                            .applyToLocalPoint(lego.visual_offset)
                            .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .up));
                        toybox.get(child_down).point = lego.point
                            .applyToLocalPoint(lego.visual_offset)
                            .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .down));
                    }
                }
                if (lego.case) {
                    const offsets: [3]Point = .{
                        .{ .pos = .xneg },
                        .{ .pos = .xpos },
                        .{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) },
                    };
                    // const pattern, const template, const fnkname = toybox.getChildrenExact(3, cur);
                    // for (.{ pattern, template, fnkname }, offsets) |i, offset| {
                    for (toybox.getChildrenExact(3, cur), offsets) |i, offset| {
                        toybox.get(i).point = lego.point.applyToLocalPoint(offset);
                    }
                }
            }
        }
    }

    fn draw(workspace: *Workspace, platform: PlatformGives, drawer: *Drawer) !void {
        const asdf = tracy.initZone(@src(), .{ .name = "draw" });
        defer asdf.deinit();

        const camera = workspace.camera;
        const toybox = &workspace.toybox;

        const roots: [2]Lego.Index = .{ workspace.main_area, workspace.hand_layer };
        for (&roots) |root| {
            var cur: Lego.Index = root;
            while (cur != .nothing) : (cur = toybox.next_preordered(cur, root).next) {
                const lego = toybox.get(cur);
                const point = lego.point.applyToLocalPoint(lego.visual_offset);
                if (lego.sexpr) |sexpr| {
                    switch (sexpr.kind) {
                        .empty => {},
                        .atom_lit => try drawer.drawAtom(camera, point, sexpr.is_pattern, sexpr.atom_name, 1),
                        .pair => try drawer.drawPairHolder(camera, point, sexpr.is_pattern, 1),
                        .atom_var => @panic("TODO"),
                    }
                }
                if (lego.handle()) |handle| try handle.draw(drawer, camera);
            }
        }

        if (display_fps) try drawer.canvas.drawText(
            0,
            camera,
            try std.fmt.allocPrint(drawer.canvas.frame_arena.allocator(), "fps: {d:.5}", .{1.0 / platform.delta_seconds}),
            .{
                .pos = workspace.camera.top_left,
                .hor = .left,
                .ver = .ascender,
            },
            workspace.camera.size.y * 0.05,
            .black,
        );
    }

    pub fn update(workspace: *Workspace, platform: PlatformGives, drawer: ?*Drawer) !void {
        const camera = workspace.camera.withAspectRatio(platform.aspect_ratio, .grow, .center);

        if (platform.keyboard.wasPressed(.KeyQ)) {
            std.log.debug("-----", .{});
            for (workspace.toybox.all_legos.items, 0..) |lego, k| {
                assert(lego.index == @as(Lego.Index, @enumFromInt(k)));
                if (lego.exists) {
                    std.log.debug("{d} \tparent: {d} \tnext: {d} \tprev: {d} \tfirst: {d}", .{
                        k,
                        lego.tree.parent.asI32(),
                        lego.tree.next.asI32(),
                        lego.tree.prev.asI32(),
                        lego.tree.first.asI32(),
                    });
                }
            }
        }

        if (platform.keyboard.wasPressed(.KeyZ)) {
            const toybox = &workspace.toybox;
            while (workspace.undo_stack.pop()) |command| {
                switch (command) {
                    .fence => break,
                    .destroy_floating => |index| {
                        toybox.destroyFloating(index);
                    },
                    .recreate_floating => |data| {
                        toybox.recreateFloating(data);
                    },
                    .insert => |insert| {
                        toybox.insert(insert.what, insert.where);
                    },
                    .set_data_except_tree => |data| {
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

        const mouse = platform.getMouse(camera);

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
        for (workspace.toybox.all_legos.items) |*lego| {
            math.lerp_towards(&lego.hot_t, if (lego.index == hot_and_dropzone.hot) 1 else 0, 0.6, platform.delta_seconds);
            comptime assert(!@hasField(Lego, "active_t"));
            comptime assert(!@hasField(Lego, "dropzone_t"));
            math.lerp_towards(&lego.dropping_t, if (lego.index == workspace.grabbing and hot_and_dropzone.dropzone != .nothing) 1 else 0, 0.6, platform.delta_seconds);
        }

        // includes dragging and snapping to dropzone, since that's just the spring between the mouse cursor and the grabbed thing
        workspace.updateSprings(mouse.cur.position, hot_and_dropzone.dropzone, platform.delta_seconds);

        if (drawer) |d| {
            try workspace.draw(platform, d);
        }

        const toybox = &workspace.toybox;
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

            var grabbed_element_index: Lego.Index = undefined;

            if (mouse.wasPressed(.right)) {
                // Case A.0: duplicating
                const new_element_index = try toybox.dupeIntoFloating(hot_index, true);
                workspace.undo_stack.appendAssumeCapacity(.{
                    .destroy_floating = new_element_index,
                });
                grabbed_element_index = new_element_index;
            } else if (hot_parent != .nothing and toybox.get(hot_parent).sexpr != null) {
                // Case A.1: plucking a nested sexpr
                const new_empty_sexpr = try toybox.dupeIntoFloating(hot_index, false);
                toybox.get(new_empty_sexpr).sexpr.?.kind = .empty;
                workspace.undo_stack.appendAssumeCapacity(.{ .destroy_floating = new_empty_sexpr });

                toybox.changeChild(hot_index, new_empty_sexpr);
                workspace.undo_stack.appendAssumeCapacity(.{ .change_child = .{
                    .new = hot_index,
                    .original = new_empty_sexpr,
                } });

                grabbed_element_index = hot_index;
            } else {
                // Case A.2: plucking a top-level thing
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
            }

            assert(workspace.grabbing == .nothing and workspace.hand_layer == .nothing);
            workspace.grabbing = grabbed_element_index;
            workspace.undo_stack.appendAssumeCapacity(.{
                .set_grabbing = .nothing,
            });
            workspace.hand_layer = grabbed_element_index;
            workspace.undo_stack.appendAssumeCapacity(.{
                .set_handlayer = .nothing,
            });
        } else if (workspace.grabbing != .nothing and
            !(mouse.cur.isDown(.left) or mouse.cur.isDown(.right)))
        {
            const dropzone_index = hot_and_dropzone.dropzone;

            if (dropzone_index == .nothing) {
                toybox.addChildFirst(workspace.main_area, workspace.grabbing);
                workspace.undo_stack.appendAssumeCapacity(.{
                    .pop = workspace.grabbing,
                });
            } else {
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

            workspace.undo_stack.appendAssumeCapacity(.{ .set_grabbing = workspace.grabbing });
            workspace.undo_stack.appendAssumeCapacity(.{ .set_handlayer = workspace.grabbing });
            workspace.grabbing = .nothing;
            workspace.hand_layer = .nothing;
        }

        workspace.camera = moveCamera(camera, platform.delta_seconds, platform.keyboard, mouse, true);
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
    try dst.workspace.init(gpa);
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
    try self.workspace.update(platform, &self.drawer);

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
