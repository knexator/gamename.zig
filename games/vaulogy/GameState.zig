pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

const core = @import("core.zig");
const Drawer = @import("Drawer.zig");

comptime {
    std.testing.refAllDecls(@import("execution_tree.zig"));
}

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
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            var mem: core.VeryPermamentGameStuff = .init(std.testing.allocator);
            defer mem.deinit();
            var workspace: Workspace = undefined;
            try workspace.init(&mem);
            defer workspace.deinit(std.testing.allocator);
            var frame_arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
            defer frame_arena.deinit();

            var test_platform: TestPlatform = .{};

            var it = std.mem.window(u8, input, @sizeOf(FakeInput), @sizeOf(FakeInput));
            while (it.next()) |cur_input_raw| {
                if (cur_input_raw.len == @sizeOf(FakeInput)) {
                    const cur_input = std.mem.bytesToValue(FakeInput, cur_input_raw);
                    test_platform.keyboard.cur.keys.KeyZ = cur_input.z_down;
                    test_platform.mouse.cur.buttons.left = cur_input.mouse_left_down;
                    test_platform.mouse.cur.position = cur_input.mouse_pos;
                    try workspace.update(test_platform.getGives(1.0 / 60.0), null, .fromCenterAndSize(.zero, .new(16, 9)), &mem, frame_arena.allocator());
                    _ = frame_arena.reset(.retain_capacity);
                }
            }
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}

test "No leaks on Workspace and Drawer" {
    var mem: core.VeryPermamentGameStuff = .init(std.testing.allocator);
    defer mem.deinit();
    var workspace: Workspace = undefined;
    try workspace.init(&mem);
    defer workspace.deinit(std.testing.allocator);
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
camera: Rect = .fromCenterAndSize(.new(100, 0), Vec2.new(16, 9).scale(2)),

core_mem: core.VeryPermamentGameStuff,
scoring_run: core.ScoringRun,
// execution_thread: core.ExecutionThread,
// anim_t: f32 = 0.99,
// result: ?core.ExecutionThread.Result = null,

tree: ExecutionTree,
snapshots: []const core.ExecutionThread,
progress_t: f32 = 0.0,

workspace: Workspace,

const HoveredSexpr = struct {
    next: ?struct {
        left: *HoveredSexpr,
        right: *HoveredSexpr,
    },
    value: f32,

    const Pool = std.heap.MemoryPool(HoveredSexpr);

    pub fn fromSexpr(pool: *Pool, value: *const Sexpr) !*HoveredSexpr {
        return store(pool, .{ .value = 0, .next = switch (value.*) {
            .atom_lit, .atom_var, .empty => null,
            .pair => |p| .{
                .left = try fromSexpr(pool, p.left),
                .right = try fromSexpr(pool, p.right),
            },
        } });
    }

    pub fn clone(original: HoveredSexpr, pool: *Pool) !*HoveredSexpr {
        return try store(pool, .{ .value = original.value, .next = if (original.next) |next| .{
            .left = try clone(next.left.*, pool),
            .right = try clone(next.right.*, pool),
        } else null });
    }

    pub fn getAt(self: *HoveredSexpr, address: core.SexprAddress) *HoveredSexpr {
        var current: *HoveredSexpr = self;
        for (address) |dir| {
            current = switch (dir) {
                .left => current.next.?.left,
                .right => current.next.?.right,
            };
        }
        return current;
    }

    pub fn update(self: *HoveredSexpr, address: ?core.SexprAddress, target_value_if_matches: f32, delta_seconds: f32) void {
        if (address) |a| {
            if (a.len == 0) {
                self._update(target_value_if_matches, delta_seconds);
                if (self.next) |next| {
                    next.left.update(null, target_value_if_matches, delta_seconds);
                    next.right.update(null, target_value_if_matches, delta_seconds);
                }
            } else {
                self._update(0, delta_seconds);
                if (self.next) |next| {
                    next.left.update(if (a[0] == .left) a[1..] else null, target_value_if_matches, delta_seconds);
                    next.right.update(if (a[0] == .right) a[1..] else null, target_value_if_matches, delta_seconds);
                }
            }
        } else {
            self._update(0, delta_seconds);
            if (self.next) |next| {
                next.left.update(null, target_value_if_matches, delta_seconds);
                next.right.update(null, target_value_if_matches, delta_seconds);
            }
        }
    }

    fn _update(self: *HoveredSexpr, goal: f32, delta_seconds: f32) void {
        math.lerp_towards(&self.value, goal, 0.6, delta_seconds);
    }

    pub fn setAt(this: *const HoveredSexpr, pool: *Pool, address: core.SexprAddress, value: *HoveredSexpr) !*HoveredSexpr {
        if (address.len == 0) {
            return value;
        } else {
            const first = address[0];
            const rest = address[1..];
            if (this.next) |next| {
                switch (first) {
                    .left => {
                        const new_left = try next.left.setAt(pool, rest, value);
                        return try store(pool, .{ .value = this.value, .next = .{ .left = new_left, .right = next.right } });
                    },
                    .right => {
                        const new_right = try next.right.setAt(pool, rest, value);
                        return try store(pool, .{ .value = this.value, .next = .{ .left = next.left, .right = new_right } });
                    },
                }
            } else panic("bad address", .{});
        }
    }

    fn store(pool: *Pool, value: HoveredSexpr) !*HoveredSexpr {
        const ptr = try pool.create();
        ptr.* = value;
        return ptr;
    }
};

const Handle = struct {
    pos: Vec2,
    hot_t: f32 = 0,
    pub const radius = 0.2;

    pub fn draw(handle: Handle, drawer: *Drawer, camera: Rect, alpha: f32) !void {
        try handle.drawCustom(drawer, camera, alpha, 1, 0.2);
    }

    pub fn drawCustom(handle: Handle, drawer: *Drawer, camera: Rect, alpha: f32, p1: f32, p2: f32) !void {
        drawer.canvas.fillCircle(camera, handle.pos, p1 * radius * (1 + handle.hot_t * p2), COLORS.bg.withAlpha(alpha));
        drawer.canvas.strokeCircle(128, camera, handle.pos, p1 * radius * (1 + handle.hot_t * p2), 0.05, .blackAlpha(alpha));
    }

    pub fn update(handle: *Handle, hot_target: f32, delta_seconds: f32) void {
        math.lerp_towards(&handle.hot_t, hot_target, 0.6, delta_seconds);
    }
};

const VeryPhysicalGarland = struct {
    // TODO: doubly linked list?
    cases: std.ArrayListUnmanaged(VeryPhysicalCase),
    handles_for_new_cases_first: HandleForNewCase,
    handles_for_new_cases_rest: std.ArrayListUnmanaged(HandleForNewCase),

    handle: Handle,
    pub const handle_radius: f32 = 0.2;
    pub const handle_drop_radius: f32 = 1.5;
    pub const case_drop_preview_perc: f32 = 0.5;
    pub const dist_between_cases_first: f32 = 1.5;
    pub const dist_between_cases_rest: f32 = 2.5;

    const HandleForNewCase = Handle;

    pub fn init(pos: Vec2) VeryPhysicalGarland {
        return .{
            .handle = .{ .pos = pos },
            .cases = .empty,
            .handles_for_new_cases_rest = .empty,
            .handles_for_new_cases_first = .{ .pos = pos },
        };
    }

    pub fn deinit(garland: *VeryPhysicalGarland, allocator: std.mem.Allocator) void {
        garland.handles_for_new_cases_rest.deinit(allocator);
        for (garland.cases.items) |*c| {
            c.next.deinit(allocator);
        }
        garland.cases.deinit(allocator);
    }

    pub fn hash(garland: *const VeryPhysicalGarland) u32 {
        var hasher = std.hash.Wyhash.init(0);
        for (garland.cases.items) |case| {
            std.hash.autoHash(&hasher, case.hash());
        }
        return @truncate(hasher.final());
    }

    pub fn fromDefinition(base: Point, definition: core.FnkBody, mem: *core.VeryPermamentGameStuff, hover_pool: *HoveredSexpr.Pool) !VeryPhysicalGarland {
        var result: VeryPhysicalGarland = .init(base.pos);
        try result.cases.ensureUnusedCapacity(mem.gpa, definition.cases.items.len);
        try result.handles_for_new_cases_rest.ensureUnusedCapacity(mem.gpa, definition.cases.items.len);
        for (definition.cases.items, 0..) |case, k| {
            try result.insertCase(mem.gpa, k, try .fromValues(hover_pool, .{
                .pattern = case.pattern,
                .fnk_name = case.fnk_name,
                .template = case.template,
                .next = if (case.next) |n|
                    try .fromDefinition(base.applyToLocalPoint(.{
                        .pos = .new(1, tof32(k)),
                    }), .{ .cases = n }, mem, hover_pool)
                else
                    .init(base.applyToLocalPoint(.{ .pos = .new(1, tof32(k)) }).pos),
            }, base.applyToLocalPoint(.{ .pos = .new(0, tof32(k)) })));
        }
        return result;
    }

    pub fn fillVariables(garland: *VeryPhysicalGarland, bindings: []const core.Binding, mem: *core.VeryPermamentGameStuff) error{
        OutOfMemory,
        BAD_INPUT,
        UsedUndefinedVariable,
    }!void {
        for (garland.cases.items) |*c| try c.fillVariables(bindings, mem);
    }

    pub fn getBoardPos(garland: VeryPhysicalGarland) Vec2 {
        return garland.handle;
    }

    pub fn clone(original: VeryPhysicalGarland, res: std.mem.Allocator, hover_pool: *HoveredSexpr.Pool) !VeryPhysicalGarland {
        var new_cases: @FieldType(VeryPhysicalGarland, "cases") = try .initCapacity(res, original.cases.items.len);
        for (original.cases.items) |c| {
            new_cases.appendAssumeCapacity(try c.clone(res, hover_pool));
        }
        assert(new_cases.items.len == original.handles_for_new_cases_rest.items.len);
        return .{
            .handle = original.handle,
            .handles_for_new_cases_first = original.handles_for_new_cases_first,
            .handles_for_new_cases_rest = try original.handles_for_new_cases_rest.clone(res),
            .cases = new_cases,
        };
    }

    pub fn draw(garland: VeryPhysicalGarland, drawer: *Drawer, camera: Rect) !void {
        try garland.drawWithBindings(null, drawer, camera);
    }

    pub fn drawWithAlpha(garland: VeryPhysicalGarland, alpha: f32, drawer: *Drawer, camera: Rect) !void {
        try garland.drawWithBindingsAndAlpha(null, alpha, drawer, camera);
    }

    pub fn drawWithBindings(garland: VeryPhysicalGarland, bindings: ?BindingsState, drawer: *Drawer, camera: Rect) !void {
        try garland.drawWithBindingsAndAlpha(bindings, 1, drawer, camera);
    }

    pub fn drawWithBindingsAndAlpha(
        garland: VeryPhysicalGarland,
        bindings: ?BindingsState,
        alpha: f32,
        drawer: *Drawer,
        camera: Rect,
    ) error{
        InvalidUtf8,
        OutOfMemory,
        BadVertexOrder,
    }!void {
        assert(math.in01(alpha));
        // TODO: Handle.draw
        var last_pos = garland.handle.pos;
        for (0..garland.cases.items.len + 1) |k| {
            const h = garland.handleForNewCases(&.{k});
            const cur_pos = h.pos;
            drawer.canvas.line(camera, &.{ last_pos, cur_pos }, 0.05, .blackAlpha(alpha));
            last_pos = cur_pos;
        }
        try garland.handle.draw(drawer, camera, alpha);
        drawer.canvas.strokeCircle(128, camera, garland.handle.pos, handle_radius * (1 + garland.handle.hot_t * 0.2), 0.05, .blackAlpha(alpha));
        for (0..garland.cases.items.len + 1) |k| {
            const h = garland.handleForNewCases(&.{k});
            try h.drawCustom(drawer, camera, alpha, 0.5, 3.0);
        }
        // TODO: cable
        for (garland.cases.items) |c| try c.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
    }

    pub fn handleForNewCases(garland: *const VeryPhysicalGarland, address: core.CaseAddress) *const HandleForNewCase {
        assert(address.len > 0);
        if (address.len == 1) {
            const k = address[0];
            assert(k <= garland.cases.items.len);
            return if (k == 0)
                &garland.handles_for_new_cases_first
            else
                &garland.handles_for_new_cases_rest.items[k - 1];
        } else {
            return garland.constChildCase(address[0 .. address.len - 1]).next.handleForNewCases(address[address.len - 1 ..]);
        }
    }

    pub fn handleForNewCasesRef(garland: *VeryPhysicalGarland, k: usize) *HandleForNewCase {
        assert(k <= garland.cases.items.len);
        return if (k == 0)
            &garland.handles_for_new_cases_first
        else
            &garland.handles_for_new_cases_rest.items[k - 1];
    }

    pub fn update(garland: *VeryPhysicalGarland, delta_seconds: f32) void {
        garland.updateWithOffset(0, delta_seconds);
    }

    // TODO: maybe remove delta_seconds
    pub fn kinematicUpdate(garland: *VeryPhysicalGarland, center: Point, delta_seconds: f32) void {
        garland.handle.pos = center.pos;
        for (garland.cases.items, 0..) |*c, k| {
            const target = center.applyToLocalPoint(.{ .pos = .new(0, dist_between_cases_first + dist_between_cases_rest * tof32(k)) });
            c.kinematicUpdate(target, null, null, delta_seconds);
        }
        for (0..garland.cases.items.len + 1) |k| {
            const h = garland.handleForNewCasesRef(k);
            h.pos = if (k == 0)
                garland.handle.pos.addY(dist_between_cases_first / 2.0)
            else
                garland.handle.pos.addY(dist_between_cases_first + dist_between_cases_rest * (tof32(k) - 0.5));
        }
    }

    pub fn updateWithOffset(garland: *VeryPhysicalGarland, offset: f32, delta_seconds: f32) void {
        assert(math.in01(offset));
        assert(garland.handles_for_new_cases_rest.items.len == garland.cases.items.len);
        for (0..garland.cases.items.len * 2 + 1) |k| {
            const dist = (if (k <= 1) dist_between_cases_first / 2.0 else dist_between_cases_rest / 2.0) +
                (if (k == 1) offset * dist_between_cases_rest else 0);
            if (k % 2 == 0) {
                const handle = garland.handleForNewCasesRef(@divExact(k, 2));
                const prev_pos = if (k == 0) garland.handle.pos else garland.cases.items[@divExact(k - 2, 2)].handle.pos;
                const target = prev_pos.addY(dist + handle.hot_t * dist * case_drop_preview_perc);
                Vec2.lerpTowards(&handle.pos, target, 0.6, delta_seconds);
            } else {
                const case = &garland.cases.items[@divExact(k - 1, 2)];
                const prev_handle = garland.handleForNewCases(&.{@divExact(k - 1, 2)});
                const target = prev_handle.pos.addY(dist + prev_handle.hot_t * dist * case_drop_preview_perc);
                Vec2.lerpTowards(&case.handle.pos, target, 0.6, delta_seconds);
                case.update(delta_seconds);
            }
        }
    }

    pub fn constChildCase(parent: *const VeryPhysicalGarland, local: core.CaseAddress) *const VeryPhysicalCase {
        assert(local.len >= 1);
        return if (local.len == 1)
            &parent.cases.items[local[0]]
        else
            parent.cases.items[local[0]].next.constChildCase(local[1..]);
    }

    pub fn childCase(parent: *VeryPhysicalGarland, local: core.CaseAddress) *VeryPhysicalCase {
        assert(local.len >= 1);
        return if (local.len == 1)
            &parent.cases.items[local[0]]
        else
            parent.cases.items[local[0]].next.childCase(local[1..]);
    }

    pub fn constChildGarland(parent: *const VeryPhysicalGarland, local: core.CaseAddress) *const VeryPhysicalGarland {
        return if (local.len == 0)
            parent
        else
            &parent.constChildCase(local).next;
    }

    pub fn childGarland(parent: *VeryPhysicalGarland, local: core.CaseAddress) *VeryPhysicalGarland {
        return if (local.len == 0)
            parent
        else
            &parent.childCase(local).next;
    }

    pub fn popCase(parent: *VeryPhysicalGarland, index: usize) VeryPhysicalCase {
        _ = parent.handles_for_new_cases_rest.orderedRemove(index);
        const case = parent.cases.orderedRemove(index);
        parent.handleForNewCasesRef(index).pos = case.handle.pos;
        return case;
    }

    pub fn popFirstCaseForExecution(parent: *VeryPhysicalGarland) VeryPhysicalCase {
        _ = parent.handles_for_new_cases_rest.orderedRemove(0);
        const case = parent.cases.orderedRemove(0);
        return case;
    }

    pub fn insertCase(parent: *VeryPhysicalGarland, mem: std.mem.Allocator, index: usize, case: VeryPhysicalCase) !void {
        try parent.handles_for_new_cases_rest.insert(mem, index, .{ .pos = parent.handleForNewCases(&.{index}).pos });
        try parent.cases.insert(mem, index, case);
    }

    pub fn childGarlandsAddressIterator(garland: *const VeryPhysicalGarland, mem: std.mem.Allocator) AddressIterator {
        return .{ .mem = mem, .garland = garland, .start_with_empty = true };
    }

    pub fn addressIterator(garland: *const VeryPhysicalGarland, mem: std.mem.Allocator) AddressIterator {
        return .{ .mem = mem, .garland = garland };
    }

    // TODO: memory usage could be improved
    pub const AddressIterator = struct {
        mem: std.mem.Allocator,
        child: ?*AddressIterator = null,
        k: usize = 0,
        garland: *const VeryPhysicalGarland,
        start_with_empty: bool = false,

        pub fn next(it: *AddressIterator) !?core.CaseAddress {
            if (it.start_with_empty) {
                it.start_with_empty = false;
                return &.{};
            }
            if (it.child == null) {
                if (it.k >= it.garland.cases.items.len) return null;
                it.child = try it.mem.create(AddressIterator);
                it.child.?.* = .{ .garland = &it.garland.cases.items[it.k].next, .mem = it.mem };
                return try it.mem.dupe(usize, &.{it.k});
            } else {
                const maybe_rest = try it.child.?.next();
                if (maybe_rest) |rest| {
                    const result = try it.mem.alloc(usize, rest.len + 1);
                    @memcpy(result[1..], rest);
                    it.mem.free(rest);
                    result[0] = it.k;
                    return result;
                } else {
                    it.mem.destroy(it.child.?);
                    it.child = null;
                    it.k += 1;
                    return it.next();
                }
            }
        }
    };

    pub fn newHandlesAddressIterator(garland: *const VeryPhysicalGarland, mem: std.mem.Allocator) NewCasesHandlesAddressIterator {
        return .{ .mem = mem, .garland = garland };
    }

    // TODO: memory usage could be improved
    pub const NewCasesHandlesAddressIterator = struct {
        mem: std.mem.Allocator,
        child: ?*NewCasesHandlesAddressIterator = null,
        k: usize = 0,
        garland: *const VeryPhysicalGarland,

        pub fn next(it: *NewCasesHandlesAddressIterator) !?core.CaseAddress {
            if (it.child == null) {
                if (it.k == it.garland.cases.items.len) {
                    it.k += 1;
                    return try it.mem.dupe(usize, &.{it.k - 1});
                }
                if (it.k > it.garland.cases.items.len) return null;
                it.child = try it.mem.create(NewCasesHandlesAddressIterator);
                it.child.?.* = .{ .garland = &it.garland.cases.items[it.k].next, .mem = it.mem };
                return try it.mem.dupe(usize, &.{it.k});
            } else {
                const maybe_rest = try it.child.?.next();
                if (maybe_rest) |rest| {
                    const result = try it.mem.alloc(usize, rest.len + 1);
                    @memcpy(result[1..], rest);
                    it.mem.free(rest);
                    result[0] = it.k;
                    return result;
                } else {
                    it.mem.destroy(it.child.?);
                    it.child = null;
                    it.k += 1;
                    return it.next();
                }
            }
        }
    };
};

const VeryPhysicalCase = struct {
    pattern: VeryPhysicalSexpr,
    fnk_name: VeryPhysicalSexpr,
    template: VeryPhysicalSexpr,
    next: VeryPhysicalGarland,
    handle: Handle,
    pub const handle_radius: f32 = 0.2;

    const fnk_name_offset: Point = .{ .scale = 0.5, .turns = 0.25, .pos = .new(4, -1) };
    const next_garland_offset: Vec2 = .new(8, -1.5);

    pub fn getBoardPos(case: VeryPhysicalCase) Vec2 {
        return case.handle;
    }

    pub fn fromValues(
        pool: *HoveredSexpr.Pool,
        values: struct {
            pattern: *const Sexpr,
            fnk_name: *const Sexpr,
            template: *const Sexpr,
            next: ?VeryPhysicalGarland = null,
        },
        center: Point,
    ) !VeryPhysicalCase {
        return .{
            .handle = .{ .pos = center.pos },
            .pattern = try .fromSexpr(pool, values.pattern, center.applyToLocalPoint(.{ .pos = .xneg }), true),
            .template = try .fromSexpr(pool, values.template, center.applyToLocalPoint(.{ .pos = .xpos }), false),
            .fnk_name = try .fromSexpr(pool, values.fnk_name, center.applyToLocalPoint(fnk_name_offset), false),
            .next = values.next orelse .init(center.applyToLocalPosition(.new(6, 0))),
        };
    }

    pub fn hash(case: *const VeryPhysicalCase) u32 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, case.pattern.value.hash());
        std.hash.autoHash(&hasher, case.template.value.hash());
        std.hash.autoHash(&hasher, case.fnk_name.value.hash());
        std.hash.autoHash(&hasher, case.next.hash());
        return @truncate(hasher.final());
    }

    pub fn clone(case: *const VeryPhysicalCase, res: std.mem.Allocator, hover_pool: *HoveredSexpr.Pool) error{OutOfMemory}!VeryPhysicalCase {
        assert(kommon.meta.isPlainOldData(Handle));
        return .{
            .pattern = try case.pattern.clone(hover_pool),
            .fnk_name = try case.fnk_name.clone(hover_pool),
            .template = try case.template.clone(hover_pool),
            .next = try case.next.clone(res, hover_pool),
            .handle = case.handle,
        };
    }

    pub fn fillVariables(case: *VeryPhysicalCase, bindings: []const core.Binding, mem: *core.VeryPermamentGameStuff) !void {
        try case.pattern.fillVariables(bindings, mem);
        try case.template.fillVariables(bindings, mem);
        try case.next.fillVariables(bindings, mem);
    }

    pub fn drawWithBindingsAndAlpha(
        case: VeryPhysicalCase,
        bindings: ?BindingsState,
        alpha: f32,
        drawer: *Drawer,
        camera: Rect,
    ) !void {
        // TODO: draw variables in the cable
        drawer.canvas.line(camera, &.{
            case.pattern.point.pos,
            case.template.point.pos,
        }, 0.05, .blackAlpha(alpha));
        try case.handle.draw(drawer, camera, alpha);
        try case.pattern.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        try case.template.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        try case.fnk_name.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        try case.next.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
    }

    pub fn drawWithBindings(case: VeryPhysicalCase, bindings: ?BindingsState, drawer: *Drawer, camera: Rect) !void {
        try case.drawWithBindingsAndAlpha(bindings, 1, drawer, camera);
    }

    pub fn draw(case: VeryPhysicalCase, drawer: *Drawer, camera: Rect) !void {
        try case.drawWithBindings(null, drawer, camera);
    }

    pub fn update(case: *VeryPhysicalCase, delta_seconds: f32) void {
        const center: Point = .{ .pos = case.handle.pos };
        case.pattern.point.lerp_towards(center.applyToLocalPoint(.{ .pos = .xneg }), 0.6, delta_seconds);
        case.template.point.lerp_towards(center.applyToLocalPoint(.{ .pos = .xpos }), 0.6, delta_seconds);
        case.fnk_name.point.lerp_towards(center.applyToLocalPoint(fnk_name_offset), 0.6, delta_seconds);
        Vec2.lerpTowards(&case.next.handle.pos, center.applyToLocalPosition(next_garland_offset), 0.6, delta_seconds);
        case.next.update(delta_seconds);
    }

    pub fn kinematicUpdate(case: *VeryPhysicalCase, center: Point, next_point_extra: ?Point, fnk_name_extra: ?Point, delta_seconds: f32) void {
        case.handle.pos = center.pos;
        case.pattern.point = center.applyToLocalPoint(.{ .pos = .xneg });
        case.template.point = center.applyToLocalPoint(.{ .pos = .xpos });
        case.fnk_name.point = center.applyToLocalPoint(fnk_name_offset).applyToLocalPoint(fnk_name_extra orelse .{});
        case.next.kinematicUpdate(center.applyToLocalPoint(.{ .pos = next_garland_offset }).applyToLocalPoint(next_point_extra orelse .{}), delta_seconds);
    }

    pub fn sexprAt(case: *VeryPhysicalCase, part: core.CasePart) *VeryPhysicalSexpr {
        return switch (part) {
            .template => &case.template,
            .pattern => &case.pattern,
            .fnk_name => &case.fnk_name,
        };
    }
};

const VeryPhysicalSexpr = struct {
    hovered: *HoveredSexpr,
    point: Point,
    value: *const Sexpr,
    is_pattern: bool,
    is_pattern_t: f32,

    pub fn getBoardPos(sexpr: VeryPhysicalSexpr) Vec2 {
        return sexpr.point.pos;
    }

    pub fn fromSexpr(pool: *HoveredSexpr.Pool, value: *const Sexpr, point: Point, is_pattern: bool) !VeryPhysicalSexpr {
        return .{
            .hovered = try HoveredSexpr.fromSexpr(pool, value),
            .point = point,
            .value = value,
            .is_pattern = is_pattern,
            .is_pattern_t = if (is_pattern) 1.0 else 0.0,
        };
    }

    pub fn empty(point: Point, hover_pool: *HoveredSexpr.Pool, is_pattern: bool) !VeryPhysicalSexpr {
        return .fromSexpr(hover_pool, Sexpr.builtin.empty, point, is_pattern);
    }

    pub fn isEmpty(sexpr: VeryPhysicalSexpr) bool {
        return switch (sexpr.value.*) {
            .empty => true,
            else => false,
        };
    }

    pub fn fillVariables(sexpr: *VeryPhysicalSexpr, bindings: []const core.Binding, mem: *core.VeryPermamentGameStuff) !void {
        sexpr.value = (try core.partiallyFillTemplateV2(sexpr.value, bindings, &mem.pool_for_sexprs)).result;
    }

    fn updateIsPattern(sexpr: *VeryPhysicalSexpr, delta_seconds: f32) void {
        math.lerp_towards(&sexpr.is_pattern_t, if (sexpr.is_pattern) 1 else 0, 0.6, delta_seconds);
    }

    fn _draw(
        drawer: *Drawer,
        camera: Rect,
        value: *const Sexpr,
        hovered: *HoveredSexpr,
        point: Point,
        is_pattern: bool,
        is_pattern_t: f32,
        maybe_bindings: ?BindingsState,
        alpha: f32,
    ) !void {
        const actual_point = point.applyToLocalPoint(.lerp(.{}, .lerp(
            .{ .turns = -0.02, .pos = .new(0.5, 0) },
            .{ .turns = 0.02, .pos = .new(-0.5, 0) },
            is_pattern_t,
        ), hovered.value / 2.0));
        switch (value.*) {
            .empty => {},
            .atom_lit, .atom_var => try if (is_pattern)
                drawer.drawPatternSexpr(camera, value, actual_point, alpha)
            else
                drawer.drawTemplateSexpr(camera, value, actual_point, alpha),
            .pair => |pair| {
                try if (is_pattern)
                    drawer.drawPatternPairHolder(camera, actual_point, alpha)
                else
                    drawer.drawTemplatePairHolder(camera, actual_point, alpha);
                // try drawTemplateWildcardLinesNonRecursive(...);
                const offset = if (is_pattern) ViewHelper.OFFSET_PATTERN else ViewHelper.OFFSET_TEMPLATE;
                try _draw(drawer, camera, pair.left, hovered.next.?.left, actual_point.applyToLocalPoint(offset.LEFT), is_pattern, is_pattern_t, maybe_bindings, alpha);
                try _draw(drawer, camera, pair.right, hovered.next.?.right, actual_point.applyToLocalPoint(offset.RIGHT), is_pattern, is_pattern_t, maybe_bindings, alpha);
            },
        }
        if (maybe_bindings) |bindings| {
            switch (value.*) {
                // TODO: cables?
                else => {},
                .atom_var => |x| {
                    // TODO: check that compiler skips the loop if anim_t is null
                    for (bindings.new) |binding| {
                        if (bindings.anim_t) |anim_t| {
                            if (std.mem.eql(u8, binding.name, x.value)) {
                                if (is_pattern) {
                                    const t = math.smoothstep(anim_t, 0, 0.1);
                                    try drawer.drawEatingPattern(camera, actual_point, binding, t, alpha);
                                } else {
                                    try drawer.clipAtomRegion(camera, actual_point);
                                    const t = math.smoothstep(anim_t, 0, 0.4);
                                    try drawer.drawSexpr(camera, .{
                                        .is_pattern = is_pattern_t,
                                        .value = binding.value,
                                        .pos = actual_point.applyToLocalPoint(.{ .pos = .new(math.remap(t, 0, 1, -2.3, 0), 0) }),
                                    }, alpha);
                                    drawer.endClip();

                                    try drawer.drawSexpr(camera, .{
                                        .is_pattern = is_pattern_t,
                                        .value = value,
                                        .pos = actual_point,
                                    }, alpha * (1 - anim_t));

                                    // TODO: uncomment
                                    // if (anim_t < 0.5) {
                                    //     try out_particles.append(.{ .point = actual_point, .t = t, .name = binding.name });
                                    // }
                                }
                            }
                            break;
                        }
                    } else for (bindings.old) |binding| {
                        if (std.mem.eql(u8, binding.name, x.value)) {
                            if (is_pattern) {
                                const t = 1;
                                try drawer.drawEatingPattern(camera, actual_point, binding, t, alpha);
                            } else {
                                try drawer.drawSexpr(camera, .{
                                    .is_pattern = is_pattern_t,
                                    .value = binding.value,
                                    .pos = actual_point,
                                }, alpha);
                            }
                            break;
                        }
                    } else {
                        try drawer.drawSexpr(camera, .{
                            .is_pattern = is_pattern_t,
                            .value = value,
                            .pos = actual_point,
                        }, alpha);
                    }
                },
            }
        }
    }

    pub fn draw(sexpr: VeryPhysicalSexpr, drawer: *Drawer, camera: Rect) !void {
        try sexpr.drawWithBindings(null, drawer, camera);
    }

    pub fn drawWithBindings(sexpr: VeryPhysicalSexpr, bindings: ?BindingsState, drawer: *Drawer, camera: Rect) !void {
        try sexpr.drawWithBindingsAndAlpha(bindings, 1, drawer, camera);
    }

    pub fn drawWithBindingsAndAlpha(
        sexpr: VeryPhysicalSexpr,
        bindings: ?BindingsState,
        alpha: f32,
        drawer: *Drawer,
        camera: Rect,
    ) !void {
        assert(math.in01(sexpr.is_pattern_t));
        assert(math.in01(alpha));
        // TODO: use the actual bool?
        const base_point = if (!sexpr.is_pattern)
            sexpr.point.applyToLocalPoint(.{ .turns = math.remap(
                sexpr.is_pattern_t,
                0.5,
                0,
                -0.25,
                0,
            ) })
        else
            sexpr.point.applyToLocalPoint(.{ .turns = math.remap(
                sexpr.is_pattern_t,
                0.5,
                1,
                0.25,
                0,
            ) });

        if (sexpr.isEmpty()) {
            try drawer.drawPlaceholder(camera, base_point, sexpr.is_pattern, alpha);
        } else {
            return _draw(
                drawer,
                camera,
                sexpr.value,
                sexpr.hovered,
                base_point,
                sexpr.is_pattern,
                sexpr.is_pattern_t,
                bindings,
                alpha,
            );
        }
    }

    pub fn updateSubValue(
        self: *VeryPhysicalSexpr,
        address: core.SexprAddress,
        new_value: *const Sexpr,
        new_hovered: *HoveredSexpr,
        core_mem: *core.VeryPermamentGameStuff,
        hover_pool: *HoveredSexpr.Pool,
    ) !void {
        self.value = try self.value.setAt(core_mem, address, new_value);
        self.hovered = try self.hovered.setAt(hover_pool, address, new_hovered);
    }

    pub fn getSubValue(
        self: VeryPhysicalSexpr,
        address: core.SexprAddress,
    ) VeryPhysicalSexpr {
        return .{
            .hovered = self.hovered.getAt(address),
            .point = ViewHelper.sexprChildView(
                self.is_pattern,
                self.point,
                address,
            ),
            .value = self.value.getAt(address).?,
            .is_pattern = self.is_pattern,
            .is_pattern_t = self.is_pattern_t,
        };
    }

    pub fn clone(self: *const VeryPhysicalSexpr, hover_pool: *HoveredSexpr.Pool) !VeryPhysicalSexpr {
        return self.dupeSubValue(&.{}, hover_pool);
    }

    pub fn dupeSubValue(
        self: *const VeryPhysicalSexpr,
        address: core.SexprAddress,
        hover_pool: *HoveredSexpr.Pool,
    ) !VeryPhysicalSexpr {
        return .{
            .hovered = try self.hovered.getAt(address).clone(hover_pool),
            .point = ViewHelper.sexprChildView(
                self.is_pattern,
                self.point,
                address,
            ),
            .value = self.value.getAt(address).?,
            .is_pattern = self.is_pattern,
            .is_pattern_t = self.is_pattern_t,
        };
    }

    pub fn popSubValue(
        self: *VeryPhysicalSexpr,
        address: core.SexprAddress,
        hover_pool: *HoveredSexpr.Pool,
        mem: *VeryPermamentGameStuff,
    ) !VeryPhysicalSexpr {
        const result: VeryPhysicalSexpr = .{
            .hovered = self.hovered.getAt(address),
            .point = ViewHelper.sexprChildView(
                self.is_pattern,
                self.point,
                address,
            ),
            .value = self.value.getAt(address).?,
            .is_pattern = self.is_pattern,
            .is_pattern_t = self.is_pattern_t,
        };
        self.value = try self.value.setAt(mem, address, Sexpr.builtin.empty);
        self.hovered = try self.hovered.setAt(hover_pool, address, try HoveredSexpr.store(hover_pool, .{ .next = null, .value = 0 }));
        return result;
    }
};

const Pill = struct {
    input: VeryPhysicalSexpr,
    pattern: VeryPhysicalSexpr,

    pub fn draw(pill: Pill, bindings: ?BindingsState, drawer: *Drawer, camera: Rect) !void {
        try pill.input.drawWithBindings(bindings, drawer, camera);
        try pill.pattern.drawWithBindings(bindings, drawer, camera);
    }
};

// automatically consumes garland cases when input is present
const Executor = struct {
    input: VeryPhysicalSexpr,
    garland: VeryPhysicalGarland,
    handle: Handle,
    spawned_by_fnkbox: ?struct {
        fnkbox: usize,
        testcase: usize,
    },

    animation: ?struct {
        t: f32 = 0,
        active_case: VeryPhysicalCase,
        matching: bool,
        invoked_fnk: ?VeryPhysicalGarland,
        new_bindings: []const core.Binding,
        original_pos: Vec2,
    } = null,
    prev_pills: std.ArrayListUnmanaged(Pill) = .empty,
    enqueued_stack: std.ArrayListUnmanaged(VeryPhysicalGarland) = .empty,
    old_bindings: std.ArrayListUnmanaged(core.Binding) = .empty,

    const relative_input_point: Point = .{ .pos = .new(-1, 1.5) };
    const relative_garland_pos: Vec2 = .new(4, 0);

    pub const MOVING_LEFT = false;

    pub fn init(pos: Vec2, hover_pool: *HoveredSexpr.Pool) !Executor {
        return .{
            .handle = .{ .pos = pos },
            .garland = .init(pos.add(relative_input_point.pos)),
            .input = try .empty((Point{ .pos = pos }).applyToLocalPoint(relative_input_point), hover_pool, false),
            .spawned_by_fnkbox = null,
        };
    }

    pub fn draw(executor: Executor, drawer: *Drawer, camera: Rect) !void {
        try executor.handle.draw(drawer, camera, 1);
        const bindings: BindingsState = if (executor.animation) |anim| .{
            .anim_t = if (anim.t < 0.2) null else math.remapTo01Clamped(anim.t, 0.2, 0.8),
            .old = executor.old_bindings.items,
            .new = anim.new_bindings,
        } else .{
            .anim_t = null,
            .old = executor.old_bindings.items,
            .new = &.{},
        };
        try executor.input.drawWithBindings(bindings, drawer, camera);
        if (executor.animation) |anim| {
            try anim.active_case.drawWithBindings(bindings, drawer, camera);
            if (anim.invoked_fnk) |f| try f.draw(drawer, camera);
        }
        for (executor.prev_pills.items) |p| try p.draw(bindings, drawer, camera);
        for (executor.enqueued_stack.items) |s| try s.drawWithBindings(bindings, drawer, camera);
        try executor.garland.draw(drawer, camera);
    }

    pub fn animating(executor: Executor) bool {
        return executor.animation != null;
    }

    pub fn startedExecution(executor: Executor) bool {
        return executor.animation == null and executor.garland.cases.items.len > 0 and !executor.input.isEmpty();
    }

    // try core.fillTemplateV2(case.template.value, new_bindings.items, &mem.pool_for_sexprs)
    pub fn update(executor: *Executor, mem: *core.VeryPermamentGameStuff, known_fnks: []const Fnkbox, hover_pool: *HoveredSexpr.Pool, delta_seconds: f32) !void {
        Vec2.lerpTowards(&executor.garland.handle.pos, executor.handle.pos.add(Executor.relative_garland_pos), 0.6, delta_seconds);
        var pill_offset: f32 = 0;
        if (executor.animation) |*animation| {
            animation.t += delta_seconds;
            const anim_t = math.clamp01(animation.t);
            if (!animation.matching) {
                const match_t = math.remapClamped(anim_t, 0, 0.2, 0, 1);
                const flyaway_t = math.remapClamped(anim_t, 0.2, 0.8, 0, 1);
                const offset_t = math.remapClamped(anim_t, 0.2, 0.8, 1, 0);

                const case_floating_away = executor.firstCasePoint()
                    .applyToLocalPoint(Point.lerp(
                    .{ .pos = .new(-match_t, 0) },
                    .{ .pos = .new(6, -2), .scale = 0, .turns = -0.2 },
                    flyaway_t,
                ));
                executor.garland.updateWithOffset(offset_t, delta_seconds);
                animation.active_case.kinematicUpdate(case_floating_away, null, null, delta_seconds);
                executor.input.point.lerp_towards(executor.inputPoint(), 0.6, delta_seconds);
            } else {
                const match_t = math.remapClamped(anim_t, 0, 0.2, 0, 1);
                // const bindings_t: ?f32 = if (anim_t < 0.2) null else math.remapTo01Clamped(anim_t, 0.2, 0.8);
                const template_t = math.remapClamped(anim_t, 0.2, 1.0, 0, 1);
                const invoking_t = math.remapClamped(anim_t, 0.0, 0.7, 0, 1);
                const enqueueing_t = math.remapClamped(anim_t, 0.2, 1, 0, 1);
                const discarded_t = anim_t;
                pill_offset = enqueueing_t;

                if (!MOVING_LEFT) {
                    executor.handle.pos = animation.original_pos.addX(enqueueing_t * 5);
                }

                const case_point = executor.firstCasePoint().applyToLocalPoint(
                    .{ .pos = .new(-match_t - enqueueing_t * 5, 0) },
                );

                executor.garland.kinematicUpdate((Point{ .pos = executor.handle.pos.add(relative_garland_pos) })
                    .applyToLocalPoint(.{ .pos = .new(0, 2.5) })
                    .applyToLocalPoint(.lerp(.{}, .{ .turns = 0.2, .scale = 0, .pos = .new(-4, 8) }, discarded_t)), delta_seconds);
                if (animation.invoked_fnk) |*invoked| {
                    const offset = (1.0 - invoking_t) + 2.0 * math.smoothstepEased(invoking_t, 0.4, 0.0, .linear);
                    const function_point = executor.garlandPoint()
                        .applyToLocalPoint(.{ .pos = .new(2 * offset + 6 - match_t - enqueueing_t * 5, 6 * offset) });
                    invoked.kinematicUpdate(function_point, delta_seconds);

                    animation.active_case.kinematicUpdate(case_point, .{
                        .pos = .new(template_t * 6, -2 * enqueueing_t),
                        .turns = math.lerp(0, -0.1, math.smoothstepEased(enqueueing_t, 0, 1, .easeInOutCubic)),
                    }, .{ .pos = .new(-invoking_t * 4, 0) }, delta_seconds);
                } else {
                    animation.active_case.kinematicUpdate(case_point, .{
                        .pos = .new(-template_t * 2, 0),
                    }, .{ .pos = .new(-invoking_t * 3, 0) }, delta_seconds);
                    for (executor.enqueued_stack.items, 0..) |*x, k| {
                        if (k == 0) {
                            const tt = 1 - template_t;
                            const et = 1 - enqueueing_t;
                            x.kinematicUpdate(executor.garlandPoint().applyToLocalPoint(.{
                                .pos = .new(tt * 6 + 2 - template_t * 2, -2 * et),
                                .turns = math.lerp(0, -0.1, math.smoothstepEased(et, 0, 1, .easeInOutCubic)),
                            }), delta_seconds);
                        } else @panic("TODO");
                    }
                }
                executor.input.point = executor.inputPoint().applyToLocalPoint(.{ .pos = .new(-enqueueing_t * 5, 0) });
            }
            if (animation.t >= 1) {
                if (animation.matching) {
                    try executor.prev_pills.append(mem.gpa, .{ .pattern = animation.active_case.pattern, .input = executor.input });
                    pill_offset -= 1;
                    try animation.active_case.fillVariables(animation.new_bindings, mem);
                    executor.input = animation.active_case.template;

                    if (animation.invoked_fnk) |fnk| {
                        executor.garland = fnk;
                        if (animation.active_case.next.cases.items.len > 0) {
                            try executor.enqueued_stack.append(mem.gpa, animation.active_case.next);
                        }
                    } else if (animation.active_case.next.cases.items.len > 0) {
                        executor.garland = animation.active_case.next;
                    } else if (executor.enqueued_stack.pop()) |g| {
                        executor.garland = g;
                    } else {
                        executor.garland = .init(executor.garlandPoint().pos);
                    }
                } else {
                    assert(animation.new_bindings.len == 0);
                }
                try executor.old_bindings.appendSlice(mem.gpa, animation.new_bindings);
                executor.animation = null;
            }
        } else {
            executor.garland.update(delta_seconds);
            executor.input.point.lerp_towards(executor.inputPoint(), 0.6, delta_seconds);
        }

        for (executor.prev_pills.items, 0..) |*pill, k| {
            const pill_input_pos = executor.inputPoint().applyToLocalPoint(.{ .pos = .new(-5 * (tof32(executor.prev_pills.items.len - k) + pill_offset), 0) });
            pill.input.point.lerp_towards(pill_input_pos, 0.6, delta_seconds);
            pill.pattern.point.lerp_towards(pill_input_pos.applyToLocalPoint(.{ .pos = .new(3, 0) }), 0.6, delta_seconds);
        }

        if (executor.animation == null and executor.garland.cases.items.len > 0 and !executor.input.isEmpty()) {
            const case = executor.garland.popFirstCaseForExecution();
            var new_bindings: std.ArrayList(core.Binding) = .init(mem.gpa);
            const matching = try core.generateBindings(case.pattern.value, executor.input.value, &new_bindings);
            const invoked_fnk: ?VeryPhysicalGarland = if (!matching)
                null
            else if (case.fnk_name.value.equals(Sexpr.builtin.identity) or case.fnk_name.isEmpty())
                null
            else blk: {
                const offset = 3.0;
                const function_point = executor.garlandPoint().applyToLocalPoint(.{ .pos = .new(2 * offset + 6, 6 * offset) });
                if (try getGarlandForFnk(known_fnks, case.fnk_name.value, function_point, mem, hover_pool)) |garland| {
                    break :blk garland;
                } else @panic("TODO: handle this");
            };
            executor.animation = .{
                .active_case = case,
                .matching = matching,
                .invoked_fnk = invoked_fnk,
                .new_bindings = try new_bindings.toOwnedSlice(),
                .original_pos = executor.handle.pos,
            };
        }
    }

    pub fn inputPoint(executor: Executor) Point {
        const center: Point = .{ .pos = executor.handle.pos };
        return center.applyToLocalPoint(Executor.relative_input_point);
    }

    pub fn firstCasePoint(executor: Executor) Point {
        return executor.garlandPoint().applyToLocalPoint(.{ .pos = .new(0, 1.5) });
    }

    pub fn garlandPoint(executor: Executor) Point {
        return .{ .pos = executor.handle.pos.add(relative_garland_pos) };
    }
};

// TODO: invoking a fnk doesn't undo
const Fnkviewer = struct {
    handle: Handle,
    fnkname: VeryPhysicalSexpr,
    garland: VeryPhysicalGarland,

    last_fnkname_hash: u32 = 0,
    // last_garland_hash: u32 = 0,

    const relative_fnkname_point: Point = .{ .pos = .new(-1, 0), .scale = 0.5, .turns = 0.25 };
    const relative_garland_point: Point = .{ .pos = .new(1, 1.5) };

    pub fn point(fnkviewer: *const Fnkviewer) Point {
        return .{ .pos = fnkviewer.handle.pos };
    }

    pub fn init(base: Point, hover_pool: *HoveredSexpr.Pool) !Fnkviewer {
        return .{
            .handle = .{ .pos = base.pos },
            .garland = .init(base.applyToLocalPoint(relative_garland_point).pos),
            .fnkname = try .empty(base.applyToLocalPoint(relative_fnkname_point), hover_pool, false),
        };
    }

    pub fn draw(fnkviewer: Fnkviewer, drawer: *Drawer, camera: Rect) !void {
        try fnkviewer.handle.draw(drawer, camera, 1);
        try fnkviewer.fnkname.draw(drawer, camera);
        try fnkviewer.garland.draw(drawer, camera);
    }

    pub fn update(fnkviewer: *Fnkviewer, mem: *core.VeryPermamentGameStuff, known_fnks: []const Fnkbox, hover_pool: *HoveredSexpr.Pool, delta_seconds: f32) !void {
        Vec2.lerpTowards(&fnkviewer.garland.handle.pos, fnkviewer.point().applyToLocalPoint(relative_garland_point).pos, 0.6, delta_seconds);
        fnkviewer.garland.update(delta_seconds);
        fnkviewer.fnkname.point.lerp_towards(fnkviewer.point().applyToLocalPoint(relative_fnkname_point), 0.6, delta_seconds);

        // TODO: some way to modify fnks
        const cur_fnkname_hash = fnkviewer.fnkname.value.hash();
        if (cur_fnkname_hash != fnkviewer.last_fnkname_hash) {
            fnkviewer.last_fnkname_hash = cur_fnkname_hash;
            if (try getGarlandForFnk(known_fnks, fnkviewer.fnkname.value, fnkviewer.point().applyToLocalPoint(relative_garland_point), mem, hover_pool)) |garland| {
                fnkviewer.garland = garland;
            }
        }
    }
};

pub const TestCase = struct {
    input: VeryPhysicalSexpr,
    expected: VeryPhysicalSexpr,
    actual: VeryPhysicalSexpr,
    tested: bool = false,
    play_button: Button,

    const Part = enum { input, expected, actual };
    pub const parts: [3]TestCase.Part = .{ .input, .expected, .actual };

    pub fn partRef(testcase: *TestCase, part: Part) *VeryPhysicalSexpr {
        return switch (part) {
            .input => &testcase.input,
            .expected => &testcase.expected,
            .actual => &testcase.actual,
        };
    }

    pub fn partConst(testcase: *const TestCase, part: Part) *const VeryPhysicalSexpr {
        return switch (part) {
            .input => &testcase.input,
            .expected => &testcase.expected,
            .actual => &testcase.actual,
        };
    }

    pub fn draw(testcase: *const TestCase, drawer: *Drawer, camera: Rect) !void {
        try testcase.input.draw(drawer, camera);
        try testcase.expected.draw(drawer, camera);
        try testcase.actual.draw(drawer, camera);
        try testcase.play_button.draw(drawer, camera);
    }
};

const Button = struct {
    hot_t: f32 = 0,
    rect: Rect,
    kind: enum { unknown, launch_testcase } = .unknown,

    pub fn draw(button: *const Button, drawer: *Drawer, camera: Rect) !void {
        drawer.canvas.fillRect(camera, button.rect, COLORS.bg);
        drawer.canvas.borderRect(camera, button.rect, math.lerp(0.05, 0.1, button.hot_t), .inner, .black);
        switch (button.kind) {
            .unknown => {},
            .launch_testcase => drawer.canvas.line(camera, &.{
                button.rect.getCenter().add(.new(-0.25, -0.25)).addX(0.15),
                button.rect.getCenter().add(.new(0, 0)).addX(0.15),
                button.rect.getCenter().add(.new(-0.25, 0.25)).addX(0.15),
            }, 0.05, .black),
        }
    }

    pub fn updateHot(button: *Button, hot: bool, delta_seconds: f32) void {
        math.lerp_towards(&button.hot_t, if (hot) 1 else 0, 0.6, delta_seconds);
    }
};

// reel + definition (+ explanation?)
const Fnkbox = struct {
    handle: Handle,
    fnkname: VeryPhysicalSexpr,
    garland: VeryPhysicalGarland,
    testcases: std.ArrayListUnmanaged(TestCase),
    scroll_testcases: f32 = 0,
    execution: ?struct {
        testcase: usize,
        executor: Executor,
        t: f32 = 0,
        state: enum { starting, executing, ending } = .starting,
    } = null,
    text: []const u8,
    folded: bool,
    folded_t: f32,
    fold_button: Button = .{ .rect = .unit },
    scroll_button_up: Button = .{ .rect = .unit },
    scroll_button_down: Button = .{ .rect = .unit },

    const relative_fnkname_point: Point = .{ .pos = .new(-1, 1), .scale = 0.5, .turns = 0.25 };
    const relative_garland_point: Point = .{ .pos = .new(1, 1) };
    const relative_bottom_testcase_point: Point = .{ .pos = .new(0, box_height - 0.5) };
    const relative_input_point: Point = .{ .pos = .new(-4, 2.5) };
    const text_height: f32 = 3;
    const testcases_height: f32 = 2.5 * visible_testcases;
    const box_height = text_height + testcases_height;
    const visible_testcases = 2;

    pub fn point(fnkbox: *const Fnkbox) Point {
        return .{ .pos = fnkbox.handle.pos };
    }

    pub fn executorPos(fnkbox: *const Fnkbox) Vec2 {
        const offset_y = box_height * (1 - fnkbox.folded_t);
        const relative_executor_point: Point = relative_garland_point.inverseApplyToLocalPoint(.{ .pos = Executor.relative_garland_pos });
        return fnkbox.point().applyToLocalPoint(relative_executor_point).pos.addY(offset_y);
    }

    pub fn init(text: []const u8, fnkname: *const Sexpr, base: Point, testcases_values: []const struct { input: *const Sexpr, expected: *const Sexpr }, hover_pool: *HoveredSexpr.Pool, mem: *core.VeryPermamentGameStuff) !Fnkbox {
        var testcases: @FieldType(Fnkbox, "testcases") = try .initCapacity(mem.gpa, testcases_values.len);
        for (testcases_values) |v| {
            testcases.appendAssumeCapacity(.{
                // TODO: better default pos
                .input = try .fromSexpr(hover_pool, v.input, base, false),
                .expected = try .fromSexpr(hover_pool, v.expected, base, false),
                .actual = try .empty(base, hover_pool, false),
                .play_button = .{ .rect = .unit, .kind = .launch_testcase },
            });
        }
        return .{
            .text = text,
            .handle = .{ .pos = base.pos },
            .garland = .init(base.applyToLocalPoint(relative_garland_point).pos),
            .fnkname = try .fromSexpr(hover_pool, fnkname, base.applyToLocalPoint(relative_fnkname_point), true),
            .testcases = testcases,
            .folded = false,
            .folded_t = 0,
        };
    }

    pub fn deinit(fnkbox: *Fnkbox, gpa: std.mem.Allocator) void {
        fnkbox.testcases.deinit(gpa);
        fnkbox.garland.deinit(gpa);
    }

    pub fn box(fnkbox: *const Fnkbox) Rect {
        return .fromMeasureAndSizeV2(
            .top_center,
            fnkbox.point().pos.addY(0.75),
            Vec2.new(16, box_height).scale(1.0 - fnkbox.folded_t),
        );
    }

    pub fn testcasesBoxUnfolded(fnkbox: *const Fnkbox) Rect {
        return .fromMeasureAndSizeV2(
            .top_center,
            fnkbox.point().pos.addY(0.75).addY(text_height),
            Vec2.new(16, testcases_height),
        );
    }

    pub fn foldButtonGoal(fnkbox: *const Fnkbox) Rect {
        return .fromMeasureAndSizeV2(
            .center,
            fnkbox.point().pos.addY(0.75),
            .half,
        );
    }

    pub fn draw(fnkbox: *const Fnkbox, drawer: *Drawer, camera: Rect) !void {
        if (false) try drawer.drawPlaceholder(camera, fnkbox.point().applyToLocalPoint(relative_input_point), false);
        const rect = fnkbox.box();
        drawer.canvas.borderRect(camera, rect, 0.05, .inner, .black);
        try fnkbox.fnkname.draw(drawer, camera);
        try fnkbox.handle.draw(drawer, camera, 1);
        if (fnkbox.folded_t < 1) {
            {
                drawer.canvas.gl.startStencil();
                drawer.canvas.fillRect(camera, rect, .white);
                drawer.canvas.gl.doneStencil();
                defer drawer.canvas.gl.stopStencil();
                try drawer.canvas.drawText(0, camera, fnkbox.text, .centeredAt(fnkbox.handle.pos.addY(2)), 0.8, .black);

                const testcases_labels_center = fnkbox.testcasesBoxUnfolded().get(.top_center).addY(-0.25).addX(0.85);
                try drawer.canvas.drawText(0, camera, "Input", .centeredAt(testcases_labels_center.addX(-4)), 0.65, .black);
                try drawer.canvas.drawText(0, camera, "Target", .centeredAt(testcases_labels_center.addX(0)), 0.65, .black);
                try drawer.canvas.drawText(0, camera, "Actual", .centeredAt(testcases_labels_center.addX(4)), 0.65, .black);
            }

            if (rect.intersect(fnkbox.testcasesBoxUnfolded())) |r| {
                drawer.canvas.gl.startStencil();
                drawer.canvas.fillRect(camera, r, .white);
                drawer.canvas.gl.doneStencil();
                defer drawer.canvas.gl.stopStencil();
                for (fnkbox.testcases.items) |t| {
                    try t.draw(drawer, camera);
                }
                try fnkbox.scroll_button_up.draw(drawer, camera);
                try fnkbox.scroll_button_down.draw(drawer, camera);
            }
        }
        if (fnkbox.execution) |e| {
            if (e.state == .ending) {
                try fnkbox.garland.drawWithAlpha(math.smoothstep(e.t, 0.9, 1), drawer, camera);
            }
            try e.executor.draw(drawer, camera);
        } else {
            try fnkbox.garland.draw(drawer, camera);
        }
        try fnkbox.fold_button.draw(drawer, camera);
    }

    pub fn updateUiHotness(fnkbox: *Fnkbox, fnkbox_index: usize, ui_hot: Workspace.Focus.UiTarget, delta_seconds: f32) void {
        fnkbox.fold_button.updateHot(switch (ui_hot.kind) {
            else => false,
            .fnkbox_toggle_fold => |k| k == fnkbox_index,
        }, delta_seconds);
        fnkbox.scroll_button_up.updateHot(switch (ui_hot.kind) {
            else => false,
            .fnkbox_scroll => |t| t.fnkbox == fnkbox_index and t.direction == .up,
        }, delta_seconds);
        fnkbox.scroll_button_down.updateHot(switch (ui_hot.kind) {
            else => false,
            .fnkbox_scroll => |t| t.fnkbox == fnkbox_index and t.direction == .down,
        }, delta_seconds);
        for (fnkbox.testcases.items, 0..) |*testcase, testcase_index| {
            const is_hot = switch (ui_hot.kind) {
                else => false,
                .fnkbox_launch_testcase => |thing| thing.fnkbox == fnkbox_index and thing.testcase == testcase_index,
            };
            testcase.play_button.updateHot(is_hot, delta_seconds);
        }
    }

    pub fn update(fnkbox: *Fnkbox, mem: *core.VeryPermamentGameStuff, known_fnks: []const Fnkbox, hover_pool: *HoveredSexpr.Pool, delta_seconds: f32) !void {
        math.lerp_towards_range(&fnkbox.scroll_testcases, 0, tof32(fnkbox.testcases.items.len) - visible_testcases, 0.6, delta_seconds);
        fnkbox.fold_button.rect.lerpTowards(fnkbox.foldButtonGoal(), 0.6, delta_seconds);
        fnkbox.scroll_button_up.rect.lerpTowards(fnkbox.testcasesBoxUnfolded().withSize(.new(1.2, 0.7), .top_left).plusMargin(-0.1), 0.6, delta_seconds);
        fnkbox.scroll_button_down.rect.lerpTowards(fnkbox.testcasesBoxUnfolded().withSize(.new(1.2, 0.7), .bottom_left).plusMargin(-0.1), 0.6, delta_seconds);
        math.lerp_towards(&fnkbox.folded_t, if (fnkbox.folded) 1 else 0, 0.6, delta_seconds);
        const offset_y = box_height * (1 - fnkbox.folded_t);
        Vec2.lerpTowards(&fnkbox.garland.handle.pos, fnkbox.point().applyToLocalPoint(relative_garland_point).pos.addY(offset_y), 0.6, delta_seconds);
        fnkbox.garland.update(delta_seconds);
        fnkbox.fnkname.point.lerp_towards(fnkbox.point().applyToLocalPoint(relative_fnkname_point), 0.6, delta_seconds);
        for (fnkbox.testcases.items, 0..) |*t, k| {
            const center = fnkbox.point()
                .applyToLocalPoint(relative_bottom_testcase_point)
                .applyToLocalPoint(.{ .pos = .new(0, -2.5 * (tof32(k) - fnkbox.scroll_testcases)) });
            t.input.point.lerp_towards(center.applyToLocalPoint(.{ .pos = .new(-4, 0) }), 0.6, delta_seconds);
            t.expected.point.lerp_towards(center.applyToLocalPoint(.{ .pos = .new(0, 0) }), 0.6, delta_seconds);
            t.actual.point.lerp_towards(center.applyToLocalPoint(.{ .pos = .new(4, 0) }), 0.6, delta_seconds);
            t.play_button.rect.lerpTowards(.fromCenterAndSize(center.applyToLocalPosition(.new(-6, 0)), .one), 0.6, delta_seconds);
        }
        if (fnkbox.execution) |*execution| {
            switch (execution.state) {
                .starting => {
                    execution.executor.input.point = .lerp(fnkbox.testcases.items[execution.testcase].input.point, execution.executor.inputPoint(), execution.t);
                    execution.t += delta_seconds / 0.8;
                    if (execution.t >= 1) {
                        execution.state = .executing;
                        execution.t = 0;
                    }
                },
                .executing => {
                    try execution.executor.update(mem, known_fnks, hover_pool, delta_seconds);
                    if (execution.executor.animation == null) {
                        execution.state = .ending;
                        execution.t = 0;
                    }
                },
                .ending => {
                    execution.executor.input.point = .lerp(
                        execution.executor.inputPoint(),
                        fnkbox.testcases.items[execution.testcase].expected.point.applyToLocalPoint(.{ .pos = .new(4, 0) }),
                        execution.t,
                    );
                    execution.t += delta_seconds / 0.8;
                    if (execution.t >= 1) {
                        fnkbox.testcases.items[execution.testcase].actual = try execution.executor.input.clone(hover_pool);
                        fnkbox.execution = null;
                    }
                },
            }

            // TODO: this is ignored when the animation is active
            // e.handle.pos.lerpTowards(fnkbox.point().applyToLocalPoint(relative_executor_point).pos, 0.6, delta_seconds);
        }
    }
};

pub fn getGarlandForFnk(
    known_fnks: []const Fnkbox,
    fnkname: *const Sexpr,
    new_point: Point,
    mem: *core.VeryPermamentGameStuff,
    hover_pool: *HoveredSexpr.Pool,
) !?VeryPhysicalGarland {
    for (known_fnks) |k| {
        if (k.fnkname.value.equals(fnkname)) {
            var garland = try k.garland.clone(mem.gpa, hover_pool);
            garland.kinematicUpdate(new_point, undefined);
            return garland;
        }
    } else return null;
}

const Workspace = struct {
    pub const Lens = struct {
        source: Vec2,
        target: Vec2,
        comptime source_radius: f32 = 0.25,
        comptime target_radius: f32 = 1,
        source_hot_t: f32 = 0,
        target_hot_t: f32 = 0,
        tmp_visible_sexprs: std.ArrayListUnmanaged(struct {
            original_place: BaseSexprPlace,
            lens_transform: Transform,
        }) = .empty,
        const handle_radius: f32 = 0.1;

        /// To understand this, think of the fixed point of the lenses zoom
        pub const Transform = struct {
            center: Vec2,
            scale: f32,

            pub const identity: Transform = .{ .center = .zero, .scale = 1 };

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
        };

        pub fn getTransform(lens: Lens) Transform {
            const scale = lens.target_radius / lens.source_radius;
            const delta = lens.target.sub(lens.source);
            return .{
                .center = lens.source.sub(delta.scale(1.0 / (scale - 1.0))),
                .scale = scale,
            };
        }

        pub fn setHandlePos(self: *Lens, part: Part, pos: Vec2) void {
            switch (part) {
                .source => self.source = pos.sub(.fromPolar(self.source_radius + 0.05, 0.125)),
                .target => self.target = pos.sub(.fromPolar(self.target_radius + 0.05, 0.125)),
            }
        }

        pub fn handlePos(self: Lens, part: Part) Vec2 {
            return switch (part) {
                .source => self.sourceHandlePos(),
                .target => self.targetHandlePos(),
            };
        }

        pub fn sourceHandlePos(self: Lens) Vec2 {
            return self.source.add(.fromPolar(self.source_radius + 0.05, 0.125));
        }

        pub fn targetHandlePos(self: Lens) Vec2 {
            return self.target.add(.fromPolar(self.target_radius + 0.05, 0.125));
        }

        pub const Part = enum { source, target };

        pub fn update(self: *Lens, part: ?Part, delta_seconds: f32) void {
            math.lerp_towards(&self.source_hot_t, if (part != null and part.? == .source) 1 else 0, 0.6, delta_seconds);
            math.lerp_towards(&self.target_hot_t, if (part != null and part.? == .target) 1 else 0, 0.6, delta_seconds);
        }

        pub fn clone(self: *const Lens) Lens {
            // not plain old data due to tmp_*, but that's not important
            return self.*;
        }
    };

    lenses: std.ArrayList(Lens),
    sexprs: std.ArrayList(VeryPhysicalSexpr),
    cases: std.ArrayList(VeryPhysicalCase),
    garlands: std.ArrayList(VeryPhysicalGarland),
    executors: std.ArrayList(Executor),
    fnkviewers: std.ArrayList(Fnkviewer),
    fnkboxes: std.ArrayList(Fnkbox),

    hover_pool: HoveredSexpr.Pool,

    focus: Focus = .{},

    undo_stack: std.ArrayList(UndoableCommand),

    const Focus = struct {
        // TODO: Not really any Target, since for sexprs it's .grabbed with no local
        grabbing: Target = .nothing,
        ui_active: UiTarget = .nothing,

        const UiTarget = struct {
            kind: Kind,

            pub const nothing: UiTarget = .{ .kind = .nothing };

            pub const Kind = union(enum) {
                nothing,
                fnkbox_toggle_fold: usize,
                fnkbox_launch_testcase: struct {
                    fnkbox: usize,
                    testcase: usize,
                },
                fnkbox_scroll: struct {
                    fnkbox: usize,
                    direction: enum { up, down },
                },
            };

            pub fn equals(a: UiTarget, b: UiTarget) bool {
                return std.meta.eql(a, b);
            }
        };

        const Target = struct {
            kind: Kind,
            lens_transform: Lens.Transform = .identity,

            pub const nothing: Target = .{ .kind = .nothing };

            pub const Kind = union(enum) {
                nothing,
                sexpr: SexprPlace,
                lens_handle: struct {
                    index: usize,
                    part: Lens.Part,
                },
                case_handle: CaseHandle,
                garland_handle: GarlandHandle,
                executor_handle: usize,
                fnkviewer_handle: usize,
                fnkbox_handle: usize,

                pub fn equals(a: Kind, b: Kind) bool {
                    return kommon.meta.eql(a, b);
                }
            };
        };
    };

    const CaseHandle = union(enum) {
        board: usize,
        garland: struct {
            local: usize,
            parent: GarlandHandle,
            existing_case: bool,
        },

        pub fn exists(case_handle: CaseHandle) bool {
            return switch (case_handle) {
                .board => true,
                .garland => |t| t.existing_case,
            };
        }

        pub fn clone(original: CaseHandle, res: std.mem.Allocator) !CaseHandle {
            return switch (original) {
                .garland => |t| .{ .garland = .{
                    .existing_case = t.existing_case,
                    .local = t.local,
                    .parent = try t.parent.clone(res),
                } },
                else => original,
            };
        }
    };

    const GarlandHandle = struct {
        local: core.CaseAddress,
        parent: Parent,

        pub const Parent = union(enum) {
            garland: usize,
            case: usize,
            executor: usize,
            fnkviewer: usize,
            fnkbox: usize,
        };

        pub fn clone(original: GarlandHandle, res: std.mem.Allocator) !GarlandHandle {
            return .{
                .local = try res.dupe(usize, original.local),
                .parent = original.parent,
            };
        }
    };

    const BaseSexprPlace = union(enum) {
        board: usize,
        case: struct {
            parent: CaseHandle,
            part: core.CasePart,
        },
        executor_input: usize,
        fnkviewer_fnkname: usize,
        fnkbox_fnkname: usize,
        fnkbox_testcase: struct {
            fnkbox: usize,
            testcase: usize,
            part: TestCase.Part,
        },

        pub fn immutable(place: BaseSexprPlace) bool {
            return switch (place) {
                .fnkbox_fnkname, .fnkbox_testcase => true,
                .board, .case, .executor_input, .fnkviewer_fnkname => false,
            };
        }

        pub fn equals(a: BaseSexprPlace, b: BaseSexprPlace) bool {
            return kommon.meta.eql(a, b);
        }

        pub fn clone(original: BaseSexprPlace, res: std.mem.Allocator) !BaseSexprPlace {
            return switch (original) {
                .case => |c| .{ .case = .{
                    .part = c.part,
                    .parent = try c.parent.clone(res),
                } },
                else => original,
            };
        }
    };
    const SexprPlace = struct {
        base: BaseSexprPlace,
        local: core.SexprAddress,
    };
    const UndoableCommand = struct {
        specific: union(enum) {
            noop,
            dropped: struct {
                at: Focus.Target,
                /// only used when 'at' is of kind sexpr
                overwritten_sexpr: ?VeryPhysicalSexpr,
                /// only used when 'at' is of kind garland_handle
                overwritten_garland: VeryPhysicalGarland,
                /// for sexprs is always 'board',
                /// for lens_handle is equal to .at,
                /// for case_handle might be different
                /// if it was dropped in a garland and thus
                /// removed from its old board position.
                old_grabbed_position: Focus.Target,
            },
            grabbed: struct {
                duplicate: bool,
                from: Focus.Target,
                /// not always used
                old_position: Vec2,
                /// not always used
                old_ispattern: bool,
            },
            started_execution: struct {
                executor: usize,
                input: VeryPhysicalSexpr,
                garland: VeryPhysicalGarland,
                prev_pills: []Pill,
                prev_old_bindings: []core.Binding,
                prev_pos: Vec2,
            },
            fnkbox_launch_testcase: struct {
                fnkbox: usize,
                testcase: usize,
            },
            fnkbox_toggle_fold: usize,
            fnkbox_scroll: struct {
                fnkbox: usize,
                old_position: f32,
            },
        },

        pub const noop: UndoableCommand = .{ .specific = .noop };
    };

    pub fn init(dst: *Workspace, mem: *core.VeryPermamentGameStuff) !void {
        dst.* = kommon.meta.initDefaultFields(Workspace);

        dst.undo_stack = .init(mem.gpa);

        dst.hover_pool = try .initPreheated(mem.gpa, 0x100);

        dst.lenses = .init(mem.gpa);
        try dst.lenses.append(.{ .source = ViewHelper.sexprTemplateChildView(
            .{},
            &.{ .right, .left },
        ).applyToLocalPosition(.new(1, 0)), .target = .new(3, 0) });
        try dst.lenses.append(.{ .source = .new(3, 0), .target = .new(6, 0) });

        dst.sexprs = .init(mem.gpa);
        var random: std.Random.DefaultPrng = .init(1);
        if (true) {
            try dst.sexprs.append(try .fromSexpr(&dst.hover_pool, try randomSexpr(mem, random.random(), 7), .{}, false));

            try dst.sexprs.append(try .fromSexpr(&dst.hover_pool, valid[0], .{ .pos = .new(-3, 0) }, false));

            for ([_][]const u8{
                "Hermes",    "Mercury",
                "Ares",      "Mars",
                "Zeus",      "Jupiter",
                "Aphrodite", "Venus",
            }, 0..) |n, k| {
                try dst.sexprs.append(try .fromSexpr(&dst.hover_pool, try mem.storeSexpr(.doLit(n)), .{ .pos = .new(tof32(k) * 3, -2 + tof32(k % 2) - tof32(k)) }, k % 2 == 0));
            }
        } else {
            try dst.sexprs.append(try .fromSexpr(&dst.hover_pool, Sexpr.pair_nil_nil, .{}, false));
        }

        dst.cases = .init(mem.gpa);
        try dst.cases.append(try .fromValues(&dst.hover_pool, .{
            .pattern = Sexpr.builtin.true,
            .template = Sexpr.builtin.false,
            .fnk_name = Sexpr.builtin.identity,
        }, .{ .pos = .new(0, 3) }));
        try dst.cases.append(try .fromValues(&dst.hover_pool, .{
            .pattern = Sexpr.builtin.vars.v1,
            .template = try mem.storeSexpr(.doPair(Sexpr.builtin.vars.v1, Sexpr.builtin.vars.v1)),
            .fnk_name = Sexpr.builtin.identity,
        }, .{ .pos = .new(-7, 0) }));

        dst.garlands = .init(mem.gpa);
        try dst.garlands.append(.{
            .cases = try .initCapacity(mem.gpa, 4),
            .handle = .{ .pos = .new(0, 5) },
            .handles_for_new_cases_first = .{ .pos = .new(0, 5) },
            .handles_for_new_cases_rest = try .initCapacity(mem.gpa, 4),
        });

        for (0..4) |k| {
            dst.garlands.items[0].handles_for_new_cases_rest.appendAssumeCapacity(.{ .pos = .new(0, tof32(k + 1)) });
        }
        for ([_][2][]const u8{
            .{ "Hermes", "Mercury" },
            .{ "Ares", "Mars" },
            .{ "Zeus", "Jupiter" },
            .{ "Aphrodite", "Venus" },
        }, 0..) |p, k| {
            dst.garlands.items[0].cases.appendAssumeCapacity(try .fromValues(&dst.hover_pool, .{
                .pattern = try mem.storeSexpr(.doLit(p[0])),
                .template = try mem.storeSexpr(.doLit(p[1])),
                .fnk_name = Sexpr.builtin.empty,
            }, .{ .pos = dst.garlands.items[0].handle.pos.addY(1 + 2.5 * tof32(k)) }));
        }
        try dst.garlands.items[0].cases.items[0].next.insertCase(mem.gpa, 0, try .fromValues(&dst.hover_pool, .{
            .pattern = Sexpr.builtin.true,
            .template = valid[1],
            .fnk_name = valid[0],
        }, .{ .pos = dst.garlands.items[0].handle.pos.addX(6) }));

        dst.executors = .init(mem.gpa);
        try dst.executors.append(try .init(.new(-5, -5), &dst.hover_pool));

        dst.fnkviewers = .init(mem.gpa);
        try dst.fnkviewers.append(try .init(.{ .pos = .new(-6, -7) }, &dst.hover_pool));

        dst.fnkboxes = .init(mem.gpa);
        try dst.fnkboxes.append(try .init(
            \\Get the lowercase version of each atom
        , valid[0], .{ .pos = .new(100, 0) }, &.{
            .{ .input = valid[1], .expected = valid[2] },
            .{ .input = valid[3], .expected = valid[4] },
            .{ .input = valid[5], .expected = valid[6] },
            .{ .input = valid[7], .expected = valid[8] },
        }, &dst.hover_pool, mem));
    }

    pub fn deinit(workspace: *Workspace, gpa: std.mem.Allocator) void {
        for (workspace.cases.items) |*c| {
            c.next.deinit(gpa);
        }
        for (workspace.garlands.items) |*g| {
            g.deinit(gpa);
        }
        inline for (.{
            workspace.executors.items,
            workspace.fnkviewers.items,
            workspace.fnkboxes.items,
        }) |things| {
            for (things) |*e| {
                if (std.meta.hasMethod(@TypeOf(e), "deinit")) {
                    e.deinit(gpa);
                } else {
                    e.garland.deinit(gpa);
                }
            }
        }
        {
            var it = workspace.known_fnks.iterator();
            while (it.next()) |kv| {
                kv.value_ptr.cases.deinit(gpa);
            }
        }
        workspace.lenses.deinit();
        workspace.sexprs.deinit();
        workspace.cases.deinit();
        workspace.hover_pool.deinit();
        workspace.undo_stack.deinit();
        workspace.garlands.deinit();
        workspace.executors.deinit();
        workspace.fnkviewers.deinit();
        workspace.fnkboxes.deinit();
        workspace.known_fnks.deinit();
    }

    const valid: []const *const Sexpr = &.{
        &Sexpr.doLit("planetFromOlympian"),
        &Sexpr.doLit("Hermes"),
        &Sexpr.doLit("Mercury"),
        &Sexpr.doLit("Aphrodite"),
        &Sexpr.doLit("Venus"),
        &Sexpr.doLit("Ares"),
        &Sexpr.doLit("Mars"),
        &Sexpr.doLit("Zeus"),
        &Sexpr.doLit("Jupiter"),
        &.empty,
    };

    fn randomSexpr(mem: *core.VeryPermamentGameStuff, random: std.Random, max_depth: usize) !*const Sexpr {
        if (max_depth == 0 or random.float(f32) < 0.3) {
            return valid[random.uintLessThan(usize, valid.len)];
        } else {
            return try mem.storeSexpr(Sexpr.doPair(
                try randomSexpr(mem, random, max_depth - 1),
                try randomSexpr(mem, random, max_depth - 1),
            ));
        }
    }

    // TODO: could be an iterator
    pub fn topLevelThings(workspace: *Workspace, res: std.mem.Allocator) ![]const Focus.Target {
        var result: std.ArrayListUnmanaged(Focus.Target) = try .initCapacity(
            res,
            workspace.sexprs.items.len +
                workspace.cases.items.len +
                workspace.garlands.items.len +
                workspace.executors.items.len +
                workspace.fnkviewers.items.len +
                workspace.fnkboxes.items.len,
        );

        for (workspace.sexprs.items, 0..) |_, k| {
            result.appendAssumeCapacity(.{ .kind = .{ .sexpr = .{ .base = .{ .board = k }, .local = &.{} } } });
        }
        for (workspace.cases.items, 0..) |_, k| {
            result.appendAssumeCapacity(.{ .kind = .{ .case_handle = .{ .board = k } } });
        }
        for (workspace.garlands.items, 0..) |_, k| {
            result.appendAssumeCapacity(.{ .kind = .{ .garland_handle = .{
                .local = &.{},
                .parent = .{ .garland = k },
            } } });
        }
        for (workspace.executors.items, 0..) |_, k| {
            result.appendAssumeCapacity(.{ .kind = .{ .executor_handle = k } });
        }
        for (workspace.fnkviewers.items, 0..) |_, k| {
            result.appendAssumeCapacity(.{ .kind = .{ .fnkviewer_handle = k } });
        }
        for (workspace.fnkboxes.items, 0..) |_, k| {
            result.appendAssumeCapacity(.{ .kind = .{ .fnkbox_handle = k } });
        }

        return try result.toOwnedSlice(res);
    }

    // TODO: could be an iterator
    pub fn ownedSexprs(workspace: *Workspace, parent: Focus.Target, res: std.mem.Allocator) ![]const BaseSexprPlace {
        var result: std.ArrayListUnmanaged(BaseSexprPlace) = .empty;

        // sexprs owned directly and not due to a case
        switch (parent.kind) {
            .sexpr => |p| {
                try result.append(res, p.base);
            },
            .executor_handle => |k| {
                try result.append(res, .{ .executor_input = k });
            },
            .fnkviewer_handle => |k| {
                try result.append(res, .{ .fnkviewer_fnkname = k });
            },
            .fnkbox_handle => |k| {
                try result.append(res, .{ .fnkbox_fnkname = k });
                const parent_fnkbox = &workspace.fnkboxes.items[k];
                for (parent_fnkbox.testcases.items, 0..) |_, testcase_index| {
                    inline for (TestCase.parts) |part| {
                        try result.append(res, .{ .fnkbox_testcase = .{
                            .fnkbox = k,
                            .testcase = testcase_index,
                            .part = part,
                        } });
                    }
                }
            },
            .nothing,
            .lens_handle,
            .case_handle,
            .garland_handle,
            => {},
        }

        // sexprs owned due to owning a case
        const owned_cases = try workspace.ownedCases(parent, res);
        for (owned_cases) |case_address| {
            if (!case_address.exists()) continue;
            inline for (core.CasePart.all) |part| {
                try result.append(res, .{ .case = .{
                    .parent = case_address,
                    .part = part,
                } });
            }
        }

        return try result.toOwnedSlice(res);
    }

    // TODO: could be an iterator
    // TODO: don't assume that parent is a top level thing
    pub fn ownedGarlands(workspace: *Workspace, parent: Focus.Target, res: std.mem.Allocator) ![]const GarlandHandle {
        var result: std.ArrayListUnmanaged(GarlandHandle) = .empty;

        if (@as(?GarlandHandle.Parent, switch (parent.kind) {
            .case_handle => |case_handle| .{ .case = case_handle.board },
            .garland_handle => |garland_handle| .{ .garland = garland_handle.parent.garland },
            .executor_handle => |k| .{ .executor = k },
            .fnkviewer_handle => |k| .{ .fnkviewer = k },
            .fnkbox_handle => |k| .{ .fnkbox = k },
            .nothing, .lens_handle, .sexpr => null,
        })) |parent_garland_handle| {
            const parent_garland = workspace.garlandAt(.{ .local = &.{}, .parent = parent_garland_handle });
            var it = parent_garland.childGarlandsAddressIterator(res);
            while (try it.next()) |address| {
                try result.append(res, .{
                    .local = address,
                    .parent = parent_garland_handle,
                });
            }
        }

        return try result.toOwnedSlice(res);
    }

    // TODO: could be an iterator
    // TODO: don't assume that parent is a top level thing
    pub fn ownedCases(workspace: *Workspace, parent: Focus.Target, res: std.mem.Allocator) ![]const CaseHandle {
        var result: std.ArrayListUnmanaged(CaseHandle) = .empty;

        // cases owned directly and not due to a garland
        switch (parent.kind) {
            .case_handle => |case_handle| {
                try result.append(res, case_handle);
            },
            .garland_handle,
            .executor_handle,
            .fnkviewer_handle,
            .fnkbox_handle,
            .nothing,
            .lens_handle,
            .sexpr,
            => {},
        }

        // cases owned due to owning a garland
        const owned_garlands = try workspace.ownedGarlands(parent, res);
        for (owned_garlands) |garland_address| {
            const garland = workspace.garlandAt(garland_address);
            for (0..garland.cases.items.len) |case_index| {
                try result.append(res, .{
                    .garland = .{
                        .local = case_index,
                        .parent = garland_address,
                        .existing_case = true,
                    },
                });
            }
            for (0..garland.cases.items.len + 1) |case_index| {
                try result.append(res, .{
                    .garland = .{
                        .local = case_index,
                        .parent = garland_address,
                        .existing_case = false,
                    },
                });
            }
        }

        return try result.toOwnedSlice(res);
    }

    fn sexprAtPlace(workspace: *Workspace, place: BaseSexprPlace) *VeryPhysicalSexpr {
        return switch (place) {
            .board => |p| workspace.getAt(VeryPhysicalSexpr, p),
            .case => |case| workspace.caseAt(case.parent).sexprAt(case.part),
            .executor_input => |k| &workspace.executors.items[k].input,
            .fnkviewer_fnkname => |k| &workspace.fnkviewers.items[k].fnkname,
            .fnkbox_fnkname => |k| &workspace.fnkboxes.items[k].fnkname,
            .fnkbox_testcase => |t| workspace.fnkboxes.items[t.fnkbox].testcases.items[t.testcase].partRef(t.part),
        };
    }

    fn caseHandleRef(workspace: *Workspace, place: CaseHandle) *Handle {
        return switch (place) {
            .board => |k| &workspace.cases.items[k].handle,
            .garland => |t| if (t.existing_case)
                &garlandAt(workspace, t.parent).cases.items[t.local].handle
            else
                garlandAt(workspace, t.parent).handleForNewCasesRef(t.local),
        };
    }

    fn garlandHandleRef(workspace: *Workspace, place: GarlandHandle) *Handle {
        return &workspace.garlandAt(place).handle;
    }

    fn garlandAt(workspace: *Workspace, place: GarlandHandle) *VeryPhysicalGarland {
        return switch (place.parent) {
            .garland => |k| workspace.garlands.items[k].childGarland(place.local),
            .case => |k| workspace.cases.items[k].next.childGarland(place.local),
            .executor => |k| workspace.executors.items[k].garland.childGarland(place.local),
            .fnkviewer => |k| workspace.fnkviewers.items[k].garland.childGarland(place.local),
            .fnkbox => |k| workspace.fnkboxes.items[k].garland.childGarland(place.local),
        };
    }

    fn caseAt(workspace: *Workspace, place: CaseHandle) *VeryPhysicalCase {
        assert(place.exists());
        return switch (place) {
            .board => |k| &workspace.cases.items[k],
            .garland => |t| &workspace.garlandAt(t.parent).cases.items[t.local],
        };
    }

    // This made more sense when it was indexed by a Vec2
    fn getAt(workspace: *Workspace, comptime T: type, k: usize) *T {
        const items = switch (T) {
            VeryPhysicalSexpr => workspace.sexprs.items,
            VeryPhysicalCase => workspace.cases.items,
            VeryPhysicalGarland => workspace.garlands.items,
            else => comptime unreachable,
        };
        return &items[k];
    }

    fn popSexprAt(workspace: *Workspace, p: SexprPlace, hover_pool: *HoveredSexpr.Pool, mem: *VeryPermamentGameStuff) !?VeryPhysicalSexpr {
        if (p.local.len == 0) switch (p.base) {
            else => {},
            .board => |k| return workspace.sexprs.orderedRemove(k),
        };

        const base = workspace.sexprAtPlace(p.base);
        return try base.popSubValue(p.local, hover_pool, mem);
    }

    fn unpopSexprAt(workspace: *Workspace, p: SexprPlace, v: VeryPhysicalSexpr, hover_pool: *HoveredSexpr.Pool, mem: *VeryPermamentGameStuff) !void {
        if (p.local.len == 0) {
            switch (p.base) {
                else => try workspace.sexprAtPlace(p.base).updateSubValue(p.local, v.value, v.hovered, mem, hover_pool),
                .board => |k| try workspace.sexprs.insert(k, v),
            }
        } else {
            try workspace.sexprAtPlace(p.base).updateSubValue(p.local, v.value, v.hovered, mem, hover_pool);
        }
    }

    fn draw(workspace: *Workspace, platform: PlatformGives, drawer: *Drawer, camera: Rect) !void {
        // TODO: remove duplication
        for (workspace.sexprs.items, 0..) |s, k| {
            if (isGrabbedSexpr(.{ .board = k }, workspace.focus.grabbing)) continue;
            try s.draw(drawer, camera);
        }

        for (workspace.cases.items, 0..) |c, k| {
            if (isGrabbedCase(.{ .board = k }, workspace.focus.grabbing)) continue;
            try c.draw(drawer, camera);
        }

        inline for (.{
            workspace.garlands.items,
            workspace.fnkboxes.items,
            workspace.executors.items,
            workspace.fnkviewers.items,
        }) |things| {
            for (things) |g| {
                try g.draw(drawer, camera);
            }
        }

        for (workspace.lenses.items) |lens| {
            drawer.canvas.fillCircle(camera, lens.target, lens.target_radius, .gray(0.5));

            if (true) {
                platform.gl.startStencil();
                drawer.canvas.fillCircle(camera, lens.target, lens.target_radius, .white);
                platform.gl.doneStencil();
                defer platform.gl.stopStencil();

                for (lens.tmp_visible_sexprs.items) |s| {
                    var scaled = workspace.sexprAtPlace(s.original_place).*;
                    scaled.point = s.lens_transform.actOn(scaled.point);
                    try scaled.draw(drawer, camera);
                }
            }

            drawer.canvas.line(camera, &.{
                lens.source.towardsPure(lens.target, lens.source_radius),
                lens.target.towardsPure(lens.source, lens.target_radius),
            }, 0.05, .black);
            drawer.canvas.strokeCircle(128, camera, lens.source, lens.source_radius, 0.05, .black);
            drawer.canvas.strokeCircle(128, camera, lens.target, lens.target_radius, 0.05, .black);
            drawer.canvas.fillCircle(
                camera,
                lens.sourceHandlePos(),
                Lens.handle_radius * (1.0 + 0.2 * lens.source_hot_t),
                .black,
            );
            drawer.canvas.fillCircle(
                camera,
                lens.targetHandlePos(),
                Lens.handle_radius * (1.0 + 0.2 * lens.target_hot_t),
                .black,
            );
        }

        switch (workspace.focus.grabbing.kind) {
            else => {},
            .case_handle => |t| try workspace.cases.items[t.board].draw(drawer, camera),
            .sexpr => |s| try workspace.sexprAtPlace(s.base).draw(drawer, camera),
        }
    }

    fn findUiAtPosition(workspace: *Workspace, pos: Vec2) !Focus.UiTarget {
        for (workspace.fnkboxes.items, 0..) |fnkbox, fnkbox_index| {
            if (fnkbox.fold_button.rect.contains(pos)) {
                return .{ .kind = .{ .fnkbox_toggle_fold = fnkbox_index } };
            }
            if (fnkbox.scroll_button_up.rect.contains(pos)) {
                return .{ .kind = .{ .fnkbox_scroll = .{ .fnkbox = fnkbox_index, .direction = .up } } };
            }
            if (fnkbox.scroll_button_down.rect.contains(pos)) {
                return .{ .kind = .{ .fnkbox_scroll = .{ .fnkbox = fnkbox_index, .direction = .down } } };
            }
            if (fnkbox.execution != null) continue;
            if (!fnkbox.box().contains(pos)) continue;
            if (!fnkbox.testcasesBoxUnfolded().contains(pos)) continue;
            for (fnkbox.testcases.items, 0..) |testcase, testcase_index| {
                if (testcase.play_button.rect.contains(pos)) {
                    return .{ .kind = .{ .fnkbox_launch_testcase = .{
                        .fnkbox = fnkbox_index,
                        .testcase = testcase_index,
                    } } };
                }
            }
        }
        return .nothing;
    }

    fn findHoverableOrDropzoneAtPosition(workspace: *Workspace, pos: Vec2, res: std.mem.Allocator, scratch: std.mem.Allocator) !Focus.Target {
        const grabbed = workspace.focus.grabbing;
        const grabbed_tag = std.meta.activeTag(grabbed.kind);

        // lenses
        if (grabbed_tag == .nothing) {
            for (workspace.lenses.items, 0..) |lens, k| {
                if (pos.distTo(lens.sourceHandlePos()) < Lens.handle_radius) {
                    return .{ .kind = .{ .lens_handle = .{ .index = k, .part = .source } } };
                }
                if (pos.distTo(lens.targetHandlePos()) < Lens.handle_radius) {
                    return .{ .kind = .{ .lens_handle = .{ .index = k, .part = .target } } };
                }
            }
        }

        // executors
        if (grabbed_tag == .nothing) {
            for (workspace.executors.items, 0..) |executor, k| {
                if (executor.animating()) continue;
                if (pos.distTo(executor.handle.pos) < Handle.radius) {
                    return .{ .kind = .{ .executor_handle = k } };
                }
            }
        }

        // fnkviewers
        if (grabbed_tag == .nothing) {
            for (workspace.fnkviewers.items, 0..) |fnkviewer, k| {
                if (pos.distTo(fnkviewer.handle.pos) < Handle.radius) {
                    return .{ .kind = .{ .fnkviewer_handle = k } };
                }
            }
        }

        // fnkboxes handles
        if (grabbed_tag == .nothing) {
            for (workspace.fnkboxes.items, 0..) |thing, k| {
                if (pos.distTo(thing.handle.pos) < Handle.radius) {
                    return .{ .kind = .{ .fnkbox_handle = k } };
                }
            }
        }

        const top_level_things = try workspace.topLevelThings(scratch);

        // sexprs inside lenses and everywhere else
        if (grabbed_tag == .nothing or grabbed_tag == .sexpr) {
            const is_dropzone = grabbed_tag != .nothing;
            for (workspace.lenses.items) |lens| {
                if (pos.distTo(lens.target) < lens.target_radius) {
                    for (lens.tmp_visible_sexprs.items) |s| {
                        if (isGrabbedSexpr(s.original_place, grabbed)) continue;
                        const original = sexprAtPlace(workspace, s.original_place);
                        if (try ViewHelper.overlapsSexpr(
                            // TODO: check this doesn't leak
                            res,
                            original.is_pattern,
                            original.value,
                            s.lens_transform.actOn(original.point),
                            pos,
                            is_dropzone,
                        )) |address| {
                            return .{
                                .kind = .{
                                    .sexpr = .{
                                        .base = try s.original_place.clone(res),
                                        .local = address,
                                    },
                                },
                                .lens_transform = s.lens_transform,
                            };
                        }
                    }
                }
            }

            for (top_level_things) |parent_address| {
                const owned_sexprs = try workspace.ownedSexprs(parent_address, scratch);
                for (owned_sexprs) |base| {
                    if (isGrabbedSexpr(base, grabbed)) continue;

                    // some special cases
                    switch (base) {
                        .executor_input => |k| {
                            if (workspace.executors.items[k].animating()) continue;
                        },
                        .fnkbox_fnkname => {
                            // TODO: change this if some part of fnkbox gets mutable
                            if (grabbed_tag != .nothing) continue;
                        },
                        .fnkbox_testcase => |t| {
                            // TODO: change this if some part of fnkbox gets mutable
                            if (grabbed_tag != .nothing) continue;
                            const fnkbox = &workspace.fnkboxes.items[t.fnkbox];
                            if (!fnkbox.box().contains(pos)) continue;
                            if (!fnkbox.testcasesBoxUnfolded().contains(pos)) continue;
                        },
                        else => {},
                    }

                    const s = workspace.sexprAtPlace(base);
                    if (try ViewHelper.overlapsSexpr(
                        // TODO: check this doesn't leak
                        res,
                        s.is_pattern,
                        s.value,
                        s.point,
                        pos,
                        is_dropzone,
                    )) |address| {
                        return .{ .kind = .{ .sexpr = .{
                            .base = try base.clone(res),
                            .local = address,
                        } } };
                    }
                }
            }
        }

        // garlands
        if (grabbed_tag == .nothing or grabbed_tag == .garland_handle) {
            for (top_level_things) |parent_address| {
                const owned_garlands = try workspace.ownedGarlands(parent_address, scratch);
                for (owned_garlands) |base| {
                    if (isGrabbedGarland(base, grabbed)) continue;

                    // some special cases
                    switch (base.parent) {
                        .garland => {
                            // don't place garlands over board-level garlands (TODO: maybe not a bad idea?)
                            if (grabbed_tag == .garland_handle and base.local.len == 0) continue;
                        },
                        .executor => |k| {
                            if (workspace.executors.items[k].animating()) continue;
                        },
                        else => {},
                    }

                    const g = workspace.garlandAt(base);
                    if (pos.distTo(g.handle.pos) < VeryPhysicalGarland.handle_radius) {
                        return .{ .kind = .{ .garland_handle = try base.clone(res) } };
                    }
                }
            }
        }

        // cases, for picking and for dropping
        for (top_level_things) |parent_address| {
            const owned_cases = try workspace.ownedCases(parent_address, scratch);
            for (owned_cases) |base| {
                if (isGrabbedCase(base, grabbed)) continue;
                const handle = workspace.caseHandleRef(base);

                // picking
                if (base.exists() and grabbed_tag == .nothing) {
                    if (pos.distTo(handle.pos) < VeryPhysicalCase.handle_radius) {
                        return .{ .kind = .{ .case_handle = try base.clone(res) } };
                    }
                }

                // dropping
                if (!base.exists() and grabbed_tag == .case_handle) {
                    if (pos.distTo(handle.pos) < VeryPhysicalGarland.handle_drop_radius) {
                        return .{ .kind = .{ .case_handle = try base.clone(res) } };
                    }
                }
            }
        }

        return .nothing;
    }

    pub fn update(workspace: *Workspace, platform: PlatformGives, drawer: ?*Drawer, camera: Rect, mem: *VeryPermamentGameStuff, frame_arena: std.mem.Allocator) !void {
        // std.log.debug("fps {d}", .{1.0 / platform.delta_seconds});

        // set lenses data
        for (workspace.lenses.items, 0..) |*lens, lens_index| {
            lens.tmp_visible_sexprs = .empty;

            // TODO: cull and only store visible parts

            const top_level_things = try workspace.topLevelThings(frame_arena);
            for (top_level_things) |parent_address| {
                const owned_sexprs = try workspace.ownedSexprs(parent_address, frame_arena);
                for (owned_sexprs) |base| {
                    try lens.tmp_visible_sexprs.append(frame_arena, .{
                        .original_place = base,
                        .lens_transform = lens.getTransform(),
                    });
                }
            }

            for (0..lens_index) |other_lens_index| {
                const other_lens = workspace.lenses.items[other_lens_index];
                if (lens.source.distTo(other_lens.target) > lens.source_radius + other_lens.target_radius) continue;
                for (other_lens.tmp_visible_sexprs.items) |s| {
                    try lens.tmp_visible_sexprs.append(frame_arena, .{
                        .original_place = s.original_place,
                        .lens_transform = .combine(s.lens_transform, lens.getTransform()),
                    });
                }
            }
        }

        // drawing
        if (drawer) |d| {
            try workspace.draw(platform, d, camera);
        }

        // state changes
        if (platform.keyboard.wasPressed(.KeyZ)) {
            if (workspace.undo_stack.pop()) |command| {
                again: switch (command.specific) {
                    .noop => {},
                    .started_execution => |g| {
                        const executor = &workspace.executors.items[g.executor];
                        executor.input = g.input;
                        executor.garland = g.garland;
                        executor.prev_pills = .fromOwnedSlice(g.prev_pills);
                        executor.old_bindings = .fromOwnedSlice(g.prev_old_bindings);
                        executor.enqueued_stack.clearRetainingCapacity();
                        executor.animation = null;
                        executor.handle.pos = g.prev_pos;
                        const next_cmd = workspace.undo_stack.pop().?;
                        assert(std.meta.activeTag(next_cmd.specific) == .dropped or std.meta.activeTag(next_cmd.specific) == .fnkbox_launch_testcase);
                        continue :again next_cmd.specific;
                    },
                    .fnkbox_launch_testcase => |t| {
                        const fnkbox = &workspace.fnkboxes.items[t.fnkbox];
                        fnkbox.execution = null;
                    },
                    .fnkbox_toggle_fold => |k| {
                        const fnkbox = &workspace.fnkboxes.items[k];
                        fnkbox.folded = !fnkbox.folded;
                    },
                    .fnkbox_scroll => |t| {
                        const fnkbox = &workspace.fnkboxes.items[t.fnkbox];
                        fnkbox.scroll_testcases = t.old_position;
                    },
                    .grabbed => |g| {
                        switch (g.from.kind) {
                            .nothing => unreachable,
                            .executor_handle => |h| {
                                const e = &workspace.executors.items[h];
                                e.handle.pos = g.old_position;
                            },
                            .fnkviewer_handle => |h| {
                                const e = &workspace.fnkviewers.items[h];
                                e.handle.pos = g.old_position;
                            },
                            .fnkbox_handle => |h| {
                                const e = &workspace.fnkboxes.items[h];
                                e.handle.pos = g.old_position;
                            },
                            .lens_handle => |h| {
                                if (g.duplicate) {
                                    _ = workspace.lenses.pop().?;
                                } else {
                                    const lens = &workspace.lenses.items[h.index];
                                    lens.setHandlePos(h.part, g.old_position);
                                    assert(workspace.focus.grabbing.kind.lens_handle.part == h.part);
                                    assert(workspace.focus.grabbing.kind.lens_handle.index == h.index);
                                }
                            },
                            .garland_handle => |h| {
                                if (g.duplicate) {
                                    _ = workspace.garlands.pop().?;
                                } else if (h.local.len == 0 and std.meta.activeTag(h.parent) == .garland) {
                                    const garland = &workspace.garlands.items[h.parent.garland];
                                    garland.handle.pos = g.old_position;
                                } else {
                                    const garland = workspace.garlands.pop().?;
                                    workspace.garlandAt(h).* = garland;
                                }
                            },
                            .case_handle => |h| {
                                if (g.duplicate) {
                                    _ = workspace.cases.pop().?;
                                } else switch (h) {
                                    .board => |k| {
                                        const case = &workspace.cases.items[k];
                                        case.handle.pos = g.old_position;
                                    },
                                    .garland => |t| {
                                        const case = workspace.cases.pop().?;
                                        const garland = workspace.garlandAt(t.parent);
                                        try garland.insertCase(mem.gpa, t.local, case);
                                    },
                                }
                            },
                            .sexpr => |h| {
                                var old = workspace.sexprs.pop().?;
                                if (g.duplicate) {
                                    workspace.sexprAtPlace(h.base).getSubValue(h.local).hovered.value = 10;
                                } else {
                                    old.hovered.value = 10;
                                    old.point = .{ .pos = g.old_position };
                                    old.is_pattern = g.old_ispattern;
                                    try workspace.unpopSexprAt(h, old, &workspace.hover_pool, mem);
                                }
                            },
                        }
                        workspace.focus.grabbing = .nothing;
                    },
                    .dropped => |g| {
                        switch (g.at.kind) {
                            .nothing => unreachable,
                            .lens_handle, .executor_handle, .fnkviewer_handle, .fnkbox_handle => {
                                workspace.focus.grabbing = g.at;
                            },
                            .garland_handle => |h| {
                                const old_k = g.old_grabbed_position.kind.garland_handle.parent.garland;
                                assert(g.old_grabbed_position.kind.garland_handle.local.len == 0);
                                if (h.local.len == 0 and std.meta.activeTag(h.parent) == .garland) {
                                    workspace.focus.grabbing = g.at;
                                } else {
                                    try workspace.garlands.insert(old_k, undefined);
                                    const place = workspace.garlandAt(h);
                                    const garland = place.*;
                                    place.* = g.overwritten_garland;
                                    workspace.garlands.items[old_k] = garland;
                                }
                            },
                            .case_handle => |h| {
                                const old_k = g.old_grabbed_position.kind.case_handle.board;
                                switch (h) {
                                    .board => {
                                        workspace.focus.grabbing = g.at;
                                    },
                                    .garland => |t| {
                                        try workspace.cases.insert(old_k, undefined);
                                        const garland = workspace.garlandAt(t.parent);
                                        const case = garland.popCase(t.local);
                                        workspace.cases.items[old_k] = case;
                                    },
                                }
                            },
                            .sexpr => |h| {
                                if (g.overwritten_sexpr) |overwritten| {
                                    try workspace.sexprs.insert(g.old_grabbed_position.kind.sexpr.base.board, undefined);
                                    const base = workspace.sexprAtPlace(h.base);
                                    var grabbed = base.getSubValue(h.local);
                                    grabbed.point.scale = 1;
                                    workspace.sexprs.items[g.old_grabbed_position.kind.sexpr.base.board] = grabbed;
                                    try base.updateSubValue(
                                        h.local,
                                        overwritten.value,
                                        overwritten.hovered,
                                        mem,
                                        &workspace.hover_pool,
                                    );
                                } else {
                                    assert(h.local.len == 0);
                                    assert(std.meta.eql(g.old_grabbed_position, g.at));
                                }
                                workspace.focus.grabbing = g.old_grabbed_position;
                            },
                        }
                        const next_cmd = workspace.undo_stack.pop().?;
                        assert(std.meta.activeTag(next_cmd.specific) == .grabbed);
                        continue :again next_cmd.specific;
                    },
                }
            }
        }

        const mouse = platform.getMouse(camera);
        const hovered_or_dropzone_thing = try workspace.findHoverableOrDropzoneAtPosition(mouse.cur.position, mem.gpa, frame_arena);

        const hovering: Focus.Target = switch (workspace.focus.grabbing.kind) {
            else => .nothing,
            .nothing => hovered_or_dropzone_thing,
        };
        const dropzone: Focus.Target = switch (workspace.focus.grabbing.kind) {
            else => hovered_or_dropzone_thing,
            .nothing => .nothing,
        };

        const ui_hot = try workspace.findUiAtPosition(mouse.cur.position);

        // cursor
        platform.setCursor(
            if (workspace.focus.grabbing.kind != .nothing)
                .grabbing
            else if (hovering.kind != .nothing)
                .could_grab
            else if (ui_hot.kind != .nothing or workspace.focus.ui_active.kind != .nothing)
                .pointer
            else
                .default,
        );

        // update hover_t for sexprs, garland handles, case handles
        const top_level_things = try workspace.topLevelThings(frame_arena);
        for (top_level_things) |parent_address| {
            const owned_sexprs = try workspace.ownedSexprs(parent_address, frame_arena);
            for (owned_sexprs) |base| {
                const s = workspace.sexprAtPlace(base);
                if (isGrabbedSexpr(base, workspace.focus.grabbing)) {
                    s.is_pattern = switch (dropzone.kind) {
                        .nothing => s.is_pattern,
                        else => unreachable,
                        .sexpr => |x| workspace.sexprAtPlace(x.base).is_pattern,
                    };
                    s.hovered.update(switch (dropzone.kind) {
                        .sexpr => &.{},
                        .nothing => null,
                        else => unreachable,
                    }, 2.0, platform.delta_seconds);
                } else {
                    const hovered = switch (hovering.kind) {
                        else => null,
                        .sexpr => |sexpr| blk: {
                            // special case: no hover anim for base values
                            if (std.meta.activeTag(sexpr.base) == .board and sexpr.local.len == 0) break :blk null;
                            break :blk if (sexpr.base.equals(base)) sexpr.local else null;
                        },
                    };
                    s.hovered.update(hovered, 1.0, platform.delta_seconds);
                }
                s.updateIsPattern(platform.delta_seconds);
            }

            const owned_garlands = try workspace.ownedGarlands(parent_address, frame_arena);
            for (owned_garlands) |base| {
                const g = workspace.garlandAt(base);
                const handle = &g.handle;
                const hovered: f32 = switch (hovered_or_dropzone_thing.kind) {
                    else => 0,
                    .garland_handle => |h| if (workspace.garlandHandleRef(h) == handle) 1 else 0,
                };
                math.lerp_towards(&handle.hot_t, hovered, 0.6, platform.delta_seconds);
            }

            const owned_cases = try workspace.ownedCases(parent_address, frame_arena);
            for (owned_cases) |base| {
                const handle = workspace.caseHandleRef(base);
                const hovered: f32 = switch (hovered_or_dropzone_thing.kind) {
                    else => 0,
                    .case_handle => |h| if (workspace.caseHandleRef(h) == handle) 1 else 0,
                };
                math.lerp_towards(&handle.hot_t, hovered, 0.6, platform.delta_seconds);
            }
        }

        // update ui hotness
        for (workspace.fnkboxes.items, 0..) |*fnkbox, fnkbox_index| {
            fnkbox.updateUiHotness(fnkbox_index, ui_hot, platform.delta_seconds);
        }

        // TODO: reduce duplication?
        // update hover_t for other kinds of handles
        for (workspace.lenses.items, 0..) |*lens, k| {
            const hovered = switch (hovering.kind) {
                else => null,
                .lens_handle => |handle| if (handle.index == k) handle.part else null,
            };
            lens.update(hovered, platform.delta_seconds);
        }
        for (workspace.executors.items, 0..) |*executor, k| {
            const hovered: f32 = switch (hovering.kind) {
                else => 0,
                .executor_handle => |index| if (index == k) 1 else 0,
            };
            executor.handle.update(hovered, platform.delta_seconds);
        }
        for (workspace.fnkviewers.items, 0..) |*thing, k| {
            const hovered: f32 = switch (hovering.kind) {
                else => 0,
                .fnkviewer_handle => |index| if (index == k) 1 else 0,
            };
            thing.handle.update(hovered, platform.delta_seconds);
        }
        for (workspace.fnkboxes.items, 0..) |*thing, k| {
            const hovered: f32 = switch (hovering.kind) {
                else => 0,
                .fnkbox_handle => |index| if (index == k) 1 else 0,
            };
            thing.handle.update(hovered, platform.delta_seconds);
        }

        // apply dragging
        switch (workspace.focus.grabbing.kind) {
            .nothing => {},
            // TODO: would be nice to unify all handle dragging
            .lens_handle => |p| {
                const lens = &workspace.lenses.items[p.index];
                lens.setHandlePos(p.part, mouse.cur.position);
            },
            .executor_handle => |h| {
                workspace.executors.items[h].handle.pos = mouse.cur.position;
            },
            .fnkviewer_handle => |h| {
                workspace.fnkviewers.items[h].handle.pos = mouse.cur.position;
            },
            .fnkbox_handle => |h| {
                workspace.fnkboxes.items[h].handle.pos = mouse.cur.position;
            },
            .case_handle => |p| workspace.caseHandleRef(p).pos = mouse.cur.position,
            .garland_handle => |p| workspace.garlandHandleRef(p).pos = mouse.cur.position,
            .sexpr => |g| {
                assert(g.local.len == 0);
                const grabbed = workspace.sexprAtPlace(g.base);
                const target: Point = switch (dropzone.kind) {
                    .sexpr => |s| ViewHelper.sexprChildView(
                        grabbed.is_pattern,
                        dropzone.lens_transform.actOn(workspace.sexprAtPlace(s.base).point),
                        s.local,
                    ),
                    .nothing => .{
                        .pos = mouse.cur.position,
                        .scale = 1,
                    },
                    else => unreachable,
                };
                grabbed.point.lerp_towards(target, 0.6, platform.delta_seconds);
            },
        }

        // apply ui-like dragging
        switch (workspace.focus.ui_active.kind) {
            else => {},
            .fnkbox_scroll => |t| {
                workspace.fnkboxes.items[t.fnkbox].scroll_testcases += platform.delta_seconds * @as(f32, switch (t.direction) {
                    .up => 1,
                    .down => -1,
                }) / 0.2;
            },
        }

        // update spring positions, and update in general (TODO: maybe separate, or join with hover_t)
        // TODO: remove duplication
        for (workspace.cases.items) |*c| {
            c.update(platform.delta_seconds);
        }

        for (workspace.garlands.items) |*g| {
            g.update(platform.delta_seconds);
        }

        for (workspace.executors.items) |*e| {
            try e.update(mem, workspace.fnkboxes.items, &workspace.hover_pool, platform.delta_seconds);
        }

        for (workspace.fnkviewers.items) |*e| {
            try e.update(mem, workspace.fnkboxes.items, &workspace.hover_pool, platform.delta_seconds);
        }

        for (workspace.fnkboxes.items) |*e| {
            try e.update(mem, workspace.fnkboxes.items, &workspace.hover_pool, platform.delta_seconds);
        }

        const action: UndoableCommand = if (workspace.focus.grabbing.kind == .nothing and (mouse.wasPressed(.left) or mouse.wasPressed(.right)))
            switch (hovering.kind) {
                .nothing => switch (ui_hot.kind) {
                    .fnkbox_toggle_fold => |k| .{ .specific = .{ .fnkbox_toggle_fold = k } },
                    .fnkbox_scroll => |t| blk: {
                        workspace.focus.ui_active = ui_hot;
                        break :blk .{ .specific = .{ .fnkbox_scroll = .{
                            .fnkbox = t.fnkbox,
                            .old_position = workspace.fnkboxes.items[t.fnkbox].scroll_testcases,
                        } } };
                    },
                    .nothing, .fnkbox_launch_testcase => blk: {
                        workspace.focus.ui_active = ui_hot;
                        break :blk .noop;
                    },
                },
                .sexpr => |s| .{
                    .specific = .{
                        .grabbed = .{
                            .from = hovering,
                            .duplicate = mouse.wasPressed(.right) or s.base.immutable(),
                            .old_position = workspace.sexprAtPlace(s.base).point.pos,
                            .old_ispattern = workspace.sexprAtPlace(s.base).is_pattern,
                        },
                    },
                },
                else => .{ .specific = .{
                    .grabbed = .{
                        .from = hovering,
                        .duplicate = mouse.wasPressed(.right),
                        .old_position = workspace.positionOf(hovering),
                        .old_ispattern = undefined,
                    },
                } },
            }
        else if (workspace.focus.grabbing.kind != .nothing and !mouse.cur.isDown(.left) and !mouse.cur.isDown(.right))
            switch (workspace.focus.grabbing.kind) {
                .nothing => unreachable,
                .lens_handle, .executor_handle, .fnkviewer_handle, .fnkbox_handle => .{ .specific = .{
                    .dropped = .{
                        .at = workspace.focus.grabbing,
                        .old_grabbed_position = workspace.focus.grabbing,
                        .overwritten_sexpr = undefined,
                        .overwritten_garland = undefined,
                    },
                } },
                .case_handle => switch (dropzone.kind) {
                    else => unreachable,
                    .nothing => .{ .specific = .{
                        .dropped = .{
                            .at = workspace.focus.grabbing,
                            .old_grabbed_position = workspace.focus.grabbing,
                            .overwritten_sexpr = undefined,
                            .overwritten_garland = undefined,
                        },
                    } },
                    .case_handle => .{ .specific = .{
                        .dropped = .{
                            .at = dropzone,
                            .old_grabbed_position = workspace.focus.grabbing,
                            .overwritten_sexpr = undefined,
                            .overwritten_garland = undefined,
                        },
                    } },
                },
                .garland_handle => .{ .specific = .{
                    .dropped = .{
                        .at = switch (dropzone.kind) {
                            .nothing => workspace.focus.grabbing,
                            .garland_handle => dropzone,
                            else => unreachable,
                        },
                        .old_grabbed_position = workspace.focus.grabbing,
                        .overwritten_sexpr = undefined,
                        .overwritten_garland = switch (dropzone.kind) {
                            .nothing => undefined,
                            .garland_handle => |h| workspace.garlandAt(h).*,
                            else => unreachable,
                        },
                    },
                } },
                .sexpr => .{
                    .specific = .{
                        .dropped = switch (dropzone.kind) {
                            .nothing => .{
                                .at = workspace.focus.grabbing,
                                .old_grabbed_position = workspace.focus.grabbing,
                                .overwritten_sexpr = null,
                                .overwritten_garland = undefined,
                            },
                            .sexpr => |s| .{
                                .at = dropzone,
                                .old_grabbed_position = workspace.focus.grabbing,
                                .overwritten_sexpr = workspace.sexprAtPlace(s.base).getSubValue(s.local),
                                .overwritten_garland = undefined,
                            },
                            else => unreachable,
                        },
                    },
                },
            }
        else if (workspace.focus.ui_active.kind != .nothing and !mouse.cur.isDown(.left)) blk: {
            const ui_active = workspace.focus.ui_active;
            workspace.focus.ui_active = .nothing;
            break :blk if (!ui_hot.equals(ui_active))
                .noop
            else switch (ui_active.kind) {
                .nothing => unreachable,
                .fnkbox_toggle_fold, .fnkbox_scroll => .noop,
                .fnkbox_launch_testcase => |t| .{ .specific = .{ .fnkbox_launch_testcase = .{
                    .fnkbox = t.fnkbox,
                    .testcase = t.testcase,
                } } },
            };
        } else .noop;

        // actually perform the action
        switch (action.specific) {
            .noop => {},
            .started_execution => unreachable,
            .fnkbox_launch_testcase => |t| {
                const fnkbox = &workspace.fnkboxes.items[t.fnkbox];
                const testcase = &fnkbox.testcases.items[t.testcase];
                assert(fnkbox.execution == null);
                testcase.actual = try .empty(testcase.actual.point, &workspace.hover_pool, false);
                fnkbox.execution = .{ .testcase = t.testcase, .executor = .{
                    .input = try testcase.input.dupeSubValue(&.{}, &workspace.hover_pool),
                    .garland = try fnkbox.garland.clone(mem.gpa, &workspace.hover_pool),
                    .handle = .{ .pos = fnkbox.executorPos() },
                    .spawned_by_fnkbox = .{
                        .fnkbox = t.fnkbox,
                        .testcase = t.testcase,
                    },
                    .animation = null,
                } };
            },
            .fnkbox_toggle_fold => |k| {
                const fnkbox = &workspace.fnkboxes.items[k];
                fnkbox.folded = !fnkbox.folded;
            },
            .fnkbox_scroll => {
                // handled in the 'apply ui-like draggin' section
            },
            .grabbed => |g| {
                switch (g.from.kind) {
                    .nothing => unreachable,
                    inline .lens_handle, .executor_handle, .fnkviewer_handle, .fnkbox_handle => |h, t| {
                        if (g.duplicate) {
                            switch (t) {
                                else => std.log.err("TODO", .{}),
                                .lens_handle => {
                                    var lens_pair = workspace.lenses.items[h.index].clone();
                                    lens_pair.source.addInPlace(.one);
                                    lens_pair.target.addInPlace(.one);
                                    try workspace.lenses.append(lens_pair);
                                    // TODO: lens transform?
                                    workspace.focus.grabbing = .{ .kind = .{
                                        .lens_handle = .{
                                            .index = workspace.lenses.items.len - 1,
                                            .part = h.part,
                                        },
                                    }, .lens_transform = .identity };
                                },
                            }
                        } else workspace.focus.grabbing = g.from;
                    },
                    .garland_handle => |h| {
                        if (!g.duplicate and h.local.len == 0 and std.meta.activeTag(h.parent) == .garland) {
                            workspace.focus.grabbing = g.from;
                        } else {
                            const place = workspace.garlandAt(h);
                            if (g.duplicate) {
                                const garland = try place.clone(mem.gpa, &workspace.hover_pool);
                                try workspace.garlands.append(garland);
                            } else {
                                const garland = place.*;
                                try workspace.garlands.append(garland);
                                place.* = .init(garland.handle.pos);
                            }
                            // TODO: lens transform?
                            workspace.focus.grabbing = .{ .kind = .{
                                .garland_handle = .{
                                    .local = &.{},
                                    .parent = .{ .garland = workspace.garlands.items.len - 1 },
                                },
                            }, .lens_transform = .identity };
                        }
                    },
                    .case_handle => |h| {
                        assert(h.exists());
                        switch (h) {
                            .board => |k| {
                                if (g.duplicate) {
                                    const case = try workspace.cases.items[k].clone(mem.gpa, &workspace.hover_pool);
                                    try workspace.cases.append(case);
                                    // TODO: lens transform?
                                    workspace.focus.grabbing = .{ .kind = .{
                                        .case_handle = .{ .board = workspace.cases.items.len - 1 },
                                    } };
                                } else {
                                    workspace.focus.grabbing = g.from;
                                }
                            },
                            .garland => |t| {
                                const garland = workspace.garlandAt(t.parent);
                                const case = if (g.duplicate)
                                    try garland.cases.items[t.local].clone(mem.gpa, &workspace.hover_pool)
                                else
                                    garland.popCase(t.local);
                                try workspace.cases.append(case);
                                // TODO: lens transform?
                                workspace.focus.grabbing = .{ .kind = .{
                                    .case_handle = .{ .board = workspace.cases.items.len - 1 },
                                } };
                            },
                        }
                    },
                    .sexpr => |h| {
                        const original = if (g.duplicate)
                            try workspace.sexprAtPlace(h.base).dupeSubValue(h.local, &workspace.hover_pool)
                        else
                            (try workspace.popSexprAt(h, &workspace.hover_pool, mem)).?;

                        try workspace.sexprs.append(.{
                            .hovered = try original.hovered.clone(&workspace.hover_pool),
                            .value = original.value,
                            .point = original.point,
                            .is_pattern = original.is_pattern,
                            .is_pattern_t = original.is_pattern_t,
                        });

                        const grabbed: usize = workspace.sexprs.items.len - 1;

                        const base = workspace.sexprAtPlace(.{ .board = grabbed });
                        base.point = hovering.lens_transform.actOn(base.point);
                        workspace.focus.grabbing = .{ .kind = .{ .sexpr = .{
                            .base = .{ .board = grabbed },
                            .local = &.{},
                        } } };
                    },
                }
            },
            .dropped => |g| {
                switch (g.at.kind) {
                    .nothing => unreachable,
                    .lens_handle, .executor_handle, .fnkviewer_handle, .fnkbox_handle => workspace.focus.grabbing = .nothing,
                    .garland_handle => |h| {
                        if (h.local.len == 0 and std.meta.activeTag(h.parent) == .garland) {
                            workspace.focus.grabbing = .nothing;
                        } else {
                            const place = workspace.garlandAt(h);
                            const k = g.old_grabbed_position.kind.garland_handle.parent.garland;
                            assert(g.old_grabbed_position.kind.garland_handle.local.len == 0);
                            const garland = workspace.garlands.items[k];
                            place.* = garland;
                            _ = workspace.garlands.orderedRemove(k);
                            workspace.focus.grabbing = .nothing;
                        }
                    },
                    .case_handle => |h| {
                        const old_k = g.old_grabbed_position.kind.case_handle.board;
                        switch (h) {
                            .board => workspace.focus.grabbing = .nothing,
                            .garland => |t| {
                                const garland = garlandAt(workspace, t.parent);
                                try garland.insertCase(mem.gpa, t.local, workspace.cases.items[old_k]);
                                _ = workspace.cases.orderedRemove(old_k);
                                workspace.focus.grabbing = .nothing;
                            },
                        }
                    },
                    .sexpr => |s| {
                        assert(std.meta.activeTag(g.old_grabbed_position.kind.sexpr.base) == .board);
                        assert(g.old_grabbed_position.kind.sexpr.local.len == 0);
                        if (g.overwritten_sexpr != null) {
                            const k = g.old_grabbed_position.kind.sexpr.base.board;
                            const grabbed = workspace.sexprs.items[k];
                            try workspace.sexprAtPlace(s.base).updateSubValue(
                                s.local,
                                grabbed.value,
                                grabbed.hovered,
                                mem,
                                &workspace.hover_pool,
                            );
                            _ = workspace.sexprs.orderedRemove(k);
                        }

                        workspace.focus.grabbing = .nothing;
                    },
                }
            },
        }
        if (action.specific != .noop) {
            try workspace.undo_stack.append(action);
        }

        for (workspace.executors.items, 0..) |e, k| {
            if (e.startedExecution()) {
                assert(e.enqueued_stack.items.len == 0);
                try workspace.undo_stack.append(.{ .specific = .{ .started_execution = .{
                    .executor = k,
                    .input = e.input,
                    .garland = try e.garland.clone(mem.gpa, &workspace.hover_pool),
                    .prev_pills = (try e.prev_pills.clone(mem.gpa)).items,
                    .prev_old_bindings = (try e.old_bindings.clone(mem.gpa)).items,
                    .prev_pos = e.handle.pos,
                } } });
            }
        }
    }

    fn positionOf(workspace: *Workspace, thing: Focus.Target) Vec2 {
        return switch (thing.kind) {
            .nothing => unreachable,
            .lens_handle => |h| workspace.lenses.items[h.index].handlePos(h.part),
            .executor_handle => |h| workspace.executors.items[h].handle.pos,
            .fnkviewer_handle => |h| workspace.fnkviewers.items[h].handle.pos,
            .fnkbox_handle => |h| workspace.fnkboxes.items[h].handle.pos,
            .case_handle => |h| workspace.caseHandleRef(h).pos,
            .garland_handle => |k| workspace.garlandHandleRef(k).pos,
            .sexpr => |s| workspace.sexprAtPlace(s.base).point.pos,
        };
    }

    fn isGrabbedSexpr(thing: BaseSexprPlace, grabbed: Focus.Target) bool {
        return grabbed.kind.equals(.{ .sexpr = .{
            .local = &.{},
            .base = thing,
        } });
    }

    fn isGrabbedGarland(thing: GarlandHandle, grabbed: Focus.Target) bool {
        return grabbed.kind.equals(.{ .garland_handle = thing });
    }

    fn isGrabbedCase(thing: CaseHandle, grabbed: Focus.Target) bool {
        return grabbed.kind.equals(.{ .case_handle = thing });
    }
};

const ThreadInitialParams = struct {
    value: *const Sexpr,
    fn_name: *const Sexpr,

    pub fn initFromText(
        input_raw: []const u8,
        fn_name_raw: []const u8,
        scoring_run: *core.ScoringRun,
    ) !@This() {
        var permanent_stuff = scoring_run.mem;
        const fn_name = try parsing.parseSingleSexpr(fn_name_raw, &permanent_stuff.pool_for_sexprs);
        const input = try parsing.parseSingleSexpr(input_raw, &permanent_stuff.pool_for_sexprs);
        return .{ .value = input, .fn_name = fn_name };
    }

    pub fn toThread(self: @This(), scoring_run: *core.ScoringRun) !core.ExecutionThread {
        return try .init(self.value, self.fn_name, scoring_run);
    }

    pub fn startThreadAndRunItToStep(self: @This(), scoring_run: *core.ScoringRun, step: usize) !core.ExecutionThread {
        var thread = try self.toThread(scoring_run);
        for (0..step) |_| {
            std.debug.assert(null == try thread.advanceTinyStep(scoring_run));
        }
        return thread;
    }
};

const ExecutionTree = struct {
    incoming_bindings: []const core.Binding,
    new_bindings: []const core.Binding,
    all_bindings: []const core.Binding,
    current_fn_name: *const Sexpr,
    input: *const Sexpr,
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

    fn getLast(self: ExecutionTree) *const Sexpr {
        const matched = self.matched;
        if (matched.next) |next| {
            return next.getLast();
        } else if (matched.funk_tangent) |fnk| {
            return fnk.tree.getLast();
        } else return matched.filled_template;
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
                    .incoming_bindings = incoming_bindings,
                    .new_bindings = try new_bindings.toOwnedSlice(),
                    .input = input,
                    .cases = cases,
                    .matched_index = case_index,
                    // .matched = if (funk_tangent == null and next_tree == null) null else .{
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
        } else @panic("nope"); // else return .{ .all_bindings = incoming_bindings, .incoming_bindings = incoming_bindings, .input = input, .matched = null };
    }

    pub fn buildFromText(scoring_run: *core.ScoringRun, fn_name_raw: []const u8, input_raw: []const u8) !ExecutionTree {
        var permanent_stuff = scoring_run.mem;
        const fn_name = try parsing.parseSingleSexpr(fn_name_raw, &permanent_stuff.pool_for_sexprs);
        const input = try parsing.parseSingleSexpr(input_raw, &permanent_stuff.pool_for_sexprs);
        return try .buildNewStack(scoring_run, fn_name, input);
    }

    pub fn draw(self: ExecutionTree, drawer: *Drawer, camera: Rect, input_point: Point) !void {
        try drawer.drawSexpr(camera, .{
            .is_pattern = 0,
            .pos = input_point,
            .value = self.input,
        });

        const matched = self.matched;
        try drawer.drawSexpr(camera, .{
            .is_pattern = 1,
            .pos = input_point.applyToLocalPoint(.{ .pos = .new(3, 0) }),
            .value = matched.pattern,
        });

        // try drawer.drawSexpr(camera, .{
        //     .is_pattern = 0,
        //     .pos = input_point.applyToLocalPoint(.{ .pos = .new(5, 0) }),
        //     .value = matched.template,
        // });
        if (matched.funk_tangent) |funk_tangent| {
            // try drawer.drawHoldedFnk(camera, input_point.applyToLocalPoint(.{ .pos = .new(5, 0) }), 0, funk_tangent.fn_name);
            // if (matched.next == null) {
            //     try drawer.drawSexpr(camera, .{
            //         .is_pattern = 0,
            //         .pos = input_point.applyToLocalPoint(.{ .pos = .new(5, -1), .turns = -1.0 / 6.0 }),
            //         .value = funk_tangent.tree.getLast(),
            //     });
            // }
            // if (matched.next) |next| {
            try funk_tangent.tree.draw(drawer, camera, input_point.applyToLocalPoint(.{ .pos = .new(5, 1), .turns = 0.1 }));
            // try next.draw(drawer, camera, input_point.applyToLocalPoint(.{ .pos = .new(5, -1), .turns = -1.0 / 6.0 }));
            // } else {
            //     try funk_tangent.tree.draw(drawer, camera, input_point.applyToLocalPoint(.{ .pos = .new(5, 0), .turns = -0.1 }));
            // }
            // } else {
            // if (matched.next) |next| {
            //     try next.draw(drawer, camera, input_point.applyToLocalPoint(.{ .pos = .new(5, -1), .turns = -1.0 / 6.0 }));
            // }
        }
        if (matched.next) |next| {
            try next.draw(drawer, camera, input_point.applyToLocalPoint(.{ .pos = .new(5, -1), .turns = -1.0 / 6.0 }));
        }

        try drawer.drawTemplateSexprWithBindings(
            camera,
            input_point.applyToLocalPoint(.{ .pos = .new(5, 1), .turns = 0.1 }),
            matched.raw_template,
            .{
                .anim_t = 0.5,
                .old = &.{},
                // .new = &.{},
                .new = self.all_bindings,
                // .old = self.bindings,
            },
        );
    }

    fn depth(self: ExecutionTree) usize {
        const matched = self.matched;
        const a = if (matched.funk_tangent) |f| f.tree.depth() else 0;
        const b = if (matched.next) |n| n.depth() else 0;
        return 1 + a + b;
    }

    pub fn drawAsThread(self: ExecutionTree, drawer: *Drawer, camera: Rect, input_point: Point, t: f32) !void {
        try drawer.drawSexpr(camera, .{
            .is_pattern = 0,
            .pos = input_point,
            .value = self.input,
        });

        const step_n: usize = @intFromFloat(@floor(t));
        const anim_t: f32 = @mod(t, 1);
        if (step_n < self.matched_index) {
            const discarded_case = self.cases[step_n];

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);
            const old_bindings: BindingsState = .{
                .anim_t = null,
                .new = &.{},
                .old = self.incoming_bindings,
            };

            const template_point = input_point;

            const fly_away = math.remapClamped(anim_t, 0.2, 0.8, 0, 1);
            const pattern_point_floating_away = template_point.applyToLocalPoint(Point.lerp(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
                .{ .pos = .new(8, -8), .scale = 0, .turns = -0.65 },
                fly_away,
            ));
            try drawCase(drawer, camera, pattern_point_floating_away, discarded_case, old_bindings, 1, null);

            const offset = math.remapClamped(anim_t, 0.2, 1, 1, 0);
            for (self.cases[step_n + 1 ..], 0..) |case, k| {
                try drawCase(drawer, camera, template_point.applyToLocalPoint(.{
                    .pos = .new(4, (tof32(k) + offset) * 3),
                }), case, old_bindings, if (k > 0) 0 else 1.0 - offset, null);
            }

            return;
        } else if (step_n == self.matched_index) {
            const matched_case = self.cases[step_n];

            try drawer.drawSexpr(camera, .{
                .is_pattern = 0,
                .value = self.input,
                .pos = input_point,
            });

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);
            const t_bindings: ?f32 = if (anim_t < 0.2) null else math.remapTo01Clamped(anim_t, 0.2, 0.8);

            const pattern_point = input_point.applyToLocalPoint(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
            );

            var next_cases_pattern_point = input_point.applyToLocalPoint(.{ .pos = .new(16, 0) });
            try drawCase(drawer, camera, pattern_point, matched_case, .{
                .anim_t = t_bindings,
                .new = self.new_bindings,
                .old = self.incoming_bindings,
            }, 1, if (self.matched.funk_tangent) |funk_tangent| .{
                .t = math.remapTo01Clamped(anim_t, 0.2, 1.0),
                .cases = funk_tangent.tree.cases,
                .next_cases_pattern_point_ptr = &next_cases_pattern_point,
                // .next_cases_pattern_point_ptr = undefined,
            } else null);

            return;
        } else {}

        const matched = self.matched;

        try drawer.drawSexpr(camera, .{
            .is_pattern = 1,
            .pos = input_point.applyToLocalPoint(.{ .pos = .new(3, 0) }),
            .value = matched.pattern,
        });

        try drawer.drawSexpr(camera, .{
            .is_pattern = 0,
            .pos = input_point.applyToLocalPoint(.{ .pos = .new(5, 0) }),
            .value = matched.filled_template,
        });

        var next_input_pos = input_point.applyToLocalPoint(.{ .pos = .new(5, 0) });

        if (matched.funk_tangent) |funk_tangent| {
            try drawer.drawHoldedFnk(
                camera,
                input_point.applyToLocalPoint(.{
                    .pos = .new(7, -1),
                    .turns = -0.25,
                    .scale = 0.5,
                }),
                0,
                funk_tangent.fn_name,
            );

            try funk_tangent.tree.drawAsThread(drawer, camera, next_input_pos, t);

            next_input_pos = input_point.applyToLocalPoint(.{ .pos = .new(5 + 5 * tof32(funk_tangent.tree.depth()), 0) });
        }

        if (matched.next) |next| {
            try next.drawAsThread(drawer, camera, next_input_pos, t);
        }
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

    pub const drawAsExecutingThread = drawAsExecutingThreadNew;

    pub fn drawAsExecutingThreadNew(original_tree: ExecutionTree, drawer: *Drawer, camera: Rect, original_input_point: Point, original_t: f32) !void {
        var shapes: DrawList = .init(drawer.canvas.frame_arena.allocator());

        var active = original_tree;
        var queued: std.ArrayList(struct {
            tree: *const ExecutionTree,
            // TODO: reduce
            bindings_stuff: ExecutionTree,
        }) = .init(drawer.canvas.frame_arena.allocator());

        var displacement: f32 = 0;
        var remaining_t = original_t;
        var input_point = original_input_point;

        try shapes.append(.{ .physical = .{
            .is_pattern = 0,
            .pos = input_point,
            .value = active.input,
        } });

        const any_left: bool = blk: while (@floor(remaining_t) > tof32(active.matched_index)) {
            try shapes.append(.{ .physical = .{
                .is_pattern = 1,
                .pos = input_point.applyToLocalPoint(.{ .pos = .new(3, 0) }),
                .value = active.matched.pattern,
            } });
            try shapes.append(.{ .templated = .{
                .point = input_point.applyToLocalPoint(.{ .pos = .new(5, 0) }),
                .template = active.matched.raw_template,
                .bindings = .{
                    .anim_t = 1,
                    .old = active.incoming_bindings,
                    .new = active.new_bindings,
                },
            } });
            // const function_point = template_point.applyToLocalPoint(.{
            //     .pos = .new(3, 0),
            //     .turns = -0.25,
            //     .scale = 0.5,
            // }).applyToLocalPoint(.{ .pos = .new(4 * invoking_t, 0) });
            remaining_t -= tof32(active.matched_index + 1);
            displacement += 1;
            input_point = input_point.applyToLocalPoint(.{ .pos = .new(5, 0) });
            // std.log.err("remaining t now: {d}", .{remaining_t});
            if (active.matched.funk_tangent) |funk_tangent| {
                if (active.matched.next) |next| {
                    try queued.append(.{ .tree = next, .bindings_stuff = active });
                }
                try shapes.append(.{ .fnk_name = .{
                    .value = funk_tangent.fn_name,
                    .point = input_point.applyToLocalPoint(.{
                        .pos = .new(3, 0),
                        .turns = -0.25,
                        .scale = 0.5,
                    }).applyToLocalPoint(.{ .pos = .new(4, 0) }),
                } });
                active = funk_tangent.tree.*;
            } else if (active.matched.next) |next| {
                active = next.*;
            } else if (queued.pop()) |q| {
                active = q.tree.*;
            } else break :blk false;
        } else true;

        var queued_extra_offset: f32 = 0;
        var last_displacement: f32 = 0;
        if (any_left) {
            const anim_t = @mod(remaining_t, 1);
            const moving_case = active.cases[@intFromFloat(@floor(remaining_t))];
            const rest_of_cases = active.cases[@as(usize, @intFromFloat(@floor(remaining_t))) + 1 ..];

            if (@floor(remaining_t) < tof32(active.matched_index)) {
                const match_t = math.remapClamped(anim_t, 0, 0.2, 0, 1);
                const flyaway_t = math.remapClamped(anim_t, 0.2, 0.8, 0, 1);
                const offset_t = math.remapClamped(anim_t, 0.2, 0.8, 0, 1);

                const old_bindings: BindingsState = .{
                    .anim_t = null,
                    .new = &.{},
                    .old = active.incoming_bindings,
                };

                const pattern_point_floating_away = input_point
                    .applyToLocalPoint(Point.lerp(
                    .{ .pos = .new(4.0 - match_t, 0) },
                    .{ .pos = .new(10, -2), .scale = 0, .turns = -0.2 },
                    flyaway_t,
                ));
                try shapes.append(.{ .case = .{
                    .pattern_point = pattern_point_floating_away,
                    .case = moving_case,
                    .bindings = old_bindings,
                } });

                for (rest_of_cases, 0..) |case, k| {
                    try shapes.append(.{ .case = .{
                        .pattern_point = input_point.applyToLocalPoint(.{
                            .pos = .new(4, (tof32(k + 1) - offset_t) * 3),
                        }),
                        .case = case,
                        .bindings = old_bindings,
                    } });
                }
            } else {
                assert(@floor(remaining_t) == tof32(active.matched_index));
                last_displacement = math.remapClamped(anim_t, 0.2, 1, 0, 1);
                defer displacement += last_displacement;

                const match_t = math.remapClamped(anim_t, 0, 0.2, 0, 1);
                const bindings_t: ?f32 = if (anim_t < 0.2) null else math.remapTo01Clamped(anim_t, 0.2, 0.8);
                const template_t = math.remapClamped(anim_t, 0.2, 1.0, 0, 1);
                const invoking_t = math.remapClamped(anim_t, 0.0, 0.7, 0, 1);
                const enqueueing_t = last_displacement;
                const discarded_t = anim_t;

                const pattern_point = input_point.applyToLocalPoint(
                    .{ .pos = .new(4.0 - match_t, 0) },
                );

                try shapes.append(.{ .physical = .{
                    .is_pattern = 1,
                    .pos = pattern_point,
                    .value = moving_case.pattern,
                } });

                const template_point = pattern_point.applyToLocalPoint(.{ .pos = .new(2, 0) });

                try shapes.append(.{ .templated = .{
                    .point = template_point,
                    .template = moving_case.template,
                    .bindings = .{
                        .anim_t = bindings_t,
                        .old = active.incoming_bindings,
                        .new = active.new_bindings,
                    },
                } });

                const old_bindings: BindingsState = .{
                    .anim_t = null,
                    .new = &.{},
                    .old = active.incoming_bindings,
                };
                for (rest_of_cases, 0..) |case, k| {
                    try shapes.append(.{ .case = .{
                        .pattern_point = input_point
                            .applyToLocalPoint(.lerp(.{}, .{ .turns = 0.2, .scale = 0, .pos = .new(-4, 8) }, discarded_t))
                            .applyToLocalPoint(.{
                            .pos = .new(4, tof32(k + 1) * 3),
                        }),
                        .case = case,
                        .bindings = old_bindings,
                    } });
                }

                if (active.matched.funk_tangent) |funk_tangent| {
                    const function_point = template_point.applyToLocalPoint(.{
                        .pos = .new(3, 0),
                        .turns = -0.25,
                        .scale = 0.5,
                    }).applyToLocalPoint(.{ .pos = .new(4 * invoking_t, 0) });

                    try shapes.append(.{ .fnk_name = .{
                        .point = function_point,
                        .value = funk_tangent.fn_name,
                    } });

                    const offset = (1.0 - invoking_t) + 2.0 * math.smoothstepEased(invoking_t, 0.4, 0.0, .linear);
                    for (funk_tangent.tree.cases, 0..) |next_case, k| {
                        try shapes.append(.{ .case = .{
                            .pattern_point = template_point.applyToLocalPoint(
                                .{ .pos = .new(4, 3 * (offset + tof32(k))) },
                            ),
                            .case = next_case,
                            .bindings = .none,
                        } });
                    }

                    if (active.matched.next) |next| {
                        queued_extra_offset += enqueueing_t;
                        const next_pattern_point = template_point
                            .applyToLocalPoint(.{ .pos = .new(6, 0) })
                            .applyToLocalPoint(.{ .pos = .new(6 * template_t, 0) })
                            .applyToLocalPoint(.{ .pos = .new(0, -2 * enqueueing_t) })
                            .rotateAroundLocalPosition(.new(-1, -1), math.lerp(
                            0,
                            -0.1,
                            math.smoothstepEased(enqueueing_t, 0, 1, .easeInOutCubic),
                        ));
                        try shapes.append(.{ .case = .{
                            .pattern_point = next_pattern_point,
                            .case = next.cases[0],
                            .bindings = .{
                                .anim_t = bindings_t,
                                .old = active.incoming_bindings,
                                .new = active.new_bindings,
                            },
                        } });
                    }
                } else if (active.matched.next) |next| {
                    const next_pattern_point = template_point
                        .applyToLocalPoint(.{ .pos = .new(6, 0) })
                        .applyToLocalPoint(.{ .pos = .new(-2 * template_t, 0) });

                    for (next.cases, 0..) |next_case, k| {
                        try shapes.append(.{ .case = .{
                            .pattern_point = next_pattern_point.applyToLocalPoint(
                                .{ .pos = .new(0, 3 * tof32(k)) },
                            ),
                            .case = next_case,
                            .bindings = .{
                                .anim_t = bindings_t,
                                .old = active.incoming_bindings,
                                .new = active.new_bindings,
                            },
                        } });
                    }
                } else {
                    queued_extra_offset -= enqueueing_t;
                }
            }
        }

        for (0..queued.items.len) |k| {
            const next = queued.items[queued.items.len - k - 1];
            const next_pattern_point = if (k == 0 and queued_extra_offset < 0)
                input_point
                    .applyToLocalPoint(.{ .pos = .new(6, 0) })
                    .applyToLocalPoint(.{ .pos = .new(6, 0) })
                    // .applyToLocalPoint(.{ .pos = .new(5 * last_displacement, 0) })
                    .applyToLocalPoint(.{ .pos = .new(-3 * math.remapClamped(queued_extra_offset, 0, -1, 0, 1), 0) })
                    .applyToLocalPoint(.{ .pos = .new(0, -2 * math.remapClamped(queued_extra_offset, 0, -1, 1, 0)) })
                    .rotateAroundLocalPosition(.new(-1, -1), math.lerp(
                    0,
                    -0.1,
                    math.smoothstepEased(queued_extra_offset, -1, 0, .easeInOutCubic),
                ))
            else
                input_point
                    .applyToLocalPoint(.{ .pos = .new(6, 0) })
                    .applyToLocalPoint(.{ .pos = .new(6, 0) })
                    .applyToLocalPoint(.{ .pos = .new(5 * last_displacement, 0) })
                    .applyToLocalPoint(.{ .pos = .new(5 * tof32(k), 0) })
                    .applyToLocalPoint(.{ .pos = .new(5 * queued_extra_offset, 0) })
                    .applyToLocalPoint(.{ .pos = .new(0, -2) })
                    .rotateAroundLocalPosition(.new(-1, -1), -0.1);
            try shapes.append(.{ .case = .{
                .pattern_point = next_pattern_point,
                .case = next.tree.cases[0],
                .bindings = .{
                    .anim_t = 1,
                    .old = next.bindings_stuff.incoming_bindings,
                    .new = next.bindings_stuff.new_bindings,
                },
            } });
        }

        for (shapes.items) |s| {
            try s.draw(drawer, camera.move(.new(displacement * 5, 0)));
        }
    }

    pub fn drawAsExecutingThreadOld(self: ExecutionTree, drawer: *Drawer, camera: Rect, input_point: Point, t: f32) !void {
        var shapes: DrawList = .init(drawer.canvas.frame_arena.allocator());
        const asdf = try self.drawAsExecutingThreadInternal(input_point, t, &shapes);
        for (shapes.items) |s| {
            try s.draw(drawer, camera.move(.new(asdf.displacement * 5, 0)));
        }
    }

    pub fn drawAsExecutingThreadInternal(self: ExecutionTree, input_point: Point, t: f32, out: *DrawList) !struct {
        queued_nexts: f32,
        displacement: f32,
        state: union(enum) {
            fully_consumed,
            right_before_exiting: f32,
            exited: struct {
                remaining_t: f32,
            },
        },
    } {
        if (t < 0) std.log.err("t was: {d}", .{t});
        const step_n: usize = @intFromFloat(@floor(t));
        const anim_t: f32 = @mod(t, 1);
        if (step_n < self.matched_index) {
            try out.append(.{ .physical = .{
                .is_pattern = 0,
                .pos = input_point,
                .value = self.input,
            } });

            const discarded_case = self.cases[step_n];

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);
            const old_bindings: BindingsState = .{
                .anim_t = null,
                .new = &.{},
                .old = self.incoming_bindings,
            };

            const template_point = input_point;

            const fly_away = math.remapClamped(anim_t, 0.2, 0.8, 0, 1);
            const pattern_point_floating_away = template_point.applyToLocalPoint(Point.lerp(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
                .{ .pos = .new(8, -8), .scale = 0, .turns = -0.65 },
                fly_away,
            ));
            try out.append(.{ .case = .{ .pattern_point = pattern_point_floating_away, .case = discarded_case, .bindings = old_bindings } });

            const offset = math.remapClamped(anim_t, 0.2, 1, 1, 0);
            for (self.cases[step_n + 1 ..], 0..) |case, k| {
                try out.append(.{ .case = .{
                    .pattern_point = template_point.applyToLocalPoint(.{
                        .pos = .new(4, (tof32(k) + offset) * 3),
                    }),
                    .case = case,
                    .bindings = old_bindings,
                } });
            }

            return .{ .state = .fully_consumed, .queued_nexts = 0, .displacement = 0 };
        } else if (step_n == self.matched_index) {
            const matched_case = self.cases[step_n];

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);
            const t_bindings: ?f32 = if (anim_t < 0.2) null else math.remapTo01Clamped(anim_t, 0.2, 0.8);
            const template_t = math.remapClamped(anim_t, 0.2, 1.0, 0, 1);
            const invoking_t = math.remapClamped(anim_t, 0.0, 0.7, 0, 1);
            const hiding_next_t = anim_t;

            try out.append(.{ .physical = .{
                .is_pattern = 0,
                .pos = input_point,
                .value = self.input,
            } });

            const pattern_point = input_point.applyToLocalPoint(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
            );

            try out.append(.{ .physical = .{
                .is_pattern = 1,
                .pos = pattern_point,
                .value = matched_case.pattern,
            } });

            const template_point = pattern_point.applyToLocalPoint(.{ .pos = .new(2, 0) });

            try out.append(.{ .templated = .{
                .point = template_point,
                .template = matched_case.template,
                .bindings = .{
                    .anim_t = t_bindings,
                    .old = self.incoming_bindings,
                    .new = self.new_bindings,
                },
            } });

            const is_last_match = (self.matched.funk_tangent == null and self.matched.next == null);

            if (self.matched.funk_tangent) |funk_tangent| {
                const function_point = template_point.applyToLocalPoint(.{
                    .pos = .new(3, 0),
                    .turns = -0.25,
                    .scale = 0.5,
                }).applyToLocalPoint(.{ .pos = .new(4 * invoking_t, 0) });

                try out.append(.{ .fnk_name = .{
                    .point = function_point,
                    .value = funk_tangent.fn_name,
                } });

                const offset = (1.0 - invoking_t) + 2.0 * math.smoothstepEased(invoking_t, 0.4, 0.0, .linear);
                for (funk_tangent.tree.cases, 0..) |next_case, k| {
                    try out.append(.{ .case = .{
                        .pattern_point = template_point.applyToLocalPoint(
                            .{ .pos = .new(4, 3 * (offset + tof32(k))) },
                        ),
                        .case = next_case,
                        .bindings = .none,
                    } });
                }

                if (self.matched.next) |next| {
                    const next_pattern_point = template_point
                        .applyToLocalPoint(.{ .pos = .new(6, 0) })
                        .applyToLocalPoint(.{ .pos = .new(6 * template_t, 0) })
                        .applyToLocalPoint(.{ .pos = .new(0, -2 * hiding_next_t) })
                        .rotateAroundLocalPosition(.new(-1, -1), math.lerp(
                        0,
                        -0.1,
                        math.smoothstepEased(hiding_next_t, 0, 1, .easeInOutCubic),
                    ));
                    try out.append(.{ .case = .{
                        .pattern_point = next_pattern_point,
                        .case = next.cases[0],
                        .bindings = .{
                            .anim_t = t_bindings,
                            .old = self.incoming_bindings,
                            .new = self.new_bindings,
                        },
                    } });
                }
            }

            if (is_last_match) {
                return .{
                    .state = .{ .right_before_exiting = anim_t },
                    .queued_nexts = 0,
                    .displacement = 0,
                };
            } else {
                return .{
                    .state = .fully_consumed,
                    .queued_nexts = if (self.matched.next == null) 0 else anim_t,
                    .displacement = if (self.matched.funk_tangent == null) 0 else anim_t,
                };
            }
        } else {
            if (true) {
                const matched_case = self.cases[self.matched_index];

                const match_dist = 0;
                const t_bindings: ?f32 = 1;

                try out.append(.{ .physical = .{
                    .is_pattern = 0,
                    .pos = input_point,
                    .value = self.input,
                } });

                const pattern_point = input_point.applyToLocalPoint(
                    .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
                );

                try out.append(.{ .physical = .{
                    .is_pattern = 1,
                    .pos = pattern_point,
                    .value = matched_case.pattern,
                } });

                const template_point = pattern_point.applyToLocalPoint(.{ .pos = .new(2, 0) });

                try out.append(.{ .templated = .{
                    .point = template_point,
                    .template = matched_case.template,
                    .bindings = .{
                        .anim_t = t_bindings,
                        .old = self.incoming_bindings,
                        .new = self.new_bindings,
                    },
                } });

                const function_point = template_point.applyToLocalPoint(.{
                    .pos = .new(3, -2),
                    .turns = -0.25,
                    .scale = 0.5,
                });

                try out.append(.{ .fnk_name = .{
                    .point = function_point,
                    .value = matched_case.fnk_name,
                } });
            }

            const asdf: funk.WithoutError(funk.ReturnOf(drawAsExecutingThreadInternal)) = if (self.matched.funk_tangent) |funk_tangent|
                try funk_tangent.tree.drawAsExecutingThreadInternal(input_point.applyToLocalPoint(.{ .pos = .new(5, 0) }), t - tof32(self.matched_index) - 1, out)
            else
                .{ .state = .{ .exited = .{ .remaining_t = t - tof32(self.matched_index) - 1 } }, .queued_nexts = 0, .displacement = 0 };

            switch (asdf.state) {
                .fully_consumed => {
                    // std.log.debug("hola, {d}", .{asdf.queued_nexts});
                    if (self.matched.next) |next| {
                        // std.log.debug("qud nextss: {d}", .{asdf.queued_nexts});
                        const hiding_next_t = 1;
                        const next_pattern_point = input_point
                            .applyToLocalPoint(.{ .pos = .new(5, 0) })
                            .applyToLocalPoint(.{ .pos = .new(12, 0) })
                            .applyToLocalPoint(.{ .pos = .new(5 * asdf.queued_nexts, 0) })
                            // .applyToLocalPoint(.{ .pos = .new(11 * asdf.queued_nexts, 0) })
                            // .applyToLocalPoint(.{ .pos = .new(11 * (asdf.queued_nexts + tof32(asdf.displacement)), 0) })
                            .applyToLocalPoint(.{ .pos = .new(5 * tof32(asdf.displacement), 0) })
                            .applyToLocalPoint(.{ .pos = .new(0, hiding_next_t * -2) })
                            .rotateAroundLocalPosition(.new(-1, -1), math.lerp(
                            0,
                            -0.1,
                            math.smoothstepEased(hiding_next_t, 0, 1, .easeInOutCubic),
                        ));
                        try out.append(.{ .case = .{ .pattern_point = next_pattern_point, .case = next.cases[0], .bindings = .{
                            .anim_t = 1,
                            .old = self.incoming_bindings,
                            .new = self.new_bindings,
                        } } });
                    }

                    return .{
                        .displacement = 1 + asdf.displacement,
                        .state = .fully_consumed,
                        // .queued_nexts = asdf.queued_nexts,
                        // .queued_nexts = 0,
                        .queued_nexts = asdf.queued_nexts + @as(f32, if (self.matched.next == null) 0 else 1),
                    };
                    // return .{ .state = .fully_consumed, .queued_nexts = 0 };
                },
                .right_before_exiting => |exit_t| {
                    // std.log.debug("hola, {d}, {any}", .{ exit_t, self.matched.next == null });
                    if (self.matched.next) |next| {
                        const hiding_next_t = 1.0 - exit_t;
                        const next_pattern_point = input_point
                            .applyToLocalPoint(.{ .pos = .new(17, 0) })
                            .applyToLocalPoint(.{ .pos = .new(5 * tof32(asdf.displacement), 0) })
                            .applyToLocalPoint(.{ .pos = .new(-3 * (1 - hiding_next_t), 0) })
                            // .applyToLocalPoint(.{ .pos = .new(-8 * (1 - hiding_next_t), 0) })
                            // .applyToLocalPoint(.{ .pos = .new(-3 * (1 - hiding_next_t), hiding_next_t * -2) })
                            .applyToLocalPoint(.{ .pos = .new(0, hiding_next_t * -2) })
                            .rotateAroundLocalPosition(.new(-1, -1), math.lerp(
                            0,
                            -0.1,
                            math.smoothstepEased(hiding_next_t, 0, 1, .easeInOutCubic),
                        ));
                        try out.append(.{ .case = .{ .pattern_point = next_pattern_point, .case = next.cases[0], .bindings = .{
                            .anim_t = 1,
                            .old = self.incoming_bindings,
                            .new = self.new_bindings,
                        } } });

                        return .{
                            .displacement = 1 + asdf.displacement + exit_t,
                            .state = .fully_consumed,
                            .queued_nexts = asdf.queued_nexts + 1 - exit_t,
                        };
                    } else {
                        return .{
                            .displacement = 1 + asdf.displacement,
                            .state = .{ .right_before_exiting = exit_t },
                            .queued_nexts = asdf.queued_nexts,
                        };
                    }
                },
                .exited => |data| {
                    // std.log.debug("line 498, data.remainting is {d}, next is null? {any}", .{ data.remaining_t, self.matched.next == null });
                    if (self.matched.next) |next| {
                        const asdf2 = try next.drawAsExecutingThreadInternal(
                            input_point
                                .applyToLocalPoint(.{ .pos = .new(5, 0) })
                                .applyToLocalPoint(.{ .pos = .new(5 * tof32(asdf.displacement), 0) }),
                            data.remaining_t,
                            out,
                        );
                        switch (asdf2.state) {
                            // .fully_consumed, .right_before_exiting => return .{ .state = .fully_consumed },
                            .fully_consumed, .right_before_exiting => return .{
                                .displacement = asdf.displacement + asdf2.displacement + 1,
                                .state = asdf2.state,
                                // .queued_nexts = asdf.queued_nexts + asdf2.queued_nexts + 1,
                                // .queued_nexts = 0,
                                .queued_nexts = asdf2.queued_nexts,
                                // .queued_nexts = asdf2.queued_nexts + 1,
                            },
                            .exited => |data2| {
                                // std.log.debug("line 509, data2.remainting is {d}", .{data2.remaining_t});
                                return .{
                                    .displacement = asdf.displacement + asdf2.displacement + 1,
                                    .state = .{ .exited = .{
                                        .remaining_t = data2.remaining_t,
                                    } },
                                    // .queued_nexts = 0,
                                    // .queued_nexts = 0 * asdf.queued_nexts + asdf2.queued_nexts + 1,
                                    .queued_nexts = asdf2.queued_nexts,
                                };
                            },
                        }
                    } else {
                        return .{
                            .displacement = asdf.displacement + 1,
                            .state = .{
                                .exited = .{
                                    .remaining_t = data.remaining_t,
                                },
                            },
                            .queued_nexts = asdf.queued_nexts + @as(f32, if (self.matched.next == null) 0 else 1),
                        };
                    }
                },
            }
        }
    }

    pub fn drawAsThreadWithFolding(self: ExecutionTree, drawer: *Drawer, camera: Rect, input_point: Point, t: f32) !union(enum) {
        fully_consumed,
        right_before_exiting: f32,
        exited: struct {
            remaining_t: f32,
            // next_input_pos: Point,
        },
    } {
        if (false) try drawer.drawSexpr(camera, .{
            .is_pattern = 0,
            .pos = input_point,
            .value = self.input,
        });

        const step_n: usize = @intFromFloat(@floor(t));
        const anim_t: f32 = @mod(t, 1);
        if (step_n < self.matched_index) {
            const discarded_case = self.cases[step_n];

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);
            const old_bindings: BindingsState = .{
                .anim_t = null,
                .new = &.{},
                .old = self.incoming_bindings,
            };

            const template_point = input_point;

            const fly_away = math.remapClamped(anim_t, 0.2, 0.8, 0, 1);
            const pattern_point_floating_away = template_point.applyToLocalPoint(Point.lerp(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
                .{ .pos = .new(8, -8), .scale = 0, .turns = -0.65 },
                fly_away,
            ));
            try drawCaseForFolding(drawer, camera, pattern_point_floating_away, discarded_case, old_bindings, 1, null);

            const offset = math.remapClamped(anim_t, 0.2, 1, 1, 0);
            for (self.cases[step_n + 1 ..], 0..) |case, k| {
                try drawCaseForFolding(drawer, camera, template_point.applyToLocalPoint(.{
                    .pos = .new(4, (tof32(k) + offset) * 3),
                }), case, old_bindings, if (k > 0) 0 else 1.0 - offset, null);
            }

            return .fully_consumed;
        } else if (step_n == self.matched_index) {
            const matched_case = self.cases[step_n];

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);
            const t_bindings: ?f32 = if (anim_t < 0.2) null else math.remapTo01Clamped(anim_t, 0.2, 0.8);

            const pattern_point = input_point.applyToLocalPoint(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
            );

            const is_last_match = (self.matched.funk_tangent == null and self.matched.next == null);

            if (is_last_match) {
                try drawer.drawSexpr(camera, .{
                    .is_pattern = 1,
                    .pos = pattern_point,
                    .value = matched_case.pattern,
                });
                try drawer.drawTemplateSexprWithBindings(camera, .lerp(
                    pattern_point.applyToLocalPoint(.{ .pos = .new(2, 0) }),
                    input_point.applyToLocalPoint(.{ .pos = .new(2, 0) }),
                    anim_t,
                ), matched_case.template, .{
                    .anim_t = t_bindings,
                    .old = self.incoming_bindings,
                    .new = self.new_bindings,
                });
                return .{ .right_before_exiting = anim_t };
            } else {
                var next_cases_pattern_point = input_point.applyToLocalPoint(.{ .pos = .new(16, 0) });
                try drawCaseForFolding(drawer, camera, pattern_point, matched_case, .{
                    .anim_t = t_bindings,
                    .new = self.new_bindings,
                    .old = self.incoming_bindings,
                }, 1, if (self.matched.funk_tangent) |funk_tangent| .{
                    .t = math.remapTo01Clamped(anim_t, 0.2, 1.0),
                    .cases = funk_tangent.tree.cases,
                    .next_cases_pattern_point_ptr = &next_cases_pattern_point,
                    // .next_cases_pattern_point_ptr = undefined,
                } else null);
                return .fully_consumed;
            }
        } else {
            const matched = self.matched;

            try drawer.drawSexpr(camera, .{
                .is_pattern = 1,
                .pos = input_point.applyToLocalPoint(.{ .pos = .new(3, 0) }),
                .value = matched.pattern,
            });

            try drawer.drawTemplateSexprWithBindings(
                camera,
                input_point.applyToLocalPoint(.{ .pos = .new(
                    if (self.matched.funk_tangent == null and self.matched.next == null)
                        2
                    else
                        5,
                    0,
                ) }),
                matched.raw_template,
                .{ .anim_t = 1, .new = self.new_bindings, .old = self.incoming_bindings },
            );

            if (self.matched.funk_tangent) |funk_tangent| {
                const asdf = try funk_tangent.tree.drawAsThreadWithFolding(drawer, camera, input_point.applyToLocalPoint(.{ .pos = .new(5, 0) }), t - tof32(self.matched_index) - 1);
                try drawer.drawHoldedFnk(camera, input_point.applyToLocalPoint(.{ .pos = .new(8, -1.5), .scale = 0.5, .turns = -0.25 }), 0, funk_tangent.fn_name);
                switch (asdf) {
                    .fully_consumed => {
                        if (self.matched.next) |next| {
                            try drawCaseForFolding(
                                drawer,
                                camera,
                                input_point.applyToLocalPoint(.{ .pos = .new(11, 0) }).rotateAroundLocalPosition(.new(-0.5, -1.5), -0.4),
                                next.cases[0],
                                .{ .anim_t = 1, .new = self.new_bindings, .old = self.incoming_bindings },
                                1,
                                null,
                            );
                        }
                    },
                    .right_before_exiting => |turn_t| {
                        if (self.matched.next) |next| {
                            try drawCaseForFolding(
                                drawer,
                                camera,
                                input_point.applyToLocalPoint(.{ .pos = .new(11, 0) }).rotateAroundLocalPosition(.new(-0.5, -1.5), math.lerp(
                                    -0.4,
                                    0,
                                    math.smoothstepEased(turn_t, 0, 1, .easeInOutCubic),
                                )),
                                next.cases[0],
                                .{ .anim_t = 1, .new = self.new_bindings, .old = self.incoming_bindings },
                                1,
                                null,
                            );
                        }
                    },
                    .exited => |info| {
                        if (self.matched.next) |next| {
                            return try next.drawAsThreadWithFolding(
                                drawer,
                                camera,
                                input_point.applyToLocalPoint(.{ .pos = .new(7, 0) }),
                                info.remaining_t,
                            );
                        } else return .{ .exited = .{ .remaining_t = t - tof32(self.matched_index) - 1 } };
                    },
                }
            } else {
                assert(self.matched.next == null);
                return .{ .exited = .{
                    .remaining_t = t - tof32(self.matched_index) - 1,
                } };
            }

            if (false) {
                var remaining_t = t;
                var next_input_pos = input_point.applyToLocalPoint(.{ .pos = .new(5, 0) });
                if (self.matched.funk_tangent) |funk_tangent| {
                    const asdf = try funk_tangent.tree.drawAsThreadWithFolding(drawer, camera, next_input_pos, t - tof32(self.matched_index) - 1);
                    remaining_t = asdf.remaining_t;
                    next_input_pos = asdf.next_input_pos;
                    if (self.cases[self.matched_index].next) |next| {
                        try drawCaseForFolding(
                            drawer,
                            camera,
                            next_input_pos.rotateAroundLocalPosition(.new(-0.5, -1.5), -0.4),
                            next.items[0],
                            .{ .anim_t = 1, .new = &.{}, .old = self.all_bindings },
                            1,
                            null,
                        );
                    }
                }

                if (remaining_t > 0) {
                    if (self.matched.next) |next| {
                        const asdf = try next.drawAsThreadWithFolding(drawer, camera, next_input_pos, remaining_t);
                        remaining_t = asdf.remaining_t;
                        next_input_pos = asdf.next_input_pos;
                    }
                }

                // return .{ .remaining_t = remaining_t, .next_input_pos = next_input_pos };
            }
            return .fully_consumed;
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

    dst.core_mem = .init(gpa);
    // dst.scoring_run = try .init(
    //     \\ peanoSum {
    //     \\     (@a . nil) -> @a;
    //     \\     (@a . (true . @b)) -> peanoSum: ((true . @a) . @b);
    //     \\ }
    //     \\
    //     \\ peanoMul {
    //     \\     (@a . nil) -> nil;
    //     \\     (@a . (true . @b)) -> peanoMul: (@a . @b) {
    //     \\         @ab -> peanoSum: (@ab . @a);
    //     \\     }
    //     \\ }
    // , &dst.core_mem);
    dst.scoring_run = try .init(
        \\planetFromOlympian {
        \\  Hermes -> Mercury;
        \\  Aphrodite -> Venus;
        \\  Ares -> Mars;
        \\  Zeus -> Jupiter;
        \\}
        \\
        \\ planetFromWrappedOlympian {
        \\     @a -> planetFromOlympian: @a {
        \\          @x -> ((top . @x) . bottom);
        \\       // @b -> wrap: @b;
        \\     }
        \\ }
        \\
        \\ wrap {
        \\     @x -> ((top . @x) . bottom);
        \\ }
        \\
        \\ mapTree {
        \\   (@a . @b) -> mapTree: @a {
        \\     @a2 -> mapTree: @b {
        \\       @b2 -> (@a2 . @b2);
        \\     }
        \\   }
        \\   @x -> planetFromOlympian: @x;
        \\ }
        \\
        \\ not {
        \\  true -> false;
        \\  false -> true;
        \\ }
        \\
        \\ nextWithoutFnkTest {
        \\   @x -> nextWithoutFnk: @x {
        \\     @y -> not: @y;
        \\   }
        \\ }
        \\ nextWithoutFnk {
        \\   (Hermes . @b) -> @b {
        \\      Ares -> true;
        \\      @x -> false;
        \\   }
        \\ }
    , &dst.core_mem);

    const fn_name: []const u8 = "nextWithoutFnkTest";
    // const fn_name: []const u8 = "peanoMul";
    // const fn_name: []const u8 = "planetFromWrappedOlympian";
    // const input: []const u8 = "Hermes";
    // const input: []const u8 = "Aphrodite";
    // const input: []const u8 = "((true . (true . (true . nil))) . (true . (true . (true . nil))))";
    // const input: []const u8 = "((true . (true . nil)) . (true . (true . nil)))";
    const input: []const u8 = "(Hermes . Ares)";
    // const input: []const u8 = "((Hermes . Aphrodite) . (Ares . Zeus))";

    const thread_initial_params: ThreadInitialParams = try .initFromText(input, fn_name, &dst.scoring_run);

    var snaps: std.ArrayList(core.ExecutionThread) = .init(gpa);
    var asdf = try thread_initial_params.toThread(&dst.scoring_run);
    var k: usize = 1;
    while (try asdf.advanceTinyStep(&dst.scoring_run) == null) {
        try snaps.append(try thread_initial_params.startThreadAndRunItToStep(&dst.scoring_run, k));
        k += 1;
    }
    dst.snapshots = try snaps.toOwnedSlice();

    dst.tree = try .buildFromText(&dst.scoring_run, fn_name, input);

    try dst.workspace.init(&dst.core_mem);
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    _ = gpa;
    self.usual.deinit(undefined);
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    try Drawer.AtomVisuals.Geometry.initFixed(self.usual.mem.forever.allocator());
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);
    // const mem = &self.usual.mem;
    // const canvas = &self.usual.canvas;

    const camera = self.camera.withAspectRatio(platform.aspect_ratio, .grow, .top_left);
    const mouse = platform.getMouse(camera);
    defer self.camera = moveCamera(self.camera, platform.delta_seconds, platform.keyboard, mouse);

    platform.gl.clear(COLORS.bg);

    if (true) {
        try self.workspace.update(platform, &self.drawer, self.camera, &self.core_mem, self.drawer.canvas.frame_arena.allocator());
    }

    if (false) {
        if (platform.keyboard.cur.isDown(.KeyE)) {
            self.progress_t += platform.delta_seconds * 2;
        }
        if (platform.keyboard.cur.isDown(.KeyQ)) {
            self.progress_t -= platform.delta_seconds * 2;
        }
        self.progress_t = math.clamp(self.progress_t, 0, @as(f32, @floatFromInt(self.snapshots.len)) - 0.01);

        if (true) {
            const execution_thread = self.snapshots[@intFromFloat(@floor(self.progress_t))];
            const anim_t = @mod(self.progress_t, 1.0);
            try drawThread(&self.drawer, camera, execution_thread, anim_t, .{});
            // try drawThreadWithFolding(&self.drawer, camera, execution_thread, anim_t, .{ .pos = .new(0, 16) });
        }

        if (true) {
            // try self.tree.drawAsThread(&self.drawer, camera, .{ .pos = .new(0, -4) }, self.progress_t);
            // _ = try self.tree.drawAsThreadWithFolding(&self.drawer, camera, .{ .pos = .new(0, 80) }, self.progress_t);
            try self.tree.drawAsExecutingThread(&self.drawer, camera, .{ .pos = .new(0, 40) }, self.progress_t);
        }

        if (true) {
            try self.tree.draw(&self.drawer, camera, .{ .pos = .new(0, -12), .turns = -0.1 });
        }
    }

    return false;
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

fn drawThread(drawer: *Drawer, camera: Rect, execution_thread: core.ExecutionThread, anim_t: f32, starting_point: Point) !void {
    var template_point: Point = starting_point;
    const old_matches = switch (execution_thread.last_visual_state) {
        .matched => execution_thread.prev_matches.items[0..execution_thread.prev_matches.items.len -| 1],
        else => execution_thread.prev_matches.items,
    };
    for (old_matches) |match| {
        try drawer.drawSexpr(camera, .{
            .is_pattern = 0,
            .value = match.value,
            .pos = template_point,
        });
        try drawer.drawSexpr(camera, .{
            .is_pattern = 1,
            .value = match.pattern,
            .pos = template_point.applyToLocalPoint(.{ .pos = .new(3, 0) }),
        });

        template_point = template_point.applyToLocalPoint(.{ .pos = .new(5, 0) });
    }

    var next_cases_pattern_point = template_point.applyToLocalPoint(.{ .pos = .new(16, 0) });
    var it = std.mem.reverseIterator(execution_thread.stack.items);
    switch (execution_thread.last_visual_state) {
        .failed_to_match => |discarded_case| {
            const active_stack: core.StackThing = it.next().?;

            try drawer.drawSexpr(camera, .{
                .is_pattern = 0,
                .value = execution_thread.active_value,
                .pos = template_point,
            });

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);

            const old_bindings: BindingsState = .{
                .anim_t = null,
                .new = &.{},
                .old = active_stack.cur_bindings.items,
            };

            const fly_away = math.remapClamped(anim_t, 0.2, 0.8, 0, 1);
            const pattern_point_floating_away = template_point.applyToLocalPoint(Point.lerp(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
                .{ .pos = .new(8, -8), .scale = 0, .turns = -0.65 },
                fly_away,
            ));
            try drawCase(drawer, camera, pattern_point_floating_away, discarded_case, old_bindings, 1, null);

            const offset = math.remapClamped(anim_t, 0.2, 1, 1, 0);
            for (active_stack.cur_cases, 0..) |case, k| {
                try drawCase(drawer, camera, template_point.applyToLocalPoint(.{
                    .pos = .new(4, (tof32(k) + offset) * 3),
                }), case, old_bindings, if (k > 0) 0 else 1.0 - offset, null);
            }
        },
        .matched => |matched| {
            try drawer.drawSexpr(camera, .{
                .is_pattern = 0,
                .value = matched.old_active_value,
                .pos = template_point,
            });

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);
            const t_bindings: ?f32 = if (anim_t < 0.2) null else math.remapTo01Clamped(anim_t, 0.2, 0.8);

            const pattern_point = template_point.applyToLocalPoint(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
            );

            try drawCase(drawer, camera, pattern_point, matched.case, .{
                .anim_t = t_bindings,
                .new = matched.new_bindings,
                .old = matched.old_bindings,
            }, 1, if (matched.added_new_fnk_to_stack) .{
                .t = math.remapTo01Clamped(anim_t, 0.2, 1.0),
                .cases = it.next().?.cur_cases,
                .next_cases_pattern_point_ptr = &next_cases_pattern_point,
                // .next_cases_pattern_point_ptr = undefined,
            } else null);

            if (matched.added_new_fnk_to_stack and !matched.tail_optimized) {
                _ = it.next();
                // std.log.debug("added? {any}, tail? {any}", .{ matched.added_new_fnk_to_stack, matched.tail_optimized });
            }

            // std.log.debug("{any}", .{matched.tail_optimized});
            if (matched.tail_optimized and !matched.added_new_fnk_to_stack) {
                if (it.next()) |active_stack| {
                    // const offset = math.remapClamped(anim_t, 0.2, 1, 1, 0);
                    if (!matched.added_new_fnk_to_stack) {
                        next_cases_pattern_point = next_cases_pattern_point.applyToLocalPoint(
                            .{ .pos = .new(5, 0) },
                            // .{ .pos = .new(5 + -5 * anim_t, 0) },
                        );
                    }
                    for (active_stack.cur_cases, 0..) |case, k| {
                        // try drawCase(drawer, camera, template_point.applyToLocalPoint(.{
                        //     .pos = .new(4, (tof32(k) + offset) * 3),
                        // }), case, old_bindings, if (k > 0) 0 else 1.0 - offset, null);

                        if (!matched.added_new_fnk_to_stack) {
                            try drawCase(drawer, camera, next_cases_pattern_point.applyToLocalPoint(
                                .{ .pos = .new(-5 - 7 * math.smoothstepEased(anim_t, 0.0, 0.4, .easeInOutCubic), 3 * tof32(k)) },
                                // .{ .pos = .new(-5 - 2 * math.smoothstepEased(anim_t, 0.0, 0.4, .easeInOutCubic), 3 * tof32(k)) },
                            ), case, .{
                                .new = &.{},
                                .old = active_stack.cur_bindings.items,
                                .anim_t = null,
                            }, if (k == 0) anim_t else 0, null);
                            // try drawCase(drawer, camera, pattern_point.applyToLocalPoint(
                            //     .{ .pos = .new(7 - 1 * math.smoothstepEased(anim_t, 0.0, 0.4, .easeInOutCubic), 3 * tof32(k)) },
                            // ), case, .{
                            //     .new = &.{},
                            //     .old = active_stack.cur_bindings.items,
                            //     .anim_t = null,
                            // }, if (k == 0) 1 else 0, null);
                        }
                    }
                }
            }

            // if (matched.added_new_fnk_to_stack) {
            // const active_stack: core.StackThing = it.next().?;
            // TODO: draw fnk getting bigger
            // try drawCases(drawer, camera, pattern_point.applyToLocalPoint(.{
            //     .pos = .new(8, 0),
            // }), active_stack.cur_cases, .none);
            // }
            // const match_dist = math.remapClamped(self.anim_t, 0, 0.2, 1, 0);
            // _ = matched;
            // const active_stack: core.StackThing = it.next().?;
            // for (active_stack.cur_cases, 0..) |case, k| {
            //     try drawCase(&self.drawer, camera, .{ .pos = .new(4, tof32(k) * 3) }, case, if (k == 0) 1 else 0);
            // }
        },
        // else => {},
        inline else => |_, t| std.log.debug("unhandled: {s}", .{@tagName(t)}),
    }

    // try drawer.drawSexpr(camera, .{
    //     .is_pattern = 1,
    //     .pos = next_cases_pattern_point,
    //     .value = Sexpr.builtin.true,
    // });

    // std.log.debug("len: {d}", .{kommon.itertools.iteratorLen(it)});

    // next_cases_pattern_point = next_cases_pattern_point.applyToLocalPoint(.{ .pos = .new(4, 0) });
    while (it.next()) |asdf| {
        // template_point = template_point.applyToLocalPoint(.{ .pos = .new(12, 0) });
        defer next_cases_pattern_point = next_cases_pattern_point.applyToLocalPoint(.{ .pos = .new(5, 0) });
        const x: core.StackThing = asdf;
        // std.log.debug("{any}", .{x.cur_cases});
        // const pattern_point = next_cases_pattern_point.applyToLocalPoint(
        //     .{ .pos = .new(4, 0) },
        // );
        assert(x.cur_cases.len > 0);
        try drawer.drawSexpr(camera, .{
            .is_pattern = 1,
            .pos = next_cases_pattern_point,
            .value = x.cur_cases[0].pattern,
        });
        for (x.cur_cases, 0..) |case, k| {
            _ = k;
            _ = case;
            // try drawer.drawSexpr(camera, .{
            //     .is_pattern = 1,
            //     .pos = next_cases_pattern_point,
            //     .value = case.pattern,
            // });
            // try drawCase(drawer, camera, next_cases_pattern_point.applyToLocalPoint(
            //     .{ .pos = .new(0, 3 * tof32(k)) },
            // ), case, .{
            //     .new = &.{},
            //     .old = x.cur_bindings.items,
            //     .anim_t = null,
            // }, if (k == 0) 1 else 0, null);
        }
    }

    // try drawer.drawSexpr(camera, .{
    //     .is_pattern = 1,
    //     .pos = next_cases_pattern_point,
    //     .value = Sexpr.builtin.false,
    // });
}

fn drawCaseForFolding(
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

    var next_pattern_point = pattern_point.applyToLocalPoint(.{ .pos = .new(8, 0) });
    var function_point = pattern_point.applyToLocalPoint(.{
        .pos = .new(5, 0),
        .turns = -0.25,
        .scale = 0.5,
    });

    if (invoking_next) |invoking| {
        next_pattern_point = next_pattern_point.rotateAroundLocalPosition(.new(-0.5, -1.5), math.lerp(
            0,
            -0.4,
            math.smoothstepEased(invoking.t, 0, 1, .easeInOutCubic),
        ));
        function_point = function_point.applyToLocalPoint(.{ .pos = .new(3 * invoking.t, 0) });

        const offset = (1.0 - invoking.t) + 2.0 * math.smoothstepEased(invoking.t, 0.4, 0.0, .linear);
        for (invoking.cases, 0..) |next_case, k| {
            try drawCase(drawer, camera, pattern_point.applyToLocalPoint(
                .{ .pos = .new(6, 3 * (offset + tof32(k))) },
            ), next_case, .none, if (k == 0) 1 else 0, null);
        }

        if (case.next != null) {
            invoking.next_cases_pattern_point_ptr.* = invoking.next_cases_pattern_point_ptr.*.applyToLocalPoint(
                .{ .pos = .new(10 * math.smoothstepEased(invoking.t, 0.0, 0.5, .easeInOutCubic), 0) },
            );
        } else {
            invoking.next_cases_pattern_point_ptr.* = invoking.next_cases_pattern_point_ptr.*.applyToLocalPoint(
                .{ .pos = .new(5 * math.smoothstepEased(invoking.t, 0.0, 0.5, .easeInOutCubic), 0) },
            );
        }
    }

    try drawer.drawSexpr(camera, .{
        .is_pattern = 1,
        .value = case.pattern,
        .pos = pattern_point,
    });

    if (unfolded > 0.1) {
        try drawer.drawHoldedFnk(camera, function_point, 0, case.fnk_name);

        try drawer.drawTemplateSexprWithBindings(
            camera,
            pattern_point.applyToLocalPoint(.{ .pos = .new(2, 0) }),
            case.template,
            bindings,
        );

        if (case.next) |next| {
            assert(next.items.len > 0);
            try drawCaseForFolding(
                drawer,
                camera,
                next_pattern_point,
                next.items[0],
                bindings,
                1,
                null,
            );
        }
    }
}

fn drawThreadWithFolding(drawer: *Drawer, camera: Rect, execution_thread: core.ExecutionThread, anim_t: f32, starting_point: Point) !void {
    var template_point: Point = starting_point;
    const old_matches = switch (execution_thread.last_visual_state) {
        .matched => execution_thread.prev_matches.items[0..execution_thread.prev_matches.items.len -| 1],
        else => execution_thread.prev_matches.items,
    };
    for (old_matches) |match| {
        try drawer.drawSexpr(camera, .{
            .is_pattern = 0,
            .value = match.value,
            .pos = template_point,
        });
        try drawer.drawSexpr(camera, .{
            .is_pattern = 1,
            .value = match.pattern,
            .pos = template_point.applyToLocalPoint(.{ .pos = .new(3, 0) }),
        });

        template_point = template_point.applyToLocalPoint(.{ .pos = .new(5, 0) });
    }

    var next_cases_pattern_point = template_point.applyToLocalPoint(.{ .pos = .new(16, 0) });
    var it = std.mem.reverseIterator(execution_thread.stack.items);
    switch (execution_thread.last_visual_state) {
        .failed_to_match => |discarded_case| {
            const active_stack: core.StackThing = it.next().?;

            try drawer.drawSexpr(camera, .{
                .is_pattern = 0,
                .value = execution_thread.active_value,
                .pos = template_point,
            });

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);

            const old_bindings: BindingsState = .{
                .anim_t = null,
                .new = &.{},
                .old = active_stack.cur_bindings.items,
            };

            const fly_away = math.remapClamped(anim_t, 0.2, 0.8, 0, 1);
            const pattern_point_floating_away = template_point.applyToLocalPoint(Point.lerp(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
                .{ .pos = .new(8, -8), .scale = 0, .turns = -0.65 },
                fly_away,
            ));
            try drawCaseForFolding(drawer, camera, pattern_point_floating_away, discarded_case, old_bindings, 1, null);

            const offset = math.remapClamped(anim_t, 0.2, 1, 1, 0);
            for (active_stack.cur_cases, 0..) |case, k| {
                try drawCaseForFolding(drawer, camera, template_point.applyToLocalPoint(.{
                    .pos = .new(4, (tof32(k) + offset) * 3),
                }), case, old_bindings, if (k > 0) 0 else 1.0 - offset, null);
            }
        },
        .matched => |matched| {
            try drawer.drawSexpr(camera, .{
                .is_pattern = 0,
                .value = matched.old_active_value,
                .pos = template_point,
            });

            const match_dist = math.remapClamped(anim_t, 0, 0.2, 1, 0);
            const t_bindings: ?f32 = if (anim_t < 0.2) null else math.remapTo01Clamped(anim_t, 0.2, 0.8);

            const pattern_point = template_point.applyToLocalPoint(
                .{ .pos = .new(math.remapFrom01(match_dist, 3, 4), 0) },
            );

            try drawCaseForFolding(drawer, camera, pattern_point, matched.case, .{
                .anim_t = t_bindings,
                .new = matched.new_bindings,
                .old = matched.old_bindings,
            }, 1, if (matched.added_new_fnk_to_stack) .{
                .t = math.remapTo01Clamped(anim_t, 0.2, 1.0),
                .cases = it.next().?.cur_cases,
                .next_cases_pattern_point_ptr = &next_cases_pattern_point,
            } else null);

            if (matched.added_new_fnk_to_stack and !matched.tail_optimized) {
                _ = it.next();
            }

            if (matched.tail_optimized and !matched.added_new_fnk_to_stack) {
                if (it.next()) |active_stack| {
                    if (!matched.added_new_fnk_to_stack) {
                        next_cases_pattern_point = next_cases_pattern_point.applyToLocalPoint(
                            .{ .pos = .new(5, 0) },
                        );
                    }
                    for (active_stack.cur_cases, 0..) |case, k| {
                        if (!matched.added_new_fnk_to_stack) {
                            try drawCaseForFolding(drawer, camera, next_cases_pattern_point.applyToLocalPoint(
                                .{ .pos = .new(-5 - 7 * math.smoothstepEased(anim_t, 0.0, 0.4, .easeInOutCubic), 3 * tof32(k)) },
                            ), case, .{
                                .new = &.{},
                                .old = active_stack.cur_bindings.items,
                                .anim_t = null,
                            }, if (k == 0) anim_t else 0, null);
                        }
                    }
                }
            }
        },
        inline else => |_, t| std.log.debug("unhandled: {s}", .{@tagName(t)}),
    }

    if (true) return;
    while (it.next()) |asdf| {
        defer next_cases_pattern_point = next_cases_pattern_point.applyToLocalPoint(.{ .pos = .new(5, 0) });
        const x: core.StackThing = asdf;
        assert(x.cur_cases.len > 0);
        try drawer.drawSexpr(camera, .{
            .is_pattern = 1,
            .pos = next_cases_pattern_point,
            .value = x.cur_cases[0].pattern,
        });
    }
}

fn moveCamera(camera: Rect, delta_seconds: f32, keyboard: Keyboard, mouse: Mouse) Rect {
    var result = camera;
    const mouse_pos = mouse.cur.position;

    result = result.zoom(mouse_pos, switch (mouse.cur.scrolled) {
        .none => 1.0,
        .down => 1.1,
        .up => 0.9,
    });

    inline for (KeyboardButton.directional_keys) |kv| {
        for (kv.keys) |key| {
            if (keyboard.cur.isDown(key)) {
                result.top_left.addInPlace(kv.dir.scale(delta_seconds * camera.size.y));
            }
        }
    }

    if (mouse.cur.isDown(.middle) and mouse.prev.isDown(.middle)) {
        result.top_left.addInPlace(mouse.deltaPos());
    }

    return result;
}

fn splitLast(T: type, arr: []const T) std.meta.Tuple(&.{ []const T, T }) {
    assert(arr.len >= 1);
    return .{
        arr[0 .. arr.len - 1],
        arr[arr.len - 1],
    };
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
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const PhysicalSexpr = @import("physical.zig").PhysicalSexpr;
const ViewHelper = @import("physical.zig").ViewHelper;
const BindingsState = @import("physical.zig").BindingsState;
