pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

const core = @import("core.zig");
const Drawer = @import("Drawer.zig");

pub const tracy = @import("tracy");

pub const display_fps = false;

const EXECUTOR_MOVES_LEFT = true;

const CRANKS_ENABLED = false;

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
            if (input.len < 8) return;
            _ = context;
            var mem: core.VeryPermamentGameStuff = .init(std.testing.allocator);
            defer mem.deinit();
            var workspace: Workspace = undefined;
            try workspace.init(&mem, std.mem.readInt(u64, input[0..8], .little));
            defer workspace.deinit(std.testing.allocator);
            var frame_arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
            defer frame_arena.deinit();

            var test_platform: TestPlatform = .{};

            var it = std.mem.window(u8, input[8..], @sizeOf(FakeInput), @sizeOf(FakeInput));
            while (it.next()) |cur_input_raw| {
                if (cur_input_raw.len == @sizeOf(FakeInput)) {
                    const cur_input = std.mem.bytesToValue(FakeInput, cur_input_raw);
                    test_platform.keyboard.cur.keys.KeyZ = cur_input.z_down;
                    test_platform.mouse.cur.buttons.left = cur_input.mouse_left_down;
                    test_platform.mouse.cur.position = cur_input.mouse_pos;
                    try workspace.update(test_platform.getGives(1.0 / 60.0), null, &mem, frame_arena.allocator());
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
    try workspace.init(&mem, std.testing.random_seed);
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
core_mem: core.VeryPermamentGameStuff,
workspace: Workspace,

pub const HoveredSexpr = struct {
    next: ?struct {
        left: *HoveredSexpr,
        right: *HoveredSexpr,
    },
    value: f32,

    pub const Pool = std.heap.MemoryPool(HoveredSexpr);

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

    pub fn fillVariables(hovered: *HoveredSexpr, original_value: *const Sexpr, bindings: []const core.Binding, pool: *Pool) !*HoveredSexpr {
        switch (original_value.*) {
            .atom_var => |templ| {
                for (0..bindings.len) |k| {
                    const bind = bindings[bindings.len - k - 1];
                    if (std.mem.eql(u8, bind.name, templ.value)) {
                        return try fromSexpr(pool, bind.value);
                    }
                }
                return hovered;
            },
            .atom_lit, .empty => return hovered,
            .pair => |templ| {
                return store(pool, .{ .value = hovered.value, .next = .{
                    .left = try fillVariables(hovered.next.?.left, templ.left, bindings, pool),
                    .right = try fillVariables(hovered.next.?.right, templ.right, bindings, pool),
                } });
            },
        }
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
    point: Point,
    hot_t: f32 = 0,
    pub const radius = 0.2;

    pub fn draw(handle: Handle, drawer: *Drawer, camera: Rect, alpha: f32) !void {
        try handle.drawCustom(drawer, camera, alpha, 1, 0.2);
    }

    pub fn drawCustom(handle: Handle, drawer: *Drawer, camera: Rect, alpha: f32, p1: f32, p2: f32) !void {
        drawer.canvas.fillCircle(camera, handle.point.pos, handle.point.scale * p1 * radius * (1 + handle.hot_t * p2), COLORS.bg.withAlpha(alpha));
        drawer.canvas.strokeCircle(128, camera, handle.point.pos, handle.point.scale * p1 * radius * (1 + handle.hot_t * p2), 0.05 * handle.point.scale, .blackAlpha(alpha));
    }

    pub fn update(handle: *Handle, hot_target: f32, delta_seconds: f32) void {
        math.lerp_towards(&handle.hot_t, hot_target, 0.6, delta_seconds);
    }

    // TODO: radius should not be a parameter
    pub fn overlapped(handle: *const Handle, pos: Vec2, comptime base_radius: f32) bool {
        return pos.distTo(handle.point.pos) < base_radius * handle.point.scale;
    }

    pub fn save(handle: *const Handle, out: std.io.AnyWriter, _: std.mem.Allocator) !void {
        try out.writeStructEndian(handle.point, .little);
    }

    pub fn load(dst: *Handle, in: std.io.AnyReader, version: u32, _: *core.VeryPermamentGameStuff) !void {
        assert(version == 0);
        dst.* = .{
            .point = try in.readStructEndian(Point, .little),
        };
    }
};

const VeryPhysicalGarland = struct {
    // TODO: doubly linked list?
    cases: std.ArrayListUnmanaged(VeryPhysicalCase),
    // TODO: change from 'handle' to 'segment'
    handles_for_new_cases_first: HandleForNewCase,
    handles_for_new_cases_rest: std.ArrayListUnmanaged(HandleForNewCase),

    /// only present on invoked fnks, always as pattern
    fnkname: ?VeryPhysicalSexpr = null,

    handle: Handle,
    pub const handle_radius: f32 = 0.2;
    pub const handle_drop_radius: f32 = 1.5;
    pub const case_drop_preview_dist: f32 = 0.5 * dist_between_cases_rest;
    pub const dist_between_cases_first: f32 = 1.5;
    pub const dist_between_cases_rest: f32 = 2.5;

    const HandleForNewCase = struct {
        length: f32,
        // TODO: extract hot_t
        handle: Handle,

        pub fn save(self: *const HandleForNewCase, out: std.io.AnyWriter, m: std.mem.Allocator) !void {
            try self.handle.save(out, m);
            try writeF32(out, self.length);
        }

        pub fn load(dst: *HandleForNewCase, in: std.io.AnyReader, version: u32, m: *core.VeryPermamentGameStuff) !void {
            assert(version == 0);
            try dst.handle.load(in, version, m);
            dst.length = try readF32(in);
        }
    };

    pub fn save(garland: *const VeryPhysicalGarland, out: std.io.AnyWriter, scratch: std.mem.Allocator) anyerror!void {
        try out.writeStructEndian(garland.handle.point, .little);
        try out.writeInt(u32, @intCast(garland.cases.items.len), .little);
        for (garland.cases.items) |case| {
            try case.save(out, scratch);
        }
        try garland.handles_for_new_cases_first.save(out, scratch);
        for (garland.handles_for_new_cases_rest.items) |c| {
            try c.save(out, scratch);
        }
        if (garland.fnkname) |fnkname| {
            try writeBool(out, true);
            try fnkname.save(out, scratch);
        } else {
            try writeBool(out, false);
        }
    }

    pub fn load(dst: *VeryPhysicalGarland, in: std.io.AnyReader, version: u32, mem: *core.VeryPermamentGameStuff) anyerror!void {
        assert(version == 0);
        dst.*.handle = .{ .point = try in.readStructEndian(Point, .little) };
        const n_cases: usize = @intCast(try in.readInt(u32, .little));
        dst.cases = .fromOwnedSlice(try mem.gpa.alloc(VeryPhysicalCase, n_cases));
        for (dst.cases.items) |*c| {
            try c.load(in, version, mem);
        }
        try dst.handles_for_new_cases_first.load(in, version, mem);
        dst.handles_for_new_cases_rest = .fromOwnedSlice(try mem.gpa.alloc(HandleForNewCase, n_cases));
        for (dst.handles_for_new_cases_rest.items) |*c| {
            try c.load(in, version, mem);
        }
        if (try readBool(in)) {
            dst.fnkname = undefined;
            try dst.fnkname.?.load(in, version, mem);
        } else {
            dst.fnkname = null;
        }
    }

    pub fn init(point: Point) VeryPhysicalGarland {
        return .{
            .handle = .{ .point = point },
            .cases = .empty,
            .handles_for_new_cases_rest = .empty,
            .handles_for_new_cases_first = .{ .length = dist_between_cases_first, .handle = .{ .point = point.applyToLocalPoint(.{ .pos = .new(0, dist_between_cases_first / 2.0) }) } },
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

    pub fn fromDefinition(base: Point, definition: core.FnkBodyV2, mem: *core.VeryPermamentGameStuff, hover_pool: *HoveredSexpr.Pool) !VeryPhysicalGarland {
        var result: VeryPhysicalGarland = .init(base);
        try result.cases.ensureUnusedCapacity(mem.gpa, definition.cases.len);
        try result.handles_for_new_cases_rest.ensureUnusedCapacity(mem.gpa, definition.cases.len);
        for (definition.cases, 0..) |case, k| {
            try result.insertCase(mem.gpa, k, try .fromValues(hover_pool, .{
                .pattern = case.pattern,
                .fnk_name = case.fnk_name,
                .template = case.template,
                .next = if (case.next) |n|
                    try .fromDefinition(base.applyToLocalPoint(.{
                        .pos = .new(1, tof32(k)),
                    }), .{ .cases = n }, mem, hover_pool)
                else
                    .init(base.applyToLocalPoint(.{ .pos = .new(1, tof32(k)) })),
            }, base.applyToLocalPoint(.{ .pos = .new(0, tof32(k)) })));
        }
        return result;
    }

    pub fn toDefinition(garland: *const VeryPhysicalGarland, res: std.mem.Allocator) !core.FnkBody {
        // TODO: leaks
        var cases: core.MatchCases = try .initCapacity(res, garland.cases.items.len);
        for (garland.cases.items) |c| {
            cases.appendAssumeCapacity(.{
                .pattern = c.pattern.value,
                .fnk_name = c.fnk_name.value,
                .template = c.template.value,
                .next = if (c.next.cases.items.len > 0)
                    (try c.next.toDefinition(res)).cases
                else
                    null,
            });
        }
        return .{ .cases = cases };
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

    pub fn draw(garland: VeryPhysicalGarland, holding: VeryPhysicalCase.Holding, drawer: *Drawer, camera: Rect) !void {
        try garland.drawWithBindings(null, holding, drawer, camera);
    }

    pub fn drawWithAlpha(garland: VeryPhysicalGarland, alpha: f32, holding: VeryPhysicalCase.Holding, drawer: *Drawer, camera: Rect) !void {
        try garland.drawWithBindingsAndAlpha(null, alpha, holding, drawer, camera);
    }

    pub fn drawWithBindings(garland: VeryPhysicalGarland, bindings: ?BindingsState, holding: VeryPhysicalCase.Holding, drawer: *Drawer, camera: Rect) !void {
        try garland.drawWithBindingsAndAlpha(bindings, 1, holding, drawer, camera);
    }

    pub fn drawWithBindingsAndAlpha(
        garland: VeryPhysicalGarland,
        bindings: ?BindingsState,
        alpha: f32,
        holding: VeryPhysicalCase.Holding,
        drawer: *Drawer,
        camera: Rect,
    ) error{
        InvalidUtf8,
        OutOfMemory,
        BadVertexOrder,

        ShaderCreationError,
        ProgramCreationError,
        AttributeLocationError,
        UniformLocationError,
        TooManyUniforms,
    }!void {
        assert(math.in01(alpha));
        if (garland.fnkname) |f| try f.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        var last_pos = garland.handle.point.pos;
        for (0..garland.cases.items.len + 1) |k| {
            const h = garland.handleForNewCases(&.{k});
            const cur_pos = h.point.pos;
            // TODO: cable with wildcard info
            drawer.canvas.line(camera, &.{ last_pos, cur_pos }, 0.05, .blackAlpha(alpha));
            last_pos = cur_pos;
        }
        try garland.handle.draw(drawer, camera, alpha);
        try garland.handle.draw(drawer, camera, alpha);
        for (0..garland.cases.items.len + 1) |k| {
            const h = garland.handleForNewCases(&.{k});
            try h.drawCustom(drawer, camera, alpha, 0.5, 3.0);
        }
        for (garland.cases.items) |c| try c.drawWithBindingsAndAlpha(bindings, alpha, holding, drawer, camera);
    }

    pub fn handleForNewCases(garland: *const VeryPhysicalGarland, address: core.CaseAddress) *const Handle {
        return &garland.handleForNewCasesInner(address).handle;
    }

    pub fn handleForNewCasesInner(garland: *const VeryPhysicalGarland, address: core.CaseAddress) *const HandleForNewCase {
        assert(address.len > 0);
        if (address.len == 1) {
            const k = address[0];
            assert(k <= garland.cases.items.len);
            return if (k == 0)
                &garland.handles_for_new_cases_first
            else
                &garland.handles_for_new_cases_rest.items[k - 1];
        } else {
            return garland.constChildCase(address[0 .. address.len - 1]).next.handleForNewCasesInner(address[address.len - 1 ..]);
        }
    }

    pub fn handleForNewCasesRef(garland: *VeryPhysicalGarland, k: usize) *Handle {
        return &garland.handleForNewCasesRefInner(k).handle;
    }

    pub fn handleForNewCasesRefInner(garland: *VeryPhysicalGarland, k: usize) *HandleForNewCase {
        assert(k <= garland.cases.items.len);
        return if (k == 0)
            &garland.handles_for_new_cases_first
        else
            &garland.handles_for_new_cases_rest.items[k - 1];
    }

    pub fn update(garland: *VeryPhysicalGarland, delta_seconds: f32) void {
        garland.updateWithOffset(0, null, delta_seconds);
    }

    // TODO: maybe remove delta_seconds
    // TODO: maybe remove this method
    pub fn kinematicUpdate(garland: *VeryPhysicalGarland, center: Point, first_ghost: ?*const VeryPhysicalCase, delta_seconds: f32) void {
        garland.updateWithOffsetMaybeKinematic(true, center, 1, first_ghost, delta_seconds);

        // assert(garland.handles_for_new_cases_rest.items.len == garland.cases.items.len);
        // garland.handle.pos = center.pos;
        // if (garland.fnkname) |*f| {
        //     f.point = center.applyToLocalPoint(Fnkviewer.fnkname_from_garland_pattern);
        // }
        // // _ = delta_seconds;
        // // for (0..100) |_| {
        // //     garland.updateWithOffset(1, first_ghost, 1.0 / 60.0);
        // // }
        // for (garland.cases.items, 0..) |*c, k| {
        //     const target = center.applyToLocalPoint(.{ .pos = .new(0, (if (first_ghost) |f| 2.5 + f.next.getExtraHeight() else 0) + dist_between_cases_first + dist_between_cases_rest * tof32(k)) });
        //     c.kinematicUpdate(target, null, null, delta_seconds);
        // }
        // for (0..garland.cases.items.len + 1) |k| {
        //     const h = garland.handleForNewCasesRef(k);
        //     h.pos = if (k == 0)
        //         garland.handle.pos.addY(dist_between_cases_first / 2.0)
        //     else
        //         garland.handle.pos.addY(dist_between_cases_first + dist_between_cases_rest * (tof32(k) - 0.5));
        // }
    }

    fn getExtraHeight(garland: *const VeryPhysicalGarland) f32 {
        var result: f32 = case_drop_preview_dist * garland.handles_for_new_cases_first.handle.hot_t;
        for (garland.cases.items, 0..) |case, k| {
            // if (k != garland.cases.items.len - 1) result += case.next.getExtraHeight();
            result += case.next.getExtraHeight() + case_drop_preview_dist * garland.handles_for_new_cases_rest.items[k].handle.hot_t;
            result += dist_between_cases_rest;
        }
        return @max(0, result - 1);
    }

    pub fn updateWithOffset(garland: *VeryPhysicalGarland, offset: f32, offset_ghost: ?*const VeryPhysicalCase, delta_seconds: f32) void {
        garland.updateWithOffsetMaybeKinematic(false, garland.handle.point, offset, offset_ghost, delta_seconds);
    }

    pub fn updateWithOffsetMaybeKinematic(garland: *VeryPhysicalGarland, comptime kinematic: bool, center: Point, offset: f32, offset_ghost: ?*const VeryPhysicalCase, delta_seconds: f32) void {
        const ratio: f32 = if (kinematic) 1 else 0.6;
        assert(math.in01(offset));
        assert(garland.handles_for_new_cases_rest.items.len == garland.cases.items.len);
        if (kinematic) garland.handle.point = center;
        if (garland.fnkname) |*f| {
            f.point.lerp_towards(center.applyToLocalPoint(Fnkviewer.fnkname_from_garland_pattern), ratio, delta_seconds);
        }

        // update segment lengths
        for (0..garland.cases.items.len + 1) |k| {
            const segment = garland.handleForNewCasesRefInner(k);
            const target_length = (if (k == 0) dist_between_cases_first else dist_between_cases_rest) +
                case_drop_preview_dist * segment.handle.hot_t +
                (if (k > 0) garland.cases.items[k - 1].next.getExtraHeight() else 0) +
                (if (offset_ghost != null and k == 0) offset * (dist_between_cases_rest + offset_ghost.?.next.getExtraHeight()) else 0);
            // segment.length = target_length;
            math.lerp_towards(&segment.length, target_length, ratio, delta_seconds);
        }

        // update handles, for new and existing cases
        for (0..garland.cases.items.len * 2 + 1) |raw_k| {
            if (raw_k % 2 == 0) {
                // updating a segment
                const k = @divExact(raw_k, 2);
                const segment = garland.handleForNewCasesRefInner(k);
                const reference_position: Point = if (k == 0) garland.handle.point else garland.cases.items[k - 1].handle.point;
                const y_offset = if (k == 0)
                    0.5 * (dist_between_cases_first + case_drop_preview_dist * segment.handle.hot_t)
                else
                    segment.length - 0.5 * (dist_between_cases_rest + case_drop_preview_dist * segment.handle.hot_t);

                const target = reference_position.applyToLocalPoint(.{ .pos = .new(0, y_offset) });
                segment.handle.point.lerp_towards(target, ratio, delta_seconds);
            } else {
                // updating a case
                const k = @divExact(raw_k - 1, 2);
                const last_point: Point = if (k == 0) center else garland.cases.items[k - 1].handle.point;
                const prev_handle = garland.handleForNewCasesInner(&.{k});
                const cur_point = last_point.applyToLocalPoint(.{ .pos = .new(0, prev_handle.length) });
                const case = &garland.cases.items[k];
                case.handle.point.lerp_towards(cur_point, ratio, delta_seconds);
                if (kinematic) {
                    case.kinematicUpdate(cur_point, null, null, delta_seconds);
                } else {
                    case.update(delta_seconds);
                }
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
        const old_segment = parent.handles_for_new_cases_rest.orderedRemove(index);
        const case = parent.cases.orderedRemove(index);
        parent.handleForNewCasesRef(index).point = case.handle.point;
        parent.handleForNewCasesRefInner(index).length += old_segment.length;
        return case;
    }

    pub fn popFirstCaseForExecution(parent: *VeryPhysicalGarland) VeryPhysicalCase {
        const old_segment = parent.handles_for_new_cases_rest.orderedRemove(0);
        parent.handles_for_new_cases_first.length += old_segment.length;
        const case = parent.cases.orderedRemove(0);
        return case;
    }

    pub fn insertCase(parent: *VeryPhysicalGarland, mem: std.mem.Allocator, index: usize, case: VeryPhysicalCase) !void {
        const old_length = parent.handleForNewCasesInner(&.{index}).length;
        try parent.handles_for_new_cases_rest.insert(mem, index, .{
            .handle = .{ .point = parent.handleForNewCases(&.{index}).point },
            .length = old_length / 2.0,
        });
        try parent.cases.insert(mem, index, case);
        parent.handleForNewCasesRefInner(index + 1).length = old_length / 2;
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

    pub fn save(case: *const VeryPhysicalCase, out: std.io.AnyWriter, scratch: std.mem.Allocator) !void {
        try case.handle.save(out, scratch);
        try case.pattern.save(out, scratch);
        try case.fnk_name.save(out, scratch);
        try case.template.save(out, scratch);
        try case.next.save(out, scratch);
    }

    pub fn load(dst: *VeryPhysicalCase, in: std.io.AnyReader, version: u32, mem: *core.VeryPermamentGameStuff) !void {
        assert(version == 0);
        try dst.handle.load(in, version, mem);
        try dst.pattern.load(in, version, mem);
        try dst.fnk_name.load(in, version, mem);
        try dst.template.load(in, version, mem);
        try dst.next.load(in, version, mem);
    }

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
            .handle = .{ .point = center },
            .pattern = try .fromSexpr(pool, values.pattern, center.applyToLocalPoint(.{ .pos = .xneg }), true),
            .template = try .fromSexpr(pool, values.template, center.applyToLocalPoint(.{ .pos = .xpos }), false),
            .fnk_name = try .fromSexpr(pool, values.fnk_name, center.applyToLocalPoint(fnk_name_offset), false),
            .next = values.next orelse .init(center.applyToLocalPoint(.{ .pos = .new(6, 0) })),
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

    pub const Holding = enum { other, sexpr, case_or_garland };

    pub fn drawWithBindingsAndAlpha(
        case: VeryPhysicalCase,
        bindings: ?BindingsState,
        alpha: f32,
        holding: Holding,
        drawer: *Drawer,
        camera: Rect,
    ) !void {
        // TODO: draw variables in the cable
        drawer.canvas.line(camera, &.{
            case.pattern.point.pos,
            case.template.point.pos,
        }, 0.05 * case.handle.point.scale, .blackAlpha(alpha));
        try case.handle.draw(drawer, camera, alpha);
        try case.pattern.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        try case.template.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        if (holding == .sexpr or !case.fnk_name.isEmpty()) try case.fnk_name.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        if (holding == .case_or_garland or case.next.cases.items.len > 0) try case.next.drawWithBindingsAndAlpha(bindings, alpha, holding, drawer, camera);
    }

    pub fn drawWithBindings(case: VeryPhysicalCase, bindings: ?BindingsState, holding: Holding, drawer: *Drawer, camera: Rect) !void {
        try case.drawWithBindingsAndAlpha(bindings, 1, holding, drawer, camera);
    }

    pub fn draw(case: VeryPhysicalCase, holding: Holding, drawer: *Drawer, camera: Rect) !void {
        try case.drawWithBindings(null, holding, drawer, camera);
    }

    pub fn update(case: *VeryPhysicalCase, delta_seconds: f32) void {
        const center: Point = case.handle.point;
        case.pattern.point.lerp_towards(center.applyToLocalPoint(.{ .pos = .xneg }), 0.6, delta_seconds);
        case.template.point.lerp_towards(center.applyToLocalPoint(.{ .pos = .xpos }), 0.6, delta_seconds);
        case.fnk_name.point.lerp_towards(center.applyToLocalPoint(fnk_name_offset), 0.6, delta_seconds);
        case.next.handle.point.lerp_towards(center.applyToLocalPoint(.{ .pos = next_garland_offset }), 0.6, delta_seconds);
        case.next.update(delta_seconds);
    }

    pub fn kinematicUpdate(case: *VeryPhysicalCase, center: Point, next_point_extra: ?Point, fnk_name_extra: ?Point, delta_seconds: f32) void {
        case.handle.point = center;
        case.pattern.point = center.applyToLocalPoint(.{ .pos = .xneg });
        case.template.point = center.applyToLocalPoint(.{ .pos = .xpos });
        case.fnk_name.point = center.applyToLocalPoint(fnk_name_offset).applyToLocalPoint(fnk_name_extra orelse .{});
        case.next.kinematicUpdate(center.applyToLocalPoint(.{ .pos = next_garland_offset }).applyToLocalPoint(next_point_extra orelse .{}), null, delta_seconds);
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

    pub fn save(sexpr: *const VeryPhysicalSexpr, out: std.io.AnyWriter, scratch: std.mem.Allocator) !void {
        try out.writeStructEndian(sexpr.point, .little);
        try writeBool(out, sexpr.is_pattern);
        var asdf: std.ArrayList(u8) = .init(scratch);
        defer asdf.deinit();
        // TODO: improve
        try sexpr.value.format("", .{}, asdf.writer().any());
        try writeString(out, asdf.items);
    }

    pub fn load(dst: *VeryPhysicalSexpr, in: std.io.AnyReader, version: u32, mem: *core.VeryPermamentGameStuff) !void {
        assert(version == 0);
        const point: Point = try in.readStructEndian(Point, .little);
        const is_pattern: bool = try readBool(in);
        // TODO: who clears this memory?
        const value_bytes = try readString(in, mem.gpa);
        const value = try core.parsing.parseSingleSexpr(value_bytes, &mem.pool_for_sexprs);
        dst.* = try .fromSexpr(&mem.hover_pool, value, point, is_pattern);
    }

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
        // TODO: make more efficient (don't replace the whole hovered if not needed)
        sexpr.hovered = try sexpr.hovered.fillVariables(sexpr.value, bindings, &mem.hover_pool);
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
        // TODO: use the actual screen resolution
        if (actual_point.scale * 1000 < camera.size.y) return;
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
                try if (is_pattern)
                    drawer.drawPatternWildcardLinesNonRecursive(camera, pair.left, pair.right, actual_point, alpha)
                else
                    drawer.drawTemplateWildcardLinesNonRecursive(camera, pair.left, pair.right, actual_point, maybe_bindings orelse .{
                        .anim_t = null,
                        .old = &.{},
                        .new = &.{},
                    }, alpha);
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
                                break;
                            }
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
        // dont draw out of bounds sexprs
        if (!camera.plusMargin(sexpr.point.scale * 3).contains(sexpr.point.pos)) return;

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
    fnkname_call: ?VeryPhysicalSexpr,
    fnkname_response: ?VeryPhysicalSexpr,
    bindings: []const core.Binding,

    pub fn draw(pill: Pill, alpha: f32, drawer: *Drawer, camera: Rect) !void {
        const bindings: BindingsState = .{
            .anim_t = null,
            .new = &.{},
            .old = pill.bindings,
        };
        try pill.input.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        try pill.pattern.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        if (pill.fnkname_call) |f| if (!f.isEmpty()) try f.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
        if (pill.fnkname_response) |f| if (!f.isEmpty()) try f.drawWithBindingsAndAlpha(bindings, alpha, drawer, camera);
    }
};

/// limited lifetime, basically particles, but the last_input remains (TODO: redesign)
const ExecutionTrace = struct {
    pills: std.ArrayListUnmanaged(Pill),
    last_input: ?VeryPhysicalSexpr,
    // bindings: ?BindingsState,

    // TODO: rethink
    velocity: Vec2,
    remaining_lifetime: f32,

    pub const empty: ExecutionTrace = .{ .pills = .empty };

    pub fn fromExecutor(
        pills: []const Pill,
        input: ?*const VeryPhysicalSexpr,
        velocity: Vec2,
        lifetime: f32,
        mem: *core.VeryPermamentGameStuff,
        hover: *HoveredSexpr.Pool,
    ) !ExecutionTrace {
        return .{
            .pills = .fromOwnedSlice(try mem.gpa.dupe(Pill, pills)),
            .last_input = if (input) |f| try f.clone(hover) else null,
            .velocity = velocity,
            .remaining_lifetime = lifetime,
        };
    }

    pub fn deinit(execution_trace: *ExecutionTrace, gpa: std.mem.Allocator) void {
        execution_trace.pills.deinit(gpa);
    }

    pub fn draw(execution_trace: ExecutionTrace, drawer: *Drawer, camera: Rect) !void {
        const alpha: f32 = math.smoothstep(execution_trace.remaining_lifetime, 0, 0.4);
        for (execution_trace.pills.items) |p| try p.draw(alpha, drawer, camera);
        if (execution_trace.last_input) |f| try f.drawWithBindingsAndAlpha(null, 1, drawer, camera);
    }

    pub fn update(execution_trace: *ExecutionTrace, delta_seconds: f32) void {
        execution_trace.remaining_lifetime -= delta_seconds;
        const offset: Point = .{ .pos = execution_trace.velocity.scale(delta_seconds) };
        for (execution_trace.pills.items) |*p| {
            p.input.point = offset.applyToLocalPoint(p.input.point);
            p.pattern.point = offset.applyToLocalPoint(p.pattern.point);
            if (p.fnkname_call) |*f| f.point = offset.applyToLocalPoint(f.point);
            if (p.fnkname_response) |*f| f.point = offset.applyToLocalPoint(f.point);
        }
        if (execution_trace.last_input) |*f| f.point = offset.applyToLocalPoint(f.point);
    }
};

// automatically consumes garland cases when input is present
const Executor = struct {
    input: VeryPhysicalSexpr,
    garland: VeryPhysicalGarland,
    handle: Handle,

    animation: ?struct {
        t: f32 = 0,
        active_case: VeryPhysicalCase,
        matching: bool,
        invoked_fnk: ?VeryPhysicalGarland,
        parent_pill: ?usize,
        new_bindings: []const core.Binding,
        original_point: Point,
        garland_fnkname: ?VeryPhysicalSexpr,
        paused: bool = false,
    } = null,
    // execution_trace: ExecutionTrace = .empty,
    prev_pills: std.ArrayListUnmanaged(Pill) = .empty,
    enqueued_stack: std.ArrayListUnmanaged(struct { garland: VeryPhysicalGarland, parent_pill: usize }) = .empty,
    /// in 0..1; 1 is braked, 0.5 is normal speed, 0 is speedup
    brake_t: f32 = 0.5,

    const relative_input_point: Point = .{ .pos = .new(-1, 1.5) };
    const relative_garland_point: Point = .{ .pos = .new(4, 0) };
    const relative_crank_center: Point = .{ .pos = .new(-1, 4) };

    pub fn init(point: Point, hover_pool: *HoveredSexpr.Pool) !Executor {
        return .{
            .handle = .{ .point = point },
            .garland = .init(point.applyToLocalPoint(relative_input_point)),
            .input = try .empty(point.applyToLocalPoint(relative_input_point), hover_pool, false),
        };
    }

    pub fn draw(executor: Executor, holding: VeryPhysicalCase.Holding, drawer: *Drawer, camera: Rect) !void {
        try executor.handle.draw(drawer, camera, 1);
        // const bindings_anim_t: ?f32 = if (executor.animation) |anim| if (anim.t < 0.2) null else math.remapTo01Clamped(anim.t, 0.2, 0.8) else null;
        const bindings_active: BindingsState = if (executor.animation) |anim| .{
            .anim_t = if (anim.t < 0.2) null else math.remapTo01Clamped(anim.t, 0.2, 0.8),
            .old = if (anim.parent_pill) |k| executor.prev_pills.items[k].bindings else &.{},
            .new = anim.new_bindings,
        } else .{
            .anim_t = null,
            .old = &.{},
            .new = &.{},
        };
        try executor.input.drawWithBindings(bindings_active, drawer, camera);
        if (executor.animation) |anim| {
            try anim.active_case.drawWithBindings(bindings_active, holding, drawer, camera);
            if (anim.garland_fnkname) |f| try f.draw(drawer, camera);
            if (anim.invoked_fnk) |f| try f.draw(holding, drawer, camera);
        }
        if (CRANKS_ENABLED) {
            if (executor.crankHandle()) |c| {
                const crank_center = executor.handle.point.applyToLocalPoint(relative_crank_center);
                drawer.canvas.fillCircleV2(camera, math.Circle.fromPoint(crank_center).scale(1.0), .gray(0.6));
                drawer.canvas.fillCircleV2(camera, c, .white);
            }
            drawer.canvas.line(camera, &kommon.funktional.mapOOP(
                &executor,
                .brakePath,
                &kommon.funktional.linspace01(10, true),
            ), executor.handle.point.scale * 0.1, .gray(0.8));
            drawer.canvas.fillCircleV2(camera, executor.brakeHandle(), .white);
        }
        for (executor.prev_pills.items) |p| try p.draw(1, drawer, camera);
        // TODO: revise that .new is correct
        for (executor.enqueued_stack.items) |s| try s.garland.drawWithBindings(.{
            .anim_t = bindings_active.anim_t,
            .new = &.{},
            .old = executor.prev_pills.items[s.parent_pill].bindings,
        }, holding, drawer, camera);
        try executor.garland.draw(holding, drawer, camera);
    }

    pub fn crankMovedTo(executor: *Executor, pos: Vec2) !void {
        assert(executor.animation != null);
        executor.brake_t = 1;
        const crank_center = executor.handle.point.applyToLocalPoint(relative_crank_center);
        const relative_pos = crank_center.inverseApplyGetLocalPosition(pos);
        const raw_t = relative_pos.getTurns();
        const cur_t = executor.animation.?.t;
        const target_t = math.clamp01(math.mod(raw_t, cur_t - 0.5, cur_t + 0.5));
        executor.animation.?.t = target_t;
    }

    pub fn brakeMovedTo(executor: *Executor, pos: Vec2) !void {
        const S = struct {
            p: Vec2,
            e: *const Executor,
            pub fn score(ctx: @This(), t: f32) f32 {
                return ctx.e.brakePath(t).sub(ctx.p).magSq();
            }
        };
        const raw_t = kommon.funktional.findFunctionMin(
            S,
            .{ .p = pos, .e = executor },
            0,
            1,
            10,
            0.0001,
        );
        executor.brake_t = raw_t;
    }

    pub fn brakePath(executor: *const Executor, t: f32) Vec2 {
        const crank_center = executor.handle.point.applyToLocalPoint(relative_crank_center);
        return crank_center
            .applyToLocalPosition(.fromPolar(1.5, math.remapFrom01(t, 0.125, 0.375)))
            .rotateAround(crank_center.applyToLocalPosition(.new(0.4, 0.25)), 0.1)
            .addY(0.25);
    }

    pub fn crankHandle(executor: *const Executor) ?math.Circle {
        if (executor.animation) |anim| {
            const crank_center = executor.handle.point.applyToLocalPoint(relative_crank_center);
            return .{
                .center = crank_center.applyToLocalPosition(.fromPolar(1, anim.t)),
                .radius = crank_center.scale * 0.2,
            };
        } else return null;
    }

    pub fn brakeHandle(executor: *const Executor) math.Circle {
        return .{
            .center = executor.brakePath(executor.brake_t),
            .radius = executor.handle.point.scale * 0.2,
        };
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

    pub fn animating(executor: Executor) bool {
        return executor.animation != null;
    }

    pub fn startedExecution(executor: Executor) bool {
        return executor.animation == null and executor.garland.cases.items.len > 0 and !executor.input.isEmpty();
    }

    // try core.fillTemplateV2(case.template.value, new_bindings.items, &mem.pool_for_sexprs)
    pub fn update(executor: *Executor, mem: *core.VeryPermamentGameStuff, known_fnks: []const Fnkbox, hover_pool: *HoveredSexpr.Pool, delta_seconds: f32) !void {
        executor.garland.handle.point.lerp_towards(executor.garlandPoint(), 0.6, delta_seconds);
        var pill_offset: f32 = 0;
        // var this_frame_ended_an_execution_without_direct_next: bool = false;
        var parent_pill_index: ?usize = null;
        if (executor.animation) |*animation| {
            assert(executor.garland.fnkname == null);
            animation.t += delta_seconds * speedScale(executor.brake_t);
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
                executor.garland.updateWithOffset(offset_t, &animation.active_case, delta_seconds);
                animation.active_case.kinematicUpdate(case_floating_away, null, null, delta_seconds);
                executor.input.point.lerp_towards(executor.inputPoint(), 0.6, delta_seconds);
                if (animation.garland_fnkname) |*f| f.point.lerp_towards(executor.inputPoint().applyToLocalPoint(.{ .pos = .new(3, -1.5), .turns = 0.25, .scale = 0.5 }), 0.6, delta_seconds);
            } else {
                const match_t = math.remapClamped(anim_t, 0, 0.2, 0, 1);
                // const bindings_t: ?f32 = if (anim_t < 0.2) null else math.remapTo01Clamped(anim_t, 0.2, 0.8);
                const invoking_t = math.remapClamped(anim_t, 0.0, 0.7, 0, 1);
                const enqueueing_t = math.remapClamped(anim_t, 0.2, 1, 0, 1);
                const discarded_t = anim_t;
                pill_offset = enqueueing_t;

                if (!EXECUTOR_MOVES_LEFT) {
                    executor.handle.pos = animation.original_pos.addX(enqueueing_t * 5);
                }

                const case_point = executor.firstCasePoint().applyToLocalPoint(
                    .{ .pos = .new(-match_t - enqueueing_t * 5, 0) },
                );

                executor.garland.kinematicUpdate(
                    executor.garlandPoint()
                        .applyToLocalPoint(.lerp(.{}, .{ .turns = 0.2, .scale = 0, .pos = .new(-4, 8) }, discarded_t)),
                    &animation.active_case,
                    delta_seconds,
                );
                if (animation.invoked_fnk) |*invoked| {
                    const offset = (1.0 - invoking_t) + 2.0 * math.smoothstepEased(invoking_t, 0.4, 0.0, .linear);
                    const function_point = executor.garlandPoint()
                        .applyToLocalPoint(.{ .pos = .new(2 * offset + 6 - match_t - enqueueing_t * 5, 6 * offset) });
                    invoked.kinematicUpdate(function_point, null, delta_seconds);

                    animation.active_case.kinematicUpdate(case_point, .{
                        .pos = .new(enqueueing_t * 6, -2 * enqueueing_t),
                        .turns = math.lerp(0, -0.1, math.smoothstepEased(enqueueing_t, 0, 1, .easeInOutCubic)),
                    }, .{ .pos = .new(-invoking_t * 4, 0) }, delta_seconds);

                    for (executor.enqueued_stack.items, 0..) |*x, k| {
                        x.garland.kinematicUpdate(case_point
                            .applyToLocalPoint(.{ .pos = VeryPhysicalCase.next_garland_offset })
                            .applyToLocalPoint(.{ .pos = .new(anim_t * 12 + 6 * tof32(k), -2), .turns = -0.1 }), null, delta_seconds);
                    }
                } else {
                    animation.active_case.kinematicUpdate(case_point, .{
                        .pos = .new(-enqueueing_t * 2, 0),
                    }, .{ .pos = .new(-invoking_t * 4, 0) }, delta_seconds);
                    for (executor.enqueued_stack.items, 0..) |*x, k| {
                        const et = 1 - enqueueing_t;
                        if (k + 1 == executor.enqueued_stack.items.len) {
                            x.garland.kinematicUpdate(executor.garlandPoint().applyToLocalPoint(.{
                                .pos = .new(et * 6 + 2 - enqueueing_t * 2, -2 * et),
                                .turns = math.lerp(0, -0.1, math.smoothstepEased(et, 0, 1, .easeInOutCubic)),
                            }), null, delta_seconds);
                        } else {
                            x.garland.kinematicUpdate(executor.garlandPoint().applyToLocalPoint(.{
                                .pos = .new((1 - anim_t) * 6 + 2 + 6 * tof32(executor.enqueued_stack.items.len - k - 1), -2),
                                .turns = -0.1,
                            }), null, delta_seconds);
                        }
                    }
                }
                executor.input.point = executor.inputPoint().applyToLocalPoint(.{ .pos = .new(-enqueueing_t * 5, 0) });
                if (animation.garland_fnkname) |*f| f.point = executor.input.point.applyToLocalPoint(.{ .pos = .new(3, -1.5), .turns = 0.25, .scale = 0.5 });
            }
            if (animation.t >= 1) {
                if (animation.matching) {
                    try executor.prev_pills.append(mem.gpa, .{
                        .pattern = animation.active_case.pattern,
                        .input = executor.input,
                        .fnkname_call = animation.active_case.fnk_name,
                        .fnkname_response = animation.garland_fnkname,
                        // TODO: should include previous bindings? not really, since they have now been merged
                        .bindings = try mem.gpa.dupe(core.Binding, animation.new_bindings),
                    });
                    pill_offset -= 1;
                    try animation.active_case.fillVariables(animation.new_bindings, mem);
                    executor.input = animation.active_case.template;

                    if (animation.invoked_fnk) |*fnk| {
                        executor.garland = fnk.*;
                        if (animation.active_case.next.cases.items.len > 0) {
                            try executor.enqueued_stack.append(mem.gpa, .{ .garland = animation.active_case.next, .parent_pill = executor.prev_pills.items.len - 1 });
                        }
                    } else if (animation.active_case.next.cases.items.len > 0) {
                        executor.garland = animation.active_case.next;
                        // this_frame_ended_an_execution_without_direct_next = true;
                        parent_pill_index = executor.prev_pills.items.len - 1;
                    } else if (executor.enqueued_stack.pop()) |g| {
                        executor.garland = g.garland;
                        // this_frame_ended_an_execution_without_direct_next = true;
                        parent_pill_index = g.parent_pill;
                    } else {
                        executor.garland = .init(executor.garlandPoint());
                    }
                } else {
                    assert(animation.new_bindings.len == 0);
                    executor.garland.fnkname = animation.garland_fnkname;
                }
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
            if (pill.fnkname_call) |*f| f.point.lerp_towards(pill_input_pos.applyToLocalPoint(.{ .pos = .new(8, -3), .turns = 0.25, .scale = 0.5 }), 0.6, delta_seconds);
            if (pill.fnkname_response) |*g| g.point.lerp_towards(pill_input_pos.applyToLocalPoint(.{ .pos = .new(3, -1.5), .turns = 0.25, .scale = 0.5 }), 0.6, delta_seconds);
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
            const garland_fnkname = executor.garland.fnkname;
            executor.garland.fnkname = null;
            executor.animation = .{
                .active_case = case,
                .matching = matching,
                .invoked_fnk = invoked_fnk,
                .new_bindings = try new_bindings.toOwnedSlice(),
                .original_point = executor.handle.point,
                .garland_fnkname = garland_fnkname,
                .parent_pill = parent_pill_index,
            };
        }
    }

    pub fn inputPoint(executor: Executor) Point {
        return executor.handle.point.applyToLocalPoint(Executor.relative_input_point);
    }

    pub fn firstCasePoint(executor: Executor) Point {
        return executor.garlandPoint().applyToLocalPoint(.{ .pos = .new(0, 1.5) });
    }

    pub fn garlandPoint(executor: Executor) Point {
        return executor.handle.point.applyToLocalPoint(Executor.relative_garland_point);
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
    const fnkname_from_garland_template: Point = Point.inverseApplyGetLocal(relative_garland_point, relative_fnkname_point);
    const fnkname_from_garland_pattern: Point = fnkname_from_garland_template.applyToLocalPoint(.{ .pos = .new(3, 0) });

    pub fn point(fnkviewer: *const Fnkviewer) Point {
        return fnkviewer.handle.point;
    }

    pub fn init(base: Point, hover_pool: *HoveredSexpr.Pool) !Fnkviewer {
        return .{
            .handle = .{ .point = base },
            .garland = .init(base.applyToLocalPoint(relative_garland_point)),
            .fnkname = try .empty(base.applyToLocalPoint(relative_fnkname_point), hover_pool, false),
        };
    }

    pub fn draw(fnkviewer: Fnkviewer, holding: VeryPhysicalCase.Holding, drawer: *Drawer, camera: Rect) !void {
        try fnkviewer.handle.draw(drawer, camera, 1);
        try fnkviewer.fnkname.draw(drawer, camera);
        try fnkviewer.garland.draw(holding, drawer, camera);
    }

    pub fn update(fnkviewer: *Fnkviewer, mem: *core.VeryPermamentGameStuff, known_fnks: []const Fnkbox, hover_pool: *HoveredSexpr.Pool, delta_seconds: f32) !void {
        fnkviewer.garland.handle.point.lerp_towards(fnkviewer.point().applyToLocalPoint(relative_garland_point), 0.6, delta_seconds);
        fnkviewer.garland.update(delta_seconds);
        fnkviewer.fnkname.point.lerp_towards(fnkviewer.point().applyToLocalPoint(relative_fnkname_point), 0.6, delta_seconds);

        // TODO: better invocation anim
        const cur_fnkname_hash = fnkviewer.fnkname.value.hash();
        if (cur_fnkname_hash != fnkviewer.last_fnkname_hash) {
            fnkviewer.last_fnkname_hash = cur_fnkname_hash;
            if (try getGarlandForFnk(known_fnks, fnkviewer.fnkname.value, fnkviewer.point().applyToLocalPoint(relative_garland_point), mem, hover_pool)) |garland| {
                fnkviewer.garland = garland;
                // TODO: handle this better
                fnkviewer.garland.fnkname = null;
            }
        }
    }
};

pub const TestCase = struct {
    input: VeryPhysicalSexpr,
    expected: VeryPhysicalSexpr,
    actual: VeryPhysicalSexpr,
    // tested: bool = false,
    play_button: Button,

    pub fn save(testcase: *const TestCase, out: std.io.AnyWriter, scratch: std.mem.Allocator) anyerror!void {
        try testcase.input.save(out, scratch);
        try testcase.expected.save(out, scratch);
        try testcase.actual.save(out, scratch);
    }

    pub fn load(dst: *TestCase, in: std.io.AnyReader, version: u32, mem: *core.VeryPermamentGameStuff) anyerror!void {
        assert(version == 0);
        dst.play_button = .{ .kind = .launch_testcase, .rect = .unit };
        try dst.input.load(in, version, mem);
        try dst.expected.load(in, version, mem);
        try dst.actual.load(in, version, mem);
    }

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
    active_t: f32 = 0,
    rect: Rect,
    enabled: bool = true,
    kind: enum { unknown, launch_testcase, see_failing_case, postit },

    pub fn draw(button: *const Button, drawer: *Drawer, camera: Rect) !void {
        switch (button.kind) {
            .postit => {
                // drawer.canvas.fillRect(camera, button.rect, .fromHex("#FFEBA1"));
                const t: f32 = 2.0 + button.hot_t * 0.7 + button.active_t * 1.2;
                drawer.canvas.fillShape(camera, .{ .pos = button.rect.getCenter(), .scale = button.rect.size.y / 2.0 }, try drawer.canvas.tmpShape(&.{
                    .new(-1, -1),
                    .new(1, -1),
                    .new(1, 1 - t * 0.1),
                    .new(1 - t * 0.25, 1),
                    .new(-1, 1),
                }), .fromHex("#FFEBA1"));
                drawer.canvas.fillShape(camera, .{ .pos = button.rect.getCenter(), .scale = button.rect.size.y / 2.0 }, try drawer.canvas.tmpShape(&.{
                    .new(1, 1 - t * 0.1),
                    .new(1 - t * 0.25, 1),
                    Vec2.new(1, 1).mirrorAroundSegment(
                        .new(1, 1 - t * 0.1),
                        .new(1 - t * 0.25, 1),
                    ),
                }), .fromHex("#d4bd68"));
            },
            .unknown => {
                drawer.canvas.fillRect(camera, button.rect, COLORS.bg);
                drawer.canvas.borderRect(camera, button.rect, math.lerp(0.05, 0.1, button.hot_t), .inner, .black);
            },
            .launch_testcase => {
                drawer.canvas.fillRect(camera, button.rect, .gray(0.4));
                const rect = button.rect.move(Vec2.new(-1, -1).scale((1 - button.hot_t) * 0.05 + (1 - @min(button.active_t, button.hot_t)) * 0.1));
                drawer.canvas.fillRect(camera, rect, COLORS.bg);
                drawer.canvas.borderRect(camera, rect, 0.05, .inner, .black);
                drawer.canvas.line(camera, &.{
                    rect.getCenter().add(.new(-0.25, -0.25)).addX(0.15),
                    rect.getCenter().add(.new(0, 0)).addX(0.15),
                    rect.getCenter().add(.new(-0.25, 0.25)).addX(0.15),
                }, 0.05, .black);
            },
            .see_failing_case => {
                if (button.enabled) {
                    drawer.canvas.rectGradient(
                        camera,
                        button.rect,
                        .gray(0.75 + button.hot_t * 0.2 - button.active_t * 0.1),
                        .gray(0.95 - button.hot_t * 0.2 - button.active_t * 0.1),
                    );
                } else {
                    drawer.canvas.fillRect(camera, button.rect, .gray(0.7));
                    return;
                }
            },
        }
    }

    pub fn updateHot(button: *Button, self: Workspace.Focus.UiTarget.Kind, hot: Workspace.Focus.UiTarget, active: Workspace.Focus.UiTarget, delta_seconds: f32) void {
        button.updateHot2(hot.equals(.{ .kind = self }), active.equals(.{ .kind = self }), delta_seconds);
    }

    pub fn updateHot2(button: *Button, hot: bool, active: bool, delta_seconds: f32) void {
        math.lerp_towards(&button.hot_t, if (hot) 1 else 0, 0.6, delta_seconds);
        math.lerp_towards(&button.active_t, if (active) 1 else 0, 0.6, delta_seconds);
    }
};

// TODO: leave execution trace behind
// reel + definition + explanation
const Fnkbox = struct {
    handle: Handle,
    input: VeryPhysicalSexpr,
    fnkname: VeryPhysicalSexpr,
    garland: VeryPhysicalGarland,
    testcases: std.ArrayListUnmanaged(TestCase),
    scroll_testcases: f32 = 0,
    execution: ?struct {
        source: union(enum) {
            testcase: usize,
            input,
        },
        /// only valid if source is testcase
        old_testcase_actual_value: *const Sexpr,
        /// if source is input, this is ignored
        state: enum { scrolling_towards_case, starting, executing, ending },
        state_t: f32,
        executor: ?Executor,
        final_result: VeryPhysicalSexpr = undefined,
    } = null,
    text: []const u8,
    status: Status,
    status_bar: Button = .{ .rect = .unit, .kind = .see_failing_case },
    folded: bool,
    folded_t: f32,
    fold_button: Button = .{ .rect = .unit, .kind = .unknown },
    scroll_button_up: Button = .{ .rect = .unit, .kind = .unknown },
    scroll_button_down: Button = .{ .rect = .unit, .kind = .unknown },

    pub fn save(fnkbox: *const Fnkbox, out: std.io.AnyWriter, scratch: std.mem.Allocator) anyerror!void {
        try fnkbox.handle.save(out, scratch);
        try fnkbox.input.save(out, scratch);
        try fnkbox.fnkname.save(out, scratch);
        try fnkbox.garland.save(out, scratch);
        try writeString(out, fnkbox.text);
        try writeBool(out, fnkbox.folded);

        // TODO: don't save testcases for default fnks
        try out.writeInt(u32, @intCast(fnkbox.testcases.items.len), .little);
        for (fnkbox.testcases.items) |c| {
            try c.save(out, scratch);
        }
    }

    pub fn load(dst: *Fnkbox, in: std.io.AnyReader, version: u32, mem: *core.VeryPermamentGameStuff) anyerror!void {
        assert(version == 0);
        try dst.handle.load(in, version, mem);
        try dst.input.load(in, version, mem);
        try dst.fnkname.load(in, version, mem);
        try dst.garland.load(in, version, mem);
        dst.text = try readString(in, mem.gpa);
        dst.folded = try readBool(in);
        dst.folded_t = if (dst.folded) 1 else 0;
        dst.execution = null;

        // TODO: avoid jumping
        dst.status_bar = .{ .rect = .unit, .kind = .see_failing_case };
        dst.fold_button = .{ .rect = .unit };
        dst.scroll_button_up = .{ .rect = .unit };
        dst.scroll_button_down = .{ .rect = .unit };

        const n_testcases: usize = @intCast(try in.readInt(u32, .little));
        dst.testcases = .fromOwnedSlice(try mem.gpa.alloc(TestCase, n_testcases));
        for (dst.testcases.items) |*c| {
            try c.load(in, version, mem);
        }
    }

    pub const Status = union(enum) {
        /// index of the failing testcase
        unsolved: usize,
        // TODO: score
        solved,
    };

    const relative_fnkname_point: Point = .{ .pos = .new(-1, 1), .scale = 0.5, .turns = 0.25 };
    const relative_bottom_testcase_point: Point = .{ .pos = .new(0, box_height - 0.5) };
    const text_height: f32 = 2.5;
    const status_bar_height: f32 = 1;
    const testcases_header_height: f32 = 0.75;
    const testcases_height: f32 = 2.5 * visible_testcases;
    const box_height = text_height + status_bar_height + testcases_header_height + testcases_height;
    const visible_testcases = 2;

    pub fn point(fnkbox: *const Fnkbox) Point {
        return fnkbox.handle.point;
    }

    pub fn executorPoint(fnkbox: *const Fnkbox) Point {
        const relative_executor_point: Point = .{ .pos = .new(-3, 1) };
        const offset_y = box_height * (1 - fnkbox.folded_t);
        return fnkbox.point().applyToLocalPoint(relative_executor_point).applyToLocalPoint(.{ .pos = .new(0, offset_y) });
    }

    pub fn inputPoint(fnkbox: *const Fnkbox) Point {
        return fnkbox.executorPoint().applyToLocalPoint(Executor.relative_input_point);
        // const relative_input_point: Point = .{ .pos = .new(-4, 2.5) };
        // const offset_y = box_height * (1 - fnkbox.folded_t);
        // return fnkbox.point().applyToLocalPoint(relative_input_point).applyToLocalPoint(.{ .pos = .new(0, offset_y) });
    }

    pub fn garlandPoint(fnkbox: *const Fnkbox) Point {
        return fnkbox.executorPoint().applyToLocalPoint(Executor.relative_garland_point);
        // const relative_garland_point: Point = .{ .pos = .new(1, 1) };
        // const offset_y = box_height * (1 - fnkbox.folded_t);
        // return fnkbox.point().applyToLocalPoint(relative_garland_point).applyToLocalPoint(.{ .pos = .new(0, offset_y) });
    }

    pub fn init(
        text: []const u8,
        fnkname: *const Sexpr,
        base: Point,
        testcases_values: []const Sample,
        initial_definition: ?core.FnkBodyV2,
        hover_pool: *HoveredSexpr.Pool,
        mem: *core.VeryPermamentGameStuff,
    ) !Fnkbox {
        var geo: Fnkbox = undefined;
        geo.handle = .{ .point = base };
        geo.folded = false;
        geo.folded_t = 0;
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
        const result: Fnkbox = .{
            .text = text,
            .handle = .{ .point = base },
            .input = try .empty(geo.inputPoint(), hover_pool, false),
            .garland = if (initial_definition) |def|
                try .fromDefinition(geo.garlandPoint(), def, mem, hover_pool)
            else
                .init(geo.garlandPoint()),
            .fnkname = try .fromSexpr(hover_pool, fnkname, base.applyToLocalPoint(relative_fnkname_point), true),
            .testcases = testcases,
            .folded = false,
            .folded_t = 0,
            .status = undefined,
        };
        return result;
    }

    pub fn deinit(fnkbox: *Fnkbox, gpa: std.mem.Allocator) void {
        fnkbox.testcases.deinit(gpa);
        fnkbox.garland.deinit(gpa);
    }

    pub fn cloneAsEmpty(
        original: *const Fnkbox,
        new_fnkname: *const Sexpr,
        mem: *core.VeryPermamentGameStuff,
    ) !Fnkbox {
        const result: Fnkbox = .{
            .text = "custom fnk",
            .handle = .{ .point = original.handle.point },
            .input = try .empty(original.inputPoint(), &mem.hover_pool, false),
            .garland = .init(original.garlandPoint()),
            .fnkname = try .fromSexpr(&mem.hover_pool, new_fnkname, original.fnkname.point, true),
            .testcases = .empty,
            .folded = original.folded,
            .folded_t = original.folded_t,
            .status = undefined,
        };
        return result;
    }

    pub fn box(fnkbox: *const Fnkbox) Rect {
        return .fromMeasureAndSizeV2(
            .top_center,
            fnkbox.point().pos.addY(0.75),
            Vec2.new(16, box_height).scale(1.0 - fnkbox.folded_t),
        );
    }

    pub fn statusBarGoal(fnkbox: *const Fnkbox) Rect {
        return .fromMeasureAndSizeV2(
            .top_center,
            fnkbox.point().pos.addY(0.75).addY(text_height),
            Vec2.new(16, status_bar_height),
        );
    }

    pub fn testcasesBoxUnfolded(fnkbox: *const Fnkbox) Rect {
        return .fromMeasureAndSizeV2(
            .top_center,
            fnkbox.point().pos.addY(0.75).addY(text_height + status_bar_height + testcases_header_height),
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

    pub fn draw(fnkbox: *const Fnkbox, holding: VeryPhysicalCase.Holding, drawer: *Drawer, camera: Rect) !void {
        const rect = fnkbox.box();
        drawer.canvas.fillRect(camera, rect, COLORS.bg.withAlpha(0.65));
        if (fnkbox.folded_t < 1) {
            {
                drawer.canvas.gl.startStencil();
                drawer.canvas.fillRect(camera, rect, .white);
                drawer.canvas.gl.doneStencil();
                defer drawer.canvas.gl.stopStencil();
                try drawer.canvas.drawText(0, camera, fnkbox.text, .centeredAt(fnkbox.handle.point.pos.addY(0.75 + text_height / 2.0)), 0.8, .black);

                try fnkbox.status_bar.draw(drawer, camera);
                switch (fnkbox.status) {
                    .solved => {
                        try drawer.canvas.drawText(0, camera, "Solved!", .centeredAt(fnkbox.statusBarGoal().getCenter()), 0.8, .black);
                    },
                    .unsolved => {
                        try drawer.canvas.drawText(0, camera, "Unsolved!", .centeredAt(fnkbox.statusBarGoal().getCenter()), 0.75, .black);
                    },
                }

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
                const min_k: usize = @intFromFloat(@max(0, fnkbox.scroll_testcases - 5));
                const max_k: usize = @min(fnkbox.testcases.items.len, @as(usize, @intFromFloat(fnkbox.scroll_testcases + visible_testcases + 5)));
                for (min_k..max_k) |k| {
                    try fnkbox.testcases.items[k].draw(drawer, camera);
                }
                // for (fnkbox.testcases.items) |t| {
                //     try t.draw(drawer, camera);
                // }
                try fnkbox.scroll_button_up.draw(drawer, camera);
                try fnkbox.scroll_button_down.draw(drawer, camera);
            }
        }
        drawer.canvas.borderRect(camera, rect, 0.05, .inner, .black);
        if (fnkbox.execution == null or !fnkbox.folded) try fnkbox.fnkname.draw(drawer, camera);
        try fnkbox.handle.draw(drawer, camera, 1);
        if (fnkbox.execution) |e| {
            if (e.state == .ending) {
                try fnkbox.garland.drawWithAlpha(math.smoothstep(e.state_t, 0.9, 1), holding, drawer, camera);
                try e.final_result.draw(drawer, camera);
            } else if (e.state == .scrolling_towards_case) {
                try fnkbox.garland.drawWithAlpha(1, holding, drawer, camera);
            } else {
                try e.executor.?.draw(holding, drawer, camera);
            }
        } else {
            try fnkbox.garland.draw(holding, drawer, camera);
            try fnkbox.input.draw(drawer, camera);
        }
        try fnkbox.fold_button.draw(drawer, camera);
    }

    pub fn updateUiHotness(fnkbox: *Fnkbox, fnkbox_index: usize, ui_hot: Workspace.Focus.UiTarget, ui_active: Workspace.Focus.UiTarget, delta_seconds: f32) void {
        fnkbox.fold_button.updateHot(.{ .fnkbox_toggle_fold = fnkbox_index }, ui_hot, ui_active, delta_seconds);
        fnkbox.status_bar.updateHot(.{ .fnkbox_see_failing_case = fnkbox_index }, ui_hot, ui_active, delta_seconds);
        fnkbox.fold_button.updateHot(.{ .fnkbox_toggle_fold = fnkbox_index }, ui_hot, ui_active, delta_seconds);
        fnkbox.scroll_button_up.updateHot(.{ .fnkbox_scroll = .{ .fnkbox = fnkbox_index, .direction = .up } }, ui_hot, ui_active, delta_seconds);
        fnkbox.scroll_button_down.updateHot(.{ .fnkbox_scroll = .{ .fnkbox = fnkbox_index, .direction = .down } }, ui_hot, ui_active, delta_seconds);
        for (fnkbox.testcases.items, 0..) |*testcase, testcase_index| {
            testcase.play_button.updateHot(.{ .fnkbox_launch_testcase = .{ .fnkbox = fnkbox_index, .testcase = testcase_index } }, ui_hot, ui_active, delta_seconds);
        }
    }

    pub fn updateStatus(fnkbox: *Fnkbox, known_fnks: []const Fnkbox, mem: *core.VeryPermamentGameStuff) !void {
        // TODO: improve somehow
        // TODO: leaks?
        var all_fnks: FnkCollection = .init(mem.scratch.allocator());
        for (known_fnks) |k| {
            try all_fnks.putNoClobber(k.fnkname.value, try k.garland.toDefinition(mem.scratch.allocator()));
        }
        var temp_mem: core.VeryPermamentGameStuff = .init(mem.scratch.allocator());
        defer temp_mem.deinit();
        var scoring_run: core.ScoringRun = try .initFromFnks(all_fnks, &temp_mem);
        defer scoring_run.deinit(false);
        // Update 'actual' values
        for (fnkbox.testcases.items) |*t| {
            var exec = try core.ExecutionThread.init(t.input.value, fnkbox.fnkname.value, &scoring_run);
            defer exec.deinit();

            const actual_output = exec.getFinalResultBoundedV2(&scoring_run, 10_000, true) catch |err| switch (err) {
                // TODO: "NoMatchingCase" is no longer an error
                error.FnkNotFound,
                error.NoMatchingCase,
                error.UsedUndefinedVariable,
                error.InvalidMetaFnk,
                error.TookTooLong,
                => Sexpr.builtin.empty,
                error.OutOfMemory => return err,
                error.BAD_INPUT => @panic("panic"),
            };
            if (!actual_output.equals(t.actual.value) and fnkbox.execution == null) {
                t.actual = try VeryPhysicalSexpr.fromSexpr(
                    &mem.hover_pool,
                    try mem.deepCloneSexpr(false, actual_output),
                    t.actual.point,
                    false,
                );
            }
        }
        // Get the actual status
        for (fnkbox.testcases.items, 0..) |t, k| {
            const actual_value: *const Sexpr = if (fnkbox.execution) |execution|
                switch (execution.source) {
                    .testcase => |k_source| if (k == k_source)
                        execution.old_testcase_actual_value
                    else
                        t.actual.value,
                    .input => t.actual.value,
                }
            else
                t.actual.value;
            if (!actual_value.equals(t.expected.value)) {
                fnkbox.status = .{ .unsolved = k };
                return;
            }
        }
        fnkbox.status = .solved;
    }

    pub fn update(fnkbox: *Fnkbox, mem: *core.VeryPermamentGameStuff, known_fnks: []const Fnkbox, hover_pool: *HoveredSexpr.Pool, delta_seconds: f32) !?ExecutionTrace {
        var result: ?ExecutionTrace = null;
        math.lerp_towards_range(&fnkbox.scroll_testcases, 0, @max(0, tof32(fnkbox.testcases.items.len) - visible_testcases), 0.6, delta_seconds);
        fnkbox.fold_button.rect.lerpTowards(fnkbox.foldButtonGoal(), 0.6, delta_seconds);
        fnkbox.status_bar.rect.lerpTowards(fnkbox.statusBarGoal(), 0.6, delta_seconds);
        fnkbox.status_bar.enabled = switch (fnkbox.status) {
            .solved => false,
            .unsolved => true,
        };
        fnkbox.scroll_button_up.rect.lerpTowards(fnkbox.testcasesBoxUnfolded().withSize(.new(1.2, 0.7), .top_left).plusMargin(-0.1), 0.6, delta_seconds);
        fnkbox.scroll_button_down.rect.lerpTowards(fnkbox.testcasesBoxUnfolded().withSize(.new(1.2, 0.7), .bottom_left).plusMargin(-0.1), 0.6, delta_seconds);
        math.lerp_towards(&fnkbox.folded_t, if (fnkbox.folded) 1 else 0, 0.6, delta_seconds);
        fnkbox.input.point.lerp_towards(fnkbox.inputPoint(), 0.6, delta_seconds);
        fnkbox.garland.handle.point.lerp_towards(fnkbox.garlandPoint(), 0.6, delta_seconds);
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
            switch (execution.source) {
                .testcase => |testcase_index| switch (execution.state) {
                    .scrolling_towards_case => {
                        const min: f32 = tof32(testcase_index) - visible_testcases + 1;
                        const max: f32 = tof32(testcase_index);
                        math.lerp_towards_range(&fnkbox.scroll_testcases, min, max, 0.1, delta_seconds);
                        math.towards_range(&fnkbox.scroll_testcases, tof32(testcase_index) - visible_testcases + 1, tof32(testcase_index), delta_seconds * 0.1);
                        if (math.inRangeClosed(fnkbox.scroll_testcases, min, max)) {
                            execution.state = .starting;
                            execution.state_t = 0;
                            execution.executor = .{
                                .input = try fnkbox.testcases.items[testcase_index].input.dupeSubValue(&.{}, hover_pool),
                                .garland = try fnkbox.garland.clone(mem.gpa, hover_pool),
                                .handle = .{ .point = fnkbox.executorPoint() },
                                .animation = null,
                            };
                            execution.executor.?.input.point = fnkbox.testcases.items[testcase_index].input.point;
                        }
                    },
                    .starting => {
                        execution.executor.?.input.point = .lerp(
                            fnkbox.testcases.items[testcase_index].input.point,
                            execution.executor.?.inputPoint(),
                            execution.state_t,
                        );
                        execution.state_t += delta_seconds / 0.8;
                        if (execution.state_t >= 1) {
                            execution.state = .executing;
                            execution.state_t = 0;
                        }
                    },
                    .executing => {
                        const executor = &execution.executor.?;
                        try executor.update(mem, known_fnks, hover_pool, delta_seconds);
                        if (execution.executor.?.animation == null) {
                            execution.state = .ending;
                            execution.state_t = 0;
                            execution.final_result = try executor.input.clone(hover_pool);
                            result = try .fromExecutor(executor.prev_pills.items, null, .new(0, 0), 0.75, mem, hover_pool);
                            execution.executor = null;
                        }
                    },
                    .ending => {
                        execution.final_result.point = .lerp(
                            fnkbox.inputPoint(),
                            fnkbox.testcases.items[testcase_index].expected.point.applyToLocalPoint(.{ .pos = .new(4, 0) }),
                            execution.state_t,
                        );

                        {
                            const min: f32 = tof32(testcase_index) - visible_testcases + 1;
                            const max: f32 = tof32(testcase_index);
                            math.lerp_towards_range(&fnkbox.scroll_testcases, min, max, 0.3, delta_seconds);
                            math.towards_range(&fnkbox.scroll_testcases, tof32(testcase_index) - visible_testcases + 1, tof32(testcase_index), delta_seconds * 0.2);
                        }

                        execution.state_t += delta_seconds / 0.8;
                        if (execution.state_t >= 1) {
                            fnkbox.testcases.items[testcase_index].actual = execution.final_result;
                            // TODO: memory leak here
                            fnkbox.execution = null;
                            // TODO: call this somewhere else
                            try fnkbox.updateStatus(known_fnks, mem);
                        }
                    },
                },
                .input => {
                    const executor = &execution.executor.?;
                    try executor.update(mem, known_fnks, hover_pool, delta_seconds);
                    if (executor.animation == null) {
                        result = try .fromExecutor(executor.prev_pills.items, &executor.input, .new(-5, 0), 0.75, mem, hover_pool);
                        fnkbox.execution = null;
                        fnkbox.input = try .empty(fnkbox.inputPoint(), hover_pool, false);
                    }
                },
            }

            // TODO: this is ignored when the animation is active
            // e.handle.pos.lerpTowards(fnkbox.point().applyToLocalPoint(relative_executor_point).pos, 0.6, delta_seconds);
        }
        return result;
    }
};

// in-world notes/tutorials
// TODO: player should be able to draw on these, freehand
pub const Postit = struct {
    button: Button,
    lines: []const []const u8,

    pub fn fromText(lines: []const []const u8, center: Vec2) Postit {
        return .{
            .button = .{ .rect = .fromCenterAndSize(center, .both(6)), .kind = .postit },
            .lines = lines,
        };
    }

    pub fn draw(postit: Postit, drawer: *Drawer, camera: Rect) !void {
        try postit.button.draw(drawer, camera);
        for (postit.lines, 0..) |line, k| {
            try drawer.canvas.drawText(0, camera, line, .centeredAt(postit.button.rect.getCenter().addY(
                (tof32(k) - (tof32(postit.lines.len) - 1) / 2.0) * 1,
            )), 0.8, .black);
        }
    }
};

const ToolbarTrash = struct {
    rect: Rect,
    hot_t: f32 = 0,

    pub fn draw(trash: *const ToolbarTrash, drawer: *Drawer, camera: Rect) !void {
        const line_width = camera.size.y * 0.003;
        drawer.canvas.fillRect(camera, trash.rect, COLORS.bg);
        drawer.canvas.borderRect(camera, trash.rect, line_width * (1 + trash.hot_t), .inner, .black);
        const center = trash.rect.getCenter();
        drawer.canvas.line(camera, &.{
            center.add(Vec2.xneg_yneg.scale(line_width * 10)),
            center.add(Vec2.xpos_ypos.scale(line_width * 10)),
        }, line_width * 3, .black);
        drawer.canvas.line(camera, &.{
            center.add(Vec2.xneg_ypos.scale(line_width * 10)),
            center.add(Vec2.xpos_yneg.scale(line_width * 10)),
        }, line_width * 3, .black);
    }
};

pub const Lens = struct {
    source: Vec2,
    target: Vec2,
    comptime source_radius: f32 = 0.25,
    comptime target_radius: f32 = 1,
    source_hot_t: f32 = 0,
    target_hot_t: f32 = 0,
    tmp_visible_sexprs: std.ArrayListUnmanaged(struct {
        original_place: Workspace.BaseSexprPlace,
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

    // TODO: refactor to use Point?
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
            garland.fnkname = try .fromSexpr(hover_pool, fnkname, .{}, true);
            garland.kinematicUpdate(new_point, null, undefined);
            return garland;
        }
    } else return null;
}

pub const SerializedTag = enum(u32) {
    VeryPhysicalSexpr,
    VeryPhysicalCase,
    VeryPhysicalGarland,
    Fnkbox,
};

const Workspace = struct {
    lenses: std.ArrayList(Lens),
    sexprs: std.ArrayList(VeryPhysicalSexpr),
    cases: std.ArrayList(VeryPhysicalCase),
    garlands: std.ArrayList(VeryPhysicalGarland),
    executors: std.ArrayList(Executor),
    fnkviewers: std.ArrayList(Fnkviewer),
    fnkboxes: std.ArrayList(Fnkbox),
    traces: std.ArrayList(ExecutionTrace),
    toolbar_case: VeryPhysicalCase,
    toolbar_trash: ToolbarTrash,
    postits: std.ArrayList(Postit),

    focus: Focus = .{},
    camera: Rect = .fromCenterAndSize(.new(100, 4), Vec2.new(16, 9).scale(2.75)),

    toolbar_left: f32 = 0,

    toolbar_case_enabled: bool = false,

    undo_stack: std.ArrayList(UndoableCommand),
    random_instance: std.Random.DefaultPrng,
    hover_pool: HoveredSexpr.Pool,

    const Focus = struct {
        // TODO: Not really any Target, since for sexprs it's .grabbed with no local
        grabbing: Target = .nothing,
        ui_active: UiTarget = .nothing,
        /// only used for postits
        grabbing_offset: Vec2 = .zero,

        const UiTarget = struct {
            kind: Kind,

            pub const nothing: UiTarget = .{ .kind = .nothing };

            pub const Kind = union(enum) {
                nothing,
                fnkbox_toggle_fold: usize,
                fnkbox_see_failing_case: usize,
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
                executor_crank_handle: ExecutorPlace,
                executor_brake_handle: ExecutorPlace,
                fnkviewer_handle: usize,
                fnkbox_handle: usize,
                postit: usize,

                pub fn equals(a: Kind, b: Kind) bool {
                    return kommon.meta.eql(a, b);
                }

                pub fn immutable(kind: Kind) bool {
                    return switch (kind) {
                        else => false,
                        .sexpr => |s| s.base.immutable(),
                        .case_handle => |c| switch (c) {
                            else => false,
                            .toolbar => true,
                        },
                    };
                }
            };
        };

        const Value = union(enum) {
            sexpr: VeryPhysicalSexpr,
            case: VeryPhysicalCase,
            garland: VeryPhysicalGarland,
            // TODO: lenses, etc
        };
    };

    const CaseHandle = union(enum) {
        board: usize,
        garland: struct {
            local: usize,
            parent: GarlandHandle,
            existing_case: bool,
        },
        toolbar,

        pub fn exists(case_handle: CaseHandle) bool {
            return switch (case_handle) {
                .board, .toolbar => true,
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
        fnkbox_input: usize,
        fnkbox_testcase: struct {
            fnkbox: usize,
            testcase: usize,
            part: TestCase.Part,
        },

        pub fn immutable(place: BaseSexprPlace) bool {
            return switch (place) {
                .fnkbox_fnkname, .fnkbox_testcase => true,
                .case => |c| switch (c.parent) {
                    .toolbar => true,
                    else => false,
                },
                .board,
                .executor_input,
                .fnkviewer_fnkname,
                .fnkbox_input,
                => false,
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

    const ExecutorPlace = union(enum) {
        board: usize,
        fnkbox: usize,
    };

    const UndoableCommand = struct {
        specific: union(enum) {
            noop,
            // TODO: rethink/remove
            spawned_trace,
            // TODO: rethink/remove
            despawned_trace: struct {
                trace: ExecutionTrace,
                spawned_sexpr: bool,
            },
            deleted: struct {
                old_place: Focus.Target,
                value: Focus.Value,
            },
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
            grabbed: Grabbed,
            started_execution_fnkbox_from_input: struct {
                fnkbox: usize,
                input: VeryPhysicalSexpr,
            },
            started_execution: struct {
                executor: usize,
                input: VeryPhysicalSexpr,
                garland: VeryPhysicalGarland,
                prev_pills: []Pill,
                prev_point: Point,
            },
            fnkbox_launch_testcase: struct {
                fnkbox: usize,
                testcase: usize,
                old_actual: VeryPhysicalSexpr,
            },
            fnkbox_toggle_fold: usize,
            fnkbox_scroll: struct {
                fnkbox: usize,
                old_position: f32,
            },
        },

        pub const noop: UndoableCommand = .{ .specific = .noop };

        pub const Grabbed = struct {
            duplicate: bool,
            from: Focus.Target,
            /// not all fields are used
            old_data: OldData,

            pub const OldData = struct {
                point: Point,
                is_pattern: bool,
            };
        };
    };

    pub fn init(dst: *Workspace, mem: *core.VeryPermamentGameStuff, random_seed: u64) !void {
        dst.* = kommon.meta.initDefaultFields(Workspace);

        dst.random_instance = .init(random_seed);

        dst.undo_stack = .init(mem.gpa);

        dst.hover_pool = try .initPreheated(mem.gpa, 0x100);

        dst.lenses = .init(mem.gpa);
        dst.sexprs = .init(mem.gpa);
        dst.cases = .init(mem.gpa);
        dst.garlands = .init(mem.gpa);
        dst.executors = .init(mem.gpa);
        dst.fnkviewers = .init(mem.gpa);
        dst.fnkboxes = .init(mem.gpa);
        dst.traces = .init(mem.gpa);
        dst.postits = .init(mem.gpa);
        dst.toolbar_case = try dst.freshToolbarCase(mem);
        dst.toolbar_trash = .{ .rect = .unit };

        if (true) {
            try dst.lenses.append(.{ .source = ViewHelper.sexprTemplateChildView(
                .{},
                &.{ .right, .left },
            ).applyToLocalPosition(.new(1, 0)), .target = .new(3, 0) });
            try dst.lenses.append(.{ .source = .new(3, 0), .target = .new(6, 0) });

            var random: std.Random.DefaultPrng = .init(1);
            try dst.sexprs.append(try .fromSexpr(&dst.hover_pool, try randomSexpr(mem, random.random(), 7), .{}, false));
            if (false) {
                try dst.sexprs.append(try .fromSexpr(&dst.hover_pool, valid[0], .{ .pos = .new(-3, 0) }, false));

                for ([_][]const u8{
                    "Hermes",    "Mercury",
                    "Ares",      "Mars",
                    "Zeus",      "Jupiter",
                    "Aphrodite", "Venus",
                }, 0..) |n, k| {
                    try dst.sexprs.append(try .fromSexpr(&dst.hover_pool, try mem.storeSexpr(.doLit(n)), .{ .pos = .new(tof32(k) * 3, -2 + tof32(k % 2) - tof32(k)) }, k % 2 == 0));
                }
                // } else {
                //     try dst.sexprs.append(try .fromSexpr(&dst.hover_pool, Sexpr.pair_nil_nil, .{}, false));
            }

            try dst.cases.append(try .fromValues(&dst.hover_pool, .{
                .pattern = Sexpr.builtin.true,
                .template = Sexpr.builtin.false,
                .fnk_name = Sexpr.builtin.identity,
            }, .{ .pos = .new(0, 3) }));
            try dst.cases.append(try .fromValues(&dst.hover_pool, .{
                .pattern = Sexpr.builtin.vars.v1,
                .template = try mem.storeSexpr(.doPair(Sexpr.builtin.vars.v1, Sexpr.builtin.vars.v1)),
                .fnk_name = Sexpr.builtin.identity,
            }, .{ .pos = .new(-7, 5) }));

            try dst.garlands.append(.{
                .cases = try .initCapacity(mem.gpa, 4),
                .handle = .{ .point = .{ .pos = .new(0, 5) } },
                .handles_for_new_cases_first = .{ .handle = .{ .point = .{ .pos = .new(0, 5) } }, .length = 1 },
                .handles_for_new_cases_rest = try .initCapacity(mem.gpa, 4),
            });

            for (0..4) |k| {
                dst.garlands.items[0].handles_for_new_cases_rest.appendAssumeCapacity(.{ .handle = .{ .point = .{ .pos = .new(0, tof32(k + 1)) } }, .length = 1 });
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
                }, dst.garlands.items[0].handle.point.applyToLocalPoint(.{ .pos = .new(0, 1 + 2.5 * tof32(k)) })));
            }
            try dst.garlands.items[0].cases.items[0].next.insertCase(mem.gpa, 0, try .fromValues(&dst.hover_pool, .{
                .pattern = Sexpr.builtin.true,
                .template = valid[1],
                .fnk_name = valid[0],
            }, dst.garlands.items[0].handle.point.applyToLocalPoint(.{ .pos = .new(6, 0) })));

            try dst.executors.append(try .init(.{ .pos = .new(-5, -5) }, &dst.hover_pool));

            try dst.fnkviewers.append(try .init(.{ .pos = .new(-10, -7) }, &dst.hover_pool));

            if (false) {
                const debug_fnk = try core.parsing.parseSingleFnk(
                    \\asdf {
                    \\ (a . @x) -> asdf: (b . f) {
                    \\  B -> @x; 
                    \\ }
                    \\ (b . @x) -> B;
                    \\ (a . B) -> nil;
                    \\}
                , &mem.pool_for_sexprs, mem.scratch.allocator());

                try dst.fnkboxes.append(
                    try .init(
                        "TODO: remove",
                        debug_fnk.name,
                        .{ .pos = .new(60, -6) },
                        &.{},
                        try debug_fnk.body.toV2(mem.scratch.allocator()),
                        &dst.hover_pool,
                        mem,
                    ),
                );
            }
        }

        // TODO: use all levels
        const levels = @import("levels_new.zig").levels;
        // const levels = @import("levels_new.zig").levels; //[0..5];
        var x: f32 = 100;
        for (levels, 0..) |level, k| {
            const samples = blk: {
                var samples_it = level.samplesIterator();
                var samples: std.ArrayListUnmanaged(Sample) = .empty;
                while (try samples_it.next(&mem.pool_for_sexprs, mem.scratch.allocator())) |item| {
                    try samples.append(mem.scratch.allocator(), item);
                }
                break :blk try samples.toOwnedSlice(mem.scratch.allocator());
            };
            try dst.fnkboxes.append(
                try .init(
                    level.description,
                    level.fnk_name,
                    .{ .pos = .new(x, if (k % 2 == 0) -6 else -5) },
                    samples,
                    level.initial_definition,
                    &dst.hover_pool,
                    mem,
                ),
            );
            x += if (k < 5) 25 else 35;
        }

        var postit_pos: Vec2 = .new(33, -3);
        dst.camera = .fromCenterAndSize(postit_pos.add(.new(13, 8)), Vec2.new(16, 9).scale(2.75));
        try dst.postits.append(.fromText(&.{ "Welcome", "to the lab!" }, postit_pos));
        postit_pos.addInPlace(.new(12, 4));
        try dst.postits.append(.fromText(&.{ "Move around", "with WASD", "or Arrow Keys" }, postit_pos));
        postit_pos.addInPlace(.new(-15, 5));
        try dst.postits.append(.fromText(&.{ "Left click", "to pick up", "Atoms ->" }, postit_pos));
        postit_pos.addInPlace(.new(4.5, 1.25));
        try dst.sexprs.append(try .fromSexpr(
            &dst.hover_pool,
            try mem.storeSexpr(.doLit("a")),
            .{ .pos = postit_pos },
            false,
        ));
        try dst.sexprs.append(try .fromSexpr(
            &dst.hover_pool,
            try mem.storeSexpr(.doLit("b")),
            .{ .pos = postit_pos.add(.new(5, -1.5)) },
            true,
        ));
        try dst.sexprs.append(try .fromSexpr(
            &dst.hover_pool,
            try mem.storeSexpr(.doLit("C")),
            .{ .pos = postit_pos.add(.new(-2, 4)) },
            false,
        ));
        postit_pos.addInPlace(.new(5.5, 5.5));
        try dst.postits.append(.fromText(&.{ "Right click to", "duplicate them" }, postit_pos));
        try dst.postits.append(.fromText(&.{"Z to undo"}, postit_pos.add(.new(6.5, 0.7))));

        postit_pos.addInPlace(.new(19, -14));
        try dst.postits.append(.fromText(&.{ "Your job:", "make machines", "that transform", "Atoms into", "other Atoms" }, postit_pos));
        postit_pos.addInPlace(.new(7, 0));
        try dst.postits.append(.fromText(&.{ "The piece below", "(when active)", "will match with", "the atom 'a'", "and transform it", "into 'b'" }, postit_pos));
        try dst.cases.append(try .fromValues(&dst.hover_pool, .{
            .pattern = try mem.storeSexpr(.doLit("a")),
            .template = try mem.storeSexpr(.doLit("b")),
            .fnk_name = Sexpr.builtin.empty,
        }, .{ .pos = postit_pos.addY(5) }));
        postit_pos.addInPlace(.new(7, 0));
        try dst.postits.append(.fromText(&.{ "The machine", "below, made of", "two pieces,", "will turn", "'a' into 'b',", "and 'b' into 'a'" }, postit_pos));
        try dst.garlands.append(try .fromDefinition(.{ .pos = postit_pos.addY(5) }, .{ .cases = &.{
            .{
                .pattern = try mem.storeSexpr(.doLit("a")),
                .template = try mem.storeSexpr(.doLit("b")),
                .fnk_name = Sexpr.builtin.empty,
                .next = null,
            },
            .{
                .pattern = try mem.storeSexpr(.doLit("b")),
                .template = try mem.storeSexpr(.doLit("a")),
                .fnk_name = Sexpr.builtin.empty,
                .next = null,
            },
        } }, mem, &mem.hover_pool));
        postit_pos.addInPlace(.new(7, 0));
        try dst.postits.append(.fromText(&.{ "I've already", "solved the first", "assignment", "for you." }, postit_pos));
        postit_pos.addInPlace(.new(7, 0));
        try dst.postits.append(.fromText(&.{ "It's that", "box -->", "and the", "solution", "hangs under it" }, postit_pos));
        try dst.postits.append(.fromText(&.{ "Click the '>'", "buttons to", "see it in action!" }, postit_pos.add(.new(-5, 7))));
        postit_pos.addInPlace(.new(25, -2));
        try dst.postits.append(.fromText(&.{"Your turn!"}, postit_pos));
        try dst.postits.append(.fromText(&.{ "Click the", "'Unsolved!'", "button to see", "a requirement", "where the", "machine fails" }, postit_pos.add(.new(0.5, 6.5))));
        postit_pos.addInPlace(.new(25, 0));
        postit_pos.addInPlace(.new(7, -6.1));
        try dst.postits.append(.fromText(&.{ "You can create", "new pieces", "by duplicating", "existing ones" }, postit_pos));
        try dst.postits.append(.fromText(&.{ "(right click", "on the piece's", "circular center)" }, postit_pos.addX(7)));
        try dst.postits.append(.fromText(&.{ "You only need", "5 pieces!" }, postit_pos.addX(7.5).addY(30)));
        postit_pos.addInPlace(.new(25, 1));
        try dst.postits.append(.fromText(&.{ "Use Wildcards", "to match", "any value", "and use it later" }, postit_pos));

        try dst.canonizeAfterChanges(mem);
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
            workspace.traces.items,
        }) |things| {
            for (things) |*e| {
                if (std.meta.hasMethod(@TypeOf(e), "deinit")) {
                    e.deinit(gpa);
                } else {
                    e.garland.deinit(gpa);
                }
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
        workspace.traces.deinit();
        workspace.postits.deinit();
    }

    fn freshToolbarCase(workspace: *Workspace, mem: *core.VeryPermamentGameStuff) !VeryPhysicalCase {
        const new_name = try mem.arena_for_names.allocator().alloc(u8, 10);
        math.Random.init(workspace.random_instance.random()).alphanumeric_bytes(new_name);
        const var_value = try mem.storeSexpr(.doVar(new_name));

        // TODO: change variable on each toolbar opening

        return .fromValues(&mem.hover_pool, .{
            .pattern = var_value,
            .fnk_name = Sexpr.builtin.empty,
            .template = try mem.storeSexpr(.doPair(var_value, Sexpr.builtin.nil)),
            .next = null,
        }, .{});
    }

    pub fn save(workspace: *const Workspace, out: std.io.AnyWriter, scratch: std.mem.Allocator) !void {
        const version: u32 = 0;
        try out.writeInt(u32, version, .little);
        try out.writeStructEndian(workspace.camera, .little);
        // TODO: don't use tags
        for (workspace.sexprs.items) |s| {
            try writeEnum(out, SerializedTag, .VeryPhysicalSexpr, .little);
            try s.save(out, scratch);
        }
        for (workspace.cases.items) |s| {
            try writeEnum(out, SerializedTag, .VeryPhysicalCase, .little);
            try s.save(out, scratch);
        }
        for (workspace.garlands.items) |s| {
            try writeEnum(out, SerializedTag, .VeryPhysicalGarland, .little);
            try s.save(out, scratch);
        }
        for (workspace.fnkboxes.items) |s| {
            try writeEnum(out, SerializedTag, .Fnkbox, .little);
            try s.save(out, scratch);
        }
    }

    pub fn load(dst: *Workspace, in: std.io.AnyReader, mem: *core.VeryPermamentGameStuff) !void {
        dst.* = kommon.meta.initDefaultFields(Workspace);
        dst.undo_stack = .init(mem.gpa);
        dst.hover_pool = try .initPreheated(mem.gpa, 0x100);
        dst.lenses = .init(mem.gpa);
        dst.sexprs = .init(mem.gpa);
        dst.cases = .init(mem.gpa);
        dst.garlands = .init(mem.gpa);
        dst.executors = .init(mem.gpa);
        dst.fnkviewers = .init(mem.gpa);
        dst.fnkboxes = .init(mem.gpa);
        dst.traces = .init(mem.gpa);
        dst.postits = .init(mem.gpa);
        dst.toolbar_case = try dst.freshToolbarCase(mem);
        dst.toolbar_trash = .{ .rect = .unit };

        const version = try in.readInt(u32, .little);
        assert(version == 0);

        dst.camera = try in.readStructEndian(Rect, .little);

        while (in.readEnum(SerializedTag, .little) catch |err| switch (err) {
            error.EndOfStream => null,
            else => return err,
        }) |tag| {
            switch (tag) {
                .VeryPhysicalSexpr => {
                    var x: VeryPhysicalSexpr = undefined;
                    try x.load(in, version, mem);
                    try dst.sexprs.append(x);
                },
                .VeryPhysicalCase => {
                    var x: VeryPhysicalCase = undefined;
                    try x.load(in, version, mem);
                    try dst.cases.append(x);
                },
                .VeryPhysicalGarland => {
                    var x: VeryPhysicalGarland = undefined;
                    try x.load(in, version, mem);
                    try dst.garlands.append(x);
                },
                .Fnkbox => {
                    var x: Fnkbox = undefined;
                    try x.load(in, version, mem);
                    try dst.fnkboxes.append(x);
                },
            }
        }

        try dst.canonizeAfterChanges(mem);
    }

    const valid: []const *const Sexpr = &.{
        &Sexpr.doLit("toLowerCase"),
        &Sexpr.doLit("A"),
        &Sexpr.doLit("a"),
        &Sexpr.doLit("B"),
        &Sexpr.doLit("b"),
        &Sexpr.doLit("C"),
        &Sexpr.doLit("c"),
        &Sexpr.doLit("D"),
        &Sexpr.doLit("d"),
        &Sexpr.doLit("E"),
        &Sexpr.doLit("e"),
        &Sexpr.doLit("F"),
        &Sexpr.doLit("f"),
        &.empty,
    };

    const valid2 = .{
        .toUpperCase = &Sexpr.doLit("toUpperCase"),
        .isVowel = &Sexpr.doLit("isVowel"),
        .swap = &Sexpr.doLit("swap"),
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
                workspace.fnkboxes.items.len +
                // for toolbar variable
                1,
            // TODO: traces
        );

        result.appendAssumeCapacity(.{ .kind = .{ .case_handle = .toolbar } });

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
                try result.append(res, .{ .fnkbox_input = k });
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
            .postit,
            .executor_crank_handle,
            .executor_brake_handle,
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
            .case_handle => |case_handle| switch (case_handle) {
                .board => |k| .{ .case = k },
                else => null,
            },
            .garland_handle => |garland_handle| .{ .garland = garland_handle.parent.garland },
            .executor_handle => |k| .{ .executor = k },
            .fnkviewer_handle => |k| .{ .fnkviewer = k },
            .fnkbox_handle => |k| .{ .fnkbox = k },
            .nothing,
            .lens_handle,
            .sexpr,
            .postit,
            .executor_crank_handle,
            .executor_brake_handle,
            => null,
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
            .executor_crank_handle,
            .executor_brake_handle,
            .fnkviewer_handle,
            .fnkbox_handle,
            .nothing,
            .lens_handle,
            .sexpr,
            .postit,
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
            .fnkbox_input => |k| &workspace.fnkboxes.items[k].input,
            .fnkbox_testcase => |t| workspace.fnkboxes.items[t.fnkbox].testcases.items[t.testcase].partRef(t.part),
        };
    }

    fn caseHandleRef(workspace: *Workspace, place: CaseHandle) *Handle {
        return switch (place) {
            .toolbar => &workspace.toolbar_case.handle,
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
            .toolbar => &workspace.toolbar_case,
        };
    }

    fn executorAt(workspace: *Workspace, place: ExecutorPlace) *Executor {
        return switch (place) {
            .fnkbox => |k| &workspace.fnkboxes.items[k].execution.?.executor.?,
            .board => |k| &workspace.executors.items[k],
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

    pub fn popAt(workspace: *Workspace, target: Focus.Target, mem: *VeryPermamentGameStuff) !Focus.Value {
        return switch (target.kind) {
            .sexpr => |p| .{ .sexpr = try workspace.popSexprAt(p, &mem.hover_pool, mem) },
            .case_handle => |c| switch (c) {
                .toolbar => unreachable,
                .board => |k| .{ .case = workspace.cases.orderedRemove(k) },
                .garland => |t| .{ .case = workspace.garlandAt(t.parent).popCase(t.local) },
            },
            .garland_handle => |g| .{ .garland = if (g.local.len == 0 and std.meta.activeTag(g.parent) == .garland)
                workspace.garlands.orderedRemove(g.parent.garland)
            else blk: {
                const place = workspace.garlandAt(g);
                const garland = place.*;
                place.* = .init(garland.handle.point);
                break :blk garland;
            } },
            else => @panic("TODO"),
        };
    }

    pub fn unpopAt(workspace: *Workspace, target: Focus.Target, value: Focus.Value, mem: *VeryPermamentGameStuff) !void {
        switch (target.kind) {
            .sexpr => |p| try workspace.unpopSexprAt(p, value.sexpr, &mem.hover_pool, mem),
            .case_handle => |c| switch (c) {
                .toolbar => unreachable,
                .board => |k| try workspace.cases.insert(k, value.case),
                .garland => |t| try workspace.garlandAt(t.parent).insertCase(mem.gpa, t.local, value.case),
            },
            .garland_handle => |g| if (g.local.len == 0 and std.meta.activeTag(g.parent) == .garland) {
                try workspace.garlands.insert(g.parent.garland, value.garland);
            } else {
                const place = workspace.garlandAt(g);
                place.* = value.garland;
            },
            else => @panic("TODO"),
        }
    }

    fn popSexprAt(workspace: *Workspace, p: SexprPlace, hover_pool: *HoveredSexpr.Pool, mem: *VeryPermamentGameStuff) !VeryPhysicalSexpr {
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

    fn draw(workspace: *Workspace, platform: PlatformGives, drawer: *Drawer) !void {
        const asdf = tracy.initZone(@src(), .{ .name = "draw" });
        defer asdf.deinit();

        const camera = workspace.camera;

        // TODO: remove duplication
        for (workspace.sexprs.items, 0..) |s, k| {
            if (isGrabbedSexpr(.{ .board = k }, workspace.focus.grabbing)) continue;
            try s.draw(drawer, camera);
        }

        const holding: VeryPhysicalCase.Holding = switch (workspace.focus.grabbing.kind) {
            .sexpr => .sexpr,
            .garland_handle, .case_handle => .case_or_garland,
            else => .other,
        };

        for (workspace.cases.items, 0..) |c, k| {
            if (isGrabbedCase(.{ .board = k }, workspace.focus.grabbing)) continue;
            try c.draw(holding, drawer, camera);
        }

        for (workspace.garlands.items, 0..) |c, k| {
            if (isGrabbedGarland(.{ .local = &.{}, .parent = .{ .garland = k } }, workspace.focus.grabbing)) continue;
            try c.draw(holding, drawer, camera);
        }

        inline for (.{
            workspace.fnkboxes.items,
            workspace.executors.items,
            workspace.fnkviewers.items,
        }) |things| {
            for (things) |g| {
                try g.draw(holding, drawer, camera);
            }
        }

        inline for (.{
            workspace.traces.items,
            workspace.postits.items,
        }) |things| {
            for (things) |g| {
                try g.draw(drawer, camera);
            }
        }

        for (workspace.lenses.items) |lens| {
            drawer.canvas.fillCircle(camera, lens.target, lens.target_radius, .gray(0.5));

            if (camera.plusMargin(lens.target_radius + 1).contains(lens.target)) {
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

        drawer.canvas.fillRect(camera, workspace.leftToolbarRect(), .gray(0.4));
        if (workspace.toolbar_case_enabled) try workspace.toolbar_case.draw(.other, drawer, camera);
        try workspace.toolbar_trash.draw(drawer, camera);

        switch (workspace.focus.grabbing.kind) {
            else => {},
            .garland_handle => |t| try workspace.garlandAt(t).draw(.other, drawer, camera),
            .case_handle => |t| try workspace.cases.items[t.board].draw(.other, drawer, camera),
            .sexpr => |s| try workspace.sexprAtPlace(s.base).draw(drawer, camera),
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

    const left_toolbar_rect_ideal: Rect = .{ .top_left = .zero, .size = .new(6, 15) };
    fn leftToolbarRect(workspace: *const Workspace) Rect {
        return workspace.camera.withAspectRatio(left_toolbar_rect_ideal.size.aspectRatio(), .shrink, .center_left)
            .moveRelative(.new(-0.9 * (1 - workspace.toolbar_left), 0));
    }

    fn findUiAtPosition(workspace: *Workspace, pos: Vec2) !Focus.UiTarget {
        for (workspace.fnkboxes.items, 0..) |fnkbox, fnkbox_index| {
            if (fnkbox.fold_button.rect.contains(pos)) {
                return .{ .kind = .{ .fnkbox_toggle_fold = fnkbox_index } };
            }
            if (!fnkbox.box().contains(pos)) continue;
            if (fnkbox.scroll_button_up.rect.contains(pos)) {
                return .{ .kind = .{ .fnkbox_scroll = .{ .fnkbox = fnkbox_index, .direction = .up } } };
            }
            if (fnkbox.scroll_button_down.rect.contains(pos)) {
                return .{ .kind = .{ .fnkbox_scroll = .{ .fnkbox = fnkbox_index, .direction = .down } } };
            }
            if (fnkbox.execution != null) continue;
            if (fnkbox.statusBarGoal().contains(pos) and std.meta.activeTag(fnkbox.status) == .unsolved) {
                return .{ .kind = .{ .fnkbox_see_failing_case = fnkbox_index } };
            }
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
                if (executor.brakeHandle().contains(pos) and CRANKS_ENABLED) {
                    return .{ .kind = .{ .executor_brake_handle = .{ .board = k } } };
                }
                if (executor.animating()) {
                    if (executor.crankHandle().?.contains(pos) and CRANKS_ENABLED) {
                        return .{ .kind = .{ .executor_crank_handle = .{ .board = k } } };
                    }
                } else {
                    if (executor.handle.overlapped(pos, Handle.radius)) {
                        return .{ .kind = .{ .executor_handle = k } };
                    }
                }
            }
        }

        // executors in fnkboxes
        if (grabbed_tag == .nothing) {
            for (workspace.fnkboxes.items, 0..) |thing, k| {
                if (thing.execution) |execution| {
                    if (execution.executor) |executor| {
                        if (executor.crankHandle()) |handle| {
                            if (handle.contains(pos) and CRANKS_ENABLED) {
                                return .{ .kind = .{ .executor_crank_handle = .{ .fnkbox = k } } };
                            }
                        }
                        if (executor.brakeHandle().contains(pos) and CRANKS_ENABLED) {
                            return .{ .kind = .{ .executor_brake_handle = .{ .fnkbox = k } } };
                        }
                    }
                }
            }
        }

        // fnkviewers
        if (grabbed_tag == .nothing) {
            for (workspace.fnkviewers.items, 0..) |fnkviewer, k| {
                if (fnkviewer.handle.overlapped(pos, Handle.radius)) {
                    return .{ .kind = .{ .fnkviewer_handle = k } };
                }
            }
        }

        // fnkboxes handles
        if (grabbed_tag == .nothing) {
            for (workspace.fnkboxes.items, 0..) |thing, k| {
                if (thing.handle.overlapped(pos, Handle.radius)) {
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
                    if (grabbed_tag != .nothing and base.immutable()) continue;
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
                        .fnkbox_input => |k| {
                            const fnkbox = &workspace.fnkboxes.items[k];
                            if (fnkbox.execution != null) continue;
                        },
                        .case => |t| switch (t.parent) {
                            else => {},
                            .toolbar => if (!workspace.toolbar_case_enabled) continue,
                            .garland => |g| switch (g.parent.parent) {
                                else => {},
                                .fnkbox => |k| {
                                    if (workspace.fnkboxes.items[k].execution != null) continue;
                                },
                                .executor => |k| {
                                    if (workspace.executors.items[k].animating()) continue;
                                },
                            },
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
                        .fnkbox => |k| {
                            if (workspace.fnkboxes.items[k].execution != null) continue;
                        },
                        else => {},
                    }

                    const g = workspace.garlandAt(base);
                    if (g.handle.overlapped(pos, VeryPhysicalGarland.handle_radius)) {
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

                // some special cases
                switch (base) {
                    .board => {},
                    .toolbar => if (!workspace.toolbar_case_enabled) continue,
                    .garland => |t| switch (t.parent.parent) {
                        else => {},
                        .fnkbox => |k| {
                            if (workspace.fnkboxes.items[k].execution != null) continue;
                        },
                        .executor => |k| {
                            if (workspace.executors.items[k].animating()) continue;
                        },
                    },
                }

                // picking
                if (base.exists() and grabbed_tag == .nothing) {
                    if (handle.overlapped(pos, VeryPhysicalCase.handle_radius)) {
                        return .{ .kind = .{ .case_handle = try base.clone(res) } };
                    }
                }

                // dropping
                if (!base.exists() and grabbed_tag == .case_handle) {
                    if (handle.overlapped(pos, VeryPhysicalGarland.handle_drop_radius)) {
                        return .{ .kind = .{ .case_handle = try base.clone(res) } };
                    }
                }
            }
        }

        // postits
        if (grabbed_tag == .nothing) {
            for (0..workspace.postits.items.len) |k_rev| {
                const k = workspace.postits.items.len - k_rev - 1;
                const postit = workspace.postits.items[k];
                if (postit.button.rect.contains(pos)) {
                    return .{ .kind = .{ .postit = k } };
                }
            }
        }

        return .nothing;
    }

    pub fn canonizeAfterChanges(workspace: *Workspace, mem: *core.VeryPermamentGameStuff) !void {
        for (workspace.fnkboxes.items) |*fnkbox| {
            try fnkbox.updateStatus(workspace.fnkboxes.items, mem);
        }
        // TODO: remove this debug
        workspace.toolbar_case_enabled = true or
            workspace.fnkboxes.items[0].status == .solved and
                workspace.fnkboxes.items[1].status == .solved and
                workspace.fnkboxes.items[2].status == .solved;
    }

    pub fn update(workspace: *Workspace, platform: PlatformGives, drawer: ?*Drawer, mem: *VeryPermamentGameStuff, frame_arena: std.mem.Allocator) !void {
        _ = mem.scratch.reset(.retain_capacity);

        // std.log.debug("fps {d}", .{1.0 / platform.delta_seconds});
        const camera = workspace.camera.withAspectRatio(platform.aspect_ratio, .grow, .center);

        workspace.toolbar_case.kinematicUpdate(workspace.leftToolbarRect().pointAsIf(
            .{ .pos = .new(2.5, 5) },
            left_toolbar_rect_ideal,
        ), null, null, undefined);
        workspace.toolbar_trash.rect = workspace.leftToolbarRect().subrectAsIf(
            .{ .top_left = .half, .size = .both(3) },
            left_toolbar_rect_ideal,
        );

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
            try workspace.draw(platform, d);
        }

        // state changes
        if (platform.keyboard.wasPressed(.KeyZ)) {
            if (workspace.undo_stack.pop()) |command| {
                again: switch (command.specific) {
                    .noop => {},
                    .deleted => |d| {
                        try workspace.unpopAt(d.old_place, d.value, mem);
                        const next_cmd = workspace.undo_stack.pop().?;
                        continue :again next_cmd.specific;
                    },
                    .spawned_trace => {
                        _ = workspace.traces.pop().?;
                        const next_cmd = workspace.undo_stack.pop().?;
                        continue :again next_cmd.specific;
                    },
                    .despawned_trace => |t| {
                        try workspace.traces.append(t.trace);
                        if (t.spawned_sexpr) _ = workspace.sexprs.pop().?;
                        const next_cmd = workspace.undo_stack.pop().?;
                        continue :again next_cmd.specific;
                    },
                    .started_execution => |g| {
                        const executor = &workspace.executors.items[g.executor];
                        executor.input = g.input;
                        executor.garland = g.garland;
                        executor.prev_pills = .fromOwnedSlice(g.prev_pills);
                        executor.enqueued_stack.clearRetainingCapacity();
                        executor.animation = null;
                        executor.handle.point = g.prev_point;
                        const next_cmd = workspace.undo_stack.pop().?;
                        assert(std.meta.activeTag(next_cmd.specific) == .dropped or std.meta.activeTag(next_cmd.specific) == .fnkbox_launch_testcase);
                        continue :again next_cmd.specific;
                    },
                    .started_execution_fnkbox_from_input => |g| {
                        const fnkbox = &workspace.fnkboxes.items[g.fnkbox];
                        fnkbox.input = g.input;
                        fnkbox.execution = null;
                        const next_cmd = workspace.undo_stack.pop().?;
                        assert(std.meta.activeTag(next_cmd.specific) == .dropped);
                        continue :again next_cmd.specific;
                    },
                    .fnkbox_launch_testcase => |t| {
                        const fnkbox = &workspace.fnkboxes.items[t.fnkbox];
                        fnkbox.testcases.items[t.testcase].actual = t.old_actual;
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
                        if (g.duplicate) {
                            // delete the last created thing
                            switch (g.from.kind) {
                                .nothing => unreachable,
                                .fnkviewer_handle,
                                .executor_handle,
                                .executor_crank_handle,
                                .executor_brake_handle,
                                => @panic("TODO"),
                                .fnkbox_handle => _ = workspace.fnkboxes.pop().?,
                                .postit => _ = workspace.postits.pop().?,
                                .lens_handle => _ = workspace.lenses.pop().?,
                                .garland_handle => {
                                    // TODO: better memory management?
                                    var old = workspace.garlands.pop().?;
                                    old.deinit(mem.gpa);
                                },
                                .case_handle => _ = workspace.cases.pop().?,
                                .sexpr => |h| {
                                    _ = workspace.sexprs.pop().?;
                                    // visual flair
                                    workspace.sexprAtPlace(h.base).getSubValue(h.local).hovered.value = 10;
                                },
                            }
                        } else {
                            // undo any grabbing-related changes to the thing
                            switch (g.from.kind) {
                                .nothing => unreachable,
                                .executor_handle => |h| {
                                    const e = &workspace.executors.items[h];
                                    e.handle.point = g.old_data.point;
                                },
                                .executor_crank_handle,
                                .executor_brake_handle,
                                => @panic("TODO"),
                                .fnkviewer_handle => |h| {
                                    const e = &workspace.fnkviewers.items[h];
                                    e.handle.point = g.old_data.point;
                                },
                                .fnkbox_handle => |h| {
                                    const e = &workspace.fnkboxes.items[h];
                                    e.handle.point = g.old_data.point;
                                },
                                .postit => |k| {
                                    const postit = &workspace.postits.items[k];
                                    postit.button.rect.top_left = g.old_data.point.pos;
                                },
                                .lens_handle => |h| {
                                    const lens = &workspace.lenses.items[h.index];
                                    lens.setHandlePos(h.part, g.old_data.point.pos);
                                    assert(workspace.focus.grabbing.kind.lens_handle.part == h.part);
                                    assert(workspace.focus.grabbing.kind.lens_handle.index == h.index);
                                },
                                .garland_handle => |h| {
                                    if (h.local.len == 0 and std.meta.activeTag(h.parent) == .garland) {
                                        const garland = &workspace.garlands.items[h.parent.garland];
                                        garland.handle.point = g.old_data.point;
                                    } else {
                                        const garland = workspace.garlands.pop().?;
                                        workspace.garlandAt(h).* = garland;
                                    }
                                },
                                .case_handle => |h| {
                                    switch (h) {
                                        .toolbar => unreachable,
                                        .board => |k| {
                                            const case = &workspace.cases.items[k];
                                            case.handle.point = g.old_data.point;
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
                                    old.hovered.value = 10;
                                    old.point = g.old_data.point;
                                    old.is_pattern = g.old_data.is_pattern;
                                    try workspace.unpopSexprAt(h, old, &workspace.hover_pool, mem);
                                },
                            }
                        }
                        workspace.focus.grabbing = .nothing;
                    },
                    .dropped => |g| {
                        switch (g.at.kind) {
                            .nothing => unreachable,
                            .executor_crank_handle,
                            .executor_brake_handle,
                            => @panic("TODO"),
                            .lens_handle, .executor_handle, .fnkviewer_handle, .fnkbox_handle, .postit => {
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
                                    .toolbar => unreachable,
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
                try workspace.canonizeAfterChanges(mem);
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

        const prev_toolbar_left = workspace.toolbar_left;
        math.lerp_towards(&workspace.toolbar_left, if (workspace.leftToolbarRect().contains(mouse.cur.position)) 1 else 0, 0.3, platform.delta_seconds);
        if (workspace.toolbar_left > 0.1 and prev_toolbar_left <= 0.1) {
            workspace.toolbar_case = try workspace.freshToolbarCase(mem);
        }

        // TODO: should maybe be a Focus.Target as a dropzone
        const hovering_toolbar_trash: bool = switch (workspace.focus.grabbing.kind) {
            .sexpr, .case_handle, .garland_handle => workspace.toolbar_trash.rect.contains(mouse.cur.position),
            .nothing => false,
            // TODO: trash should handle everything
            else => false,
        };

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
                            // if (std.meta.activeTag(sexpr.base) == .board and sexpr.local.len == 0) break :blk null;
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
            fnkbox.updateUiHotness(fnkbox_index, ui_hot, workspace.focus.ui_active, platform.delta_seconds);
        }

        for (workspace.postits.items, 0..) |*postit, k| {
            postit.button.updateHot2(switch (hovering.kind) {
                else => false,
                .postit => |k2| k == k2,
            }, switch (workspace.focus.grabbing.kind) {
                else => false,
                .postit => |k2| k == k2,
            }, platform.delta_seconds);
        }

        math.lerp_towards(&workspace.toolbar_trash.hot_t, if (hovering_toolbar_trash) 1 else 0, 0.6, platform.delta_seconds);

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
        const mouse_point: Point = .{ .pos = mouse.cur.position };
        switch (workspace.focus.grabbing.kind) {
            .nothing => {},
            // TODO: would be nice to unify all handle dragging
            .postit => |k| {
                workspace.postits.items[k].button.rect.top_left = mouse.cur.position.sub(workspace.focus.grabbing_offset);
            },
            .lens_handle => |p| {
                const lens = &workspace.lenses.items[p.index];
                lens.setHandlePos(p.part, mouse.cur.position);
            },
            .executor_handle => |h| {
                workspace.executors.items[h].handle.point = mouse_point;
            },
            .executor_crank_handle => |h| try workspace.executorAt(h).crankMovedTo(mouse_point.pos),
            .executor_brake_handle => |h| try workspace.executorAt(h).brakeMovedTo(mouse_point.pos),
            .fnkviewer_handle => |h| {
                workspace.fnkviewers.items[h].handle.point = mouse_point;
            },
            .fnkbox_handle => |h| {
                workspace.fnkboxes.items[h].handle.point = mouse_point;
            },
            .case_handle => |p| workspace.caseHandleRef(p).point = mouse_point,
            .garland_handle => |p| workspace.garlandHandleRef(p).point = mouse_point,
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

        for (workspace.traces.items) |*c| {
            c.update(platform.delta_seconds);
        }

        {
            var k: usize = 0;
            while (k < workspace.traces.items.len) {
                if (workspace.traces.items[k].remaining_lifetime <= 0) {
                    const old = workspace.traces.swapRemove(k);
                    // TODO: call this on undo_stack deinit
                    // old.deinit(mem.gpa);
                    if (old.last_input) |l| try workspace.sexprs.append(l);
                    try workspace.undo_stack.append(.{ .specific = .{ .despawned_trace = .{
                        .trace = old,
                        .spawned_sexpr = old.last_input != null,
                    } } });
                } else k += 1;
            }
        }

        for (workspace.executors.items) |*e| {
            try e.update(mem, workspace.fnkboxes.items, &workspace.hover_pool, platform.delta_seconds);
        }

        for (workspace.fnkviewers.items) |*e| {
            try e.update(mem, workspace.fnkboxes.items, &workspace.hover_pool, platform.delta_seconds);
        }

        for (workspace.fnkboxes.items) |*e| {
            // TODO: revise
            const added_trace = try e.update(mem, workspace.fnkboxes.items, &workspace.hover_pool, platform.delta_seconds);
            if (added_trace) |trace| {
                try workspace.traces.append(trace);
                try workspace.undo_stack.append(.{ .specific = .spawned_trace });
            }
        }

        // 'var' since the deleted command gets completed at execution
        var action: UndoableCommand = if (workspace.focus.grabbing.kind == .nothing and (mouse.wasPressed(.left) or mouse.wasPressed(.right)))
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
                    .nothing, .fnkbox_launch_testcase, .fnkbox_see_failing_case => blk: {
                        workspace.focus.ui_active = ui_hot;
                        break :blk .noop;
                    },
                },
                else => .{ .specific = .{
                    .grabbed = .{
                        .from = hovering,
                        .duplicate = mouse.wasPressed(.right) or hovering.kind.immutable(),
                        .old_data = workspace.getOldData(hovering),
                    },
                } },
            }
        else if (workspace.focus.grabbing.kind != .nothing and ((!mouse.cur.isDown(.left) and !mouse.cur.isDown(.right)) or workspace.grabbingSomethingIllegal()))
            if (hovering_toolbar_trash)
                .{ .specific = .{ .deleted = .{
                    .old_place = workspace.focus.grabbing,
                    .value = undefined,
                } } }
            else switch (workspace.focus.grabbing.kind) {
                .nothing => unreachable,
                .lens_handle,
                .executor_handle,
                .fnkviewer_handle,
                .fnkbox_handle,
                .postit,
                .executor_crank_handle,
                .executor_brake_handle,
                => .{ .specific = .{
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
                .fnkbox_see_failing_case => |k| op: {
                    const fnkbox = &workspace.fnkboxes.items[k];
                    break :op .{
                        .specific = .{ .fnkbox_launch_testcase = .{
                            .fnkbox = k,
                            .testcase = fnkbox.status.unsolved,
                            .old_actual = try fnkbox.testcases.items[fnkbox.status.unsolved].actual.clone(&workspace.hover_pool),
                        } },
                    };
                },
                .fnkbox_launch_testcase => |t| .{ .specific = .{ .fnkbox_launch_testcase = .{
                    .fnkbox = t.fnkbox,
                    .testcase = t.testcase,
                    .old_actual = try workspace.fnkboxes.items[t.fnkbox].testcases.items[t.testcase].actual.clone(&workspace.hover_pool),
                } } },
            };
        } else .noop;

        // camera controls
        {
            const hovering_fnkbox: ?usize = for (workspace.fnkboxes.items, 0..) |f, k| {
                if (f.box().contains(mouse.cur.position)) break k;
            } else null;

            if (hovering_fnkbox) |k| {
                workspace.fnkboxes.items[k].scroll_testcases += mouse.cur.scrolled.toNumber() * platform.delta_seconds / 0.05;
            }

            workspace.camera = moveCamera(camera, platform.delta_seconds, platform.keyboard, mouse, hovering_fnkbox == null);
        }

        // actually perform the action
        switch (action.specific) {
            .noop => {},
            .started_execution,
            .started_execution_fnkbox_from_input,
            .spawned_trace,
            .despawned_trace,
            => unreachable,
            .fnkbox_launch_testcase => |t| {
                const fnkbox = &workspace.fnkboxes.items[t.fnkbox];
                const testcase = &fnkbox.testcases.items[t.testcase];
                assert(fnkbox.execution == null);
                const old_actual = testcase.actual.value;
                testcase.actual = try .empty(testcase.actual.point, &workspace.hover_pool, false);
                fnkbox.execution = .{
                    .source = .{ .testcase = t.testcase },
                    .old_testcase_actual_value = old_actual,
                    .executor = null,
                    .state_t = undefined,
                    .state = .scrolling_towards_case,
                };
            },
            .fnkbox_toggle_fold => |k| {
                const fnkbox = &workspace.fnkboxes.items[k];
                fnkbox.folded = !fnkbox.folded;
            },
            .fnkbox_scroll => {
                // handled in the 'apply ui-like draggin' section
            },
            .deleted => |*d| {
                workspace.focus.grabbing = .nothing;
                d.value = try workspace.popAt(d.old_place, mem);
            },
            .grabbed => |g| {
                switch (g.from.kind) {
                    .nothing => unreachable,
                    inline .lens_handle,
                    .executor_handle,
                    .fnkviewer_handle,
                    .fnkbox_handle,
                    .postit,
                    .executor_crank_handle,
                    .executor_brake_handle,
                    => |h, t| {
                        workspace.focus.grabbing_offset = switch (t) {
                            else => .zero,
                            .postit => mouse.cur.position.sub(g.old_data.point.pos),
                        };
                        if (g.duplicate) {
                            switch (t) {
                                else => std.log.err("TODO", .{}),
                                .fnkbox_handle => {
                                    const original = workspace.fnkboxes.items[h];

                                    const S = struct {
                                        var random_instance: std.Random.DefaultPrng = std.Random.DefaultPrng.init(1);
                                    };
                                    const new_name = try mem.gpa.alloc(u8, 10);
                                    math.Random.init(S.random_instance.random()).alphanumeric_bytes(new_name);
                                    while (for (workspace.fnkboxes.items) |fnkbox| {
                                        if (fnkbox.fnkname.value.equals(&Sexpr.doLit(new_name))) break true;
                                    } else false) {
                                        math.Random.init(S.random_instance.random()).alphanumeric_bytes(new_name);
                                    }

                                    try workspace.fnkboxes.append(try original.cloneAsEmpty(try mem.storeSexpr(.doLit(new_name)), mem));
                                    workspace.focus.grabbing = .{ .kind = .{
                                        .fnkbox_handle = workspace.fnkboxes.items.len - 1,
                                    } };
                                },
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
                                place.* = .init(garland.handle.point);
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
                            .toolbar => {
                                assert(g.duplicate);
                                try workspace.cases.append(workspace.toolbar_case);
                                // TODO: undoable
                                // TODO: decide
                                // workspace.toolbar_case = try workspace.freshToolbarCase(mem);
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
                            try workspace.popSexprAt(h, &workspace.hover_pool, mem);

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
                workspace.focus.grabbing = .nothing;
                switch (g.at.kind) {
                    .nothing => unreachable,
                    .lens_handle,
                    .executor_handle,
                    .fnkviewer_handle,
                    .fnkbox_handle,
                    .postit,
                    .executor_crank_handle,
                    .executor_brake_handle,
                    => {},
                    .garland_handle => |h| {
                        if (h.local.len == 0 and std.meta.activeTag(h.parent) == .garland) {} else {
                            const place = workspace.garlandAt(h);
                            const k = g.old_grabbed_position.kind.garland_handle.parent.garland;
                            assert(g.old_grabbed_position.kind.garland_handle.local.len == 0);
                            const garland = workspace.garlands.items[k];
                            place.* = garland;
                            _ = workspace.garlands.orderedRemove(k);
                        }
                    },
                    .case_handle => |h| {
                        const old_k = g.old_grabbed_position.kind.case_handle.board;
                        switch (h) {
                            .board => {},
                            .toolbar => unreachable,
                            .garland => |t| {
                                const garland = garlandAt(workspace, t.parent);
                                try garland.insertCase(mem.gpa, t.local, workspace.cases.items[old_k]);
                                _ = workspace.cases.orderedRemove(old_k);
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
                    },
                }
            },
        }
        if (action.specific != .noop) {
            try workspace.undo_stack.append(action);
            try workspace.canonizeAfterChanges(mem);
        }

        for (workspace.fnkboxes.items, 0..) |*fnkbox, k| {
            if (fnkbox.execution == null and !fnkbox.input.isEmpty() and fnkbox.garland.cases.items.len > 0) {
                var garland = try fnkbox.garland.clone(mem.gpa, &workspace.hover_pool);
                assert(garland.fnkname == null);
                if (fnkbox.folded) garland.fnkname = try fnkbox.fnkname.clone(&workspace.hover_pool);
                fnkbox.execution = .{
                    .source = .input,
                    .state = .executing,
                    .state_t = undefined,
                    .old_testcase_actual_value = undefined,
                    .executor = .{
                        .input = fnkbox.input,
                        .garland = garland,
                        .handle = .{ .point = fnkbox.executorPoint() },
                        .animation = null,
                    },
                };
                try workspace.undo_stack.append(.{ .specific = .{ .started_execution_fnkbox_from_input = .{
                    .fnkbox = k,
                    .input = fnkbox.input,
                } } });
                try workspace.canonizeAfterChanges(mem);
            }
        }
        for (workspace.executors.items, 0..) |e, k| {
            if (e.startedExecution()) {
                assert(e.enqueued_stack.items.len == 0);
                try workspace.undo_stack.append(.{ .specific = .{ .started_execution = .{
                    .executor = k,
                    .input = e.input,
                    .garland = try e.garland.clone(mem.gpa, &workspace.hover_pool),
                    .prev_pills = (try e.prev_pills.clone(mem.gpa)).items,
                    .prev_point = e.handle.point,
                } } });
                try workspace.canonizeAfterChanges(mem);
            }
        }
    }

    fn grabbingSomethingIllegal(workspace: *const Workspace) bool {
        switch (workspace.focus.grabbing.kind) {
            else => return false,
            .executor_brake_handle => |h| switch (h) {
                .board => return false,
                .fnkbox => |k| {
                    const fnkbox = workspace.fnkboxes.items[k];
                    return fnkbox.execution == null or
                        fnkbox.execution.?.executor == null;
                },
            },
            .executor_crank_handle => |h| switch (h) {
                .board => return false,
                .fnkbox => |k| {
                    const fnkbox = workspace.fnkboxes.items[k];
                    return fnkbox.execution == null or
                        fnkbox.execution.?.executor == null or
                        fnkbox.execution.?.executor.?.animation == null;
                },
            },
        }
    }

    fn pointOf(workspace: *Workspace, thing: Focus.Target) Point {
        return switch (thing.kind) {
            .nothing => unreachable,
            .postit => |k| .{ .pos = workspace.postits.items[k].button.rect.top_left, .scale = undefined },
            .lens_handle => |h| .{ .pos = workspace.lenses.items[h.index].handlePos(h.part) },
            .executor_handle => |h| workspace.executors.items[h].handle.point,
            .executor_crank_handle => |h| workspace.executorAt(h).crankHandle().?.asPoint(),
            .executor_brake_handle => |h| workspace.executorAt(h).brakeHandle().asPoint(),
            .fnkviewer_handle => |h| workspace.fnkviewers.items[h].handle.point,
            .fnkbox_handle => |h| workspace.fnkboxes.items[h].handle.point,
            .case_handle => |h| workspace.caseHandleRef(h).point,
            .garland_handle => |k| workspace.garlandHandleRef(k).point,
            .sexpr => |s| workspace.sexprAtPlace(s.base).point,
        };
    }

    fn getOldData(workspace: *Workspace, hovering: Focus.Target) UndoableCommand.Grabbed.OldData {
        return .{
            .point = workspace.pointOf(hovering),
            .is_pattern = switch (hovering.kind) {
                else => undefined,
                .sexpr => |s| workspace.sexprAtPlace(s.base).is_pattern,
            },
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
    try dst.workspace.init(&dst.core_mem, random_seed);
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.usual.deinit(undefined);
    self.core_mem.deinit();
    self.workspace.deinit(gpa);
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    try Drawer.AtomVisuals.Geometry.initFixed(self.usual.mem.forever.allocator(), self.usual.canvas.gl);
    self.drawer.atom_visuals_cache = try .init(self.usual.mem.forever.allocator(), self.usual.canvas.gl);
    try self.workspace.init(&self.core_mem, 0);
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
    try self.workspace.update(platform, &self.drawer, &self.core_mem, self.drawer.canvas.frame_arena.allocator());

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
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const PhysicalSexpr = @import("physical.zig").PhysicalSexpr;
const ViewHelper = @import("physical.zig").ViewHelper;
const BindingsState = @import("physical.zig").BindingsState;
const Sample = @import("levels_new.zig").Sample;
