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
            defer workspace.deinit();
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
camera: Rect = .fromCenterAndSize(.zero, .new(16, 9)),

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
            .atom_lit, .atom_var => null,
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

const VeryPhysicalCase = struct {
    pattern: VeryPhysicalSexpr,
    fnk_name: VeryPhysicalSexpr,
    template: VeryPhysicalSexpr,
    next: void = {},
    handle: Vec2,
    handle_hot_t: f32 = 0,
    pub const handle_radius: f32 = 0.2;

    pub fn fromValues(
        pool: *HoveredSexpr.Pool,
        values: struct {
            pattern: *const Sexpr,
            fnk_name: *const Sexpr,
            template: *const Sexpr,
        },
        center: Point,
    ) !VeryPhysicalCase {
        return .{
            .handle = center.pos,
            .pattern = try .fromSexpr(pool, values.pattern, center.applyToLocalPoint(.{ .pos = .xneg }), true),
            .template = try .fromSexpr(pool, values.template, center.applyToLocalPoint(.{ .pos = .xpos }), false),
            .fnk_name = try .fromSexpr(pool, values.fnk_name, center
                .applyToLocalPoint(.{ .pos = .new(3, 0) }).applyToLocalPoint(.{ .scale = 0.5 }), false),
        };
    }

    pub fn draw(case: VeryPhysicalCase, drawer: *Drawer, camera: Rect) !void {
        drawer.canvas.strokeCircle(128, camera, case.handle, handle_radius * (1 + case.handle_hot_t * 0.2), 0.05, .black);
        try case.pattern.draw(drawer, camera);
        try case.template.draw(drawer, camera);
        // try case.fnk_name.draw(drawer, camera);
    }

    pub fn update(case: *VeryPhysicalCase, delta_seconds: f32) void {
        const center: Point = .{ .pos = case.handle };
        case.pattern.point.lerp_towards(center.applyToLocalPoint(.{ .pos = .xneg }), 0.6, delta_seconds);
        case.template.point.lerp_towards(center.applyToLocalPoint(.{ .pos = .xpos }), 0.6, delta_seconds);
        // case.fnk_name.point.lerp_towards(...);
    }

    // TODO: pointers?
    pub fn childCase(parent: *VeryPhysicalCase, local: core.CaseAddress) *VeryPhysicalCase {
        assert(local.len == 0);
        return parent;
        // var cur = parent;
        // for (local) |k| {
        //     cur = cur.next.?.[k];
        // }
        // return cur;
    }

    // TODO: pointers?
    pub fn sexprAt(case: *VeryPhysicalCase, part: core.CasePart) *VeryPhysicalSexpr {
        return switch (part) {
            .template => &case.template,
            .pattern => &case.pattern,
            .fnk_name => &case.fnk_name,
        };
    }

    pub fn constSexprAt(case: *const VeryPhysicalCase, part: core.CasePart) *const VeryPhysicalSexpr {
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

    pub fn fromSexpr(pool: *HoveredSexpr.Pool, value: *const Sexpr, point: Point, is_pattern: bool) !VeryPhysicalSexpr {
        return .{
            .hovered = try HoveredSexpr.fromSexpr(pool, value),
            .point = point,
            .value = value,
            .is_pattern = is_pattern,
            .is_pattern_t = if (is_pattern) 1.0 else 0.0,
        };
    }

    fn updateIsPattern(sexpr: *VeryPhysicalSexpr, delta_seconds: f32) void {
        math.lerp_towards(&sexpr.is_pattern_t, if (sexpr.is_pattern) 1 else 0, 0.6, delta_seconds);
    }

    fn _draw(drawer: *Drawer, camera: Rect, value: *const Sexpr, hovered: *HoveredSexpr, point: Point, is_pattern: bool, is_pattern_t: f32) !void {
        const actual_point = point.applyToLocalPoint(.lerp(.{}, .lerp(
            .{ .turns = -0.02, .pos = .new(0.5, 0) },
            .{ .turns = 0.02, .pos = .new(-0.5, 0) },
            is_pattern_t,
        ), hovered.value / 2.0));
        switch (value.*) {
            .atom_lit, .atom_var => try if (is_pattern)
                drawer.drawPatternSexpr(camera, value, actual_point)
            else
                drawer.drawTemplateSexpr(camera, value, actual_point),
            .pair => |pair| {
                try if (is_pattern)
                    drawer.drawPatternPairHolder(camera, actual_point)
                else
                    drawer.drawTemplatePairHolder(camera, actual_point);
                // try drawTemplateWildcardLinesNonRecursive(...);
                const offset = if (is_pattern) ViewHelper.OFFSET_PATTERN else ViewHelper.OFFSET_TEMPLATE;
                try _draw(drawer, camera, pair.left, hovered.next.?.left, actual_point.applyToLocalPoint(offset.LEFT), is_pattern, is_pattern_t);
                try _draw(drawer, camera, pair.right, hovered.next.?.right, actual_point.applyToLocalPoint(offset.RIGHT), is_pattern, is_pattern_t);
            },
        }
    }

    pub fn draw(sexpr: VeryPhysicalSexpr, drawer: *Drawer, camera: Rect) !void {
        assert(math.in01(sexpr.is_pattern_t));
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

        return _draw(drawer, camera, sexpr.value, sexpr.hovered, base_point, sexpr.is_pattern, sexpr.is_pattern_t);
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
};

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
    };

    lenses: std.ArrayList(Lens),
    sexprs: std.ArrayList(VeryPhysicalSexpr),
    cases: std.ArrayList(VeryPhysicalCase),
    grabbed_sexpr: ?VeryPhysicalSexpr = null,

    hover_pool: HoveredSexpr.Pool,

    focus: Focus = .{},

    undo_stack: std.ArrayList(UndoableCommand),

    const Focus = struct {
        // TODO: Not really any Target, since for sexprs it's .grabbed with no local
        grabbing: Target = .nothing,

        const Target = struct {
            kind: union(enum) {
                nothing,
                sexpr: SexprPlace,
                lens_handle: struct {
                    index: usize,
                    part: Lens.Part,
                },
                case_handle: struct {
                    index: usize,
                },
            },
            lens_transform: Lens.Transform = .identity,

            pub const nothing: Target = .{ .kind = .nothing };
            pub const grabbed_sexpr: Target = .{ .kind = .{ .sexpr = .{ .base = .grabbed, .local = &.{} } } };
        };
    };

    const BaseSexprPlace = union(enum) {
        board: Vec2,
        case: struct {
            parent_handle_pos: Vec2,
            local: core.CaseAddress,
            part: core.CasePart,
        },
        grabbed,
    };
    const SexprPlace = struct {
        base: BaseSexprPlace,
        local: core.SexprAddress,
    };
    const UndoableCommand = struct {
        specific: union(enum) {
            noop,
            dropped_sexpr: struct {
                new_place: SexprPlace,
                overwritten_value: ?VeryPhysicalSexpr,
            },
            grabbed_sexpr: struct {
                origin: SexprPlace,
            },
            // TODO: unify?
            picked_lens_or_case: struct {
                /// always is .lens_handle or .case_handle
                target: Focus.Target,
                old_position: Vec2,
            },
            dropped_lens_or_case: struct {
                /// always is .lens_handle or .case_handle
                target: Focus.Target,
                new_position: Vec2,
            },
            // moved_sexpr: struct {
            //     sexpr_index: usize,
            //     old_position: Vec2,
            //     new_position: Vec2,
            // },
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
        } else {
            try dst.sexprs.append(try .fromSexpr(&dst.hover_pool, Sexpr.pair_nil_nil, .{}, false));
        }

        dst.cases = .init(mem.gpa);
        try dst.cases.append(try .fromValues(&dst.hover_pool, .{
            .pattern = Sexpr.builtin.true,
            .template = Sexpr.builtin.false,
            .fnk_name = Sexpr.builtin.identity,
        }, .{ .pos = .new(0, 3) }));
    }

    pub fn deinit(workspace: *Workspace) void {
        workspace.lenses.deinit();
        workspace.sexprs.deinit();
        workspace.cases.deinit();
        workspace.hover_pool.deinit();
        workspace.undo_stack.deinit();
    }

    const valid: []const *const Sexpr = &.{
        &Sexpr.doLit("Hermes"),
        &Sexpr.doLit("Mercury"),
        &Sexpr.doLit("Ares"),
        &Sexpr.doLit("Mars"),
        &Sexpr.doLit("Zeus"),
        &Sexpr.doLit("Jupiter"),
        &Sexpr.doLit("Aphrodite"),
        &Sexpr.doLit("Venus"),
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

    fn popGrabbedSexpr(workspace: *Workspace) VeryPhysicalSexpr {
        const r = workspace.grabbed_sexpr.?;
        workspace.grabbed_sexpr = null;
        return r;
    }

    fn pushGrabbedSexpr(workspace: *Workspace, sexpr: VeryPhysicalSexpr) void {
        assert(workspace.grabbed_sexpr == null);
        workspace.grabbed_sexpr = sexpr;
    }

    fn sexprAtPlace(workspace: *Workspace, place: BaseSexprPlace) *VeryPhysicalSexpr {
        return switch (place) {
            .board => |p| &workspace.sexprs.items[workspace.boardSexprIndexAtPos(p).?],
            .grabbed => &workspace.grabbed_sexpr.?,
            .case => |case| workspace.cases.items[
                workspace.boardCaseIndexAtPos(case.parent_handle_pos).?
            ].childCase(case.local).sexprAt(case.part),
        };
    }

    fn boardSexprIndexAtPos(workspace: *Workspace, pos: Vec2) ?usize {
        if (@import("builtin").mode == .Debug) {
            var count: usize = 0;
            for (workspace.sexprs.items) |s| {
                if (s.point.pos.equals(pos)) {
                    count += 1;
                }
            }
            assert(count <= 1);
        }

        for (workspace.sexprs.items, 0..) |s, k| {
            if (s.point.pos.equals(pos)) return k;
        } else return null;
    }

    // TODO: reduce duplication
    fn boardCaseIndexAtPos(workspace: *Workspace, pos: Vec2) ?usize {
        if (@import("builtin").mode == .Debug) {
            var count: usize = 0;
            for (workspace.cases.items) |s| {
                if (s.handle.equals(pos)) {
                    count += 1;
                }
            }
            assert(count <= 1);
        }

        for (workspace.cases.items, 0..) |s, k| {
            if (s.handle.equals(pos)) return k;
        } else return null;
    }

    fn draw(workspace: *Workspace, platform: PlatformGives, drawer: *Drawer, camera: Rect) !void {
        for (workspace.sexprs.items) |s| {
            try s.draw(drawer, camera);
        }

        for (workspace.cases.items) |c| {
            try c.draw(drawer, camera);
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

        if (workspace.grabbed_sexpr) |s| try s.draw(drawer, camera);
    }

    fn findHoverableOrDropzoneAtPosition(workspace: *Workspace, pos: Vec2, res: std.mem.Allocator) !Focus.Target {
        const grabbed_tag = std.meta.activeTag(workspace.focus.grabbing.kind);

        // lenses
        if (grabbed_tag == .nothing) {
            for (workspace.lenses.items, 0..) |*lens, k| {
                if (pos.distTo(lens.sourceHandlePos()) < Lens.handle_radius) {
                    return .{ .kind = .{ .lens_handle = .{ .index = k, .part = .source } } };
                }
                if (pos.distTo(lens.targetHandlePos()) < Lens.handle_radius) {
                    return .{ .kind = .{ .lens_handle = .{ .index = k, .part = .target } } };
                }
            }
        }

        // sexprs inside lenses and on the board and on cases
        if (grabbed_tag == .nothing or grabbed_tag == .sexpr) {
            for (workspace.lenses.items) |lens| {
                if (pos.distTo(lens.target) < lens.target_radius) {
                    for (lens.tmp_visible_sexprs.items) |s| {
                        if (s.original_place == .grabbed) continue;
                        const original = sexprAtPlace(workspace, s.original_place);
                        if (try ViewHelper.overlapsSexpr(
                            // TODO: don't leak
                            res,
                            original.is_pattern,
                            original.value,
                            s.lens_transform.actOn(original.point),
                            pos,
                        )) |address| {
                            return .{
                                .kind = .{
                                    .sexpr = .{
                                        .base = s.original_place,
                                        .local = address,
                                    },
                                },
                                .lens_transform = s.lens_transform,
                            };
                        }
                    }
                }
            }

            for (workspace.sexprs.items) |s| {
                if (try ViewHelper.overlapsSexpr(res, s.is_pattern, s.value, s.point, pos)) |address| {
                    return .{ .kind = .{ .sexpr = .{ .base = .{ .board = s.point.pos }, .local = address } } };
                }
            }

            for (workspace.cases.items) |c| {
                assert(@FieldType(VeryPhysicalCase, "next") == void);
                inline for (core.CasePart.all) |part| {
                    const s = c.constSexprAt(part);
                    if (try ViewHelper.overlapsSexpr(res, part == .pattern, s.value, s.point, pos)) |address| {
                        return .{ .kind = .{ .sexpr = .{ .base = .{ .case = .{
                            .parent_handle_pos = c.handle,
                            .part = part,
                            .local = &.{},
                        } }, .local = address } } };
                    }
                }
            }
        }

        if (grabbed_tag == .nothing) {
            for (workspace.cases.items, 0..) |*case, k| {
                if (pos.distTo(case.handle) < VeryPhysicalCase.handle_radius) {
                    return .{ .kind = .{ .case_handle = .{ .index = k } } };
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
            for (workspace.sexprs.items) |s| {
                try lens.tmp_visible_sexprs.append(frame_arena, .{
                    .original_place = .{ .board = s.point.pos },
                    .lens_transform = lens.getTransform(),
                });
            }
            if (workspace.grabbed_sexpr) |_| {
                try lens.tmp_visible_sexprs.append(frame_arena, .{
                    .original_place = .grabbed,
                    .lens_transform = lens.getTransform(),
                });
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
                    .picked_lens_or_case => |p| {
                        switch (p.target.kind) {
                            .lens_handle => |h| {
                                const lens = &workspace.lenses.items[h.index];
                                lens.setHandlePos(h.part, p.old_position);
                                assert(workspace.focus.grabbing.kind.lens_handle.part == h.part);
                                assert(workspace.focus.grabbing.kind.lens_handle.index == h.index);
                            },
                            .case_handle => |h| {
                                const case = &workspace.cases.items[h.index];
                                case.handle = p.old_position;
                                assert(workspace.focus.grabbing.kind.case_handle.index == h.index);
                            },
                            else => unreachable,
                        }
                        workspace.focus.grabbing = .nothing;
                    },
                    .dropped_lens_or_case => |p| {
                        workspace.focus.grabbing = p.target;
                        const next_cmd = workspace.undo_stack.pop().?;
                        assert(std.meta.activeTag(next_cmd.specific) == .picked_lens_or_case);
                        continue :again next_cmd.specific;
                    },
                    .grabbed_sexpr => |g| {
                        var grabbed = workspace.popGrabbedSexpr();
                        // recreate it if needed
                        switch (g.origin.base) {
                            .grabbed => unreachable,
                            .board => |position| if (g.origin.local.len == 0) {
                                grabbed.point.pos = position;
                                try workspace.sexprs.append(grabbed);
                            },
                            .case => {},
                        }
                        const old = workspace.sexprAtPlace(g.origin.base).getSubValue(g.origin.local);
                        old.hovered.value = 10;
                        workspace.focus.grabbing = .nothing;
                    },
                    .dropped_sexpr => |p| {
                        if (p.overwritten_value) |overwritten| {
                            const base = workspace.sexprAtPlace(p.new_place.base);
                            var grabbed = base.getSubValue(p.new_place.local);
                            grabbed.point.scale = 1;
                            try base.updateSubValue(
                                p.new_place.local,
                                overwritten.value,
                                overwritten.hovered,
                                mem,
                                &workspace.hover_pool,
                            );
                            workspace.pushGrabbedSexpr(grabbed);
                        } else {
                            assert(p.new_place.local.len == 0);
                            const grabbed = switch (p.new_place.base) {
                                else => unreachable,
                                .board => |position| workspace.sexprs.swapRemove(workspace.boardSexprIndexAtPos(position).?),
                            };
                            workspace.pushGrabbedSexpr(grabbed);
                        }
                        workspace.focus.grabbing = .grabbed_sexpr;
                        const next_cmd = workspace.undo_stack.pop().?;
                        assert(std.meta.activeTag(next_cmd.specific) == .grabbed_sexpr);
                        continue :again next_cmd.specific;
                    },
                }
            }
        }

        const mouse = platform.getMouse(camera);
        const hovered_or_dropzone_thing = try workspace.findHoverableOrDropzoneAtPosition(mouse.cur.position, mem.gpa);

        const hovering: Focus.Target = switch (workspace.focus.grabbing.kind) {
            else => .nothing,
            .nothing => hovered_or_dropzone_thing,
        };
        const dropzone: Focus.Target = switch (workspace.focus.grabbing.kind) {
            else => hovered_or_dropzone_thing,
            .nothing => .nothing,
        };

        // cursor
        platform.setCursor(
            if (workspace.focus.grabbing.kind != .nothing)
                .grabbing
            else if (hovering.kind != .nothing)
                .could_grab
            else
                .default,
        );

        // update hover_t
        // TODO: iterator over all sexprs?
        for (workspace.sexprs.items) |*s| {
            const hovered = switch (hovering.kind) {
                else => null,
                .sexpr => |sexpr| switch (sexpr.base) {
                    // special case: no hover anim for base values
                    .board => |pos| if (pos.equals(s.point.pos) and sexpr.local.len > 0) sexpr.local else null,
                    .case => null,
                    .grabbed => unreachable,
                },
            };
            s.hovered.update(hovered, 1.0, platform.delta_seconds);
            s.updateIsPattern(platform.delta_seconds);
        }
        for (workspace.cases.items) |*c| {
            assert(@FieldType(VeryPhysicalCase, "next") == void);
            inline for (core.CasePart.all) |part| {
                const hovered = switch (hovering.kind) {
                    else => null,
                    .sexpr => |sexpr| switch (sexpr.base) {
                        .board => null,
                        .case => |case| if (case.parent_handle_pos.equals(c.handle) and case.part == part) sexpr.local else null,
                        .grabbed => unreachable,
                    },
                };
                c.sexprAt(part).hovered.update(hovered, 1.0, platform.delta_seconds);
                c.sexprAt(part).updateIsPattern(platform.delta_seconds);
            }
        }
        if (workspace.grabbed_sexpr) |*grabbed| {
            grabbed.is_pattern = switch (dropzone.kind) {
                .nothing => grabbed.is_pattern,
                .lens_handle, .case_handle => unreachable,
                .sexpr => |s| workspace.sexprAtPlace(s.base).is_pattern,
            };
            grabbed.hovered.update(switch (dropzone.kind) {
                .sexpr => &.{},
                .nothing => null,
                else => unreachable,
            }, 2.0, platform.delta_seconds);
            grabbed.updateIsPattern(platform.delta_seconds);
        }
        for (workspace.cases.items, 0..) |*c, k| {
            const target: f32 = switch (hovering.kind) {
                else => 0,
                .case_handle => |handle| if (handle.index == k) 1 else 0,
            };
            math.lerp_towards(&c.handle_hot_t, target, 0.6, platform.delta_seconds);
        }
        for (workspace.lenses.items, 0..) |*lens, k| {
            const hovered = switch (hovering.kind) {
                else => null,
                .lens_handle => |handle| if (handle.index == k) handle.part else null,
            };
            lens.update(hovered, platform.delta_seconds);
        }

        // apply dragging
        switch (workspace.focus.grabbing.kind) {
            .nothing => {},
            .lens_handle => |p| {
                const lens = &workspace.lenses.items[p.index];
                switch (p.part) {
                    .source => lens.source.addInPlace(mouse.deltaPos()),
                    .target => lens.target.addInPlace(mouse.deltaPos()),
                }
            },
            .case_handle => |p| {
                const case = &workspace.cases.items[p.index];
                case.handle = mouse.cur.position;
            },
            .sexpr => {
                assert(workspace.grabbed_sexpr != null);
                const target: Point = switch (dropzone.kind) {
                    .sexpr => |s| blk: {
                        break :blk ViewHelper.sexprChildView(
                            workspace.grabbed_sexpr.?.is_pattern,
                            dropzone.lens_transform.actOn(workspace.sexprAtPlace(s.base).point),
                            s.local,
                        );
                    },
                    .nothing => .{
                        .pos = mouse.cur.position,
                        .scale = 1,
                    },
                    else => unreachable,
                };
                workspace.grabbed_sexpr.?.point.lerp_towards(target, 0.6, platform.delta_seconds);
            },
        }

        // update cases spring positions
        for (workspace.cases.items) |*c| {
            c.update(platform.delta_seconds);
        }

        const action: UndoableCommand = if (workspace.focus.grabbing.kind == .nothing and mouse.wasPressed(.left))
            switch (hovering.kind) {
                .nothing => .noop,
                .lens_handle => |h| .{ .specific = .{
                    .picked_lens_or_case = .{
                        .target = hovering,
                        .old_position = workspace.lenses.items[h.index].handlePos(h.part),
                    },
                } },
                .case_handle => |h| .{ .specific = .{
                    .picked_lens_or_case = .{
                        .target = hovering,
                        .old_position = workspace.cases.items[h.index].handle,
                    },
                } },
                .sexpr => |s| .{
                    .specific = .{
                        .grabbed_sexpr = .{ .origin = s },
                    },
                },
            }
        else if (workspace.focus.grabbing.kind != .nothing and !mouse.cur.isDown(.left))
            switch (workspace.focus.grabbing.kind) {
                .nothing => unreachable,
                .lens_handle => |h| .{ .specific = .{
                    .dropped_lens_or_case = .{
                        .target = workspace.focus.grabbing,
                        .new_position = workspace.lenses.items[h.index].handlePos(h.part),
                    },
                } },
                .case_handle => |h| .{ .specific = .{
                    .dropped_lens_or_case = .{
                        .target = workspace.focus.grabbing,
                        .new_position = workspace.cases.items[h.index].handle,
                    },
                } },
                .sexpr => .{
                    .specific = .{
                        .dropped_sexpr = switch (dropzone.kind) {
                            .nothing => .{
                                .new_place = .{ .base = .{
                                    .board = workspace.grabbed_sexpr.?.point.pos,
                                }, .local = &.{} },
                                .overwritten_value = null,
                            },
                            .sexpr => |s| .{
                                .new_place = s,
                                .overwritten_value = workspace.sexprAtPlace(s.base).getSubValue(s.local),
                            },
                            else => unreachable,
                        },
                    },
                },
            }
        else
            .noop;

        // actually perform the action
        switch (action.specific) {
            .noop => {},
            .picked_lens_or_case => |h| workspace.focus.grabbing = h.target,
            .dropped_lens_or_case => workspace.focus.grabbing = .nothing,
            .dropped_sexpr => {
                switch (dropzone.kind) {
                    else => unreachable,
                    .nothing => try workspace.sexprs.append(workspace.popGrabbedSexpr()),
                    .sexpr => |s| {
                        const grabbed = workspace.popGrabbedSexpr();
                        try workspace.sexprAtPlace(s.base).updateSubValue(
                            s.local,
                            grabbed.value,
                            grabbed.hovered,
                            mem,
                            &workspace.hover_pool,
                        );
                    },
                }
                workspace.focus.grabbing = .nothing;
            },
            .grabbed_sexpr => {
                switch (hovering.kind) {
                    else => unreachable,
                    .sexpr => |h| {
                        const destroyed_grabbed: ?VeryPhysicalSexpr = switch (h.base) {
                            .grabbed => unreachable,
                            .board => |p| if (h.local.len == 0)
                                workspace.sexprs.swapRemove(workspace.boardSexprIndexAtPos(p).?)
                            else
                                null,
                            .case => null,
                        };
                        var grabbed: VeryPhysicalSexpr = destroyed_grabbed orelse blk: {
                            const original = workspace.sexprAtPlace(h.base).getSubValue(h.local);
                            break :blk .{
                                .hovered = try original.hovered.clone(&workspace.hover_pool),
                                .value = original.value,
                                .point = original.point,
                                .is_pattern = original.is_pattern,
                                .is_pattern_t = original.is_pattern_t,
                            };
                        };
                        grabbed.point = hovering.lens_transform.actOn(grabbed.point);
                        workspace.pushGrabbedSexpr(grabbed);
                    },
                }
                workspace.focus.grabbing = .{ .kind = .{ .sexpr = .{ .base = .grabbed, .local = &.{} } } };
            },
        }
        if (action.specific != .noop) {
            try workspace.undo_stack.append(action);
        }
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
