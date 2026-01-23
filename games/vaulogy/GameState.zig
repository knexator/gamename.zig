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
                .recording_log = null,

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
            try workspace.init(std.testing.allocator);
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
            try player.workspace.update(player.test_platform.getGives(1.0 / 60.0), null);
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
        GameState.FuzzerContext.FakeInput{ .z_down = false, .mouse_left_down = false, .mouse_pos = .new(0.61098903, 0.38476562) },
        GameState.FuzzerContext.FakeInput{ .z_down = false, .mouse_left_down = false, .mouse_pos = .new(0.53186816, 0.46679688) },
        GameState.FuzzerContext.FakeInput{ .z_down = false, .mouse_left_down = true, .mouse_pos = .new(0.53406596, 0.46289062) },
        GameState.FuzzerContext.FakeInput{ .z_down = false, .mouse_left_down = true, .mouse_pos = .new(0.545055, 0.28515625) },
        GameState.FuzzerContext.FakeInput{ .z_down = false, .mouse_left_down = false, .mouse_pos = .new(0.545055, 0.28515625) },
        GameState.FuzzerContext.FakeInput{ .z_down = false, .mouse_left_down = false, .mouse_pos = .new(0.5802198, 0.22265625) },
        GameState.FuzzerContext.FakeInput{ .z_down = true, .mouse_left_down = false, .mouse_pos = .new(0.5857143, 0.21875) },
    };
    for (inputs) |input| try player.advance(input);
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

/// Might be an Area, a Sexpr, a Case, etc
pub const Lego = struct {
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

    // ll = linked list
    ll_area_prev: Index = .nothing,
    ll_area_next: Index = .nothing,

    /// only valid for freed nodes
    free_next: Index = undefined,

    specific: Specific,

    pub const Specific = union(enum) {
        sexpr: Sexpr,
        area: Area,

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

        pub const Area = struct {
            first: Index = .nothing,
            last: Index = .nothing,

            pub fn addChildLast(area: *Area, toybox: *Toybox, child: Lego.Index) void {
                const child_lego = toybox.get(child);
                assert(child_lego.ll_area_prev == .nothing);
                assert(child_lego.ll_area_next == .nothing);
                if (area.last != .nothing) {
                    assert(area.first != .nothing);
                    const old_last = toybox.get(area.last);
                    child_lego.ll_area_prev = area.last;
                    assert(toybox.get(area.last).ll_area_next == .nothing);
                    old_last.ll_area_next = child;
                    area.last = child;
                } else {
                    assert(area.first == .nothing);
                    area.first = child;
                    area.last = child;
                }
            }

            pub fn popChild(area: *Area, toybox: *Toybox, child: Lego.Index) void {
                assert(area.first != .nothing and area.last != .nothing);
                const old_prev = toybox.get(child).ll_area_prev;
                const old_next = toybox.get(child).ll_area_next;
                toybox.get(child).ll_area_next = .nothing;
                toybox.get(child).ll_area_prev = .nothing;

                if (old_prev != .nothing) {
                    assert(area.first != child);
                    toybox.get(old_prev).ll_area_next = old_next;
                } else {
                    assert(area.first == child);
                    area.first = old_next;
                }

                if (old_next != .nothing) {
                    assert(area.last != child);
                    toybox.get(old_next).ll_area_prev = old_prev;
                } else {
                    assert(area.last == child);
                    area.last = old_prev;
                }
            }

            pub fn restoreChild(area: *Area, toybox: *Toybox, child: Lego.Index) void {
                assert(child != .nothing);
                defer assert(area.first != .nothing and area.last != .nothing);

                const old_prev = toybox.get(child).ll_area_prev;
                const old_next = toybox.get(child).ll_area_next;

                if (old_prev != .nothing) {
                    toybox.get(old_prev).ll_area_next = child;
                } else {
                    area.first = child;
                }

                if (old_next != .nothing) {
                    toybox.get(old_next).ll_area_prev = child;
                } else {
                    area.last = child;
                }
            }
        };

        pub const Sexpr = struct {
            kind: Kind,
            is_pattern: bool,
            is_pattern_t: f32,
            atom_name: []const u8,
            children: struct { up: Lego.Index, down: Lego.Index },

            pub const Kind = enum { pair, empty, atom_var, atom_lit };

            pub fn contains(sexpr_point: Point, is_pattern: bool, kind: Kind, needle_pos: Vec2) bool {
                return ViewHelper.overlapsAtom(is_pattern, sexpr_point, needle_pos, switch (kind) {
                    .atom_var, .atom_lit, .empty => .atom,
                    .pair => .pair,
                });
            }
        };
    };

    pub const Index = enum(u32) { nothing = std.math.maxInt(u32), _ };

    // TODO: could be a better API
    const Interaction = struct {
        reverse_path: std.ArrayListUnmanaged(Lego.Index) = .empty,
        kind: Kind,

        pub const Kind = enum { nothing, hot, dropzone };
    };
    pub fn findHotAndDropzone(toybox: *Toybox, index: Lego.Index, needle_pos: Vec2, grabbing: Lego.Index, allocator: std.mem.Allocator, depth: usize) !Interaction {
        const lego = toybox.get(index);
        switch (lego.specific) {
            .area => |area| {
                var cur = area.last;
                while (cur != .nothing) : (cur = toybox.get(cur).ll_area_prev) {
                    var result = try findHotAndDropzone(toybox, cur, needle_pos, grabbing, allocator, depth + 1);
                    if (result.kind != .nothing) {
                        result.reverse_path.appendAssumeCapacity(index);
                        return result;
                    }
                }
            },
            .sexpr => |sexpr| {
                if (sexpr.kind == .pair and lego.point.inverseApplyGetLocalPosition(needle_pos).magSq() < 9) {
                    var interaction_up = try findHotAndDropzone(toybox, sexpr.children.up, needle_pos, grabbing, allocator, depth + 1);
                    if (interaction_up.kind != .nothing) {
                        try interaction_up.reverse_path.append(allocator, index);
                        return interaction_up;
                    }

                    var interaction_down = try findHotAndDropzone(toybox, sexpr.children.down, needle_pos, grabbing, allocator, depth + 1);
                    if (interaction_down.kind != .nothing) {
                        try interaction_down.reverse_path.append(allocator, index);
                        return interaction_down;
                    }
                }

                if (Specific.Sexpr.contains(lego.point, sexpr.is_pattern, sexpr.kind, needle_pos)) {
                    const interaction: Interaction.Kind = if (grabbing == .nothing)
                        .hot
                    else if (toybox.get(grabbing).specific.tag() == .sexpr)
                        .dropzone
                    else
                        .nothing;

                    if (interaction != .nothing) {
                        var path: std.ArrayListUnmanaged(Lego.Index) = try .initCapacity(allocator, depth + 1);
                        path.appendAssumeCapacity(index);
                        return .{ .reverse_path = path, .kind = interaction };
                    }
                }
            },
        }

        return .{ .kind = .nothing };
    }

    pub fn updateSprings(toybox: *Toybox, index: Lego.Index, mouse_pos: Vec2, grabbing: Lego.Index, dropzone: Lego.Index, delta_seconds: f32) void {
        const lego = toybox.get(index);
        if (index == grabbing) {
            const target: Point = if (dropzone == .nothing) .{ .pos = mouse_pos } else toybox.get(dropzone).point;
            lego.point.lerp_towards(target, 0.6, delta_seconds);
            // TODO: set 'is_pattern'
        }
        switch (lego.specific) {
            .area => |area| {
                var cur = area.first;
                while (cur != .nothing) : (cur = toybox.get(cur).ll_area_next) {
                    updateSprings(toybox, cur, mouse_pos, grabbing, dropzone, delta_seconds);
                }
            },
            .sexpr => |sexpr| {
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
                    toybox.get(sexpr.children.up).point = lego.point
                        .applyToLocalPoint(lego.visual_offset)
                        .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .up));
                    toybox.get(sexpr.children.down).point = lego.point
                        .applyToLocalPoint(lego.visual_offset)
                        .applyToLocalPoint(ViewHelper.offsetFor(sexpr.is_pattern, .down));
                    updateSprings(toybox, sexpr.children.up, mouse_pos, grabbing, dropzone, delta_seconds);
                    updateSprings(toybox, sexpr.children.down, mouse_pos, grabbing, dropzone, delta_seconds);
                }
            },
        }
    }

    pub fn draw(toybox: *Toybox, index: Lego.Index, camera: Rect, platform: PlatformGives, drawer: *Drawer) !void {
        const lego = toybox.get(index);
        const point = lego.point.applyToLocalPoint(lego.visual_offset);
        switch (lego.specific) {
            .area => |area| {
                var cur = area.first;
                while (cur != .nothing) : (cur = toybox.get(cur).ll_area_next) {
                    try draw(toybox, cur, camera, platform, drawer);
                }
            },
            .sexpr => |sexpr| {
                // TODO: don't draw if small or far from camera
                switch (sexpr.kind) {
                    .empty => {},
                    .atom_lit => try drawer.drawAtom(camera, point, sexpr.is_pattern, sexpr.atom_name, 1),
                    .pair => try drawer.drawPairHolder(camera, point, sexpr.is_pattern, 1),
                    .atom_var => @panic("TODO"),
                }

                if (sexpr.kind == .pair) {
                    try draw(toybox, sexpr.children.up, camera, platform, drawer);
                    try draw(toybox, sexpr.children.down, camera, platform, drawer);
                }
            },
        }
    }

    pub fn destroyFloating(toybox: *Toybox, thing: Lego.Index) void {
        assert(toybox.get(thing).ll_area_prev == .nothing);
        assert(toybox.get(thing).ll_area_next == .nothing);
        toybox.free(thing);
    }

    pub fn recreateFloating(toybox: *Toybox, data_ignoring_index: Lego) !Lego.Index {
        assert(data_ignoring_index.ll_area_next == .nothing);
        assert(data_ignoring_index.ll_area_prev == .nothing);

        const new_thing = try toybox.add(undefined);
        const index = new_thing.index;
        new_thing.* = data_ignoring_index;
        new_thing.index = index;

        return index;
    }

    /// returns true if child could be released, false if not (which means you should make a copy of it)
    pub fn releaseChild(toybox: *Toybox, child: Lego.Index, parent: Lego.Index) bool {
        assert(child != .nothing and parent != .nothing);
        switch (toybox.get(parent).specific) {
            .area => |*area| {
                area.popChild(toybox, child);
                return true;
            },
            .sexpr => return false,
        }
    }

    pub fn recaptureChild(toybox: *Toybox, child_data: Lego, parent: Lego.Index) void {
        assert(child_data.index != .nothing and parent != .nothing);
        switch (toybox.get(parent).specific) {
            .area => |*area| {
                // restore the position, links, etc
                toybox.get(child_data.index).* = child_data;
                area.restoreChild(toybox, child_data.index);
            },
            .sexpr => unreachable,
        }
    }

    // pub fn onClicked(toybox: *Toybox, index: Lego.Index) !void {
    //     const lego = toybox.get(index);
    //     switch (lego.specific) {
    //         .area => unreachable,
    //     }
    // }
};

pub const Toybox = struct {
    // TODO: use a fancy arena thing
    all_legos: std.ArrayListUnmanaged(Lego),
    all_legos_arena: std.heap.ArenaAllocator,
    free_head: Lego.Index = .nothing,

    pub fn init(dst: *Toybox, gpa: std.mem.Allocator) !void {
        dst.* = .{
            .all_legos = .empty,
            .all_legos_arena = .init(gpa),
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

    pub fn add(toybox: *Toybox, specific: Lego.Specific) !*Lego {
        if (toybox.free_head == .nothing) {
            const result = try toybox.all_legos.addOne(toybox.all_legos_arena.allocator());
            result.* = .{
                .index = @enumFromInt(toybox.all_legos.items.len - 1),
                .specific = specific,
            };
            return result;
        } else {
            const index = toybox.free_head;
            const result = toybox.get(index);
            toybox.free_head = result.free_next;
            result.* = .{
                .index = index,
                .specific = specific,
            };
            return result;
        }
    }

    pub fn free(toybox: *Toybox, lego: Lego.Index) void {
        toybox.get(lego).* = undefined;
        toybox.get(lego).index = lego;
        toybox.get(lego).free_next = toybox.free_head;
        toybox.free_head = lego;
    }

    pub fn get(toybox: *Toybox, index: Lego.Index) *Lego {
        return &toybox.all_legos.items[@intFromEnum(index)];
    }

    pub fn dupeFloating(toybox: *Toybox, original: Lego.Index) !*Lego {
        // TODO: should dupe children?
        const result = try toybox.add(undefined);
        const result_index = result.index;
        result.* = toybox.get(original).*;
        result.index = result_index;
        result.ll_area_next = .nothing;
        result.ll_area_prev = .nothing;
        return result;
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
        set_grabbing: struct { grabbing: Lego.Index, hand_layer: Lego.Index },
        reset_data: Lego,
        recapture_child: struct { data: Lego, parent: Lego.Index },
        release_child: struct { child: Lego.Index, parent: Lego.Index },
        destroy_floating: Lego.Index,
        recreate_floating_and_maybe_set_it_grabbing_and_hand_layer: Lego,
        // setGrabbing: Lego.Index,
        // addChildFirst: struct { parent: Lego.Index, new_child: Lego.Index },
        // changeChild: struct { original_child: Lego.Index, new_child: Lego.Index },
        // setData: struct { data: Lego, target: Lego.Index },
    };

    pub fn init(dst: *Workspace, gpa: std.mem.Allocator) !void {
        dst.* = kommon.meta.initDefaultFields(Workspace);
        dst.undo_stack = .init(gpa);
        try dst.toybox.init(gpa);

        const toybox = &dst.toybox;

        const main_area = try toybox.add(.{ .area = .{} });
        dst.main_area = main_area.index;

        if (true) {
            const sample_sexpr = try toybox.add(.{ .sexpr = .{
                .atom_name = "true",
                .kind = .atom_lit,
                .is_pattern = false,
                .is_pattern_t = 0,
                .children = undefined,
            } });
            main_area.specific.area.addChildLast(toybox, sample_sexpr.index);
        }

        if (true) {
            const sample_sexpr = try toybox.add(.{ .sexpr = .{
                .atom_name = "false",
                .kind = .atom_lit,
                .is_pattern = false,
                .is_pattern_t = 0,
                .children = undefined,
            } });
            sample_sexpr.point.pos = .new(0, 1);
            main_area.specific.area.addChildLast(toybox, sample_sexpr.index);
        }

        if (true) {
            const child_1 = try toybox.add(.{ .sexpr = .{
                .atom_name = "true",
                .kind = .atom_lit,
                .is_pattern = false,
                .is_pattern_t = 0,
                .children = undefined,
            } });
            const child_2 = try toybox.add(.{ .sexpr = .{
                .atom_name = "false",
                .kind = .atom_lit,
                .is_pattern = false,
                .is_pattern_t = 0,
                .children = undefined,
            } });
            const sample_sexpr = try toybox.add(.{ .sexpr = .{
                .atom_name = undefined,
                .kind = .pair,
                .is_pattern = false,
                .is_pattern_t = 0,
                .children = .{ .up = child_1.index, .down = child_2.index },
            } });
            sample_sexpr.point.pos = .new(4, 0);
            main_area.specific.area.addChildLast(toybox, sample_sexpr.index);
        }
    }

    pub fn deinit(workspace: *Workspace) void {
        workspace.toybox.deinit();
        workspace.undo_stack.deinit();
    }

    fn draw(workspace: *Workspace, platform: PlatformGives, drawer: *Drawer) !void {
        const asdf = tracy.initZone(@src(), .{ .name = "draw" });
        defer asdf.deinit();

        const camera = workspace.camera;
        const toybox = &workspace.toybox;

        const roots: [2]Lego.Index = .{ workspace.main_area, workspace.hand_layer };
        for (&roots) |root| {
            if (root == .nothing) continue;
            try Lego.draw(
                toybox,
                root,
                camera,
                platform,
                drawer,
            );
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
        const mouse = platform.getMouse(camera);

        if (platform.recording_log) |log| {
            const S = struct {
                var prev_input: FuzzerContext.FakeInput = .{
                    .mouse_left_down = false,
                    .z_down = false,
                    .mouse_pos = .zero,
                };
            };
            // TODO: actually use this
            const cur_input: FuzzerContext.FakeInput = .{
                .mouse_left_down = mouse.cur.isDown(.left),
                .z_down = platform.keyboard.cur.isDown(.KeyZ),
                .mouse_pos = platform.getMouse(.unit).cur.position,
            };
            if (!std.meta.eql(cur_input, S.prev_input)) {
                try log.print("{any},\n", .{cur_input});
                // try log.writeStruct(cur_input);
                S.prev_input = cur_input;
            }
        }

        if (platform.keyboard.wasPressed(.KeyZ)) {
            const toybox = &workspace.toybox;
            while (workspace.undo_stack.pop()) |command| {
                switch (command) {
                    .fence => break,
                    .destroy_floating => |index| {
                        Lego.destroyFloating(toybox, index);
                    },
                    .recreate_floating_and_maybe_set_it_grabbing_and_hand_layer => |data| {
                        const new_index = try Lego.recreateFloating(toybox, data);
                        // Some things might be pointing to the old index, so we update them here
                        if (workspace.grabbing == data.index) workspace.grabbing = new_index;
                        if (workspace.hand_layer == data.index) workspace.hand_layer = new_index;
                    },
                    .recapture_child => |restore| {
                        Lego.recaptureChild(toybox, restore.data, restore.parent);
                    },
                    .release_child => |pop| {
                        assert(Lego.releaseChild(toybox, pop.child, pop.parent));
                    },
                    .set_grabbing => |set| {
                        workspace.grabbing = set.grabbing;
                        workspace.hand_layer = set.hand_layer;
                    },
                    .reset_data => |data| {
                        toybox.get(data.index).* = data;
                    },
                }
            }
        }

        const interaction: Lego.Interaction = try Lego.findHotAndDropzone(
            &workspace.toybox,
            workspace.main_area,
            mouse.cur.position,
            workspace.grabbing,
            platform.frame_arena,
            0,
        );

        // const hovering: Lego.Index = if (workspace.focus.grabbing == .nothing) hovered_or_dropzone_thing.which else .nothing;
        // const dropzone: Lego.Index = if (workspace.focus.grabbing != .nothing) hovered_or_dropzone_thing.which else .nothing;

        // cursor
        platform.setCursor(
            if (workspace.grabbing != .nothing)
                .grabbing // or maybe .pointer, if it's UI
            else if (interaction.kind == .hot)
                .could_grab // or maybe .pointer, if it's UI
            else
                .default,
        );

        // update _t
        for (workspace.toybox.all_legos.items) |*lego| {
            math.lerp_towards(&lego.hot_t, if (interaction.kind == .hot and
                lego.index == interaction.reverse_path.items[0]) 1 else 0, 0.6, platform.delta_seconds);
            comptime assert(!@hasField(Lego, "active_t"));
            comptime assert(!@hasField(Lego, "dropzone_t"));
            math.lerp_towards(&lego.dropping_t, if (interaction.kind == .dropzone and
                lego.index == workspace.grabbing) 1 else 0, 0.6, platform.delta_seconds);
        }

        // includes dragging and snapping to dropzone, since that's just the spring between the mouse cursor/dropzone and the grabbed thing
        for ([2]Lego.Index{ workspace.main_area, workspace.hand_layer }) |root| {
            if (root == .nothing) continue;
            Lego.updateSprings(
                &workspace.toybox,
                root,
                mouse.cur.position,
                workspace.grabbing,
                if (interaction.kind == .dropzone) interaction.reverse_path.items[0] else .nothing,
                platform.delta_seconds,
            );
        }

        if (drawer) |d| {
            try workspace.draw(platform, d);
        }

        // INTERACTION
        const toybox = &workspace.toybox;
        try workspace.undo_stack.ensureUnusedCapacity(10);
        if (workspace.grabbing == .nothing and
            interaction.kind == .hot and
            (mouse.wasPressed(.left) or mouse.wasPressed(.right)))
        {
            // Main case A: plucking/grabbing/clicking something
            try workspace.undo_stack.append(.fence);

            const hot_index = interaction.reverse_path.items[0];
            const hot_parent_index = interaction.reverse_path.items[1];

            const original_hot_data = toybox.get(hot_index).*;
            var grabbed_element_index: Lego.Index = undefined;
            if (toybox.get(hot_parent_index).specific.as(.area)) |area| {
                // Case A.1: plucking a top-level thing from an area
                area.popChild(toybox, hot_index);
                workspace.undo_stack.appendAssumeCapacity(.{
                    .recapture_child = .{
                        .data = original_hot_data,
                        .parent = hot_parent_index,
                    },
                });
                grabbed_element_index = hot_index;
            } else if (toybox.get(hot_index).specific.as(.sexpr)) |sexpr| {
                // Case A.2: plucking a nested sexpr
                const new_element = try toybox.dupeFloating(hot_index);
                try workspace.undo_stack.append(.{ .destroy_floating = new_element.index });
                grabbed_element_index = new_element.index;

                sexpr.kind = .empty;
                try workspace.undo_stack.append(.{ .reset_data = original_hot_data });
            } else unreachable;

            assert(workspace.grabbing == .nothing and workspace.hand_layer == .nothing);
            try workspace.undo_stack.append(.{ .set_grabbing = .{
                .grabbing = .nothing,
                .hand_layer = .nothing,
            } });
            workspace.grabbing = grabbed_element_index;
            workspace.hand_layer = grabbed_element_index;
        } else if (workspace.grabbing != .nothing and
            !(mouse.cur.isDown(.left) or mouse.cur.isDown(.right)))
        {
            // Main case B: droping/releasing something

            // const overwritten_lego_index = if (interaction.kind == .dropzone)
            //     interaction.reverse_path.items[0]
            // else blk: {
            //     const asdf = try toybox.add(undefined);
            //     toybox.get(workspace.main_area).specific.area.addChildLast(toybox, asdf.index);
            //     break :blk asdf.index;
            // }

            if (interaction.kind == .dropzone) {
                const dropzone_index = interaction.reverse_path.items[0];

                if (toybox.get(dropzone_index).specific.tag() == .sexpr) {
                    // Case B.1: dropping a sexpr on a sexpr
                    // the grabbed sexpr gets destroyed and the dropzone one gets modified,
                    // so that the parent of the dropzone one sees no changes
                    assert(toybox.get(workspace.grabbing).specific.tag() == .sexpr);

                    const index_of_overwritten = dropzone_index;
                    const overwritten_data = toybox.get(index_of_overwritten).*;
                    const grabbed_data = toybox.get(workspace.grabbing).*;

                    // overwrite it completely, except its area relation and its index
                    var new_data = grabbed_data;
                    new_data.index = overwritten_data.index;
                    new_data.ll_area_next = overwritten_data.ll_area_next;
                    new_data.ll_area_prev = overwritten_data.ll_area_prev;
                    toybox.get(index_of_overwritten).* = new_data;
                    workspace.undo_stack.appendAssumeCapacity(.{ .reset_data = overwritten_data });

                    Lego.destroyFloating(toybox, workspace.grabbing);
                    workspace.undo_stack.appendAssumeCapacity(.{ .recreate_floating_and_maybe_set_it_grabbing_and_hand_layer = grabbed_data });
                } else unreachable;
            } else {
                // Case B.2: dropping something on fresh space
                // we directly set it as the area's last child

                // could be unified with the other case, by creating a fresh last child and then overwriting it
                // but that seems to complicate rather than simplify
                assert(interaction.kind == .nothing);
                toybox.get(workspace.main_area).specific.area.addChildLast(toybox, workspace.grabbing);
                try workspace.undo_stack.append(.{ .release_child = .{
                    .child = workspace.grabbing,
                    .parent = workspace.main_area,
                } });
            }

            // if workspace.grabbing was destroyed, this index will be wrong!
            // It should be the index returned by .recreateFloating, which we
            // fix by hardcoding the fix in the undo command (TODO: less hacky?)
            try workspace.undo_stack.append(.{ .set_grabbing = .{
                .grabbing = workspace.grabbing,
                .hand_layer = workspace.hand_layer,
            } });
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
