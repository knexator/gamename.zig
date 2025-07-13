pub const GameState = @This();
const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "papuzo",
        .author = "knexator",
        .desired_aspect_ratio = 1.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "assets/fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

canvas: Canvas,
ui: UI.State,
mem: struct {
    frame: std.heap.ArenaAllocator,

    pub fn init(gpa: std.mem.Allocator) @This() {
        return .{ .frame = .init(gpa) };
    }

    pub fn deinit(self: *@This()) void {
        self.frame.deinit();
    }

    pub fn onFrameBegin(self: *@This()) void {
        _ = self.frame.reset(.retain_capacity);
    }
},

painting: ?enum { none, cross } = null,
board: kommon.Grid2D(TileState),
const TileState = union(enum) {
    block: ?u8,
    gap: struct {
        mark: enum { none, lamp, cross } = .none,
        lighted: bool = false,
    },
};

const raw_puzzle =
    \\......0...1
    \\.0.X...X...
    \\...........
    \\...2.2.....
    \\......1....
    \\.1..2...2..
    \\...........
    \\....1.X....
    \\....X....2.
    \\...........
    \\.....0..1..
;

pub fn init(
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !GameState {
    return .{
        .canvas = try .init(gl, gpa, &.{@embedFile("assets/fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}),
        .ui = .init(gpa),
        .mem = .init(gpa),
        .board = try .fromAsciiAndMap(gpa, raw_puzzle, struct {
            pub fn anon(c: u8) TileState {
                return switch (c) {
                    '.' => .{ .gap = .{} },
                    '0'...'4' => |n| .{ .block = n - '0' },
                    'X' => .{ .block = null },
                    else => panic("bad char: {d}", .{c}),
                };
            }
        }.anon),
    };
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    self.board.deinit(gpa);
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.mem.onFrameBegin();
    const camera: Rect = .{ .top_left = .zero, .size = .both(4) };
    {
        try self.ui.beginBuild(platform.getMouse(camera), platform.global_seconds, camera, .y);
        defer self.ui.endBuild(platform.delta_seconds);

        for (0..self.board.height) |j| {
            self.ui.setActiveDesiredSizeAndLayoutAxis(.{
                .{ .kind = .percent_of_parent, .value = 1.0 },
                .{ .kind = .percent_of_parent, .value = 1.0 / tof32(self.board.height) },
            }, .x);
            const cur_row = try self.ui.build_box(.fromFormat("row {d}", .{j}), .{});
            try self.ui.push_parent(cur_row);
            defer self.ui.pop_parent();

            self.ui.setActiveDesiredSizeAndLayoutAxis(.{
                .{ .kind = .percent_of_parent, .value = 1.0 / tof32(self.board.width) },
                .{ .kind = .percent_of_parent, .value = 1.0 },
            }, .x);
            for (0..self.board.width) |i| {
                const tile_state = self.board.getPtr(.new(i, j)) catch unreachable;
                const is_gap = std.meta.activeTag(tile_state.*) == .gap;

                self.ui.setActiveBackgroundColor(switch (tile_state.*) {
                    .block => .black,
                    .gap => |g| if (g.lighted) .fromHex("#BBFF87") else .white,
                });

                const box = try self.ui.build_box(
                    .fromFormat("tile {d} {d}", .{ i, j }),
                    .{ .clickable = is_gap, .visible = true },
                );

                box.text = switch (tile_state.*) {
                    .gap => |g| switch (g.mark) {
                        .none => null,
                        .lamp => "o",
                        .cross => ".",
                    },
                    .block => |k| if (k) |v| switch (v) {
                        0 => "0",
                        1 => "1",
                        2 => "2",
                        3 => "3",
                        4 => "4",
                        else => unreachable,
                    } else null,
                };
                box.text_color = switch (tile_state.*) {
                    .gap => |g| switch (g.mark) {
                        .none => undefined,
                        .lamp => .fromHex("#007F61"),
                        .cross => .fromHex("#00A11B"),
                    },
                    .block => |k| if (k != null) .white else undefined,
                };

                const signal = UI.signal_from_box(&self.ui, box);
                if (signal.flags.released.get(.right)) self.painting = null;
                if (signal.flags.pressed.get(.right)) {
                    self.painting = switch (tile_state.gap.mark) {
                        .cross => .none,
                        else => .cross,
                    };
                }
                if (self.painting) |paint| {
                    if (is_gap and signal.flags.mouse_over) {
                        tile_state.gap.mark = switch (paint) {
                            .cross => .cross,
                            .none => .none,
                        };
                    }
                } else if (signal.flags.pressed.get(.left)) {
                    tile_state.gap.mark = switch (tile_state.gap.mark) {
                        .lamp => .none,
                        else => .lamp,
                    };
                }
            }
        }

        var lamp_positions: std.ArrayList(UVec2) = try .initCapacity(self.mem.frame.allocator(), self.board.width * 4);
        // set all tiles to off, and store lamp positions
        {
            var it = self.board.iterator(true);
            while (it.next()) |tile| {
                switch (tile.value.*) {
                    .block => continue,
                    .gap => {
                        tile.value.gap.lighted = false;
                        if (tile.value.gap.mark == .lamp) {
                            try lamp_positions.append(tile.pos);
                        }
                    },
                }
            }
        }

        for (lamp_positions.items) |lamp_pos| {
            for (IVec2.cardinal_directions) |dir| {
                var it = self.board.rayIterator(lamp_pos, dir);
                while (it.next()) |pos| {
                    const tile = self.board.getPtr(pos) catch unreachable;
                    switch (tile.*) {
                        .block => break,
                        .gap => tile.gap.lighted = true,
                    }
                }
            }
        }
    }
    platform.gl.clear(.gray(0.5));

    {
        var it = self.ui.root.iterator_pre();
        while (it.next()) |box_ptr| {
            const box = box_ptr.*;
            if (box.flags.visible) {
                self.canvas.fillRect(camera, box.final_rect, box.background_color);
                if (box.text) |text| {
                    try self.canvas.text_renderers[0].drawLine(
                        platform.gl,
                        camera,
                        .{ .center = box.final_rect.get(.center) },
                        text,
                        box.final_rect.size.y,
                        box.text_color,
                        self.mem.frame.allocator(),
                    );
                }
                self.canvas.fillRect(camera, box.final_rect, FColor.gray(0.5).withAlpha(@max(0, box.hot_t * 0.4 - box.active_t * 0.2)));
            }
        }
    }

    return false;
}

pub const UI = struct {
    pub const MouseButton = enum(usize) {
        left = 0,
        right = 1,
        middle = 2,
        pub const each = std.enums.values(MouseButton);
    };
    pub const Key = enum(u64) {
        none = 0,
        _,

        pub fn fromString(str: []const u8) Key {
            return @enumFromInt(std.hash.Wyhash.hash(0, str));
        }

        pub fn fromFormat(comptime fmt: []const u8, args: anytype) Key {
            var buf: [0x1000]u8 = undefined;
            return fromString(std.fmt.bufPrint(&buf, fmt, args) catch panic("Key fmt was too long", .{}));
        }
    };
    pub const Signal = struct {
        box: *Box,
        scroll: Vec2 = .zero,
        flags: struct {
            /// pressed while hovering
            pressed: std.EnumArray(MouseButton, bool) = .initFill(false),
            /// still holding
            dragging: std.EnumArray(MouseButton, bool) = .initFill(false),
            /// previously pressed, user released mouse, in or out of bounds
            released: std.EnumArray(MouseButton, bool) = .initFill(false),
            /// pressed & released in bounds
            clicked: std.EnumArray(MouseButton, bool) = .initFill(false),
            double_clicked: std.EnumArray(MouseButton, bool) = .initFill(false),
            /// hovering specifically this box
            hovering: bool = false,
            /// mouse is over, but may be occluded
            mouse_over: bool = false,
        } = .{},
    };
    pub const Box = struct {
        // hashmap
        // hash_next: ?*Box,
        // hash_prev: ?*Box,

        // persistent
        hot_t: f32 = 0,
        active_t: f32 = 0,
        disabled_t: f32 = 0,
        first_touched_build_index: usize = undefined,
        last_touched_build_index: usize = undefined,

        // per build equipment
        key: Key,
        flags: Flags,
        // TODO
        tree: struct {
            child_count: usize = 0,
            first: ?*Box = null,
            last: ?*Box = null,
            next: ?*Box = null,
            prev: ?*Box = null,
            parent: ?*Box = null,
        },
        /// x, y
        desired_size: [2]Size,
        layout_axis: Vec2.Coord,
        background_color: FColor,
        text_color: FColor,
        text: ?[]const u8,

        // per build artifacts
        computed_relative_position: Vec2 = undefined,
        computed_size: Vec2 = undefined,
        final_rect: Rect,

        pub const Flags = struct {
            clickable: bool = false,
            disabled: bool = false,
            clip: bool = false,
            scroll: bool = false,
            clamp_scroll_x: bool = false,
            clamp_scroll_y: bool = false,
            dropsite: bool = false,
            visible: bool = false,
            // draw_background: bool = false,
            // draw_border: bool = false,
            // draw_hot_effects: bool = false,
            // draw_active_effects: bool = false,
            // TODO: many
        };

        pub fn addChildLast(self: *Box, child: *Box) void {
            if (self.tree.first == null) {
                assert(self.tree.last == null);
                assert(self.tree.child_count == 0);
                self.tree.first = child;
                self.tree.last = child;
            } else {
                assert(self.tree.last != null);
                child.tree.prev = self.tree.last;
                self.tree.last.?.tree.next = child;
                self.tree.last = child;
            }
            self.tree.child_count += 1;
            child.tree.parent = self;
        }

        pub fn iterator_pre(self: *Box) PreIterator {
            return .{ .box = self };
        }

        pub fn iterator_post(self: *Box) PostIterator {
            return .init(self);
        }

        /// depth-first, from first to last child
        pub const PreIterator = struct {
            box: ?*Box,

            pub fn next(self: *PreIterator) ?*Box {
                if (self.box) |cur| {
                    if (cur.tree.first) |first| {
                        self.box = first;
                    } else if (cur.tree.next) |next_sibling| {
                        self.box = next_sibling;
                    } else if (cur.tree.parent) |parent| {
                        self.box = parent.tree.next;
                    } else {
                        self.box = null;
                    }
                    return cur;
                } else {
                    return null;
                }
            }
        };

        /// leaf to root
        pub const PostIterator = struct {
            box: ?*Box,

            pub fn init(root: *Box) PostIterator {
                return .{ .box = firstLeaf(root) };
            }

            fn firstLeaf(root: *Box) *Box {
                var box = root;
                while (box.tree.first) |b| {
                    box = b;
                }
                return box;
            }

            pub fn next(self: *PostIterator) ?*Box {
                if (self.box) |cur| {
                    if (cur.tree.next) |next_sibling| {
                        self.box = firstLeaf(next_sibling);
                    } else if (cur.tree.parent) |parent| {
                        self.box = parent;
                    } else {
                        self.box = null;
                    }
                    return cur;
                } else {
                    return null;
                }
            }
        };
    };
    pub const State = struct {
        // arena: std.heap.ArenaAllocator,

        build_index: usize = 0,
        // build_arenas: [2]std.heap.ArenaAllocator,

        box_cache: std.AutoHashMap(Key, *Box),
        box_pool: std.heap.MemoryPool(Box),

        // roots: std.BoundedArray(*const Box, 1) = std.BoundedArray(*const Box, 1).init(0) catch unreachable,
        root: *Box = undefined,

        // TODO: FIFO
        parent_stack: std.BoundedArray(*Box, 10) = .{},
        // TODO: join these into a single 'active' effect, and also make it a stack
        active_desired_size: [2]Size = undefined,
        active_layout_axis: Vec2.Coord = undefined,
        active_background_color: FColor = undefined,

        // user interaction state
        hot: Key = .none,
        active: std.EnumArray(MouseButton, Key) = .initFill(.none),
        drop_hot: Key = .none,
        last_press: std.EnumArray(MouseButton, ?struct { pos: Vec2, time: f32, box: Key }) = .initFill(null),
        // press_history: [2]?struct { pos: Vec2, time: f32, box: Key } = .{ null, null },
        dragging_data: ?*const anyopaque = null,
        // drag_start: ?Vec2 = null,
        last_time_mouse_moved: f32 = -std.math.inf(f32),

        // TODO: change to DoublyLinkedList
        // events: std.DoublyLinkedList(Event) = .{},
        events: std.ArrayList(Event),
        mouse: Vec2 = undefined,

        // fn get_build_arena(self: UI.State) std.heap.ArenaAllocator {
        //     return self.build_arenas[@mod(self.build_index, self.build_arenas.len)];
        // }

        // TODO: return non-ptr?
        fn box_from_key(self: State, key: Key) ?*Box {
            if (key == .none) return null;
            return self.box_cache.get(key);
        }

        fn push_parent(self: *State, box: *Box) !void {
            try self.parent_stack.append(box);
        }

        fn pop_parent(self: *State) void {
            _ = self.parent_stack.pop();
        }

        // TODO: should be a stack, probably
        fn setActiveDesiredSizeAndLayoutAxis(self: *State, size: [2]Size, axis: Vec2.Coord) void {
            self.active_desired_size = size;
            self.active_layout_axis = axis;
        }

        fn setActiveBackgroundColor(self: *State, background_color: FColor) void {
            self.active_background_color = background_color;
        }

        fn active_parent(self: State) ?*Box {
            if (self.parent_stack.len == 0) return null;
            return self.parent_stack.get(self.parent_stack.len - 1);
        }

        fn build_box(self: *State, key: Key, flags: Box.Flags) !*Box {
            // const box, const is_new = blk: {
            //     self.box_from_key(key);
            //     self.box_from_key(key) orelse try self.box_pool.create();
            // };
            const box, const existed = blk: {
                if (self.box_from_key(key)) |box| {
                    break :blk .{ box, true };
                } else {
                    const box = try self.box_pool.create();
                    try self.box_cache.put(key, box);
                    box.first_touched_build_index = self.build_index;
                    box.key = key;
                    break :blk .{ box, false };
                }
            };
            box.last_touched_build_index = self.build_index;
            box.flags = flags;
            box.tree = .{};
            box.desired_size = self.active_desired_size;
            box.layout_axis = self.active_layout_axis;
            if (!existed) {
                box.background_color = self.active_background_color;
            } else {
                box.background_color = .lerp(box.background_color, self.active_background_color, 0.2);
            }
            box.text = null;
            box.text_color = undefined;
            box.computed_size = .zero;
            box.computed_relative_position = .zero;
            if (self.active_parent()) |parent| {
                parent.addChildLast(box);
            }
            return box;
        }

        pub fn init(gpa: std.mem.Allocator) State {
            return .{
                // .arena = .init(gpa),
                .box_pool = .init(gpa),
                .box_cache = .init(gpa),
                .events = .init(gpa),
            };
        }

        pub fn deinit(state: *State) void {
            state.box_pool.deinit();
            state.box_cache.deinit();
        }

        pub fn beginBuild(self: *UI.State, mouse: Mouse, time: f32, root: Rect, default_axis: Vec2.Coord) !void {
            self.events.clearRetainingCapacity();
            if (!mouse.cur.position.equals(mouse.prev.position)) {
                try self.events.append(.{
                    .kind = .move,
                    .time = time,
                    .pos = mouse.cur.position,
                    .key = undefined,
                });
            }
            inline for (.{ .left, .right, .middle }) |button| {
                if (mouse.wasPressed(button)) {
                    try self.events.append(.{
                        .kind = .press,
                        .time = time,
                        .pos = mouse.cur.position,
                        .key = button,
                    });
                }
                if (mouse.wasReleased(button)) {
                    try self.events.append(.{
                        .kind = .release,
                        .time = time,
                        .pos = mouse.cur.position,
                        .key = button,
                    });
                }
            }
            if (mouse.cur.scrolled != .none) {
                try self.events.append(.{
                    .kind = .scroll,
                    .time = time,
                    .pos = mouse.cur.position,
                    .key = undefined,
                    // TODO: proper units
                    .scroll = .new(0, switch (mouse.cur.scrolled) {
                        .up => 1.0,
                        .down => -1.0,
                        .none => unreachable,
                    }),
                });
            }
            self.mouse = mouse.cur.position;

            // reset per-build ui state
            self.parent_stack.clear();

            // self.roots.clear();
            // self.roots.append()
            //   {
            //     Rng2F32 window_rect = os_client_rect_from_window(window);
            //     Vec2F32 window_rect_size = dim_2f32(window_rect);
            //     ui_set_next_fixed_width(window_rect_size.x);
            //     ui_set_next_fixed_height(window_rect_size.y);
            //     ui_set_next_child_layout_axis(Axis2_X);
            //     UI_Box *root = ui_build_box_from_stringf(0, "###%I64x", window.u64[0]);
            //     ui_push_parent(root);
            //     ui_state->root = root;
            //   }
            self.setActiveDesiredSizeAndLayoutAxis(.{
                .{ .kind = .world, .value = root.size.x },
                .{ .kind = .world, .value = root.size.y },
            }, default_axis);
            self.root = try self.build_box(.fromString("root"), .{});
            try self.push_parent(self.root);
            // a bit hacky...
            self.root.computed_relative_position = root.top_left;
            self.root.final_rect = root;
            errdefer comptime unreachable;

            // if no active box, reset hot
            if (blk: for (self.active.values) |key| {
                if (key != .none) break :blk false;
            } else true) {
                self.hot = .none;
            }

            self.drop_hot = .none;

            // reset active if it's disabled or deleted
            for (MouseButton.each) |button| {
                if (self.box_from_key(self.active.get(button))) |box| {
                    if (box.flags.disabled) {
                        self.active.set(button, .none);
                    }
                } else {
                    self.active.set(button, .none);
                }
            }
        }

        pub fn endBuild(self: *UI.State, delta_seconds: f32) void {
            // TODO: remove untouched boxes?

            // layout
            {
                // step 1: standalone sizes
                {
                    var it = Box.iterator_pre(self.root);
                    while (it.next()) |box| {
                        inline for (box.desired_size, Vec2.coords) |size, coord| {
                            switch (size.kind) {
                                .world => box.computed_size.setInPlace(coord, size.value),
                                else => {},
                            }
                        }
                    }
                }

                // step 2: upwards dependent sizes
                {
                    var it = Box.iterator_pre(self.root);
                    while (it.next()) |box| {
                        inline for (box.desired_size, Vec2.coords, 0..) |size, coord, k| {
                            switch (size.kind) {
                                .percent_of_parent => {
                                    // find parent that has computed a fixed size
                                    const fixed_parent_size: f32 = blk: {
                                        var cur = box.tree.parent;
                                        while (cur) |b| {
                                            if (switch (b.desired_size[k].kind) {
                                                .world, .percent_of_parent => true,
                                                .children_sum => false,
                                            }) {
                                                break :blk b.computed_size.get(coord);
                                            }
                                            cur = b.tree.parent;
                                        } else unreachable;
                                    };
                                    box.computed_size.setInPlace(
                                        coord,
                                        fixed_parent_size * size.value,
                                    );
                                },
                                else => {},
                            }
                        }
                    }
                }

                // step 3: downwards dependent sizes
                {
                    var it = Box.iterator_post(self.root);
                    while (it.next()) |box| {
                        inline for (box.desired_size, Vec2.coords) |size, coord| {
                            switch (size.kind) {
                                .children_sum => {
                                    var res: f32 = 0;
                                    var child = box.tree.first;
                                    while (child) |b| {
                                        if (coord == box.layout_axis) {
                                            res += b.computed_size.get(coord);
                                        } else {
                                            res = @max(res, b.computed_size.get(coord));
                                        }
                                        child = b.tree.next;
                                    }
                                    box.computed_size.setInPlace(coord, res);
                                },
                                else => {},
                            }
                        }
                    }
                }

                // TODO step 4: solve violations

                // step 5: compute final rects
                {
                    var it = Box.iterator_pre(self.root);
                    while (it.next()) |box| {
                        inline for (Vec2.coords) |coord| {
                            var pos: f32 = 0.0;
                            var maybe_child = box.tree.first;
                            while (maybe_child) |child| {
                                child.computed_relative_position.setInPlace(coord, pos);
                                child.final_rect.top_left.setInPlace(
                                    coord,
                                    box.final_rect.top_left.get(coord) + pos,
                                );
                                child.final_rect.size.setInPlace(
                                    coord,
                                    child.computed_size.get(coord),
                                );

                                if (coord == box.layout_axis) {
                                    pos += child.computed_size.get(coord);
                                }

                                maybe_child = child.tree.next;
                            }
                        }
                    }
                }
            }

            {
                var it = self.box_cache.valueIterator();
                while (it.next()) |box_ptr| {
                    const box = box_ptr.*;
                    const is_hot = box.key == self.hot or
                        box.key == self.drop_hot;
                    const is_active = box.key == self.active.get(.left);
                    const is_disabled = box.flags.disabled;

                    const rate = 0.2;
                    math.lerp_towards(&box.hot_t, math.tof32(is_hot), rate, delta_seconds);
                    math.lerp_towards(&box.active_t, math.tof32(is_active), rate, delta_seconds);
                    math.lerp_towards(&box.disabled_t, math.tof32(is_disabled), rate, delta_seconds);
                }
            }
            // TODO: anims (box->hot_t += ..., positions)
            // TODO: set cursor
            self.build_index += 1;
            // _ = self.get_build_arena().reset(.retain_capacity);
        }
    };
    pub const Size = struct {
        kind: Kind,
        value: f32,
        // TODO
        // strictness: f32,

        pub const Kind = enum { world, percent_of_parent, children_sum };
    };
    const double_click_time = 0.3;
    const Event = struct {
        kind: enum { press, release, scroll, move },
        pos: Vec2,
        time: f32,
        key: MouseButton,
        scroll: Vec2 = undefined,
    };
    pub fn signal_from_box(state: *State, box: *UI.Box) Signal {
        var signal: Signal = .{ .box = box };

        const rect: Rect = blk: {
            var rect = box.final_rect;
            var b = box.tree.parent;
            while (b != null) : (b = b.?.tree.parent) {
                if (b.?.flags.clip) {
                    rect = rect.intersect(b.?.final_rect) orelse .{ .top_left = .zero, .size = .zero };
                }
            }
            break :blk rect;
        };

        // var view_scrolled = false;

        // var next_event = state.events.first;
        // while (next_event) |event_node| {
        //     defer next_event = event_node.next;
        //     const event = event_node.data;

        var event_index: usize = 0;
        while (event_index < state.events.items.len) : (event_index += 1) {
            const event = state.events.items[event_index];
            const mouse_button: MouseButton = event.key;

            var taken = false;

            // mouse presses in box
            const in_bounds = rect.contains(event.pos);
            if (box.flags.clickable and
                event.kind == .press and
                in_bounds)
            {
                state.hot = box.key;
                state.active.set(mouse_button, box.key);
                signal.flags.pressed.set(mouse_button, true);
                if (state.last_press.get(mouse_button)) |last_press| {
                    if (last_press.box == box.key and
                        double_click_time > (event.time - last_press.time))
                    {
                        signal.flags.double_clicked.set(mouse_button, true);
                    }
                }
                state.last_press.set(mouse_button, .{
                    .box = box.key,
                    .time = event.time,
                    .pos = event.pos,
                });
                taken = true;
            }

            // mouse releases in active box
            if (box.flags.clickable and
                event.kind == .release and
                state.active.get(mouse_button) == box.key and
                in_bounds)
            {
                state.active.set(mouse_button, .none);
                signal.flags.released.set(mouse_button, true);
                signal.flags.clicked.set(mouse_button, true);
                taken = true;
            }

            // mouse releases outside active box
            if (box.flags.clickable and
                event.kind == .release and
                state.active.get(mouse_button) == box.key and
                !in_bounds)
            {
                state.hot = .none;
                state.active.set(mouse_button, .none);
                signal.flags.released.set(mouse_button, true);
                taken = true;
            }

            // TODO
            // // scrolling
            // if (box.flags.scroll and
            //     event.kind == .scroll and
            //     in_bounds)
            // {
            //     box.view_offset_target.addSelf(event.scroll);
            //     view_scrolled = true;
            //     taken = true;
            // }

            if (taken) {
                // state.events.remove(event_node);
                _ = state.events.orderedRemove(event_index);
            }
        }

        // TODO
        // // clamp scroll
        // if (view_scrolled and box.flags.clamp_scroll_x) {
        //     math.clampPtr(
        //         &box.view_offset_target.x,
        //         0,
        //         box.rect.size.x,
        //     );
        // }
        // if (view_scrolled and box.flags.clamp_scroll_y) {
        //     math.clampPtr(
        //         &box.view_offset_target.y,
        //         0,
        //         box.rect.size.y,
        //     );
        // }

        // dragging
        if (box.flags.clickable) {
            for (MouseButton.each) |button| {
                if (state.active.get(button) == box.key or signal.flags.pressed.get(button)) {
                    signal.flags.dragging.set(button, true);
                }
            }
        }

        if (rect.contains(state.mouse)) {
            signal.flags.mouse_over = true;
        }

        // mouse is over this box's rect, no other hot key? -> set hot key, mark hovering
        if (box.flags.clickable and
            rect.contains(state.mouse) and
            (state.hot == .none or state.hot == box.key) and
            (state.active.get(.left) == .none or state.active.get(.left) == box.key) and
            (state.active.get(.right) == .none or state.active.get(.right) == box.key) and
            (state.active.get(.middle) == .none or state.active.get(.middle) == box.key))
        {
            state.hot = box.key;
            signal.flags.hovering = true;
        }

        // set drop
        if (box.flags.dropsite and
            rect.contains(state.mouse) and
            (state.drop_hot == .none or state.drop_hot == box.key))
        {
            state.drop_hot = box.key;
        }

        // remove drop
        if (box.flags.dropsite and
            !rect.contains(state.mouse) and
            state.drop_hot == box.key)
        {
            state.drop_hot = .none;
        }

        return signal;
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
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const IVec2 = math.IVec2;
const funk = kommon.funktional;
const maybeMirror = math.maybeMirror;
const Noise = kommon.Noise;
pub const Mouse = kommon.input.Mouse;
pub const Keyboard = kommon.input.Keyboard;
pub const KeyboardButton = kommon.input.KeyboardButton;
pub const PrecomputedShape = kommon.renderer.PrecomputedShape;
pub const RenderableInfo = kommon.renderer.RenderableInfo;
pub const Gl = kommon.Gl;
pub const Canvas = kommon.Canvas;
pub const TextRenderer = Canvas.TextRenderer;
