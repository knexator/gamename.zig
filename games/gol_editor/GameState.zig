pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "gol_editor",
        .author = "knexator",
        .desired_aspect_ratio = 4.0 / 3.0,
    },
    .sounds = .{},
    .loops = .{},
    .preloaded_images = .{
        // TODO: don't require this here
        .arial_atlas = "fonts/Arial.png",
    },
};
pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const COLORS = struct {
    bg: FColor = .gray(0.5),
}{};

const CellState = enum {
    black,
    gray,
    white,

    pub fn color(self: CellState) FColor {
        return switch (self) {
            .black => .fromHex("#444444"),
            .gray => .fromHex("#999999"),
            .white => .white,
        };
    }
};

const CellType = enum {
    empty,
    @"+",
    @"*",
    o,
    @"=",
    @"-",

    pub const all: [6]CellType = .{ .@"+", .@"*", .o, .@"=", .@"-", .empty };

    pub fn text(self: CellType) []const u8 {
        return switch (self) {
            .empty => "",
            inline else => |x| @tagName(x),
        };
    }
};

const Toolbar = struct {
    active_state: CellState = .white,
    active_type: CellType = .@"+",
    selected_rect_inner_corner1: IVec2 = .zero,
    selected_rect_inner_corner2: IVec2 = .zero,
    rect_tool_state: union(enum) {
        none,
        selecting,
        moving: kommon.Grid2D(BoardState.FullCell),
    } = .none,

    active_tool: Tool,
    /// only defined when active tool is catalogue
    prev_tool: Tool = undefined,

    const Tool = enum { paint_state, paint_type, selecting_rect, catalogue, panning };

    pub fn selectedRect(self: Toolbar) math.IBounds {
        return math.IBounds.empty.bounding(self.selected_rect_inner_corner1).bounding(self.selected_rect_inner_corner2);
    }

    pub fn moveSelectedRect(self: *Toolbar, delta: IVec2) void {
        self.selected_rect_inner_corner1.addInPlace(delta);
        self.selected_rect_inner_corner2.addInPlace(delta);
    }
};

const BoardState = struct {
    cells_states: std.AutoArrayHashMap(IVec2, CellState),
    cells_types: std.AutoArrayHashMap(IVec2, CellType),

    const FullCell = struct { cell_state: CellState, cell_type: CellType };

    pub fn init(dst: *BoardState, gpa: std.mem.Allocator) !void {
        dst.cells_states = .init(gpa);
        dst.cells_types = .init(gpa);

        try dst.cells_states.put(.new(1, 1), .gray);
        try dst.cells_states.put(.new(1, 2), .white);
        try dst.cells_types.put(.new(1, 3), .@"+");
        try dst.cells_types.put(.new(1, 4), .@"*");
        try dst.cells_types.put(.new(1, 5), .o);
        try dst.cells_types.put(.new(1, 6), .@"=");
        try dst.cells_types.put(.new(1, 7), .@"-");
    }

    fn clone(self: BoardState) !BoardState {
        return .{
            .cells_states = try self.cells_states.clone(),
            .cells_types = try self.cells_types.clone(),
        };
    }

    pub fn cloneAndGetPtr(self: BoardState, pool: *std.heap.MemoryPool(BoardState)) !*BoardState {
        const res = try pool.create();
        res.* = try self.clone();
        return res;
    }

    pub fn equals(self: BoardState, other: BoardState) bool {
        return self.isSubsetOf(other) and other.isSubsetOf(self);
    }

    fn isSubsetOf(self: BoardState, other: BoardState) bool {
        {
            var it = self.cells_states.iterator();
            while (it.next()) |kv| {
                if (other.stateAt(kv.key_ptr.*) != kv.value_ptr.*) return false;
            }
        }
        {
            var it = self.cells_types.iterator();
            while (it.next()) |kv| {
                if (other.typeAt(kv.key_ptr.*) != kv.value_ptr.*) return false;
            }
        }
        return true;
    }

    pub fn boundingRect(self: BoardState) math.IBounds {
        var result: math.IBounds = .empty;

        var it_states = self.cells_states.iterator();
        while (it_states.next()) |kv| {
            if (kv.value_ptr.* == .black) continue;
            result.plusTile(kv.key_ptr.*);
        }

        var it_types = self.cells_types.iterator();
        while (it_types.next()) |kv| {
            if (kv.value_ptr.* == .empty) continue;
            result.plusTile(kv.key_ptr.*);
        }

        return result;
    }

    pub fn stateAt(self: BoardState, pos: IVec2) CellState {
        return self.cells_states.get(pos) orelse .black;
    }

    pub fn typeAt(self: BoardState, pos: IVec2) CellType {
        return self.cells_types.get(pos) orelse .empty;
    }

    pub fn fullAt(self: BoardState, pos: IVec2) FullCell {
        return .{
            .cell_state = self.stateAt(pos),
            .cell_type = self.typeAt(pos),
        };
    }

    pub fn toText(self: BoardState, out: std.io.AnyWriter) !void {
        const bounds = self.boundingRect();
        try out.writeAll("V1\n");
        for (0..bounds.inner_size.y) |dj| {
            for (0..bounds.inner_size.x) |di| {
                const p = bounds.top_left.addUnsigned(.new(di, dj));
                try out.writeAll(switch (self.stateAt(p)) {
                    .black => " ",
                    .gray => ".",
                    .white => ":",
                });
                try out.writeAll(switch (self.typeAt(p)) {
                    .empty => ".",
                    else => |t| t.text(),
                });
            }
            try out.writeAll("\n");
        }
    }

    pub fn fromText(dst: *BoardState, scratch: std.mem.Allocator, in: std.io.AnyReader) !void {
        dst.cells_states.clearRetainingCapacity();
        dst.cells_types.clearRetainingCapacity();
        const contents = try in.readAllAlloc(scratch, std.math.maxInt(usize));
        assert(std.mem.startsWith(u8, contents, "V1\n"));
        const raw_ascii = try kommon.Grid2D([2]u8).fromAsciiWide(2, scratch, std.mem.trimRight(u8, contents["V1\n".len..], &std.ascii.whitespace));
        var it = raw_ascii.iteratorSigned();
        while (it.next()) |p| {
            const t = raw_ascii.atSigned(p);
            const cell_state: CellState = switch (t[0]) {
                ' ' => .black,
                '.' => .gray,
                ':' => .white,
                else => return error.BadText,
            };
            const cell_type: CellType = switch (t[1]) {
                '.' => .empty,
                '+' => .@"+",
                '*' => .@"*",
                'o' => .o,
                '=' => .@"=",
                '-' => .@"-",
                else => return error.BadText,
            };
            if (cell_state != .black) try dst.cells_states.put(p, cell_state);
            if (cell_type != .empty) try dst.cells_types.put(p, cell_type);
        }
    }

    pub fn getSubrect(self: BoardState, alloc: std.mem.Allocator, bounds: math.IBounds) !kommon.Grid2D(FullCell) {
        const result: kommon.Grid2D(FullCell) = try .initUndefined(alloc, bounds.inner_size);
        var it = result.iteratorSigned();
        while (it.next()) |p| {
            result.setSigned(p, self.fullAt(p.add(bounds.top_left)));
        }
        return result;
    }

    pub fn clearSubrect(self: *BoardState, bounds: math.IBounds) !void {
        // TODO: could be optimized, by removing instead of adding elements
        var it = kommon.itertools.inIBounds(bounds);
        while (it.next()) |p| {
            try self.cells_states.put(p, .black);
            try self.cells_types.put(p, .empty);
        }
    }

    pub fn setSubrect(self: *BoardState, values: kommon.Grid2D(FullCell), bounds: math.IBounds) !void {
        assert(bounds.inner_size.equals(values.size));
        var it = values.iteratorSigned();
        while (it.next()) |p| {
            const cell = values.atSigned(p);
            try self.cells_states.put(p.add(bounds.top_left), cell.cell_state);
            try self.cells_types.put(p.add(bounds.top_left), cell.cell_type);
        }
    }
};

toolbar: Toolbar = .{ .active_tool = .paint_state },
catalogue_index: usize = 0,
catalogue_view_offset: f32 = 0,
saved_states: std.ArrayList(*const BoardState),
camera: Rect = .{ .top_left = .new(-10, -5), .size = Vec2.new(4, 3).scale(8) },
board: *BoardState,
pool: std.heap.MemoryPool(BoardState),

random: std.Random.DefaultPrng,
canvas: Canvas,
mem: Mem,
smooth: kommon.LazyState,
// lazy_state: LocalDecisions,

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
    random_seed: u64,
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.smooth = .init(dst.mem.forever.allocator());
    // dst.lazy_state = .init(gpa);
    dst.random = .init(random_seed);

    dst.pool = .init(gpa);
    dst.board = try dst.pool.create();
    try dst.board.init(gpa);
    dst.saved_states = .init(gpa);
    try dst.saved_states.append(try dst.board.cloneAndGetPtr(&dst.pool));
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    // self.mem.deinit();
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    _ = self;
}

fn onCatalogueOpened(self: *GameState) !void {
    try self.saveState();
}

fn saveState(self: *GameState) !void {
    for (self.saved_states.items, 0..) |state, k| {
        if (self.board.equals(state.*)) {
            self.catalogue_index = k;
            break;
        }
    } else {
        self.catalogue_index += 1;
        try self.saved_states.insert(self.catalogue_index, try self.board.cloneAndGetPtr(&self.pool));
    }
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    _ = self.mem.frame.reset(.retain_capacity);
    _ = self.mem.scratch.reset(.retain_capacity);
    self.smooth.last_delta_seconds = platform.delta_seconds;
    // self.lazy_state.frameStart();

    if (platform.userUploadedFile()) |reader| {
        defer platform.forgetUserUploadedFile();
        try self.board.fromText(self.mem.frame.allocator(), reader);
    }

    const camera = self.camera.withAspectRatio(platform.aspect_ratio, .grow, .top_left);
    const mouse = platform.getMouse(camera);
    const cell_under_mouse = mouse.cur.position.toInt(isize);

    const ui_cam: Rect = .{ .top_left = .zero, .size = Vec2.new(platform.aspect_ratio, 1).scale(10) };
    const ui_mouse = platform.getMouse(ui_cam);
    var ui_buttons: std.ArrayList(struct {
        pos: Rect,
        color: ?FColor,
        text: ?[]const u8,
        radio_selected: bool,
    }) = .init(self.mem.frame.allocator());
    var catalogue_buttons: std.ArrayList(struct {
        pos: Rect,
        radio_selected: bool,
        board: *const BoardState,
        hot: bool,
    }) = .init(self.mem.frame.allocator());

    var mouse_over_ui = false;

    // paint cell states
    for ([3]CellState{ .black, .gray, .white }, 0..) |c, k| {
        const button: Rect = (Rect{ .top_left = .new(tof32(k), 0), .size = .one }).plusMargin(-0.1);
        try ui_buttons.append(.{
            .pos = button,
            .color = c.color(),
            .text = null,
            .radio_selected = self.toolbar.active_tool == .paint_state and (c == self.toolbar.active_state),
        });
        if (button.contains(ui_mouse.cur.position)) {
            mouse_over_ui = true;
            if (mouse.wasPressed(.left)) {
                self.toolbar.active_state = c;
                self.toolbar.active_tool = .paint_state;
            }
        }
    }

    // paint cell types
    for (CellType.all, 0..) |t, k| {
        const button: Rect = (Rect{ .top_left = .new(tof32(@mod(k, 3)), tof32(1 + @divFloor(k, 3))), .size = .one }).plusMargin(-0.1);
        try ui_buttons.append(.{
            .pos = button,
            .color = null,
            .text = t.text(),
            .radio_selected = self.toolbar.active_tool == .paint_type and (t == self.toolbar.active_type),
        });
        if (button.contains(ui_mouse.cur.position)) {
            mouse_over_ui = true;
            if (mouse.wasPressed(.left)) {
                self.toolbar.active_type = t;
                self.toolbar.active_tool = .paint_type;
            }
        }
    }

    // rect select mode
    if (true) {
        const button: Rect = (Rect{ .top_left = .new(4, 1), .size = .one }).plusMargin(-0.1);
        try ui_buttons.append(.{
            .pos = button,
            .color = null,
            .text = "[]",
            .radio_selected = self.toolbar.active_tool == .selecting_rect,
        });
        if (button.contains(ui_mouse.cur.position)) {
            mouse_over_ui = true;
            if (mouse.wasPressed(.left)) {
                self.toolbar.active_tool = .selecting_rect;
            }
        }
    }

    // panning mode
    if (true) {
        const button: Rect = (Rect{ .top_left = .new(5, 1), .size = .one }).plusMargin(-0.1);
        try ui_buttons.append(.{
            .pos = button,
            .color = null,
            .text = "g",
            .radio_selected = self.toolbar.active_tool == .panning,
        });
        if (button.contains(ui_mouse.cur.position)) {
            mouse_over_ui = true;
            if (mouse.wasPressed(.left)) {
                self.toolbar.active_tool = .panning;
            }
        }
    }

    // toggle catalogue
    if (true) {
        const button: Rect = (Rect{ .top_left = .new(4, 0), .size = .one }).plusMargin(-0.1);
        const hot = button.contains(ui_mouse.cur.position);
        try ui_buttons.append(.{
            .pos = button,
            .color = null,
            .text = "C",
            .radio_selected = hot or self.toolbar.active_tool == .catalogue,
        });
        if (hot) {
            mouse_over_ui = true;
            if (mouse.wasPressed(.left)) {
                if (self.toolbar.active_tool == .catalogue) {
                    self.toolbar.active_tool = self.toolbar.prev_tool;
                    self.toolbar.prev_tool = undefined;
                } else {
                    self.toolbar.prev_tool = self.toolbar.active_tool;
                    self.toolbar.active_tool = .catalogue;
                    try self.onCatalogueOpened();
                }
            }
        }
    }

    if (platform.keyboard.wasPressed(.KeyC)) {
        if (self.toolbar.active_tool == .catalogue) {
            self.toolbar.active_tool = self.toolbar.prev_tool;
            self.toolbar.prev_tool = undefined;
        } else {
            self.toolbar.prev_tool = self.toolbar.active_tool;
            self.toolbar.active_tool = .catalogue;
            try self.onCatalogueOpened();
        }
    }

    // save button
    if (true) {
        const button: Rect = (Rect{ .top_left = .new(5, 0), .size = .one }).plusMargin(-0.1);
        const hot = button.contains(ui_mouse.cur.position);
        try ui_buttons.append(.{
            .pos = button,
            .color = null,
            .text = "X",
            .radio_selected = hot,
        });
        if (hot) {
            mouse_over_ui = true;
            if (mouse.wasPressed(.left)) {
                try self.saveState();
            }
        }
    }

    if (platform.keyboard.wasPressed(.KeyX)) {
        try self.saveState();
    }

    // save to file button
    if (true) {
        const button: Rect = (Rect.fromPivotAndSize(ui_cam.get(.top_right), Rect.MeasureKind.top_right.asPivot(), .new(2, 1))).plusMargin(-0.1);
        const hot = button.contains(ui_mouse.cur.position);
        try ui_buttons.append(.{
            .pos = button,
            .color = null,
            .text = "save",
            .radio_selected = hot,
        });
        if (hot) {
            mouse_over_ui = true;
            if (mouse.wasPressed(.left)) {
                var buf: std.ArrayList(u8) = .init(self.mem.frame.allocator());
                defer buf.deinit();
                try self.board.toText(buf.writer().any());
                platform.downloadAsFile("gol_level.txt", buf.items);
            }
        }
    }

    // load from file button
    if (true) {
        const button: Rect = (Rect.fromPivotAndSize(ui_cam.get(.top_right), Rect.MeasureKind.top_right.asPivot(), .new(2, 1))).move(.e2).plusMargin(-0.1);
        const hot = button.contains(ui_mouse.cur.position);
        try ui_buttons.append(.{
            .pos = button,
            .color = null,
            .text = "load",
            .radio_selected = hot,
        });
        if (hot) {
            mouse_over_ui = true;
            if (mouse.wasPressed(.left)) {
                platform.askUserForFile();
            }
        }
    }

    var visible_board: *const BoardState = self.board;

    // tool specific UI
    if (self.toolbar.active_tool == .catalogue) {
        math.lerp_towards(&self.catalogue_view_offset, tof32(self.catalogue_index), 0.2, platform.delta_seconds);
        for (self.saved_states.items, 0..) |board, k| {
            const button: Rect = ui_cam.with2(.size, .one, .bottom_left).move(.new(tof32(k) - self.catalogue_view_offset + ui_cam.size.x / 2 - 0.5, 0)).plusMargin(-0.1);
            const hot = button.contains(ui_mouse.cur.position);
            try catalogue_buttons.append(.{
                .pos = button,
                .board = board,
                .radio_selected = k == self.catalogue_index,
                .hot = hot,
            });
            if (hot) {
                mouse_over_ui = true;
                visible_board = board;
                if (mouse.wasPressed(.left)) {
                    self.catalogue_index = k;
                    self.pool.destroy(self.board);
                    self.board = try board.cloneAndGetPtr(&self.pool);
                }
                if (mouse.wasPressed(.right) and self.saved_states.items.len > 1) {
                    _ = self.saved_states.orderedRemove(k);
                    break;
                }
            }
        }
    }

    // tool specific mouse scroll
    if (mouse.cur.scrolled != .none) {
        switch (self.toolbar.active_tool) {
            .catalogue => {
                self.catalogue_index = kommon.moveIndex(self.catalogue_index, -mouse.cur.scrolled.toInt(), self.saved_states.items.len, .clamp);
                self.pool.destroy(self.board);
                self.board = try self.saved_states.items[self.catalogue_index].cloneAndGetPtr(&self.pool);
            },
            else => {
                self.camera = self.camera.zoom(mouse.cur.position, switch (mouse.cur.scrolled) {
                    .none => unreachable,
                    .down => 1.1,
                    .up => 0.9,
                });
            },
        }
    }

    // tool mouse interactions
    if (!mouse_over_ui) {
        platform.setCursor(.default);
        switch (self.toolbar.active_tool) {
            .paint_state => {
                if (mouse.cur.isDown(.left)) {
                    try self.board.cells_states.put(cell_under_mouse, self.toolbar.active_state);
                }
                if (mouse.wasPressed(.right)) {
                    self.toolbar.active_state = self.board.cells_states.get(cell_under_mouse) orelse .black;
                }
            },
            .paint_type => {
                if (mouse.cur.isDown(.left)) {
                    try self.board.cells_types.put(cell_under_mouse, self.toolbar.active_type);
                }
                if (mouse.wasPressed(.right)) {
                    self.toolbar.active_type = self.board.cells_types.get(cell_under_mouse) orelse .empty;
                }
            },
            .selecting_rect => {
                const inside_rect = self.toolbar.selectedRect().contains(cell_under_mouse);
                switch (self.toolbar.rect_tool_state) {
                    .none => {
                        if (inside_rect) {
                            platform.setCursor(.could_grab);
                            if (mouse.wasPressed(.left)) {
                                self.toolbar.rect_tool_state = .{ .moving = try self.board.getSubrect(
                                    self.mem.gpa,
                                    self.toolbar.selectedRect(),
                                ) };
                                try self.board.clearSubrect(self.toolbar.selectedRect());
                            }
                        } else {
                            if (mouse.wasPressed(.left)) {
                                self.toolbar.rect_tool_state = .selecting;
                                self.toolbar.selected_rect_inner_corner1 = cell_under_mouse;
                                self.toolbar.selected_rect_inner_corner2 = cell_under_mouse;
                            }
                        }
                    },
                    .selecting => {
                        if (mouse.cur.isDown(.left)) {
                            self.toolbar.selected_rect_inner_corner2 = cell_under_mouse;
                        } else {
                            self.toolbar.rect_tool_state = .none;
                        }
                    },
                    .moving => {
                        platform.setCursor(.grabbing);
                        if (mouse.cur.isDown(.left)) {
                            platform.setCursor(.grabbing);
                            const prev_cell_under_mouse = mouse.prev.position.toInt(isize);
                            const mouse_cell_delta = cell_under_mouse.sub(prev_cell_under_mouse);
                            self.toolbar.moveSelectedRect(mouse_cell_delta);
                        } else {
                            try self.board.setSubrect(self.toolbar.rect_tool_state.moving, self.toolbar.selectedRect());
                            self.toolbar.rect_tool_state.moving.deinit(self.mem.gpa);
                            self.toolbar.rect_tool_state = .none;
                        }
                    },
                }
            },
            .catalogue => {
                if (mouse.wasPressed(.left) or mouse.wasPressed(.right)) {
                    self.toolbar.active_tool = self.toolbar.prev_tool;
                    self.toolbar.prev_tool = undefined;
                }
            },
            .panning => {
                if (mouse.cur.isDown(.left)) {
                    platform.setCursor(.grabbing);
                    self.camera = self.camera.move(mouse.deltaPos().neg());
                } else {
                    platform.setCursor(.could_grab);
                }
            },
        }
    } else {
        platform.setCursor(.pointer);
    }

    // tool keyboard interactions
    switch (self.toolbar.active_tool) {
        .paint_state => {
            for (&[_]CellState{ .black, .gray, .white }, 0..) |t, k| {
                if (platform.keyboard.wasPressed(.digit(k + 1))) {
                    self.toolbar.active_state = t;
                }
            }
            if (platform.keyboard.wasPressed(.Backquote) or platform.keyboard.wasPressed(.Space)) {
                self.toolbar.active_tool = .paint_type;
            }
        },
        .paint_type => {
            for (&CellType.all, 0..) |t, k| {
                if (platform.keyboard.wasPressed(.digit(k + 1))) {
                    self.toolbar.active_type = t;
                }
            }
            if (platform.keyboard.wasPressed(.Backquote) or platform.keyboard.wasPressed(.Space)) {
                self.toolbar.active_tool = .paint_state;
            }
        },
        .selecting_rect, .catalogue, .panning => {},
    }

    // always available: move camera
    for (Vec2.cardinal_directions, &[4][]const KeyboardButton{
        &.{ .KeyD, .ArrowRight },
        &.{ .KeyS, .ArrowDown },
        &.{ .KeyA, .ArrowLeft },
        &.{ .KeyW, .ArrowUp },
    }) |d, ks| {
        for (ks) |k| {
            if (platform.keyboard.cur.isDown(k)) {
                self.camera = self.camera.move(d.scale(platform.delta_seconds * 0.8 * self.camera.size.y));
            }
        }
    }

    // always available: drag view
    if (mouse.cur.isDown(.middle) or platform.keyboard.cur.isDown(.KeyG)) {
        platform.setCursor(.grabbing);
        self.camera = self.camera.move(mouse.deltaPos().neg());
    }

    platform.gl.clear(CellState.black.color());

    if (true) {
        var cell_bgs: std.ArrayList(Canvas.InstancedShapeInfo) = .init(self.mem.frame.allocator());
        defer self.canvas.fillShapesInstanced(camera, self.canvas.DEFAULT_SHAPES.square, cell_bgs.items);

        var it = visible_board.cells_states.iterator();
        while (it.next()) |kv| {
            if (kv.value_ptr.* == .black) continue;
            try cell_bgs.append(.{
                .point = .{ .pos = kv.key_ptr.*.tof32() },
                .color = kv.value_ptr.*.color(),
            });
        }
    }

    if (true) {
        var cell_texts = self.canvas.textBatch(0);
        defer cell_texts.draw(camera);

        var it = visible_board.cells_types.iterator();
        while (it.next()) |kv| {
            if (kv.value_ptr.* == .empty) continue;
            try cell_texts.addText(
                kv.value_ptr.*.text(),
                .centeredAt(kv.key_ptr.*.tof32().add(.half)),
                1.0,
                if ((visible_board.cells_states.get(kv.key_ptr.*) orelse .black) == .black)
                    .white
                else
                    .black,
            );
        }
    }

    if (std.meta.activeTag(self.toolbar.rect_tool_state) == .moving) {
        const values = self.toolbar.rect_tool_state.moving;

        if (true) {
            var cell_bgs: std.ArrayList(Canvas.InstancedShapeInfo) = .init(self.mem.frame.allocator());
            var cell_texts = self.canvas.textBatch(0);

            defer cell_texts.draw(camera);
            defer self.canvas.fillShapesInstanced(camera, self.canvas.DEFAULT_SHAPES.square, cell_bgs.items);

            var it = values.iteratorSigned();
            while (it.next()) |p| {
                const cell = values.atSigned(p);
                try cell_bgs.append(.{
                    .point = .{ .pos = p.add(self.toolbar.selectedRect().top_left).tof32() },
                    .color = cell.cell_state.color(),
                });

                if (cell.cell_type != .empty) {
                    try cell_texts.addText(
                        cell.cell_type.text(),
                        .centeredAt(p.add(self.toolbar.selectedRect().top_left).tof32().add(.half)),
                        1.0,
                        if (cell.cell_state == .black) .white else .black,
                    );
                }
            }
        }
    }

    // board lines
    if (true) {
        var segments: std.ArrayList(Canvas.Segment) = .init(self.mem.frame.allocator());
        {
            var x = @floor(camera.top_left.x);
            while (x <= camera.top_left.x + camera.size.x) : (x += 1) {
                try segments.append(.{ .a = .new(x, camera.top_left.y), .b = .new(x, camera.top_left.y + camera.size.y), .color = .black });
            }
        }
        {
            var y = @floor(camera.top_left.y);
            while (y <= camera.top_left.y + camera.size.y) : (y += 1) {
                try segments.append(.{ .a = .new(camera.top_left.x, y), .b = .new(camera.top_left.x + camera.size.x, y), .color = .black });
            }
        }

        self.canvas.instancedSegments(camera, segments.items, 0.05);
    }

    if (self.toolbar.active_tool == .selecting_rect) {
        const rect = self.toolbar.selectedRect().asRect();
        self.canvas.strokeRect(camera, rect, 0.1, .cyan);
    }

    {
        var ui_texts = self.canvas.textBatch(0);
        defer ui_texts.draw(ui_cam);
        for (ui_buttons.items) |button| {
            self.canvas.fillRect(ui_cam, button.pos, if (button.radio_selected) .cyan else .red);
            self.canvas.fillRect(ui_cam, button.pos.plusMargin(-0.05), button.color orelse CellState.gray.color());
            if (button.text) |text| try ui_texts.addText(text, .centeredAt(button.pos.getCenter()), 0.75, .black);
        }
    }

    {
        for (catalogue_buttons.items) |button| {
            self.canvas.fillRect(ui_cam, button.pos, if (button.radio_selected or button.hot) .cyan else .red);
            self.canvas.fillRect(ui_cam, button.pos.plusMargin(-0.05), CellState.black.color());

            const bounds = button.board.boundingRect().asRect().withAspectRatio(1.0, .grow, .center);
            const offset = bounds.top_left;
            const scale = 1.0 / bounds.size.y;

            {
                var cell_bgs: std.ArrayList(Canvas.InstancedShapeInfo) = .init(self.mem.frame.allocator());
                var it = button.board.cells_states.iterator();
                while (it.next()) |kv| {
                    if (kv.value_ptr.* == .black) continue;

                    try cell_bgs.append(.{
                        .point = .{ .pos = button.pos.applyToLocalPosition(kv.key_ptr.*.tof32().sub(offset).scale(scale)), .scale = scale * button.pos.size.y },
                        .color = kv.value_ptr.*.color(),
                    });
                }
                self.canvas.fillShapesInstanced(ui_cam, self.canvas.DEFAULT_SHAPES.square, cell_bgs.items);
            }

            {
                var cell_texts = self.canvas.textBatch(0);
                var it = button.board.cells_types.iterator();
                while (it.next()) |kv| {
                    if (kv.value_ptr.* == .empty) continue;

                    try cell_texts.addText(
                        kv.value_ptr.*.text(),
                        .centeredAt(button.pos.applyToLocalPosition(kv.key_ptr.*.tof32().sub(offset).add(.half).scale(scale))),
                        scale * button.pos.size.y,
                        if ((button.board.cells_states.get(kv.key_ptr.*) orelse .black) == .black)
                            .white
                        else
                            .black,
                    );
                }
                cell_texts.draw(ui_cam);
            }
        }
    }

    return false;
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
