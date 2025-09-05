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

var COLORS: struct {
    cell: struct {
        black: FColor = .fromHex("#444444"),
        gray: FColor = .fromHex("#999999"),
        white: FColor = .white,
    } = .{},
    cell_text: struct {
        on_black: FColor = .white,
        on_gray: FColor = .black,
        on_white: FColor = .black,
    } = .{},
    grid: FColor = .fromHex("#545454"),
    rect_selection_border: FColor = .cyan,
} = .{};

var CONFIG: struct {
    grid_width: f32 = 0.05,
} = .{};

const CellState = enum {
    black,
    gray,
    white,

    pub fn color(self: CellState) FColor {
        return switch (self) {
            .black => COLORS.cell.black,
            .gray => COLORS.cell.gray,
            .white => COLORS.cell.white,
        };
    }

    pub fn textColorOver(self: CellState) FColor {
        return switch (self) {
            .black => COLORS.cell_text.on_black,
            .gray => COLORS.cell_text.on_gray,
            .white => COLORS.cell_text.on_white,
        };
    }
};

const CellType = enum {
    empty,
    @"+",
    @"-",
    @"~",
    @"*",
    o,
    @"=",
    @"?",

    pub const all: [@typeInfo(CellType).@"enum".fields.len]CellType = .{ .@"+", .@"-", .@"~", .@"*", .o, .@"=", .@"?", .empty };

    pub fn text(self: CellType) []const u8 {
        return switch (self) {
            .empty => "",
            inline else => |x| @tagName(x),
        };
    }

    /// this gets added to the cell's vertical pos
    pub fn verticalCorrection(self: CellType) f32 {
        return switch (self) {
            .@"*" => 0.3,
            .o => -0.125,
            .@"-" => -0.15,
            else => 0,
        };
    }

    /// text size gets mutliplied by this
    pub fn sizeCorrection(self: CellType) f32 {
        return switch (self) {
            .@"*" => 1.3,
            .@"-" => 1.5,
            .@"?", .@"~" => 1.0,
            else => 1.3,
        };
    }
};

const Toolbar = struct {
    painting: bool = false,
    active_state: CellState = .white,
    active_type: CellType = .@"+",
    selected_rect_inner_corner1: IVec2 = .zero,
    selected_rect_inner_corner2: IVec2 = .zero,
    rect_tool_state: union(enum) {
        none,
        selecting,
        moving: kommon.Grid2D(BoardState.FullCell),
    } = .none,
    rect_tool_moving_include_blank: bool = true,
    catalogue_index: usize = 0,
    catalogue_view_offset: f32 = 0,
    zoom: Zoom = .@"15x15",

    active_tool: Tool,
    /// only defined when active tool is catalogue
    prev_tool: Tool = undefined,

    const Tool = enum { paint_state, paint_type, rect, catalogue, panning };

    const Zoom = enum {
        @"15x15",
        bounds_lit,
        bounds_all,
        free,

        pub fn levels(is_editor: bool) []const Zoom {
            if (is_editor) {
                return &.{ .@"15x15", .bounds_lit, .bounds_all, .free };
            } else {
                return &.{ .@"15x15", .bounds_lit, .bounds_all };
            }
        }

        pub fn text(self: Zoom) []const u8 {
            return switch (self) {
                .@"15x15" => "z",
                .bounds_lit => "Z1",
                .bounds_all => "Z2",
                .free => "free",
            };
        }

        pub fn allowsMove(self: Zoom) bool {
            return switch (self) {
                .@"15x15", .free => true,
                .bounds_lit, .bounds_all => false,
            };
        }
    };

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
    }

    pub fn deinit(self: *BoardState) void {
        self.cells_states.deinit();
        self.cells_types.deinit();
    }

    fn clone(self: BoardState) !BoardState {
        return .{
            .cells_states = try self.cells_states.clone(),
            .cells_types = try self.cells_types.clone(),
        };
    }

    pub fn cloneAndGetPtr(self: BoardState, pool_boardstate: *std.heap.MemoryPool(BoardState)) !*BoardState {
        const res = try pool_boardstate.create();
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

    pub fn userBounds(self: BoardState) math.IBounds {
        return self.boundingRectV2(.{ .lit = false, .elements = true }).plusMargin(20);
    }

    pub fn boundingRectV2(self: BoardState, include: struct {
        lit: bool = false,
        elements: bool = false,
    }) math.IBounds {
        var result: math.IBounds = .empty;

        if (include.lit) {
            var it_states = self.cells_states.iterator();
            while (it_states.next()) |kv| {
                if (kv.value_ptr.* == .black) continue;
                result.plusTile(kv.key_ptr.*);
            }
        }

        if (include.elements) {
            var it_types = self.cells_types.iterator();
            while (it_types.next()) |kv| {
                if (kv.value_ptr.* == .empty) continue;
                result.plusTile(kv.key_ptr.*);
            }
        }

        return result;
    }

    pub fn boundingRect(self: BoardState) math.IBounds {
        return self.boundingRectV2(.{ .lit = true, .elements = true });
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

    pub fn fromText(dst: *BoardState, scratch: std.mem.Allocator, text: []const u8) !void {
        dst.cells_states.clearRetainingCapacity();
        dst.cells_types.clearRetainingCapacity();
        const contents = std.mem.trim(u8, text, &std.ascii.whitespace);
        assert(std.mem.startsWith(u8, contents, "V1\n"));
        const raw_ascii = try kommon.Grid2D([2]u8).fromAsciiWide(2, scratch, std.mem.trimRight(u8, contents["V1\n".len..], &std.ascii.whitespace));
        defer raw_ascii.deinit(scratch);
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

    pub fn setSubrect(self: *BoardState, values: kommon.Grid2D(FullCell), bounds: math.IBounds, include_blanks: bool) !void {
        assert(bounds.inner_size.equals(values.size));
        var it = values.iteratorSigned();
        while (it.next()) |p| {
            const cell = values.atSigned(p);
            if (include_blanks or cell.cell_state != .black) try self.cells_states.put(p.add(bounds.top_left), cell.cell_state);
            if (include_blanks or cell.cell_type != .empty) try self.cells_types.put(p.add(bounds.top_left), cell.cell_type);
        }
    }
};

const LevelState = struct {
    toolbar: Toolbar = .{ .active_tool = .paint_state },
    camera: Rect = .{ .top_left = .both(-6), .size = .both(15) },
    saved_states: std.ArrayList(*const BoardState),
    board: *BoardState,
    initial_board: *const BoardState,

    fn setCurrentStateAsInitial(self: *LevelState, pool_boardstate: *std.heap.MemoryPool(BoardState)) !void {
        pool_boardstate.destroy(@constCast(self.initial_board));
        self.initial_board = try self.board.cloneAndGetPtr(pool_boardstate);
        try self.saveState(pool_boardstate);
    }

    fn saveState(self: *LevelState, pool_boardstate: *std.heap.MemoryPool(BoardState)) !void {
        for (self.saved_states.items, 0..) |state, k| {
            if (self.board.equals(state.*)) {
                self.toolbar.catalogue_index = k;
                break;
            }
        } else {
            self.toolbar.catalogue_index += 1;
            try self.saved_states.insert(
                self.toolbar.catalogue_index,
                try self.board.cloneAndGetPtr(pool_boardstate),
            );
        }
    }

    fn onCatalogueOpened(self: *LevelState, pool_boardstate: *std.heap.MemoryPool(BoardState)) !void {
        try self.saveState(pool_boardstate);
    }

    fn loadState(self: *LevelState, board: *const BoardState, pool_boardstate: *std.heap.MemoryPool(BoardState)) !void {
        self.board.deinit();
        pool_boardstate.destroy(self.board);
        self.board = try board.cloneAndGetPtr(pool_boardstate);
    }
};

all_levels: std.ArrayList(*LevelState),
cur_level: ?*LevelState = null,
is_editor: bool = false,
levelselect_view_offset: f32 = 0,

pool_boardstate: std.heap.MemoryPool(BoardState),
pool_levelstate: std.heap.MemoryPool(LevelState),

usual: kommon.Usual,

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
    random_seed: u64,
    tweakable: type,
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

    dst.pool_levelstate = .init(gpa);
    dst.pool_boardstate = .init(gpa);

    dst.all_levels = .init(gpa);
    try dst.addEmptyLevel();

    tweakable.fcolor("Off", &COLORS.cell.black);
    tweakable.fcolor("Dim", &COLORS.cell.gray);
    tweakable.fcolor("Bright", &COLORS.cell.white);

    tweakable.fcolor("grid", &COLORS.grid);
    tweakable.fcolor("rect border", &COLORS.rect_selection_border);

    tweakable.fcolor("text over Off", &COLORS.cell_text.on_black);
    tweakable.fcolor("text over Dim", &COLORS.cell_text.on_gray);
    tweakable.fcolor("text over Bright", &COLORS.cell_text.on_white);

    tweakable.float("grid width", &CONFIG.grid_width, 0.0, 0.2);
}

test "tokenize two spaces" {
    var it = std.mem.tokenizeSequence(u8, "a b c  d e f   g h i  ", "  ");
    try std.testing.expectEqualStrings("a b c", std.mem.trim(u8, it.next().?, " "));
    try std.testing.expectEqualStrings("d e f", std.mem.trim(u8, it.next().?, " "));
    try std.testing.expectEqualStrings("g h i", std.mem.trim(u8, it.next().?, " "));
    try std.testing.expect(it.next() == null);
}

fn saveWorld(world: []const *LevelState, out: std.io.AnyWriter) !void {
    try out.writeAll("W1\n");

    for (world) |l| {
        try l.board.toText(out);
        try out.writeAll("\n");
    }
}

fn loadWorld(self: *GameState, in: std.io.AnyReader) !void {
    // deinit all levels
    for (self.all_levels.items) |l| {
        l.board.deinit();
        for (l.saved_states.items) |b| {
            @constCast(b).deinit();
        }
        l.saved_states.deinit();
    }
    _ = self.pool_boardstate.reset(.retain_capacity);
    _ = self.pool_levelstate.reset(.retain_capacity);
    self.all_levels.clearRetainingCapacity();

    try nextReadIs(in, "W1\n");

    const contents = try in.readAllAlloc(self.usual.mem.scratch.allocator(), std.math.maxInt(usize));
    var it = std.mem.tokenizeSequence(u8, contents, "\n\n");
    while (it.next()) |c| {
        try self.addLevelFromText(c);
    }
}

fn addLevelFromText(self: *GameState, text: []const u8) !void {
    const level = try self.pool_levelstate.create();
    level.* = .{
        .initial_board = blk: {
            const res = try self.pool_boardstate.create();
            try res.init(self.usual.mem.gpa);
            try res.fromText(self.usual.mem.scratch.allocator(), text);
            break :blk res;
        },
        .board = undefined,
        .saved_states = .init(self.usual.mem.gpa),
    };
    level.board = try level.initial_board.cloneAndGetPtr(&self.pool_boardstate);
    try level.saved_states.append(try level.board.cloneAndGetPtr(&self.pool_boardstate));
    try self.all_levels.append(level);
}

fn addEmptyLevel(self: *GameState) !void {
    const level = try self.pool_levelstate.create();
    level.* = .{
        .initial_board = blk: {
            const res = try self.pool_boardstate.create();
            try res.init(self.usual.mem.gpa);

            try res.cells_states.put(.new(0, 1), .gray);
            try res.cells_states.put(.new(0, 2), .white);
            try res.cells_types.put(.new(1, 0), .@"+");
            try res.cells_types.put(.new(1, 1), .@"*");
            try res.cells_types.put(.new(1, 2), .o);
            try res.cells_types.put(.new(2, 0), .@"=");
            try res.cells_types.put(.new(2, 1), .@"-");

            break :blk res;
        },
        .board = undefined,
        .saved_states = .init(self.usual.mem.gpa),
    };
    level.board = try level.initial_board.cloneAndGetPtr(&self.pool_boardstate);
    try level.saved_states.append(try level.board.cloneAndGetPtr(&self.pool_boardstate));
    try self.all_levels.append(level);
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
    _ = self;
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);
    const mem = &self.usual.mem;
    const canvas = &self.usual.canvas;

    if (platform.userUploadedFile()) |reader| {
        defer platform.forgetUserUploadedFile();

        if (self.cur_level) |cur_level| {
            const text = try reader.readAllAlloc(mem.frame.allocator(), std.math.maxInt(usize));
            defer mem.frame.allocator().free(text);
            try cur_level.board.fromText(mem.frame.allocator(), text);
        } else {
            try self.loadWorld(reader);
        }
    }

    if (self.cur_level) |cur_level| {
        var target_camera = switch (cur_level.toolbar.zoom) {
            .free => cur_level.camera,
            .@"15x15" => cur_level.camera.withSize(.both(15), .center),
            .bounds_all => cur_level.board.boundingRect().plusMargin(1).asRect(),
            .bounds_lit => cur_level.board.boundingRectV2(.{
                .lit = true,
                .elements = false,
            }).plusMargin(1).asRect(),
        };
        target_camera = target_camera.setMinSize(.both(15), .center);
        target_camera = target_camera.withAspectRatio(1.0, .grow, .center);
        if (!self.is_editor) {
            target_camera = target_camera.setMaxSize(cur_level.board.userBounds().plusMargin(1).asRect().size, .center);
            target_camera = target_camera.moveToBeInsideRect(cur_level.board.userBounds().plusMargin(1).asRect());
        }
        cur_level.camera = .lerp(cur_level.camera, target_camera, 0.2);
    }

    const camera: Rect = if (self.cur_level) |cur_level|
        cur_level.camera.withAspectRatio(platform.aspect_ratio, .grow, .center)
    else
        (Rect{ .top_left = .new(0, self.levelselect_view_offset), .size = .new(4, 3) })
            .withAspectRatio(platform.aspect_ratio, .grow, .top_left);
    const mouse = platform.getMouse(camera);

    const ui_cam: Rect = if (self.cur_level == null) camera else (Rect{ .top_left = .zero, .size = .new(16, 12) })
        .withAspectRatio(platform.aspect_ratio, .grow, .center);
    const ui_mouse = platform.getMouse(ui_cam);
    var ui_buttons: std.ArrayList(struct {
        pos: Rect,
        color: ?FColor,
        text: ?[]const u8,
        text_scale: ?f32 = null,
        radio_selected: bool,
    }) = .init(mem.frame.allocator());
    var catalogue_buttons: std.ArrayList(struct {
        pos: Rect,
        radio_selected: bool,
        board: *const BoardState,
        hot: bool,
    }) = .init(mem.frame.allocator());

    // always available: toggle editor
    if (platform.keyboard.wasPressed(.KeyE)) {
        self.is_editor = !self.is_editor;
    }

    var mouse_over_ui = false;

    platform.gl.clear(CellState.black.color());
    if (self.cur_level) |cur_level| {
        var toolbar = &cur_level.toolbar;
        const cell_under_mouse = mouse.cur.position.toInt(isize);

        // paint cell states
        for ([3]CellState{ .black, .gray, .white }, 0..) |c, k| {
            const button: Rect = (Rect{ .top_left = Vec2.zero.add(.new(0, tof32(k))), .size = .one }).plusMargin(-0.1);
            try ui_buttons.append(.{
                .pos = button,
                .color = c.color(),
                .text = null,
                .radio_selected = toolbar.active_tool == .paint_state and (c == toolbar.active_state),
            });
            if (button.contains(ui_mouse.cur.position)) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    toolbar.active_state = c;
                    toolbar.active_tool = .paint_state;
                    toolbar.painting = false;
                }
            }
        }

        // paint cell types
        if (self.is_editor) {
            for (CellType.all, 0..) |t, k| {
                const button: Rect = (Rect{ .top_left = .new(tof32(@mod(k, 2)), tof32(5 + @mod(@divFloor(k, 2), 4))), .size = .one }).plusMargin(-0.1);
                try ui_buttons.append(.{
                    .pos = button,
                    .color = null,
                    .text = t.text(),
                    .radio_selected = toolbar.active_tool == .paint_type and (t == toolbar.active_type),
                });
                if (button.contains(ui_mouse.cur.position)) {
                    mouse_over_ui = true;
                    if (mouse.wasPressed(.left)) {
                        toolbar.active_type = t;
                        toolbar.active_tool = .paint_type;
                        toolbar.painting = false;
                    }
                }
            }
        }

        // rect select/move mode
        if (self.is_editor) {
            const button: Rect = (Rect{ .top_left = .new(0, 3), .size = .one }).plusMargin(-0.1);
            try ui_buttons.append(.{
                .pos = button,
                .color = null,
                .text = "[]",
                .radio_selected = toolbar.active_tool == .rect,
            });
            if (button.contains(ui_mouse.cur.position)) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    toolbar.active_tool = .rect;
                }
            }
        }

        // panning mode
        if (self.is_editor or toolbar.zoom.allowsMove()) {
            const button: Rect = (Rect{ .top_left = .new(1, 3), .size = .one }).plusMargin(-0.1);
            try ui_buttons.append(.{
                .pos = button,
                .color = null,
                .text = "g",
                .radio_selected = toolbar.active_tool == .panning,
            });
            if (button.contains(ui_mouse.cur.position)) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    toolbar.active_tool = .panning;
                }
            }
        }

        const bottom_left_buttons: [4]Rect = blk: {
            const base = (Rect.fromPivotAndSize(ui_cam.get(.bottom_left), Rect.MeasureKind.bottom_left.asPivot(), .one));
            break :blk .{
                base,
                base.move(.new(1, 0)),
                base.move(.new(0, -1)),
                base.move(.new(1, -1)),
            };
        };

        if (true) {
            for (Toolbar.Zoom.levels(self.is_editor), 0..) |t, k| {
                const button: Rect = bottom_left_buttons[k].plusMargin(-0.1);
                const hot = button.contains(ui_mouse.cur.position);
                try ui_buttons.append(.{
                    .pos = button,
                    .color = null,
                    .text = t.text(),
                    .text_scale = 0.75,
                    .radio_selected = hot or t == toolbar.zoom,
                });
                if (hot) {
                    mouse_over_ui = true;
                    if (mouse.wasPressed(.left)) {
                        toolbar.zoom = t;
                    }
                }
            }
        }

        const top_right_button: Rect = (Rect.fromPivotAndSize(ui_cam.get(.top_right), Rect.MeasureKind.top_right.asPivot(), .one));

        // overwrite initial state
        if (self.is_editor) {
            // const button: Rect = (Rect{ .top_left = .new(5, 1), .size = .new(3, 1) }).plusMargin(-0.1);

            const button: Rect = top_right_button.move(.new(0, 1)).withSize(.new(2, 1), .top_right).plusMargin(-0.1);
            const hot = button.contains(ui_mouse.cur.position);
            try ui_buttons.append(.{
                .pos = button,
                .color = null,
                .text = "set initial",
                .text_scale = 0.6,
                .radio_selected = hot,
            });
            if (hot) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    try cur_level.setCurrentStateAsInitial(&self.pool_boardstate);
                }
            }
        }

        // toggle catalogue
        if (true) {
            const button: Rect = top_right_button.plusMargin(-0.1);
            const hot = button.contains(ui_mouse.cur.position);
            try ui_buttons.append(.{
                .pos = button,
                .color = null,
                .text = "C",
                .radio_selected = hot or toolbar.active_tool == .catalogue,
            });
            if (hot) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    if (toolbar.active_tool == .catalogue) {
                        toolbar.active_tool = toolbar.prev_tool;
                        toolbar.prev_tool = undefined;
                    } else {
                        toolbar.prev_tool = toolbar.active_tool;
                        toolbar.active_tool = .catalogue;
                        try cur_level.onCatalogueOpened(&self.pool_boardstate);
                    }
                }
            }
        }

        if (platform.keyboard.wasPressed(.KeyC)) {
            if (toolbar.active_tool == .catalogue) {
                toolbar.active_tool = toolbar.prev_tool;
                toolbar.prev_tool = undefined;
            } else {
                toolbar.prev_tool = toolbar.active_tool;
                toolbar.active_tool = .catalogue;
                try cur_level.onCatalogueOpened(&self.pool_boardstate);
            }
        }

        // save to catalogue button
        if (true) {
            const button: Rect = top_right_button.move(.new(-1, 0)).plusMargin(-0.1);
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
                    try cur_level.saveState(&self.pool_boardstate);
                }
            }
        }

        if (platform.keyboard.wasPressed(.KeyX)) {
            try cur_level.saveState(&self.pool_boardstate);
        }

        const bottom_right_buttons: [4]Rect = blk: {
            const base = (Rect.fromPivotAndSize(ui_cam.get(.bottom_right), Rect.MeasureKind.bottom_right.asPivot(), .new(2, 1)));
            break :blk .{
                base.move(.new(0, -2)),
                base.move(.new(0, -3)),
                base,
                base.move(.new(0, -1)),
            };
        };

        // save to file button
        if (true) {
            const button: Rect = bottom_right_buttons[0].plusMargin(-0.1);
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
                    var buf: std.ArrayList(u8) = .init(mem.frame.allocator());
                    defer buf.deinit();
                    try cur_level.board.toText(buf.writer().any());
                    platform.downloadAsFile("gol_level.txt", buf.items);
                }
            }
        }

        // load from file button
        if (true) {
            const button: Rect = bottom_right_buttons[1].plusMargin(-0.1);
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

        // exit to menu button
        if (true) {
            const button: Rect = bottom_right_buttons[2].plusMargin(-0.1);
            const hot = button.contains(ui_mouse.cur.position);
            try ui_buttons.append(.{
                .pos = button,
                .color = null,
                .text = "back",
                .radio_selected = hot,
            });
            if (hot) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    self.cur_level = null;
                }
            }
        }

        // reset level state button
        if (true) {
            const button: Rect = bottom_right_buttons[3].plusMargin(-0.1);
            const hot = button.contains(ui_mouse.cur.position);
            try ui_buttons.append(.{
                .pos = button,
                .color = null,
                .text = "reset",
                .radio_selected = hot,
            });
            if (hot) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    try cur_level.loadState(cur_level.initial_board, &self.pool_boardstate);
                }
            }
        }

        var ghost_board: ?*const BoardState = null;

        // tool specific UI
        if (toolbar.active_tool == .catalogue) {
            math.lerp_towards(&toolbar.catalogue_view_offset, tof32(toolbar.catalogue_index), 0.2, platform.delta_seconds);
            for (cur_level.saved_states.items, 0..) |board, k| {
                const button: Rect = ui_cam.with2(.size, .both(2), .bottom_left)
                    .move(Vec2.new(2 * (tof32(k) - toolbar.catalogue_view_offset) + ui_cam.size.x / 2 - 0.5, 0)).plusMargin(-0.1);
                const hot = button.contains(ui_mouse.cur.position);
                try catalogue_buttons.append(.{
                    .pos = button,
                    .board = board,
                    .radio_selected = k == toolbar.catalogue_index,
                    .hot = hot,
                });
                if (hot) {
                    mouse_over_ui = true;
                    ghost_board = board;
                    if (mouse.wasPressed(.left)) {
                        toolbar.catalogue_index = k;
                        try cur_level.loadState(board, &self.pool_boardstate);
                    }
                    if (mouse.wasPressed(.right) and cur_level.saved_states.items.len > 1) {
                        const old_board: *BoardState = @constCast(cur_level.saved_states.orderedRemove(k));
                        old_board.deinit();
                        ghost_board = null;
                        _ = catalogue_buttons.pop();
                        if (k == toolbar.catalogue_index) {
                            toolbar.catalogue_index = @min(toolbar.catalogue_index, cur_level.saved_states.items.len - 1);
                            try cur_level.loadState(cur_level.saved_states.items[toolbar.catalogue_index], &self.pool_boardstate);
                        }
                        break;
                    }
                }
            }
        }

        // tool specific mouse scroll
        if (mouse.cur.scrolled != .none) {
            switch (toolbar.active_tool) {
                .catalogue => {
                    toolbar.catalogue_index = kommon.moveIndex(toolbar.catalogue_index, -mouse.cur.scrolled.toInt(), cur_level.saved_states.items.len, .clamp);
                    try cur_level.loadState(cur_level.saved_states.items[toolbar.catalogue_index], &self.pool_boardstate);
                },
                else => if (toolbar.zoom == .free) {
                    cur_level.camera = cur_level.camera.zoom(mouse.cur.position, switch (mouse.cur.scrolled) {
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
            switch (toolbar.active_tool) {
                .paint_state => {
                    if (toolbar.painting) {
                        if (!mouse.cur.isDown(.left)) {
                            toolbar.painting = false;
                        } else {
                            if (self.is_editor or cur_level.board.userBounds().contains(cell_under_mouse)) {
                                try cur_level.board.cells_states.put(cell_under_mouse, toolbar.active_state);
                            }
                        }
                    } else {
                        if (mouse.wasPressed(.left)) {
                            toolbar.painting = true;
                        }
                        if (mouse.wasPressed(.right)) {
                            toolbar.active_state = cur_level.board.cells_states.get(cell_under_mouse) orelse .black;
                        }
                    }
                },
                .paint_type => {
                    if (toolbar.painting) {
                        if (!mouse.cur.isDown(.left)) {
                            toolbar.painting = false;
                        } else {
                            try cur_level.board.cells_types.put(cell_under_mouse, toolbar.active_type);
                        }
                    } else {
                        if (mouse.wasPressed(.left)) {
                            toolbar.painting = true;
                        }
                        if (mouse.wasPressed(.right)) {
                            toolbar.active_type = cur_level.board.cells_types.get(cell_under_mouse) orelse .empty;
                        }
                    }
                },
                .rect => {
                    const inside_rect = toolbar.selectedRect().contains(cell_under_mouse);
                    switch (toolbar.rect_tool_state) {
                        .none => {
                            if (inside_rect) {
                                platform.setCursor(.could_grab);
                                if (mouse.wasPressed(.left)) {
                                    toolbar.rect_tool_state = .{ .moving = try cur_level.board.getSubrect(
                                        mem.gpa,
                                        toolbar.selectedRect(),
                                    ) };
                                    try cur_level.board.clearSubrect(toolbar.selectedRect());
                                } else if (mouse.wasPressed(.right)) {
                                    toolbar.rect_tool_state = .{ .moving = try cur_level.board.getSubrect(
                                        mem.gpa,
                                        toolbar.selectedRect(),
                                    ) };
                                }
                            } else {
                                if (mouse.wasPressed(.left)) {
                                    toolbar.rect_tool_state = .selecting;
                                    toolbar.selected_rect_inner_corner1 = cell_under_mouse;
                                    toolbar.selected_rect_inner_corner2 = cell_under_mouse;
                                }
                            }
                        },
                        .selecting => {
                            if (mouse.cur.isDown(.left)) {
                                toolbar.selected_rect_inner_corner2 = cell_under_mouse;
                            } else {
                                toolbar.rect_tool_state = .none;
                            }
                        },
                        .moving => {
                            platform.setCursor(.grabbing);
                            toolbar.rect_tool_moving_include_blank = platform.keyboard.cur.isShiftDown();
                            if (mouse.cur.isDown(.left) or mouse.cur.isDown(.right)) {
                                platform.setCursor(.grabbing);
                                const prev_cell_under_mouse = mouse.prev.position.toInt(isize);
                                const mouse_cell_delta = cell_under_mouse.sub(prev_cell_under_mouse);
                                toolbar.moveSelectedRect(mouse_cell_delta);
                            } else {
                                try cur_level.board.setSubrect(toolbar.rect_tool_state.moving, toolbar.selectedRect(), toolbar.rect_tool_moving_include_blank);
                                toolbar.rect_tool_state.moving.deinit(mem.gpa);
                                toolbar.rect_tool_state = .none;
                            }
                        },
                    }
                },
                .catalogue => {
                    if (mouse.wasPressed(.left) or mouse.wasPressed(.right)) {
                        toolbar.active_tool = toolbar.prev_tool;
                        toolbar.prev_tool = undefined;
                    }
                },
                .panning => {
                    if (mouse.cur.isDown(.left)) {
                        platform.setCursor(.grabbing);
                        cur_level.camera = cur_level.camera.move(mouse.deltaPos().neg());
                    } else {
                        platform.setCursor(.could_grab);
                    }
                },
            }
        } else {
            platform.setCursor(.pointer);
        }

        // tool keyboard interactions
        switch (toolbar.active_tool) {
            .paint_state => {
                for (&[_]CellState{ .black, .gray, .white }, 0..) |t, k| {
                    if (platform.keyboard.wasPressed(.digit(k + 1))) {
                        toolbar.active_state = t;
                    }
                }
                if (platform.keyboard.wasPressed(.Backquote) or platform.keyboard.wasPressed(.Space)) {
                    toolbar.active_tool = .paint_type;
                }
            },
            .paint_type => {
                for (&CellType.all, 0..) |t, k| {
                    if (platform.keyboard.wasPressed(.digit(k + 1))) {
                        toolbar.active_type = t;
                    }
                }
                if (platform.keyboard.wasPressed(.Backquote) or platform.keyboard.wasPressed(.Space)) {
                    toolbar.active_tool = .paint_state;
                }
            },
            .rect => switch (toolbar.rect_tool_state) {
                else => {},
                .moving => |*r| {
                    if (platform.keyboard.wasPressed(.KeyV)) {
                        r.mirrorVertically();
                    }
                    if (platform.keyboard.wasPressed(.KeyH)) {
                        r.mirrorHorizontally();
                    }
                    if (platform.keyboard.wasPressed(.KeyQ)) {
                        const new_r = try r.rotatedPositive(mem.gpa);
                        r.deinit(mem.gpa);
                        r.* = new_r;
                        toolbar.selected_rect_inner_corner1 = cell_under_mouse.add(toolbar.selected_rect_inner_corner1.sub(cell_under_mouse).rotateOnce());
                        toolbar.selected_rect_inner_corner2 = cell_under_mouse.add(toolbar.selected_rect_inner_corner2.sub(cell_under_mouse).rotateOnce());
                    }
                },
            },
            .catalogue, .panning => {},
        }

        // always available: move camera
        if (toolbar.zoom.allowsMove()) {
            for (Vec2.cardinal_directions, &[4][]const KeyboardButton{
                &.{ .KeyD, .ArrowRight },
                &.{ .KeyS, .ArrowDown },
                &.{ .KeyA, .ArrowLeft },
                &.{ .KeyW, .ArrowUp },
            }) |d, ks| {
                for (ks) |k| {
                    if (platform.keyboard.cur.isDown(k)) {
                        cur_level.camera = cur_level.camera.move(d.scale(platform.delta_seconds * 0.8 * cur_level.camera.size.y));
                    }
                }
            }
        }

        // always available: drag view
        if (toolbar.zoom.allowsMove()) {
            if (mouse.cur.isDown(.middle) or platform.keyboard.cur.isDown(.KeyG)) {
                platform.setCursor(.grabbing);
                cur_level.camera = cur_level.camera.move(mouse.deltaPos().neg());
            }
        }

        // always available: exit to level select
        if (platform.keyboard.wasPressed(.Escape)) {
            self.cur_level = null;
        }

        // always available: reset level state
        if (platform.keyboard.wasPressed(.KeyR)) {
            try cur_level.loadState(cur_level.initial_board, &self.pool_boardstate);
        }

        //////////////
        // draw

        const visible_board = ghost_board orelse cur_level.board;

        if (true) {
            var cell_bgs: std.ArrayList(Canvas.InstancedShapeInfo) = .init(mem.frame.allocator());
            defer canvas.fillShapesInstanced(camera, canvas.DEFAULT_SHAPES.square, cell_bgs.items);

            const cam_bounds: math.IBounds = .fromRect(cur_level.camera.plusMargin(1.1));
            var it = visible_board.cells_states.iterator();
            while (it.next()) |kv| {
                if (kv.value_ptr.* == .black) continue;
                if (!cam_bounds.contains(kv.key_ptr.*)) continue;
                try cell_bgs.append(.{
                    .point = .{ .pos = kv.key_ptr.*.tof32() },
                    .color = kv.value_ptr.*.color(),
                });
            }
        }

        if (toolbar.zoom != .bounds_lit) {
            var cell_texts = canvas.textBatch(0);
            defer cell_texts.draw(camera);

            const cam_bounds: math.IBounds = .fromRect(cur_level.camera.plusMargin(1.1));
            var it = visible_board.cells_types.iterator();
            while (it.next()) |kv| {
                if (kv.value_ptr.* == .empty) continue;
                if (!cam_bounds.contains(kv.key_ptr.*)) continue;
                try cell_texts.addText(
                    kv.value_ptr.*.text(),
                    .centeredAt(kv.key_ptr.*.tof32().add(.half).addY(kv.value_ptr.verticalCorrection())),
                    kv.value_ptr.sizeCorrection(),
                    @as(CellState, visible_board.cells_states.get(kv.key_ptr.*) orelse .black).textColorOver(),
                );
            }
        }

        if (std.meta.activeTag(toolbar.rect_tool_state) == .moving) {
            const values = toolbar.rect_tool_state.moving;

            var cell_bgs: std.ArrayList(Canvas.InstancedShapeInfo) = .init(mem.frame.allocator());
            var cell_texts = canvas.textBatch(0);

            defer cell_texts.draw(camera);
            defer canvas.fillShapesInstanced(camera, canvas.DEFAULT_SHAPES.square, cell_bgs.items);

            var it = values.iteratorSigned();
            while (it.next()) |p| {
                const cell = values.atSigned(p);
                const pos = p.add(toolbar.selectedRect().top_left);

                const visible_state = if (!toolbar.rect_tool_moving_include_blank and cell.cell_state == .black)
                    cur_level.board.stateAt(pos)
                else
                    cell.cell_state;

                const visible_type = if (!toolbar.rect_tool_moving_include_blank and cell.cell_type == .empty)
                    cur_level.board.typeAt(pos)
                else
                    cell.cell_type;

                try cell_bgs.append(.{
                    .point = .{ .pos = pos.tof32() },
                    .color = visible_state.color(),
                });

                if (visible_type != .empty) {
                    try cell_texts.addText(
                        visible_type.text(),
                        .centeredAt(pos.tof32().add(.half).addY(visible_type.verticalCorrection())),
                        visible_type.sizeCorrection(),
                        visible_state.textColorOver(),
                    );
                }
            }
        }

        // board lines
        if (true) {
            var segments: std.ArrayList(Canvas.Segment) = .init(mem.frame.allocator());
            {
                var x = @floor(camera.top_left.x);
                while (x <= camera.top_left.x + camera.size.x) : (x += 1) {
                    try segments.append(.{ .a = .new(x, camera.top_left.y), .b = .new(x, camera.top_left.y + camera.size.y), .color = COLORS.grid });
                }
            }
            {
                var y = @floor(camera.top_left.y);
                while (y <= camera.top_left.y + camera.size.y) : (y += 1) {
                    try segments.append(.{ .a = .new(camera.top_left.x, y), .b = .new(camera.top_left.x + camera.size.x, y), .color = COLORS.grid });
                }
            }

            canvas.instancedSegments(camera, segments.items, CONFIG.grid_width);
        }

        if (toolbar.active_tool == .rect) {
            const rect = toolbar.selectedRect().asRect();
            canvas.strokeRect(camera, rect, 0.1, COLORS.rect_selection_border);
        }

        // hide non-square part of the board
        if (true) {
            canvas.fillRect(
                .{ .top_left = .zero, .size = .new(4, 3) },
                .{ .top_left = .zero, .size = .new(0.5, 3) },
                CellState.black.color(),
            );
            canvas.fillRect(
                .{ .top_left = .zero, .size = .new(4, 3) },
                .from(.{ .{ .bottom_right = .new(4, 3) }, .{ .size = .new(0.5, 3) } }),
                CellState.black.color(),
            );
        }
    } else {
        var level_to_destroy: ?usize = null;
        for (self.all_levels.items, 0..) |l, k| {
            const button: Rect = (Rect{ .top_left = .new(tof32(@mod(k, 4)), tof32(1 + @divFloor(k, 4))), .size = .one }).plusMargin(-0.1);
            const hot = button.contains(ui_mouse.cur.position);
            try catalogue_buttons.append(.{
                .board = l.board,
                .pos = button,
                .radio_selected = false,
                .hot = hot,
            });
            if (hot) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    self.cur_level = l;
                } else if (mouse.wasPressed(.right)) {
                    // avoid modifying the list while iterating over it
                    level_to_destroy = k;
                    _ = catalogue_buttons.pop();
                }
            }
        }
        if (self.is_editor) {
            const k = self.all_levels.items.len;
            const button: Rect = (Rect{ .top_left = .new(tof32(@mod(k, 4)), tof32(1 + @divFloor(k, 4))), .size = .one }).plusMargin(-0.1);
            const hot = button.contains(ui_mouse.cur.position);
            try ui_buttons.append(.{
                .pos = button,
                .radio_selected = hot,
                .color = null,
                .text = "+",
            });
            if (hot) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    try self.addEmptyLevel();
                }
            }
        }

        // save world to file button
        if (true) {
            const button: Rect = (Rect{ .top_left = .zero, .size = .new(0.5, 0.25) }).plusMargin(-0.025);
            const hot = button.contains(ui_mouse.cur.position);
            try ui_buttons.append(.{
                .pos = button,
                .color = null,
                .text = "save",
                .text_scale = 0.25,
                .radio_selected = hot,
            });
            if (hot) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    var buf: std.ArrayList(u8) = .init(mem.frame.allocator());
                    defer buf.deinit();
                    try saveWorld(self.all_levels.items, buf.writer().any());
                    platform.downloadAsFile("gol_world.txt", buf.items);
                }
            }
        }

        // load world from file button
        if (true) {
            const button: Rect = (Rect{ .top_left = .new(0, 0.25), .size = .new(0.5, 0.25) }).plusMargin(-0.025);
            const hot = button.contains(ui_mouse.cur.position);
            try ui_buttons.append(.{
                .pos = button,
                .color = null,
                .text = "load",
                .text_scale = 0.25,
                .radio_selected = hot,
            });
            if (hot) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    platform.askUserForFile();
                }
            }
        }

        if (level_to_destroy) |k| {
            const l = self.all_levels.orderedRemove(k);
            l.board.deinit();
            self.pool_boardstate.destroy(l.board);
            for (l.saved_states.items) |b| {
                @constCast(b).deinit();
                self.pool_boardstate.destroy(@constCast(b));
            }
        }

        self.levelselect_view_offset -= 10 * mouse.cur.scrolled.toNumber() * platform.delta_seconds;
        self.levelselect_view_offset = math.clamp(
            self.levelselect_view_offset,
            0,
            tof32(try std.math.divCeil(usize, self.all_levels.items.len + @as(usize, if (self.is_editor) 1 else 0), 4)),
        );

        //////////////
        // draw

        if (true) {
            var logo_text = canvas.textBatch(0);
            try logo_text.addText("GoL", .centeredAt(.new(2, 0.5)), 0.5, .white);
            logo_text.draw(camera);
        }
    }

    // ui buttons
    if (true) {
        var ui_texts = canvas.textBatch(0);
        defer ui_texts.draw(ui_cam);
        for (ui_buttons.items) |button| {
            canvas.fillRect(ui_cam, button.pos, if (button.radio_selected) .cyan else .red);
            canvas.fillRect(ui_cam, button.pos.plusMargin(-0.005 * ui_cam.size.y), button.color orelse CellState.gray.color());
            if (button.text) |text| try ui_texts.addText(
                text,
                .centeredAt(button.pos.getCenter()),
                0.75 * (button.text_scale orelse 1),
                if (button.color == null) CellState.textColorOver(.gray) else .black,
            );
        }
    }

    // ui buttons special case: catalogue buttons
    if (true) {
        for (catalogue_buttons.items) |button| {
            // only draw if actually visible
            if (ui_cam.intersect(button.pos) == null) continue;

            canvas.fillRect(ui_cam, button.pos, if (button.radio_selected or button.hot) .cyan else .red);
            canvas.fillRect(ui_cam, button.pos.plusMargin(-0.005 * ui_cam.size.y), CellState.black.color());

            const bounds = button.board.boundingRect().asRect().withAspectRatio(1.0, .grow, .center);
            const offset = bounds.top_left;
            const scale = 1.0 / bounds.size.y;

            {
                var cell_bgs: std.ArrayList(Canvas.InstancedShapeInfo) = .init(mem.frame.allocator());
                var it = button.board.cells_states.iterator();
                while (it.next()) |kv| {
                    if (kv.value_ptr.* == .black) continue;

                    try cell_bgs.append(.{
                        .point = .{ .pos = button.pos.applyToLocalPosition(kv.key_ptr.*.tof32().sub(offset).scale(scale)), .scale = scale * button.pos.size.y },
                        .color = kv.value_ptr.*.color(),
                    });
                }
                canvas.fillShapesInstanced(ui_cam, canvas.DEFAULT_SHAPES.square, cell_bgs.items);
            }

            {
                var cell_texts = canvas.textBatch(0);
                var it = button.board.cells_types.iterator();
                while (it.next()) |kv| {
                    if (kv.value_ptr.* == .empty) continue;

                    try cell_texts.addText(
                        kv.value_ptr.*.text(),
                        .centeredAt(button.pos.applyToLocalPosition(kv.key_ptr.*.tof32().sub(offset).add(.half).addY(kv.value_ptr.verticalCorrection()).scale(scale))),
                        scale * kv.value_ptr.sizeCorrection() * button.pos.size.y,
                        @as(CellState, button.board.cells_states.get(kv.key_ptr.*) orelse .black).textColorOver(),
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

fn nextReadIs(in: std.io.AnyReader, comptime expected: []const u8) !void {
    var buf: [expected.len]u8 = undefined;
    const n_read = try in.readAll(&buf);
    if (n_read != expected.len) return error.BadNextRead;
    if (!std.mem.eql(u8, &buf, expected)) return error.BadNextRead;
}
