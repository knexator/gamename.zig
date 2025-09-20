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
        off: FColor = .fromHex("#444444"),
        dim: FColor = .fromHex("#999999"),
        bright: FColor = .white,
    } = .{},
    cell_text: struct {
        on_off: FColor = .white,
        on_dim: FColor = .black,
        on_bright: FColor = .black,
    } = .{},
    grid: FColor = .fromHex("#545454"),
    rect_selection_border: FColor = .cyan,
} = .{};

var CONFIG: struct {
    grid_width: f32 = 0.05,
} = .{};

const MoteType = enum {
    fire,
    water,
    air,
    earth,
    sulfur,
    quicksilver,
    salt,

    pub const all: [@typeInfo(MoteType).@"enum".fields.len]MoteType = .{
        .fire,
        .water,
        .air,
        .earth,
        .salt,
        .sulfur,
        .quicksilver,
    };

    // a bit hacky :(
    pub const all_and_empty: [1 + @typeInfo(MoteType).@"enum".fields.len]?MoteType = .{
        .fire,
        .water,
        .air,
        .earth,
        .salt,
        .sulfur,
        .quicksilver,
        null,
    };

    // TODO: delete
    pub fn text(self: MoteType) []const u8 {
        return switch (self) {
            inline else => |x| &.{comptime x.toChar()},
        };
    }

    pub fn toChar(self: MoteType) u8 {
        return switch (self) {
            .fire => '+',
            .water => '-',
            .air => '=',
            .earth => '~',
            .sulfur => 'o',
            .quicksilver => '?',
            .salt => '*',
        };
    }

    pub fn fromChar(char: u8) !?MoteType {
        if (char == '.') return null;
        inline for (all) |c| {
            if (char == c.toChar()) return c;
        } else return error.BadText;
    }

    /// this gets added to the cell's vertical pos
    pub fn verticalCorrection(self: MoteType) f32 {
        return switch (self.toChar()) {
            '*' => 0.3,
            'o' => -0.125,
            '-' => -0.15,
            else => 0,
        };
    }

    /// text size gets mutliplied by this
    pub fn sizeCorrection(self: MoteType) f32 {
        return switch (self.toChar()) {
            '*' => 1.3,
            '-' => 1.5,
            '?', '~' => 1.0,
            else => 1.3,
        };
    }
};

const Cell = struct {
    state: State,
    motes: std.EnumArray(MoteType, i32),

    pub const empty: Cell = .{
        .state = .off,
        .motes = .initFill(0),
    };

    pub fn equals(this: Cell, other: Cell) bool {
        return std.meta.eql(this, other);
    }

    pub fn fromStateAndMote(state: State, maybe_mote: ?MoteType) Cell {
        var result: Cell = .empty;
        result.state = state;
        if (maybe_mote) |mote| result.motes.set(mote, 1);
        return result;
    }

    pub fn moteCount(cell: Cell) i32 {
        var res: i32 = 0;
        inline for (MoteType.all) |t| {
            res += cell.motes.get(t);
        }
        return res;
    }

    pub fn hasNonQuicksilverMotes(cell: Cell) bool {
        inline for (MoteType.all) |t| {
            if (t == .quicksilver) continue;
            if (cell.motes.get(t) != 0) return true;
        } else return false;
    }

    pub fn setSingleMote(cell: *Cell, mote: ?MoteType) void {
        cell.motes = .initFill(0);
        if (mote) |m| cell.motes.set(m, 1);
    }

    pub fn getSingleMote(cell: Cell) ?MoteType {
        if (cell.moteCount() > 1) std.log.warn("losing info!", .{});
        inline for (MoteType.all) |m| {
            if (cell.motes.get(m) > 0) return m;
        } else return null;
    }

    pub const State = enum {
        off,
        dim,
        bright,

        pub fn color(self: State) FColor {
            return switch (self) {
                .off => COLORS.cell.off,
                .dim => COLORS.cell.dim,
                .bright => COLORS.cell.bright,
            };
        }

        pub fn textColorOver(self: State) FColor {
            return switch (self) {
                .off => COLORS.cell_text.on_off,
                .dim => COLORS.cell_text.on_dim,
                .bright => COLORS.cell_text.on_bright,
            };
        }
    };
};

const Shape = std.AutoArrayHashMap(IVec2, void);

const Signals = struct {
    values: std.AutoArrayHashMap(IVec2, Signal),

    pub fn init(scratch: std.mem.Allocator) Signals {
        return .{ .values = .init(scratch) };
    }

    pub fn clone(source: Signals) !Signals {
        return .{ .values = try .clone(source.values) };
    }

    pub fn getPtr(signals: *Signals, p: IVec2) !*Signal {
        const gop = try signals.values.getOrPut(p);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        return gop.value_ptr;
    }

    pub fn at(signals: Signals, p: IVec2) Signal {
        return signals.values.get(p) orelse .empty;
    }

    pub fn equals(self: Signals, other: Signals) bool {
        return self.isSubsetOf(other) and other.isSubsetOf(self);
    }

    fn isSubsetOf(self: Signals, other: Signals) bool {
        var it = self.values.iterator();
        while (it.next()) |kv| {
            if (!other.at(kv.key_ptr.*).equals(kv.value_ptr.*)) return false;
        }
        return true;
    }
};

/// all the signals arriving on a given cell
const Signal = struct {
    fire: i32,
    water: i32,
    earth: i32,
    sulfur: i32,

    push: IVec2,

    /// the vector-summed offsets from the origin
    /// of each *on* signal to the destination cell
    on_offset: IVec2,
    /// the vector-summed offsets from the origin
    /// of each *off* signal to the destination cell
    off_offset: IVec2,

    pub const empty: Signal = .{
        .fire = 0,
        .water = 0,
        .earth = 0,
        .sulfur = 0,
        .push = .zero,
        .on_offset = .zero,
        .off_offset = .zero,
    };

    pub fn send(signals: *Signals, signal: Signal, source: IVec2, destination: IVec2) !void {
        const target = try signals.getPtr(destination);

        target.fire += signal.fire;
        target.earth += signal.earth;
        target.sulfur += signal.sulfur;
        target.water += signal.water;

        const delta = destination.sub(source);
        target.on_offset.addInPlace(delta.scale(signal.fire + signal.sulfur + signal.earth));
        target.off_offset.addInPlace(delta.scale(signal.water));
        target.push.addInPlace(signal.push);
    }

    pub fn equals(this: Signal, other: Signal) bool {
        return std.meta.eql(this, other);
    }
};

const SaltPositions = std.ArrayList(struct {
    pos: IVec2,
    salt: i32,
});

fn sendSulfurSignals(board: BoardState, signals: *Signals, sulfur_shape: *Shape, source: IVec2, scratch: std.mem.Allocator) !void {
    if (sulfur_shape.get(source) != null) return;
    var positions: std.ArrayList(IVec2) = .init(scratch);
    try positions.append(source);
    try sulfur_shape.put(source, {});

    var cursor: usize = 0;
    var bounds: math.IBounds = .empty;
    while (cursor < positions.items.len) : (cursor += 1) {
        const p = positions.items[cursor];
        bounds.plusTile(p);
        for (IVec2.cardinal_directions) |d| {
            try visitBrightSulfur(board, sulfur_shape, &positions, p.add(d));
        }
    }

    for (positions.items) |p| {
        const signal: Signal = .{
            .fire = 0,
            .water = 0,
            .earth = 0,
            .sulfur = board.cellAt(p).motes.get(.sulfur),
            .push = .zero,
            .on_offset = .zero,
            .off_offset = .zero,
        };
        // TODO NOW: add one?
        const size = bounds.inner_size; // .add(.one);
        for (IVec2.eight_directions) |d| {
            try Signal.send(signals, signal, p, p.add(d.mul(size.cast(isize))));
        }
    }
}

fn visitBrightSulfur(board: BoardState, sulfur_shape: *Shape, positions: *std.ArrayList(IVec2), p: IVec2) !void {
    if (sulfur_shape.get(p) == null and
        board.cellAt(p).state == .bright and
        board.cellAt(p).motes.get(.sulfur) > 0)
    {
        try positions.append(p);
        try sulfur_shape.put(p, {});
    }
}

fn collectSalts(board: BoardState, scratch: std.mem.Allocator) !SaltPositions {
    var result: SaltPositions = .init(scratch);
    var it = board.cells.iterator();
    while (it.next()) |kv| {
        const pos = kv.key_ptr.*;
        const cell = kv.value_ptr.*;
        if (cell.motes.get(.salt) == 0) continue;
        try result.append(.{ .pos = pos, .salt = cell.motes.get(.salt) });
    }
    return result;
}

fn lightOneSulfur(board: *BoardState, pre_light: BoardState, stack: *std.ArrayList(IVec2), pos: IVec2) !void {
    if (board.cellAt(pos).state == .off and
        pre_light.cellAt(pos).state == .off and
        board.cellAt(pos).motes.get(.sulfur) > 0)
    {
        try stack.append(pos);
        try board.setStateAt(pos, .bright);
    }
}

fn lightConnectedSulfur(board: *BoardState, pre_light: BoardState, initial_position: IVec2, scratch: std.mem.Allocator) !void {
    var stack: std.ArrayList(IVec2) = .init(scratch);
    try stack.append(initial_position);
    while (stack.pop()) |p| {
        for (IVec2.cardinal_directions) |d| {
            try lightOneSulfur(board, pre_light, &stack, p.add(d));
        }
    }
}

const Toolbar = struct {
    painting: bool = false,
    active_state: Cell.State = .bright,
    active_type: ?MoteType = .fire,
    selected_rect_inner_corner1: IVec2 = .zero,
    selected_rect_inner_corner2: IVec2 = .zero,
    rect_tool_state: union(enum) {
        none,
        selecting,
        moving: kommon.Grid2D(Cell),
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
    // TODO: use grids
    cells: std.AutoArrayHashMap(IVec2, Cell),

    pub fn init(dst: *BoardState, gpa: std.mem.Allocator) !void {
        dst.cells = .init(gpa);
    }

    pub fn deinit(self: *BoardState) void {
        self.cells.deinit();
    }

    pub fn next(board: *BoardState, scratch: std.mem.Allocator) !void {
        var signals: Signals = .init(scratch);
        var sulfur_shape: Shape = .init(scratch);
        var cell_it = board.cells.iterator();

        // step 1: send signals for bright cells.  the signals arriving at each cell
        // are recorded to be applied in later steps.
        while (cell_it.next()) |kv| {
            const pos = kv.key_ptr.*;
            const cell = kv.value_ptr.*;
            if (cell.state != .bright) continue;
            if (cell.motes.get(.fire) != 0 or cell.motes.get(.water) != 0 or cell.motes.get(.earth) != 0) {
                for (IVec2.cardinal_directions) |delta| {
                    try Signal.send(&signals, .{
                        .fire = cell.motes.get(.fire),
                        .water = cell.motes.get(.water),
                        .earth = cell.motes.get(.earth),
                        .sulfur = 0,
                        .push = delta.scale(cell.motes.get(.earth)),
                        .on_offset = .zero,
                        .off_offset = .zero,
                    }, pos, pos.add(delta));
                }
            }
            if (cell.motes.get(.sulfur) != 0) {
                try sendSulfurSignals(board.*, &signals, &sulfur_shape, pos, scratch);
            }
        }

        // step 2a: air motes extend the reach of all signals they receive.
        // this step repeats until a steady state is reached.
        var air_received: Signals = .init(scratch);
        var air_sent: Signals = try .clone(signals);
        while (!Signals.equals(air_received, air_sent)) {
            air_received = air_sent;
            air_sent = .init(scratch);
            cell_it.reset();
            while (cell_it.next()) |kv| {
                const pos = kv.key_ptr.*;
                const cell = kv.value_ptr.*;
                if (cell.motes.get(.air) == 0) continue;
                const received = air_received.at(pos);
                if (cell.state == .off and received.water != 0) {
                    try Signal.send(&signals, .{
                        .fire = 0,
                        .water = received.water * cell.motes.get(.air),
                        .earth = 0,
                        .sulfur = 0,
                        .push = .zero,
                        .on_offset = .zero,
                        .off_offset = .zero,
                    }, pos.sub(received.off_offset), pos.add(received.off_offset));
                    try Signal.send(&air_sent, .{
                        .fire = 0,
                        .water = received.water * cell.motes.get(.air),
                        .earth = 0,
                        .sulfur = 0,
                        .push = .zero,
                        .on_offset = .zero,
                        .off_offset = .zero,
                    }, pos.sub(received.off_offset), pos.add(received.off_offset));
                } else if (cell.state != .off and (received.fire != 0 or received.earth != 0 or received.sulfur != 0)) {
                    try Signal.send(&signals, .{
                        .fire = received.fire * cell.motes.get(.air),
                        .water = 0,
                        .earth = received.earth * cell.motes.get(.air),
                        .sulfur = received.sulfur * cell.motes.get(.air),
                        .push = received.push.scale(cell.motes.get(.air)),
                        .on_offset = .zero,
                        .off_offset = .zero,
                    }, pos.sub(received.on_offset), pos.add(received.on_offset));
                    try Signal.send(&air_sent, .{
                        .fire = received.fire * cell.motes.get(.air),
                        .water = 0,
                        .earth = received.earth * cell.motes.get(.air),
                        .sulfur = received.sulfur * cell.motes.get(.air),
                        .push = received.push.scale(cell.motes.get(.air)),
                        .on_offset = .zero,
                        .off_offset = .zero,
                    }, pos.sub(received.on_offset), pos.add(received.on_offset));
                }
            }
        }

        // step 2b: if a cell receives new signals in the steady state, it will
        // receive an infinite amount.  for now, add 1000 as a reasonably big number.
        var air_received_it = air_received.values.iterator();
        while (air_received_it.next()) |kv| {
            const pos = kv.key_ptr.*;
            const received = kv.value_ptr.*;
            if (received.fire != 0 or received.water != 0 or received.earth != 0 or received.sulfur != 0) {
                std.log.warn("infinite signal at {any}!\n", .{pos});
                const signal = try signals.getPtr(pos);
                signal.fire += 1000 * received.fire;
                signal.water += 1000 * received.water;
                signal.earth += 1000 * received.earth;
                signal.sulfur += 1000 * received.sulfur;
                signal.push.addInPlace(received.push.scale(1000));
            }
        }

        // step 3: signals arriving at salt motes are duplicated onto all the other
        // salt motes with the same count.
        {
            const salt_positions = try collectSalts(board.*, scratch);
            var i: usize = 0;
            while (i < salt_positions.items.len) {
                const salt = salt_positions.items[i].salt;
                var duplicated: Signal = .empty;

                var n = i;
                while (n < salt_positions.items.len) : (n += 1) {
                    const p = salt_positions.items[n];
                    if (p.salt != salt) {
                        break;
                    }
                    const signal = signals.at(p.pos);
                    duplicated.fire += signal.fire;
                    duplicated.water += signal.water;
                    duplicated.sulfur += signal.sulfur;
                }
                while (i < n) : (i += 1) {
                    const p = salt_positions.items[i];
                    const signal = try signals.getPtr(p.pos);
                    signal.*.fire = duplicated.fire;
                    signal.*.water = duplicated.water;
                    signal.*.sulfur = duplicated.sulfur;
                }
            }
        }

        // step 4: dim all bright cells that contain a mote.
        cell_it.reset();
        while (cell_it.next()) |kv| {
            const cell = kv.value_ptr;
            if (cell.state != .bright or cell.moteCount() == 0) continue;
            cell.state = .dim;
        }

        // step 5: off quicksilver consumes signals, converting them to motes.
        // these signals have no effect in later steps.
        cell_it.reset();
        while (cell_it.next()) |kv| {
            const cell = kv.value_ptr;
            if (cell.state != .off or
                cell.motes.get(.quicksilver) == 0 or
                cell.hasNonQuicksilverMotes())
            {
                continue;
            }
            const signal = try signals.getPtr(kv.key_ptr.*);
            inline for ([_]MoteType{ .fire, .water, .earth, .sulfur }) |t| {
                cell.motes.set(t, cell.motes.get(t) * @field(signal, @tagName(t)));
            }
            signal.* = .empty;
        }

        // step 6: cells containing a mote which receives signal transition either
        // from off to bright or from lit to off, depending on the signal type.
        // cells containing quicksilver which transition from lit to off reset the
        // cell to just one quicksilver.
        const pre_light: BoardState = try .cloneWithAllocator(board.*, scratch);
        cell_it.reset();
        while (cell_it.next()) |kv| {
            const cell = kv.value_ptr;
            if (cell.moteCount() == 0) continue;
            const signal = signals.at(kv.key_ptr.*);
            if (cell.state == .off and (signal.fire != 0 or signal.sulfur != 0)) {
                cell.state = .bright;
            } else if (cell.state != .off and signal.water != 0) {
                cell.state = .off;
                if (cell.motes.get(.quicksilver) != 0) {
                    cell.motes = .initFill(0);
                    cell.motes.set(.quicksilver, 1);
                }
            }
        }

        // step 7: bright cells within a sulfur shape that's turned off are extended
        // to fill the entire connected shape.  if a cell was just turned off in
        // step 6, it is not included in the shape for the purposes of this step.
        cell_it.reset();
        while (cell_it.next()) |kv| {
            const pos = kv.key_ptr.*;
            const cell = kv.value_ptr.*;
            if (cell.state != .bright or cell.motes.get(.sulfur) == 0) continue;
            try lightConnectedSulfur(board, pre_light, pos, scratch);
        }

        // step 8: motes are pushed according to the accumulated push signal.
        // quicksilver motes and lit air motes are not pushed.
        const pre_push = try board.cloneWithAllocator(scratch);
        var signals_it = signals.values.iterator();
        while (signals_it.next()) |kv| {
            const pos = kv.key_ptr.*;
            const signal = kv.value_ptr.*;
            if (signal.push.equals(.zero)) continue;
            const cell = pre_push.cellAt(pos);
            if (!cell.hasNonQuicksilverMotes()) continue;
            const dest_pos = pos.add(signal.push);
            const b = try board.cellPtrAt(pos);
            const d = try board.cellPtrAt(dest_pos);
            inline for ([_]MoteType{ .fire, .water, .earth, .sulfur, .salt }) |t| {
                b.motes.getPtr(t).* -= cell.motes.get(t);
                d.motes.getPtr(t).* += cell.motes.get(t);
            }
            if (cell.state == .off) {
                b.motes.getPtr(.air).* -= cell.motes.get(.air);
                d.motes.getPtr(.air).* += cell.motes.get(.air);
            }
        }

        // step 9: if there is a conflict between lit states among salt motes with
        // the same count, the salt motes are removed.
        {
            const salt_positions = try collectSalts(board.*, scratch);
            var i: usize = 0;
            while (i < salt_positions.items.len) {
                const salt = salt_positions.items[i].salt;
                const state = board.cellAt(salt_positions.items[i].pos).state;
                var conflict = false;
                var n = i;
                while (n < salt_positions.items.len) : (n += 1) {
                    const p = salt_positions.items[n];
                    if (p.salt != salt) break;
                    if (board.cellAt(p.pos).state != state) {
                        conflict = true;
                    }
                }
                if (conflict) {
                    while (i < n) : (i += 1) {
                        (try board.cellPtrAt(salt_positions.items[i].pos)).motes.set(.salt, 0);
                    }
                } else {
                    i = n;
                }
            }
        }
    }

    fn cloneWithAllocator(self: BoardState, allocator: std.mem.Allocator) !BoardState {
        return .{ .cells = try self.cells.cloneWithAllocator(allocator) };
    }

    fn clone(self: BoardState) !BoardState {
        return .{ .cells = try self.cells.clone() };
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
        var it = self.cells.iterator();
        while (it.next()) |kv| {
            if (!other.cellAt(kv.key_ptr.*).equals(kv.value_ptr.*)) return false;
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

        var it = self.cells.iterator();
        while (it.next()) |kv| {
            const cell = kv.value_ptr.*;
            const pos = kv.key_ptr.*;
            if (include.lit and cell.state != .off) {
                result.plusTile(pos);
            }
            if (include.elements and cell.moteCount() > 0) {
                result.plusTile(pos);
            }
        }

        return result;
    }

    pub fn boundingRect(self: BoardState) math.IBounds {
        return self.boundingRectV2(.{ .lit = true, .elements = true });
    }

    pub fn cellAt(self: BoardState, pos: IVec2) Cell {
        return self.cells.get(pos) orelse .empty;
    }

    pub fn cellPtrAt(self: *BoardState, pos: IVec2) !*Cell {
        const gop = try self.cells.getOrPut(pos);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        return gop.value_ptr;
    }

    pub fn setStateAt(self: *BoardState, pos: IVec2, state: Cell.State) !void {
        (try self.cellPtrAt(pos)).state = state;
    }

    pub fn setSingleMoteAt(self: *BoardState, pos: IVec2, mote: ?MoteType) !void {
        (try self.cellPtrAt(pos)).setSingleMote(mote);
    }

    pub fn toText(self: BoardState, out: std.io.AnyWriter) !void {
        const bounds = self.boundingRect();
        try out.writeAll("V1\n");
        for (0..bounds.inner_size.y) |dj| {
            for (0..bounds.inner_size.x) |di| {
                const p = bounds.top_left.addUnsigned(.new(di, dj));
                const cell = self.cellAt(p);
                try out.writeAll(switch (cell.state) {
                    .off => " ",
                    .dim => ".",
                    .bright => ":",
                });
                if (cell.moteCount() > 1) std.log.warn("Cell has multiple motes; information will be lost.", .{});
                try out.writeAll(for (MoteType.all) |t| {
                    if (cell.motes.get(t) > 0) break t.text();
                } else ".");
            }
            try out.writeAll("\n");
        }
    }

    pub fn fromText(dst: *BoardState, scratch: std.mem.Allocator, text: []const u8) !void {
        dst.cells.clearRetainingCapacity();
        const contents = std.mem.trim(u8, text, &std.ascii.whitespace);
        assert(std.mem.startsWith(u8, contents, "V1\n"));
        const raw_ascii = try kommon.Grid2D([2]u8).fromAsciiWide(2, scratch, std.mem.trimRight(u8, contents["V1\n".len..], &std.ascii.whitespace));
        defer raw_ascii.deinit(scratch);
        var it = raw_ascii.iteratorSigned();
        while (it.next()) |p| {
            const t = raw_ascii.atSigned(p);
            const cell_state: Cell.State = switch (t[0]) {
                ' ' => .off,
                '.' => .dim,
                ':' => .bright,
                else => return error.BadText,
            };
            const cell_mote = try MoteType.fromChar(t[1]);
            const cell: Cell = .fromStateAndMote(cell_state, cell_mote);
            if (!cell.equals(.empty)) try dst.cells.put(p, cell);
        }
    }

    pub fn getSubrect(self: BoardState, alloc: std.mem.Allocator, bounds: math.IBounds) !kommon.Grid2D(Cell) {
        const result: kommon.Grid2D(Cell) = try .initUndefined(alloc, bounds.inner_size);
        var it = result.iteratorSigned();
        while (it.next()) |p| {
            result.setSigned(p, self.cellAt(p.add(bounds.top_left)));
        }
        return result;
    }

    pub fn clearSubrect(self: *BoardState, bounds: math.IBounds) !void {
        var it = kommon.itertools.inIBounds(bounds);
        while (it.next()) |p| {
            _ = self.cells.swapRemove(p);
        }
    }

    pub fn setSubrect(self: *BoardState, values: kommon.Grid2D(Cell), bounds: math.IBounds, include_blanks: bool) !void {
        assert(bounds.inner_size.equals(values.size));
        var it = values.iteratorSigned();
        while (it.next()) |p| {
            const cell = values.atSigned(p);
            if (include_blanks or !cell.equals(.empty)) try self.cells.put(p.add(bounds.top_left), cell);
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

    tweakable.fcolor("Off", &COLORS.cell.off);
    tweakable.fcolor("Dim", &COLORS.cell.dim);
    tweakable.fcolor("Bright", &COLORS.cell.bright);

    tweakable.fcolor("grid", &COLORS.grid);
    tweakable.fcolor("rect border", &COLORS.rect_selection_border);

    tweakable.fcolor("text over Off", &COLORS.cell_text.on_off);
    tweakable.fcolor("text over Dim", &COLORS.cell_text.on_dim);
    tweakable.fcolor("text over Bright", &COLORS.cell_text.on_bright);

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
            const res: *BoardState = try self.pool_boardstate.create();
            try res.init(self.usual.mem.gpa);

            try res.setStateAt(.new(0, 1), .dim);
            try res.setStateAt(.new(0, 2), .bright);
            try res.setSingleMoteAt(.new(1, 0), .fire);
            try res.setSingleMoteAt(.new(1, 1), .salt);
            try res.setSingleMoteAt(.new(1, 2), .sulfur);
            try res.setSingleMoteAt(.new(2, 0), .air);
            try res.setSingleMoteAt(.new(2, 1), .water);

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

    platform.gl.clear(Cell.State.off.color());
    if (self.cur_level) |cur_level| {
        var toolbar = &cur_level.toolbar;
        const cell_under_mouse = mouse.cur.position.toInt(isize);

        // paint cell states
        for ([3]Cell.State{ .off, .dim, .bright }, 0..) |c, k| {
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
            for (MoteType.all_and_empty, 0..) |t, k| {
                const button: Rect = (Rect{ .top_left = .new(tof32(@mod(k, 2)), tof32(5 + @mod(@divFloor(k, 2), 4))), .size = .one }).plusMargin(-0.1);
                try ui_buttons.append(.{
                    .pos = button,
                    .color = null,
                    .text = if (t) |tt| tt.text() else "",
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

        // advace state button
        if (true) {
            const button: Rect = top_right_button.move(.new(0, 2)).plusMargin(-0.1);
            const hot = button.contains(ui_mouse.cur.position);
            try ui_buttons.append(.{
                .pos = button,
                .color = null,
                .text = "V",
                .radio_selected = hot,
            });
            if (hot) {
                mouse_over_ui = true;
                if (mouse.wasPressed(.left)) {
                    try cur_level.board.next(mem.scratch.allocator());
                }
            }
        }

        if (platform.keyboard.wasPressed(.KeyV)) {
            try cur_level.board.next(mem.scratch.allocator());
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
                                try cur_level.board.setStateAt(cell_under_mouse, toolbar.active_state);
                            }
                        }
                    } else {
                        if (mouse.wasPressed(.left)) {
                            toolbar.painting = true;
                        }
                        if (mouse.wasPressed(.right)) {
                            toolbar.active_state = cur_level.board.cellAt(cell_under_mouse).state;
                        }
                    }
                },
                .paint_type => {
                    if (toolbar.painting) {
                        if (!mouse.cur.isDown(.left)) {
                            toolbar.painting = false;
                        } else {
                            try cur_level.board.setSingleMoteAt(cell_under_mouse, toolbar.active_type);
                        }
                    } else {
                        if (mouse.wasPressed(.left)) {
                            toolbar.painting = true;
                        }
                        if (mouse.wasPressed(.right)) {
                            toolbar.active_type = cur_level.board.cellAt(cell_under_mouse).getSingleMote();
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
                for (&[_]Cell.State{ .off, .dim, .bright }, 0..) |t, k| {
                    if (platform.keyboard.wasPressed(.digit(k + 1))) {
                        toolbar.active_state = t;
                    }
                }
                if (platform.keyboard.wasPressed(.Backquote) or platform.keyboard.wasPressed(.Space)) {
                    toolbar.active_tool = .paint_type;
                }
            },
            .paint_type => {
                for (&MoteType.all, 0..) |t, k| {
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
            var cell_texts = canvas.textBatch(0);

            defer cell_texts.draw(camera);
            defer canvas.fillShapesInstanced(camera, canvas.DEFAULT_SHAPES.square, cell_bgs.items);

            const cam_bounds: math.IBounds = .fromRect(cur_level.camera.plusMargin(1.1));
            var it = visible_board.cells.iterator();
            while (it.next()) |kv| {
                const pos = kv.key_ptr.*;
                const cell = kv.value_ptr.*;
                if (!cam_bounds.contains(pos)) continue;
                if (cell.state != .off) {
                    try cell_bgs.append(.{
                        .point = .{ .pos = pos.tof32() },
                        .color = cell.state.color(),
                    });
                }
                if (toolbar.zoom != .bounds_lit) {
                    inline for (MoteType.all) |t| {
                        if (cell.motes.get(t) > 0) {
                            try cell_texts.addText(
                                t.text(),
                                .centeredAt(pos.tof32().add(.half).addY(t.verticalCorrection())),
                                t.sizeCorrection(),
                                cell.state.textColorOver(),
                            );
                        }
                    }
                }
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

                const visible_state = if (!toolbar.rect_tool_moving_include_blank and cell.state == .off)
                    cur_level.board.cellAt(pos).state
                else
                    cell.state;

                const visible_motes = if (!toolbar.rect_tool_moving_include_blank and cell.moteCount() == 0)
                    cur_level.board.cellAt(pos).motes
                else
                    cell.motes;

                try cell_bgs.append(.{
                    .point = .{ .pos = pos.tof32() },
                    .color = visible_state.color(),
                });

                inline for (MoteType.all) |t| {
                    if (visible_motes.get(t) > 0) {
                        try cell_texts.addText(
                            t.text(),
                            .centeredAt(pos.tof32().add(.half).addY(t.verticalCorrection())),
                            t.sizeCorrection(),
                            visible_state.textColorOver(),
                        );
                    }
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
                Cell.State.off.color(),
            );
            canvas.fillRect(
                .{ .top_left = .zero, .size = .new(4, 3) },
                .from(.{ .{ .bottom_right = .new(4, 3) }, .{ .size = .new(0.5, 3) } }),
                Cell.State.off.color(),
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
            canvas.fillRect(ui_cam, button.pos.plusMargin(-0.005 * ui_cam.size.y), button.color orelse Cell.State.dim.color());
            if (button.text) |text| try ui_texts.addText(
                text,
                .centeredAt(button.pos.getCenter()),
                0.75 * (button.text_scale orelse 1),
                if (button.color == null) Cell.State.textColorOver(.dim) else .black,
            );
        }
    }

    // ui buttons special case: catalogue buttons
    if (true) {
        for (catalogue_buttons.items) |button| {
            // only draw if actually visible
            if (ui_cam.intersect(button.pos) == null) continue;

            canvas.fillRect(ui_cam, button.pos, if (button.radio_selected or button.hot) .cyan else .red);
            canvas.fillRect(ui_cam, button.pos.plusMargin(-0.005 * ui_cam.size.y), Cell.State.off.color());

            const bounds = button.board.boundingRect().asRect().withAspectRatio(1.0, .grow, .center);
            const offset = bounds.top_left;
            const scale = 1.0 / bounds.size.y;

            {
                var cell_bgs: std.ArrayList(Canvas.InstancedShapeInfo) = .init(mem.frame.allocator());
                var cell_texts = canvas.textBatch(0);

                defer cell_texts.draw(ui_cam);
                defer canvas.fillShapesInstanced(ui_cam, canvas.DEFAULT_SHAPES.square, cell_bgs.items);

                var it = button.board.cells.iterator();
                while (it.next()) |kv| {
                    const pos = kv.key_ptr.*;
                    const cell = kv.value_ptr.*;
                    if (cell.state != .off) {
                        try cell_bgs.append(.{
                            .point = .{ .pos = button.pos.applyToLocalPosition(pos.tof32().sub(offset).scale(scale)), .scale = scale * button.pos.size.y },
                            .color = cell.state.color(),
                        });
                    }
                    inline for (MoteType.all) |t| {
                        if (cell.motes.get(t) > 0) {
                            try cell_texts.addText(
                                t.text(),
                                .centeredAt(button.pos.applyToLocalPosition(kv.key_ptr.*.tof32().sub(offset).add(.half).addY(t.verticalCorrection()).scale(scale))),
                                scale * t.sizeCorrection() * button.pos.size.y,
                                cell.state.textColorOver(),
                            );
                        }
                    }
                }
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
