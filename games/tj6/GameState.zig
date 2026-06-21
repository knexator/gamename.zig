pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};
pub const tracy = @import("tracy");

const CONFIG: struct {
    can_grab_fireflies: bool = true,
} = .{};

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "tj6",
        .author = "knexator",
        .desired_aspect_ratio = 4.0 / 3.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
        .atlas_texture = "assets/spritesheet.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

usual: kommon.Usual,
levels: []LevelState,
cur_level: usize = 0,
started_grabbing_at: ?IVec2 = null,
atlas_texture: Gl.Texture,

editing: bool = false,

const levels_ascii_raw: []const u8 = @embedFile("levels.txt");

fn testMove(
    allocator: std.mem.Allocator,
    start_ascii: []const u8,
    expected_ascii: []const u8,
    pos: IVec2,
    dir: IVec2,
) !void {
    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();
    var start: LevelState = try .fromAscii(allocator, start_ascii);
    defer start.deinit(allocator);
    const actual = try start.states_history.getLast().doMove(start.info, allocator, arena.allocator(), pos, pos.add(dir));
    try std.testing.expect(actual.new_state != null);
    defer allocator.free(actual.new_state.?.animals);
    const actual_ascii = try actual.new_state.?.toAscii(start.info, allocator);
    defer allocator.free(actual_ascii);

    try std.testing.expectEqualStrings(
        std.mem.trim(u8, expected_ascii, &std.ascii.whitespace),
        std.mem.trim(u8, actual_ascii, &std.ascii.whitespace),
    );
}

test "simple move" {
    try testMove(std.testing.allocator,
        \\#####
        \\#...#
        \\@.F.#
        \\#F..#
        \\#K.F#
        \\#####
    ,
        \\#####
        \\#...#
        \\@.F.#
        \\#F..#
        \\#.KF#
        \\#####
    , .new(1, 4), .new(1, 0));
}

test "catslime" {
    try testMove(std.testing.allocator,
        \\C.@
        \\K..
    ,
        \\.C@
        \\.K.
    , .zero, .new(1, 0));

    try testMove(std.testing.allocator,
        \\CC....@
        \\K......
    ,
        \\.CC...@
        \\.K.....
    , .zero, .new(1, 0));

    try testMove(std.testing.allocator,
        \\CC....@
        \\K......
    ,
        \\.CC...@
        \\.K.....
    , .new(1, 0), .new(1, 0));

    try testMove(std.testing.allocator,
        \\.......
        \\CC....@
        \\K......
    ,
        \\CC.....
        \\K.....@
        \\.......
    , .new(0, 1), .new(0, -1));

    try testMove(std.testing.allocator,
        \\.......
        \\CC....@
        \\K......
    ,
        \\CC.....
        \\K.....@
        \\.......
    , .new(1, 1), .new(0, -1));

    try testMove(std.testing.allocator,
        \\.......
        \\CC....@
        \\DF.....
    ,
        \\CC.....
        \\DF....@
        \\.......
    , .new(1, 1), .new(0, -1));

    try testMove(std.testing.allocator,
        \\.......
        \\CC#...@
        \\DFK....
    ,
        \\CC.....
        \\DF#...@
        \\..K....
    , .new(1, 1), .new(0, -1));

    try testMove(std.testing.allocator,
        \\.......
        \\CC#...@
        \\DFK....
    ,
        \\CC.....
        \\DF#...@
        \\..K....
    , .new(0, 1), .new(0, -1));

    try testMove(std.testing.allocator,
        \\....
        \\@...
        \\.CC#
        \\.FDK
    ,
        \\....
        \\@CC.
        \\.FD#
        \\...K
    , .new(1, 2), .new(0, -1));

    try testMove(std.testing.allocator,
        \\...
        \\@..
        \\.CC
        \\.FD
    ,
        \\...
        \\@CC
        \\.FD
        \\...
    , .new(2, 2), .new(0, -1));

    try testMove(std.testing.allocator,
        \\@.......
        \\...#....
        \\.CCCCC..
        \\.FDKKK..
    ,
        \\@.......
        \\.CC#....
        \\.FDCCC..
        \\...KKK..
    , .new(2, 2), .new(0, -1));

    try testMove(std.testing.allocator,
        \\@.......
        \\....#...
        \\.CCCCC..
        \\.FDKKK..
    ,
        \\@.......
        \\.CCC#...
        \\.FDKCC..
        \\....KK..
    , .new(1, 2), .new(0, -1));

    try testMove(std.testing.allocator,
        \\@.......
        \\..#.....
        \\.CCCCC..
        \\.FDKKK..
    ,
        \\@.......
        \\..#CCC..
        \\.CCKKK..
        \\.FD.....
    , .new(5, 2), .new(0, -1));
}

const LevelState = struct {
    states_history: std.ArrayListUnmanaged(Snapshot),
    info: Constant,

    const Constant = struct {
        walls: Grid2D(bool),
        lock: IVec2,

        pub fn size(info: Constant) IVec2 {
            return info.walls.size.cast(isize);
        }
    };

    const Snapshot = struct {
        animals: []const Animal,
        solved: bool,

        fn animalAt(self: Snapshot, pos: IVec2) ?usize {
            return animalAt2(self.animals, pos);
        }

        fn animalAt2(animals: []const Animal, pos: IVec2) ?usize {
            for (animals, 0..) |animal, k| {
                if (animal.pos.equals(pos)) return k;
            } else return null;
        }

        fn surroundingAnimals(self: Snapshot, pos: IVec2) std.BoundedArray(Animal.Kind, 4) {
            var result: std.BoundedArray(Animal.Kind, 4) = .{};
            for (IVec2.cardinal_directions) |dir| {
                if (self.animalAt(pos.add(dir))) |a| result.appendAssumeCapacity(self.animals[a].kind);
            }
            return result;
        }

        fn doMove(old_state: Snapshot, info: Constant, allocator: std.mem.Allocator, scratch: std.mem.Allocator, old_pos: IVec2, new_pos: IVec2) !struct {
            message: ?[]const []const u8,
            new_state: ?Snapshot,

            const empty: @This() = .{ .message = null, .new_state = null };
            fn badMove(msg: []const []const u8) @This() {
                return .{ .new_state = null, .message = msg };
            }
        } {
            if (old_pos.sub(new_pos).taxiMag() != 1) return .empty;
            if (info.walls.atSigned(new_pos)) return .empty;
            const grabbed = old_state.animalAt(old_pos) orelse return .empty;
            const animal_kind = old_state.animals[grabbed].kind;
            if (animal_kind == .firefly and !CONFIG.can_grab_fireflies) {
                return .badMove(&.{ "Ouch! These fireflies are", "too hot to handle" });
            }

            const dir = new_pos.sub(old_pos);
            const animals = old_state.animals;

            // all animals connected to the current one, via catslimes
            const in_group = try scratch.alloc(bool, animals.len);
            @memset(in_group, false);
            in_group[grabbed] = true;
            var stack: std.ArrayListUnmanaged(usize) = .empty;
            try stack.append(scratch, grabbed);
            var cursor: usize = 0;
            while (cursor < stack.items.len) : (cursor += 1) {
                const source_index = stack.items[cursor];
                for (IVec2.cardinal_directions) |d| {
                    const target_index = animalAt2(animals, animals[source_index].pos.add(d)) orelse continue;
                    if (in_group[target_index]) continue;
                    if (animals[source_index].kind == .catslime or animals[target_index].kind == .catslime) {
                        in_group[target_index] = true;
                        try stack.append(scratch, target_index);
                    }
                }
            }

            // remove any animals blocked by a wall, and those reachable through it
            const moving = try scratch.dupe(bool, in_group);
            const reachable = try scratch.alloc(bool, animals.len);
            while (true) {
                var changed = false;

                // A member blocked by a wall or a stationary animal stops.
                for (animals, 0..) |animal, idx| {
                    if (!moving[idx]) continue;
                    const dest = animal.pos.add(dir);
                    const blocked_by_wall = info.walls.atSignedSafe(dest) orelse true;
                    const blocked_by_animal = if (animalAt2(animals, dest)) |j|
                        !moving[j]
                    else
                        false;

                    if (blocked_by_wall or blocked_by_animal) {
                        moving[idx] = false;
                        changed = true;
                    }
                }

                // Only animals still reachable from the grabbed one, through
                // members that are themselves moving, keep being dragged.
                @memset(reachable, false);
                if (moving[grabbed]) {
                    reachable[grabbed] = true;
                    stack.clearRetainingCapacity();
                    try stack.append(scratch, grabbed);
                    var c: usize = 0;
                    while (c < stack.items.len) : (c += 1) {
                        const idx = stack.items[c];
                        for (IVec2.cardinal_directions) |d| {
                            const j = animalAt2(animals, animals[idx].pos.add(d)) orelse continue;
                            if (reachable[j] or !moving[j]) continue;
                            if (animals[idx].kind == .catslime or animals[j].kind == .catslime) {
                                reachable[j] = true;
                                try stack.append(scratch, j);
                            }
                        }
                    }
                }
                for (moving, 0..) |*m, idx| {
                    if (m.* and !reachable[idx]) {
                        m.* = false;
                        changed = true;
                    }
                }

                if (!changed) break;
            }

            // Nothing happens if the grabbed animal itself can't move.
            if (!moving[grabbed]) return .empty;

            const new_animals = try allocator.dupe(Animal, animals);
            for (new_animals, 0..) |*animal, idx| {
                if (moving[idx]) animal.pos.addInPlace(dir);
            }

            var result: Snapshot = .{ .animals = new_animals, .solved = old_state.solved };

            if (true) { // validate rule: all sundragons next to a firefly
                for (new_animals) |animal| {
                    if (animal.kind != .sundragon) continue;
                    const any_firefly = for (result.surroundingAnimals(animal.pos).constSlice()) |k| {
                        if (k == .firefly) break true;
                    } else false;
                    if (!any_firefly) {
                        allocator.free(new_animals);
                        return .badMove(&.{ "Sundragon must always", "be next to a firefly" });
                    }
                }
            }

            return .{ .new_state = result, .message = null };
        }

        fn toAscii(self: Snapshot, info: Constant, allocator: std.mem.Allocator) ![]const u8 {
            const ascii_grid: Grid2D(u8) = try info.walls.map(allocator, u8, struct {
                fn anon(is_wall: bool) u8 {
                    return if (is_wall) '#' else '.';
                }
            }.anon);
            defer ascii_grid.deinit(allocator);

            ascii_grid.setSigned(info.lock, '@');
            for (self.animals) |animal| {
                ascii_grid.setSigned(animal.pos, animal.kind.toAscii());
            }

            return ascii_grid.toAscii(allocator);
        }
    };

    pub fn cur(self: LevelState) Snapshot {
        return self.states_history.getLast();
    }

    pub fn deinit(self: *LevelState, allocator: std.mem.Allocator) void {
        self.info.walls.deinit(allocator);
        for (self.states_history.items) |snapshot| {
            allocator.free(snapshot.animals);
        }
        self.states_history.deinit(allocator);
    }

    pub fn fromAscii(allocator: std.mem.Allocator, ascii: []const u8) !LevelState {
        const ascii_grid: Grid2D(u8) = try .fromAscii(allocator, ascii);
        defer ascii_grid.deinit(allocator);

        if (true) { // validate input
            for (ascii_grid.data, 0..) |char, k| {
                if (char == '#' or char == '.' or char == '@') continue;
                if (Animal.Kind.fromAscii(char) != null) continue;
                std.log.err("size: {d}x{d}", .{ ascii_grid.size.x, ascii_grid.size.y });
                std.log.err(
                    "at index {d},  pos {d}, {d}: bad ascii char: {s}, {d}",
                    .{ k, @mod(k, ascii_grid.size.x), @divFloor(k, ascii_grid.size.x), @as([]const u8, &.{char}), char },
                );
                return error.BadData;
            }
        }

        var states_history: std.ArrayListUnmanaged(Snapshot) = .empty;

        var initial_state: std.ArrayListUnmanaged(Animal) = .empty;
        var it = ascii_grid.iteratorSigned();
        while (it.next()) |pos| {
            const kind = Animal.Kind.fromAscii(ascii_grid.atSigned(pos)) orelse continue;
            try initial_state.append(allocator, .{
                .kind = kind,
                .pos = pos,
            });
        }
        try states_history.append(allocator, .{ .animals = try initial_state.toOwnedSlice(allocator), .solved = false });

        return .{
            .states_history = states_history,
            .info = .{
                .walls = try ascii_grid.map(allocator, bool, struct {
                    fn anon(c: u8) bool {
                        return c == '#';
                    }
                }.anon),
                .lock = (try ascii_grid.findSingle('@')).cast(isize),
            },
        };
    }
};

const Animal = struct {
    pos: IVec2,
    kind: Kind,

    const Kind = enum {
        beetlekey,
        firefly,
        sundragon,
        catslime,
        ant,
        stonefish,
        iceburd,
        fourheadedfrog,

        fn toAscii(kind: Kind) u8 {
            return switch (kind) {
                .beetlekey => 'K',
                .firefly => 'F',
                .sundragon => 'S',
                .catslime => 'C',
                .ant => 'A',
                .stonefish => 'R',
                .iceburd => 'I',
                .fourheadedfrog => '4',
            };
        }

        fn fromAscii(char: u8) ?Kind {
            inline for (@typeInfo(Kind).@"enum".fields) |field| {
                const cur: Kind = @enumFromInt(field.value);
                if (toAscii(cur) == std.ascii.toUpper(char)) return cur;
            } else return null;
        }
    };
};

pub fn init(
    dst: *GameState,
    runtime_params: kommon.engine.InitRuntimeParamsFor(GameState),
    comptime _: kommon.engine.InitComptimeParamsFor(GameState),
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);

    const gpa = runtime_params.gpa;
    const gl = runtime_params.gl;
    const loaded_images = runtime_params.loaded_images;
    const random_seed = runtime_params.random_seed;
    dst.usual.init(gpa, random_seed, try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}));

    dst.cur_level = 0;
    var it = std.mem.splitSequence(u8, levels_ascii_raw, "\n\n");
    dst.levels = try gpa.alloc(LevelState, kommon.itertools.iteratorLen(it));
    var k: usize = 0;
    while (it.next()) |src| {
        errdefer std.log.err("bad level at index {d}: \n{s}", .{ k - 1, src });
        dst.levels[k] = try .fromAscii(gpa, src);
        k += 1;
    }

    dst.atlas_texture = gl.buildTexture2D(loaded_images.get(.atlas_texture), true);
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

const Drawer = struct {
    batch: Canvas.SpriteBatch,

    const sheet: Canvas.SpriteSheet = .{
        .count = .new(6, 3),
        .margin_px = 1,
        .resolution = .new(6 * 18, 3 * 18),
    };

    pub fn start(canvas: *Canvas, atlas: Gl.Texture) Drawer {
        return .{ .batch = .{
            .canvas = canvas,
            .texture = atlas,
            .sprites = .empty,
        } };
    }

    pub fn end(drawer: *Drawer, camera: Rect) void {
        drawer.batch.draw(camera);
    }

    fn sprite(self: *Drawer, pos: IVec2, index: usize) void {
        self.batch.add(.{
            .point = .{ .pos = pos.tof32() },
            .texcoord = sheet.at(index),
        });
    }

    pub fn wall(self: *Drawer, pos: UVec2) void {
        // self.canvas.fillRect(camera, .{ .top_left = pos.tof32(), .size = .one }, .fromHex("#885522"));
        self.sprite(pos.cast(isize), 0);
    }

    pub fn lock(self: *Drawer, pos: IVec2) void {
        // self.canvas.fillRect(camera, .{ .top_left = pos.tof32(), .size = .one }, .cyan);
        self.sprite(pos, 1);
    }

    pub fn animal(self: *Drawer, animal_: Animal) void {
        self.sprite(animal_.pos, switch (animal_.kind) {
            .catslime => 10,
            .firefly => 3,
            .beetlekey => 6,
            .sundragon => 4,
            .ant => 11,
            .stonefish => 15,
            .iceburd => 12,
            .fourheadedfrog => 8,
        });
    }
};

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);
    const mem = &self.usual.mem;
    // const smooth = &self.usual.smooth;

    if (platform.keyboard.wasPressed(.KeyD) or platform.keyboard.wasPressed(.ArrowRight)) {
        self.cur_level += 1;
        self.cur_level %= self.levels.len;
    }
    if (platform.keyboard.wasPressed(.KeyA) or platform.keyboard.wasPressed(.ArrowLeft)) {
        self.cur_level += self.levels.len - 1;
        self.cur_level %= self.levels.len;
    }

    // TODO(tj6): disable on release
    if (platform.keyboard.wasPressed(.KeyE)) {
        self.editing = !self.editing;
    }

    const level = &self.levels[self.cur_level];

    if (platform.wasKeyPressedOrRetriggered(.KeyZ, 0.1, 0.2) and
        level.states_history.items.len > 1)
    {
        _ = level.states_history.pop();
    }

    if (self.editing) {
        if (platform.keyboard.wasPressed(.KeyI)) {
            const new_walls = try level.info.walls.plusMargin(mem.gpa, 1, false);
            level.info.walls.deinit(mem.gpa);
            level.info.walls = new_walls;
            level.info.lock.addInPlace(.one);
            const new_animals = try mem.gpa.dupe(Animal, level.states_history.getLast().animals);
            for (new_animals) |*a| a.pos.addInPlace(.one);

            for (level.states_history.items) |state| {
                mem.gpa.free(state.animals);
            }
            level.states_history.clearRetainingCapacity();
            level.states_history.appendAssumeCapacity(.{ .solved = false, .animals = new_animals });
        }

        if (platform.keyboard.wasPressed(.KeyO)) {
            var bounds: math.IBounds = .empty;
            bounds.plusTile(level.info.lock);
            if (true) {
                var it = level.info.walls.iterator();
                while (it.next()) |p| {
                    if (level.info.walls.at2(p)) {
                        bounds.plusTile(p.cast(isize));
                    }
                }
            }
            for (level.states_history.getLast().animals) |a| {
                bounds.plusTile(a.pos);
            }

            const new_walls: Grid2D(bool) = try .initFill(mem.gpa, bounds.inner_size, false);
            if (true) {
                var it = level.info.walls.iteratorSigned();
                while (it.next()) |p| {
                    if (level.info.walls.atSigned(p)) {
                        new_walls.setSigned(p.sub(bounds.top_left), true);
                    }
                }
            }
            level.info.walls.deinit(mem.gpa);
            level.info.walls = new_walls;
            level.info.lock.addInPlace(bounds.top_left.neg());
            const new_animals = try mem.gpa.dupe(Animal, level.states_history.getLast().animals);
            for (new_animals) |*a| a.pos.addInPlace(bounds.top_left.neg());

            for (level.states_history.items) |state| {
                mem.gpa.free(state.animals);
            }
            level.states_history.clearRetainingCapacity();
            level.states_history.appendAssumeCapacity(.{ .solved = false, .animals = new_animals });
        }

        if (platform.keyboard.wasPressed(.KeyC)) {
            const str = try level.cur().toAscii(level.info, mem.scratch.allocator());
            std.log.info("\n{s}", .{str});
            platform.setClipboardText(str);
        }

        if (platform.keyboard.wasPressed(.KeyV)) {
            if (platform.getClipboardText()) |ascii| {
                // std.log.debug("acsii len {d}:\n{s}\n\n{any}", .{ ascii.len, ascii, ascii });
                if (LevelState.fromAscii(mem.gpa, ascii)) |new_level| {
                    // std.log.debug("created level", .{});
                    level.deinit(mem.gpa);
                    level.* = new_level;
                } else |err| {
                    std.log.err("bad ascii: {s}", .{@errorName(err)});
                }
            }
        }
    }

    const camera: Rect = .withAspectRatio(.plusMargin3(.plusMargin(.{
        .top_left = .zero,
        .size = level.info.size().tof32(),
    }, 0.5), .bottom, 1.5), platform.aspect_ratio, .grow, .center);
    const mouse = platform.getMouse(camera);
    const active_tile = level.info.walls.tileSignedAt(mouse.cur.position, .{ .top_left = .zero, .size = level.info.size().tof32() });

    if (self.editing and active_tile != null and mouse.wasPressed(.right) and !level.info.lock.equals(active_tile.?)) {
        if (level.cur().animalAt(active_tile.?)) |animal_id| {
            var new_animals: std.ArrayListUnmanaged(Animal) = .fromOwnedSlice(try mem.gpa.dupe(Animal, level.cur().animals));
            _ = new_animals.swapRemove(animal_id);
            try level.states_history.append(mem.gpa, .{ .animals = try new_animals.toOwnedSlice(mem.gpa), .solved = false });
        } else {
            level.info.walls.setSigned(active_tile.?, !level.info.walls.atSigned(active_tile.?));
        }
    }

    if (self.editing and active_tile != null) {
        inline for (@typeInfo(Animal.Kind).@"enum".fields, 1..) |field, digit| {
            comptime assert(digit < 10);
            if (platform.keyboard.wasPressed(.digit(digit))) {
                var new_animals: std.ArrayListUnmanaged(Animal) = .fromOwnedSlice(try mem.gpa.dupe(Animal, level.cur().animals));
                if (level.cur().animalAt(active_tile.?)) |old| {
                    _ = new_animals.swapRemove(old);
                }
                try new_animals.append(mem.gpa, .{ .pos = active_tile.?, .kind = @enumFromInt(field.value) });
                try level.states_history.append(mem.gpa, .{ .animals = try new_animals.toOwnedSlice(mem.gpa), .solved = false });
            }
        }
    }

    var message: ?[]const []const u8 = null;

    if (self.started_grabbing_at == null) {
        platform.setCursor(if (active_tile != null and level.cur().animalAt(active_tile.?) != null)
            .could_grab
        else
            .default);
        if (mouse.wasPressed(.left)) {
            self.started_grabbing_at = active_tile;
        }
    } else {
        platform.setCursor(.grabbing);
        if (!mouse.cur.isDown(.left)) {
            self.started_grabbing_at = null;
        } else if (active_tile != null) {
            if (self.editing) {
                if (!level.info.walls.atSigned(active_tile.?) and
                    level.cur().animalAt(active_tile.?) == null)
                {
                    if (level.cur().animalAt(self.started_grabbing_at.?)) |grabbed| {
                        const new_animals = try mem.gpa.dupe(Animal, level.cur().animals);
                        new_animals[grabbed].pos = active_tile.?;
                        try level.states_history.append(mem.gpa, .{ .animals = new_animals, .solved = false });
                        self.started_grabbing_at = active_tile.?;
                    } else if (level.info.lock.equals(self.started_grabbing_at.?)) {
                        level.info.lock = active_tile.?;
                        self.started_grabbing_at = active_tile.?;
                    }
                }
            } else {
                const result = try level.cur().doMove(level.info, mem.gpa, mem.frame.allocator(), self.started_grabbing_at.?, active_tile.?);
                message = result.message;
                if (result.new_state) |new_state| {
                    try level.states_history.append(mem.gpa, new_state);
                    self.started_grabbing_at = active_tile.?;
                }
            }
        }
    }

    if (self.editing) {
        message = &.{
            "i to increase level size, o to decrease it",
            "c/v to save/load into clipboard, 1-9 to add animals",
            "right click to delete animals and toggle walls",
        };
    }

    platform.gl.clear(.fromHex("#181818"));

    var drawer: Drawer = .start(&self.usual.canvas, self.atlas_texture);

    if (true) { // draw walls
        var it = level.info.walls.iterator();
        while (it.next()) |p| {
            if (level.info.walls.at2(p)) {
                drawer.wall(p);
            }
        }
    }
    if (!level.cur().solved) {
        drawer.lock(level.info.lock);
    }
    for (level.cur().animals) |animal| {
        drawer.animal(animal);
    }

    drawer.end(camera);

    if (message) |msg| {
        try self.usual.canvas.drawTextLines(
            0,
            camera,
            .center,
            .{ .center = level.info.size().tof32().mul(.new(0.5, 1)).addY(1) },
            msg,
            0.5,
            1.1,
            .white,
        );
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
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const IVec2 = math.IVec2;
const Hex = kommon.hex.Hex;
const Grid2D = kommon.Grid2D;
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
