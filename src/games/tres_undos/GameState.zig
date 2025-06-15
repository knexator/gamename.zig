pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "tres_undos",
        .author = "knexator",
        .desired_aspect_ratio = 1152.0 / 648.0,
    },

    .sounds = .{
        .crash = "sounds/crash.wav",
        .step = "sounds/step1.wav",
    },

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
        .player = "images/player.png",
        .tiles = "images/tiles.png",
        .walls = "images/walls.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

const Mem = @FieldType(GameState, "mem");

const COLORS = struct {
    CRATES: [4]FColor = .{
        .fromHex("#CFCFCF"),
        .fromHex("#FF9500"),
        .fromHex("#E74059"),
        .fromHex("#9D15EC"),
    },
}{};

const turn_duration = 0.111;
const transition_duration = turn_duration * 5;
const key_retrigger_time = 0.2;

canvas: Canvas,
mem: struct {
    /// same lifetime as a frame
    frame: std.heap.ArenaAllocator,

    /// same lifetime as a function call
    scratch: std.heap.ArenaAllocator,

    /// same lifetime as a level
    level: std.heap.ArenaAllocator,

    /// same lifetime as the game
    forever: std.heap.ArenaAllocator,

    pub fn init(gpa: std.mem.Allocator) @This() {
        return .{
            .frame = .init(gpa),
            .scratch = .init(gpa),
            .level = .init(gpa),
            .forever = .init(gpa),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.frame.deinit();
        self.scratch.deinit();
        self.level.deinit();
        self.forever.deinit();
    }

    pub fn get(self: *@This(), comptime lifetime: enum { frame, scratch, level, forever }) std.mem.Allocator {
        return switch (lifetime) {
            .frame => self.frame.allocator(),
            .scratch => self.scratch.allocator(),
            .level => self.level.allocator(),
            .forever => self.forever.allocator(),
        };
    }
},

smooth: @import("../akari/GameState.zig").LazyState,

textures: struct {
    tiles: Gl.Texture,
    player: Gl.Texture,
    walls: Gl.Texture,
},

input_queue: kommon.CircularBuffer(Input, 32) = .init,
cur_transition: ?struct {
    t: f32 = -1,
    target_index: usize,
    done: bool = false,
} = null,
cur_level: LevelState,
// TODO: store in level?
cur_level_index: usize = 0,
anim_t: f32 = 1.0,
in_menu: bool = false,
pressed_undos_in_a_row: usize = 0,

const Input = union(enum) {
    dir: IVec2,
    undo: usize,
};

const LevelData = struct {
    ascii: []const u8,
    in_dir: IVec2,
    out_dir: IVec2,
    label: []const u8,
};
const levels_raw: []const LevelData = &.{
    .{ .ascii = 
    \\########.#####
    \\########*#####
    \\########.#..##
    \\########_..1.#
    \\########..1..#
    \\########.#._.#
    \\########.#####
    \\..O...1._#####
    \\##############
    , .in_dir = .e1, .out_dir = .new(0, -1), .label = "1.0" },
    .{ .ascii = 
    \\##########
    \\##....####
    \\#.1.2.####
    \\#..#..__.#
    \\##O#####*#
    \\##.#####.#
    , .in_dir = .new(0, -1), .out_dir = .e2, .label = "1.1" },
};

const LevelState = struct {
    const GeoKind = enum { wall, air, hole };
    geometry: kommon.Grid2D(GeoKind),
    true_timeline_undos: std.ArrayList(usize),
    player: Undoable(struct {
        pos: IVec2,
        in_hole: bool,
        dir: IVec2,
        par: bool,

        pub fn spriteIndex(self: @This()) usize {
            var index: usize = blk: inline for (IVec2.cardinal_directions, &.{ 6, 2, 4, 0 }) |d, k| {
                if (self.dir.equals(d)) break :blk k;
            } else unreachable;
            // if (self.pushing) index += 8;
            if (self.par) index += 1;
            return index;
        }
    }),
    crates: std.ArrayList(Undoable(struct {
        pos: IVec2,
        in_hole: bool,
    })),
    target_player_pos: IVec2,
    in_dir: IVec2,
    out_dir: IVec2,

    pub fn init(dst: *LevelState, mem: *Mem, data: LevelData) !void {
        defer _ = mem.scratch.reset(.retain_capacity);
        _ = mem.level.reset(.retain_capacity);

        dst.true_timeline_undos = .init(mem.level.allocator());

        const chars: kommon.Grid2D(u8) = try .fromAscii(mem.scratch.allocator(), data.ascii);

        const initial_player_pos: IVec2 = (try chars.findSingle('O')).cast(isize);
        dst.player = try .init(mem, &dst.true_timeline_undos, 0, .{ .pos = initial_player_pos, .in_hole = false, .dir = data.in_dir, .par = true });

        dst.target_player_pos = (try chars.findSingle('*')).cast(isize);

        dst.crates = .init(mem.level.allocator());
        var it = chars.iterator();
        while (it.next()) |pos| {
            const char = chars.at2(pos);
            if ('1' <= char and char <= '9') {
                try dst.crates.append(try .init(
                    mem,
                    &dst.true_timeline_undos,
                    char - '1',
                    .{ .pos = pos.cast(isize), .in_hole = false },
                ));
            }
        }

        dst.geometry = try chars.map(mem.level.allocator(), GeoKind, struct {
            pub fn anon(c: u8) GeoKind {
                return switch (c) {
                    '#' => .wall,
                    '_' => .hole,
                    else => .air,
                };
            }
        }.anon);

        dst.in_dir = data.in_dir;
        dst.out_dir = data.out_dir;
    }

    pub fn isWall(self: LevelState, pos: IVec2) bool {
        return !self.geometry.inBoundsSigned(pos) or self.geometry.atSigned(pos) == .wall;
    }

    pub fn anyCrateBlockingAt(self: LevelState, pos: IVec2) bool {
        for (self.crates.items) |crate| {
            if (crate.cur().pos.equals(pos) and !crate.cur().in_hole) return true;
        } else return false;
    }

    pub fn openHoleAt(self: LevelState, pos: IVec2) bool {
        if (self.geometry.atSigned(pos) == .hole) {
            for (self.crates.items) |crate| {
                if (crate.cur().pos.equals(pos) and crate.cur().in_hole)
                    return false;
            } else return true;
        } else return false;
    }

    pub fn doTurn(self: *LevelState, mem: *Mem, input: Input) !union(enum) { usual, wall_crash, won } {
        defer _ = mem.scratch.reset(.retain_capacity);
        try self.true_timeline_undos.append(switch (input) {
            .dir => 0,
            .undo => |k| k,
        });
        switch (input) {
            .dir => |dir| {
                if (self.player.cur().pos.equals(self.target_player_pos) and dir.equals(self.out_dir)) {
                    try self.player.setCurrent(.{
                        .pos = self.player.cur().pos.add(dir),
                        .dir = dir,
                        .par = !self.player.cur().par,
                        .in_hole = false,
                    });
                    return .won;
                }

                if (self.player.cur().in_hole) return .wall_crash;

                const new_player_pos = self.player.cur().pos.add(dir);
                if (self.isWall(new_player_pos)) return .wall_crash;

                var pushing_crates: std.ArrayList(usize) = .init(mem.scratch.allocator());
                for (self.crates.items, 0..) |crate, k| {
                    if (crate.cur().pos.equals(new_player_pos) and !crate.cur().in_hole) try pushing_crates.append(k);
                }
                if (pushing_crates.items.len > 0) {
                    const new_crates_pos = new_player_pos.add(dir);
                    if (self.isWall(new_crates_pos) or self.anyCrateBlockingAt(new_crates_pos)) {
                        return .wall_crash;
                    }
                    const fell_on_hole = self.openHoleAt(new_crates_pos);
                    for (pushing_crates.items) |pushed_crate_index| {
                        var cur = self.crates.items[pushed_crate_index].cur();
                        cur.pos = new_crates_pos;
                        cur.in_hole = fell_on_hole;
                        try self.crates.items[pushed_crate_index].setCurrent(cur);
                    }
                }

                try self.player.setCurrent(.{
                    .pos = new_player_pos,
                    .dir = dir,
                    .par = !self.player.cur().par,
                    .in_hole = self.openHoleAt(new_player_pos),
                });

                return .usual;
            },
            .undo => |k| {
                try self.player.undo(k);
                for (self.crates.items) |*crate| {
                    try crate.undo(k);
                }
                return .usual;
            },
        }
    }

    pub fn draw(self: *LevelState, anim_t: f32, mem: *Mem, camera: Rect, canvas: *Canvas, textures: @FieldType(GameState, "textures")) void {
        const tiles: Canvas.SpriteSheet = .{
            .count = .new(3, 4),
            .margin_px = 2,
            .resolution = textures.tiles.resolution,
        };
        const players: Canvas.SpriteSheet = .{
            .count = .new(4, 4),
            .margin_px = 0,
            .resolution = textures.player.resolution,
        };
        const walls: Canvas.SpriteSheet = .{
            .count = .new(4, 7),
            .margin_px = 2,
            .resolution = textures.walls.resolution,
        };
        _ = mem;
        var layers = .{
            .basement = canvas.spriteBatch(textures.tiles),
            .basement_player = canvas.spriteBatch(textures.player),
            .main = canvas.spriteBatch(textures.tiles),
            .main_player = canvas.spriteBatch(textures.player),
            .walls = canvas.spriteBatch(textures.walls),
            .gradients = canvas.spriteBatch(textures.walls),
        };
        var it = self.geometry.iterator();
        while (it.next()) |pos| {
            const tile = self.geometry.at2(pos);
            switch (tile) {
                .air => {
                    layers.main.add(.{
                        .point = .{ .pos = pos.tof32() },
                        .texcoord = tiles.at(0),
                    });
                },
                .hole => {
                    layers.main.add(.{
                        .point = .{ .pos = pos.tof32() },
                        .texcoord = tiles.at(1),
                    });
                    layers.basement.add(.{
                        .point = .{ .pos = pos.tof32() },
                        .texcoord = tiles.at(3),
                    });
                },
                .wall => {},
            }
        }
        for (self.crates.items) |crate| {
            const in_hole = crate.cur().in_hole and (anim_t == 1.0 or crate.prev().in_hole);
            const layer = if (in_hole) &layers.basement else &layers.main;
            const color = COLORS.CRATES[crate.immune_to];
            layer.add(.{
                .point = .{ .pos = if (in_hole) crate.cur().pos.tof32() else crate.curPos(anim_t) },
                .texcoord = tiles.at(6 + crate.immune_to),
                .tint = color,
            });
        }
        // first turn: enter level animation
        if (self.true_timeline_undos.items.len == 0) {
            layers.main_player.add(.{
                .point = .{ .pos = self.player.cur().pos.tof32().sub(self.in_dir.tof32().scale(1 - anim_t)) },
                .texcoord = players.at(self.player.cur().spriteIndex()),
            });
        } else {
            const in_hole = self.player.cur().in_hole and anim_t == 1.0;
            const layer = if (in_hole) &layers.basement_player else &layers.main_player;
            layer.add(.{
                .point = .{ .pos = if (in_hole) self.player.cur().pos.tof32() else self.player.curPos(anim_t) },
                .texcoord = players.at(self.player.cur().spriteIndex()),
            });
        }

        for (layers.basement.sprites.items) |*spr| spr.tint = spr.tint.scaleRGB(0.4);
        for (layers.basement_player.sprites.items) |*spr| spr.tint = spr.tint.scaleRGB(0.4);

        layers.basement.draw(camera);
        layers.basement_player.draw(camera);
        layers.main.draw(camera);
        layers.main_player.draw(camera);

        for (0..self.geometry.size.y + 1) |j| {
            for (0..self.geometry.size.x + 1) |i| {
                const p: IVec2 = UVec2.new(i, j).cast(isize);
                const Asdf = enum {
                    wall,
                    air,
                    outside,

                    pub fn from(geo: ?GeoKind) @This() {
                        return if (geo) |v| switch (v) {
                            .wall => .wall,
                            else => .air,
                        } else .outside;
                    }

                    pub fn corner(str: *const [4:0]u8) [4]@This() {
                        var res: [4]@This() = undefined;
                        for (str, &res) |c, *dst| {
                            dst.* = switch (c) {
                                '#' => .wall,
                                '.' => .air,
                                ',' => .outside,
                                else => unreachable,
                            };
                        }
                        return res;
                    }
                };
                const tl: Asdf = .from(self.geometry.atSignedSafe(p.sub(.one)));
                const tr: Asdf = .from(self.geometry.atSignedSafe(p.sub(.e2)));
                const bl: Asdf = .from(self.geometry.atSignedSafe(p.sub(.e1)));
                const br: Asdf = .from(self.geometry.atSignedSafe(p.sub(.zero)));
                const wall_sprites = funk.map(Asdf.corner, comptime &.{
                    "###.", "##.#", ".#.#", "..##",
                    "#.##", ".###", "##..", "#.#.",
                    "...#", "..#.", "#..#", ".##.",
                    ".#..", "#...", ",,,,", ",,,,",
                    "#.,,", ".#,,", "#,.,", ",#,.",
                    ",,#.", ",,.#", ".,#,", ",.,#",
                    "..,,", ",,..", ".,.,", ",.,.",
                });
                inline for (wall_sprites, 0..) |combo, k| {
                    if (std.meta.eql(combo, .{ tl, tr, bl, br })) {
                        var pos = p.tof32().sub(.half);
                        if (bl == .outside and br == .outside) {
                            pos.y -= 0.5;
                        } else if (tr == .outside and tr == .outside) {
                            pos.y += 0.5;
                        } else if (tr == .outside and br == .outside) {
                            pos.x -= 0.5;
                        } else if (tl == .outside and bl == .outside) {
                            pos.x += 0.5;
                        }
                        layers.walls.add(.{
                            .point = .{ .pos = pos },
                            .texcoord = walls.at(k),
                        });
                        break;
                    }
                }
            }
        }
        layers.walls.draw(camera);
    }
};

pub fn Undoable(T: type) type {
    return struct {
        const Self = @This();

        true_timeline_undos: *const std.ArrayList(usize),
        true_values: std.ArrayList(T),
        immune_to: usize,

        pub fn init(mem: *Mem, true_timeline_undos: *const std.ArrayList(usize), immune_to: usize, initial_value: T) !Self {
            var true_values: std.ArrayList(T) = try .initCapacity(mem.level.allocator(), 64);
            true_values.appendAssumeCapacity(initial_value);
            return .{
                .true_values = true_values,
                .immune_to = immune_to,
                .true_timeline_undos = true_timeline_undos,
            };
        }

        pub fn setCurrent(self: *Self, value: T) !void {
            const last_known = self.cur();
            const real_tick = self.true_timeline_undos.items.len;
            while (self.true_values.items.len < real_tick) {
                try self.true_values.append(last_known);
            }
            try self.true_values.append(value);
        }

        pub fn cur(self: Self) T {
            return self.at(self.true_timeline_undos.items.len);
        }

        pub fn prev(self: Self) T {
            if (self.true_timeline_undos.items.len == 0) return self.cur();
            return self.at(self.true_timeline_undos.items.len - 1);
        }

        pub fn curPos(self: Self, anim_t: f32) Vec2 {
            assert(@hasField(T, "pos"));
            assert(@FieldType(T, "pos") == IVec2);
            if (anim_t == 1.0) {
                return self.cur().pos.tof32();
            } else {
                return .lerp(self.prev().pos.tof32(), self.cur().pos.tof32(), anim_t);
            }
        }

        pub fn at(self: Self, tick: usize) T {
            if (tick < self.true_values.items.len) {
                return self.true_values.items[tick];
            } else {
                return self.true_values.getLast();
            }
        }

        pub fn undo(self: *Self, undo_level: usize) !void {
            assert(undo_level > 0);
            assert(self.true_values.items.len > 0);
            if (self.immune_to >= undo_level) {
                try self.setCurrent(self.cur());
            } else {
                const tick = get_original_tick(
                    self.true_values.items.len,
                    self.immune_to,
                    self.true_timeline_undos.items,
                );
                try self.setCurrent(self.at(tick));
            }
        }

        fn get_original_tick(tick: usize, immune_to: usize, true_timeline_undos: []const usize) usize {
            assert(tick <= true_timeline_undos.len);
            if (true_timeline_undos[tick - 1] <= immune_to) {
                return tick;
            } else {
                const travel_depth = true_timeline_undos[tick - 1];
                var counter: usize = 1;
                var res = tick - 1;
                while (counter > 0 and res > 0) {
                    const cur_depth = true_timeline_undos[res - 1];
                    if (cur_depth == travel_depth) {
                        counter += 1;
                        res -= 1;
                    } else if (cur_depth < travel_depth) {
                        counter -= 1;
                        res -= 1;
                    } else {
                        res = get_original_tick(res, immune_to, true_timeline_undos);
                    }
                }
                return res;
            }
        }
    };
}

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);

    dst.mem = .init(gpa);

    dst.textures = .{
        .tiles = gl.buildTexture2D(loaded_images.get(.tiles), true),
        .player = gl.buildTexture2D(loaded_images.get(.player), true),
        .walls = gl.buildTexture2D(loaded_images.get(.walls), true),
    };
    dst.canvas = try .init(
        gl,
        gpa,
        &.{@embedFile("../../fonts/Arial.json")},
        &.{loaded_images.get(.arial_atlas)},
    );
    dst.smooth = .init(dst.mem.forever.allocator());

    assert(dst.cur_level_index == 0);
    try dst.cur_level.init(&dst.mem, levels_raw[0]);
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    self.mem.deinit();
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    _ = self.mem.frame.reset(.retain_capacity);

    if (platform.keyboard.wasPressed(.Escape)) self.in_menu = !self.in_menu;

    if (self.cur_transition) |*transition| {
        math.towards(&transition.t, 1, platform.delta_seconds * 2.0 / transition_duration);
        if (!transition.done and transition.t >= 0) {
            try self.cur_level.init(&self.mem, levels_raw[transition.target_index]);
            self.cur_level_index = transition.target_index;
            transition.done = true;
            self.anim_t = 0;
            self.in_menu = false;
            self.pressed_undos_in_a_row = 0;
        }
        if (transition.done and transition.t >= 1) {
            self.cur_transition = null;
        }
    }

    if (self.in_menu) {
        const ui_cam: Rect = Rect.fromCenterAndSize(.zero, Vec2.new(6, 3).scale(1.75)).withAspectRatio(platform.aspect_ratio, .grow, .center);
        const asdf: kommon.Grid2D(void) = try .initUndefined(self.mem.frame.allocator(), .new(6, 3));
        var buttons: std.ArrayList(Rect) = .init(self.mem.frame.allocator());
        var it = asdf.iterator();
        while (it.next()) |p| {
            try buttons.append(asdf.getTileRect(.fromCenterAndSize(.zero, .new(6, 3)), p).plusMargin(-0.05));
        }

        const mouse = platform.getMouse(ui_cam);
        const hovered: ?usize = blk: for (buttons.items, 0..) |r, k| {
            if (r.contains(mouse.cur.position)) break :blk k;
        } else null;

        try self.drawGame(platform);
        self.canvas.fillRect(.unit, .unit, FColor.black.withAlpha(0.5));

        self.canvas.fillRect(
            ui_cam,
            Rect.fromCenterAndSize(.zero, .new(6, 3)).plusMargin(0.05),
            .fromHex("#383F69"),
        );
        for (buttons.items, 0..) |r, k| {
            const is_hovered = k == hovered;
            const is_active = k == self.cur_level_index;
            defer self.canvas.borderRect(ui_cam, r, 0.025, .inner, .fromHex("#565EA1"));
            defer if (is_active) self.canvas.borderRect(ui_cam, r, 0.15, .inner, .fromHex("#7988C0"));
            self.canvas.fillRect(ui_cam, r, try self.smooth.fcolor(
                .fromFormat("bg {d}", .{k}),
                if (is_hovered) .fromHex("#7988C0") else .fromHex("#4F69BA"),
            ));
            try self.canvas.drawTextLine(
                0,
                ui_cam,
                .{ .center = r.getCenter() },
                (kommon.safeAt(LevelData, levels_raw, k) orelse std.mem.zeroInit(LevelData, .{ .label = "???" })).label,
                0.3,
                try self.smooth.fcolor(.fromFormat("text {d}", .{k}), if (is_hovered) .white else .black),
            );
        }

        if (mouse.wasPressed(.left) and hovered != null) {
            self.cur_transition = .{ .target_index = hovered.? };
        }
    } else {
        try self.updateGame(platform);
        try self.drawGame(platform);
    }

    if (self.cur_transition) |transition| {
        const dir = levels_raw[transition.target_index].in_dir;
        if (transition.t <= 0) {
            const t = math.remapClamped(transition.t, -1, 0, 0, 1);
            self.canvas.fillRect(
                .fromCenterAndSize(.zero, .one),
                .fromCenterAndSize(dir.tof32().neg().scale(1 - t), .one),
                .fromHex("#383D68"),
            );
        } else {
            const t = math.remap(transition.t, 0, 1, 0, 1);
            self.canvas.fillRect(
                .fromCenterAndSize(.zero, .one),
                .fromCenterAndSize(dir.tof32().scale(t), .one),
                .fromHex("#383D68"),
            );
        }
    }

    return false;
}

fn updateGame(self: *GameState, platform: PlatformGives) !void {
    // TODO: getKeyRetriggerTime
    for ([_]std.meta.Tuple(&.{ KeyboardButton, IVec2 }){
        .{ .KeyD, .new(1, 0) },  .{ .ArrowRight, .new(1, 0) },
        .{ .KeyA, .new(-1, 0) }, .{ .ArrowLeft, .new(-1, 0) },
        .{ .KeyW, .new(0, -1) }, .{ .ArrowUp, .new(0, -1) },
        .{ .KeyS, .new(0, 1) },  .{ .ArrowDown, .new(0, 1) },
    }) |binding| {
        const key, const dir = binding;
        if (platform.wasKeyPressedOrRetriggered(key, key_retrigger_time)) {
            platform.setKeyChanged(key);
            try self.input_queue.append(.{ .dir = dir });
        }
    }

    for ([_]KeyboardButton{ .KeyZ, .KeyX, .KeyC, .KeyV }, 0..) |undo_key, undo_level| {
        if (platform.wasKeyPressedOrRetriggered(
            undo_key,
            key_retrigger_time *
                if (self.pressed_undos_in_a_row < 2)
                    @as(f32, 1.2)
                else
                    @as(f32, 0.5),
        )) {
            try self.input_queue.append(.{ .undo = undo_level + 1 });
            self.pressed_undos_in_a_row += 1;
        } else if (platform.keyboard.wasReleased(undo_key)) {
            self.pressed_undos_in_a_row = 0;
        }
    }

    if (self.anim_t == 1) {
        if (self.input_queue.popFirst()) |input| {
            switch (try self.cur_level.doTurn(&self.mem, input)) {
                .usual => {
                    platform.sound_queue.insert(.step);
                    self.anim_t = 0;
                },
                .wall_crash => {
                    platform.sound_queue.insert(.crash);
                    // hacky, should be on the levelstate itself
                    _ = self.cur_level.true_timeline_undos.pop();
                },
                .won => {
                    if (self.cur_level_index + 1 < levels_raw.len) {
                        self.cur_transition = .{ .target_index = self.cur_level_index + 1 };
                    }
                    self.anim_t = 0;
                },
            }
        }
    } else {
        math.towards(&self.anim_t, 1.0, platform.delta_seconds / turn_duration);
    }
}

fn drawGame(self: *GameState, platform: PlatformGives) !void {
    const camera: Rect = Rect.withAspectRatio(
        .{ .top_left = .zero, .size = self.cur_level.geometry.size.tof32() },
        platform.aspect_ratio,
        .grow,
        .center,
    );
    // const mouse = platform.getMouse(camera);
    platform.gl.clear(.fromHex("#4E4E4E"));
    self.cur_level.draw(self.anim_t, &self.mem, camera, &self.canvas, self.textures);

    if (self.cur_level_index == 0) {
        // TODO: multiline
        try self.canvas.drawTextLine(0, camera, .{ .center = .new(11.5, 7) }, "WASD / Arrow", 0.5, .fromHex("#7988C0"));
        try self.canvas.drawTextLine(0, camera, .{ .center = .new(11.5, 7.5) }, "Keys to Move", 0.5, .fromHex("#7988C0"));

        try self.canvas.drawTextLine(0, camera, .{ .center = .new(11.5, 8.2) }, "Z to Undo", 0.5, .fromHex("#7988C0"));
    }
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
