pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};
pub const tracy = @import("tracy");

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "tj6",
        .author = "knexator",
        .desired_aspect_ratio = 16.0 / 9.0,
    },

    .sounds = .{
        // .firefly_1 = "assets/sfx/sfx_Firefly1.wav",
        // .firefly_2 = "assets/sfx/sfx_Firefly2.wav",
        .poweroff_1 = "assets/sfx/sfx_PwrOff1.wav",
        .poweroff_2 = "assets/sfx/sfx_PwrOff2.wav",
        .poweroff_3 = "assets/sfx/sfx_PwrOff3.wav",
        .frog_1 = "assets/sfx/sfx_Frog1.wav",
        .frog_2 = "assets/sfx/sfx_Frog2.wav",
        .frog_3 = "assets/sfx/sfx_Frog3.wav",
        .frog_4 = "assets/sfx/sfx_Frog4.wav",
        .ice_1 = "assets/sfx/sfx_Ice1.wav",
        .ice_2 = "assets/sfx/sfx_Ice2.wav",
        .slimecat_1 = "assets/sfx/sfx_SlimeCat1.wav",
        .slimecat_2 = "assets/sfx/sfx_SlimeCat2.wav",
        .slimecat_3 = "assets/sfx/sfx_SlimeCat3.wav",
        .step_1 = "assets/sfx/sfx_Step1.wav",
        .step_2 = "assets/sfx/sfx_Step2.wav",
        .step_3 = "assets/sfx/sfx_Step3.wav",
        .step_4 = "assets/sfx/sfx_Step4.wav",
        .stone_1 = "assets/sfx/sfx_Stone1.wav",
        .stone_2 = "assets/sfx/sfx_Stone2.wav",
        .unlock = "assets/sfx/sfx_Unlock.wav",
    },

    .loops = if (@import("builtin").target.ofmt == .wasm) .{
        .music = "assets/music/mx_BGM_Full.mp3",
        .muffled = "assets/music/mx_BGM_Full_Muffled.mp3",
        // TODO: find out why is this needed
        // .alarm = "assets/music/mx_BGM_Full.mp3",
    } else .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
        .atlas_texture = "assets/spritesheet.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

usual: kommon.Usual,
levels: []LevelState,
cur_level: usize = 0,
level_change_t: f32 = 1,
started_grabbing_at: ?IVec2 = null,
atlas_texture: Gl.Texture,
anim_t: f32 = 1,

keyboard_wasd_offset: IVec2 = .zero,

time_in_youwin: f32 = 0,

prev_active_tile: ?IVec2 = null,
prev_active_tile_time: f32 = 0,

editing: bool = false,

started_playing: bool = false,
show_final_message: bool = false,

in_menu: bool = true,
in_menu_t: f32 = 1,

sfx_muted: bool = false,
music_muted: bool = false,

hovered: ?usize = null,
active: ?usize = null,
buttons: [8]Button = .{
    .{ .text = "Continue", .action = .continue_game, .layer = .menu },
    .{ .text = "<", .action = .prev_level, .layer = .menu },
    .{ .text = ">", .action = .next_level, .layer = .menu },
    .{ .text = "SFX", .action = .toggle_sfx, .layer = .menu },
    .{ .text = "Music", .action = .toggle_music, .layer = .menu },

    .{ .text = "Menu", .action = .exit, .layer = .game },
    .{ .text = "Reset", .action = .reset, .layer = .game },
    .{ .text = "Undo", .action = .undo, .layer = .game },
},

const Button = struct {
    active_t: f32 = 0,
    hovered_t: f32 = 0,
    enabled_t: f32 = 0,
    latched_t: f32 = 0,
    text: []const u8,
    rect: Rect = undefined,
    enabled: bool = undefined,
    exists: bool = true,
    latched: bool = false,
    action: Action,
    layer: enum { game, menu },

    const Action = enum {
        // in-game
        undo,
        reset,
        exit,
        // main menu
        continue_game,
        prev_level,
        next_level,
        toggle_sfx,
        toggle_music,
    };

    pub fn draw(button: Button, canvas: *Canvas, camera: Rect) !void {
        if (!button.exists) return;
        canvas.fillRect(
            camera,
            button.rect
                .plusMargin(button.hovered_t * 0.1)
                .plusMargin2(Vec2.new(1, -1).scale(0.15 * button.active_t)),
            .lerp(
                .fromHex("#2b2f3a"),
                .lerp(
                    .fromHex("#3d4860"),
                    .fromHex("#63749b"),
                    button.hovered_t,
                ),
                button.enabled_t,
            ),
        );

        try canvas.drawText(
            0,
            camera,
            button.text,
            .centeredAt(button.rect.get(.center)),
            0.5 * button.rect.size.y,
            .lerp(.gray(0.5), .white, button.enabled_t),
        );

        canvas.line(camera, &.{
            button.rect.get(.top_right), button.rect.get(.bottom_left),
        }, 0.1, .withAlpha(.red, button.latched_t));
    }
};

const levels_ascii_raw: []const u8 = @embedFile("levels.txt");

fn randomSound(options: []const std.meta.FieldEnum(@TypeOf(stuff.sounds))) std.meta.FieldEnum(@TypeOf(stuff.sounds)) {
    const S = struct {
        var counter: usize = 0;
    };
    defer S.counter += 1;
    return options[@mod(S.counter, options.len)];
}

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

test "multipush" {
    try testMove(std.testing.allocator,
        \\KF..@
    ,
        \\.KF.@
    , .new(0, 0), .new(1, 0));
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
        \\SF.....
    ,
        \\CC.....
        \\SF....@
        \\.......
    , .new(1, 1), .new(0, -1));

    try testMove(std.testing.allocator,
        \\.......
        \\CC#...@
        \\SFK....
    ,
        \\CC.....
        \\SF#...@
        \\..K....
    , .new(1, 1), .new(0, -1));

    try testMove(std.testing.allocator,
        \\.......
        \\CC#...@
        \\SFK....
    ,
        \\CC.....
        \\SF#...@
        \\..K....
    , .new(0, 1), .new(0, -1));

    try testMove(std.testing.allocator,
        \\....
        \\@...
        \\.CC#
        \\.FSK
    ,
        \\....
        \\@CC.
        \\.FS#
        \\...K
    , .new(1, 2), .new(0, -1));

    try testMove(std.testing.allocator,
        \\...
        \\@..
        \\.CC
        \\.FS
    ,
        \\...
        \\@CC
        \\.FS
        \\...
    , .new(2, 2), .new(0, -1));

    try testMove(std.testing.allocator,
        \\@.......
        \\...#....
        \\.CCCCC..
        \\.FSKKK..
    ,
        \\@.......
        \\.CC#....
        \\.FSCCC..
        \\...KKK..
    , .new(2, 2), .new(0, -1));

    try testMove(std.testing.allocator,
        \\@.......
        \\....#...
        \\.CCCCC..
        \\.FSKKK..
    ,
        \\@.......
        \\.CCC#...
        \\.FSKCC..
        \\....KK..
    , .new(1, 2), .new(0, -1));

    try testMove(std.testing.allocator,
        \\@.......
        \\..#.....
        \\.CCCCC..
        \\.FSKKK..
    ,
        \\@.......
        \\..#CCC..
        \\.CCKKK..
        \\.FS.....
    , .new(5, 2), .new(0, -1));
}

test "catslime multipush" {
    try testMove(std.testing.allocator,
        \\CK..@
        \\KK...
    ,
        \\.CK.@
        \\.KK..
    , .new(0, 0), .new(1, 0));
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
        is_automove: bool = false,
        is_lost: bool = false,
        any_eaten_ever: bool = false,
        any_eaten_now: bool = false,
        started_frog_attack: bool = false,
        was_reset: bool = false,
        message: ?[]const []const u8 = null,

        fn solved(self: Snapshot, info: Constant) bool {
            if (self.is_lost) return false;
            for (self.animals) |a| {
                if (a.kind == .beetlekey and a.pos.equals(info.lock)) return true;
                if (a.kind == .beetlekey and info.walls.inEdge(a.pos)) return true;
            } else return false;
        }

        fn animalAt(self: Snapshot, pos: IVec2) ?usize {
            return animalAt2(self.animals, pos);
        }

        fn animalAt2(animals: []const Animal, pos: IVec2) ?usize {
            for (animals, 0..) |animal, k| {
                if (animal.pos.equals(pos)) return k;
            } else return null;
        }

        fn surroundingAnimals(self: Snapshot, pos: IVec2) std.BoundedArray(Animal.Kind, 4) {
            return surroundingAnimals2(self.animals, pos);
        }

        fn surroundingAnimals2(animals: []const Animal, pos: IVec2) std.BoundedArray(Animal.Kind, 4) {
            var result: std.BoundedArray(Animal.Kind, 4) = .{};
            for (IVec2.cardinal_directions) |dir| {
                if (animalAt2(animals, pos.add(dir))) |a| result.appendAssumeCapacity(animals[a].kind);
            }
            return result;
        }

        fn distToBug(animals: []const Animal, info: Constant, pos: IVec2, dir: IVec2) ?isize {
            var dist: isize = 0;

            var target = pos.add(dir);

            while (true) {
                if (info.lock.equals(target) or (info.walls.atSignedSafe(target) orelse true)) {
                    return null;
                }

                if (animalAt2(animals, target)) |animal| {
                    const kind = animals[animal].kind;
                    if (kind.isBug()) {
                        return dist;
                    } else if (!kind.isTongue()) {
                        return null;
                    }
                }

                target = target.add(dir);
                dist += 1;
            }

            return dist;
        }

        fn tongueLen(animals: []const Animal, pos: IVec2, dir: IVec2) isize {
            var dist: isize = 0;

            var target = pos.add(dir);

            while (true) {
                const valid = if (animalAt2(animals, target)) |animal|
                    animals[animal].kind == Animal.Kind.tongue(dir)
                else
                    false;

                if (!valid) return dist;

                target = target.add(dir);
                dist += 1;
            }

            return dist;
        }

        fn autoMove(old_state: Snapshot, info: Constant, allocator: std.mem.Allocator, scratch: std.mem.Allocator) !?Snapshot {
            var new_animals: std.ArrayListUnmanaged(Animal) = try .initCapacity(allocator, old_state.animals.len);

            var any_changes = false;
            var any_eaten = false;
            var started_frog_attack = false;

            for (old_state.animals) |animal| {
                if (animal.prev_move == .eaten) {
                    any_changes = true;
                    any_eaten = true;
                } else {
                    var a = animal;
                    a.prev_move = .nothing;
                    new_animals.appendAssumeCapacity(a);
                }
            }

            _ = scratch;

            var frog_index: usize = 0;
            while (frog_index < new_animals.items.len) : (frog_index += 1) {
                const frog = new_animals.items[frog_index];
                if (frog.kind != .fourheadedfrog) continue;

                for (IVec2.cardinal_directions) |dir| {
                    const tongue_len = tongueLen(new_animals.items, frog.pos, dir);
                    const maybe_bug_dist = distToBug(new_animals.items, info, frog.pos, dir);

                    if (maybe_bug_dist) |bug_dist| {
                        any_changes = true;
                        if (tongue_len < bug_dist) {
                            if (tongue_len == 0) started_frog_attack = true;
                            try new_animals.append(allocator, .{ .kind = .tongue(dir), .pos = frog.pos.add(dir.scale(tongue_len + 1)) });
                        } else if (bug_dist == 0) {
                            assert(tongue_len == bug_dist);
                            new_animals.items[animalAt2(new_animals.items, frog.pos.add(dir)).?].prev_move = .{ .eaten = frog.pos };
                        } else {
                            assert(tongue_len == bug_dist);
                            _ = new_animals.swapRemove(animalAt2(new_animals.items, frog.pos.add(dir.scale(tongue_len))).?);
                            const bug = &new_animals.items[animalAt2(new_animals.items, frog.pos.add(dir.scale(bug_dist + 1))).?];
                            bug.prev_move = .{ .moved = bug.pos };
                            bug.pos.addInPlace(dir.neg());
                        }
                    } else if (tongue_len > 0) {
                        any_changes = true;
                        _ = new_animals.swapRemove(animalAt2(new_animals.items, frog.pos.add(dir.scale(tongue_len))).?);
                    }
                }
            }

            if (true) { // validate rule: all sundragons next to a firefly
                for (new_animals.items) |*animal| {
                    if (animal.kind != .sundragon) continue;
                    const any_firefly = for (surroundingAnimals2(new_animals.items, animal.pos).constSlice()) |k| {
                        if (k == .firefly) break true;
                    } else false;
                    if (!any_firefly) {
                        animal.alternative = true;
                        // sounds.insert(randomSound(&.{ .poweroff_1, .poweroff_2, .poweroff_3 }));
                    }
                }
            }

            if (true) { // validate rule: no iceburds next to a firefly
                for (new_animals.items) |*animal| {
                    if (animal.kind != .iceburd) continue;
                    const any_firefly = for (surroundingAnimals2(new_animals.items, animal.pos).constSlice()) |k| {
                        if (k == .firefly) break true;
                    } else false;
                    if (any_firefly) {
                        animal.alternative = true;
                        // sounds.insert(randomSound(&.{ .ice_1, .ice_2 }));
                    }
                }
            }

            if (any_changes) {
                if (true) { // slimed
                    for (new_animals.items) |*animal| {
                        if (animal.kind.isTongue()) continue;
                        const any_cat = for (surroundingAnimals2(new_animals.items, animal.pos).constSlice()) |k| {
                            if (k == .catslime) break true;
                        } else false;
                        animal.slimed = .next(animal.slimed, any_cat);
                    }
                }

                const any_eaten_ever = old_state.any_eaten_ever or any_eaten;
                return .{
                    .animals = try new_animals.toOwnedSlice(allocator),
                    .is_automove = true,
                    .any_eaten_ever = any_eaten_ever,
                    .any_eaten_now = any_eaten,
                    .message = if (any_eaten_ever) &.{"Frog ate a bug!"} else null,
                    .is_lost = old_state.is_lost or any_eaten_ever,
                    .started_frog_attack = started_frog_attack,
                };
            } else {
                new_animals.deinit(allocator);
                return null;
            }
        }

        /// Floods, into `out`, the set of animals dragged when `grabbed` moves
        /// in `dir`. From a member `a`, an adjacent animal `b` joins when:
        /// either is a catslime (sticky in any direction), or `b` is directly in front of `a`.
        /// When `allowed` is given, only flagged animals can join or be traversed through.
        fn findDragGroup(
            animals: []const Animal,
            grabbed: usize,
            dir: IVec2,
            allowed: ?[]const bool,
            scratch: std.mem.Allocator,
            out: []bool,
        ) !void {
            @memset(out, false);
            if (allowed) |a| {
                if (!a[grabbed]) return;
            }
            out[grabbed] = true;
            var stack: std.ArrayListUnmanaged(usize) = .empty;
            try stack.append(scratch, grabbed);
            var cursor: usize = 0;
            while (cursor < stack.items.len) : (cursor += 1) {
                const a = stack.items[cursor];
                for (IVec2.cardinal_directions) |d| {
                    const b = animalAt2(animals, animals[a].pos.add(d)) orelse continue;
                    if (out[b]) continue;
                    if (allowed) |al| {
                        if (!al[b]) continue;
                    }
                    const catslime = animals[a].kind == .catslime or animals[b].kind == .catslime;
                    const shoved = d.equals(dir);
                    if (catslime or shoved) {
                        out[b] = true;
                        try stack.append(scratch, b);
                    }
                }
            }
        }

        fn doMove(old_state: Snapshot, info: Constant, allocator: std.mem.Allocator, scratch: std.mem.Allocator, old_pos: IVec2, new_pos: IVec2) !struct {
            message: ?[]const []const u8,
            new_state: ?Snapshot,
            sounds: std.EnumSet(std.meta.FieldEnum(@TypeOf(stuff.sounds))) = .initEmpty(),

            const empty: @This() = .{ .message = null, .new_state = null };
        } {
            if (old_pos.sub(new_pos).taxiMag() != 1) return .empty;
            if (info.walls.atSigned(new_pos)) return .empty;
            const grabbed = old_state.animalAt(old_pos) orelse return .empty;
            const animal_kind = old_state.animals[grabbed].kind;
            if (animal_kind == .stonefish) {
                return .{ .new_state = null, .message = &.{"Too heavy to move"}, .sounds = .initOne(randomSound(&.{ .stone_1, .stone_2 })) };
            }

            const dir = new_pos.sub(old_pos);
            const animals = old_state.animals;

            // all animals connected to the current one
            const in_group = try scratch.alloc(bool, animals.len);
            try findDragGroup(animals, grabbed, dir, null, scratch, in_group);

            // remove any animals blocked by a wall, and those reachable through it
            // iterates to a fixpoint, always terminates
            const moving = try scratch.dupe(bool, in_group);
            const reachable = try scratch.alloc(bool, animals.len);
            while (true) {
                var changed = false;

                // A member blocked by a wall or a stationary animal stops.
                for (animals, 0..) |animal, idx| {
                    if (!moving[idx]) continue;
                    const dest = animal.pos.add(dir);
                    const blocked_by_wall = info.walls.atSignedSafe(dest) orelse true;
                    const blocked_by_lock = info.lock.equals(dest) and animal.kind != .beetlekey;
                    const blocked_by_animal = if (animalAt2(animals, dest)) |j|
                        !moving[j]
                    else
                        false;

                    if (blocked_by_wall or blocked_by_lock or blocked_by_animal) {
                        moving[idx] = false;
                        changed = true;
                    }
                }

                // Only animals still reachable from the grabbed one, through
                // members that are themselves moving, keep being dragged.
                try findDragGroup(animals, grabbed, dir, moving, scratch, reachable);
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

            var any_slime_moved = false;
            const new_animals = try allocator.dupe(Animal, animals);
            for (new_animals) |*a| a.prev_move = .nothing;
            for (new_animals, 0..) |*animal, idx| {
                if (moving[idx]) {
                    animal.prev_move = .{ .moved = animal.pos };
                    animal.pos.addInPlace(dir);
                    if (animal.kind == .catslime) any_slime_moved = true;
                }
            }

            var result: Snapshot = .{ .animals = new_animals };
            var sounds: std.EnumSet(std.meta.FieldEnum(@TypeOf(stuff.sounds))) = .initOne(if (any_slime_moved)
                randomSound(&.{ .slimecat_1, .slimecat_2, .slimecat_3 })
            else
                randomSound(&.{ .step_1, .step_2, .step_3, .step_4 }));

            if (true) { // validate rule: all sundragons next to a firefly
                for (new_animals) |*animal| {
                    if (animal.kind != .sundragon) continue;
                    const any_firefly = for (result.surroundingAnimals(animal.pos).constSlice()) |k| {
                        if (k == .firefly) break true;
                    } else false;
                    if (!any_firefly) {
                        animal.alternative = true;
                        result.is_lost = true;
                        result.message = &.{ "Sundragon must always", "be next to a firefly" };
                        sounds.insert(randomSound(&.{ .poweroff_1, .poweroff_2, .poweroff_3 }));
                    }
                }
            }

            if (true) { // validate rule: no iceburds next to a firefly
                for (new_animals) |*animal| {
                    if (animal.kind != .iceburd) continue;
                    const any_firefly = for (result.surroundingAnimals(animal.pos).constSlice()) |k| {
                        if (k == .firefly) break true;
                    } else false;
                    if (any_firefly) {
                        animal.alternative = true;
                        result.is_lost = true;
                        result.message = &.{"Iceburd melted!"};
                        sounds.insert(randomSound(&.{ .ice_1, .ice_2 }));
                    }
                }
            }

            if (true) { // slimed
                for (new_animals) |*animal| {
                    if (animal.kind.isTongue()) continue;
                    const any_cat = for (result.surroundingAnimals(animal.pos).constSlice()) |k| {
                        if (k == .catslime) break true;
                    } else false;
                    animal.slimed = .next(animal.slimed, any_cat);
                }
            }

            if (result.solved(info)) {
                sounds = .initOne(.unlock);
            }

            return .{ .new_state = result, .message = null, .sounds = sounds };
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

        if (true) { // slimed
            for (initial_state.items) |*animal| {
                if (animal.kind.isTongue()) continue;
                const any_cat = for (Snapshot.surroundingAnimals2(initial_state.items, animal.pos).constSlice()) |k| {
                    if (k == .catslime) break true;
                } else false;
                animal.slimed = if (any_cat) .yes else .no;
            }
        }

        try states_history.append(allocator, .{ .animals = try initial_state.toOwnedSlice(allocator) });

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
    alternative: bool = false,
    slimed: enum {
        no,
        recently,
        yes,
        pub fn next(cur: @This(), any_cat: bool) @This() {
            return if (any_cat) switch (cur) {
                .no => .recently,
                .recently, .yes => .yes,
            } else .no;
        }
        pub fn visible(cur: @This(), anim_t: f32) bool {
            return switch (cur) {
                .no => false,
                .yes => true,
                .recently => anim_t >= 0.8,
            };
        }
    } = .no,
    prev_move: union(enum) {
        nothing,
        /// prev pos
        moved: IVec2,
        /// frog pos
        eaten: IVec2,
    } = .nothing,

    const Kind = enum {
        beetlekey,
        firefly,
        sundragon,
        catslime,
        ant,
        stonefish,
        iceburd,
        fourheadedfrog,

        tongue_up,
        tongue_down,
        tongue_right,
        tongue_left,

        fn isTongue(kind: Kind) bool {
            return switch (kind) {
                else => false,
                .tongue_up,
                .tongue_down,
                .tongue_right,
                .tongue_left,
                => true,
            };
        }

        fn tongue(dir: IVec2) Kind {
            assert(dir.isCardinalDirection());
            if (dir.equals(.e1)) {
                return .tongue_right;
            } else if (dir.equals(.ne1)) {
                return .tongue_left;
            } else if (dir.equals(.e2)) {
                return .tongue_up;
            } else if (dir.equals(.ne2)) {
                return .tongue_down;
            } else unreachable;
        }

        fn isBug(kind: Kind) bool {
            return switch (kind) {
                .beetlekey,
                .firefly,
                .ant,
                => true,
                .sundragon,
                .catslime,
                .stonefish,
                .iceburd,
                .fourheadedfrog,
                .tongue_up,
                .tongue_down,
                .tongue_right,
                .tongue_left,
                => false,
            };
        }

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
                .tongue_up => 0,
                .tongue_down => 0,
                .tongue_right => 0,
                .tongue_left => 0,
            };
        }

        fn fromAscii(char: u8) ?Kind {
            inline for (@typeInfo(Kind).@"enum".fields) |field| {
                const cur: Kind = @enumFromInt(field.value);
                if (toAscii(cur) == std.ascii.toUpper(char)) return cur;
            } else return null;
        }

        fn hoverMessage(kind: Kind) ?[]const []const u8 {
            return switch (kind) {
                .beetlekey => &.{ "Keetle: Its horns can", "unlock cages." },
                .ant => &.{ "Ant: I guess it's not very magical.", "Just a regular old ant." },
                .stonefish => &.{ "Stonefish: Heavy.", "Needs assistance moving." },
                .fourheadedfrog => &.{ "Four-headed Frog:", "Eats any bugs it can see." },
                .catslime => &.{ "Slimecat: Sticky. Pulls anything adjacent to it", "in the same direction when moving." },
                .iceburd => &.{"Iceburd: Melts when near fireflies."},
                .firefly => &.{"Firefly: Hot."},
                .sundragon => &.{"Sundragon: Needs the warmth of fireflies."},

                .tongue_down,
                .tongue_up,
                .tongue_right,
                .tongue_left,
                => null,
            };
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

    var it = std.mem.splitSequence(u8, levels_ascii_raw, "\n\n");
    dst.levels = try gpa.alloc(LevelState, kommon.itertools.iteratorLen(it));
    var k: usize = 0;
    while (it.next()) |src| {
        errdefer std.log.err("bad level at index {d}: \n{s}", .{ k, src });
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

    pub fn drawLevel(drawer: *Drawer, camera: Rect, level: *const LevelState, anim_t: f32) void {
        if (true) { // draw walls
            var it = level.info.walls.iterator();
            while (it.next()) |p| {
                if (level.info.walls.at2(p)) {
                    drawer.wall(p);
                }
            }
        }
        if (!level.cur().solved(level.info)) {
            drawer.lock(level.info.lock);
        }
        for (level.cur().animals) |animal| {
            drawer.drawAnimal(animal, anim_t);
        }

        drawer.end(camera);
    }

    fn sprite(self: *Drawer, pos: Vec2, index: usize) void {
        self.batch.add(.{
            .point = .{ .pos = pos },
            .texcoord = sheet.at(index),
        });
    }

    pub fn wall(self: *Drawer, pos: UVec2) void {
        // self.canvas.fillRect(camera, .{ .top_left = pos.tof32(), .size = .one }, .fromHex("#885522"));
        self.sprite(pos.tof32(), 0);
    }

    pub fn lock(self: *Drawer, pos: IVec2) void {
        // self.canvas.fillRect(camera, .{ .top_left = pos.tof32(), .size = .one }, .cyan);
        self.sprite(pos.tof32(), 1);
    }

    pub fn drawAnimal(self: *Drawer, animal: Animal, anim_t: f32) void {
        const pos: Vec2 = switch (animal.prev_move) {
            .nothing => animal.pos.tof32(),
            .moved => |prev| .lerp(prev.tof32(), animal.pos.tof32(), anim_t),
            .eaten => |next| .lerp(animal.pos.tof32(), next.tof32(), anim_t),
        };
        self.sprite(pos, switch (animal.kind) {
            .catslime => 10,
            .firefly => 3,
            .beetlekey => 6,
            .sundragon => if (animal.alternative) 5 else 4,
            .ant => 11,
            .stonefish => 15,
            .iceburd => if (animal.alternative) 13 else 12,
            .fourheadedfrog => 8,

            .tongue_up => 2,
            .tongue_down => 14,
            .tongue_right => 9,
            .tongue_left => 7,
        });
        if (animal.slimed.visible(anim_t)) {
            self.sprite(pos, 16);
        }
    }
};

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);
    const mem = &self.usual.mem;
    // const smooth = &self.usual.smooth;

    const turn_duration = 0.1;
    math.towards(&self.anim_t, 1, platform.delta_seconds / turn_duration);
    const level_swap_duration = 0.4;
    math.towards(&self.level_change_t, 1, platform.delta_seconds / level_swap_duration);
    const menu_move_duration = 0.4;
    math.towards(&self.in_menu_t, if (self.in_menu) 1 else 0, platform.delta_seconds / menu_move_duration);

    if (self.in_menu_t == 0) self.started_playing = true;

    // if (platform.keyboard.wasPressed(.KeyD) or platform.keyboard.wasPressed(.ArrowRight)) {
    //     self.cur_level += 1;
    //     self.cur_level %= self.levels.len;
    // }
    // if (platform.keyboard.wasPressed(.KeyA) or platform.keyboard.wasPressed(.ArrowLeft)) {
    //     self.cur_level += self.levels.len - 1;
    //     self.cur_level %= self.levels.len;
    // }

    // if (platform.keyboard.wasPressed(.KeyE)) {
    //     self.editing = !self.editing;
    // }

    const prev_level = self.levels[self.cur_level -| 1];
    const level = &self.levels[self.cur_level];

    const ui_camera: Rect = .withAspectRatio(.fromCenterAndSize(
        .zero,
        .new(16, 9),
    ), platform.aspect_ratio, .grow, .center);
    const ui_mouse = platform.getMouse(ui_camera);

    const in_menu_t = math.easings.easeInOutCubic(self.in_menu_t);
    const buttons = &self.buttons;
    for (buttons) |*button| {
        const delta_game: Vec2 = .new(0, in_menu_t * ui_camera.size.y);
        const delta_menu: Vec2 = .new(0, (in_menu_t - 1) * ui_camera.size.y);
        switch (button.action) {
            .undo => {
                button.rect = ui_camera.plusMargin(-0.5).withSize(.new(2.5, 1.5), .bottom_left).move(delta_game);
                button.enabled = level.states_history.items.len > 1;
            },
            .reset => {
                button.rect = ui_camera.plusMargin(-0.5).withSize(.new(2.5, 1.5), .bottom_right).move(delta_game);
                button.enabled = level.states_history.items.len > 1 and !level.cur().was_reset;
            },
            .exit => {
                button.rect = ui_camera.plusMargin(-0.5).withSize(.new(2, 1), .top_right).move(delta_game);
                button.enabled = true;
            },
            .continue_game => {
                button.rect = Rect.fromCenterAndSize(.new(0, 2.6), .new(6, 1.5)).move(delta_menu);
                button.enabled = true;
                button.text = if (self.started_playing)
                    if (self.cur_level > 22)
                        try std.fmt.allocPrint(mem.frame.allocator(), "Extra ({d})", .{
                            self.cur_level - 22,
                        })
                    else
                        try std.fmt.allocPrint(mem.frame.allocator(), "Continue ({d})", .{
                            self.cur_level + 1,
                        })
                else
                    "Start";
            },
            .prev_level => {
                button.rect = Rect.fromCenterAndSize(.new(-4.5, 2.6), .new(1.5, 1.5)).move(delta_menu);
                button.enabled = true;
                button.exists = self.started_playing;
            },
            .next_level => {
                button.rect = Rect.fromCenterAndSize(.new(4.5, 2.6), .new(1.5, 1.5)).move(delta_menu);
                button.enabled = true;
                button.exists = self.started_playing;
            },
            .toggle_sfx => {
                button.rect = ui_camera.plusMargin(-0.5).withSize(.new(2, 1), .top_left).move(delta_menu);
                button.enabled = true;
                button.latched = self.sfx_muted;
            },
            .toggle_music => {
                button.rect = ui_camera.plusMargin(-0.5).withSize(.new(2, 1), .top_right).move(delta_menu);
                button.enabled = true;
                button.latched = self.music_muted;
            },
        }
    }

    if (!ui_mouse.cur.isDown(.left)) self.active = null;
    self.hovered = for (buttons, 0..) |button, k| {
        if ((self.active == null or self.active == k) and
            button.exists and button.enabled and
            button.rect.contains(ui_mouse.cur.position)) break k;
    } else null;
    self.active, const action: ?Button.Action = if (ui_mouse.wasPressed(.left))
        .{ self.hovered, if (self.hovered) |h| buttons[h].action else null }
    else
        .{ self.active, null };

    for (buttons, 0..) |*button, k| {
        math.lerpTowards(&button.hovered_t, if (k == self.hovered) 1 else 0, .fast, platform.delta_seconds);
        math.lerpTowards(&button.active_t, if (k == self.active) 1 else 0, .fast, platform.delta_seconds);
        math.lerpTowards(&button.enabled_t, if (button.enabled) 1 else 0, .fast, platform.delta_seconds);
        math.lerpTowards(&button.latched_t, if (button.latched) 1 else 0, .fast, platform.delta_seconds);
    }

    if (action) |a| switch (a) {
        .undo, .reset, .exit => {},
        .continue_game => {
            self.in_menu = false;
            self.level_change_t = 1;
            self.anim_t = 1;
            self.hovered = null;
            self.active = null;

            if (level.cur().solved(level.info)) {
                for (level.states_history.items[1..]) |s| {
                    mem.gpa.free(s.animals);
                }
                level.states_history.shrinkRetainingCapacity(1);
            }
        },
        .prev_level => self.cur_level = (self.cur_level + self.levels.len - 1) % self.levels.len,
        .next_level => self.cur_level = (self.cur_level + 1) % self.levels.len,
        .toggle_sfx => self.sfx_muted = !self.sfx_muted,
        .toggle_music => self.music_muted = !self.music_muted,
    };

    if (self.anim_t >= 1 or self.started_grabbing_at != null) {
        if (try level.cur().autoMove(level.info, mem.gpa, mem.frame.allocator())) |new_state| {
            try level.states_history.append(mem.gpa, new_state);
            self.anim_t = 0;
            self.started_grabbing_at = null;
            if (new_state.any_eaten_now or new_state.started_frog_attack) platform.sound_queue.insert(randomSound(&.{ .frog_1, .frog_2, .frog_3, .frog_4 }));
        }
    }

    if ((action == .undo or platform.wasKeyPressedOrRetriggered(.KeyZ, 0.1, 0.2)) and
        level.states_history.items.len > 1)
    {
        while (true) {
            const old = level.states_history.pop().?;
            if (!old.is_automove) break;
            assert(level.states_history.items.len > 1);
        }
        self.anim_t = 1;
    }

    if ((action == .reset or platform.keyboard.wasPressed(.KeyR)) and
        level.states_history.items.len > 1)
    {
        const restart_animals = try mem.gpa.dupe(Animal, level.states_history.items[0].animals);
        try level.states_history.append(mem.gpa, .{ .animals = restart_animals, .was_reset = true });
        self.anim_t = 1;
    }

    if (self.cur_level == 22 and self.in_menu == false) {
        self.time_in_youwin += platform.delta_seconds;
    } else {
        self.time_in_youwin = 0;
    }

    if (action == .exit or platform.keyboard.wasPressed(.Escape) or self.time_in_youwin >= 5) {
        self.in_menu = true;
        self.show_final_message = self.cur_level == 22;
    } else {
        if (self.in_menu_t < 0.01 or self.cur_level != 22) self.show_final_message = false;
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
            level.states_history.appendAssumeCapacity(.{ .animals = new_animals });
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
            level.states_history.appendAssumeCapacity(.{ .animals = new_animals });
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

    const level_change_t = math.easings.easeInOutCubic(self.level_change_t);
    const prev_camera: Rect = .withAspectRatio(.plusMargin3(.plusMargin(.{
        .top_left = .new(10 * level_change_t, 0),
        .size = prev_level.info.size().tof32(),
    }, 0.5), .bottom, 1.5), platform.aspect_ratio, .grow, .center);
    const camera: Rect = .withAspectRatio(.plusMargin3(.plusMargin(.{
        .top_left = .new(-10 * (1 - level_change_t), 0),
        .size = level.info.size().tof32(),
    }, 0.5), .bottom, 1.5), platform.aspect_ratio, .grow, .center);
    const mouse = platform.getMouse(camera);

    if (self.started_grabbing_at == null) {
        self.keyboard_wasd_offset = .zero;
    } else {
        for (KeyboardButton.directional_keys) |s| {
            for (s.keys) |k| {
                if (platform.keyboard.wasPressed(k)) {
                    self.keyboard_wasd_offset.addInPlace(s.dir);
                }
            }
        }
    }

    const active_tile: ?IVec2 = if (self.in_menu or self.hovered != null or self.active != null)
        null
    else
        level.info.walls.tileSignedAt(mouse.cur.position.add(self.keyboard_wasd_offset.tof32()), .{
            .top_left = .zero,
            .size = level.info.size().tof32(),
        });
    defer {
        if (self.prev_active_tile != null and
            active_tile != null and
            self.started_grabbing_at == null and
            self.prev_active_tile.?.equals(active_tile.?))
        {
            self.prev_active_tile_time += platform.delta_seconds;
        } else {
            self.prev_active_tile_time = 0;
        }

        self.prev_active_tile = active_tile;
    }

    if (self.editing and active_tile != null and mouse.wasPressed(.right) and !level.info.lock.equals(active_tile.?)) {
        if (level.cur().animalAt(active_tile.?)) |animal_id| {
            var new_animals: std.ArrayListUnmanaged(Animal) = .fromOwnedSlice(try mem.gpa.dupe(Animal, level.cur().animals));
            _ = new_animals.swapRemove(animal_id);
            try level.states_history.append(mem.gpa, .{ .animals = try new_animals.toOwnedSlice(mem.gpa) });
        } else {
            level.info.walls.setSigned(active_tile.?, !level.info.walls.atSigned(active_tile.?));
        }
    }

    if (self.editing and active_tile != null) {
        inline for (@typeInfo(Animal.Kind).@"enum".fields, 1..) |field, digit| {
            comptime assert(digit < 10 or std.mem.startsWith(u8, field.name, "tongue"));
            if (digit < 10 and platform.keyboard.wasPressed(.digit(digit))) {
                var new_animals: std.ArrayListUnmanaged(Animal) = .fromOwnedSlice(try mem.gpa.dupe(Animal, level.cur().animals));
                if (level.cur().animalAt(active_tile.?)) |old| {
                    _ = new_animals.swapRemove(old);
                }
                try new_animals.append(mem.gpa, .{ .pos = active_tile.?, .kind = @enumFromInt(field.value) });
                try level.states_history.append(mem.gpa, .{ .animals = try new_animals.toOwnedSlice(mem.gpa) });
            }
        }
    }

    var message = level.cur().message;
    const time_until_message = 0.3;
    if (!level.cur().is_lost and active_tile != null and
        self.prev_active_tile_time > time_until_message and
        self.prev_active_tile != null and self.prev_active_tile.?.equals(active_tile.?))
    {
        if (level.cur().animalAt(active_tile.?)) |i| {
            message = level.cur().animals[i].kind.hoverMessage();
        }
    }

    if (!level.cur().is_lost and (!level.cur().is_automove or self.anim_t >= 1)) {
        if (self.started_grabbing_at == null) {
            platform.setCursor(if (active_tile != null and level.cur().animalAt(active_tile.?) != null)
                .could_grab
            else if (self.hovered != null or self.active != null)
                .pointer
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
                            try level.states_history.append(mem.gpa, .{ .animals = new_animals });
                            self.started_grabbing_at = active_tile.?;
                        } else if (level.info.lock.equals(self.started_grabbing_at.?)) {
                            level.info.lock = active_tile.?;
                            self.started_grabbing_at = active_tile.?;
                        }
                    }
                } else if (!level.cur().is_automove or self.anim_t >= 1) {
                    const result = try level.cur().doMove(level.info, mem.gpa, mem.frame.allocator(), self.started_grabbing_at.?, active_tile.?);
                    if (self.prev_active_tile == null or !active_tile.?.equals(self.prev_active_tile.?)) {
                        platform.sound_queue.setUnion(result.sounds);
                    }
                    message = result.message;
                    if (result.new_state) |new_state| {
                        try level.states_history.append(mem.gpa, new_state);
                        self.anim_t = 0;
                        self.started_grabbing_at = active_tile.?;
                    }
                }
            }
        }
    } else {
        platform.setCursor(.default);
    }

    if (@import("builtin").target.ofmt == .wasm) {
        const music_gain: f32 = if (self.music_muted) 0 else 1;
        math.lerpTowards(platform.loop_volumes.getPtr(.music), music_gain * @as(f32, if (level.cur().is_lost) 0 else 0.5), .fast, platform.delta_seconds);
        math.lerpTowards(platform.loop_volumes.getPtr(.muffled), music_gain * @as(f32, if (level.cur().is_lost) 0.5 else 0), .fast, platform.delta_seconds);
    }

    if (self.sfx_muted) platform.sound_queue.* = .initEmpty();

    if (self.editing) {
        message = &.{
            "i to increase level size, o to decrease it",
            "c/v to save/load into clipboard, 1-9 to add animals",
            "right click to delete animals and toggle walls",
        };
    }

    platform.gl.clear(.fromHex("#181818"));

    var drawer: Drawer = .start(&self.usual.canvas, self.atlas_texture);
    if (level_change_t < 1) {
        drawer.drawLevel(prev_camera, &prev_level, self.anim_t);
    }
    drawer.drawLevel(camera, level, self.anim_t);

    for (self.buttons) |button| {
        if (button.layer == .game) {
            try button.draw(&self.usual.canvas, ui_camera);
        }
    }

    if (message) |msg| {
        try self.usual.canvas.drawTextLines(
            0,
            ui_camera,
            .center,
            .{ .center = ui_camera.getAt(.new(0.5, 1)).addY(-1.4) },
            msg,
            0.7,
            1.1,
            .white,
        );
    }

    self.usual.canvas.fillRect(.unit, .unit, .blackAlpha((@as(f32, if (self.started_playing and !self.show_final_message) 0.5 else 1)) * in_menu_t));

    if (!self.started_playing) {
        try self.usual.canvas.drawTextLines(
            0,
            ui_camera,
            .center,
            .{ .center = ui_camera.getAt(.new(0.5, 0.45 - (1.0 - in_menu_t))) },
            &.{
                "The owner of the magical menagerie",
                "has left for lunch, help the creatures",
                "work together through 22 levels to",
                "unlock their cages and escape the menagerie",
                "(without letting any of them die)!",
            },
            0.7,
            1.1,
            .white,
        );
    }

    if (self.show_final_message) {
        try self.usual.canvas.drawTextLines(
            0,
            ui_camera,
            .center,
            .{ .center = ui_camera.getAt(.new(0.5, 0.45 - (1.0 - in_menu_t))) },
            &.{
                "Congrats on completing the game.",
                "You've saved the animals but there's",
                "still more to free. The following",
                "are 4 challenge levels more difficult",
                "than the main game.",
            },
            0.7,
            1.1,
            .white,
        );
    }

    for (self.buttons) |button| {
        if (button.layer == .menu) {
            try button.draw(&self.usual.canvas, ui_camera);
        }
    }

    if (!self.in_menu and level.cur().solved(level.info) and !mouse.cur.isDown(.left)) {
        if (self.cur_level + 1 < self.levels.len) {
            self.cur_level += 1;
            self.level_change_t = 0;
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
