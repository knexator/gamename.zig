//! This should be unchanged regardless of platform

// TODO: maybe combine Editing & Executing (they both share Camera and some UI)

const std = @import("std");
const assert = std.debug.assert;

pub const kommon = @import("kommon");
pub const safeAt = kommon.safeAt;
pub const Mouse = kommon.input.Mouse;
pub const KeyboardButton = enum { left, right, up, down, space };
pub const Keyboard = kommon.input.CustomKeyboard(KeyboardButton);
const math = kommon.math;
pub const Vec2 = math.Vec2;
pub const Rect = math.Rect;
pub const Camera = math.Camera;
pub const Color = math.UColor;
pub const Point = math.Point;
const Random = math.Random;
const tof32 = math.tof32;
const lerp = math.lerp;
const in01 = math.in01;
const clamp = math.clamp;
const clamp01 = math.clamp01;
const remap = math.remap;
const inRange = math.inRange;
const funk = kommon.funktional;

const core = @import("main.zig");
const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const OoM = error{ OutOfMemory, TODO, BAD_INPUT };

const TestSetScore = struct {
    code_size: usize,
    compile_time: usize,
    total_successful_matches: usize,
    total_max_stack: usize,

    pub const all_0 = std.mem.zeroes(@This());

    var display_text_buf: [0x1000]u8 = undefined;
    pub fn display(self: @This(), meta_enabled: bool) ![:0]const u8 {
        if (meta_enabled) {
            return std.fmt.bufPrintZ(&display_text_buf,
                \\Code Size: {d} 
                \\Total Execution Time: {d}
                \\Total Max stack: {d}
                \\Compile Time: {d}
            , .{ self.code_size, self.total_successful_matches, self.total_max_stack, self.compile_time }) catch |err| switch (err) {
                error.NoSpaceLeft => error.OutOfMemory,
            };
        } else {
            return std.fmt.bufPrintZ(&display_text_buf,
                \\Code Size: {d} 
                \\Total Execution Time: {d}
                \\Total Max stack: {d}
            , .{ self.code_size, self.total_successful_matches, self.total_max_stack }) catch |err| switch (err) {
                error.NoSpaceLeft => error.OutOfMemory,
            };
        }
    }
};

pub const DESIGN = @import("DESIGN");

pub const Platform = struct {
    gpa: std.mem.Allocator,
    getPlayerData: fn (mem: *VeryPermamentGameStuff) OoM!?PlayerData,
    setPlayerData: fn (player_data: PlayerData, mem: *VeryPermamentGameStuff) OoM!void,
    downloadPlayerData: fn (player_data: PlayerData, alloc: std.mem.Allocator) OoM!void,
    uploadPlayerData: fn () std.io.AnyReader,
    getMouse: fn () Mouse,
    getKeyboard: fn () Keyboard,
    setCursor: fn (cursor: Cursor) void,

    pub const Cursor = enum(u8) { default, could_grab, grabbing, pointer };
};

/// Stuff that doesn't change when changing which fnk is being edited, etc
fn SessionPersistent(platform: Platform, drawer: Drawer) type {
    const Editing = EditingFnk(platform, drawer);
    return struct {
        list_viewer: Editing.ListViewer,
        sexpr_holders: [3]Editing.SexprHolder,
    };
}

// TODO NOW: allow non-ascii sexpr names
// TODO: join samples & fnks
pub const PlayerData = struct {
    // TODO: this field should not be here.
    ascii_data: []const u8,
    ascii_data_for_custom_samples: []const u8,
    ascii_data_for_fav_fnks: []const u8,

    fnks: FnkCollection,
    custom_samples: SamplesCollection,
    is_builtin_level_solved: [builtin_levels.len]bool,
    first_time: bool = true,
    // hacky as hell lol
    favorite_fnk_names: std.ArrayList(*const Sexpr),

    const no_samples: []const Sample = &.{};
    pub const SamplesCollection = std.ArrayHashMap(*const Sexpr, []const Sample, core.SexprContext, true);

    pub fn allFnkNames(self: PlayerData) []const *const Sexpr {
        return self.fnks.keys();
    }

    pub fn empty(mem: *VeryPermamentGameStuff) !PlayerData {
        var asdf: FnkCollection = .init(mem.gpa);
        // TODO: WITHOUT THIS LINE, IT CRASHES WHEN ADDING A 5th FNK
        try asdf.ensureTotalCapacity(1000);
        var custom_samples: SamplesCollection = .init(mem.gpa);
        // TODO: WITHOUT THIS LINE, IT CRASHES WHEN ADDING A 5th FNK
        try custom_samples.ensureTotalCapacity(1000);
        return .{
            .ascii_data = "",
            .ascii_data_for_custom_samples = "",
            .ascii_data_for_fav_fnks = "",
            .fnks = asdf,
            // .fnks = FnkCollection.init(mem.gpa),
            .custom_samples = custom_samples,
            .is_builtin_level_solved = @splat(false),
            .favorite_fnk_names = .init(mem.gpa),
        };
    }

    pub fn updateSolvedStatusOfAll(self: *PlayerData, mem: *VeryPermamentGameStuff) !void {
        for (0..builtin_levels.len) |k| {
            try self.updateSolvedStatus(k, mem);
        }
    }

    pub fn updateSolvedStatus(self: *PlayerData, level_index: usize, mem: *VeryPermamentGameStuff) !void {
        std.debug.assert(level_index < builtin_levels.len);
        self.is_builtin_level_solved[level_index] = try isSolved(builtin_levels[level_index], self.fnks, mem);
    }

    fn isSolved(level: BuiltinLevel, fnks: FnkCollection, mem: *VeryPermamentGameStuff) !bool {
        var score = try core.ScoringRun.initFromFnks(fnks, mem);
        defer score.deinit(false);

        for (level.manual_samples) |sample| {
            var exec = core.ExecutionThread.init(sample.input, level.fnk_name, &score) catch |err| switch (err) {
                error.FnkNotFound => return false,
                else => return err,
            };
            defer exec.deinit();

            const actual_output = exec.getFinalResultBounded(&score, 10_000) catch |err| switch (err) {
                error.FnkNotFound, error.NoMatchingCase, error.InvalidMetaFnk, error.UsedUndefinedVariable, error.TookTooLong => return false,
                error.OutOfMemory => return err,
                error.BAD_INPUT => return err,
            };
            if (!actual_output.equals(sample.output.?)) return false;
        } else {
            return true;
        }
    }

    const SEPARATOR = "\n////////////////////\n";

    pub fn fromAsciiNew(ascii: []const u8, mem: *VeryPermamentGameStuff) !PlayerData {
        var it = std.mem.splitSequence(u8, ascii, SEPARATOR);
        const version = trimmed(it.first());
        assert(std.mem.eql(u8, version, "v0"));
        const fnks_data = trimmed(it.next().?);
        const custom_samples_data = trimmed(it.next().?);
        const favorite_fnks_data = trimmed(it.next().?);
        assert(it.next() == null);
        return try fromAscii(fnks_data, custom_samples_data, favorite_fnks_data, mem);
    }

    fn trimmed(x: []const u8) []const u8 {
        return std.mem.trim(u8, x, &std.ascii.whitespace);
    }

    pub fn fromAscii(fnks_data: []const u8, custom_samples_data: []const u8, favorite_fnks_data: []const u8, mem: *VeryPermamentGameStuff) !PlayerData {
        const ascii_data = try mem.gpa.dupe(u8, fnks_data);
        var parser = parsing.Parser{ .remaining_text = ascii_data };
        var fnks = FnkCollection.init(mem.gpa);
        errdefer fnks.deinit();
        // TODO: WITHOUT THIS LINE, IT CRASHES WHEN ADDING A 5th FNK
        try fnks.ensureTotalCapacity(1000);
        try parser.parseFnkCollection(&fnks, &mem.pool_for_sexprs, mem.arena_for_cases.allocator());

        const ascii_data_for_custom_samples = try mem.gpa.dupe(u8, custom_samples_data);
        var parser2 = parsing.Parser{ .remaining_text = ascii_data_for_custom_samples };
        var fnks2 = FnkCollection.init(mem.gpa);
        errdefer fnks2.deinit();
        // TODO: WITHOUT THIS LINE, IT CRASHES WHEN ADDING A 5th FNK
        try fnks2.ensureTotalCapacity(1000);
        try parser2.parseFnkCollection(&fnks2, &mem.pool_for_sexprs, mem.arena_for_cases.allocator());
        var custom_samples: SamplesCollection = .init(mem.gpa);
        // TODO: WITHOUT THIS LINE, IT CRASHES WHEN ADDING A 5th FNK
        try custom_samples.ensureTotalCapacity(1000);
        var it = fnks2.iterator();
        while (it.next()) |entry| {
            const cases = entry.value_ptr.cases.items;
            const samples = try mem.gpa.alloc(Sample, cases.len);
            for (cases, samples) |case, *dst| {
                assert(case.next == null);
                assert(case.fnk_name.equals(Sexpr.builtin.identity));
                dst.* = .{
                    .input = case.pattern,
                    .output = case.template,
                };
            }
            try custom_samples.putNoClobber(entry.key_ptr.*, samples);
        }

        const ascii_data_for_fav_fnks = try mem.gpa.dupe(u8, favorite_fnks_data);
        var parser3 = parsing.Parser{ .remaining_text = ascii_data_for_fav_fnks };
        var fav_fnks: std.ArrayList(*const Sexpr) = .init(mem.gpa);
        while (try parser3.maybeParseSexpr(&mem.pool_for_sexprs)) |name| {
            try fav_fnks.append(name);
        }

        var is_builtin_level_solved: [builtin_levels.len]bool = undefined;
        for (builtin_levels, &is_builtin_level_solved) |level, *target| {
            target.* = try isSolved(level, fnks, mem);
        }
        return PlayerData{
            .fnks = fnks,
            .custom_samples = custom_samples,
            .ascii_data = ascii_data,
            .is_builtin_level_solved = is_builtin_level_solved,
            .ascii_data_for_custom_samples = ascii_data_for_custom_samples,
            .ascii_data_for_fav_fnks = ascii_data_for_fav_fnks,
            .favorite_fnk_names = fav_fnks,
        };
    }

    pub fn toAsciiNew(this: PlayerData, alloc: std.mem.Allocator) ![]const u8 {
        const asdf = try this.toAscii(alloc);

        var result = std.ArrayList(u8).init(alloc);
        try result.appendSlice("v0");
        try result.appendSlice(SEPARATOR);
        try result.appendSlice(asdf.fnks);
        try result.appendSlice(SEPARATOR);
        try result.appendSlice(asdf.samples);
        try result.appendSlice(SEPARATOR);
        try result.appendSlice(asdf.fav_fnks);

        return result.toOwnedSlice();
    }

    pub fn toAscii(this: PlayerData, alloc: std.mem.Allocator) !struct {
        fnks: []const u8,
        samples: []const u8,
        fav_fnks: []const u8,
    } {
        var result = std.ArrayList(u8).init(alloc);
        var it = this.fnks.iterator();
        while (it.next()) |x| {
            const fnk = Fnk{ .name = x.key_ptr.*, .body = x.value_ptr.* };
            const str = try std.fmt.allocPrint(alloc, "{any}\n", .{fnk});
            defer alloc.free(str);
            try result.appendSlice(str);
        }

        var result2 = std.ArrayList(u8).init(alloc);
        var it2 = this.custom_samples.iterator();
        const writer2 = result2.writer();
        while (it2.next()) |x| {
            try writer2.print("{any}", .{x.key_ptr.*});
            try writer2.writeAll(" {\n");
            for (x.value_ptr.*) |sample| {
                try writer2.print("\t{any} -> {any};\n", .{ sample.input, sample.output });
            }
            try writer2.writeAll("}\n\n");
        }

        var result3 = std.ArrayList(u8).init(alloc);
        const writer3 = result3.writer();
        for (this.favorite_fnk_names.items) |name| {
            try writer3.print("{any}\n", .{name});
        }

        return .{
            .fnks = try result.toOwnedSlice(),
            .samples = try result2.toOwnedSlice(),
            .fav_fnks = try result3.toOwnedSlice(),
        };
    }

    pub fn deinit(this: *PlayerData, mem: *VeryPermamentGameStuff) void {
        this.fnks.deinit();
        mem.gpa.free(this.ascii_data);
    }

    test "PlayerData" {
        var mem = VeryPermamentGameStuff.init(std.testing.allocator);
        defer mem.deinit();

        var sut = try PlayerData.fromAscii(
            \\
            \\foo {
            \\  x -> y;
            \\}
            \\bar {
            \\  @a -> foo: x {
            \\      @r -> @a;
            \\  }
            \\}
        , &mem);
        defer sut.deinit(&mem);

        try std.testing.expectEqual(2, sut.fnks.count());

        const str = try sut.toAscii(mem.gpa);
        defer mem.gpa.free(str);
        var sut2 = try PlayerData.fromAscii(str, &mem);
        defer sut2.deinit(&mem);

        try std.testing.expectEqual(2, sut2.fnks.count());
    }
};

pub const AtomVisuals = struct {
    profile: []const Vec2,
    color: Color,
    display: ?[]const u8 = null,
};

pub const Drawer = struct {
    clear: fn (color: Color) void,
    asdfBackground: fn () void,
    setTransparency: fn (alpha: f32) void,
    drawLine: fn (camera: Camera, points: []const Vec2, color: Color) void,
    drawRect: fn (camera: Camera, rect: Rect, stroke: ?Color, fill: ?Color) void,
    drawShape: fn (camera: Camera, points: []const Vec2, stroke: ?Color, fill: ?Color) void,
    drawShapeV2: fn (camera: Camera, parent_world_point: Point, local_points: []const Vec2, stroke: ?Color, fill: ?Color) void,
    clipAtomRegion: fn (camera: Camera, point: Point) void,
    endClip: fn () void,
    drawDebugText: fn (camera: Camera, center: Point, text: [:0]const u8, color: Color) void,
    drawAtomDebug: fn (camera: Camera, world_point: Point) void,
    drawAtom: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,
    drawPatternAtomOutline: fn (camera: Camera, world_point: Point) void,
    drawPatternAtomDebug: fn (camera: Camera, world_point: Point) void,
    drawPairHolder: fn (camera: Camera, world_point: Point) void,
    drawPatternPairHolder: fn (camera: Camera, world_point: Point) void,
    drawPatternAtom: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,
    drawCable: fn (camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void,
    drawCaseHolder: fn (camera: Camera, world_point: Point) void,
    drawFnkHolder: fn (camera: Camera, world_point: Point) void,
    // TODO: think how to use the visuals when drawing a variable
    drawVariable: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,
    drawPatternVariable: fn (camera: Camera, world_point: Point, visuals: AtomVisuals) void,
    drawWildcardsCable: fn (camera: Camera, points: []const Vec2, visuals: []const AtomVisuals) void,

    pub fn drawCaseHolderFromPatternPoint(self: Drawer, camera: Camera, pattern_point: Point) void {
        self.drawCaseHolder(camera, .{ .pos = pattern_point.pos.sub(.new(3, 0)) });
    }

    pub fn drawCaseHolderExtended(self: Drawer, camera: Camera, world_point: Point, enabled: bool) void {
        if (!enabled) self.setTransparency(0.5);
        self.drawCaseHolder(camera, world_point);
        if (!enabled) self.setTransparency(1);
    }

    pub fn drawLineV2(self: Drawer, camera: Camera, parent: Point, local_positions: []const Vec2, color: Color) void {
        var buf: [128]Vec2 = undefined;
        if (local_positions.len > buf.len) @panic("oops");
        for (local_positions, buf[0..local_positions.len]) |p, *target| {
            target.* = parent.applyToLocalPosition(p);
        }
        self.drawLine(camera, buf[0..local_positions.len], color);
    }

    pub fn drawCircle(self: Drawer, camera: Camera, center: Point, stroke: ?Color, fill: ?Color) void {
        self.drawShapeV2(camera, center, &funk.fromCount(64, struct {
            pub fn anon(k: usize) Vec2 {
                return Vec2.fromTurns(math.tof32(k) / 64);
            }
        }.anon), stroke, fill);
    }

    pub fn drawArrowForSample(self: Drawer, camera: Camera, center: Point, solved: ?bool) void {
        const color: Color = if (solved) |s|
            if (s) .from01(0.2, 1, 0.5) else .from01(1, 0.2, 0.3)
        else
            .black;
        self.drawLine(camera, &.{
            center.applyToLocalPosition(.new(-1, 0)),
            center.applyToLocalPosition(.new(2, 0)),
        }, color);
        self.drawLine(camera, &.{
            center.applyToLocalPosition(.new(1, -1)),
            center.applyToLocalPosition(.new(2, 0)),
            center.applyToLocalPosition(.new(1, 1)),
        }, color);
    }

    pub fn drawAtomHint(self: Drawer, camera: Camera, pos: Point) void {
        self.drawLine(camera, &funk.mapWithCtx(Point.applyToLocalPosition, &([1]Vec2{.new(1.5, -1)} ++ funk.fromCount(32, struct {
            pub fn anon(k: usize) Vec2 {
                return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
            }
        }.anon) ++ [1]Vec2{.new(1.5, 1)}), pos), .white);
    }

    const dummySignatures = struct {
        pub fn nothing() void {
            unreachable;
        }
        pub fn color(c: Color) void {
            _ = c;
            unreachable;
        }
        pub fn camera_point_visuals(camera: Camera, world_point: Point, visuals: AtomVisuals) void {
            _ = camera;
            _ = world_point;
            _ = visuals;
            unreachable;
        }
        pub fn camera_point(camera: Camera, world_point: Point) void {
            _ = camera;
            _ = world_point;
            unreachable;
        }
    };
    pub const dummy = Drawer{
        .clear = dummySignatures.color,
        .setTransparency = struct {
            pub fn anon(alpha: f32) void {
                _ = alpha;
                unreachable;
            }
        }.anon,
        .drawLine = struct {
            pub fn anon(camera: Camera, points: []const Vec2, color: Color) void {
                _ = camera;
                _ = points;
                _ = color;
                unreachable;
            }
        }.anon,
        .drawRect = struct {
            pub fn anon(camera: Camera, rect: Rect, stroke: ?Color, fill: ?Color) void {
                _ = camera;
                _ = rect;
                _ = stroke;
                _ = fill;
                unreachable;
            }
        }.anon,
        .drawShape = struct {
            pub fn anon(camera: Camera, points: []const Vec2, stroke: ?Color, fill: ?Color) void {
                _ = camera;
                _ = points;
                _ = stroke;
                _ = fill;
                unreachable;
            }
        }.anon,
        .drawShapeV2 = struct {
            pub fn anon(camera: Camera, parent_world_point: Point, local_points: []const Vec2, stroke: ?Color, fill: ?Color) void {
                _ = camera;
                _ = parent_world_point;
                _ = local_points;
                _ = stroke;
                _ = fill;
                unreachable;
            }
        }.anon,
        .clipAtomRegion = dummySignatures.camera_point,
        .endClip = dummySignatures.nothing,
        .drawAtomDebug = dummySignatures.camera_point,
        .drawAtom = dummySignatures.camera_point_visuals,
        .drawVariable = dummySignatures.camera_point_visuals,
        .drawPatternVariable = dummySignatures.camera_point_visuals,
        .drawPatternAtomDebug = dummySignatures.camera_point,
        .drawCaseHolder = dummySignatures.camera_point,
        .drawFnkHolder = dummySignatures.camera_point,
        .drawPairHolder = dummySignatures.camera_point,
        .drawPatternPairHolder = dummySignatures.camera_point,
        .drawPatternAtom = dummySignatures.camera_point_visuals,
        .drawPatternAtomOutline = dummySignatures.camera_point,
        .drawCable = struct {
            pub fn anon(camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void {
                _ = camera;
                _ = world_from;
                _ = world_to;
                _ = world_scale;
                _ = offset;
                unreachable;
            }
        }.anon,
        .drawDebugText = struct {
            pub fn anon(camera: Camera, center: Point, text: [:0]const u8, color: Color) void {
                _ = camera;
                _ = center;
                _ = text;
                _ = color;
                unreachable;
            }
        }.anon,
        .drawWildcardsCable = struct {
            pub fn anon(camera: Camera, points: []const Vec2, visuals: []const AtomVisuals) void {
                _ = camera;
                _ = points;
                _ = visuals;
                unreachable;
            }
        }.anon,
    };
};

fn moveCamera(camera: *Camera, delta_seconds: f32, keyboard: Keyboard, mouse: Mouse) void {
    if (DESIGN.allow_zoom) {
        const mouse_pos = mouse.cur.pos(camera.*);
        camera.* = camera.zoom(mouse_pos, camera.height * switch (mouse.cur.scrolled) {
            .none => tof32(1.0),
            .down => 1.1,
            .up => 0.9,
        });
    }

    inline for (.{
        .{ KeyboardButton.left, Vec2.new(-1, 0) },
        .{ KeyboardButton.right, Vec2.new(1, 0) },
        .{ KeyboardButton.up, Vec2.new(0, -1) },
        .{ KeyboardButton.down, Vec2.new(0, 1) },
    }) |key_dir| {
        if (keyboard.cur.isDown(key_dir[0])) {
            camera.center = camera.center.add(key_dir[1].scale(delta_seconds * camera.height));
        }
    }

    if (mouse.cur.isDown(.middle) and mouse.prev.isDown(.middle)) {
        camera.* = camera.*.drag(
            mouse.prev.client_pos,
            mouse.cur.client_pos,
        );
    }
}

fn defaultFnkBody(mem: *VeryPermamentGameStuff) FnkBody {
    const default_fnk =
        \\default {
        \\  @x -> @x;
        \\}
    ;
    var parser = parsing.Parser{ .remaining_text = default_fnk };
    const fnk = parser.parseFnkNew(&mem.pool_for_sexprs, mem.arena_for_cases.allocator()) catch unreachable;
    return fnk.body;
}

/// The full game, from loading screen to end credits
pub fn Presenter(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();

        mem: VeryPermamentGameStuff,
        persistence: PlayerData,

        // TODO: remove this hack
        scoring_run: core.ScoringRun,

        camera: Camera,

        session_persistent: SessionPersistent(platform, drawer),

        pending_upload_savegame: ?std.io.AnyReader = null,

        state: union(enum) {
            /// not used for now
            intro: IntroSequence(platform, drawer),
            level_select: LevelSelect(platform, drawer),
            loading_editing_fnk: LoadingAnEditingFnk(platform, drawer),
            editing_fnk: EditingFnk(platform, drawer),
            executing_fnk: struct { main: ExecutingFnk(platform, drawer), prev_editing: EditingFnk(platform, drawer) },
            testing_fnk: struct { main: TestingFnk(platform, drawer), prev_editing: EditingFnk(platform, drawer) },
        },
        /// only valid when state is editing_fnk
        prev_editing_names: std.SegmentedList(*const Sexpr, 8) = .{},

        pub fn init(result: *Self) !void {
            const platform_alloc = platform.gpa;
            result.mem = VeryPermamentGameStuff.init(platform_alloc);
            const player_data = (try platform.getPlayerData(&result.mem)) orelse (try PlayerData.empty(&result.mem));
            if (!player_data.first_time) return error.TODO;

            result.persistence = player_data;

            result.state = .{
                .level_select = try .init(&result.persistence),
            };

            try Artist(platform, drawer).init();

            result.session_persistent = .{ .list_viewer = .init(), .sexpr_holders = .{
                .{ .point = .{ .pos = .new(13, 15) } },
                .{ .point = .{ .pos = .new(14, 15) } },
                .{ .point = .{ .pos = .new(15, 15) } },
            } };
        }

        fn initEditingAndMaybeFindBuiltinLevel(self: *Self, fnk_name: *const Sexpr) !void {
            const level = findBuiltinLevel(fnk_name);
            try self.initEditingAndMaybeCompile(
                fnk_name,
                if (level) |l| l.manual_samples else null,
                if (level) |l| l.premade_solution else null,
                if (level) |l| l.tutorial_state else null,
            );
        }

        fn initEditingAndMaybeCompile(
            self: *Self,
            fnk_name: *const Sexpr,
            builtin_samples: ?[]const Sample,
            premade_solution: ?[]const u8,
            tutorial_state: ?TutorialState,
        ) !void {
            // TODO: always try to compile first, and/or avoid the player storing fnks with a compilable name
            const fnk_body: core.FnkBody = if (self.persistence.fnks.get(fnk_name)) |fnk_body| fnk_body else blk: {
                if (premade_solution) |raw_fnk| {
                    var parser = parsing.Parser{ .remaining_text = raw_fnk };
                    const fnk = try parser.parseFnkNew(&self.mem.pool_for_sexprs, self.mem.arena_for_cases.allocator());
                    std.debug.assert(fnk.name.equals(fnk_name));
                    try self.persistence.fnks.putNoClobber(fnk_name, fnk.body);
                } else {
                    // Try to compile it
                    var asdf: core.ScoringRun = try .initFromFnks(self.persistence.fnks, &self.mem);
                    const maybe_compiled: ?*const FnkBody = asdf.findFunktion(fnk_name) catch null;
                    if (maybe_compiled) |body| {
                        _ = body;
                        // findFunktion already stores the compiled fnk
                        // try self.persistence.fnks.putNoClobber(fnk_name, body.*);
                    } else {
                        try self.persistence.fnks.putNoClobber(fnk_name, defaultFnkBody(&self.mem));
                    }
                }
                break :blk self.persistence.fnks.get(fnk_name).?;
            };

            const custom_samples = self.persistence.custom_samples.get(fnk_name) orelse PlayerData.no_samples;

            self.state = .{
                .editing_fnk = try .init(
                    fnk_name,
                    builtin_samples orelse PlayerData.no_samples,
                    custom_samples,
                    fnk_body,
                    &self.mem,
                    &self.persistence,
                    &self.session_persistent,
                    tutorial_state orelse .none,
                ),
            };
            self.scoring_run = undefined;
        }

        pub fn update(self: *Self, delta_seconds: f32) !void {
            if (1.0 / delta_seconds < 40) {
                std.log.info("Low FPS: {d}", .{1.0 / delta_seconds});
            }
            if (self.pending_upload_savegame) |reader| {
                const player_data_ascii = reader.readAllAlloc(self.mem.gpa, std.math.maxInt(usize)) catch |err| switch (err) {
                    error.FileNotReady => return,
                    else => return err,
                };
                const player_data = try PlayerData.fromAsciiNew(player_data_ascii, &self.mem);
                try platform.setPlayerData(player_data, &self.mem);
                self.persistence = player_data;
                self.state = .{ .level_select = try .init(&self.persistence) };
                self.pending_upload_savegame = null;
            }
            switch (self.state) {
                .level_select => |*ui| switch (ui.update(delta_seconds)) {
                    .nothing => {},
                    .selected => |level_index| {
                        self.state = .{
                            .loading_editing_fnk = .initFromLevelSelect(ui.*, level_index),
                        };
                    },
                    .uploading => {
                        self.pending_upload_savegame = platform.uploadPlayerData();
                    },
                },
                .loading_editing_fnk => |*anim| if (anim.update(delta_seconds)) {
                    try self.initEditingAndMaybeFindBuiltinLevel(anim.fnk_name);
                },
                .editing_fnk => |*editing| switch (try editing.update(delta_seconds)) {
                    .nothing => {},
                    .back_to_level_select => {
                        try editing.persist(&self.persistence);
                        if (self.prev_editing_names.pop()) |prev| {
                            try self.initEditingAndMaybeFindBuiltinLevel(prev);
                        } else {
                            self.state = .{ .level_select = try .init(&self.persistence) };
                        }
                    },
                    .launch_test => |which| {
                        try editing.persist(&self.persistence);
                        try editing.resetSolvedSamples(which);
                        const prev_editing = editing.*;
                        self.state = .{ .testing_fnk = .{ .main = try .init(
                            editing.camera,
                            which,
                            try std.mem.concat(self.mem.gpa, TestCase, &.{ editing.samples, editing.custom_tests.items }),
                            editing.fnk_name,
                            editing.cases,
                            try .initFromFnks(self.persistence.fnks, &self.mem),
                            editing.tests_reel,
                        ), .prev_editing = prev_editing } };
                    },
                    .launch_execution => |input| {
                        const fnk = try editing.getFnk();
                        try editing.persist(&self.persistence);
                        self.scoring_run = try core.ScoringRun.initFromFnks(
                            self.persistence.fnks,
                            &self.mem,
                        );
                        const prev_editing = editing.*;
                        self.state = .{ .executing_fnk = .{ .main = try .init(
                            if (DESIGN.no_current_data) input else editing.main_input,
                            fnk.name,
                            &self.scoring_run,
                            editing.camera,
                            null,
                        ), .prev_editing = prev_editing } };
                    },
                    .change_to => |foo| {
                        try self.prev_editing_names.append(platform.gpa, editing.fnk_name);
                        try editing.persist(&self.persistence);
                        self.state = .{
                            .loading_editing_fnk = .initFromPoint(foo.fnk_name, Camera.remap(
                                UI.cam,
                                foo.ui_point,
                                // EditingFnk(platform, drawer).FnkManager.sexpr_point,
                                editing.camera,
                            )),
                        };
                    },
                },
                // TODO
                .executing_fnk => |*executing| switch (try executing.main.update(delta_seconds)) {
                    .nothing => {},
                    .back_to_editing => self.state = .{ .editing_fnk = executing.prev_editing },
                },
                .testing_fnk => |*testing| switch (try testing.main.update(delta_seconds, &self.mem)) {
                    .nothing => {},
                    .back_to_editing => {
                        testing.prev_editing.tests_reel = testing.main.tests_reel;
                        try testing.prev_editing.updateSolvedSamplesHelper(testing.main.which);
                        self.state = .{ .editing_fnk = testing.prev_editing };
                    },
                },
                inline else => |*x| x.update(delta_seconds),
            }
        }

        pub fn draw(self: Self) OoM!void {
            drawer.clear(Color.gray(128));
            drawer.asdfBackground();
            try switch (self.state) {
                inline .testing_fnk, .executing_fnk => |x| x.main.draw(),
                .editing_fnk => |x| x.draw(self.persistence.custom_samples),
                inline else => |x| x.draw(),
            };
        }
    };
}

// pub fn Template(platform: Platform, drawer: Drawer) type {
//     _ = platform;
//     _ = drawer;
//     return struct {
//         const Self = @This();
//         pub fn init() Self {}
//         pub fn update(self: *Self, delta_seconds: f32) void {
//             _ = self;
//             _ = delta_seconds;
//         }
//         pub fn draw(self: Self) void {
//             _ = self;
//         }
//     };
// }

pub const Sample = struct {
    input: *const Sexpr,
    output: ?*const Sexpr,

    const Part = enum { input, output };
    const Address = struct {
        index: usize,
        which: Sample.Part,
        local: core.SexprAddress,
    };
    fn get(self: Sample, which: Part) ?*const Sexpr {
        return switch (which) {
            .input => self.input,
            .output => self.output,
        };
    }
};

const TestCase = struct {
    input: *const Sexpr,
    expected: ?*const Sexpr,
    actual: union(enum) {
        unknown,
        undefined,
        value: *const Sexpr,
    },

    fn isSolved(self: TestCase) bool {
        if (self.expected) |expected| {
            return switch (self.actual) {
                else => false,
                .value => |v| v.equals(expected),
            };
        } else return true;
    }

    const Part = enum { input, expected, actual };
    const Address = struct {
        index: usize,
        which: Part,
        local: core.SexprAddress,
    };
    fn get(self: TestCase, which: Part) ?*const Sexpr {
        return switch (which) {
            .input => self.input,
            .expected => self.expected,
            .actual => switch (self.actual) {
                .value => |v| v,
                else => null,
            },
        };
    }

    fn setSexpr(self: *TestCase, which: Part, local: core.SexprAddress, mem: *VeryPermamentGameStuff, value: *const Sexpr) !void {
        switch (which) {
            .input => self.input = try self.input.setAt(mem, local, value),
            .expected => self.expected = try self.expected.?.setAt(mem, local, value),
            .actual => unreachable,
        }
    }

    pub fn allSolvedBase(vs: []const TestCase) bool {
        for (vs) |v| {
            if (v.expected) |x| {
                if (v.get(.actual)) |y| {
                    if (!x.equals(y)) return false;
                } else return false;
            }
        } else return true;
    }
};

const TutorialState = union(enum) {
    none,
    /// hardcoded map
    first_level,
    /// wildcard
    second_level,
    /// apply hardcoded map to unwrapped
    third_level,
    /// nested case
    fourth_level,
    /// free level
    fifth_level,
    not_yet_creating_vaus_or_lists,
    intro_to_list_viewer,
    not_yet_creating_vaus,
    intro_to_create_vaus,

    pub fn allowMeta(self: TutorialState) bool {
        // TODO
        return self == .none;
    }

    pub fn allowCustomTests(self: TutorialState) bool {
        // TODO: tutorial text
        return switch (self) {
            .first_level,
            .second_level,
            .third_level,
            .fourth_level,
            .fifth_level,
            => false,
            else => true,
        };
    }

    pub fn allowPickingVaus(self: TutorialState) bool {
        // TODO: hardcode to true so the player can save the first vau
        if (true) return true;
        return switch (self) {
            .first_level, .second_level => false,
            else => true,
        };
    }

    pub fn allowCreatingVaus(self: TutorialState) bool {
        return switch (self) {
            .none, .intro_to_create_vaus => true,
            else => false,
        };
    }

    pub fn allowGrabbingCases(self: TutorialState) bool {
        return self != .first_level;
    }

    pub fn allowPickingIdentity(self: TutorialState) bool {
        return self == .none;
    }

    pub fn hasListViewer(self: TutorialState) bool {
        return switch (self) {
            .none,
            .intro_to_list_viewer,
            .not_yet_creating_vaus,
            .intro_to_create_vaus,
            => true,
            else => false,
        };
    }

    pub fn hasSexprHolders(self: TutorialState) bool {
        // TODO
        _ = self;
        return true;
    }
};

// TODO: move most of this and have it be a general level (since custom levels should allow custom description)
pub const BuiltinLevel = struct {
    // TODO: remove fnk_name, making it a key in a hashmap?
    fnk_name: *const Sexpr,
    // TODO: remove the defaul value
    solution: *const fn (input: *const Sexpr, mem: *VeryPermamentGameStuff) OoM!?*const Sexpr = struct {
        fn anon(input: *const Sexpr, mem: *VeryPermamentGameStuff) OoM!?*const Sexpr {
            _ = input;
            _ = mem;
            @panic("TODO");
        }
    }.anon,
    manual_samples: []const Sample,
    description: [:0]const u8,
    premade_solution: ?[]const u8,
    tutorial_state: TutorialState,

    // TODO: have a comptime pool of Sexprs so this works for solutions that actually use mem
    pub fn init(
        fnk_name: *const Sexpr,
        solution: *const fn (input: *const Sexpr, mem: *VeryPermamentGameStuff) OoM!?*const Sexpr,
        comptime manual_inputs: []const *const Sexpr,
        description: [:0]const u8,
        premade_solution: ?[]const u8,
    ) BuiltinLevel {
        var manual_samples: [manual_inputs.len]Sample = undefined;
        for (manual_inputs, &manual_samples) |input, *sample| {
            sample.input = input;
            sample.output = solution(input, undefined);
        }
        const manual_samples_done = manual_samples;
        return BuiltinLevel{
            .fnk_name = fnk_name,
            .solution = solution,
            .manual_samples = &manual_samples_done,
            .description = description,
            .premade_solution = premade_solution,
        };
    }
};

pub const builtin_levels: []const BuiltinLevel = @import("levels.zig").builtin_levels;

// code smell
fn findBuiltinLevel(fnk_name: *const Sexpr) ?BuiltinLevel {
    // TODO: change the loop into hashmap?
    for (builtin_levels) |level| {
        if (fnk_name.equals(level.fnk_name)) return level;
    }
    return null;
}

/// Like Drawer, but higher level
fn Artist(platform: Platform, drawer: Drawer) type {
    const AtomVisualCache = struct {
        var visuals_cache: std.StringHashMap(AtomVisuals) = std.StringHashMap(AtomVisuals).init(platform.gpa);

        const HardcodedAtomVisuals = struct {
            profile: ?[]const Vec2,
            color: Color,
            display: ?[]const u8 = null,
        };
        const hardcoded_visuals = .{
            .identity = HardcodedAtomVisuals{
                .color = Color.white,
                .profile = &.{},
            },
            .nil = HardcodedAtomVisuals{
                .color = .from01(0.45, 0.45, 0.45),
                .profile = &.{.new(0.75, -0.25)},
            },
            .input = HardcodedAtomVisuals{
                .color = .from01(0.1, 0.6, 0.6),
                .profile = &.{ .new(0.2, 0.2), .new(0.8, 0.2) },
            },
            .true = HardcodedAtomVisuals{
                .color = .from01(0.5, 0.9, 0.5),
                .profile = &blk: {
                    const N = 10;
                    var buffer: [N]Vec2 = undefined;
                    for (0..N) |k| {
                        const t = tof32(k) / N;
                        buffer[k] = Vec2.new(t, -0.2 * @sin(t * std.math.pi));
                    }
                    const res = buffer;
                    break :blk res;
                },
            },
            .false = HardcodedAtomVisuals{
                .color = .from01(0.9, 0.5, 0.5),
                .profile = &.{ .new(1.0 / 6.0, 0.2), .new(0.5, -0.2), .new(5.0 / 6.0, 0.2) },
            },

            //  Zeus -> Jupiter;
            .Hermes = HardcodedAtomVisuals{
                .color = .fromHex("#FA00FF"),
                .profile = null,
                .display = "A",
            },
            .Mercury = HardcodedAtomVisuals{
                .color = .fromHex("#FF8EEC"),
                .profile = &.{
                    .new(1.224892e-1, 1.97281936e-1),
                    .new(4.2850158e-1, -7.022254e-2),
                    .new(5.059528e-1, -1.8252338e-1),
                    .new(6.5015405e-1, -1.3726442e-1),
                    .new(8.53909e-1, -3.479591e-2),
                },
                .display = "a",
            },
            .Aphrodite = HardcodedAtomVisuals{
                .color = .fromHex("#FFB600"),
                .profile = null,
                .display = "B",
            },
            .Venus = HardcodedAtomVisuals{
                .color = .fromHex("#FFE18E"),
                .profile = &.{
                    .new(0.7142284e-1, 1.6622247e-1),
                    .new(1.341461e-1, 1.9398443e-1),
                    .new(2.3471789e-1, 1.4246653e-1),
                    .new(2.947409e-1, -1.603865e-1),
                    .new(3.9733455e-1, -1.6812957e-1),
                    .new(4.707473e-1, -1.522803e-1),
                    .new(5.3846923e-1, 1.506581e-1),
                    .new(5.7485698e-1, 1.3897786e-1),
                    .new(6.1954176e-1, 1.56606e-1),
                    .new(7.7189485e-1, -1.6031578e-1),
                    .new(8.802021e-1, -1.4742844e-1),
                    .new(9.332106e-1, -1.6913065e-1),
                },
                .display = "b",
            },
            .Ares = HardcodedAtomVisuals{
                .color = .fromHex("#00E5FF"),
                .profile = null,
                .display = "C",
            },
            .Mars = HardcodedAtomVisuals{
                .color = .fromHex("#9EFFF2"),
                .profile = null,
                .display = "c",
            },
            .Zeus = HardcodedAtomVisuals{
                .color = .fromHex("#97F200"),
                .profile = null,
                .display = "D",
            },
            .Jupiter = HardcodedAtomVisuals{
                .color = .fromHex("#C8ED8F"),
                .profile = null,
                .display = "d",
            },
        };

        pub fn init() !void {
            inline for (std.meta.fields(@TypeOf(hardcoded_visuals))) |field| {
                const atom_name = field.name;
                const input = @field(hardcoded_visuals, field.name);
                const atom_visuals: AtomVisuals = .{
                    .color = input.color,
                    .profile = input.profile orelse try newAtomProfile(atom_name),
                    .display = input.display,
                };
                try visuals_cache.put(atom_name, atom_visuals);
            }
        }

        fn newAtomProfile(name: []const u8) ![]const Vec2 {
            const seed = std.array_hash_map.hashString(name);
            var rnd_state = std.Random.DefaultPrng.init(seed);
            var rnd = Random{ .rnd = rnd_state.random() };

            const profile = try platform.gpa.alloc(Vec2, rnd.rnd.intRangeLessThan(usize, 2, 15));
            for (profile) |*p| {
                p.* = Vec2.new(rnd.between(0, 1), rnd.around0(0.2));
            }
            std.mem.sortUnstable(Vec2, profile, {}, struct {
                pub fn lessThanFn(context: void, lhs: Vec2, rhs: Vec2) bool {
                    _ = context;
                    return lhs.x < rhs.x;
                }
            }.lessThanFn);
            // std.log.debug("new profile for {s}:\n{any}", .{ name, profile });
            return profile;
        }

        fn newAtomColor(name: []const u8) !Color {
            const seed = std.array_hash_map.hashString(name);
            var rnd_state = std.Random.DefaultPrng.init(seed);
            var rnd = Random{ .rnd = rnd_state.random() };
            const color = rnd.color();
            return color;
        }

        pub fn getAtomVisuals(name: []const u8) !AtomVisuals {
            const v = try visuals_cache.getOrPut(name);
            if (!v.found_existing) {
                const res = AtomVisuals{
                    .color = try newAtomColor(name),
                    .profile = try newAtomProfile(name),
                };
                v.value_ptr.* = res;
            }
            return v.value_ptr.*;
        }
    };

    return struct {
        var highlighted_var: ?[]const u8 = null;

        pub fn init() !void {
            return AtomVisualCache.init();
        }

        // TODO NOW: pass scale to wildcard lines

        fn getAllVarVisuals(value: *const Sexpr) !std.ArrayList(AtomVisuals) {
            const gpa = platform.gpa;
            var names: std.ArrayList([]const u8) = .init(gpa);
            defer names.deinit();
            try value.getAllVarNames(&names);
            var result: std.ArrayList(AtomVisuals) = try .initCapacity(gpa, names.items.len);
            for (names.items) |name| {
                try result.append(try AtomVisualCache.getAtomVisuals(name));
            }
            return result;
        }

        fn drawTemplateWildcardLinesNonRecursive(
            camera: Camera,
            left: *const Sexpr,
            right: *const Sexpr,
            point: Point,
            bindings: BindingsState,
        ) !void {
            var left_names: std.ArrayList([]const u8) = .init(platform.gpa);
            defer left_names.deinit();
            try left.getAllVarNames(&left_names);
            if (bindings.anim_t) |anim_t| if (anim_t >= 0.4) {
                try removeBoundNames(&left_names, bindings.new);
            };
            try removeBoundNames(&left_names, bindings.old);

            var right_names: std.ArrayList([]const u8) = .init(platform.gpa);
            defer right_names.deinit();
            try right.getAllVarNames(&right_names);
            if (bindings.anim_t) |anim_t| if (anim_t >= 0.4) {
                try removeBoundNames(&right_names, bindings.new);
            };
            try removeBoundNames(&right_names, bindings.old);

            if (!DESIGN.round_data) {
                try drawWildcardsCable(camera, &.{
                    point.applyToLocalPosition(.new(-0.5, 0)),
                    point.applyToLocalPosition(.new(0, -0.5)),
                    point.applyToLocalPosition(.new(0.25, -0.5)),
                }, left_names.items);

                try drawWildcardsCable(camera, &.{
                    point.applyToLocalPosition(.new(-0.5, 0)),
                    point.applyToLocalPosition(.new(0, 0.5)),
                    point.applyToLocalPosition(.new(0.25, 0.5)),
                }, right_names.items);
            } else {
                // TODO: these numbers are not exact, issues when zooming in
                try drawWildcardsCable(camera, &([1]Vec2{
                    point.applyToLocalPosition(.new(-0.5, 0)),
                } ++ funk.fromCountAndCtx(32, struct {
                    pub fn anon(k: usize, p: Point) Vec2 {
                        return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.5 + 0.25 / 2.0, 0.75, math.tof32(k) / 32)).scale(0.75).add(.new(0.25, 0.25)));
                    }
                }.anon, point)), left_names.items);

                try drawWildcardsCable(camera, &([1]Vec2{
                    point.applyToLocalPosition(.new(-0.5, 0)),
                } ++ funk.fromCountAndCtx(32, struct {
                    pub fn anon(k: usize, p: Point) Vec2 {
                        return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.5 - 0.25 / 2.0, 0.25, math.tof32(k) / 32)).scale(0.75).add(.new(0.25, -0.25)));
                    }
                }.anon, point)), right_names.items);
            }
        }

        pub fn drawTemplateWildcardLines(camera: Camera, value: *const Sexpr, point: Point) !void {
            switch (value.*) {
                else => {},
                .pair => |pair| {
                    try drawTemplateWildcardLinesNonRecursive(camera, pair.left, pair.right, point, .none);
                    try drawTemplateWildcardLines(camera, pair.left, point.applyToLocalPoint(OFFSET_TEMPLATE_PAIR_LEFT));
                    try drawTemplateWildcardLines(camera, pair.right, point.applyToLocalPoint(OFFSET_TEMPLATE_PAIR_RIGHT));
                },
            }
        }

        fn drawPatternWildcardLinesNonRecursive(camera: Camera, left: *const Sexpr, right: *const Sexpr, point: Point) !void {
            var left_visuals = try getAllVarVisuals(left);
            defer left_visuals.deinit();
            var right_visuals = try getAllVarVisuals(right);
            defer right_visuals.deinit();
            if (!DESIGN.round_data) {
                if (true) {
                    drawer.drawWildcardsCable(camera, &.{
                        point.applyToLocalPosition(.new(-0.75, -0.5)),
                        point.applyToLocalPosition(.new(-0.5, -0.5)),
                        point.applyToLocalPosition(.new(0, 0)),
                        point.applyToLocalPosition(.new(0.5, 0)),
                    }, left_visuals.items);

                    drawer.drawWildcardsCable(camera, &.{
                        point.applyToLocalPosition(.new(-0.75, 0.5)),
                        point.applyToLocalPosition(.new(-0.5, 0.5)),
                        point.applyToLocalPosition(.new(0, 0)),
                        point.applyToLocalPosition(.new(0.5, 0)),
                    }, right_visuals.items);
                } else {
                    // TODO: delete this
                    drawer.drawWildcardsCable(camera, &.{
                        point.applyToLocalPosition(.new(-0.75, -0.5)),
                        point.applyToLocalPosition(.new(-0.5 + 0.25 / 2.0, -0.5)),
                        point.applyToLocalPosition(.new(0.25 / 2.0, 0)),
                        point.applyToLocalPosition(.new(0.5, 0)),
                    }, left_visuals.items);

                    drawer.drawWildcardsCable(camera, &.{
                        point.applyToLocalPosition(.new(-0.75, 0.5)),
                        point.applyToLocalPosition(.new(-0.5 + 0.25 / 2.0, 0.5)),
                        point.applyToLocalPosition(.new(0.25 / 2.0, 0)),
                        point.applyToLocalPosition(.new(0.5, 0)),
                    }, right_visuals.items);
                }
            } else {
                const magic_1 = @sqrt(2.0) * 2.0 / 4.0;
                const magic_2 = @sqrt(2.0) * 3.0 / 4.0;
                drawer.drawWildcardsCable(camera, &(funk.fromCountAndCtx(32, struct {
                    pub fn anon(k: usize, p: Point) Vec2 {
                        return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(-0.25, -0.25 / 2.0, math.tof32(k) / 32)).scale(magic_1).add(.new(-0.75, -0.5)).addY(magic_1));
                    }
                }.anon, point) ++ funk.fromCountAndCtx(32, struct {
                    pub fn anon(k: usize, p: Point) Vec2 {
                        return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.25 + 0.25 / 2.0, 0.25, math.tof32(k) / 32)).scale(magic_2).add(.new(0.5, 0)).addY(-magic_2));
                    }
                }.anon, point)), left_visuals.items);
                drawer.drawWildcardsCable(camera, &(funk.fromCountAndCtx(32, struct {
                    pub fn anon(k: usize, p: Point) Vec2 {
                        return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.25, 0.25 / 2.0, math.tof32(k) / 32)).scale(magic_1).add(.new(-0.75, 0.5)).addY(-magic_1));
                    }
                }.anon, point) ++ funk.fromCountAndCtx(32, struct {
                    pub fn anon(k: usize, p: Point) Vec2 {
                        return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(-0.25 - 0.25 / 2.0, -0.25, math.tof32(k) / 32)).scale(magic_2).add(.new(0.5, 0)).addY(magic_2));
                    }
                }.anon, point)), right_visuals.items);
            }
        }

        pub fn drawPatternWildcardLines(camera: Camera, value: *const Sexpr, point: Point) !void {
            switch (value.*) {
                else => {},
                .pair => |pair| {
                    try drawPatternWildcardLinesNonRecursive(camera, pair.left, pair.right, point);
                    try drawPatternWildcardLines(camera, pair.left, point.applyToLocalPoint(OFFSET_PATTERN_PAIR_LEFT));
                    try drawPatternWildcardLines(camera, pair.right, point.applyToLocalPoint(OFFSET_PATTERN_PAIR_RIGHT));
                },
            }
        }

        pub fn drawPlacedWildcardsCable(
            camera: Camera,
            pattern_point: Point,
            template_point: Point,
            pattern_value: *const Sexpr,
            template_value: *const Sexpr,
            held_wildcard_names: ?[]const []const u8,
            inbound_wildcard_names: []const []const u8,
            outbound_wildcard_names: []const []const u8,
            bindings: BindingsState,
            // TODO: properly use this argument
            hiding_children: f32,
            // TODO: relative address? (maybe pass the casegroup, then)
        ) !void {
            // // TODO: avoid memory management here by having a single scratch allocator for the whole frame/drawing

            var names: std.ArrayList([]const u8) = .init(platform.gpa);
            defer names.deinit();

            try template_value.getAllVarNames(&names);
            try removeBoundNamesV3(&names, bindings);
            if (hiding_children < 1) try drawWildcardsCable(camera, &.{
                pattern_point.applyToLocalPosition(.new(1, 0)),
                template_point.applyToLocalPosition(.new(-0.5, 0)),
            }, names.items);

            try appendUniqueNames(&names, outbound_wildcard_names);
            try removeBoundNamesV3(&names, bindings);
            if (hiding_children < 1) try drawWildcardsCable(camera, &.{
                pattern_point.applyToLocalPosition(.new(0.5, 0)),
                pattern_point.applyToLocalPosition(.new(1, 0)),
            }, names.items);

            // TODO: draws duplicated wildcard names!
            if (DESIGN.round_data) {
                try drawWildcardsCable(camera, &([1]Vec2{
                    // this -3 assumes not gen0
                    pattern_point.applyToLocalPosition(.new(-3 + hiding_children, 1)),
                } ++ funk.fromCountAndCtx(32, struct {
                    pub fn anon(k: usize, p: Point) Vec2 {
                        return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.25, 0, math.tof32(k) / 32)).add(.new(-0.5, 0)));
                    }
                }.anon, pattern_point)), try removeBoundNamesV2(platform.gpa, inbound_wildcard_names, bindings));
            } else {
                try drawWildcardsCable(camera, &.{
                    // this -3 assumes not gen0
                    pattern_point.applyToLocalPosition(.new(-3 + hiding_children, 1)),
                    pattern_point.applyToLocalPosition(.new(0, 1)),
                    pattern_point.applyToLocalPosition(.new(0.5, 0)),
                }, try removeBoundNamesV2(platform.gpa, inbound_wildcard_names, bindings));
            }

            const lost_wildcards = try visualsForUnusedWildcards(pattern_value, template_value, outbound_wildcard_names, held_wildcard_names);
            defer platform.gpa.free(lost_wildcards);
            drawer.drawWildcardsCable(camera, &.{
                pattern_point.applyToLocalPosition(.new(0.5, 0)),
                pattern_point.applyToLocalPosition(.new(0.75, -0.1)),
                pattern_point.applyToLocalPosition(.new(0.65, -0.25)),
                pattern_point.applyToLocalPosition(.new(0.75, -0.30)),
            }, lost_wildcards);
        }

        /// visuals for the wildcards present in pattern but not template, outbound names, or held names
        fn visualsForUnusedWildcards(
            pattern: *const Sexpr,
            template: *const Sexpr,
            outbound_names: []const []const u8,
            held_wildcard_names: ?[]const []const u8,
        ) ![]const AtomVisuals {
            // TODO: better memory management

            const gpa = platform.gpa;

            var names: std.ArrayList([]const u8) = .init(gpa);
            defer names.deinit();

            try pattern.getAllVarNames(&names);
            template.removeAllVarNames(&names);
            for (outbound_names) |name_to_remove| {
                while (funk.indexOfString(names.items, name_to_remove)) |i| {
                    std.debug.assert(std.mem.eql(u8, name_to_remove, names.swapRemove(i)));
                }
            }
            if (held_wildcard_names) |held_names| {
                for (held_names) |name_to_remove| {
                    while (funk.indexOfString(names.items, name_to_remove)) |i| {
                        std.debug.assert(std.mem.eql(u8, name_to_remove, names.swapRemove(i)));
                    }
                }
            }

            var diff: std.ArrayList(AtomVisuals) = .init(platform.gpa);
            for (names.items) |name| {
                try diff.append(try AtomVisualCache.getAtomVisuals(name));
            }

            return try diff.toOwnedSlice();
        }

        /// visuals for the wildcards present in pattern and template
        fn visualsForCommonWildcards(pattern: *const Sexpr, template: *const Sexpr) ![]const AtomVisuals {
            // TODO: better memory management

            const gpa = platform.gpa;

            var pattern_names: std.ArrayList([]const u8) = .init(gpa);
            defer pattern_names.deinit();
            try pattern.getAllVarNames(&pattern_names);

            var template_names: std.ArrayList([]const u8) = .init(gpa);
            defer template_names.deinit();
            try template.getAllVarNames(&template_names);

            var common: std.ArrayList(AtomVisuals) = .init(platform.gpa);

            for (pattern_names.items) |p| {
                for (template_names.items) |t| {
                    if (std.mem.eql(u8, p, t)) {
                        try common.append(try AtomVisualCache.getAtomVisuals(p));
                    }
                }
            }

            return try common.toOwnedSlice();
        }

        fn drawWildcardsCable(camera: Camera, points: []const Vec2, names: []const []const u8) !void {
            var visuals: std.ArrayList(AtomVisuals) = try .initCapacity(platform.gpa, names.len);
            defer visuals.deinit();
            for (names) |name| {
                visuals.appendAssumeCapacity(try AtomVisualCache.getAtomVisuals(name));
            }
            drawer.drawWildcardsCable(camera, points, visuals.items);
        }

        // TODO: better memory management
        pub fn drawWildcardLinesToFloating(camera: Camera, parent_cases_point: Point, cases: CaseGroup, grabbing_point: Point, grabbing_wildcards: []const []const u8) !void {
            if (cases.getUnfoldedChild()) |case| {
                const pattern_point = parent_cases_point.applyToLocalPoint(case.pattern_point_relative_to_parent);

                var wildcard_names: std.ArrayList([]const u8) = .init(platform.gpa);
                defer wildcard_names.deinit();
                try case.pattern.getAllVarNames(&wildcard_names);

                var common: std.ArrayList(AtomVisuals) = .init(platform.gpa);
                defer common.deinit();

                for (wildcard_names.items) |pattern| {
                    for (grabbing_wildcards) |grabbing| {
                        if (std.mem.eql(u8, pattern, grabbing)) {
                            try common.append(try AtomVisualCache.getAtomVisuals(pattern));
                        }
                    }
                }

                drawer.drawWildcardsCable(camera, &.{
                    pattern_point.applyToLocalPosition(.new(0.5, 0)),
                    grabbing_point.applyToLocalPosition(.new(-0.5, 0)),
                }, common.items);

                // const visuals = try AtomVisualCache.getAtomVisuals(pattern);
                // drawer.drawLine(camera, &.{
                //     pattern_point.applyToLocalPosition(.new(0.5, 0)),
                //     grabbing_point.applyToLocalPosition(.new(-0.5, 0)),
                // }, visuals.color);

                if (case.next) |next| {
                    try drawWildcardLinesToFloating(camera, pattern_point, next, grabbing_point, grabbing_wildcards);
                }
            }
        }

        pub fn drawFnkHolderForFnkAt(camera: Camera, fnk_point: Point, is_main: f32) void {
            drawer.drawFnkHolder(camera, fnk_point
                .applyToLocalPoint(.{ .scale = lerp(1, 0.5, is_main) })
                .applyToLocalPoint(.{ .pos = .new(lerp(-1.5, -2.5, is_main), 0), .turns = 0.25 }));
        }

        pub fn drawHoldedFnk(camera: Camera, fnk_point: Point, is_main: f32, value: *const Sexpr) !void {
            drawFnkHolderForFnkAt(camera, fnk_point, is_main);
            if (!value.equals(Sexpr.builtin.identity)) {
                try drawSexpr(
                    camera,
                    fnk_point,
                    value,
                );
            }
        }

        pub fn drawOffscreenCableTo(camera: Camera, pattern: Point) void {
            // TODO: store some state to avoid cable jumps? or maybe make the cable periodic
            drawer.drawCable(camera, pattern.applyToLocalPosition(.new(-CABLE_OFFSCREEN_DIST, 0)), pattern.applyToLocalPosition(.new(-0.5, 0)), pattern.scale, -CABLE_OFFSCREEN_DIST);
        }

        pub fn drawCableTo(camera: Camera, origin: Vec2, pattern: Point) void {
            drawer.drawCable(camera, origin, pattern.applyToLocalPosition(.new(-0.5, 0)), pattern.scale, 0);
        }

        pub fn drawPatternOutline(camera: Camera, world_point: Point) !void {
            drawer.drawPatternAtomOutline(camera, world_point);
        }

        pub fn drawVariable(camera: Camera, world_point: Point, name: []const u8) !void {
            const visuals = try AtomVisualCache.getAtomVisuals(name);
            if (highlighted_var != null and std.mem.eql(u8, name, highlighted_var.?)) {
                drawer.drawVariable(camera, world_point.applyToLocalPoint(.{ .scale = 1.1 }), visuals);
            } else {
                drawer.drawVariable(camera, world_point, visuals);
            }
        }

        pub fn drawPatternVariable(camera: Camera, world_point: Point, name: []const u8) !void {
            const visuals = try AtomVisualCache.getAtomVisuals(name);
            if (highlighted_var != null and std.mem.eql(u8, name, highlighted_var.?)) {
                drawer.drawPatternVariable(camera, world_point.applyToLocalPoint(.{ .scale = 1.1 }), visuals);
            } else {
                drawer.drawPatternVariable(camera, world_point, visuals);
            }
        }

        pub fn drawAtom(camera: Camera, world_point: Point, name: []const u8) !void {
            const visuals = try AtomVisualCache.getAtomVisuals(name);
            drawer.drawAtom(camera, world_point, visuals);
        }

        pub fn drawPatternAtom(camera: Camera, world_point: Point, name: []const u8) !void {
            const visuals = try AtomVisualCache.getAtomVisuals(name);
            drawer.drawPatternAtom(camera, world_point, visuals);
        }

        const BindingParticle = struct {
            point: Point,
            t: f32,
            name: []const u8,
        };

        pub fn drawSexprWithBindings(camera: Camera, world_point: Point, sexpr: *const Sexpr, bindings: BindingsState) !void {
            var out_particles: std.ArrayList(BindingParticle) = .init(platform.gpa);
            defer out_particles.deinit();
            try _drawSexprWithBindings(camera, world_point, sexpr, bindings, &out_particles);
            for (out_particles.items) |particle| {
                const visuals = try AtomVisualCache.getAtomVisuals(particle.name);
                drawer.drawRect(
                    camera,
                    .fromCenterAndSize(
                        particle.point.applyToLocalPosition(.new(1, 0)),
                        .both(lerp(7.5, 2.5, particle.t) * particle.point.scale),
                    ),
                    visuals.color.toFColor().lighter().lighter().toUColor(),
                    null,
                );
            }
        }

        fn _drawSexprWithBindings(camera: Camera, world_point: Point, sexpr: *const Sexpr, bindings: BindingsState, out_particles: *std.ArrayList(BindingParticle)) !void {
            switch (sexpr.*) {
                .atom_lit => |lit| {
                    try drawAtom(camera, world_point, lit.value);
                },
                .pair => |pair| {
                    try _drawSexprWithBindings(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(0.5, -0.5),
                        .scale = 0.5,
                    }), pair.left, bindings, out_particles);
                    try _drawSexprWithBindings(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(0.5, 0.5),
                        .scale = 0.5,
                    }), pair.right, bindings, out_particles);
                    drawer.drawPairHolder(camera, world_point);
                    try drawTemplateWildcardLinesNonRecursive(camera, pair.left, pair.right, world_point, bindings);
                },
                .atom_var => |x| {
                    // TODO: check that compiler skips the loop if anim_t is null
                    for (bindings.new) |binding| {
                        if (bindings.anim_t) |anim_t| {
                            if (std.mem.eql(u8, binding.name, x.value)) {
                                drawer.clipAtomRegion(camera, world_point);
                                const t = math.smoothstep(anim_t, 0, 0.4);
                                try drawSexpr(camera, world_point.applyToLocalPoint(.{ .pos = .new(remap(t, 0, 1, -2.3, 0), 0) }), binding.value);
                                drawer.endClip();

                                drawer.setTransparency(1 - anim_t);
                                try drawVariable(camera, world_point, x.value);
                                drawer.setTransparency(1);

                                if (anim_t < 0.5) {
                                    try out_particles.append(.{ .point = world_point, .t = t, .name = binding.name });
                                }

                                break;
                            }
                        }
                    } else for (bindings.old) |binding| {
                        if (std.mem.eql(u8, binding.name, x.value)) {
                            try drawSexpr(camera, world_point, binding.value);
                            break;
                        }
                    } else {
                        try drawVariable(camera, world_point, x.value);
                    }
                },
            }
        }

        // unbalance 0 = full left, 1 = full right
        fn unbalancedOffset(which: core.SexprAddressItem, unbalance: f32) Point {
            return switch (which) {
                .left => .{ .scale = 1 - unbalance, .pos = .new(unbalance, -unbalance) },
                .right => .{ .scale = unbalance, .pos = .new(1 - unbalance, 1 - unbalance) },
            };
        }

        pub fn drawPairHolderUnbalanced(camera: Camera, world_point: Point, unbalance: f32) void {
            const local_positions = if (DESIGN.round_data)
                funk.fromCount(32, struct {
                    pub fn anon(k: usize) Vec2 {
                        return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                    }
                }.anon) ++ funk.fromCountAndCtx(32, struct {
                    pub fn anon(k: usize, ctx_unbalance: f32) Vec2 {
                        const local: Vec2 = Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).addX(0.5);
                        return unbalancedOffset(.right, ctx_unbalance).applyToLocalPosition(local);
                    }
                }.anon, unbalance) ++ funk.fromCountAndCtx(32, struct {
                    pub fn anon(k: usize, ctx_unbalance: f32) Vec2 {
                        const local = Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).addX(0.5);
                        return unbalancedOffset(.left, ctx_unbalance).applyToLocalPosition(local);
                    }
                }.anon, unbalance)
            else
                // TODO: this
                [_]Vec2{
                    Vec2.new(-0.5, 0),
                    Vec2.new(0, 1),
                    Vec2.new(0.5, 1),
                    Vec2.new(0.25, 0.5),
                    Vec2.new(0.5, 0),
                    Vec2.new(0.25, -0.5),
                    Vec2.new(0.5, -1),
                    Vec2.new(0, -1),
                };
            drawer.drawShapeV2(camera, world_point, &local_positions, .black, .gray(96));
            // drawer.drawShapeV2(camera, world_point, &local_positions, null, .gray(96));
        }

        // TODO: use this somewhere
        pub fn drawSexprUnbalanced(camera: Camera, world_point: Point, sexpr: *const Sexpr, unbalance: f32) !void {
            switch (sexpr.*) {
                .atom_lit => |lit| {
                    try drawAtom(camera, world_point, lit.value);
                },
                .pair => |pair| {
                    drawPairHolderUnbalanced(camera, world_point, unbalance);
                    // TODO
                    // try drawTemplateWildcardLinesNonRecursive(camera, pair.left, pair.right, world_point, .none);
                    try drawSexpr(camera, world_point.applyToLocalPoint(unbalancedOffset(.left, unbalance)), pair.left);
                    try drawSexpr(camera, world_point.applyToLocalPoint(unbalancedOffset(.right, unbalance)), pair.right);
                },
                .atom_var => |x| {
                    try drawVariable(camera, world_point, x.value);
                },
            }
        }

        fn computeUnbalance(a: usize, b: usize) f32 {
            // perfectly balanced
            // return tof32(b + 1) / tof32(a + b + 2);
            // half biased to blindness
            return lerp(tof32(b + 1) / tof32(a + b + 2), 0.5, 0.5);
        }

        pub fn drawSexpr(camera: Camera, world_point: Point, sexpr: *const Sexpr) OoM!void {
            switch (sexpr.*) {
                .atom_lit => |lit| {
                    try drawAtom(camera, world_point, lit.value);
                },
                .pair => |pair| {
                    if (false) {
                        try drawSexprUnbalanced(
                            camera,
                            world_point,
                            sexpr,
                            computeUnbalance(pair.left.getMaxDepth(), pair.right.getMaxDepth()),
                        );
                        return;
                    }

                    drawer.drawPairHolder(camera, world_point);
                    try drawTemplateWildcardLinesNonRecursive(camera, pair.left, pair.right, world_point, .none);
                    try drawSexpr(camera, world_point.applyToLocalPoint(OFFSET_TEMPLATE_PAIR_LEFT), pair.left);
                    try drawSexpr(camera, world_point.applyToLocalPoint(OFFSET_TEMPLATE_PAIR_RIGHT), pair.right);
                },
                .atom_var => |x| {
                    try drawVariable(camera, world_point, x.value);
                },
            }
        }

        pub fn drawPhysicalSexpr(camera: Camera, s: PhysicalSexpr) !void {
            try drawBothSexpr(camera, s.pos, s.is_pattern, s.value);
        }

        pub fn drawBothSexpr(camera: Camera, world_point: Point, is_pattern: f32, sexpr: *const Sexpr) !void {
            std.debug.assert(in01(is_pattern));
            if (is_pattern > 0.5) {
                try drawPatternSexpr(
                    camera,
                    world_point.applyToLocalPoint(.{ .turns = remap(
                        is_pattern,
                        0.5,
                        1,
                        0.5,
                        0,
                    ) }),
                    sexpr,
                );
            } else {
                try drawSexpr(
                    camera,
                    world_point.applyToLocalPoint(.{ .turns = remap(
                        is_pattern,
                        0.5,
                        0,
                        -0.5,
                        0,
                    ) }),
                    sexpr,
                );
            }
        }

        pub fn drawPatternSexpr(camera: Camera, world_point: Point, sexpr: *const Sexpr) !void {
            switch (sexpr.*) {
                .atom_lit => |lit| {
                    try drawPatternAtom(camera, world_point, lit.value);
                },
                .pair => |pair| {
                    drawer.drawPatternPairHolder(camera, world_point);
                    try drawPatternWildcardLinesNonRecursive(camera, pair.left, pair.right, world_point);
                    try drawPatternSexpr(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(-1, -0.5),
                        .scale = 0.5,
                    }), pair.left);
                    try drawPatternSexpr(camera, world_point.applyToLocalPoint(.{
                        .pos = .new(-1, 0.5),
                        .scale = 0.5,
                    }), pair.right);
                },
                .atom_var => |x| {
                    try drawPatternVariable(camera, world_point, x.value);
                },
            }
        }
    };
}
const BindingsState = struct {
    new: []const core.Binding,
    old: []const core.Binding,
    anim_t: ?f32,
    pub const none: BindingsState = .{ .anim_t = null, .new = &.{}, .old = &.{} };
};

const PhysicalSexpr = struct {
    value: *const Sexpr,
    pos: Point,
    is_pattern: f32,

    pub fn updatePattern(self: *PhysicalSexpr, is_pattern: ?bool, delta_seconds: f32) void {
        const target_pattern_value: f32 = if (is_pattern) |v|
            if (v) 1 else 0
        else
            @round(self.is_pattern);
        math.lerp_towards(&self.is_pattern, target_pattern_value, 0.6, delta_seconds);
    }
};
const SexprView = struct {
    pub fn overlapsPatternAtom(atom_point: Point, needle_pos: Vec2, kind: enum { atom, pair }) bool {
        const p = atom_point.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;
        return inRange(p.y, -1, 1) and switch (kind) {
            .atom => inRange(p.x, -1, 0.5 * (1 - @abs(p.y))),
            .pair => inRange(p.x, -1 + 0.25 * (1 - @abs(@abs(p.y) - 0.5) / 0.5), 0.5 * (1 - @abs(p.y))),
        };
    }

    pub fn overlapsSexpr(alloc: std.mem.Allocator, sexpr: *const Sexpr, sexpr_pos: Point, needle_pos: Vec2) !?core.SexprAddress {
        var result = std.ArrayList(core.SexprAddressItem).init(alloc);
        defer result.deinit();
        // TODO (low priority): probably can be made more efficient by using less changes of coordinates

        var cur_sexpr_pos = sexpr_pos;
        var cur_sexpr = sexpr;
        while (true) {
            switch (cur_sexpr.*) {
                .atom_lit, .atom_var => {
                    if (overlapsAtom(cur_sexpr_pos, needle_pos, .atom)) {
                        return try result.toOwnedSlice();
                    } else {
                        return null;
                    }
                },
                .pair => |pair| {
                    const p = cur_sexpr_pos.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;

                    if (overlapsAtom(cur_sexpr_pos, needle_pos, .pair)) {
                        return try result.toOwnedSlice();
                    } else if (inRange(p.y, -1, 0)) {
                        try result.append(.left);
                        cur_sexpr = pair.left;
                        cur_sexpr_pos = cur_sexpr_pos.applyToLocalPoint(.{
                            .pos = .new(0.5, -0.5),
                            .scale = 0.5,
                        });
                    } else if (inRange(p.y, 0, 1)) {
                        try result.append(.right);
                        cur_sexpr = pair.right;
                        cur_sexpr_pos = cur_sexpr_pos.applyToLocalPoint(.{
                            .pos = .new(0.5, 0.5),
                            .scale = 0.5,
                        });
                    } else {
                        return null;
                    }
                },
            }
        }
    }

    pub fn overlapsPatternSexpr(alloc: std.mem.Allocator, sexpr: *const Sexpr, sexpr_pos: Point, needle_pos: Vec2) !?core.SexprAddress {
        var result = std.ArrayList(core.SexprAddressItem).init(alloc);
        defer result.deinit();
        // TODO (low priority): probably can be made more efficient by using less changes of coordinates

        var cur_sexpr_pos = sexpr_pos;
        var cur_sexpr = sexpr;
        while (true) {
            switch (cur_sexpr.*) {
                .atom_lit, .atom_var => {
                    if (overlapsPatternAtom(cur_sexpr_pos, needle_pos, .atom)) {
                        return try result.toOwnedSlice();
                    } else {
                        return null;
                    }
                },
                .pair => |pair| {
                    const p = cur_sexpr_pos.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;

                    if (overlapsPatternAtom(cur_sexpr_pos, needle_pos, .pair)) {
                        return try result.toOwnedSlice();
                    } else if (inRange(p.y, -1, 0)) {
                        try result.append(.left);
                        cur_sexpr = pair.left;
                        cur_sexpr_pos = cur_sexpr_pos.applyToLocalPoint(.{
                            .pos = .new(-1, -0.5),
                            .scale = 0.5,
                        });
                    } else if (inRange(p.y, 0, 1)) {
                        try result.append(.right);
                        cur_sexpr = pair.right;
                        cur_sexpr_pos = cur_sexpr_pos.applyToLocalPoint(.{
                            .pos = .new(-1, 0.5),
                            .scale = 0.5,
                        });
                    } else {
                        return null;
                    }
                },
            }
        }
    }

    pub fn overlapsAtom(atom_point: Point, needle_pos: Vec2, kind: enum { atom, pair }) bool {
        const p = atom_point.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;
        return inRange(p.y, -1, 1) and switch (kind) {
            .pair => inRange(p.x, -0.5 * (1 - @abs(p.y)), 0.5 - 0.25 * (1 - @abs(@abs(p.y) - 0.5) / 0.5)),
            .atom => inRange(p.x, -0.5 * (1 - @abs(p.y)), 2),
        };
    }

    pub fn sexprPatternChildView(parent: Point, address: core.SexprAddress) Point {
        var result = parent;
        for (address) |cur| {
            result = result.applyToLocalPoint(.{
                .pos = switch (cur) {
                    .left => .new(-1, -0.5),
                    .right => .new(-1, 0.5),
                },
                .scale = 0.5,
            });
        }
        return result;
    }

    pub fn sexprChildView(parent: Point, address: core.SexprAddress) Point {
        var result = parent;
        for (address) |cur| {
            result = result.applyToLocalPoint(.{
                .pos = switch (cur) {
                    .left => .new(0.5, -0.5),
                    .right => .new(0.5, 0.5),
                },
                .scale = 0.5,
            });
        }
        return result;
    }
};

const DEFAULT_CAM: Camera = .{ .center = .new(8, 3), .height = 15.0 };
const DIST_TO_TEMPLATE = 5;
const FNK_NAME_OFFSET = Point{
    .pos = .new(DIST_TO_TEMPLATE - 1, -0.75),
    .turns = -0.25,
    .scale = 0.5,
};
const FNK_NAME_OFFSET_FROM_TEMPLATE = Point{
    .pos = .new(-1, -0.75),
    .turns = -0.25,
    .scale = 0.5,
};
const MAIN_INPUT_POS = Point{ .pos = .new(1, 0) };
const MAIN_FNK_POS = Point{ .pos = .new(0, -1.25), .turns = -0.25 };
const DIST_BETWEEN_QUEUED_FNKS = 3.5;
const CABLE_OFFSCREEN_DIST = 15;

const OFFSET_TEMPLATE_PAIR_LEFT: Point = .{ .pos = .new(0.5, -0.5), .scale = 0.5 };
const OFFSET_TEMPLATE_PAIR_RIGHT: Point = .{ .pos = .new(0.5, 0.5), .scale = 0.5 };
const OFFSET_PATTERN_PAIR_LEFT: Point = .{ .pos = .new(-1, -0.5), .scale = 0.5 };
const OFFSET_PATTERN_PAIR_RIGHT: Point = .{ .pos = .new(-1, 0.5), .scale = 0.5 };

const CaseState = struct {
    // TODO: generic tree type to avoid duplication
    pattern: *const Sexpr,
    fnk_name: *const Sexpr,
    template: *const Sexpr,
    next: ?CaseGroup,

    // TODO: separate this?
    incoming_wildcards: []const []const u8,
    outgoing_wildcards: []const []const u8,

    pattern_point_relative_to_parent: Point,

    pub fn updateWildcards(self: *CaseState, gpa: std.mem.Allocator) ![]const []const u8 {
        gpa.free(self.incoming_wildcards);
        gpa.free(self.outgoing_wildcards);

        self.outgoing_wildcards = if (self.next) |*next|
            try next.updateWildcards(gpa)
        else
            &.{};

        var incoming: std.ArrayList([]const u8) = try .initCapacity(gpa, self.outgoing_wildcards.len);
        incoming.appendSliceAssumeCapacity(self.outgoing_wildcards);
        try self.template.getAllVarNames(&incoming);
        self.pattern.removeAllVarNames(&incoming);
        self.incoming_wildcards = try incoming.toOwnedSlice();

        return self.incoming_wildcards;
    }
};
const CaseGroup = struct {
    cases: std.ArrayListUnmanaged(CaseState),
    unfolded: usize,

    pub fn usesWildcardAnywhere(self: CaseGroup, wildcard_name: []const u8) bool {
        for (self.cases.items) |asdf| {
            if (asdf.fnk_name.usesWildcardAnywhere(wildcard_name)) return true;
            if (asdf.pattern.usesWildcardAnywhere(wildcard_name)) return true;
            if (asdf.template.usesWildcardAnywhere(wildcard_name)) return true;
            if (asdf.next) |next| {
                if (next.usesWildcardAnywhere(wildcard_name)) return true;
            }
        } else return false;
    }

    // TODO: revise self.unfolded (make it optional, or ensure its always in bounds)
    pub fn getUnfoldedChild(self: CaseGroup) ?CaseState {
        return safeAt(CaseState, self.cases.items, self.unfolded);
    }

    pub fn updateWildcards(self: *CaseGroup, gpa: std.mem.Allocator) OoM![]const []const u8 {
        var all_required: std.ArrayList([]const u8) = .init(gpa);
        for (self.cases.items) |*case| {
            try all_required.appendSlice(try case.updateWildcards(gpa));
        }
        return all_required.toOwnedSlice();
    }

    pub fn anyWildcardInPlay(self: CaseGroup) bool {
        for (self.cases.items) |asdf| {
            if (!asdf.fnk_name.isFullyResolved()) return true;
            if (!asdf.pattern.isFullyResolved()) return true;
            if (!asdf.template.isFullyResolved()) return true;
            if (asdf.next) |next| {
                if (next.anyWildcardInPlay()) return true;
            }
        }
        return false;
    }

    pub fn caseAt(self: CaseGroup, address: core.CaseAddress) !CaseState {
        return (try caseRefAt(self, address)).*;
    }

    // TODO: could this triplication be removed?
    pub fn caseRefAt(self: CaseGroup, address: core.CaseAddress) !*CaseState {
        if (address.len == 0) {
            return error.BAD_INPUT;
        } else if (address.len == 1) {
            return &self.cases.items[address[0]];
        } else if (self.cases.items[address[0]].next) |next| {
            return next.caseRefAt(address[1..]);
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn insertAt(self: *CaseGroup, mem: *VeryPermamentGameStuff, address: core.CaseAddress, case: CaseState) !void {
        if (address.len == 0) {
            return error.BAD_INPUT;
        } else if (address.len == 1) {
            try self.cases.insert(mem.gpa, address[0], case);
        } else if (self.cases.items[address[0]].next) |*next| {
            try next.insertAt(mem, address[1..], case);
        } else if (address.len == 2 and address[1] == 0) {
            var new_next: CaseGroup = .{
                .unfolded = 0,
                .cases = std.ArrayListUnmanaged(CaseState){},
            };
            try new_next.cases.append(mem.gpa, case);
            self.cases.items[address[0]].next = new_next;
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn removeAt(self: *CaseGroup, address: core.CaseAddress) !CaseState {
        if (address.len == 0) {
            return error.BAD_INPUT;
        } else if (address.len == 1) {
            return self.cases.orderedRemove(address[0]);
        } else if (self.cases.items[address[0]].next) |*next| {
            const result = next.removeAt(address[1..]);
            if (next.cases.items.len == 0) {
                self.cases.items[address[0]].next = null;
            }
            return result;
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn setAllUnfoldedToZero(self: *CaseGroup) void {
        self.unfolded = 0;
        for (self.cases.items) |*case| {
            if (case.next) |*next| {
                next.setAllUnfoldedToZero();
            }
        }
    }

    pub fn setUnfolded(self: *CaseGroup, address: core.CaseAddress) !void {
        if (address.len == 0) {
            return error.BAD_INPUT;
        } else if (address.len == 1) {
            self.unfolded = address[0];
        } else if (self.cases.items[address[0]].next) |*next| {
            try next.setUnfolded(address[1..]);
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn getGlobalPointOf(self: CaseGroup, parent_point: Point, full_address: core.FullAddress) !Point {
        switch (full_address.which) {
            .pattern => return SexprView.sexprPatternChildView(
                try self.getPatternGlobalPoint(parent_point, full_address.case_address),
                full_address.sexpr_address,
            ),
            .template => return SexprView.sexprChildView(
                (try self.getPatternGlobalPoint(
                    parent_point,
                    full_address.case_address,
                )).applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                full_address.sexpr_address,
            ),
            .fnk_name => return SexprView.sexprChildView(
                (try self.getPatternGlobalPoint(
                    parent_point,
                    full_address.case_address,
                )).applyToLocalPoint(FNK_NAME_OFFSET),
                full_address.sexpr_address,
            ),
        }
    }

    pub fn getPatternGlobalPoint(self: CaseGroup, parent_point: Point, address: core.CaseAddress) !Point {
        if (address.len == 0) {
            return parent_point;
        } else if (address.len == 1) {
            return parent_point.applyToLocalPoint(
                self.cases.items[address[0]].pattern_point_relative_to_parent,
            );
        } else if (self.cases.items[address[0]].next) |*next| {
            return next.getPatternGlobalPoint(parent_point.applyToLocalPoint(
                self.cases.items[address[0]].pattern_point_relative_to_parent,
            ), address[1..]);
        } else {
            return error.BAD_INPUT;
        }
    }

    pub fn getSexprAt(self: CaseGroup, full_address: core.FullAddress) !*const core.Sexpr {
        const case = try self.caseAt(full_address.case_address);
        return switch (full_address.which) {
            .pattern => case.pattern.getAt(full_address.sexpr_address) orelse error.BAD_INPUT,
            .template => case.template.getAt(full_address.sexpr_address) orelse error.BAD_INPUT,
            .fnk_name => case.fnk_name.getAt(full_address.sexpr_address) orelse error.BAD_INPUT,
        };
    }

    pub fn setSexprAt(self: CaseGroup, mem: *VeryPermamentGameStuff, full_address: core.FullAddress, value: *const core.Sexpr) !void {
        const case_ref = try self.caseRefAt(full_address.case_address);
        switch (full_address.which) {
            .pattern => case_ref.pattern = try case_ref.pattern.setAt(mem, full_address.sexpr_address, value),
            .template => case_ref.template = try case_ref.template.setAt(mem, full_address.sexpr_address, value),
            .fnk_name => case_ref.fnk_name = try case_ref.fnk_name.setAt(mem, full_address.sexpr_address, value),
        }
    }
};

fn TestingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);
        const Editing = EditingFnk(platform, drawer);

        cur_sample_index: usize,
        state: union(enum) {
            starting: struct {
                t: f32,
            },
            executing: ExecutingFnk(platform, drawer),
        },
        samples: []TestCase,
        fnk_name: *const Sexpr,
        fnk_cases: CaseGroup,
        scoring_run: core.ScoringRun,
        camera: Camera,
        fast: bool,
        tests_reel: Editing.TestsReel,
        ui_state: UI.State,
        which: Which,

        pub const Which = union(enum) { all, only: usize };

        pub fn init(
            camera: Camera,
            which: Which,
            samples: []TestCase,
            fnk_name: *const Sexpr,
            fnk_cases: CaseGroup,
            scoring_run: core.ScoringRun,
            tests_reel: Editing.TestsReel,
        ) !Self {
            var cases = fnk_cases;
            cases.setAllUnfoldedToZero();
            return .{
                .which = which,
                .cur_sample_index = switch (which) {
                    .all => 0,
                    .only => |k| k,
                },
                .state = .{ .starting = .{ .t = 0 } },
                .camera = camera,
                .samples = samples,
                .fnk_name = fnk_name,
                .fnk_cases = cases,
                .scoring_run = scoring_run,
                .fast = false,
                .tests_reel = tests_reel,
                .ui_state = .{ .buttons = try UI.Button.row(platform.gpa, .zero, .one, &.{
                    "⏹",
                    "Reset\nView",
                }) },
            };
        }

        pub fn update(self: *Self, delta_seconds: f32, mem: *VeryPermamentGameStuff) !enum { nothing, back_to_editing } {
            switch (self.state) {
                .starting => |*starting| {
                    if (self.ui_state.update(platform.getMouse(), delta_seconds)) |pressed_button| {
                        switch (pressed_button) {
                            0 => return .back_to_editing,
                            1 => self.camera = DEFAULT_CAM,
                            else => return error.TODO,
                        }
                    }
                    math.towards(&starting.t, 1, (if (self.fast) tof32(4) else tof32(1)) * delta_seconds / 0.75);
                    _ = try Editing.updateCasePositionsAndReturnMouseOverlap(
                        mem,
                        &.{},
                        null,
                        self.fnk_cases,
                        delta_seconds,
                    );
                    const target_scroll: f32 = clamp(tof32(self.cur_sample_index) - 1, 0, self.tests_reel.reel.getMaxScroll(self.samples.len));
                    math.lerp_towards(&self.tests_reel.reel.scroll, target_scroll, 0.1, delta_seconds);

                    if (starting.t >= 1) {
                        self.state = .{ .executing = try .init(
                            .{
                                .value = self.samples[self.cur_sample_index].input,
                                .is_pattern = 0,
                                .pos = MAIN_INPUT_POS,
                            },
                            self.fnk_name,
                            &self.scoring_run,
                            self.camera,
                            self.tests_reel.getUIPoint(self.cur_sample_index, .actual),
                        ) };
                        self.state.executing.fast = self.fast;
                        self.tests_reel.reel.scroll = target_scroll;
                    }
                },
                .executing => |*executing| {
                    switch (try executing.update(delta_seconds)) {
                        .nothing => {},
                        .back_to_editing => return .back_to_editing,
                    }
                    if (executing.isFinished()) {
                        self.samples[self.cur_sample_index].actual = switch (executing.result.?) {
                            else => .undefined,
                            .result => |v| .{ .value = v },
                        };
                        self.fast = executing.fast;
                        if (self.cur_sample_index + 1 < self.samples.len and self.which == .all) {
                            self.cur_sample_index += 1;
                            self.state = .{ .starting = .{ .t = 0 } };
                        } else {
                            return .back_to_editing;
                        }
                    }
                },
            }

            return .nothing;
        }

        pub fn draw(self: Self) !void {
            // TODO: custom tests
            try self.tests_reel.draw(self.samples, &.{});
            switch (self.state) {
                .starting => |starting| {
                    self.ui_state.draw(drawer);
                    try artist.drawSexpr(self.camera, .lerp(
                        self.tests_reel.getWorldPoint(self.camera, self.cur_sample_index, .input),
                        MAIN_INPUT_POS,
                        math.smoothstep(starting.t, 0.15, 1),
                    ), self.samples[self.cur_sample_index].input);
                    artist.drawOffscreenCableTo(self.camera, MAIN_INPUT_POS);
                    try artist.drawHoldedFnk(self.camera, MAIN_FNK_POS, 1, self.fnk_name);
                    try Editing.drawCases(self.camera, true, .{}, self.fnk_cases, null);
                    drawer.drawRect(UI.cam, if (self.cur_sample_index > 0) .lerp(
                        self.tests_reel.rectHighlightingRow(self.samples.len, self.cur_sample_index - 1),
                        self.tests_reel.rectHighlightingRow(self.samples.len, self.cur_sample_index),
                        clamp01(remap(starting.t, 0, 0.2, 0, 1)),
                    ) else self.tests_reel.rectHighlightingRow(self.samples.len, self.cur_sample_index), .white, null);
                },
                .executing => |*executing| {
                    try executing.draw();
                    drawer.drawRect(
                        UI.cam,
                        self.tests_reel.rectHighlightingRow(self.samples.len, self.cur_sample_index),
                        .white,
                        null,
                    );
                },
            }
        }
    };
}

fn LoadingAnEditingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);
        const camera: Camera = DEFAULT_CAM;

        starting_point: Point,
        fnk_name: *const Sexpr,
        t: f32,

        pub fn initFromPoint(fnk_name: *const Sexpr, point: Point) Self {
            return .{
                .fnk_name = fnk_name,
                .starting_point = point,
                .t = 0,
            };
        }

        pub fn initFromLevelSelect(prev: LevelSelect(platform, drawer), level_index: usize) Self {
            return .{
                .fnk_name = builtin_levels[level_index].fnk_name,
                .starting_point = Camera.remap(
                    UI.cam,
                    prev.getLevelButtonPoint(level_index),
                    camera,
                ),
                .t = 0,
            };
        }

        pub fn update(self: *Self, delta_seconds: f32) bool {
            math.towards(&self.t, 1, delta_seconds / 0.5);
            return self.t >= 1;
        }

        pub fn draw(self: Self) !void {
            try artist.drawHoldedFnk(camera, Point.lerp(self.starting_point, MAIN_FNK_POS, self.t), 1, self.fnk_name);
        }
    };
}

pub fn EditingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);

        const CaseAddressWithPoint = struct {
            address: core.CaseAddress,
            pattern_point_relative_to_parent: Point,
        };
        const CasePlace = union(enum) {
            main_fnk: union(enum) {
                existing: core.CaseAddress,
                ghost: CaseAddressWithPoint,

                // pub fn plainAddress(self: @This()) core.CaseAddress {
                //     return switch (self) {
                //         .existing => |x| x,
                //         .ghost => |x| x,
                //     };
                // }
            },
            toolbar_special_case,
            meta_converter,

            pub fn equals(self: @This(), other: @This()) bool {
                if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
                return switch (self) {
                    .main_fnk => |self_case| if (std.meta.activeTag(self_case) != std.meta.activeTag(other.main_fnk))
                        false
                    else switch (self_case) {
                        .existing => |self_existing| std.mem.eql(usize, self_existing, other.main_fnk.existing),
                        .ghost => |self_ghost| std.mem.eql(usize, self_ghost.address, other.main_fnk.ghost.address),
                    },
                    .toolbar_special_case => true,
                    .meta_converter => true,
                };
            }

            pub fn acceptsDrop(place: CasePlace) bool {
                return switch (place) {
                    .main_fnk => true,
                    .toolbar_special_case => false,
                    .meta_converter => true,
                };
            }
        };

        const SexprPlace = union(enum) {
            full_address: core.FullAddress,
            toolbar: usize,
            toolbar_special_var,
            main_input: if (DESIGN.no_current_data) void else core.SexprAddress,
            main_fnk_name: core.SexprAddress,
            sample: TestCase.Address,
            custom_test: TestCase.Address,
            external_fnk: fnks_reel.Address,
            fnk_manager,
            meta_converter: core.SexprAddress,
            list_viewer: ListViewer.Address,
            sexpr_holder: SexprHolder.FullAddress,

            pub fn equals(self: @This(), other: @This()) bool {
                if (std.meta.activeTag(self) != std.meta.activeTag(other)) return false;
                return switch (self) {
                    .full_address => |self_full| self_full.equals(other.full_address),
                    .toolbar => |self_toolbar| self_toolbar == other.toolbar,
                    .main_input => |self_local| if (DESIGN.no_current_data) true else core.equalSexprAddress(self_local, other.main_input),
                    .main_fnk_name => |self_local| core.equalSexprAddress(self_local, other.main_fnk_name),
                    .sample => |self_sample| self_sample.index == other.sample.index and
                        self_sample.which == other.sample.which and
                        core.equalSexprAddress(self_sample.local, other.sample.local),
                    .custom_test => |self_custom_test| self_custom_test.index == other.custom_test.index and
                        self_custom_test.which == other.custom_test.which and
                        core.equalSexprAddress(self_custom_test.local, other.custom_test.local),
                    .external_fnk => |self_fnk| self_fnk.index == other.external_fnk.index and
                        core.equalSexprAddress(self_fnk.local, other.external_fnk.local),
                    .toolbar_special_var => true,
                    .fnk_manager => true,
                    .meta_converter => |self_local| core.equalSexprAddress(self_local, other.meta_converter),
                    .list_viewer => |self_list| core.equalSexprAddress(self_list.local, other.list_viewer.local) and self_list.which.equals(other.list_viewer.which),
                    .sexpr_holder => |self_holder| self_holder.index == other.sexpr_holder.index and core.equalSexprAddress(self_holder.address, other.sexpr_holder.address),
                };
            }

            pub fn getGlobalPoint(address: @This(), self: Self) !Point {
                return switch (address) {
                    .full_address => |full_address| try self.cases.getGlobalPointOf(
                        Point{},
                        full_address,
                    ),
                    .toolbar => |index| if (toolbar.things.len == 0) unreachable else Camera.remap(UI.cam, toolbar.things[index].point, self.camera),
                    .main_input => |local| if (DESIGN.no_current_data) MAIN_INPUT_POS else SexprView.sexprChildView(MAIN_INPUT_POS, local),
                    .main_fnk_name => |local| SexprView.sexprChildView(MAIN_FNK_POS, local),
                    .toolbar_special_var => Camera.remap(UI.cam, toolbar.special_var_point, self.camera),
                    .sample => |sample| SexprView.sexprChildView(self.tests_reel.getWorldPoint(self.camera, sample.index, sample.which), sample.local),
                    .custom_test => |sample| SexprView.sexprChildView(self.tests_reel.getWorldPoint(self.camera, self.samples.len + sample.index, sample.which), sample.local),
                    .external_fnk => |fnk| SexprView.sexprChildView(fnks_reel.getWorldPoint(self.camera, fnk.index), fnk.local),
                    .fnk_manager => Camera.remap(UI.cam, self.fnk_manager.sexpr_point, self.camera),
                    .meta_converter => |local| SexprView.sexprChildView(meta_converter.sexprWorldPoint(self.camera), local),
                    .list_viewer => |list| SexprView.sexprChildView(self.session_persistent.list_viewer.getWorldPoint(self.camera, list.which), list.local),
                    .sexpr_holder => |addr| self.session_persistent.sexpr_holders[addr.index].getWorldPoint(self.camera, addr.address),
                };
            }

            pub fn isAnInvisibleIdentity(address: SexprPlace, self: Self) !bool {
                return switch (address) {
                    else => false,
                    .full_address => |full_address| switch (full_address.which) {
                        else => false,
                        .fnk_name => ((try address.getSexpr(self)).?).equals(Sexpr.builtin.identity),
                    },
                };
            }

            /// null for places that might be empty (like list_viewer.main when not viewing a list)
            pub fn getSexpr(address: @This(), self: Self) !?*const Sexpr {
                return switch (address) {
                    .full_address => |full_address| try self.cases.getSexprAt(full_address),
                    .toolbar => |index| if (toolbar.things.len == 0) unreachable else toolbar.things[index].value,
                    .main_input => |local| if (DESIGN.no_current_data) null else self.main_input.getAt(local).?,
                    .main_fnk_name => |local| self.fnk_name.getAt(local).?,
                    .toolbar_special_var => toolbar.special_var_state.next_value,
                    .sample => |sample| self.samples[sample.index].get(sample.which).?.getAt(sample.local).?,
                    .custom_test => |sample| self.custom_tests.items[sample.index].get(sample.which).?.getAt(sample.local).?,
                    .external_fnk => |fnk| self.favorite_fnks.items[fnk.index].getAt(fnk.local).?,
                    .fnk_manager => return self.fnk_manager.cur,
                    .meta_converter => |local| if (meta_converter.sexpr) |v| v.getAt(local).? else null,
                    .list_viewer => |addr| if (self.session_persistent.list_viewer.getSexpr(addr.which)) |v| v.getAt(addr.local).? else null,
                    .sexpr_holder => |addr| if (self.session_persistent.sexpr_holders[addr.index].value) |v| v.getAt(addr.address).? else null,
                    // examples_reel.getPoint(sample.index, sample.which), sample.local),
                    // .main_input => |local| self.main_input.getAt(local).?,
                };
            }

            pub fn setSexpr(address: @This(), self: *Self, value: *const Sexpr) !void {
                switch (address) {
                    .full_address => |full_address| try self.cases.setSexprAt(self.mem, full_address, value),
                    .main_input => |local_address| {
                        if (DESIGN.no_current_data) unreachable;
                        const value_without_variables = try value.changeAllVariablesToNil(self.mem);
                        self.main_input = try self.main_input.setAt(self.mem, local_address, value_without_variables);
                    },
                    .fnk_manager => {
                        self.fnk_manager.cur = try value.changeAllVariablesToNil(self.mem);
                    },
                    .meta_converter => |local_address| {
                        try meta_converter.setSexpr(self.mem, value, local_address);
                    },
                    .list_viewer => |addr| try self.session_persistent.list_viewer.setSexpr(self.mem, value, addr),
                    .sexpr_holder => |addr| try self.session_persistent.sexpr_holders[addr.index].setSexpr(self.mem, value, addr.address),
                    .custom_test => |sample| try self.custom_tests.items[sample.index].setSexpr(sample.which, sample.local, self.mem, value),
                    .toolbar, .main_fnk_name, .toolbar_special_var, .sample, .external_fnk => unreachable,
                }
            }

            pub fn isPattern(address: @This()) bool {
                return switch (address) {
                    .full_address => |full_address| full_address.which == .pattern,
                    .toolbar_special_var => true,
                    else => false,
                };
            }

            pub fn acceptsDrop(address: @This()) bool {
                return switch (address) {
                    .toolbar => false,
                    .main_fnk_name => false,
                    .toolbar_special_var => false,
                    .sample => false,
                    .custom_test => |sample| sample.which != .actual,
                    .external_fnk => false,
                    .full_address => true,
                    .main_input => true,
                    .fnk_manager => true,
                    .meta_converter => true,
                    .list_viewer => true,
                    .sexpr_holder => true,
                };
            }

            // TODO: some visual feedback
            pub fn acceptsWildcards(address: @This()) bool {
                assert(address.acceptsDrop());
                return switch (address) {
                    .full_address => |x| x.which != .fnk_name,
                    .main_input => false,
                    .fnk_manager => false,
                    .custom_test => false,
                    .meta_converter => true,
                    .sexpr_holder => true,
                    else => unreachable,
                };
            }
        };

        fn freshVarName(self: Self) !*const Sexpr {
            const S = struct {
                var random_instance: std.Random.DefaultPrng = std.Random.DefaultPrng.init(0);
            };

            const new_name = try self.mem.gpa.alloc(u8, 10);
            while (true) {
                Random.init(S.random_instance.random()).alphanumeric_bytes(new_name);

                if (!self.cases.usesWildcardAnywhere(new_name) and
                    !std.mem.eql(u8, new_name, toolbar.special_var_state.next_value.atom_var.value) and
                    !std.mem.eql(u8, new_name, toolbar.special_case_state.next_var.atom_var.value))
                {
                    return try self.mem.storeSexpr(Sexpr.doVar(new_name));
                }
            }
        }

        const UI_State = struct {
            comptime {
                std.debug.assert(DESIGN.no_current_data);
            }

            const Label = union(enum) {
                fixed: FixedLabel,
                temp: TempLabel,

                pub fn equalsFixed(label: ?Label, fixed: FixedLabel) bool {
                    if (label == null) return false;
                    if (std.meta.activeTag(label.?) != .fixed) return false;
                    return label.?.fixed == fixed;
                }

                pub fn equalsTemp(label: ?Label, temp: TempLabel) bool {
                    if (label == null) return false;
                    if (std.meta.activeTag(label.?) != .temp) return false;
                    return std.meta.eql(label.?.temp, temp);
                }
            };

            const FixedLabel = enum {
                back,
                reset_view,
                check_all,
                back_to_menu,

                fnk_manager_load,
                toggle_cur_fnk_is_fav,
            };
            const TempLabel = union(enum) {
                run_test: usize,
                delete_custom_test: usize,
                add_custom_test,
            };

            const ButtonState = struct {
                pos: Rect,
                hot_t: f32 = 0,
                active_t: f32 = 0,
                text: ?[:0]const u8 = null,
                enabled: bool = true,
                visible: bool = true,

                pub fn draw(button: ButtonState) void {
                    if (!button.visible) return;
                    if (!button.enabled) drawer.setTransparency(0.5);
                    defer if (!button.enabled) drawer.setTransparency(1);

                    drawer.drawRect(UI.cam, button.pos.plusMargin(
                        clamp01(button.hot_t - button.active_t) * 0.1,
                    ), .black, .white);
                    if (button.text) |text| {
                        drawer.drawDebugText(UI.cam, .{
                            .pos = button.pos.getCenter(),
                            .scale = button.pos.size.y / (1.5 + tof32(std.mem.count(u8, text, "\n"))),
                        }, text, Color.black);
                    }
                }
            };

            buttons: std.EnumArray(FixedLabel, ButtonState),
            temp_buttons: std.AutoArrayHashMap(TempLabel, ButtonState),

            temp_buttons_not_seen_this_frame: std.AutoArrayHashMap(TempLabel, void),

            pub fn init(tests_reel_rect: Rect, fnk_manager_load: Rect, gpa: std.mem.Allocator) @This() {
                return .{
                    .temp_buttons_not_seen_this_frame = .init(gpa),
                    .temp_buttons = .init(gpa),
                    .buttons = .init(.{
                        .back = .{
                            .pos = .{ .top_left = .zero, .size = .one },
                            .text = "Back",
                        },
                        .reset_view = .{
                            .pos = .{ .top_left = .new(1, 0), .size = .one },
                            .text = "Reset\nView",
                        },
                        .check_all = .{
                            .pos = .from(.{
                                .{ .top_center = tests_reel_rect.get(.bottom_center) },
                                .{ .size = .new(tests_reel_rect.size.x, 0.8) },
                            }),
                            .text = "Check all",
                        },
                        .back_to_menu = .{
                            .pos = .from(.{
                                .{ .top_center = tests_reel_rect.get(.bottom_center) },
                                .{ .size = .new(tests_reel_rect.size.x, 0.8) },
                            }),
                            .text = "Back to menu",
                            .visible = false,
                        },
                        .fnk_manager_load = .{
                            .pos = fnk_manager_load,
                            .text = "Load",
                            .visible = false,
                        },
                        .toggle_cur_fnk_is_fav = .{
                            .pos = undefined,
                            .text = "+-",
                            .visible = true,
                            .enabled = true,
                        },
                    }),
                };
            }

            pub fn draw(self: @This()) void {
                for (self.buttons.values) |button| button.draw();
                var it = self.temp_buttons.iterator();
                while (it.next()) |entry| entry.value_ptr.draw();
            }

            pub fn tempButton(self: *@This(), label: TempLabel, pos: Rect, enabled: bool, text: ?[:0]const u8) !void {
                _ = self.temp_buttons_not_seen_this_frame.swapRemove(label);
                const gop = try self.temp_buttons.getOrPut(label);
                if (gop.found_existing) {
                    gop.value_ptr.pos = pos;
                    gop.value_ptr.enabled = enabled;
                    gop.value_ptr.text = text;
                } else {
                    gop.value_ptr.* = .{
                        .pos = pos,
                        .enabled = enabled,
                        .text = text,
                    };
                }
            }

            pub fn beginFrame(self: *@This()) !void {
                self.temp_buttons_not_seen_this_frame.clearRetainingCapacity();
                var it = self.temp_buttons.iterator();
                while (it.next()) |e| {
                    try self.temp_buttons_not_seen_this_frame.putNoClobber(e.key_ptr.*, {});
                }
            }

            pub fn endFrame(self: *@This()) void {
                var it = self.temp_buttons_not_seen_this_frame.iterator();
                while (it.next()) |l| {
                    assert(self.temp_buttons.swapRemove(l.key_ptr.*));
                }
            }

            pub fn update(self: *@This(), delta_seconds: f32, active: ?Label, hot: ?Label) void {
                {
                    var it = self.buttons.iterator();
                    while (it.next()) |entry| {
                        const button = entry.value;
                        const label = entry.key;
                        math.lerp_towards(
                            &button.hot_t,
                            if (Label.equalsFixed(hot, label)) 1 else 0,
                            0.6,
                            delta_seconds,
                        );
                        math.lerp_towards(
                            &button.active_t,
                            if (Label.equalsFixed(active, label)) 1 else 0,
                            0.6,
                            delta_seconds,
                        );
                    }
                }

                {
                    var it = self.temp_buttons.iterator();
                    while (it.next()) |entry| {
                        const button = entry.value_ptr;
                        const label = entry.key_ptr.*;
                        math.lerp_towards(
                            &button.hot_t,
                            if (Label.equalsTemp(hot, label)) 1 else 0,
                            0.6,
                            delta_seconds,
                        );
                        math.lerp_towards(
                            &button.active_t,
                            if (Label.equalsTemp(active, label)) 1 else 0,
                            0.6,
                            delta_seconds,
                        );
                    }
                }
            }

            pub fn getOverlap(self: *@This(), mouse_ui_pos: Vec2) ?Label {
                {
                    var it = self.buttons.iterator();
                    while (it.next()) |entry| {
                        const button = entry.value;
                        const label = entry.key;
                        if (button.enabled and button.visible and button.pos.contains(mouse_ui_pos)) {
                            return .{ .fixed = label };
                        }
                    }
                }
                {
                    var it = self.temp_buttons.iterator();
                    while (it.next()) |entry| {
                        const button = entry.value_ptr.*;
                        const label = entry.key_ptr.*;
                        if (button.enabled and button.visible and button.pos.contains(mouse_ui_pos)) {
                            return .{ .temp = label };
                        }
                    }
                }
                return null;
            }
        };

        persistence: *PlayerData,
        mem: *VeryPermamentGameStuff,
        camera: Camera = DEFAULT_CAM,
        samples: []TestCase,
        custom_tests: std.ArrayList(TestCase),
        fnk_name: *const Sexpr,
        favorite_fnks: *std.ArrayList(*const Sexpr),
        cases: CaseGroup,
        main_input: if (DESIGN.no_current_data) enum { invalid_field } else *const Sexpr,
        score: ?TestSetScore,

        tutorial_state: TutorialState,

        ui_state: UI_State,

        // TODO, maybe
        // focusV2: struct {
        //     thing: enum { case, sexpr, list_viewer_handle, fnk_manager_handle },
        //     action: enum()
        // },
        focus: union(enum) {
            nothing,
            hovering: union(enum) {
                case: struct {
                    address: CasePlace,
                    hot: f32,
                },
                sexpr: struct {
                    address: SexprPlace,
                    global_point: Point,
                },

                // TODO: hotness
                handle: Handle,

                ui: UI_State.Label,
            },
            grabbing: union(enum) {
                case: struct {
                    case: CaseState,
                    address_if_released: ?CasePlace,
                },
                sexpr: struct {
                    sexpr: *const Sexpr,
                    address_if_released: ?SexprPlace,
                    point: Point,
                    is_pattern: f32,
                    limitation: enum { none, pattern, template },
                },
                handle: Handle,
                ui: UI_State.Label,
            },
            // TODO: remove duplication

            pub fn isAlreadyHovering(self: @This(), hovering: @FieldType(@This(), "hovering")) bool {
                return switch (self) {
                    .nothing, .grabbing => false,
                    .hovering => |cur_hover| switch (hovering) {
                        .case => |case| std.meta.activeTag(cur_hover) == .case and cur_hover.case.address.equals(case.address),
                        .sexpr => |sexpr| std.meta.activeTag(cur_hover) == .sexpr and cur_hover.sexpr.address.equals(sexpr.address),
                        .handle => |handle| std.meta.activeTag(cur_hover) == .handle and std.meta.eql(cur_hover.handle, handle),
                        .ui => |button| std.meta.activeTag(cur_hover) == .ui and std.meta.eql(cur_hover.ui, button),
                    },
                };
            }

            // these two are a bit hacky
            pub fn getActiveUiLabel(self: @This()) ?UI_State.Label {
                return switch (self) {
                    .nothing, .hovering => null,
                    .grabbing => |x| switch (x) {
                        else => null,
                        .ui => |button| button,
                    },
                };
            }
            pub fn getHotUiLabel(self: @This()) ?UI_State.Label {
                return switch (self) {
                    .nothing, .grabbing => null,
                    .hovering => |x| switch (x) {
                        else => null,
                        .ui => |button| button,
                    },
                };
            }
        } = .nothing,

        tests_reel: TestsReel,
        fnk_manager: FnkManager,
        session_persistent: *SessionPersistent(platform, drawer),

        // TODO: abstract
        particles: std.ArrayList(ParticleState),
        particles_cases: std.ArrayList(CaseParticleState),

        const Handle = union(enum) {
            list_viewer,
            fnk_manager,
            sexpr_holder: usize,

            pub fn point(handle: Handle, self: Self) Point {
                return switch (handle) {
                    .list_viewer => self.session_persistent.list_viewer.handlePoint(),
                    .fnk_manager => self.fnk_manager.handlePoint(),
                    .sexpr_holder => |k| self.session_persistent.sexpr_holders[k].handlePoint(),
                };
            }
        };

        const CaseParticleState = struct {
            main: CaseState,
            remaining_lifetime: f32,
            velocity: Point,

            pub fn initFloater(v: CaseState) CaseParticleState {
                return .{
                    .main = v,
                    .remaining_lifetime = 0.5,
                    .velocity = .{ .turns = -0.1, .pos = .new(3, -2), .scale = 0.5 },
                };
            }

            pub fn draw(self: CaseParticleState, camera: Camera) !void {
                drawer.setTransparency(@min(1, self.remaining_lifetime));
                const asdf = try platform.gpa.dupe(CaseState, &.{self.main});
                defer platform.gpa.free(asdf);
                try artist.drawPatternSexpr(camera, self.main.pattern_point_relative_to_parent, self.main.pattern);
                try drawCaseExtra(camera, self.main.pattern_point_relative_to_parent, self.main, null);
                drawer.setTransparency(1);
            }

            pub fn update(self: *CaseParticleState, delta_seconds: f32) !void {
                self.main.pattern_point_relative_to_parent = self.main.pattern_point_relative_to_parent
                    .applyToLocalPoint(.lerp(.{}, self.velocity, delta_seconds));
                self.remaining_lifetime -= delta_seconds * 4;
                self.remaining_lifetime = @max(0, self.remaining_lifetime);
            }
        };

        const ParticleState = struct {
            main: PhysicalSexpr,
            remaining_lifetime: f32,
            velocity: Point,

            pub fn init(v: PhysicalSexpr) ParticleState {
                return .{
                    .main = v,
                    .remaining_lifetime = 1,
                    .velocity = .{ .turns = 0.2, .pos = .new(math.maybeMirror(5.0, v.is_pattern > 0.5), 5), .scale = 0.8 },
                };
            }

            pub fn initFloater(v: PhysicalSexpr) ParticleState {
                return .{
                    .main = v,
                    .remaining_lifetime = 0.5,
                    .velocity = .{ .turns = -0.1, .pos = .new(math.maybeMirror(3.0, v.is_pattern > 0.5), -2), .scale = 0.5 },
                };
            }

            pub fn draw(self: ParticleState, camera: Camera) !void {
                drawer.setTransparency(@min(1, self.remaining_lifetime));
                try artist.drawPhysicalSexpr(camera, self.main);
                drawer.setTransparency(1);
            }

            pub fn update(self: *ParticleState, delta_seconds: f32) !void {
                self.main.updatePattern(null, delta_seconds);
                self.main.pos = self.main.pos.applyToLocalPoint(.lerp(.{}, self.velocity, delta_seconds));
                self.remaining_lifetime -= delta_seconds * 4;
                self.remaining_lifetime = @max(0, self.remaining_lifetime);
            }
        };

        fn overlapsWithTinyCase(mouse_pos: Vec2, case_point: Point) bool {
            const local_point = case_point
                .inverseApplyGetLocalPosition(mouse_pos);

            return local_point.mag() < 2;
        }

        // TODO: cooler, by taking a 'hot' param
        fn drawTinyCase(camera: Camera, case_point: Point, pattern: *const Sexpr, template: *const Sexpr) !void {
            try artist.drawPatternSexpr(camera, case_point
                .applyToLocalPoint(.{ .pos = .new(-1, 0) }), pattern);
            try artist.drawSexpr(camera, case_point
                .applyToLocalPoint(.{ .pos = .new(1, 0) }), template);
            // TODO: artist.drawCableBetween(camera, pattern_pos, template_pos);
            drawer.drawCable(
                camera,
                case_point.applyToLocalPosition(.new(-0.5, 0)),
                case_point.applyToLocalPosition(.new(0.5, 0)),
                case_point.scale,
                0,
            );
        }

        fn drawTinyCaseHolder(case_point: Point, hot: f32) void {
            const camera = UI.cam;
            // TODO: cooler
            drawer.drawCaseHolder(camera, case_point
                .applyToLocalPoint(.{ .pos = .new(-2, 0) })
                .applyToLocalPoint(.{ .scale = hot }));
        }

        const toolbar = struct {
            const atom_values = [_]*const Sexpr{
                // Sexpr.pair_nil_nil,
                // Sexpr.builtin.nil,
                // Sexpr.builtin.true,
                // Sexpr.builtin.false,
            };
            const things = blk: {
                var result: [atom_values.len]struct { value: *const Sexpr, point: Point, index: usize } = undefined;
                for (atom_values, 0..) |atom, k| {
                    result[k] = .{ .value = atom, .point = Camera.remap(DEFAULT_CAM, .{
                        .pos = .new(tof32(k) * 1.6 + 3.5, -2.5),
                        .scale = 0.5,
                    }, UI.cam), .index = k };
                }
                const xx = result;
                break :blk xx;
            };

            const special_var_point = (if (things.len == 0) Camera.remap(DEFAULT_CAM, .{
                .pos = .new(3.5, -2.5),
                .scale = 0.5,
            }, UI.cam) else things[0].point).applyToLocalPoint(.{ .pos = .new(-2, 0) });
            var special_var_state: struct {
                random_instance: std.Random.DefaultPrng = std.Random.DefaultPrng.init(0),
                next_value: *const Sexpr = &Sexpr.doVar("first_var"),

                pub fn next(self: *@This(), mem: *VeryPermamentGameStuff, cases: CaseGroup) !void {
                    const new_name = try mem.gpa.alloc(u8, 10);
                    while (true) {
                        Random.init(self.random_instance.random()).alphanumeric_bytes(new_name);

                        if (!cases.usesWildcardAnywhere(new_name)) {
                            self.next_value = try mem.storeSexpr(Sexpr.doVar(new_name));
                            break;
                        }
                    }
                }
            } = .{};

            const special_case_point = if (things.len == 0)
                special_var_point.applyToLocalPoint(.{ .pos = .new(4, 0) })
            else
                things[things.len - 1].point.applyToLocalPoint(.{ .pos = .new(5.4, 0) });
            var special_case_state: struct {
                random_instance: std.Random.DefaultPrng = std.Random.DefaultPrng.init(1),
                next_var: *const Sexpr = Sexpr.builtin.vars.v1,

                pub fn next(self: *@This(), mem: *VeryPermamentGameStuff, cases: CaseGroup) !void {
                    const new_name = try mem.gpa.alloc(u8, 10);
                    while (true) {
                        Random.init(self.random_instance.random()).alphanumeric_bytes(new_name);
                        if (!cases.usesWildcardAnywhere(new_name)) {
                            self.next_var = try mem.storeSexpr(Sexpr.doVar(new_name));
                            break;
                        }
                    }
                }

                pub fn value(self: @This(), camera: Camera) CaseState {
                    return .{
                        .fnk_name = Sexpr.builtin.identity,
                        .pattern = self.next_var,
                        .template = self.next_var,
                        .next = null,
                        .pattern_point_relative_to_parent = Camera.remap(UI.cam, special_case_point, camera),
                        .incoming_wildcards = &.{},
                        .outgoing_wildcards = &.{},
                    };
                }
            } = .{};

            pub const Modifier = enum {
                normal,
                hidden,
                only_special_var,
                all_except_case,

                pub fn from(tutorial_state: TutorialState) Modifier {
                    return switch (tutorial_state) {
                        .first_level => .hidden,
                        .second_level => .only_special_var,
                        .third_level => .all_except_case,
                        else => .normal,
                    };
                }

                pub fn specialVarEnabled(modifier: Modifier, wildcard_in_play: bool) bool {
                    _ = wildcard_in_play;
                    return switch (modifier) {
                        .hidden => false,
                        .normal => true,
                        // else => !wildcard_in_play,
                        else => true,
                    };
                }

                pub fn specialCaseEnabled(modifier: Modifier) bool {
                    return modifier == .normal;
                }

                pub fn thingsEnabled(modifier: Modifier) bool {
                    return switch (modifier) {
                        .normal, .all_except_case => true,
                        .hidden, .only_special_var => false,
                    };
                }
            };

            pub fn overlapsWithSpecialVar(mouse_ui_pos: Vec2, modifier: Modifier, wildcard_in_play: bool) bool {
                return modifier.specialVarEnabled(wildcard_in_play) and SexprView.overlapsPatternAtom(special_var_point, mouse_ui_pos, .atom);
            }

            pub fn overlapsWithSpecialCase(mouse_ui_pos: Vec2, modifier: Modifier) bool {
                return modifier.specialCaseEnabled() and overlapsWithTinyCase(mouse_ui_pos, special_case_point);
            }

            pub fn findOverlap(mouse_ui_pos: Vec2, modifier: Modifier) ?std.meta.Elem(@TypeOf(things)) {
                if (!modifier.thingsEnabled()) return null;
                for (things) |thing| {
                    if (SexprView.overlapsAtom(thing.point, mouse_ui_pos, .atom)) {
                        return thing;
                    }
                }
                return null;
            }

            pub fn draw(modifier: Modifier, wildcard_in_play: bool) !void {
                if (modifier == .hidden) return;

                const camera = UI.cam;

                if (modifier.specialVarEnabled(wildcard_in_play)) {
                    try artist.drawPatternSexpr(camera, special_var_point, special_var_state.next_value);
                }
                if (modifier.thingsEnabled()) {
                    for (things) |thing| {
                        try artist.drawSexpr(camera, thing.point, thing.value);
                    }
                }

                if (modifier.specialCaseEnabled()) {
                    try drawTinyCase(UI.cam, special_case_point, special_case_state.value(undefined).pattern, special_case_state.value(undefined).template);
                }
            }
        };

        const Reel = struct {
            scroll: f32 = 0,
            /// in UI coords
            top_left: Point,
            /// in UI coords
            rect: Rect,
            scroll_bar_active: bool = false,
            n_visible_rows: usize,

            // TODO: don't take a pointer to Mouse
            pub fn update(self: *Reel, n_rows: usize, mouse: *Mouse, delta_seconds: f32) void {
                if (self.scroll_bar_active) {
                    if (!mouse.cur.isDown(.left)) {
                        self.scroll_bar_active = false;
                    } else {
                        self.scroll += (mouse.cur.pos(UI.cam).y - mouse.prev.pos(UI.cam).y) * self.top_left.scale;
                    }
                } else if (self.scrollBarRect(n_rows).contains(mouse.cur.pos(UI.cam)) and mouse.wasPressed(.left)) {
                    self.scroll_bar_active = true;
                }
                if (self.rect.contains(mouse.cur.pos(UI.cam))) {
                    self.scroll -= delta_seconds * 10 * mouse.cur.scrolled.toNumber();
                    mouse.cur.scrolled = .none;
                }
                self.updateScroll(n_rows, delta_seconds);
            }

            pub fn getMaxScroll(self: Reel, n_rows: usize) f32 {
                return @max(0, tof32(n_rows -| self.n_visible_rows));
            }

            pub fn updateScroll(self: *Reel, n_rows: usize, delta_seconds: f32) void {
                math.lerp_towards_range(&self.scroll, 0, self.getMaxScroll(n_rows), 0.1, delta_seconds);
            }

            pub fn getRowUIPos(self: Reel, k: usize, x: f32) Point {
                const index: f32 = tof32(k) - self.scroll;
                const y = 1.25 + index * 2.5;
                const scale = @min(
                    math.smoothstep(index, -0.5, 0),
                    math.smoothstep(index, tof32(self.n_visible_rows) - 0.5, tof32(self.n_visible_rows) - 1),
                );
                return self.top_left
                    .applyToLocalPoint(.{ .pos = .new(x, y) })
                    .applyToLocalPoint(.{ .scale = scale, .pos = .lerp(.new(0.5, math.maybeMirror(0.5, y > self.rect.size.y / 2)), .zero, scale) });
            }

            pub fn getRawPos(self: Reel, k: f32, x: f32) Point {
                const index: f32 = k - self.scroll;
                const y = 1.25 + index * 2.5;
                return self.top_left
                    .applyToLocalPoint(.{ .pos = .new(x, y) });
            }

            fn scrollBarRect(self: Reel, n_rows: usize) Rect {
                const scroll_perc = self.scroll / self.getMaxScroll(n_rows);
                const bar_height = tof32(self.n_visible_rows) / tof32(self.getMaxScroll(n_rows));
                return .{
                    .top_left = self.rect.get(.top_right).addY((self.rect.size.y - bar_height) * scroll_perc),
                    .size = .new(0.2, bar_height),
                };
            }

            fn drawScrollBar(self: Reel, n_rows: usize) void {
                drawer.drawRect(UI.cam, self.scrollBarRect(n_rows), null, .black);
            }
        };

        const TestsReel = struct {
            reel: Reel,
            allow_custom_tests: bool,

            pub fn init(top_left: Point, allow_custom_tests: bool) TestsReel {
                return .{ .reel = .{
                    .top_left = top_left,
                    .rect = .{
                        .top_left = top_left.pos,
                        .size = Vec2.new(9.25, 10).scale(top_left.scale),
                    },
                    .n_visible_rows = 4,
                }, .allow_custom_tests = allow_custom_tests };
            }

            pub fn update(self: *TestsReel, samples_len: usize, custom_tests_len: usize, mouse: *Mouse, delta_seconds: f32, ui: *UI_State) !void {
                self.reel.update(samples_len + custom_tests_len + if (self.allow_custom_tests) 1 else @as(usize, 0), mouse, delta_seconds);
                for (0..samples_len + custom_tests_len) |k| {
                    const point = self.getUIPoint(k, .input).applyToLocalPoint(.{ .pos = .new(-1, 0) });
                    try ui.tempButton(
                        .{ .run_test = k },
                        .fromPoint(point, .center, .one),
                        true,
                        ">",
                    );
                }
                for (0..custom_tests_len) |k| {
                    const point = self.getUIPoint(samples_len + k, .input).applyToLocalPoint(.{ .pos = .new(-1, 1) });
                    try ui.tempButton(
                        .{ .delete_custom_test = k },
                        .fromPoint(point, .center, .one),
                        true,
                        "x",
                    );
                }
                if (self.allow_custom_tests) {
                    const point = self.getUIPoint(samples_len + custom_tests_len, .expected).applyToLocalPoint(.{ .pos = .new(1, 0) });
                    try ui.tempButton(
                        .add_custom_test,
                        .fromPoint(point, .center, .both(1.5)),
                        true,
                        "+",
                    );
                }
            }

            /// in UI coords
            pub fn getUIPoint(self: TestsReel, k: usize, which: TestCase.Part) Point {
                return self.reel.getRowUIPos(k, switch (which) {
                    .input => 0.75,
                    .expected => 3.75,
                    .actual => 6.75,
                });
            }

            /// in world coords
            pub fn getWorldPoint(self: TestsReel, camera: Camera, k: usize, which: TestCase.Part) Point {
                return Camera.remap(UI.cam, self.getUIPoint(k, which), camera);
            }

            pub fn findOverlap(self: TestsReel, mouse_ui_pos: Vec2, samples: []const TestCase, custom_tests: []const TestCase) !?struct {
                address: TestCase.Address,
                is_custom: bool,
            } {
                inline for (&.{ samples, custom_tests }, &.{ 0, samples.len }, &.{ false, true }) |asdf, offset, is_custom| {
                    for (asdf, 0..) |sample, k| {
                        if (try SexprView.overlapsSexpr(
                            platform.gpa,
                            sample.input,
                            self.getUIPoint(k + offset, .input),
                            mouse_ui_pos,
                        )) |local| {
                            return .{
                                .address = TestCase.Address{ .index = k, .local = local, .which = .input },
                                .is_custom = is_custom,
                            };
                        }
                        if (sample.expected) |expected| {
                            if (try SexprView.overlapsSexpr(
                                platform.gpa,
                                expected,
                                self.getUIPoint(k + offset, .expected),
                                mouse_ui_pos,
                            )) |local| {
                                return .{
                                    .address = TestCase.Address{ .index = k, .local = local, .which = .expected },
                                    .is_custom = is_custom,
                                };
                            }
                        }
                        if (sample.get(.actual)) |actual| {
                            if (try SexprView.overlapsSexpr(
                                platform.gpa,
                                actual,
                                self.getUIPoint(k + offset, .actual),
                                mouse_ui_pos,
                            )) |local| {
                                return .{
                                    .address = TestCase.Address{ .index = k, .local = local, .which = .actual },
                                    .is_custom = is_custom,
                                };
                            }
                        }
                    }
                }
                return null;
            }

            pub fn draw(self: TestsReel, samples: []const TestCase, custom_tests: []const TestCase) !void {
                const camera = UI.cam;
                drawer.drawRect(camera, self.reel.rect, .black, null);
                const p = self.reel.rect.get(.top_center).addY(-0.3);
                drawer.drawDebugText(UI.cam, .{
                    .pos = p.addX(-2.25),
                    .scale = 0.75,
                }, "Input", .black);
                drawer.drawDebugText(UI.cam, .{
                    .pos = p.addX(0),
                    .scale = 0.75,
                }, "Target", .black);
                drawer.drawDebugText(UI.cam, .{
                    .pos = p.addX(2.25),
                    .scale = 0.75,
                }, "Actual", .black);
                inline for (&.{ samples, custom_tests }, &.{ 0, samples.len }) |asdf, offset| {
                    for (asdf, 0..) |sample, k| {
                        try artist.drawSexpr(
                            camera,
                            self.getUIPoint(k + offset, .input),
                            sample.input,
                        );
                        if (sample.expected) |output| {
                            try artist.drawSexpr(
                                camera,
                                self.getUIPoint(k + offset, .expected),
                                output,
                            );
                        } else {
                            return error.TODO;
                        }
                        switch (sample.actual) {
                            .undefined => {
                                drawer.drawDebugText(camera, self.getUIPoint(k + offset, .actual).applyToLocalPoint(.{ .scale = 2, .pos = .new(0.8, 0) }), "❌", .black);
                            },
                            .unknown => {
                                drawer.drawDebugText(camera, self.getUIPoint(k + offset, .actual).applyToLocalPoint(.{ .scale = 2, .pos = .new(0.8, 0) }), "?", .black);
                            },
                            .value => |v| {
                                try artist.drawSexpr(
                                    camera,
                                    self.getUIPoint(k + offset, .actual),
                                    v,
                                );
                            },
                        }

                        if (std.meta.activeTag(sample.actual) != .unknown) {
                            drawer.drawDebugText(
                                camera,
                                self.getUIPoint(k + offset, .actual).applyToLocalPoint(.{ .scale = 1.5, .pos = .new(-0.5, 0) }),
                                if (sample.isSolved()) "✔️" else "❌",
                                .black,
                            );
                        }
                    }
                }
                self.reel.drawScrollBar(samples.len + custom_tests.len + if (self.allow_custom_tests) 1 else @as(usize, 0));
                // drawer.drawDebugText(camera, .{ .pos = rect.get(.top_center).addY(-0.35) }, "tests", .black);
            }

            pub fn rectHighlightingRow(self: TestsReel, samples_len: usize, row: usize) Rect {
                std.debug.assert(row < samples_len);
                return .fromCenterAndSize(
                    self.getUIPoint(row, .expected).applyToLocalPosition(.new(0.9, 0)),
                    .new(self.reel.rect.size.x - 0.05, 1.75),
                );
            }
        };

        const SexprHolder = struct {
            value: ?*const Sexpr = null,
            point: Point,

            pub fn sexprPoint(self: SexprHolder) Point {
                return self.point;
            }

            pub fn handlePoint(self: SexprHolder) Point {
                return self.point.applyToLocalPoint(.{ .pos = .new(-0.5, -1), .scale = 0.4 });
            }

            pub fn move(self: *SexprHolder, mouse: Mouse) void {
                self.point.pos.addInPlace(mouse.cur.pos(UI.cam)
                    .sub(mouse.prev.pos(UI.cam)));
            }

            pub fn getWorldPoint(self: SexprHolder, camera: Camera, address: core.SexprAddress) Point {
                return Camera.remap(UI.cam, self.getUIPoint(address), camera);
            }

            pub fn getUIPoint(self: SexprHolder, address: core.SexprAddress) Point {
                return SexprView.sexprChildView(self.sexprPoint(), address);
            }

            pub fn setSexpr(self: *SexprHolder, mem: *VeryPermamentGameStuff, new_sexpr: *const Sexpr, address: core.SexprAddress) !void {
                if (self.value) |existing| {
                    self.value = try existing.setAt(mem, address, new_sexpr);
                } else {
                    assert(address.len == 0);
                    self.value = new_sexpr;
                }
            }

            pub fn findOverlap(self: SexprHolder, mouse_ui_pos: Vec2) !?core.SexprAddress {
                if (self.value) |v| {
                    return try SexprView.overlapsSexpr(platform.gpa, v, self.sexprPoint(), mouse_ui_pos);
                } else if (SexprView.overlapsAtom(self.sexprPoint(), mouse_ui_pos, .atom)) {
                    return &.{};
                } else return null;
            }

            pub fn draw(self: SexprHolder) !void {
                const camera = UI.cam;
                drawer.drawLineV2(camera, self.point, &([1]Vec2{.new(2, -1.1)} ++
                    funk.fromCount(32, struct {
                        pub fn anon(k: usize) Vec2 {
                            // TODO: center the circle actually on the sexprPoint
                            return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).scale(1.1).addX(0.5);
                        }
                    }.anon) ++
                    [1]Vec2{.new(2, 1.1)}), .black);
                drawer.drawCircle(
                    camera,
                    self.handlePoint(),
                    .black,
                    null,
                );
                if (self.value) |v| {
                    try artist.drawSexpr(camera, self.sexprPoint(), v);
                }
            }

            pub const FullAddress = struct { index: usize, address: core.SexprAddress };
        };

        // TODO: user can add/remove elements from list
        // TODO: display element index
        const ListViewer = struct {
            reel: Reel,
            value: ?*const Sexpr,

            const Address = struct {
                const Which = union(enum) {
                    main,
                    element: usize,
                    /// the index if placed
                    between: usize,

                    pub fn equals(a: Which, b: Which) bool {
                        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
                        return switch (a) {
                            .main => true,
                            .element => |i| i == b.element,
                            .between => |i| i == b.between,
                        };
                    }
                };
                which: Which,
                local: core.SexprAddress,
            };

            pub fn move(self: *ListViewer, mouse: Mouse) void {
                self.reel.top_left.pos.addInPlace(mouse.cur.pos(UI.cam)
                    .sub(mouse.prev.pos(UI.cam)));
                self.reel.rect.top_left = self.reel.top_left.pos;
            }

            pub fn getWorldPoint(self: ListViewer, camera: Camera, which: Address.Which) Point {
                return Camera.remap(UI.cam, self.getUIPoint(which), camera);
            }

            pub fn getUIPoint(self: ListViewer, which: Address.Which) Point {
                // TODO: improve .between
                return switch (which) {
                    .main => self.mainPoint(),
                    .element => |k| self.reel.getRowUIPos(k, 0.8),
                    .between => |k| self.reel.getRawPos(tof32(k) - 0.5, 1).applyToLocalPoint(.{ .scale = 0.5 }),
                };
            }

            pub fn getSexpr(self: ListViewer, which: Address.Which) ?*const Sexpr {
                return switch (which) {
                    .main => self.value,
                    .element => |k| blk: {
                        var list = (self.curList() catch @panic("OoM")).?;
                        defer list.deinit();
                        break :blk list.items[k];
                    },
                    .between => null,
                };
            }

            pub fn setSexpr(self: *ListViewer, mem: *VeryPermamentGameStuff, new_sexpr: *const Sexpr, address: Address) !void {
                switch (address.which) {
                    .main => if (self.value) |existing| {
                        self.value = try existing.setAt(mem, address.local, new_sexpr);
                    } else {
                        std.debug.assert(address.local.len == 0);
                        self.value = new_sexpr;
                    },
                    .element => |k| {
                        var list = (self.curList() catch @panic("OoM")).?;
                        defer list.deinit();
                        list.items[k] = try list.items[k].setAt(mem, address.local, new_sexpr);
                        self.value = try core.toListPlusSentinel(list.items, Sexpr.builtin.nil, &mem.pool_for_sexprs);
                    },
                    .between => |k| {
                        var list = (self.curList() catch @panic("OoM")).?;
                        defer list.deinit();
                        std.debug.assert(address.local.len == 0);
                        try list.insert(k, new_sexpr);
                        self.value = try core.toListPlusSentinel(list.items, Sexpr.builtin.nil, &mem.pool_for_sexprs);
                    },
                }
            }

            pub fn removeElement(self: *ListViewer, k: usize, mem: *VeryPermamentGameStuff) !void {
                var list = (try self.curList()).?;
                defer list.deinit();
                _ = list.orderedRemove(k);
                self.value = try core.toListPlusSentinel(list.items, Sexpr.builtin.nil, &mem.pool_for_sexprs);
            }

            pub fn findOverlap(self: ListViewer, mouse_ui_pos: Vec2, holding_some_sexpr: bool) !?Address {
                if (self.value) |v| {
                    if (try SexprView.overlapsSexpr(platform.gpa, v, self.mainPoint(), mouse_ui_pos)) |local| {
                        return .{ .which = .main, .local = local };
                    } else if (try self.curList()) |list| {
                        defer list.deinit();
                        for (list.items, 0..) |w, k| {
                            if (try SexprView.overlapsSexpr(platform.gpa, w, self.getUIPoint(.{ .element = k }), mouse_ui_pos)) |local| {
                                return .{ .which = .{ .element = k }, .local = local };
                            }
                        }
                        if (holding_some_sexpr and self.reel.rect.contains(mouse_ui_pos)) {
                            const y = self.reel.top_left.inverseApplyGetLocalPosition(mouse_ui_pos).y;
                            const index: usize = @intFromFloat(math.clamp(@ceil((y - 1.25) / 2.5 + self.reel.scroll), 0, tof32(list.items.len)));
                            return .{ .which = .{ .between = index }, .local = &.{} };
                        }
                    }
                } else if (SexprView.overlapsAtom(self.mainPoint(), mouse_ui_pos, .atom)) {
                    return .{ .which = .main, .local = &.{} };
                }
                return null;
            }

            pub fn onEnterLevel(self: *ListViewer, is_intro: bool) void {
                const top_left: Vec2 = if (is_intro) .new(24.5, 9.75) else .new(30, 13);
                self.reel.rect.top_left = top_left;
                self.reel.top_left.pos = top_left;
            }

            pub fn init() ListViewer {
                const rect_size: Vec2 = .new(1.7, 5);
                return .{
                    .value = null,
                    .reel = .{
                        .top_left = .{
                            .pos = undefined,
                            .scale = 0.5,
                        },
                        .n_visible_rows = 4,
                        .rect = .{
                            .top_left = undefined,
                            .size = rect_size,
                        },
                    },
                };
            }

            fn mainPoint(self: ListViewer) Point {
                return self.reel.top_left.applyToLocalPoint(.{ .pos = .new(-7.5, 5), .scale = 4 });
            }

            fn handlePoint(self: ListViewer) Point {
                return self.reel.top_left.applyToLocalPoint(.{ .pos = .new(-10, 1), .scale = 1 });
            }

            fn curList(self: ListViewer) !?std.ArrayList(*const Sexpr) {
                if (self.value) |v| {
                    var buffer: std.ArrayList(*const Sexpr) = .init(platform.gpa);
                    errdefer buffer.deinit();
                    const sentinel = try core.asListPlusSentinel(v, &buffer);
                    if (sentinel.equals(Sexpr.builtin.nil)) {
                        return buffer;
                    } else {
                        buffer.deinit();
                        return null;
                    }
                } else return null;
            }

            pub fn update(self: *ListViewer, mouse: *Mouse, delta_seconds: f32) !void {
                if (try self.curList()) |buffer| {
                    defer buffer.deinit();
                    self.reel.update(buffer.items.len, mouse, delta_seconds);
                }
            }

            pub fn draw(self: ListViewer) !void {
                const camera = UI.cam;
                drawer.drawRect(camera, self.reel.rect, .black, null);
                drawer.drawLineV2(camera, .{ .pos = self.reel.top_left.pos, .scale = self.reel.rect.size.y }, &([1]Vec2{.zero} ++
                    funk.fromCount(32, struct {
                        pub fn anon(k: usize) Vec2 {
                            // TODO: center the circle actually on the mainPoint
                            return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).scale(0.5).addY(0.5).addX(-0.55);
                        }
                    }.anon) ++
                    [1]Vec2{.new(0, 1)}), .black);
                drawer.drawCircle(
                    camera,
                    self.handlePoint(),
                    .black,
                    null,
                );
                if (self.value) |v| {
                    try artist.drawSexpr(camera, self.mainPoint(), v);

                    if (try self.curList()) |buffer| {
                        defer buffer.deinit();
                        for (buffer.items, 0..) |item, k| {
                            try artist.drawSexpr(
                                camera,
                                self.getUIPoint(.{ .element = k }),
                                item,
                            );
                        }
                        self.reel.drawScrollBar(buffer.items.len);
                    }
                }
            }
        };

        const SamplesReel = struct {
            reel: Reel,

            pub fn init() SamplesReel {
                const rect_size: Vec2 = .new(fnks_reel.reel.rect.size.x, 7.5 * 0.75);
                const top_left_pos: Vec2 = fnks_reel.reel.rect.top_left.addY(-rect_size.y).addY(-0.25);
                return .{
                    .reel = .{
                        .top_left = .{
                            .pos = top_left_pos,
                            .scale = 0.75,
                        },
                        .n_visible_rows = 3,
                        .rect = .{
                            .top_left = top_left_pos,
                            .size = rect_size,
                        },
                    },
                };
            }

            // TODO: don't take a pointer to Mouse
            pub fn update(self: *SamplesReel, samples_len: usize, mouse: *Mouse, delta_seconds: f32) void {
                self.reel.update(samples_len, mouse, delta_seconds);
            }

            /// in UI coords
            pub fn getUIPoint(self: SamplesReel, k: usize, which: Sample.Part) Point {
                return self.reel.getRowUIPos(k, switch (which) {
                    .input => 0.65,
                    .output => 3.95,
                });
            }

            /// in world coords
            pub fn getWorldPoint(self: SamplesReel, camera: Camera, k: usize, which: Sample.Part) Point {
                return Camera.remap(UI.cam, self.getUIPoint(k, which), camera);
            }

            pub fn findOverlap(self: SamplesReel, mouse_ui_pos: Vec2, samples: []const Sample) !?Sample.Address {
                for (samples, 0..) |sample, k| {
                    if (try SexprView.overlapsSexpr(
                        platform.gpa,
                        sample.input,
                        self.getUIPoint(k, .input),
                        mouse_ui_pos,
                    )) |local| {
                        return Sample.Address{ .index = k, .local = local, .which = .input };
                    }
                    if (sample.output) |output| {
                        if (try SexprView.overlapsSexpr(
                            platform.gpa,
                            output,
                            self.getUIPoint(k, .output),
                            mouse_ui_pos,
                        )) |local| {
                            return Sample.Address{ .index = k, .local = local, .which = .output };
                        }
                    }
                }
                return null;
            }

            pub fn draw(self: SamplesReel, samples: []const Sample, solved_status: []const bool, n_revealed_cases: usize) !void {
                const camera = UI.cam;
                std.debug.assert(samples.len == solved_status.len);
                drawer.drawRect(camera, self.reel.rect, .black, null);
                for (samples, solved_status, 0..) |sample, solved, k| {
                    drawer.drawArrowForSample(camera, self.getUIPoint(k, .output).applyToLocalPoint(.{
                        .pos = .new(-1.25, 0),
                        .scale = 0.25,
                    }), if (k < n_revealed_cases) solved else null);
                    try artist.drawSexpr(
                        camera,
                        self.getUIPoint(k, .input),
                        sample.input,
                    );
                    if (sample.output) |output| {
                        try artist.drawSexpr(
                            camera,
                            self.getUIPoint(k, .output),
                            output,
                        );
                    } else {
                        return error.TODO;
                    }
                }
                self.reel.drawScrollBar(samples.len);
                // drawer.drawDebugText(camera, .{ .pos = rect.get(.top_center).addY(-0.35) }, "tests", .black);
            }

            pub fn sampleCenter(self: SamplesReel, index: usize) Vec2 {
                return Point.lerp(
                    self.getUIPoint(index, .input),
                    self.getUIPoint(index, .output),
                    0.75,
                ).applyToLocalPosition(.zero);
            }
        };

        // TODO: would be nice to classify fnks by name
        const fnks_reel = struct {
            var reel: Reel = blk: {
                const top_left: Point = Camera.remap(
                    .{ .center = .new(7, 3), .height = 15.0 },
                    .{ .pos = .new(-6.1, 0.75 + 7.5 * 0.75), .scale = 0.75 },
                    UI.cam,
                );
                break :blk .{
                    .top_left = top_left,
                    .rect = .{
                        .top_left = top_left.pos,
                        .size = Vec2.new(6.25, 5.25).scale(top_left.scale),
                    },
                    .n_visible_rows = 2,
                };
            };

            const N_FNKS_PER_ROW = 3;

            pub const Address = struct {
                index: usize,
                local: core.SexprAddress,
            };

            /// in UI coords
            fn getUIPoint(k: usize) Point {
                const v_index: f32 = tof32(k / N_FNKS_PER_ROW) - reel.scroll;
                const y = 2 + v_index * 2.5;
                const x = 1.1 + tof32(k % N_FNKS_PER_ROW) * 2;
                const scale = @min(
                    math.smoothstep(v_index, -0.5, 0),
                    math.smoothstep(v_index, tof32(reel.n_visible_rows) - 0.5, 1),
                );
                return reel.top_left.applyToLocalPoint(.{
                    .pos = .new(x, y - if (y < reel.rect.size.y / 2.0) 0 else lerp(1, 0, scale)),
                    .scale = scale * 0.75,
                    .turns = -0.25,
                });
            }

            pub fn getWorldPoint(camera: Camera, k: usize) Point {
                return Camera.remap(UI.cam, getUIPoint(k), camera);
            }

            pub const Modifier = enum {
                normal,
                only_first,

                pub fn from(tutorial_state: TutorialState) Modifier {
                    _ = tutorial_state;
                    return .normal;
                    // return switch (self) {
                    //     .none => .normal,
                    //     else => .only_first,
                    // };
                }
            };

            pub fn findOverlap(mouse_ui_pos: Vec2, available_fnks: []const *const Sexpr, modifier: Modifier) !?Address {
                for (available_fnks, 0..) |fnk_name, k| {
                    if (try SexprView.overlapsSexpr(
                        platform.gpa,
                        fnk_name,
                        getUIPoint(k),
                        mouse_ui_pos,
                    )) |local| {
                        return .{ .index = k, .local = local };
                    }
                    if (modifier == .only_first) break;
                }
                return null;
            }

            pub fn draw(available_fnks: []const *const Sexpr, modifier: Modifier) !void {
                const camera = UI.cam;
                drawer.drawRect(camera, reel.rect, .black, null);
                for (available_fnks, 0..) |fnk_name, k| {
                    try artist.drawSexpr(camera, getUIPoint(k), fnk_name);
                    if (modifier == .only_first and k == 0) drawer.setTransparency(0.5);
                }
                if (modifier == .only_first) drawer.setTransparency(1);
                reel.drawScrollBar(nRows(available_fnks.len));
                // drawer.drawDebugText(camera, .{ .pos = reel.rect.get(.top_center).addY(0.2) }, "vaus", .black);
            }

            fn nRows(fnks_len: usize) usize {
                return std.math.divCeil(usize, fnks_len, N_FNKS_PER_ROW) catch unreachable;
            }

            pub fn update(ui: *UI_State, enabled: bool) void {
                ui.buttons.getPtr(.toggle_cur_fnk_is_fav).pos = .from(.{ .{ .bottom_left = reel.top_left.pos }, .{ .size = .one } });
                ui.buttons.getPtr(.toggle_cur_fnk_is_fav).enabled = enabled;
                ui.buttons.getPtr(.toggle_cur_fnk_is_fav).visible = enabled;
            }
        };

        /// create/edit/delete fnks
        const FnkManager = struct {
            sexpr_point: Point = .{ .pos = .new(2, 3), .scale = 0.5, .turns = -0.25 },

            cur: ?*const Sexpr,

            pub fn getButtonRect(self: FnkManager, which: enum { load }) Rect {
                std.debug.assert(which == .load);
                return .{
                    .top_left = self.sexpr_point.applyToLocalPosition(.new(2, 1.5)),
                    .size = .new(1.25, 0.75),
                };
            }

            fn generalRect(self: FnkManager) Rect {
                return .{
                    .top_left = self.sexpr_point.applyToLocalPosition(.new(2.5, -2)),
                    .size = .new(3.1, 2.2),
                };
            }

            pub fn init() FnkManager {
                return .{ .cur = null };
            }

            fn handlePoint(self: FnkManager) Point {
                return self.sexpr_point.applyToLocalPoint(.{ .pos = .new(2.1, -2), .scale = 0.5 });
            }

            pub fn move(self: *FnkManager, mouse: Mouse, ui: *UI_State) void {
                self.sexpr_point.pos.addInPlace(mouse.cur.pos(UI.cam)
                    .sub(mouse.prev.pos(UI.cam)));
                ui.buttons.getPtr(.fnk_manager_load).pos = self.getButtonRect(.load);
            }

            pub fn findOverlap(self: FnkManager, mouse_ui_pos: Vec2) bool {
                return SexprView.overlapsAtom(self.sexpr_point, mouse_ui_pos, .atom);
            }

            pub fn update(self: FnkManager, ui: *UI_State, enabled: bool) void {
                ui.buttons.getPtr(.fnk_manager_load).enabled = self.cur != null;
                ui.buttons.getPtr(.fnk_manager_load).visible = enabled;
            }

            pub fn draw(self: FnkManager) !void {
                const camera = UI.cam;
                drawer.drawRect(camera, self.generalRect(), .black, null);
                drawer.drawCircle(
                    camera,
                    self.handlePoint(),
                    .black,
                    .gray(128),
                );
                artist.drawFnkHolderForFnkAt(camera, self.sexpr_point, 1);
                if (self.cur) |v| {
                    try artist.drawSexpr(camera, self.sexpr_point, v);
                } else {
                    drawer.drawAtomHint(camera, self.sexpr_point);
                }
                // drawer.drawDebugText(camera, sexpr_point.applyToLocalPoint(.{ .pos = .new(-1, 0) }), "change vau", .black);
            }
        };

        /// sexprs to cases and vice versa
        const meta_converter = struct {
            const parent_point: Point = Camera.remap(DEFAULT_CAM, .{ .pos = .new(10, -3), .scale = 0.75 }, UI.cam);
            const relative_sexpr_point: Point = .{};
            const relative_case_point: Point = .{ .pos = .new(0, 2.5) };

            pub const Overlap = union(enum) {
                // TODO: detailed case overlap
                case,
                sexpr: core.SexprAddress,
            };

            /// Don't set this directly
            var sexpr: ?*const Sexpr = null;
            /// Don't set this directly
            var case: ?core.MatchCaseDefinition = null;

            pub fn setSexpr(mem: *VeryPermamentGameStuff, new_sexpr: *const Sexpr, local_address: core.SexprAddress) !void {
                sexpr = if (meta_converter.sexpr) |existing| blk: {
                    // const value_without_variables = try new_sexpr.changeAllVariablesToNil(mem);
                    break :blk try existing.setAt(mem, local_address, new_sexpr);
                } else blk: {
                    std.debug.assert(local_address.len == 0);
                    break :blk new_sexpr;
                };

                case = core.caseFromSexpr(sexpr.?, mem.arena_for_cases.allocator(), &mem.pool_for_sexprs) catch |err| switch (err) {
                    error.InvalidMetaFnk, error.BAD_INPUT => null,
                    else => return err,
                };
            }

            pub fn setCase(mem: *VeryPermamentGameStuff, new_case: core.MatchCaseDefinition) !void {
                case = new_case;
                sexpr = try core.sexprFromCase(case.?, &mem.pool_for_sexprs);
                std.log.debug("set case", .{});
            }

            pub fn findOverlap(asdf_mouse_pos: Vec2) !?Overlap {
                const mouse_pos = parent_point.inverseApplyGetLocalPosition(asdf_mouse_pos);
                if (sexpr) |s| {
                    if (try SexprView.overlapsSexpr(
                        platform.gpa,
                        s,
                        relative_sexpr_point,
                        mouse_pos,
                    )) |local| {
                        return .{ .sexpr = local };
                    }
                } else if (SexprView.overlapsAtom(
                    relative_sexpr_point,
                    mouse_pos,
                    .atom,
                )) {
                    return .{ .sexpr = core.emptySexprAddress };
                }

                if (overlapsWithTinyCase(mouse_pos, relative_case_point)) {
                    return .case;
                }

                return null;
            }

            pub fn sexprUiPoint() Point {
                return parent_point.applyToLocalPoint(relative_sexpr_point);
            }

            pub fn caseUiPoint() Point {
                return parent_point.applyToLocalPoint(relative_case_point);
            }

            pub fn sexprWorldPoint(camera: Camera) Point {
                return Camera.remap(UI.cam, sexprUiPoint(), camera);
            }

            pub fn caseWorldPoint(camera: Camera) Point {
                return Camera.remap(UI.cam, caseUiPoint(), camera);
            }

            pub fn draw(camera: Camera) !void {
                const sexpr_point = sexprUiPoint();
                if (sexpr) |s| {
                    try artist.drawSexpr(camera, sexpr_point, s);
                } else {
                    drawer.drawRect(
                        camera,
                        Rect.fromCenterAndSize(sexpr_point.pos, .both(sexpr_point.scale)),
                        .black,
                        null,
                    );
                }

                const case_point = caseUiPoint();
                if (case) |c| {
                    // TODO: case fnk_name and "has_next"
                    try drawTinyCase(camera, case_point, c.pattern, c.template);
                    std.log.debug("drawing tiny case", .{});
                } else {
                    drawer.drawRect(
                        camera,
                        Rect.fromCenterAndSize(case_point.pos, .both(case_point.scale)),
                        .black,
                        null,
                    );
                }
            }
        };

        fn makeCasePhysical(gpa: std.mem.Allocator, case: core.MatchCaseDefinition, point: Point) !CaseState {
            return .{
                .fnk_name = case.fnk_name,
                .pattern = case.pattern,
                .template = case.template,
                .next = if (case.next) |next| try makeCasesPhysical(gpa, next) else null,
                .pattern_point_relative_to_parent = point,
                .incoming_wildcards = &.{},
                .outgoing_wildcards = &.{},
            };
        }

        fn makeCasesPhysical(gpa: std.mem.Allocator, cases: core.MatchCases) OoM!CaseGroup {
            var result = std.ArrayListUnmanaged(CaseState){};
            for (cases.items, 0..) |case, k| {
                try result.append(gpa, try makeCasePhysical(gpa, case, .{ .pos = .new(3, 2.5 + 1.5 * tof32(k)), .scale = 0.5 }));
            }
            return .{ .cases = result, .unfolded = 0 };
        }

        pub fn persist(editing: Self, persistence: *PlayerData) !void {
            const fnk = try editing.getFnk();
            try persistence.fnks.put(fnk.name, fnk.body);
            try persistence.custom_samples.put(fnk.name, try editing.getCustomSamples());
            try platform.setPlayerData(persistence.*, editing.mem);
            try persistence.updateSolvedStatusOfAll(editing.mem);
        }

        pub fn getFnk(self: Self) !Fnk {
            return Fnk{
                .name = self.fnk_name,
                .body = .{ .cases = try getMatchCases(self.mem, self.cases) },
            };
        }

        pub fn getCustomSamples(self: Self) ![]const Sample {
            const result = try self.mem.gpa.alloc(Sample, self.custom_tests.items.len);
            for (self.custom_tests.items, result) |t, *dst| {
                dst.* = .{ .input = t.input, .output = t.expected };
            }
            return result;
        }

        fn makeCaseVirtual(mem: *VeryPermamentGameStuff, case: CaseState) !core.MatchCaseDefinition {
            return .{
                .fnk_name = case.fnk_name,
                .pattern = case.pattern,
                .template = case.template,
                .next = if (case.next) |next|
                    (try getMatchCases(mem, next))
                else
                    null,
            };
        }

        fn getMatchCases(mem: *VeryPermamentGameStuff, group: CaseGroup) OoM!core.MatchCases {
            var result = std.ArrayListUnmanaged(core.MatchCaseDefinition){};
            for (group.cases.items) |case| {
                try result.append(mem.arena_for_cases.allocator(), try makeCaseVirtual(mem, case));
            }
            return result;
        }

        pub fn init(
            fnk_name: *const Sexpr,
            builtin_samples: []const Sample,
            custom_samples: []const Sample,
            fnk_body: core.FnkBody,
            mem: *VeryPermamentGameStuff,
            persistence: *PlayerData,
            session_persistent: *SessionPersistent(platform, drawer),
            tutorial_state: TutorialState,
        ) !Self {
            var cases = try makeCasesPhysical(mem.gpa, fnk_body.cases);
            _ = try cases.updateWildcards(platform.gpa);
            const main_input: *const Sexpr = Sexpr.builtin.nil;

            const tests_reel: TestsReel = .init(.{ .pos = .new(19.5, 0.75), .scale = 0.75 }, tutorial_state.allowCustomTests());

            const tests: []TestCase = try mem.gpa.alloc(TestCase, builtin_samples.len);
            for (tests, builtin_samples) |*dst, src| {
                dst.* = .{ .actual = .unknown, .input = src.input, .expected = src.output };
            }

            try toolbar.special_var_state.next(mem, cases);
            try toolbar.special_case_state.next(mem, cases);

            var custom_tests: std.ArrayList(TestCase) = try .initCapacity(platform.gpa, custom_samples.len);
            for (custom_samples) |sample| {
                try custom_tests.append(.{
                    .input = sample.input,
                    .expected = sample.output,
                    .actual = .unknown,
                });
            }

            var score: ?TestSetScore = null;
            if (DESIGN.instant_feedback) {
                score = try updateSolvedSamples(.{ .name = fnk_name, .body = fnk_body }, tests, custom_tests.items, persistence, mem);
            }

            session_persistent.list_viewer.onEnterLevel(tutorial_state == .intro_to_list_viewer);

            const fnk_manager: FnkManager = .init();
            return .{
                .score = score,
                .tests_reel = tests_reel,
                .fnk_name = fnk_name,
                .samples = tests,
                .mem = mem,
                .persistence = persistence,
                .cases = cases,
                .main_input = if (DESIGN.no_current_data) .invalid_field else main_input,
                .ui_state = .init(tests_reel.reel.rect, fnk_manager.getButtonRect(.load), mem.gpa),
                .favorite_fnks = &persistence.favorite_fnk_names,
                .tutorial_state = tutorial_state,
                .custom_tests = custom_tests,
                .session_persistent = session_persistent,
                // .tutorial_state = if (fnk_name.equals(builtin_levels[0].fnk_name))
                //     .first_level
                // else if (fnk_name.equals(builtin_levels[1].fnk_name))
                //     .second_level
                // else if (fnk_name.equals(builtin_levels[2].fnk_name))
                //     .third_level
                // else if (fnk_name.equals(builtin_levels[3].fnk_name))
                //     .fourth_level
                // else if (fnk_name.equals(builtin_levels[4].fnk_name))
                //     .fifth_level
                // else if (fnk_name.equals(&Sexpr.doLit("reverse")))
                //     .intro_to_create_vaus
                // else
                //     .not_yet_creating_vaus,
                .particles = .init(platform.gpa),
                .particles_cases = .init(platform.gpa),
                .fnk_manager = fnk_manager,
            };
        }

        // TODO: don't call this if nothing actually changed
        fn onChangedSomething(self: *Self) !void {
            try self.resetSolvedSamples(.all);
            _ = try self.cases.updateWildcards(platform.gpa);
        }

        fn resetSolvedSamples(self: *Self, which: TestingFnk(platform, drawer).Which) !void {
            self.score = null;
            if (DESIGN.instant_feedback) {
                self.score = try updateSolvedSamples(try self.getFnk(), self.samples, self.custom_tests.items, self.persistence, self.mem);
            } else {
                self.forgetSolvedSamples(which);
            }
        }

        fn forgetSolvedSamples(self: Self, which: TestingFnk(platform, drawer).Which) void {
            switch (which) {
                .all => {
                    for (self.samples) |*sample| {
                        sample.actual = .unknown;
                    }
                    for (self.custom_tests.items) |*sample| {
                        sample.actual = .unknown;
                    }
                },
                .only => |k| {
                    const sample: *TestCase = self.sampleAt(k);
                    sample.actual = .unknown;
                },
            }
        }

        fn sampleAt(self: Self, k: usize) *TestCase {
            return if (k < self.samples.len) &self.samples[k] else &self.custom_tests.items[k - self.samples.len];
        }

        fn updateSolvedSamplesHelper(self: *Self, which: TestingFnk(platform, drawer).Which) !void {
            switch (which) {
                .all => self.score = try updateSolvedSamples(try self.getFnk(), self.samples, self.custom_tests.items, self.persistence, self.mem),
                .only => |k| {
                    const fnk = try self.getFnk();
                    // TODO: move this elsewhere
                    try self.persistence.fnks.put(fnk.name, fnk.body);
                    var score = try core.ScoringRun.initFromFnks(self.persistence.fnks, self.mem);
                    defer score.deinit(false);
                    const sample = self.sampleAt(k);
                    sample.actual = blk: {
                        var exec = core.ExecutionThread.init(sample.input, fnk.name, &score) catch |err| switch (err) {
                            error.FnkNotFound => break :blk .undefined,
                            else => return err,
                        };
                        defer exec.deinit();

                        const actual_output = exec.getFinalResultBounded(&score, 10_000) catch |err| switch (err) {
                            error.FnkNotFound, error.NoMatchingCase, error.InvalidMetaFnk, error.UsedUndefinedVariable, error.TookTooLong => break :blk .undefined,
                            error.BAD_INPUT => break :blk .undefined,
                            error.OutOfMemory => return err,
                        };
                        break :blk .{ .value = actual_output };
                    };
                },
            }
        }

        fn updateSolvedSamples(fnk: Fnk, samples: []TestCase, custom_tests: []TestCase, persistence: *PlayerData, mem: *VeryPermamentGameStuff) !?TestSetScore {
            // TODO: move this elsewhere
            try persistence.fnks.put(fnk.name, fnk.body);

            var total_score: TestSetScore = .all_0;
            var any_failed = false;

            var score = try core.ScoringRun.initFromFnks(persistence.fnks, mem);
            defer score.deinit(false);

            inline for (&.{ samples, custom_tests }, &.{ true, false }) |asdf, is_official_test| {
                for (asdf) |*sample| {
                    sample.actual = blk: {
                        var exec = core.ExecutionThread.init(sample.input, fnk.name, &score) catch |err| switch (err) {
                            error.FnkNotFound => break :blk .undefined,
                            else => return err,
                        };
                        defer exec.deinit();

                        const actual_output = exec.getFinalResultBounded(&score, 10_000) catch |err| switch (err) {
                            error.FnkNotFound, error.NoMatchingCase, error.InvalidMetaFnk, error.UsedUndefinedVariable, error.TookTooLong => {
                                any_failed = true;
                                break :blk .undefined;
                            },
                            error.BAD_INPUT => {
                                any_failed = true;
                                break :blk .undefined;
                            },
                            error.OutOfMemory => return err,
                        };

                        if (is_official_test) {
                            total_score.total_max_stack += exec.score.max_stack;
                            total_score.total_successful_matches += exec.score.successful_matches;
                        }

                        break :blk .{ .value = actual_output };
                    };
                }
            }

            total_score.code_size = score.score.code_size;
            total_score.compile_time = score.score.compile_time;

            return if (any_failed) null else total_score;
        }

        // TODO: deinit

        fn debugMakeAddress(self: *Self, k: usize) !core.CaseAddress {
            return try debugMakeAddress2(self.mem, k);
        }
        fn debugMakeAddress2(mem: *VeryPermamentGameStuff, k: usize) !core.CaseAddress {
            return try mem.gpa.dupe(usize, &.{k});
        }
        fn childAddress(mem: *VeryPermamentGameStuff, parent_address: core.CaseAddress, k: usize) !core.CaseAddress {
            const new_buf = try mem.gpa.alloc(usize, parent_address.len + 1);
            @memcpy(new_buf[0..parent_address.len], parent_address);
            new_buf[parent_address.len] = k;
            return new_buf;
        }

        pub fn update(self: *Self, delta_seconds: f32) !union(enum) {
            nothing,
            back_to_level_select,
            launch_test: TestingFnk(platform, drawer).Which,
            launch_execution: if (DESIGN.no_current_data) PhysicalSexpr else void,
            change_to: struct {
                fnk_name: *const Sexpr,
                ui_point: Point,
            },
        } {
            var mouse = platform.getMouse();

            try self.ui_state.beginFrame();
            defer self.ui_state.endFrame();

            // assumes that changing cursor is free
            defer platform.setCursor(switch (self.focus) {
                .grabbing => |x| switch (x) {
                    .ui => .pointer,
                    else => .grabbing,
                },
                .hovering => |y| switch (y) {
                    .sexpr => |x| if ((x.address.getSexpr(self.*) catch @panic("TODO")) != null) .could_grab else .default,
                    .ui => .pointer,
                    else => .could_grab,
                },
                .nothing => .default,
            });

            self.ui_state.buttons.getPtr(.back_to_menu).visible = self.allSolved();
            self.ui_state.buttons.getPtr(.check_all).visible = !self.allSolved();
            fnks_reel.update(&self.ui_state, self.tutorial_state.allowPickingVaus());

            try self.tests_reel.update(self.samples.len, self.custom_tests.items.len, &mouse, delta_seconds, &self.ui_state);
            try self.session_persistent.list_viewer.update(&mouse, delta_seconds);
            fnks_reel.reel.update(fnks_reel.nRows(self.favorite_fnks.items.len), &mouse, delta_seconds);
            moveCamera(&self.camera, delta_seconds, platform.getKeyboard(), mouse);
            self.fnk_manager.update(&self.ui_state, self.tutorial_state.allowCreatingVaus());
            self.ui_state.update(delta_seconds, self.focus.getActiveUiLabel(), self.focus.getHotUiLabel());

            const camera = self.camera;

            // focus-specific updates
            switch (self.focus) {
                .grabbing => |*x| switch (x.*) {
                    .ui => {},
                    .handle => |handle| switch (handle) {
                        .list_viewer => self.session_persistent.list_viewer.move(platform.getMouse()),
                        .fnk_manager => self.fnk_manager.move(platform.getMouse(), &self.ui_state),
                        .sexpr_holder => |k| self.session_persistent.sexpr_holders[k].move(platform.getMouse()),
                    },
                    .case => |*grabbing| {
                        // grabbing case parent is the nothing!
                        grabbing.case.pattern_point_relative_to_parent.lerp_towards((Point{
                            .pos = platform.getMouse().cur.pos(camera),
                            .scale = if (grabbing.address_if_released == null) 0.5 else 1,
                        }).applyToLocalPoint(.{ .pos = .new(3, 0) }), 0.6, delta_seconds);
                    },
                    .sexpr => |*grabbing| {
                        grabbing.point.lerp_towards(if (grabbing.address_if_released) |goal|
                            (try goal.getGlobalPoint(self.*))
                                .applyToLocalPoint(switch (goal) {
                                .full_address => |full| switch (full.which) {
                                    .pattern => .{ .turns = 0.02, .pos = .new(-0.5, 0) },
                                    .template => .{ .turns = -0.02, .pos = .new(0.5, 0) },
                                    .fnk_name => .{ .turns = 0.02, .pos = .new(0.5, 0) },
                                },
                                .main_input, .meta_converter, .list_viewer, .custom_test, .sexpr_holder => .{ .turns = -0.02, .pos = .new(0.5, 0) },
                                .fnk_manager => .{ .turns = 0.02, .pos = .new(0.5, 0) },
                                .toolbar, .main_fnk_name, .toolbar_special_var, .sample, .external_fnk => unreachable,
                            })
                        else
                            // TODO: it would be nice to have the scale instantly correct when the camera zooms
                            Point{
                                .pos = platform.getMouse().cur.pos(camera),
                                .scale = camera.height / DEFAULT_CAM.height,
                            }, 0.6, delta_seconds);
                        math.lerp_towards(&grabbing.is_pattern, switch (grabbing.limitation) {
                            .pattern => 1,
                            .template => 0,
                            .none => if (grabbing.address_if_released) |goal|
                                if (goal.isPattern()) 1 else 0
                            else
                                @round(grabbing.is_pattern),
                        }, 0.6, delta_seconds);
                    },
                },
                .nothing => {},
                .hovering => |*x| switch (x.*) {
                    .ui => {},
                    .sexpr => |*hovering| {
                        if (std.meta.activeTag(hovering.address) == .full_address) {
                            const unfolded = hovering.address.full_address.case_address;
                            try self.cases.setUnfolded(unfolded);
                        }
                        hovering.global_point.lerp_towards(
                            (try hovering.address.getGlobalPoint(self.*)).applyToLocalPoint(.{ .scale = 1.1 }),
                            0.6,
                            delta_seconds,
                        );
                    },
                    .case => |*hovering| {
                        if (std.meta.activeTag(hovering.address) == .main_fnk) {
                            try self.cases.setUnfolded(hovering.address.main_fnk.existing);
                        }
                        math.lerp_towards(&hovering.hot, 1, 0.6, delta_seconds);
                    },
                    .handle => {},
                },
            }

            // update cases & focus
            {
                const mouse_pos = platform.getMouse().cur.pos(camera);
                const mouse_ui_pos = platform.getMouse().cur.pos(UI.cam);
                const Overlapped = union(enum) {
                    case: CasePlace,
                    sexpr: SexprPlace,
                    handle: Handle,
                    ui: UI_State.Label,
                };
                const maybe_overlapped: ?Overlapped = if (blk: {
                    if (try asdfUpdateAndReturnOverlap(
                        self,
                        mouse_pos,
                        delta_seconds,
                    )) |overlap|
                        switch (overlap) {
                            .case => |case| break :blk Overlapped{ .case = .{ .main_fnk = case } },
                            .sexpr => |sexpr| break :blk if (!self.tutorial_state.allowPickingIdentity() and
                                sexpr.which == .fnk_name and
                                (try self.cases.getSexprAt(sexpr)).equals(Sexpr.builtin.identity) and
                                self.focus == .nothing)
                                break :blk null
                            else
                                break :blk Overlapped{ .sexpr = .{ .full_address = sexpr } },
                        }
                    else
                        break :blk null;
                }) |overlap|
                    overlap
                else if (toolbar.findOverlap(mouse_ui_pos, .from(self.tutorial_state))) |overlap|
                    .{ .sexpr = .{ .toolbar = overlap.index } }
                else if (try self.tests_reel.findOverlap(mouse_ui_pos, self.samples, self.custom_tests.items)) |overlap|
                    if (overlap.is_custom)
                        .{ .sexpr = .{ .custom_test = overlap.address } }
                    else
                        .{ .sexpr = .{ .sample = overlap.address } }
                else if (if (self.tutorial_state.allowPickingVaus()) try fnks_reel.findOverlap(mouse_ui_pos, self.favorite_fnks.items, .from(self.tutorial_state)) else null) |overlap|
                    .{ .sexpr = .{ .external_fnk = overlap } }
                else if (self.tutorial_state.allowCreatingVaus() and self.fnk_manager.findOverlap(mouse_ui_pos))
                    .{ .sexpr = .fnk_manager }
                else if (self.tutorial_state.allowCreatingVaus() and self.fnk_manager.handlePoint().inverseApplyGetLocalPosition(mouse_ui_pos).magSq() < 1)
                    .{ .handle = .fnk_manager }
                else if (if (self.tutorial_state.allowMeta()) try meta_converter.findOverlap(mouse_ui_pos) else null) |overlap| switch (overlap) {
                    .sexpr => |local| .{ .sexpr = .{ .meta_converter = local } },
                    .case => .{ .case = .meta_converter },
                } else if (toolbar.overlapsWithSpecialVar(mouse_ui_pos, .from(self.tutorial_state), self.cases.anyWildcardInPlay()))
                    .{ .sexpr = .toolbar_special_var }
                else if (DESIGN.no_current_data and SexprView.overlapsAtom(MAIN_INPUT_POS, mouse_pos, .atom))
                    .{ .sexpr = .main_input }
                else if (if (DESIGN.no_current_data) null else try SexprView.overlapsSexpr(self.mem.gpa, self.main_input, MAIN_INPUT_POS, mouse_pos)) |overlap|
                    .{ .sexpr = .{ .main_input = overlap } }
                else if (try SexprView.overlapsSexpr(self.mem.gpa, self.fnk_name, MAIN_FNK_POS, mouse_pos)) |overlap|
                    .{ .sexpr = .{ .main_fnk_name = overlap } }
                else if (toolbar.overlapsWithSpecialCase(mouse_ui_pos, .from(self.tutorial_state)))
                    .{ .case = .toolbar_special_case }
                else if (if (self.tutorial_state.hasListViewer()) try self.session_persistent.list_viewer.findOverlap(mouse_ui_pos, std.meta.activeTag(self.focus) == .grabbing and std.meta.activeTag(self.focus.grabbing) == .sexpr) else null) |overlap|
                    .{ .sexpr = .{ .list_viewer = overlap } }
                else if (if (self.tutorial_state.hasSexprHolders()) blk: {
                    for (self.session_persistent.sexpr_holders, 0..) |holder, k| {
                        if (try holder.findOverlap(mouse_ui_pos)) |v| {
                            break :blk @as(SexprHolder.FullAddress, .{ .index = k, .address = v });
                        }
                    } else break :blk null;
                } else null) |overlap|
                    .{ .sexpr = .{ .sexpr_holder = overlap } }
                else if (if (self.tutorial_state.hasSexprHolders()) blk: {
                    for (self.session_persistent.sexpr_holders, 0..) |holder, k| {
                        if (holder.handlePoint().inverseApplyGetLocalPosition(mouse_ui_pos).magSq() < 1) {
                            break :blk @as(Handle, .{ .sexpr_holder = k });
                        }
                    } else break :blk null;
                } else null) |overlap|
                    .{ .handle = overlap }
                else if (self.tutorial_state.hasListViewer() and self.session_persistent.list_viewer.handlePoint().inverseApplyGetLocalPosition(mouse_ui_pos).magSq() < 1)
                    .{ .handle = .list_viewer }
                else if (self.ui_state.getOverlap(mouse_ui_pos)) |label|
                    .{ .ui = label }
                else
                    null;

                switch (self.focus) {
                    .grabbing => |*x| switch (x.*) {
                        .handle, .ui => {},
                        .case => |*grabbing| if (maybe_overlapped) |overlapped|
                            switch (overlapped) {
                                .case => |place| {
                                    grabbing.address_if_released = if (place.acceptsDrop()) place else null;
                                },
                                .sexpr, .ui, .handle => {
                                    grabbing.address_if_released = null;
                                },
                            }
                        else {
                            grabbing.address_if_released = null;
                        },
                        .sexpr => |*grabbing| if (maybe_overlapped) |overlapped|
                            switch (overlapped) {
                                .case => |place| {
                                    if (std.meta.activeTag(place) == .main_fnk) {
                                        try self.cases.setUnfolded(place.main_fnk.existing);
                                    }
                                    grabbing.address_if_released = null;
                                },
                                .ui, .handle => {
                                    grabbing.address_if_released = null;
                                },
                                .sexpr => |place| {
                                    if (std.meta.activeTag(place) == .full_address) {
                                        try self.cases.setUnfolded(place.full_address.case_address);
                                    }
                                    grabbing.address_if_released = if (!place.acceptsDrop()) null else switch (grabbing.limitation) {
                                        .none => place,
                                        .pattern => if (!place.acceptsWildcards()) null else if (place.isPattern()) place else null,
                                        .template => if (!place.acceptsWildcards()) null else if (place.isPattern()) null else place,
                                    };
                                },
                            }
                        else {
                            grabbing.address_if_released = null;
                        },
                    },
                    .nothing, .hovering => if (maybe_overlapped) |overlapped| {
                        const new_hovering_focus: @TypeOf(self.focus.hovering) = switch (overlapped) {
                            .case => |place| .{ .case = .{ .address = place, .hot = 0 } },
                            .sexpr => |place| .{ .sexpr = .{
                                .address = place,
                                .global_point = try place.getGlobalPoint(self.*),
                            } },
                            .handle => |handle| .{ .handle = handle },
                            .ui => |label| .{ .ui = label },
                        };

                        if (!self.focus.isAlreadyHovering(new_hovering_focus)) {
                            self.focus = .{ .hovering = new_hovering_focus };
                        }
                    } else {
                        self.focus = .nothing;
                    },
                }
            }

            {
                for (self.particles.items) |*p| try p.update(delta_seconds);
                {
                    var k: usize = 0;
                    while (k < self.particles.items.len) {
                        if (self.particles.items[k].remaining_lifetime <= 0) {
                            _ = self.particles.swapRemove(k);
                        } else {
                            k += 1;
                        }
                    }
                }
                for (self.particles_cases.items) |*p| try p.update(delta_seconds);
                {
                    var k: usize = 0;
                    while (k < self.particles_cases.items.len) {
                        if (self.particles_cases.items[k].remaining_lifetime <= 0) {
                            _ = self.particles_cases.swapRemove(k);
                        } else {
                            k += 1;
                        }
                    }
                }
            }

            // Mouse interaction
            if (platform.getMouse().wasPressed(.left)) {
                switch (self.focus) {
                    .nothing => {},
                    .grabbing => |*x| switch (x.*) {
                        .case => |*grabbing| {
                            if (grabbing.address_if_released) |place| {
                                switch (place) {
                                    .main_fnk => |case| {
                                        const address = case.ghost.address;
                                        const global_point = grabbing.case.pattern_point_relative_to_parent;
                                        const parent_point = try self.cases.getPatternGlobalPoint(.{}, address[0 .. address.len - 1]);
                                        grabbing.case.pattern_point_relative_to_parent = parent_point.inverseApplyGetLocal(global_point);
                                        try self.cases.insertAt(self.mem, address, grabbing.case);
                                        try self.onChangedSomething();
                                        self.focus = .{ .hovering = .{ .case = .{ .address = .{ .main_fnk = .{ .existing = address } }, .hot = 1 } } };
                                    },
                                    .meta_converter => {
                                        try meta_converter.setCase(self.mem, try makeCaseVirtual(self.mem, grabbing.case));
                                        self.focus = .{ .hovering = .{ .case = .{ .address = place, .hot = 1 } } };
                                    },
                                    .toolbar_special_case => unreachable,
                                }
                            } else {
                                try self.particles_cases.append(.initFloater(grabbing.case));
                                // try addParticlesForCase(&self.particles, grabbing.case);
                                self.focus = .{ .nothing = {} };
                            }
                        },
                        .sexpr => |grabbing| {
                            if (grabbing.address_if_released) |address| {
                                if (DESIGN.no_current_data and address == .main_input) {
                                    self.focus = .nothing;
                                    return .{ .launch_execution = .{ .value = try grabbing.sexpr.changeAllVariablesToNil(self.mem), .pos = grabbing.point, .is_pattern = grabbing.is_pattern } };
                                } else {
                                    // particle stuff
                                    {
                                        if (try address.getSexpr(self.*)) |old_value| {
                                            if (!try address.isAnInvisibleIdentity(self.*)) {
                                                try self.particles.append(.init(.{
                                                    .is_pattern = if (address.isPattern()) 1 else 0,
                                                    .value = old_value,
                                                    .pos = try address.getGlobalPoint(self.*),
                                                }));
                                            }
                                        }
                                    }

                                    try address.setSexpr(self, grabbing.sexpr);
                                    try self.onChangedSomething();
                                    if (DESIGN.autograb_wildcard_template_after_pattern and grabbing.limitation == .pattern) {
                                        self.focus = .{ .grabbing = .{ .sexpr = .{
                                            .sexpr = grabbing.sexpr,
                                            .address_if_released = null,
                                            .limitation = .template,
                                            .is_pattern = 1,
                                            .point = grabbing.point,
                                        } } };
                                    } else {
                                        self.focus = .{ .hovering = .{ .sexpr = .{
                                            .address = address,
                                            .global_point = grabbing.point,
                                        } } };
                                    }
                                }
                            } else {
                                try self.particles.append(.initFloater(.{
                                    .is_pattern = grabbing.is_pattern,
                                    .value = grabbing.sexpr,
                                    .pos = grabbing.point,
                                }));
                                self.focus = .{ .nothing = {} };
                            }
                        },
                        .handle, .ui => unreachable,
                    },
                    .hovering => |*x| switch (x.*) {
                        .case => |hovering| if (self.tutorial_state.allowGrabbingCases()) {
                            switch (hovering.address) {
                                .main_fnk => |unfolded| {
                                    const global_point = try self.cases.getPatternGlobalPoint(.{}, unfolded.existing);
                                    var asdf = try self.cases.removeAt(unfolded.existing);
                                    try self.onChangedSomething();
                                    const old_point = asdf.pattern_point_relative_to_parent;
                                    asdf.pattern_point_relative_to_parent = global_point;
                                    self.focus = .{ .grabbing = .{ .case = .{
                                        .case = asdf,
                                        .address_if_released = .{ .main_fnk = .{ .ghost = .{
                                            .address = unfolded.existing,
                                            .pattern_point_relative_to_parent = old_point,
                                        } } },
                                    } } };
                                },
                                .meta_converter => {
                                    if (meta_converter.case) |case| {
                                        self.focus = .{ .grabbing = .{
                                            .case = .{
                                                .case = try makeCasePhysical(self.mem.gpa, case, meta_converter.caseWorldPoint(self.camera)),
                                                .address_if_released = hovering.address,
                                            },
                                        } };
                                    }
                                },
                                .toolbar_special_case => {
                                    self.focus = .{ .grabbing = .{ .case = .{
                                        .case = toolbar.special_case_state.value(self.camera),
                                        .address_if_released = null,
                                    } } };
                                    try toolbar.special_case_state.next(self.mem, self.cases);
                                },
                            }
                        },
                        .sexpr => |hovering| {
                            if (try hovering.address.getSexpr(self.*)) |v| {
                                self.focus = .{ .grabbing = .{
                                    .sexpr = .{
                                        .address_if_released = if (hovering.address.acceptsDrop()) hovering.address else null,
                                        .is_pattern = if (hovering.address.isPattern()) 1 else 0,
                                        .point = hovering.global_point,
                                        .sexpr = v,
                                        .limitation = if (v.isFullyResolved())
                                            .none
                                        else if (hovering.address == .toolbar_special_var)
                                            .pattern
                                        else
                                            .template,
                                    },
                                } };

                                switch (hovering.address) {
                                    .full_address => |full| if (full.which == .fnk_name) {
                                        (try self.cases.caseRefAt(hovering.address.full_address.case_address)).fnk_name = Sexpr.builtin.identity;
                                        try self.onChangedSomething();
                                    },
                                    .toolbar_special_var => try toolbar.special_var_state.next(self.mem, self.cases),
                                    .list_viewer => |list| switch (list.which) {
                                        else => {},
                                        .main => if (list.local.len == 0) {
                                            self.session_persistent.list_viewer.value = null;
                                        },
                                        .element => |k| try self.session_persistent.list_viewer.removeElement(k, self.mem),
                                    },
                                    else => {},
                                }
                            }
                        },
                        .handle => |handle| self.focus = .{ .grabbing = .{ .handle = handle } },
                        .ui => |button| {
                            self.focus = .{ .grabbing = .{ .ui = button } };
                        },
                    },
                }
            } else if (platform.getMouse().wasReleased(.left)) {
                switch (self.focus) {
                    .grabbing => |x| switch (x) {
                        else => {},
                        .handle => |handle| self.focus = .{ .hovering = .{ .handle = handle } },
                        .ui => |button| {
                            self.focus = .{ .hovering = .{ .ui = button } };
                            switch (button) {
                                .fixed => |fixed| switch (fixed) {
                                    .back => return .back_to_level_select,
                                    .reset_view => self.camera = DEFAULT_CAM,
                                    // .play => if (DESIGN.no_current_data) unreachable else return .launch_execution,
                                    .check_all => return .{ .launch_test = .all },
                                    .back_to_menu => return .back_to_level_select,
                                    .fnk_manager_load => return .{ .change_to = .{ .fnk_name = self.fnk_manager.cur.?, .ui_point = self.fnk_manager.sexpr_point } },
                                    .toggle_cur_fnk_is_fav => {
                                        for (self.favorite_fnks.items, 0..) |name, k| {
                                            if (name.equals(self.fnk_name)) {
                                                _ = self.favorite_fnks.orderedRemove(k);
                                                break;
                                            }
                                        } else {
                                            try self.favorite_fnks.insert(0, self.fnk_name);
                                        }
                                    },
                                },
                                .temp => |temp| switch (temp) {
                                    .run_test => |k| return .{ .launch_test = .{ .only = k } },
                                    .delete_custom_test => |k| _ = self.custom_tests.orderedRemove(k),
                                    .add_custom_test => try self.custom_tests.append(.{
                                        .input = Sexpr.builtin.nil,
                                        .expected = Sexpr.builtin.nil,
                                        .actual = .unknown,
                                    }),
                                },
                            }
                        },
                    },
                    else => {},
                }
            }

            if (platform.getMouse().wasPressed(.right)) {
                switch (self.focus) {
                    else => {},
                    .hovering => |x| switch (x) {
                        else => {},
                        .sexpr => |hovering| {
                            if (hovering.address.acceptsDrop()) {
                                if (try hovering.address.getSexpr(self.*)) |old_value| {
                                    const new_value = try self.mem.storeSexpr(Sexpr.doPair(
                                        if (hovering.address.isPattern())
                                            try self.freshVarName()
                                        else
                                            Sexpr.builtin.nil,
                                        old_value,
                                    ));
                                    try hovering.address.setSexpr(self, new_value);
                                    try self.onChangedSomething();
                                }
                            }
                        },
                    },
                }
            }

            return .nothing;
        }

        fn isPattern(which: @FieldType(core.FullAddress, "which")) f32 {
            return switch (which) {
                .pattern => 1,
                else => 0,
            };
        }

        fn allSolved(self: Self) bool {
            return TestCase.allSolvedBase(self.samples) and TestCase.allSolvedBase(self.custom_tests.items);
        }

        pub fn draw(self: Self, custom_samples: PlayerData.SamplesCollection) !void {
            artist.highlighted_var = switch (self.focus) {
                else => null,
                .hovering => |x| switch (x) {
                    else => null,
                    .sexpr => |hovering| if (try hovering.address.getSexpr(self)) |value|
                        if (value.isVar()) value.atom_var.value else null
                    else
                        null,
                },
            };

            const camera = self.camera;
            {
                artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                if (!DESIGN.no_current_data) try artist.drawSexpr(
                    camera,
                    MAIN_INPUT_POS,
                    self.main_input,
                );
                try artist.drawHoldedFnk(camera, MAIN_FNK_POS, 1, self.fnk_name);
            }

            for (self.particles.items) |p| try p.draw(camera);
            for (self.particles_cases.items) |p| try p.draw(camera);

            const wildcard_names_in_grabbing_sexpr: ?std.ArrayList([]const u8) = blk: {
                switch (self.focus) {
                    else => break :blk null,
                    .grabbing => |x| switch (x) {
                        else => break :blk null,
                        .sexpr => |grabbing| if (grabbing.limitation == .template) {
                            var r: std.ArrayList([]const u8) = .init(platform.gpa);
                            try grabbing.sexpr.getAllVarNames(&r);
                            break :blk r;
                        } else break :blk null,
                    },
                }
            };
            defer if (wildcard_names_in_grabbing_sexpr) |x| x.deinit();
            try drawCases(camera, true, .{}, self.cases, if (wildcard_names_in_grabbing_sexpr) |x| x.items else null);
            try toolbar.draw(.from(self.tutorial_state), self.cases.anyWildcardInPlay());
            // switch (self.tutorial_state.getToolbar()) {
            //     .normal =>
            //     .hidden => {},
            // }

            try self.tests_reel.draw(self.samples, self.custom_tests.items);
            if (self.allSolved()) {
                drawer.drawDebugText(UI.cam, .{ .pos = self.tests_reel.reel.rect.get(.bottom_center).add(.new(0, 1.3)), .scale = 0.75 }, "All Tests passed!", .black);
            }
            if (self.score) |score| {
                drawer.drawDebugText(
                    UI.cam,
                    .{
                        .pos = self.tests_reel.reel.rect.get(.bottom_center).add(.new(0, 3)),
                        .scale = 0.75,
                    },
                    try score.display(self.tutorial_state.allowMeta()),
                    .black,
                );
            }
            if (self.tutorial_state.allowPickingVaus()) try fnks_reel.draw(self.favorite_fnks.items, .from(self.tutorial_state));
            if (self.tutorial_state.allowCreatingVaus()) try self.fnk_manager.draw();
            if (self.tutorial_state.allowMeta()) try meta_converter.draw(UI.cam);
            if (self.tutorial_state.hasListViewer()) try self.session_persistent.list_viewer.draw();
            if (self.tutorial_state.hasSexprHolders()) for (self.session_persistent.sexpr_holders) |h| try h.draw();

            if (self.tutorial_state == .third_level and self.cases.cases.items.len > 0) {
                const address_to_place_vau_name: core.FullAddress = .{
                    .case_address = &.{0},
                    .which = .fnk_name,
                    .sexpr_address = &.{},
                };
                if ((try self.cases.getSexprAt(address_to_place_vau_name)).equals(Sexpr.builtin.identity))
                    drawer.drawAtomHint(camera, try self.cases.getGlobalPointOf(.{}, address_to_place_vau_name));
            }

            switch (self.focus) {
                .nothing => {},
                .hovering => |x| switch (x) {
                    .ui => {},
                    .handle => |handle| drawer.drawCircle(UI.cam, handle.point(self), .black, .gray(160)),
                    .sexpr => |hovering| {
                        if (try hovering.address.getSexpr(self)) |value| {
                            try artist.drawBothSexpr(
                                camera,
                                hovering.global_point,
                                if (hovering.address.isPattern()) 1 else 0,
                                value,
                            );
                            // try artist.drawPatternOutline(camera, artist.sexprPatternChildView(
                            //     case.pattern_point,
                            //     full_address.sexpr_address,
                            // ));
                            if (value.isVar()) {}
                            switch (hovering.address) {
                                else => {},
                                .external_fnk => |address| if (address.local.len == 0) {
                                    if (findBuiltinLevel(value)) |level| {
                                        const asdf: SamplesReel = .init();
                                        const foo: []bool = try platform.gpa.alloc(bool, level.manual_samples.len);
                                        defer platform.gpa.free(foo);
                                        drawer.setTransparency(0.5);
                                        defer drawer.setTransparency(1);
                                        try asdf.draw(level.manual_samples, foo, 0);
                                    } else if (custom_samples.get(value)) |samples| {
                                        const asdf: SamplesReel = .init();
                                        const foo: []bool = try platform.gpa.alloc(bool, samples.len);
                                        defer platform.gpa.free(foo);
                                        drawer.setTransparency(0.5);
                                        defer drawer.setTransparency(1);
                                        try asdf.draw(samples, foo, 0);
                                    }
                                },
                            }
                        }
                    },
                    .case => |hovering| switch (hovering.address) {
                        .main_fnk => |unfolded| {
                            const pattern_point = try self.cases.getPatternGlobalPoint(.{}, unfolded.existing);
                            drawer.drawCaseHolderExtended(camera, .{
                                .pos = pattern_point.pos.sub(.new(3, 0)),
                                .scale = hovering.hot,
                            }, self.tutorial_state != .first_level);
                        },
                        .meta_converter => if (meta_converter.case != null) {
                            drawTinyCaseHolder(meta_converter.caseUiPoint(), hovering.hot);
                        },
                        .toolbar_special_case => {
                            drawTinyCaseHolder(toolbar.special_case_point, hovering.hot);
                        },
                    },
                },
                .grabbing => |x| switch (x) {
                    .ui => {},
                    .handle => |handle| drawer.drawCircle(UI.cam, handle.point(self), .black, .gray(192)),
                    .sexpr => |grabbing| {
                        try artist.drawBothSexpr(
                            camera,
                            grabbing.point,
                            grabbing.is_pattern,
                            grabbing.sexpr,
                        );
                        if (grabbing.limitation == .template) {
                            try artist.drawWildcardLinesToFloating(camera, .{}, self.cases, grabbing.point, wildcard_names_in_grabbing_sexpr.?.items);
                        }
                    },
                    .case => |grabbing| {
                        if (grabbing.address_if_released) |place| {
                            switch (place) {
                                .main_fnk => |address| {
                                    drawer.setTransparency(0.5);
                                    const parent_pattern_point = (try self.cases
                                        .getPatternGlobalPoint(.{}, address.ghost.address[0 .. address.ghost.address.len - 1]));
                                    const pattern_point = parent_pattern_point
                                        .applyToLocalPoint(address.ghost.pattern_point_relative_to_parent);
                                    try artist.drawPatternSexpr(
                                        camera,
                                        pattern_point,
                                        grabbing.case.pattern,
                                    );
                                    try drawCaseExtra(camera, pattern_point, grabbing.case, null);
                                    drawer.drawCaseHolderFromPatternPoint(camera, pattern_point);
                                    const pos = pattern_point.applyToLocalPosition(.new(0, 1));
                                    const esquina = pos.sub(.new(if (address.ghost.address.len == 1) 5 else 3, 0));
                                    drawer.drawCable(camera, esquina, pos, 1, 0);
                                    drawer.drawLine(camera, &.{ esquina, parent_pattern_point.applyToLocalPosition(.new(if (address.ghost.address.len == 1) 0 else 1, 0)) }, .black);
                                    drawer.setTransparency(1);
                                },
                                else => {}, // TODO
                            }
                        }
                        // grabbing case parent is the nothing!
                        const pattern_point = grabbing.case.pattern_point_relative_to_parent;
                        try artist.drawPatternSexpr(
                            camera,
                            pattern_point,
                            grabbing.case.pattern,
                        );
                        try drawCaseExtra(camera, pattern_point, grabbing.case, null);
                    },
                },
            }

            self.ui_state.draw();

            switch (self.tutorial_state) {
                // ↑←→
                .none, .not_yet_creating_vaus, .not_yet_creating_vaus_or_lists => {},
                .first_level => {
                    // drawer.drawDebugText(camera, .{ .pos = .new(-3.55, -2), .scale = 0.75 }, "That's the name of →\nthe Vau you're editing.", .black);
                    drawer.drawDebugText(camera, .{ .pos = .new(4.5, -2), .scale = 0.75 }, "← That's the name of\nthe Vau you're editing.", .black);
                    if (DESIGN.no_current_data) {
                        drawer.drawDebugText(camera, .{ .pos = .new(8, 0), .scale = 0.75 }, "↓ a Vau is a list of Cases: if the left Data matches,\nthe result will be the right side's Data.", .black);
                        // drawer.drawDebugText(camera, .{ .pos = .new(9, 0), .scale = 0.75 }, "← Place some Data here to run the Vau on it.", .black);
                    } else {
                        drawer.drawDebugText(camera, .{ .pos = .new(8, 0), .scale = 0.75 }, "← That gray thing is the current Data;\nfeel free to change it by\ndropping some other Data on it.", .black);
                        // drawer.drawDebugText(camera, .{ .pos = .new(6, -1.85), .scale = 0.75 }, "↓ That gray thing is the current Data;\nfeel free to change it by\ndropping some other Data on it.", .black);
                        drawer.drawDebugText(camera, .{ .pos = .new(3.5, -4), .scale = 0.75 }, "← Click Play to see the Vau applied to the current Data.", .black);
                    }
                    drawer.drawDebugText(camera, .{ .pos = .new(-3, 4), .scale = 0.75 }, "Left click to\npick/drop Data", .black);
                    // drawer.drawDebugText(camera, .{ .pos = .new(10, 1), .scale = 0.75 }, "↓ These are the Cases that make up the Vau.", .black);
                    if (!self.allSolved()) {
                        drawer.drawDebugText(UI.cam, .{ .pos = self.tests_reel.reel.rect.get(.bottom_center).add(.new(0, 2)), .scale = 1.25 }, "Click to check ↑\nif your Vau works", .black);
                        // drawer.drawDebugText(UI.cam, .{ .pos = self.samples_reel.top_left.pos.add(.new(2.75, 6.75)), .scale = 0.75 }, "↑\nThese Tests are the Data\ntransformations your Vau\nmust achieve.", .black);
                    }
                },
                .second_level => {
                    drawer.drawDebugText(camera, .{ .pos = .new(7, -3.75), .scale = 0.75 }, "↓ This special Data is called a Wildcard,\nand will match with any other Data.", .black);
                    drawer.drawDebugText(camera, .{ .pos = .new(8, 8), .scale = 0.75 }, "All the Tests for this Vau have the same structure; use a Wildcard to solve them with a single Case.", .black);
                },
                .third_level => {
                    drawer.drawDebugText(camera, .{ .pos = .new(-3.2, -2), .scale = 0.6 }, "Remember, each Vau\nhas a name →", .black);
                    drawer.drawDebugText(UI.cam, .{ .pos = fnks_reel.reel.rect.get(.top_right).add(.new(3.5, 1)), .scale = 0.75 }, "← your collection of Vaus.", .black);
                    drawer.drawDebugText(camera, .{ .pos = .new(9, 0.6), .scale = 0.75 }, "Place a Vau name here to call it on the result.\n↓", .black);
                    if (!DESIGN.no_current_data) drawer.drawDebugText(camera, .{ .pos = .new(2.5, -4), .scale = 0.75 }, "← Don't forget to hit Play to see the Vau in action!", .black);
                },
                .fourth_level => {
                    drawer.drawDebugText(camera, .{ .pos = .new(7.5, -3.5), .scale = 0.75 }, "↓ Add new Cases with this", .black);
                    drawer.drawDebugText(camera, .{ .pos = .new(3, 6), .scale = 0.75 }, "Nested Cases will\nbe called on the result →", .black);
                },
                .fifth_level => {
                    drawer.drawDebugText(camera, .{ .pos = .new(5, 9.5), .scale = 0.75 }, "You're now on your own. Good luck!", .black);
                },
                .intro_to_create_vaus => {
                    drawer.drawDebugText(UI.cam, self.fnk_manager.sexpr_point.applyToLocalPoint(.{ .pos = .new(-3, 0) }), "↑\nPlace any Data here\nto create a Vau\nwith that name", .black);
                },
                .intro_to_list_viewer => {
                    drawer.drawDebugText(
                        UI.cam,
                        self.session_persistent.list_viewer.mainPoint().applyToLocalPoint(.{ .pos = .new(-2.5, 0.2) }).applyToLocalPoint(.{ .scale = 0.5 }),
                        "Here's a helper tool\nto work with lists →",
                        .black,
                    );
                },
            }
        }

        fn drawCases(camera: Camera, is_first: bool, parent_point: Point, group: CaseGroup, held_wildcard_names: ?[]const []const u8) OoM!void {
            for (group.cases.items) |case| {
                const pattern_point = parent_point.applyToLocalPoint(case.pattern_point_relative_to_parent);
                try artist.drawPatternSexpr(
                    camera,
                    pattern_point,
                    case.pattern,
                );

                const pos = pattern_point.applyToLocalPosition(.new(0, 1));
                drawer.drawCable(
                    camera,
                    pos.sub(.new(parent_point.scale * if (is_first) tof32(5.0) else tof32(3.0), 0)),
                    pos,
                    1,
                    0,
                );

                if (case.pattern_point_relative_to_parent.scale >= 0.9) {
                    try drawCaseExtra(camera, pattern_point, case, held_wildcard_names);
                }
            }

            var prev_point = parent_point.applyToLocalPosition(if (is_first) .zero else .new(1, 0));
            for (group.cases.items, 0..) |case, k| {
                const cur_point = parent_point
                    .applyToLocalPoint(case.pattern_point_relative_to_parent)
                    .applyToLocalPosition(.new(0, 1))
                    .sub(.new(parent_point.scale * if (is_first) tof32(5.0) else tof32(3.0), 0));
                defer prev_point = cur_point;
                drawer.drawLine(camera, &.{ prev_point, cur_point }, .black);

                var asdf: std.ArrayList([]const u8) = .init(platform.gpa);
                defer asdf.deinit();
                for (group.cases.items[k..]) |next_case| {
                    try appendUniqueNames(&asdf, next_case.incoming_wildcards);
                }
                try artist.drawWildcardsCable(camera, &.{ prev_point, cur_point }, asdf.items);
            }
        }

        fn drawCaseExtra(camera: Camera, pattern_point: Point, case: CaseState, held_wildcard_names: ?[]const []const u8) !void {
            const template_point = if (DESIGN.horizontal_depth) blk: {
                var template_point = pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) });
                template_point.pos.y = 0;
                break :blk template_point;
            } else pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) });
            try artist.drawSexpr(
                camera,
                template_point,
                case.template,
            );
            try artist.drawHoldedFnk(camera, template_point.applyToLocalPoint(FNK_NAME_OFFSET_FROM_TEMPLATE), 0, case.fnk_name);
            drawer.drawCable(
                camera,
                pattern_point.applyToLocalPosition(.new(0.5, 0)),
                template_point.applyToLocalPosition(.new(-0.5, 0)),
                pattern_point.scale,
                0,
            );
            try artist.drawPlacedWildcardsCable(
                camera,
                pattern_point,
                template_point,
                case.pattern,
                case.template,
                held_wildcard_names,
                case.incoming_wildcards,
                case.outgoing_wildcards,
                .none,
                0,
            );

            try artist.drawTemplateWildcardLines(camera, case.template, template_point);
            try artist.drawPatternWildcardLines(camera, case.pattern, pattern_point);
            if (case.next) |next| {
                try drawCases(
                    camera,
                    false,
                    if (DESIGN.horizontal_depth)
                        template_point.applyToLocalPoint(.{ .pos = .new(-DIST_TO_TEMPLATE * 0.5, 0) })
                    else
                        pattern_point,
                    next,
                    held_wildcard_names,
                );
            }
        }

        fn asdfUpdateAndReturnOverlap(self: *Self, mouse_pos: Vec2, delta_seconds: f32) !?OverlapResult {
            if (std.meta.activeTag(self.focus) == .grabbing and std.meta.activeTag(self.focus.grabbing) == .case) {
                const main_fnk_address_if_released = if (self.focus.grabbing.case.address_if_released) |address_if_released|
                    switch (address_if_released) {
                        .main_fnk => |x| x.ghost.address,
                        else => null,
                    }
                else
                    null;
                try doGrabbingCaseFirstPass(self.mem, main_fnk_address_if_released, &.{}, self.cases, delta_seconds);
                const asdf: ?CaseAddressWithPoint = if (self.cases.cases.items.len == 0)
                    .{ .address = try self.debugMakeAddress(0), .pattern_point_relative_to_parent = relativePatternPoint(true, false, 2) }
                else
                    try doGrabbingCaseSecondPass(
                        mouse_pos,
                        main_fnk_address_if_released,
                        self.mem,
                        &.{},
                        &self.cases,
                    );
                if (asdf) |x| {
                    return OverlapResult{ .case = .{ .ghost = x } };
                } else {
                    return null;
                }
            } else {
                return try updateCasePositionsAndReturnMouseOverlap(
                    self.mem,
                    &.{},
                    mouse_pos,
                    self.cases,
                    delta_seconds,
                );
            }
        }

        const OverlapResult = union(enum) {
            case: @FieldType(CasePlace, "main_fnk"),
            // case: union(enum) {
            //     existing: core.CaseAddress,
            //     ghost: core.CaseAddress,
            // },
            sexpr: core.FullAddress,
        };

        fn relativePatternPoint(is_gen0: bool, is_folded: bool, cur_top_line: f32) Point {
            return .{
                .pos = .new(if (is_gen0) 5 else 4, cur_top_line + if (is_folded) tof32(0.5) else 1.0),
                .scale = if (is_folded) 0.5 else 1,
            };
        }

        fn updateCasePositionsAndReturnMouseOverlap(mem: *VeryPermamentGameStuff, parent_address: core.CaseAddress, maybe_relative_mouse_pos: ?Vec2, group: CaseGroup, delta_seconds: f32) !?OverlapResult {
            const is_gen0 = parent_address.len == 0;
            var cur_top_line: f32 = 2;
            const unfolded = group.unfolded;

            var overlapped: ?OverlapResult = null;
            for (group.cases.items, 0..) |*case, k| {
                const is_folded: bool = k != unfolded;
                defer cur_top_line += if (is_folded) 1.5 else 2.5;
                const relative_pattern_point = relativePatternPoint(is_gen0, is_folded, cur_top_line);
                case.pattern_point_relative_to_parent.lerp_towards(relative_pattern_point, 0.6, delta_seconds);

                const cur_address = try childAddress(mem, parent_address, k);

                const maybe_local_mouse_pos = if (maybe_relative_mouse_pos) |relative_mouse_pos|
                    relative_pattern_point.inverseApplyGetLocalPosition(relative_mouse_pos)
                else
                    null;

                if (maybe_relative_mouse_pos) |relative_mouse_pos| {
                    const local_mouse_pos = maybe_local_mouse_pos.?;
                    if (try SexprView.overlapsPatternSexpr(
                        platform.gpa,
                        case.pattern,
                        relative_pattern_point,
                        relative_mouse_pos,
                    )) |local_address| {
                        overlapped = .{ .sexpr = .{
                            .case_address = cur_address,
                            .sexpr_address = local_address,
                            .which = .pattern,
                        } };
                    } else if (blk: {
                        if (is_folded) break :blk null;
                        break :blk try SexprView.overlapsSexpr(
                            platform.gpa,
                            case.template,
                            relative_pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                            relative_mouse_pos,
                        );
                    }) |local_address| {
                        overlapped = .{ .sexpr = .{
                            .case_address = cur_address,
                            .sexpr_address = local_address,
                            .which = .template,
                        } };
                    } else if (blk: {
                        if (is_folded) break :blk null;
                        break :blk try SexprView.overlapsSexpr(
                            platform.gpa,
                            case.fnk_name,
                            relative_pattern_point.applyToLocalPoint(FNK_NAME_OFFSET),
                            relative_mouse_pos,
                        );
                    }) |local_address| {
                        overlapped = .{ .sexpr = .{
                            .case_address = cur_address,
                            .sexpr_address = local_address,
                            .which = .fnk_name,
                        } };
                    } else if (inRange(local_mouse_pos.y, -1, 1) and
                        inRange(local_mouse_pos.x, -5 / case.pattern_point_relative_to_parent.scale, 0))
                    {
                        overlapped = .{ .case = .{ .existing = cur_address } };
                    }
                }

                if (!is_folded) if (case.next) |next| {
                    const child_overlap = try updateCasePositionsAndReturnMouseOverlap(
                        mem,
                        cur_address,
                        maybe_local_mouse_pos,
                        next,
                        delta_seconds,
                    );
                    if (child_overlap) |child| {
                        overlapped = child;
                    }
                };
            }
            return overlapped;
        }

        fn getUnfoldedChild(address_if_released: ?core.CaseAddress, parent_address: core.CaseAddress, group_unfolded: usize) union(enum) {
            normal: usize,
            above: usize,
        } {
            if (address_if_released) |k| {
                if (k.len == parent_address.len + 1) {
                    return .{ .above = k[parent_address.len] };
                } else {
                    return .{ .normal = group_unfolded };
                }
            } else {
                return .{ .normal = group_unfolded };
            }
        }

        fn doGrabbingCaseFirstPass(mem: *VeryPermamentGameStuff, address_if_released: ?core.CaseAddress, parent_address: core.CaseAddress, group: CaseGroup, delta_seconds: f32) !void {
            // first pass just to update positions almost as usual
            const is_gen0 = parent_address.len == 0;
            var cur_top_line: f32 = 2;
            const unfolded = getUnfoldedChild(address_if_released, parent_address, group.unfolded);
            for (group.cases.items, 0..) |*case, k| {
                if (std.meta.eql(unfolded, .{ .above = k })) {
                    cur_top_line += 1.5;
                }
                const is_folded = !std.meta.eql(unfolded, .{ .normal = k });
                defer cur_top_line += if (is_folded) 1.5 else 2.5;
                const relative_pattern_point = Point{
                    .pos = .new(if (is_gen0) 5 else 4, cur_top_line + if (is_folded) tof32(0.5) else 1.0),
                    .scale = if (is_folded) 0.5 else 1,
                };
                case.pattern_point_relative_to_parent.lerp_towards(
                    relative_pattern_point,
                    0.6,
                    delta_seconds,
                );

                const cur_address = try childAddress(mem, parent_address, k);
                if (!is_folded) if (case.next) |next| {
                    try doGrabbingCaseFirstPass(
                        mem,
                        address_if_released,
                        cur_address,
                        next,
                        delta_seconds,
                    );
                };
            }
        }

        fn doGrabbingCaseSecondPass(
            mouse_pos_relative_to_parent: Vec2,
            address_if_released: ?core.CaseAddress,
            mem: *VeryPermamentGameStuff,
            parent_address: core.CaseAddress,
            group: *CaseGroup,
        ) !?CaseAddressWithPoint {
            // second pass to update the grabbing state
            for (group.cases.items, 0..) |*case, k| {
                const grabbing_pos_relative_to_cur = Point.inverseApplyGetLocalPosition(
                    case.pattern_point_relative_to_parent,
                    mouse_pos_relative_to_parent,
                );
                if (inRange(
                    grabbing_pos_relative_to_cur.y,
                    -1,
                    1,
                ) and inRange(
                    grabbing_pos_relative_to_cur.x,
                    -5.0 / case.pattern_point_relative_to_parent.scale,
                    0,
                )) {
                    group.unfolded = k;
                    return null;
                }
            } else {
                const below_the_cable = mouse_pos_relative_to_parent.y > 0;
                for (group.cases.items, 0..) |*case, k| {
                    const grabbing_pos_relative_to_cur = Point.inverseApplyGetLocalPosition(
                        case.pattern_point_relative_to_parent,
                        mouse_pos_relative_to_parent,
                    );
                    if (below_the_cable and grabbing_pos_relative_to_cur.y < 0 and inRange(
                        grabbing_pos_relative_to_cur.x,
                        -5.0 / case.pattern_point_relative_to_parent.scale,
                        0,
                    )) {
                        return .{
                            .address = try childAddress(mem, parent_address, k),
                            .pattern_point_relative_to_parent = case
                                .pattern_point_relative_to_parent
                                .applyToLocalPoint(.{ .scale = 1.5, .pos = .new(0, -3) }),
                        };
                    }
                } else {
                    if (group.cases.items.len > 0) {
                        const last_case = group.cases.items[group.cases.items.len - 1];
                        const grabbing_pos_relative_to_last = Point.inverseApplyGetLocalPosition(
                            last_case.pattern_point_relative_to_parent,
                            mouse_pos_relative_to_parent,
                        );
                        if (grabbing_pos_relative_to_last.y > 0 and inRange(
                            grabbing_pos_relative_to_last.x,
                            -5.0 / last_case.pattern_point_relative_to_parent.scale,
                            0,
                        )) {
                            return .{
                                .address = try childAddress(mem, parent_address, group.cases.items.len),
                                .pattern_point_relative_to_parent = last_case
                                    .pattern_point_relative_to_parent
                                    .applyToLocalPoint(.{ .scale = 1.5, .pos = .new(0, 3) }),
                            };
                        }
                    }

                    const unfolded = getUnfoldedChild(address_if_released, parent_address, group.unfolded);
                    for (group.cases.items, 0..) |*case, k| {
                        const is_unfolded = std.meta.eql(unfolded, .{ .normal = k });
                        if (is_unfolded) {
                            const cur_address = try childAddress(mem, parent_address, k);
                            const cur_relative_mouse = Point.inverseApplyGetLocalPosition(
                                case.pattern_point_relative_to_parent,
                                mouse_pos_relative_to_parent,
                            );
                            if (case.next) |*next| {
                                const child_thing = try doGrabbingCaseSecondPass(
                                    cur_relative_mouse,
                                    address_if_released,
                                    mem,
                                    cur_address,
                                    next,
                                );
                                if (child_thing) |x| {
                                    return x;
                                }
                            } else if (inRange(cur_relative_mouse.x, 0, 5) and cur_relative_mouse.y > 0) {
                                return .{
                                    .address = try childAddress(mem, cur_address, 0),
                                    .pattern_point_relative_to_parent = relativePatternPoint(false, false, 2),
                                };
                            } else {
                                return null;
                            }
                        }
                    } else {
                        return null;
                    }
                }
            }
        }
    };
}

pub fn ExecutingFnk(platform: Platform, drawer: Drawer) type {
    return struct {
        const Self = @This();
        const artist = Artist(platform, drawer);

        const BASE_SPEED = 1;
        const SKIP_SPEED_MULT = 1.5;

        const text_pos: Point = (Point{ .pos = DEFAULT_CAM.center, .scale = 3 }).applyToLocalPoint(.{ .pos = .new(0, -1) });
        pub const result_pos: Point = text_pos.applyToLocalPoint(.{ .pos = .new(-1, 2) });
        pub const bad_fnk_pos: Point = text_pos.applyToLocalPoint(.{ .pos = .new(0, 2), .turns = -0.25, .scale = 0.5 });
        pub const expected_output_pos: Point = text_pos.applyToLocalPoint(.{ .scale = 0.25, .pos = .new(3.25, -0.75) });

        // TODO: previous step, speed controls, different step speed
        // TODO: draw the variable name on bound values

        // TODO: remove this, probably
        scoring_run: *core.ScoringRun,
        thread: core.ExecutionThread,
        camera: Camera,
        ui_state: UI.State,
        good_result_ui_state: UI.State,
        bad_result_ui_state: UI.State,

        // TODO: better rewind
        thread_initial_params: struct {
            value: *const Sexpr,
            fn_name: *const Sexpr,

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
        },
        done_steps: usize = 0,

        anim_t: f32 = 0,
        anim_state: union(enum) {
            normal,
            paused,
            advancing,
            backwards: ?core.ExecutionThread,
        } = .normal,
        fast: bool = false,

        result: ?core.ExecutionThread.Result = null,
        main_input: if (DESIGN.no_current_data) PhysicalSexpr else enum { invalid_field },

        result_ui_point_for_test: ?Point,

        pub fn init(
            input: if (DESIGN.no_current_data) PhysicalSexpr else *const Sexpr,
            fn_name: *const Sexpr,
            scoring_run: *core.ScoringRun,
            camera: Camera,
            result_ui_point_for_test: ?Point,
        ) !Self {
            const thread_initial_params: @FieldType(Self, "thread_initial_params") = .{
                .value = if (DESIGN.no_current_data) input.value else input,
                .fn_name = fn_name,
            };
            var result = Self{
                .thread_initial_params = thread_initial_params,
                .thread = try thread_initial_params.toThread(scoring_run),
                .scoring_run = scoring_run,
                .camera = camera,
                .ui_state = .{ .buttons = try UI.Button.row(platform.gpa, .zero, .one, &.{
                    "⏹",
                    "Reset\nView",
                    "⏯",
                    "⏩",
                    "⏮",
                    "⏭",
                }) },
                .good_result_ui_state = .{ .buttons = try platform.gpa.dupe(UI.Button, &[1]UI.Button{
                    .{ .pos = .fromCenterAndSize(text_pos.applyToLocalPosition(
                        .new(5, 4),
                    ), .new(3, 1)), .text = "Back to menu" },
                }) },
                .bad_result_ui_state = .{ .buttons = try platform.gpa.dupe(UI.Button, &[1]UI.Button{
                    .{ .pos = .fromCenterAndSize(text_pos.applyToLocalPosition(
                        .new(5, 4),
                    ), .new(3, 1)), .text = "Keep editing" },
                }) },
                .main_input = if (DESIGN.no_current_data) input else .invalid_field,
                .result_ui_point_for_test = result_ui_point_for_test,
            };

            // for now, skip the "start" anim
            // std.debug.assert(null == try result.thread.advanceTinyStep(result.scoring_run));
            const asdf = try result.thread.advanceTinyStep(result.scoring_run);
            result.result = asdf;

            return result;
        }

        pub fn isFinished(self: Self) bool {
            return self.result != null and self.anim_t >= 1;
        }

        pub fn update(self: *Self, delta_seconds: f32) OoM!union(enum) { nothing, back_to_editing } {
            if (self.ui_state.update(platform.getMouse(), delta_seconds)) |pressed_button|
                switch (pressed_button) {
                    0 => return .back_to_editing,
                    1 => self.camera = DEFAULT_CAM,
                    2 => {
                        self.anim_state = switch (self.anim_state) {
                            .paused => .normal,
                            else => .paused,
                        };
                        self.fast = false;
                    },
                    3 => {
                        self.anim_state = .normal;
                        self.fast = !self.fast;
                    },
                    4 => self.anim_state = .{ .backwards = if (self.done_steps == 0 or self.anim_t > 0)
                        null
                    else
                        self.thread_initial_params.startThreadAndRunItToStep(self.scoring_run, self.done_steps) catch |err| switch (err) {
                            error.OutOfMemory => |x| return x,
                            else => unreachable,
                        } },
                    5 => self.anim_state = .advancing,
                    else => return error.TODO,
                };

            // move camera
            moveCamera(&self.camera, delta_seconds, platform.getKeyboard(), platform.getMouse());

            if (DESIGN.no_current_data) {
                self.main_input.pos.lerp_towards(MAIN_INPUT_POS, 0.6, delta_seconds);
                math.lerp_towards(&self.main_input.is_pattern, 0, 0.6, delta_seconds);
            }

            const debug_slowdown: f32 = if (platform.getMouse().cur.isDown(.right)) 0.05 else 1;
            switch (self.anim_state) {
                .paused => {},
                .normal => {
                    const speed: f32 = if (self.fast) 4 else 1;
                    self.anim_t += debug_slowdown * speed * delta_seconds * BASE_SPEED * self.curStepSpeed();
                    if (self.anim_t >= 1 and self.result != null) {
                        self.anim_t = 1;
                        self.anim_state = .paused;
                    }
                },
                .advancing => {
                    const advance_step_size = delta_seconds * SKIP_SPEED_MULT * BASE_SPEED * self.curStepSpeed();
                    self.anim_t += advance_step_size;
                    if (self.anim_t >= 1) {
                        self.anim_t = 1;
                        self.anim_state = .paused;
                    }
                },
                .backwards => |*asdf| {
                    if (asdf.*) |x| {
                        const advance_step_size = delta_seconds * SKIP_SPEED_MULT * BASE_SPEED * self.curStepSpeed();
                        self.anim_t -= advance_step_size;
                        if (self.anim_t < 0) {
                            self.anim_t += 1;
                            self.done_steps -= 1;
                            self.result = null;
                            self.thread = x;
                            asdf.* = null;
                        }
                    } else {
                        const advance_step_size = delta_seconds * SKIP_SPEED_MULT * BASE_SPEED * self.curStepSpeed();
                        self.anim_t -= advance_step_size;
                        if (self.anim_t <= 0.0) {
                            self.anim_t = @max(0.0, self.anim_t);
                            self.anim_state = .paused;
                        }
                    }
                },
            }

            while (self.anim_t >= 1 and self.result == null) {
                self.anim_t -= 1;
                self.done_steps += 1;
                self.result = try self.thread.advanceTinyStep(self.scoring_run);
            }
            return .nothing;

            // if (platform.getMouse().wasPressed(.left)) {
            //     return try self.thread.advanceTinyStep(self.scoring_run);
            // } else {
            //     return null;
            // }
        }

        fn curStepSpeed(self: Self) f32 {
            return stepSpeed(self.anim_t, self.thread.last_visual_state, self.thread.stack.items.len, self.result_ui_point_for_test != null);
        }

        fn stepSpeed(anim_t: f32, state: core.ExecutionThread.VisualState, execution_stack_count: usize, is_test: bool) f32 {
            return switch (state) {
                .just_started => @panic("TODO"),
                .ended => if (is_test) lerp(1.2, 0.8, anim_t) else lerp(2, 4, anim_t),
                else => 1,
                .matched => |matched| if (matched.added_new_fnk_to_stack and anim_t > 0.5)
                    lerp(0.5, 0.8, math.smoothstep(anim_t, 0.7, 0.9))
                else if (matched.tail_optimized and execution_stack_count > 0 and anim_t > 0.5)
                    lerp(0.5, 0.8, math.smoothstep(anim_t, 0.7, 0.9))
                else
                    1,
            };
        }

        pub fn draw(self: Self) !void {
            if (self.result) |result| {
                if (self.anim_t >= 1) {
                    const camera = DEFAULT_CAM;

                    switch (result) {
                        .result => |value| {
                            if (self.result_ui_point_for_test == null) {
                                drawer.drawDebugText(camera, text_pos, "Result:", .black);
                                try artist.drawSexpr(camera, result_pos, value);
                            }
                        },
                        .no_matching_case => drawer.drawDebugText(camera, text_pos, "Ran out of cases!", .black),
                        .missing_or_uncompilable_fnk => |fnk_name| {
                            drawer.drawDebugText(camera, text_pos, "Could not find\nor compile this vau:", .black);
                            try artist.drawSexpr(camera, bad_fnk_pos, fnk_name);
                        },
                        .used_undefined_variable => |asdf| {
                            drawer.drawDebugText(camera, text_pos, "Could not fill in\nall Wildcards:", .black);
                            try artist.drawSexpr(camera, result_pos, asdf.template);
                        },
                    }

                    self.ui_state.draw(drawer);
                    return;
                }
            }

            const camera = self.camera;
            var parent_point = Point{};

            // TODO: take the is_pattern into account
            const input_pos = if (DESIGN.no_current_data) self.main_input.pos else MAIN_INPUT_POS;

            // std.log.debug("cur state: {s}", .{@tagName(self.thread.last_visual_state)});
            var it = std.mem.reverseIterator(self.thread.stack.items);
            switch (self.thread.last_visual_state) {
                .just_started => {
                    // const active_stack: core.StackThing = it.next().?;
                    // try artist.drawSexpr(camera, parent_point.applyToLocalPoint(MAIN_INPUT_POS), self.thread.active_value);
                    // try artist.drawSexpr(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), active_stack.cur_fnk_name);
                    // try drawCases(true, parent_point, active_stack.cur_cases, true);
                },
                .failed_to_match => |discarded_case| {
                    const active_stack: core.StackThing = it.next().?;
                    artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                    // TODO: is the parent_point.apply required here?
                    try artist.drawSexpr(camera, parent_point.applyToLocalPoint(input_pos), self.thread.active_value);
                    try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, active_stack.cur_fnk_name);
                    if (self.anim_t < 0.5) {
                        try drawCasesAsdf(
                            camera,
                            self.anim_t,
                            parent_point,
                            discarded_case,
                            active_stack.cur_cases,
                            .{ .anim_t = null, .new = &.{}, .old = active_stack.cur_bindings.items },
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);
                        const pattern_point_floating_away = parent_point.applyToLocalPoint(Point.lerp(
                            .{ .pos = .new(4, 0) },
                            .{ .pos = .new(12, -4), .scale = 0, .turns = -0.65 },
                            t,
                        ));
                        try drawCase(camera, discarded_case, pattern_point_floating_away
                            .applyToLocalPoint(.{ .pos = .new(-4, 1) }), .{
                            .anim_t = null,
                            .new = &.{},
                            .old = active_stack.cur_bindings.items,
                        }, .{
                            .is_gen0 = 1,
                            .hiding_children = 0,
                            .matching = 1,
                            .unfolded = 1,
                            .with_extra = true,
                        });
                        if (active_stack.cur_cases.len > 0) {
                            try drawCases(
                                camera,
                                1,
                                parent_point,
                                active_stack.cur_cases,
                                .{ .unfolding = t },
                                0,
                                .{ .anim_t = null, .new = &.{}, .old = active_stack.cur_bindings.items },
                            );
                        }
                    }
                },
                .matched => |matched| {
                    if (self.anim_t < 0.5) {
                        if (matched.added_new_fnk_to_stack) {
                            _ = it.next().?;
                        }
                        if (!matched.tail_optimized) {
                            _ = it.next().?;
                        }

                        artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                        try artist.drawSexpr(
                            camera,
                            parent_point.applyToLocalPoint(input_pos),
                            matched.old_active_value,
                        );
                        try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, matched.old_fnk_name);

                        try drawCasesAsdf(
                            camera,
                            self.anim_t,
                            parent_point,
                            matched.case,
                            matched.discarded_cases,
                            .{ .anim_t = null, .new = matched.new_bindings, .old = matched.old_bindings },
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);

                        try drawCases(
                            camera,
                            1,
                            parent_point.applyToLocalPoint(.lerp(
                                .{},
                                .{ .turns = 0.2, .scale = 0, .pos = .new(-4, 8) },
                                clamp01(remap(t, 0, 0.8, 0, 1)),
                            )),
                            matched.discarded_cases,
                            .{ .offset = 0.5 },
                            0,
                            .{ .anim_t = null, .new = matched.new_bindings, .old = matched.old_bindings },
                        );

                        // TODO: draw centered
                        const t2 = clamp01(remap(self.anim_t, 0.5, 0.8, 0, 1));
                        const hiding_children_t = math.smoothstep(self.anim_t, 0.5, 0.65);
                        const dissolving_pattern_point = parent_point
                            .applyToLocalPoint(MAIN_INPUT_POS)
                            .applyToLocalPoint(.{ .scale = 1 - t2 });
                        const cable_asdf_pos = dissolving_pattern_point.applyToLocalPosition(.new(3.5, 0));
                        drawer.drawCable(
                            camera,
                            .new(-CABLE_OFFSCREEN_DIST, 0),
                            dissolving_pattern_point.applyToLocalPosition(.new(-0.5, 0)),
                            1,
                            t2 * 3,
                        );
                        try artist.drawSexpr(
                            camera,
                            dissolving_pattern_point,
                            matched.old_active_value,
                        );
                        try artist.drawPatternSexpr(
                            camera,
                            dissolving_pattern_point
                                .applyToLocalPoint(.{ .pos = .new(3, 0) }),
                            matched.case.pattern,
                        );
                        drawer.drawCable(
                            camera,
                            dissolving_pattern_point.applyToLocalPosition(.new(-1, 1)),
                            dissolving_pattern_point.applyToLocalPosition(.new(3, 1)),
                            dissolving_pattern_point.scale,
                            0,
                        );

                        const active_value_cur_pos = parent_point.applyToLocalPoint(Point.lerp(
                            .{ .pos = .new(5 + DIST_TO_TEMPLATE - 1, 0) },
                            MAIN_INPUT_POS,
                            t,
                        ));
                        artist.drawCableTo(camera, cable_asdf_pos, active_value_cur_pos);
                        if (t < 0.25) {
                            // TODO NOW
                            var names: std.ArrayList([]const u8) = .init(platform.gpa);
                            defer names.deinit();
                            try matched.case.template.getAllVarNames(&names);
                            try removeBoundNamesV3(&names, .{
                                .anim_t = t,
                                .new = matched.new_bindings,
                                .old = matched.old_bindings,
                            });
                            try artist.drawWildcardsCable(camera, &.{
                                cable_asdf_pos,
                                active_value_cur_pos.applyToLocalPosition(.new(-0.5, 0)),
                            }, names.items);
                            const outgoing = (try getWildcards(matched.case, .none)).outgoing;
                            for (matched.new_bindings) |binding| {
                                if (funk.indexOfString(names.items, binding.name) != null) {
                                    try artist.drawSexpr(camera, .{ .pos = .lerp(
                                        cable_asdf_pos,
                                        active_value_cur_pos.applyToLocalPosition(.new(-0.5, 0)),
                                        t * 4,
                                    ), .scale = 0.25 }, binding.value);
                                }

                                if (funk.indexOfString(outgoing, binding.name) != null) {
                                    try artist.drawSexpr(camera, .{
                                        .pos = cable_asdf_pos.add(.new(
                                            if (!matched.added_new_fnk_to_stack)
                                                0
                                            else
                                                lerp(0, -1 - DIST_BETWEEN_QUEUED_FNKS, t2),
                                            t * 10,
                                        )),
                                        .scale = 0.25,
                                    }, binding.value);
                                }
                            }
                        }
                        try artist.drawSexprWithBindings(
                            camera,
                            active_value_cur_pos,
                            matched.case.template,
                            .{ .anim_t = t, .new = matched.new_bindings, .old = matched.old_bindings },
                        );

                        if (matched.added_new_fnk_to_stack) {
                            const active_stack: core.StackThing = it.next().?;
                            try artist.drawHoldedFnk(camera, parent_point
                                .applyToLocalPoint(Point.lerp(
                                (Point{ .pos = .new(DIST_TO_TEMPLATE - 1, 0) })
                                    .applyToLocalPoint(FNK_NAME_OFFSET),
                                MAIN_FNK_POS,
                                t,
                            )), t, active_stack.cur_fnk_name);
                            const t3 = math.smoothstep(t, 0, 0.25);
                            try drawCases(
                                camera,
                                1,
                                parent_point.applyToLocalPoint(.{
                                    // TODO: improve this anim
                                    .pos = .new(
                                        lerp(
                                            DIST_TO_TEMPLATE * 3,
                                            lerp(2 * (DIST_TO_TEMPLATE - 1), 0, t),
                                            t3,
                                        ),
                                        lerp(7, 0, t3),
                                    ),
                                }),
                                active_stack.cur_cases,
                                .{ .unfolding = 1 },
                                0,
                                .{ .anim_t = null, .new = &.{}, .old = active_stack.cur_bindings.items },
                            );
                        }

                        // TODO: join the 3 prev_stack cases in 1, maybe
                        if (matched.tail_optimized) {
                            try artist.drawHoldedFnk(
                                camera,
                                parent_point
                                    .applyToLocalPoint(Point.lerp(
                                    MAIN_FNK_POS,
                                    .{ .pos = .new(4, -8), .scale = 0, .turns = -0.65 },
                                    t,
                                )),
                                1,
                                matched.old_fnk_name,
                            );

                            // THINKING
                            // const maybe_prev_stack = it.next();
                            // const how_unfolded_is_prev_stack: f32 = if (matched.tail_optimized and !matched.added_new_fnk_to_stack)
                            //     t
                            // else if (!matched.tail_optimized and matched.added_new_fnk_to_stack)
                            //     1 - t2
                            // else if (!matched.tail_optimized and !matched.added_new_fnk_to_stack)
                            //     0.0
                            // else blk: {
                            //     // this case is just nothing
                            //     std.log.debug("hhmmm", .{});
                            //     break :blk 0.0;
                            // };

                            if (!matched.added_new_fnk_to_stack) {
                                defer parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(lerp(-DIST_BETWEEN_QUEUED_FNKS, 0, t), 0) });

                                if (it.next()) |prev_stack| {
                                    try artist.drawHoldedFnk(
                                        camera,
                                        parent_point
                                            .applyToLocalPoint(.{ .pos = .new(lerp(-DIST_BETWEEN_QUEUED_FNKS, 0, t), 0) })
                                            .applyToLocalPoint(MAIN_FNK_POS),
                                        1,
                                        prev_stack.cur_fnk_name,
                                    );
                                    try drawCases(
                                        camera,
                                        t,
                                        parent_point.applyToLocalPoint(.{
                                            .pos = .new(lerp(-1 - DIST_BETWEEN_QUEUED_FNKS, 0, t), 0),
                                        }),
                                        prev_stack.cur_cases,
                                        .{ .unfolding = 1 },
                                        1 - hiding_children_t,
                                        .{ .anim_t = t, .new = &.{}, .old = prev_stack.cur_bindings.items },
                                        // .{ .anim_t = t, .new = matched.new_bindings, .old = matched.old_bindings },
                                    );
                                }
                            }
                        } else {
                            const prev_stack: core.StackThing = it.next().?;
                            if (matched.added_new_fnk_to_stack) {
                                defer parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(-DIST_BETWEEN_QUEUED_FNKS * t2, 0) });

                                try artist.drawHoldedFnk(
                                    camera,
                                    parent_point
                                        .applyToLocalPoint(.{ .pos = .new(lerp(0, -DIST_BETWEEN_QUEUED_FNKS, t2), 0) })
                                        .applyToLocalPoint(MAIN_FNK_POS),
                                    1,
                                    prev_stack.cur_fnk_name,
                                );
                                // TODO: revise this (waiting cases should be gen0?)
                                try drawCases(
                                    camera,
                                    0,
                                    parent_point.applyToLocalPoint(.{
                                        .pos = .new(lerp(DIST_TO_TEMPLATE - 1, -1 - DIST_BETWEEN_QUEUED_FNKS, t2), 0),
                                    }),
                                    prev_stack.cur_cases,
                                    .{ .unfolding = 1 },
                                    hiding_children_t,
                                    // TODO: revise, might be prev_stack.cur_bindings.items
                                    .{ .anim_t = t, .new = matched.new_bindings, .old = matched.old_bindings },
                                );
                            } else {
                                try artist.drawHoldedFnk(
                                    camera,
                                    parent_point
                                        .applyToLocalPoint(MAIN_FNK_POS),
                                    1,
                                    prev_stack.cur_fnk_name,
                                );

                                try drawCases(
                                    camera,
                                    t,
                                    parent_point
                                        .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE - 1, 0, t), 0) }),
                                    prev_stack.cur_cases,
                                    .{ .unfolding = 1 },
                                    0,
                                    .{ .anim_t = t, .new = matched.new_bindings, .old = matched.old_bindings },
                                );
                            }
                        }
                    }
                },
                .ran_out_of_cases => {
                    parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(-self.anim_t * 12, 0) });
                    const active_stack: core.StackThing = it.next().?;
                    artist.drawOffscreenCableTo(camera, parent_point.applyToLocalPoint(MAIN_INPUT_POS));
                    try artist.drawSexpr(camera, parent_point.applyToLocalPoint(input_pos), self.thread.active_value);
                    try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, active_stack.cur_fnk_name);
                },
                .failed_to_find_or_compile_fnk => |asdf| {
                    // TODO: revise
                    // const active_stack: core.StackThing = it.next().?;
                    if (self.anim_t < 0.5) {
                        artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                        try artist.drawSexpr(
                            camera,
                            parent_point.applyToLocalPoint(MAIN_INPUT_POS),
                            asdf.old_active_value,
                        );
                        try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, asdf.old_fnk_name);

                        try drawCasesAsdf(
                            camera,
                            self.anim_t,
                            parent_point,
                            asdf.case,
                            &.{}, // TODO: also have discarded_cases
                            // TODO NOW
                            .{ .anim_t = null, .new = &.{}, .old = undefined },
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);
                        const cam = Camera.lerp(camera, DEFAULT_CAM, t);
                        const p = Point.lerp(parent_point
                            .applyToLocalPoint(.{ .pos = .new(4, 0) })
                            .applyToLocalPoint(FNK_NAME_OFFSET), bad_fnk_pos, t);
                        try artist.drawSexpr(cam, p, asdf.case.fnk_name);
                    }
                },
                .undefined_variable => |asdf| {
                    // TODO: revisit
                    _ = it.next();
                    if (self.anim_t < 0.5) {
                        artist.drawOffscreenCableTo(camera, MAIN_INPUT_POS);
                        try artist.drawSexpr(
                            camera,
                            parent_point.applyToLocalPoint(MAIN_INPUT_POS),
                            asdf.old_active_value,
                        );
                        try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, asdf.old_fnk_name);

                        try drawCasesAsdf(
                            camera,
                            self.anim_t,
                            parent_point,
                            asdf.case,
                            &.{}, // TODO: also have discarded_cases
                            // TODO NOW
                            .{ .anim_t = null, .new = &.{}, .old = undefined },
                        );
                    } else {
                        const t = remap(self.anim_t, 0.5, 1, 0, 1);
                        const cam = Camera.lerp(camera, DEFAULT_CAM, t);
                        const p = Point.lerp(parent_point
                            .applyToLocalPoint(.{ .pos = .new(4, 0) })
                            .applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }), result_pos, t);
                        try artist.drawSexprWithBindings(cam, p, asdf.case.template, .{
                            .new = asdf.new_bindings,
                            .old = asdf.old_bindings,
                            .anim_t = t,
                        });
                    }
                },
                .ended => |result| {
                    if (self.result_ui_point_for_test) |ui_point_for_test| {
                        const cam = Camera.lerp(camera, UI.cam, self.anim_t);
                        const p = Point.lerp(MAIN_INPUT_POS, ui_point_for_test, self.anim_t);
                        artist.drawOffscreenCableTo(cam, p);
                        try artist.drawSexpr(cam, p, result);
                    } else {
                        const cam = Camera.lerp(camera, DEFAULT_CAM, self.anim_t);
                        const p = Point.lerp(MAIN_INPUT_POS, self.result_ui_point_for_test orelse result_pos, self.anim_t);
                        artist.drawOffscreenCableTo(cam, p);
                        try artist.drawSexpr(cam, p, result);
                    }
                },
            }
            while (it.next()) |x| {
                parent_point = parent_point.applyToLocalPoint(.{ .pos = .new(-DIST_BETWEEN_QUEUED_FNKS, 0) });
                try artist.drawHoldedFnk(camera, parent_point.applyToLocalPoint(MAIN_FNK_POS), 1, x.cur_fnk_name);

                try drawCases(
                    camera,
                    0,
                    parent_point.applyToLocalPoint(.{ .pos = .new(-1, 0) }),
                    x.cur_cases,
                    .{ .unfolding = 1 },
                    1,
                    .{ .anim_t = null, .new = &.{}, .old = x.cur_bindings.items },
                );
            }

            self.ui_state.draw(drawer);
        }

        fn drawCasesAsdf(
            camera: Camera,
            t_0_05: f32,
            parent_point: Point,
            first_case: core.MatchCaseDefinition,
            rest_cases: []const core.MatchCaseDefinition,
            bindings: BindingsState,
        ) OoM!void {
            const t1 = clamp01(remap(t_0_05, 0, 0.4, 0, 1));
            const t2 = clamp01(remap(t_0_05, 0.4, 0.5, 0, 1));
            try drawCases(
                camera,
                1,
                parent_point,
                rest_cases,
                .{ .offset = 0.5 + lerp(1.5, 0, t1) },
                0,
                bindings,
            );
            try drawCase(
                camera,
                first_case,
                parent_point
                    .applyToLocalPoint(.{ .pos = .new(0, 1 + lerp(3, 0, t1)) }),
                bindings,
                .{
                    .is_gen0 = 1,
                    .with_extra = true,
                    .hiding_children = 0,
                    .matching = t2,
                    .unfolded = 1,
                },
            );
            if (rest_cases.len == 0) {
                drawer.drawLine(camera, &.{
                    parent_point.applyToLocalPosition(.zero),
                    parent_point.applyToLocalPosition(.new(0, 1 + lerp(3, 0, t1))),
                }, .black);
            }
        }

        fn getRelativePatternPointAsdf(
            is_gen0: f32,
            first_state: StateOfFirst,
            hiding_children: f32,
            k: usize,
        ) Point {
            return switch (first_state) {
                .offset => |offset| Point{
                    .pos = .new(lerp(4, 5, is_gen0) - hiding_children, offset + 3.5 + tof32(k) * 1.5),
                    .scale = 0.5,
                },
                .unfolding => |unfolded| if (k == 0)
                    Point{
                        .pos = .new(lerp(4, 5, is_gen0) - hiding_children, lerp(3.5, 3, unfolded)),
                        .scale = lerp(0.5, 1, unfolded),
                    }
                else
                    Point{
                        .pos = .new(lerp(4, 5, is_gen0) - hiding_children, 3.5 + tof32(k) * 1.5),
                        .scale = 0.5,
                    },
            };
        }

        const StateOfFirst = union(enum) {
            unfolding: f32,
            offset: f32,
            pub fn unfolded(self: @This(), k: usize) f32 {
                return switch (self) {
                    .offset => 0,
                    .unfolding => |v| if (k == 0) v else 0,
                };
            }
            pub fn offseting(self: @This()) f32 {
                return switch (self) {
                    .offset => |v| v,
                    .unfolding => 0.5,
                };
            }
        };
        // TODO: remove this duplication from EditingFnk
        fn drawCases(
            camera: Camera,
            is_gen0: f32,
            parent_point: Point,
            cases: []const core.MatchCaseDefinition,
            first_state: StateOfFirst,
            hiding_children: f32,
            bindings: BindingsState,
        ) OoM!void {
            for (cases, 0..) |case, k| {
                // const relative_pattern_point = getRelativePatternPointAsdf(is_gen0, first_state, hiding_children, k);
                // const pattern_point = parent_point.applyToLocalPoint(relative_pattern_point);

                try drawCase(
                    camera,
                    case,
                    parent_point
                        .applyToLocalPoint(.{ .pos = .new(1 - is_gen0, first_state.offseting() + 3.5 + tof32(k) * 1.5) }),
                    bindings,
                    .{
                        .is_gen0 = is_gen0,
                        .with_extra = std.meta.activeTag(first_state) == .unfolding and k == 0,
                        .hiding_children = hiding_children,
                        .unfolded = first_state.unfolded(k),
                        .matching = 0,
                    },
                );
            }

            var prev_point = parent_point.applyToLocalPosition(.new(1 - is_gen0, 0));
            for (0..cases.len) |k| {
                const cur_point = parent_point
                    .applyToLocalPosition(.new(1 - is_gen0, first_state.offseting() + 3.5 + tof32(k) * 1.5));
                // .applyToLocalPoint(getRelativePatternPointAsdf(is_gen0, first_state, hiding_children, k))
                // .applyToLocalPosition(.new(0, 1))
                // .sub(.new(parent_point.scale * lerp(3, 5, is_gen0) - hiding_children, 0));
                defer prev_point = cur_point;
                drawer.drawLine(camera, &.{ prev_point, cur_point }, .black);

                var asdf: std.ArrayList([]const u8) = .init(platform.gpa);
                defer asdf.deinit();
                for (cases[k..]) |next_case| {
                    const incoming_wildcards = (try getWildcards(next_case, bindings)).incoming;
                    defer platform.gpa.free(incoming_wildcards);
                    try appendUniqueNames(&asdf, incoming_wildcards);
                }
                try artist.drawWildcardsCable(camera, &.{ prev_point, cur_point }, asdf.items);
            }
        }

        fn getWildcards(case: core.MatchCaseDefinition, bindings: BindingsState) !struct {
            incoming: []const []const u8,
            outgoing: []const []const u8,
        } {
            // TODO: leak
            var physical = try EditingFnk(platform, drawer).makeCasePhysical(platform.gpa, case, .{});
            _ = try physical.updateWildcards(platform.gpa);

            return .{
                // TODO: leak
                .incoming = try removeBoundNamesV2(platform.gpa, physical.incoming_wildcards, bindings),
                .outgoing = try removeBoundNamesV2(platform.gpa, physical.outgoing_wildcards, bindings),
            };
        }

        // TODO: drawCases(first, rest)

        fn drawCase(
            camera: Camera,
            case: core.MatchCaseDefinition,
            attachment_point: Point,
            bindings: BindingsState,
            matices: struct {
                // TODO: some of these are redundant
                unfolded: f32,
                is_gen0: f32,
                with_extra: bool,
                hiding_children: f32,
                matching: f32,
            },
        ) OoM!void {
            // const pattern_point = pattern_point_raw.applyToLocalPoint(.{ .pos = .new(lerp(0, -0.5, hiding_children), 0) });

            // .applyToLocalPoint(.{ .pos = .new(lerp(DIST_TO_TEMPLATE, 4, t2), lerp(3, 0, t)) }),
            // TODO ASDF
            const pattern_point = attachment_point.applyToLocalPoint(.{ .pos = .new(
                lerp(3, 5, matices.is_gen0) - matices.hiding_children - matices.matching,
                -lerp(0.5, 1, matices.unfolded),
            ), .scale = lerp(0.5, 1, matices.unfolded) });
            try artist.drawPatternSexpr(
                camera,
                pattern_point,
                case.pattern,
            );

            const cable_from = attachment_point.pos;
            const cable_to = pattern_point.applyToLocalPosition(.new(0, 1));
            drawer.drawCable(
                camera,
                cable_from,
                cable_to,
                pattern_point.scale,
                0,
            );

            if (matices.with_extra) {
                try drawCaseExtra(camera, pattern_point, case, bindings, matices.hiding_children);
            }
        }

        // TODO: remove duplication with EditingCase
        fn drawCaseExtra(
            camera: Camera,
            pattern_point_raw: Point,
            case: core.MatchCaseDefinition,
            bindings: BindingsState,
            hiding_children: f32,
        ) !void {
            const pattern_point = pattern_point_raw.applyToLocalPoint(.{ .scale = 1 - hiding_children });
            try artist.drawSexprWithBindings(
                camera,
                pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                case.template,
                bindings,
            );
            try artist.drawHoldedFnk(camera, pattern_point.applyToLocalPoint(FNK_NAME_OFFSET), 0, case.fnk_name);
            drawer.drawCable(
                camera,
                pattern_point.applyToLocalPosition(.new(0.5, 0)),
                pattern_point.applyToLocalPosition(.new(DIST_TO_TEMPLATE - 0.5, 0)),
                pattern_point.scale,
                0,
            );
            // TODO: draw the bound values travelling on the wire
            // TODO NOW: don't draw the cables for already bound wildcards
            const asdf = try getWildcards(case, bindings);
            try artist.drawPlacedWildcardsCable(
                camera,
                pattern_point_raw,
                pattern_point.applyToLocalPoint(.{ .pos = .new(DIST_TO_TEMPLATE, 0) }),
                case.pattern,
                case.template,
                null,
                asdf.incoming,
                asdf.outgoing,
                bindings,
                hiding_children,
            );
            if (case.next) |next| {
                try drawCases(camera, 0, pattern_point, next.items, .{ .unfolding = 1 }, 0, bindings);
                // , if (hiding_children == 0) .normal else .floating
            }
        }

        fn drawExpected(expected: *const Sexpr) !void {
            const camera = DEFAULT_CAM;
            drawer.drawDebugText(camera, expected_output_pos.applyToLocalPoint(.{ .pos = .new(1, -2) }), "Expected\nresult:", .black);
            try artist.drawSexpr(camera, expected_output_pos, expected);
        }
    };
}

test {
    const dummy_platform = Platform{
        .gpa = std.testing.allocator,
        .getMouse = struct {
            pub fn anon() Mouse {
                unreachable;
            }
        }.anon,
        .getKeyboard = struct {
            pub fn anon() Keyboard {
                unreachable;
            }
        }.anon,
        .getPlayerData = undefined,
        .setPlayerData = undefined,
        .setCursor = struct {
            pub fn anon(_: Platform.Cursor) void {
                unreachable;
            }
        }.anon,
    };
    std.testing.refAllDecls(EditingFnk(dummy_platform, Drawer.dummy));
    std.testing.refAllDecls(ExecutingFnk(dummy_platform, Drawer.dummy));
}

const UI = struct {
    const cam = Camera.fromTopleftAndHeight(Vec2.zero, 15);

    pub const State = struct {
        hot: ?usize = null,
        active: ?usize = null,
        buttons: []Button,

        pub fn isHot(self: State, k: usize) bool {
            return if (self.hot) |hot| hot == k else false;
        }

        pub fn isActive(self: State, k: usize) bool {
            return if (self.active) |active| active == k else false;
        }

        pub fn update(self: *State, mouse: Mouse, delta_seconds: f32) ?usize {
            self.hot = null;
            var result: ?usize = null;

            for (self.buttons, 0..) |button, k| {
                if (!button.enabled) continue;

                if (button.pos.contains(mouse.cur.pos(UI.cam))) {
                    self.hot = k;
                    if (self.active == null and mouse.cur.isDown(.left)) {
                        self.active = k;
                    }
                }

                if (self.isActive(k) and self.isHot(k) and !mouse.cur.isDown(.left)) {
                    result = k;
                }
            }
            if (!mouse.cur.isDown(.left)) {
                if (self.active) |_| {
                    self.active = null;
                }
            } else if (self.active == null) {
                // TODO: better
                self.active = self.buttons.len + 999;
            }
            for (self.buttons, 0..) |*button, k| {
                math.lerp_towards(
                    &button.hot_t,
                    if (self.isHot(k)) 1 else 0,
                    0.6,
                    delta_seconds,
                );
                math.lerp_towards(
                    &button.active_t,
                    if (self.isActive(k)) 1 else 0,
                    0.6,
                    delta_seconds,
                );
            }
            return result;
        }

        pub fn draw(self: State, comptime drawer: Drawer) void {
            for (self.buttons) |button| {
                if (!button.enabled) drawer.setTransparency(0.5);
                defer if (!button.enabled) drawer.setTransparency(1);

                drawer.drawRect(UI.cam, button.pos.plusMargin(
                    clamp01(button.hot_t - button.active_t) * 0.1,
                ), .black, .white);
                if (button.text) |text| {
                    drawer.drawDebugText(UI.cam, .{
                        .pos = button.pos.getCenter(),
                        .scale = button.pos.size.y / (1.5 + tof32(std.mem.count(u8, text, "\n"))),
                    }, text, Color.black);
                }
            }
        }
    };

    pub const Button = struct {
        pos: Rect,
        hot_t: f32 = 0,
        active_t: f32 = 0,
        text: ?[:0]const u8 = null,
        enabled: bool = true,

        pub fn row(alloc: std.mem.Allocator, top_left: Vec2, size: Vec2, texts: []const ?[:0]const u8) ![]Button {
            const result: []Button = try alloc.alloc(Button, texts.len);
            for (texts, result, 0..) |text, *target, k| {
                target.* = .{ .pos = Rect{ .top_left = top_left.addX(size.x * tof32(k)), .size = size }, .text = text };
            }
            return result;
        }

        pub fn rowWithExtra(alloc: std.mem.Allocator, top_left: Vec2, size: Vec2, texts: []const ?[:0]const u8, extra: []const Button) ![]Button {
            const result: []Button = try alloc.alloc(Button, texts.len + extra.len);
            for (texts, result[0..texts.len], 0..) |text, *target, k| {
                target.* = .{ .pos = Rect{ .top_left = top_left.addX(size.x * tof32(k)), .size = size }, .text = text };
            }
            @memcpy(result[texts.len..], extra);
            return result;
        }
    };
};

pub fn LevelSelect(platform: Platform, drawer: Drawer) type {
    const artist = Artist(platform, drawer);
    return struct {
        const Self = @This();

        level_select_buttons: UI.State,
        load_save_buttons: UI.State,
        play_level_button: UI.State,
        selected_level: ?usize = null,
        persistence: *const PlayerData,

        pub fn init(persistence: *const PlayerData) !Self {
            const res = platform.gpa.alloc(UI.Button, builtin_levels.len) catch unreachable;
            for (res, 0..) |*b, k| {
                b.* = .{
                    .pos = Rect{ .top_left = .new(2 + 2.5 * tof32(@divFloor(k, 5)), 2.5 + 2.5 * @as(f32, @floatFromInt(@mod(k, 5)))), .size = .one },
                    // locked levels
                    .enabled = if (DESIGN.all_levels_unlocked) true else if (k == 0) true else persistence.is_builtin_level_solved[k - 1],
                };
            }
            return Self{
                .level_select_buttons = .{ .buttons = res },
                .play_level_button = .{ .buttons = try platform.gpa.dupe(UI.Button, &.{
                    .{ .pos = Rect{ .top_left = .new(10, 10), .size = .new(2, 1) }, .text = "Play" },
                }) },
                .load_save_buttons = .{ .buttons = try platform.gpa.dupe(UI.Button, &.{
                    .{ .pos = Rect{ .top_left = .new(15.0 * 16.0 / 9.0 - 2, 0), .size = .new(1, 1) }, .text = "Save" },
                    .{ .pos = Rect{ .top_left = .new(15.0 * 16.0 / 9.0 - 1, 0), .size = .new(1, 1) }, .text = "Load" },
                }) },
                .persistence = persistence,
            };
        }

        pub fn update(self: *Self, delta_seconds: f32) union(enum) { nothing, selected: usize, uploading } {
            const mouse = platform.getMouse();

            if (self.load_save_buttons.update(mouse, delta_seconds)) |pressed| switch (pressed) {
                0 => platform.downloadPlayerData(self.persistence.*, platform.gpa) catch @panic("OoM"),
                1 => return .uploading,
                else => unreachable,
            };

            if (self.level_select_buttons.update(mouse, delta_seconds)) |pressed| {
                self.selected_level = pressed;
            }
            if (self.selected_level) |selected| {
                if (self.play_level_button.update(mouse, delta_seconds) != null) {
                    return .{ .selected = selected };
                }
            }

            // assumes that changing cursor is free
            platform.setCursor(if (self.level_select_buttons.active != null)
                .grabbing
            else if (self.level_select_buttons.hot != null)
                .could_grab
            else
                .default);

            return .nothing;
        }

        pub fn draw(self: Self) OoM!void {
            self.load_save_buttons.draw(drawer);

            for (self.level_select_buttons.buttons, 0..) |button, k| {
                if (!button.enabled) drawer.setTransparency(0.5);
                defer if (!button.enabled) drawer.setTransparency(1);

                if (self.persistence.is_builtin_level_solved[k]) {
                    drawer.drawRect(UI.cam, button.pos.plusMargin(0.2), Color.fromHex("#55ff55"), null);
                }
                if (k == self.selected_level) {
                    drawer.drawRect(UI.cam, button.pos.plusMargin(0.4), .black, null);
                } else if (button.hot_t > 0) {
                    drawer.drawRect(UI.cam, button.pos.plusMargin(button.hot_t - 0.5 - 0.1 * button.active_t), .black, null);
                }
                try artist.drawSexpr(UI.cam, .{
                    .pos = button.pos.top_left.add(.new(0.5, 1)),
                    .turns = -0.25,
                    .scale = 0.5,
                }, builtin_levels[k].fnk_name);
            }

            if (self.selected_level) |selected| {
                const level = builtin_levels[selected];
                drawer.drawDebugText(UI.cam, .{ .pos = UI.cam.center.addX(2) }, level.description, .black);
                self.play_level_button.draw(drawer);
            } else {
                drawer.drawDebugText(
                    UI.cam,
                    .{ .pos = UI.cam.center.addX(2) },
                    if (!self.persistence.is_builtin_level_solved[0])
                        \\Welcome to the Vaulogy lab! 
                        \\Vaus are simple machines for transforming Data into Data.
                        \\Your goal is to fill in all these Vaus.
                    else if (!self.persistence.is_builtin_level_solved[1])
                        \\Good job! On to the next one...
                    else if (!self.persistence.is_builtin_level_solved[2])
                        \\Vaus can be combined. For the next one,
                        \\you will reuse the first one.
                    else if (!self.persistence.is_builtin_level_solved[3])
                        \\You can also call a Vau on the result of another one
                    else
                        "",
                    .black,
                );
            }
        }

        pub fn getLevelButtonPoint(self: Self, level_index: usize) Point {
            return .{
                .pos = self.level_select_buttons.buttons[level_index].pos.top_left.add(.new(0.5, 1)),
                .turns = -0.25,
                .scale = 0.5,
            };
        }
    };
}

pub fn IntroSequence(platform: Platform, drawer: Drawer) type {
    _ = platform;
    return struct {
        const Self = @This();

        t: f32,
        background_atoms: [40]struct {
            cur: Point,
            vel: Point,
        },

        const initial_camera = Camera{
            .center = .zero,
            .height = 50,
        };
        const second_camera = Camera{
            .center = .new(4, 0),
            .height = 12,
        };

        const snap = .{
            .pos = Point{ .pos = .new(3, 0) },
            // the velocity at the moment of snapping
            .vel = Point{
                .pos = .new(-1, -1),
                .turns = -0.1,
            },
        };

        pub fn init() Self {
            var background_atoms: @FieldType(Self, "background_atoms") = undefined;
            var rnd_state = std.Random.DefaultPrng.init(14);
            const rnd = Random{ .rnd = rnd_state.random() };

            for (&background_atoms) |*atom| {
                atom.* = .{ .cur = .{
                    .pos = rnd.inRect(initial_camera.toRect()),
                    .turns = rnd.rnd.floatNorm(f32) / 100.0,
                }, .vel = Point{
                    .pos = rnd.direction().scale(0.2),
                    .turns = rnd.around0(0.02),
                } };
            }
            // hack
            background_atoms[30] = .{ .cur = .{
                .pos = rnd.inRect(initial_camera.toRect()),
                .turns = rnd.rnd.floatNorm(f32) / 100.0,
            }, .vel = Point{
                .pos = rnd.direction().scale(0.2),
                .turns = rnd.around0(0.02),
            } };
            return .{ .t = 0, .background_atoms = background_atoms };
        }

        pub fn update(self: *Self, delta_seconds: f32) void {
            for (&self.background_atoms) |*atom| {
                atom.cur.pos = atom.cur.pos.add(atom.vel.pos.scale(delta_seconds));
                atom.cur.turns += delta_seconds * atom.vel.turns;
            }
            self.t += delta_seconds;
            // self.t = std.math.clamp(self.t, 0, 1);
        }

        pub fn draw(self: Self) void {
            const camera = Camera.lerp(
                initial_camera,
                second_camera,
                math.smoothstep(self.t, 2, 6),
            );
            for (self.background_atoms) |atom| {
                drawer.drawAtomDebug(camera, atom.cur);
            }

            if (self.t <= 8) {
                drawer.drawCable(camera, .new(-50, 0), .new(-0.5, 0), 1, 0);
                drawer.drawAtomDebug(
                    camera,
                    Point{ .pos = .zero, .scale = 1, .turns = 0 },
                );

                const cur = Point.lerp(
                    .{
                        .pos = snap.pos.pos.sub(snap.vel.pos).scale(8),
                        .turns = (snap.pos.turns - snap.vel.turns) * 8,
                    },
                    snap.pos,
                    clamp(self.t / 8, 0, 1),
                );
                drawer.drawPatternAtomDebug(camera, cur);
                drawer.drawAtomDebug(camera, cur.applyToLocalPoint(.{ .pos = .new(3.8, 0) }));
                drawer.drawCable(
                    camera,
                    cur.applyToLocalPosition(.new(0.5, 0)),
                    cur.applyToLocalPosition(.new(3.3, 0)),
                    1,
                    53.5,
                );
            } else {
                const pull = math.smoothstep(self.t, 8.3, 12) * 6.8;
                drawer.drawCable(camera, .new(-50, 0), .new(6.3 - pull, 0), 1, pull);
                drawer.drawAtomDebug(camera, snap.pos.applyToLocalPoint(.{ .pos = .new(3.8 - pull, 0) }));

                const cur = Point.lerp(
                    snap.pos,
                    .{
                        .pos = snap.pos.pos.sub(snap.vel.pos).add(.new(0, 3)).scale(-4),
                        .turns = (snap.pos.turns - snap.vel.turns) * -12,
                    },
                    clamp((self.t - 8) / 8, 0, 1),
                );
                drawer.drawAtomDebug(camera, cur.applyToLocalPoint(.{ .pos = .new(-3, 0) }));
                drawer.drawPatternAtomDebug(camera, cur);
            }
        }
    };
}

fn appendUniqueNames(list: *std.ArrayList([]const u8), names: []const []const u8) !void {
    for (names) |name| {
        if (funk.indexOfString(list.items, name) == null) {
            try list.append(name);
        }
    }
}

fn removeNames(list: *std.ArrayList([]const u8), names: []const []const u8) !void {
    for (names) |name_to_remove| {
        while (funk.indexOfString(list.items, name_to_remove)) |i| {
            std.debug.assert(std.mem.eql(u8, name_to_remove, list.swapRemove(i)));
        }
    }
}

fn removeBoundNames(list: *std.ArrayList([]const u8), bindings: []const core.Binding) !void {
    for (bindings) |binding| {
        const name_to_remove = binding.name;
        while (funk.indexOfString(list.items, name_to_remove)) |i| {
            std.debug.assert(std.mem.eql(u8, name_to_remove, list.swapRemove(i)));
        }
    }
}

// TODO: most callers of this function are causing leaks
fn removeBoundNamesV2(gpa: std.mem.Allocator, list: []const []const u8, bindings: BindingsState) ![]const []const u8 {
    var incoming: std.ArrayList([]const u8) = .init(gpa);
    try incoming.appendSlice(list);
    try removeBoundNamesV3(&incoming, bindings);
    return try incoming.toOwnedSlice();
}

fn removeBoundNamesV3(list: *std.ArrayList([]const u8), bindings: BindingsState) !void {
    try removeBoundNames(list, bindings.old);
    if (if (bindings.anim_t) |t| t > 0.4 else false) {
        try removeBoundNames(list, bindings.new);
    }
}

fn allTrue(arr: []const bool) bool {
    for (arr) |v| {
        if (!v) return false;
    } else return true;
}
