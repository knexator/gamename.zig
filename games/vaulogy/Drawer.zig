// FUTURE: would be nice if drawSexpr(is_pattern_t) gradually changed the geometry shape

pub const Drawer = @This();
pub const Gl = kommon.Gl;

canvas: *Canvas,
atom_visuals_cache: AtomVisualCache,

pub fn init(usual: *kommon.Usual) !Drawer {
    try AtomVisuals.Geometry.initFixed(usual.mem.forever.allocator(), usual.canvas.gl);
    return .{
        .canvas = &usual.canvas,
        .atom_visuals_cache = try .init(usual.mem.forever.allocator(), usual.canvas.gl),
    };
}

pub const AtomVisuals = struct {
    profile: []const Vec2,
    color: FColor,
    display: ?[]const u8 = null,
    geometry: Geometry,

    fn build(mem: std.mem.Allocator, params: struct {
        profile: []const Vec2,
        color: FColor,
        display: ?[]const u8 = null,
    }, gl: kommon.Gl) !AtomVisuals {
        const geo: Geometry = try .fromProfile(mem, params.profile, gl);
        return .{
            .profile = params.profile,
            .color = params.color,
            .display = params.display,
            .geometry = geo,
        };
    }

    pub const Geometry = struct {
        template: Canvas.PrecomputedShape,
        pattern: Canvas.PrecomputedShape,

        var template_placeholder: Canvas.PrecomputedShape = undefined;
        var pattern_placeholder: Canvas.PrecomputedShape = undefined;
        var template_variable: Canvas.PrecomputedShape = undefined;
        var pattern_variable: Canvas.PrecomputedShape = undefined;
        var template_pair_holder: Canvas.PrecomputedShape = undefined;
        var pattern_pair_holder: Canvas.PrecomputedShape = undefined;

        // other shapes
        pub var ridged_circle: Canvas.PrecomputedShape = undefined;

        pub fn fromProfile(mem: std.mem.Allocator, profile: []const Vec2, gl: kommon.Gl) !Geometry {
            const template: Canvas.PrecomputedShape = blk: {
                const skeleton_positions = [1]Vec2{.new(2, -1)} ++
                    funk.fromCount(32, struct {
                        pub fn anon(k: usize) Vec2 {
                            return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                        }
                    }.anon) ++ [1]Vec2{.new(2, 1)};
                var local_positions: []Vec2 = try mem.alloc(Vec2, skeleton_positions.len + profile.len * 2);
                for (skeleton_positions, 0..) |pos, i| {
                    local_positions[i] = pos;
                }
                for (profile, 0..) |pos, i| {
                    local_positions[skeleton_positions.len + i] = Vec2.new(2.0 - pos.y, 1.0 - pos.x);
                    local_positions[skeleton_positions.len + profile.len * 2 - i - 1] = Vec2.new(2.0 + pos.y, -1.0 + pos.x);
                }
                break :blk try .fromOwnedPoints(mem, local_positions, gl);
            };

            const pattern: Canvas.PrecomputedShape = blk: {
                const skeleton_positions =
                    [1]Vec2{.new(-1, -1)} ++
                    funk.fromCount(32, struct {
                        pub fn anon(k: usize) Vec2 {
                            return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                        }
                    }.anon) ++ [1]Vec2{.new(-1, 1)};
                var local_positions: []Vec2 = try mem.alloc(Vec2, skeleton_positions.len + profile.len * 2);
                for (skeleton_positions, 0..) |pos, i| {
                    local_positions[i] = pos;
                }
                for (profile, 0..) |pos, i| {
                    local_positions[skeleton_positions.len + i] = Vec2.new(-1.0 - pos.y, 1.0 - pos.x);
                    local_positions[skeleton_positions.len + profile.len * 2 - i - 1] = Vec2.new(-1.0 + pos.y, -1.0 + pos.x);
                }

                break :blk try .fromOwnedPoints(mem, local_positions, gl);
            };

            return .{
                .template = template,
                .pattern = pattern,
            };
        }

        pub fn initFixed(mem: std.mem.Allocator, gl: kommon.Gl) !void {
            Geometry.template_variable = try .fromPoints(mem, &(funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).mul(.new(0.5, 1)).addX(0.5);
                }
            }.anon)), gl);

            Geometry.pattern_variable = try .fromPoints(mem, &(funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, -0.25, math.tof32(k) / 32)).mul(.new(0.5, 1)).addX(-0.5);
                }
            }.anon)), gl);

            Geometry.template_pair_holder = try .fromPoints(mem, &(funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).scale(0.5).add(.new(0.75, 0.5));
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).scale(0.5).add(.new(0.75, -0.5));
                }
            }.anon)), gl);

            Geometry.pattern_pair_holder = try .fromPoints(mem, &(funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, -0.25, math.tof32(k) / 32)).scale(0.5).add(.new(-1.25, 0.5));
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, -0.25, math.tof32(k) / 32)).scale(0.5).add(.new(-1.25, -0.5));
                }
            }.anon)), gl);

            Geometry.template_placeholder = try .fromPoints(mem, &([1]Vec2{.new(2, -1)} ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ [1]Vec2{.new(2, 1)}), gl);
            Geometry.pattern_placeholder = try .fromPoints(mem, &([1]Vec2{.new(-1, -1)} ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                }
            }.anon) ++ [1]Vec2{.new(-1, 1)}), gl);

            Geometry.ridged_circle = try .fromPoints(mem, &(funk.map(struct {
                pub fn anon(t: f32) Vec2 {
                    return Vec2.fromPolar(@abs(0.1 * math.cos(t * 4)) + 1, t);
                }
            }.anon, &funk.linspace01(128, false))), gl);
        }
    };
};

const AtomVisualCache = struct {
    arena: std.mem.Allocator,
    visuals_cache: std.StringHashMap(AtomVisuals),

    const HardcodedAtomVisuals = struct {
        profile: ?[]const Vec2,
        color: ?FColor,
        display: ?[]const u8 = null,
    };
    const hardcoded_visuals = .{
        .identity = HardcodedAtomVisuals{
            .color = .white,
            .profile = &.{},
        },
        .nil = HardcodedAtomVisuals{
            .color = .new(0.45, 0.45, 0.45),
            .profile = &.{.new(0.75, -0.25)},
        },
        .input = HardcodedAtomVisuals{
            .color = .new(0.1, 0.6, 0.6),
            .profile = &.{ .new(0.2, 0.2), .new(0.8, 0.2) },
        },
        .true = HardcodedAtomVisuals{
            .color = .new(0.5, 0.9, 0.5),
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
            .color = .new(0.9, 0.5, 0.5),
            .profile = &.{ .new(1.0 / 6.0, 0.2), .new(0.5, -0.2), .new(5.0 / 6.0, 0.2) },
        },
        .A = HardcodedAtomVisuals{
            .color = .fromHex("#FA00FF"),
            .profile = &.{
                .new(1.224892e-1, 1.97281936e-1),
                .new(4.2850158e-1, -7.022254e-2),
                .new(5.059528e-1, -1.8252338e-1),
                .new(6.5015405e-1, -1.3726442e-1),
                .new(8.53909e-1, -3.479591e-2),
            },
            .display = "A",
        },
        .a = HardcodedAtomVisuals{
            .color = .fromHex("#FF8EEC"),
            .profile = &.{
                .new(0.10995328, 0.09992044),
                .new(0.21312654, -0.04334166),
                .new(0.25613064, -0.12345280),
                .new(0.37610608, -0.05840308),
                .new(0.47197282, 0.01870134),
                .new(0.59129760, 0.07036817),
                .new(0.62762183, 0.01940327),
                .new(0.70940369, -0.07002519),
                .new(0.73607069, -0.12074131),
                .new(0.83327174, -0.08838315),
                .new(0.93121342, -0.02669808),
            },
            .display = "a",
        },
        .B = HardcodedAtomVisuals{
            .color = .fromHex("#FFB600"),
            .profile = &.{
                .new(0.1434, 0.1898),
                .new(0.3051, 0.1550),
                .new(0.4668, 0.1661),
                .new(0.6074, -0.1808),
                .new(0.7382, -0.1714),
                .new(0.8648, -0.1829),
            },
            .display = "B",
        },
        .b = HardcodedAtomVisuals{
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
        .C = HardcodedAtomVisuals{
            .color = .fromHex("#00E5FF"),
            .profile = &.{
                .new(0.2628, -0.0853),
                .new(0.4236, 0.3236),
                .new(0.6736, 0.2520),
                .new(0.6907, -0.0213),
                .new(0.9278, -0.0660),
            },
            .display = "C",
        },
        .c = HardcodedAtomVisuals{
            .color = .fromHex("#9EFFF2"),
            .profile = &.{
                .new(0.2433, -0.1498),
                .new(0.3402, 0.1545),
                .new(0.4974, 0.0821),
                .new(0.5276, -0.1117),
                .new(0.6937, -0.1824),
                .new(0.7411, -0.0126),
                .new(0.9522, -0.1181),
            },
            .display = "c",
        },
        .D = HardcodedAtomVisuals{
            .color = .fromHex("#97F200"),
            .profile = &.{
                .new(0.0477, 0.1012),
                .new(0.1360, -0.0462),
                .new(0.3433, -0.1838),
                .new(0.5987, -0.1784),
                .new(0.7620, -0.0463),
                .new(0.8471, -0.1488),
            },
            .display = "D",
        },
        .d = HardcodedAtomVisuals{
            .color = .fromHex("#C8ED8F"),
            .profile = &.{
                .new(0.0419, -0.0446),
                .new(0.0988, -0.0823),
                .new(0.1603, -0.0893),
                .new(0.2078, -0.0724),
                .new(0.2551, -0.0323),
                .new(0.3195, 0.0442),
                .new(0.3459, 0.0098),
                .new(0.4010, 0.0674),
                .new(0.4776, 0.0789),
                .new(0.5534, 0.0731),
                .new(0.6065, 0.0335),
                .new(0.6438, -0.0137),
                .new(0.7175, -0.0749),
                .new(0.8084, -0.0900),
                .new(0.8719, -0.0392),
                .new(0.8992, -0.0767),
            },
            .display = "d",
        },
        .E = HardcodedAtomVisuals{
            .color = .fromHex("#AD32FF"),
            .profile = &.{
                .new(0.17830753, -0.13791077),
                .new(0.41116619, -0.26240891),
                .new(0.50885516, -0.32534608),
                .new(0.58351475, -0.26843008),
                .new(0.86495407, -0.10338914),
            },
            .display = "E",
        },
        .e = HardcodedAtomVisuals{
            .color = .fromHex("#D18EFF"),
            .profile = &.{
                .new(0.10608470, -0.06664436),
                .new(0.16116321, -0.10300380),
                .new(0.19659382, -0.03716342),
                .new(0.31172156, 0.19285297),
                .new(0.39058083, 0.14926526),
                .new(0.81054364, -0.09515910),
                .new(0.89740840, -0.14845288),
                .new(0.93807962, -0.09639150),
            },
            .display = "e",
        },
        .F = HardcodedAtomVisuals{
            .color = .fromHex("#39af97"),
            .profile = &.{
                .new(0.18635559, -0.04046786),
                .new(0.37327039, 0.01354287),
                .new(0.42133540, 0.24769944),
                .new(0.57160512, 0.24997044),
                .new(0.61456209, 0.03512997),
                .new(0.81552447, -0.01316329),
            },
            .display = "F",
        },
        .f = HardcodedAtomVisuals{
            .color = .fromHex("#7FB5B0"),
            .profile = &.{
                .new(0.12697715, -0.02727234),
                .new(0.26351720, 0.00376181),
                .new(0.27961403, 0.12140922),
                .new(0.35451549, 0.12426998),
                .new(0.37303621, -0.00036693),
                .new(0.46742564, -0.02484719),
                .new(0.57099321, -0.02638154),
                .new(0.67700320, -0.05534639),
                .new(0.70734432, -0.16099595),
                .new(0.78599425, -0.14897414),
                .new(0.79141130, -0.03685517),
            },
            .display = "f",
        },
        .firstAsUppercase = HardcodedAtomVisuals{
            .color = .fromHex("#99BBFF"),
            .profile = null,
        },
        .up = HardcodedAtomVisuals{
            .color = .fromHex("#00ffff"),
            .profile = null,
        },
        .down = HardcodedAtomVisuals{
            .color = .fromHex("#ff9900"),
            .profile = null,
        },
        .left = HardcodedAtomVisuals{
            .color = .fromHex("#99ff00"),
            .profile = null,
        },
        .right = HardcodedAtomVisuals{
            .color = .fromHex("#ffff00"),
            .profile = null,
        },
        .letter = HardcodedAtomVisuals{
            .color = .fromHex("#123456"),
            .profile = null,
        },
        // TODO: the debug web build crashes without this!
        .other = HardcodedAtomVisuals{
            .color = .fromHex("#9955ff"),
            .profile = null,
        },
    };

    pub fn init(arena: std.mem.Allocator, gl: Gl) !AtomVisualCache {
        var res: AtomVisualCache = .{
            .visuals_cache = .init(arena),
            .arena = arena,
        };

        inline for (std.meta.fields(@TypeOf(hardcoded_visuals))) |field| {
            const atom_name = field.name;
            const input = @field(hardcoded_visuals, field.name);
            const atom_visuals: AtomVisuals = try .build(arena, .{
                .color = input.color orelse newAtomColor(atom_name),
                .profile = input.profile orelse try res.newAtomProfile(atom_name),
                .display = input.display,
            }, gl);
            try res.visuals_cache.put(atom_name, atom_visuals);
        }

        return res;
    }

    fn newAtomProfile(cache: *AtomVisualCache, name: []const u8) ![]const Vec2 {
        const seed = std.array_hash_map.hashString(name);
        var rnd_state = std.Random.DefaultPrng.init(seed);
        var rnd = Random{ .rnd = rnd_state.random() };

        const profile = try cache.arena.alloc(Vec2, rnd.rnd.intRangeLessThan(usize, 2, 15));
        for (profile) |*p| {
            p.* = Vec2.new(rnd.between(0.01, 0.99), rnd.around0(0.2));
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

    fn newAtomColor(name: []const u8) FColor {
        const seed = std.array_hash_map.hashString(name);
        var rnd_state = std.Random.DefaultPrng.init(seed);
        var rnd = Random{ .rnd = rnd_state.random() };
        return rnd.fcolor();
    }

    pub fn getAtomVisuals(cache: *AtomVisualCache, name: []const u8, gl: Gl) !AtomVisuals {
        const v = try cache.visuals_cache.getOrPut(name);
        if (!v.found_existing) {
            v.value_ptr.* = try .build(cache.arena, .{
                .color = newAtomColor(name),
                .profile = try cache.newAtomProfile(name),
            }, gl);
        }
        return v.value_ptr.*;
    }
};

pub fn drawCase(drawer: *Drawer, camera: Rect, center: Point, case: core.MatchCaseDefinition) !void {
    try drawer.drawSexpr(camera, .{
        .is_pattern = 1,
        .value = case.pattern,
        .pos = center.applyToLocalPoint(.{ .pos = .new(-1, 0) }),
    });
    try drawer.drawSexpr(camera, .{
        .is_pattern = 0,
        .value = case.template,
        .pos = center.applyToLocalPoint(.{ .pos = .new(1, 0) }),
    });
}

pub const Clip = union(enum) {
    circle: struct {
        center: Vec2,
        radius: f32,
    },
};

pub fn drawSexprClipped(drawer: *Drawer, camera: Rect, sexpr: PhysicalSexpr, bounds: Clip) !void {
    assert(in01(sexpr.is_pattern));
    if (sexpr.is_pattern != 0) @panic("TODO");
    try drawer.drawTemplateSexprClipped(camera, sexpr.value, sexpr.pos, bounds);
}

fn drawTemplateSexprClipped(drawer: *Drawer, camera: Rect, sexpr: *const Sexpr, point: Point, bounds: Clip) !void {
    switch (sexpr.*) {
        .atom_lit => |lit| {
            if (ViewHelper.Bounds.touchesTemplateAtom(point, bounds)) {
                const visuals = try drawer.atom_visuals_cache.getAtomVisuals(lit.value);
                try drawer.drawTemplateAtom(camera, point, visuals);
            }
        },
        .atom_var => |v| {
            if (ViewHelper.Bounds.touchesTemplateAtom(point, bounds)) {
                const visuals = try drawer.atom_visuals_cache.getAtomVisuals(v.value);
                try drawer.drawTemplateVariable(camera, point, visuals);
            }
        },
        .pair => |pair| {
            if (ViewHelper.Bounds.touchesTemplateAtom(point, bounds)) {
                if (ViewHelper.Bounds.pairHolderTemplateFullyContained(point, bounds)) {
                    try drawer.drawTemplatePairHolder(camera, point);
                }
                try drawer.drawTemplateSexprClipped(camera, pair.left, point.applyToLocalPoint(ViewHelper.OFFSET_TEMPLATE_PAIR_LEFT), bounds);
                try drawer.drawTemplateSexprClipped(camera, pair.right, point.applyToLocalPoint(ViewHelper.OFFSET_TEMPLATE_PAIR_RIGHT), bounds);
            }
        },
    }
}

pub fn drawSexpr(drawer: *Drawer, camera: Rect, sexpr: PhysicalSexpr, alpha: f32) !void {
    assert(in01(sexpr.is_pattern));
    if (sexpr.is_pattern <= 0.5) {
        try drawer.drawTemplateSexpr(camera, sexpr.value, sexpr.pos.applyToLocalPoint(.{ .turns = remap(
            sexpr.is_pattern,
            0.5,
            0,
            -0.25,
            0,
        ) }), alpha);
    } else {
        try drawer.drawPatternSexpr(camera, sexpr.value, sexpr.pos.applyToLocalPoint(.{ .turns = remap(
            sexpr.is_pattern,
            0.5,
            1,
            0.25,
            0,
        ) }), alpha);
    }
}

pub fn drawHoldedFnk(drawer: *Drawer, camera: Rect, fnk_point: Point, is_main: f32, value: *const Sexpr) !void {
    _ = is_main;
    // drawFnkHolderForFnkAt(camera, fnk_point, is_main);
    if (!value.equals(Sexpr.builtin.identity)) {
        try drawer.drawTemplateSexpr(
            camera,
            value,
            fnk_point,
        );
    }
}

pub fn drawAtom(drawer: *Drawer, camera: Rect, point: Point, is_pattern: bool, name: []const u8, alpha: f32) !void {
    const visuals = drawer.atom_visuals_cache.getAtomVisuals(name, drawer.canvas.gl) catch {
        std.log.err("error getting visuals for atom literal: {s}", .{name});
        return;
    };
    if (is_pattern) {
        try drawer.drawPatternAtom(camera, point, visuals, alpha);
    } else {
        try drawer.drawTemplateAtom(camera, point, visuals, alpha);
    }
}

pub fn drawPairHolder(drawer: *Drawer, camera: Rect, point: Point, is_pattern: bool, alpha: f32) !void {
    if (is_pattern) {
        try drawer.drawPatternPairHolder(camera, point, alpha);
    } else {
        try drawer.drawTemplatePairHolder(camera, point, alpha);
    }
}

pub fn drawTemplateSexpr(drawer: *Drawer, camera: Rect, sexpr: *const Sexpr, point: Point, alpha: f32) !void {
    switch (sexpr.*) {
        .empty => {},
        .atom_lit => |lit| {
            // const visuals = try drawer.atom_visuals_cache.getAtomVisuals(lit.value);
            const visuals = drawer.atom_visuals_cache.getAtomVisuals(lit.value, drawer.canvas.gl) catch {
                std.log.err("error getting visuals for atom literal: {s}", .{lit.value});
                return;
            };
            try drawer.drawTemplateAtom(camera, point, visuals, alpha);
        },
        .atom_var => |v| {
            const visuals = try drawer.atom_visuals_cache.getAtomVisuals(v.value, drawer.canvas.gl);
            try drawer.drawTemplateVariable(camera, point, visuals, alpha);
        },
        .pair => |pair| {
            try drawer.drawTemplatePairHolder(camera, point, alpha);
            // try drawTemplateWildcardLinesNonRecursive(camera, pair.left, pair.right, world_point, .none);
            try drawer.drawTemplateSexpr(camera, pair.left, point.applyToLocalPoint(ViewHelper.OFFSET_TEMPLATE_PAIR_LEFT), alpha);
            try drawer.drawTemplateSexpr(camera, pair.right, point.applyToLocalPoint(ViewHelper.OFFSET_TEMPLATE_PAIR_RIGHT), alpha);
        },
    }
}

pub fn drawPatternSexpr(drawer: *Drawer, camera: Rect, sexpr: *const Sexpr, point: Point, alpha: f32) !void {
    switch (sexpr.*) {
        .empty => {},
        .atom_lit => |lit| {
            const visuals = try drawer.atom_visuals_cache.getAtomVisuals(lit.value, drawer.canvas.gl);
            try drawer.drawPatternAtom(camera, point, visuals, alpha);
        },
        .atom_var => |v| {
            const visuals = try drawer.atom_visuals_cache.getAtomVisuals(v.value, drawer.canvas.gl);
            try drawer.drawPatternVariable(camera, point, visuals, alpha);
        },
        .pair => |pair| {
            try drawer.drawPatternPairHolder(camera, point, alpha);
            // try drawPatternWildcardLinesNonRecursive(camera, pair.left, pair.right, world_point);
            try drawer.drawPatternSexpr(camera, pair.left, point.applyToLocalPoint(ViewHelper.OFFSET_PATTERN_PAIR_LEFT), alpha);
            try drawer.drawPatternSexpr(camera, pair.right, point.applyToLocalPoint(ViewHelper.OFFSET_PATTERN_PAIR_RIGHT), alpha);
        },
    }
}

pub fn pixelWidth(camera: Rect) f32 {
    // const pixel_width = camera.height / window_size.y;
    return camera.size.y / 800.0;
}

pub fn drawPlaceholder(self: *Drawer, camera: Rect, world_point: Point, is_pattern: bool, alpha: f32) !void {
    try self.drawShapeV3(camera, world_point, if (is_pattern)
        AtomVisuals.Geometry.pattern_placeholder
    else
        AtomVisuals.Geometry.template_placeholder, FColor.white.withAlpha(0.5), null, alpha);
}

// TODO: remove
fn drawShapeV3(
    self: *Drawer,
    camera: Rect,
    parent_world_point: Point,
    shape: Canvas.PrecomputedShape,
    stroke: ?FColor,
    fill: ?FColor,
    alpha: f32,
) !void {
    if (fill) |col| {
        // TODO: what if each drawing uses a different camera? what about stencils?
        // var batch: Canvas.ShapesBatch = .{ .canvas = self.canvas };
        // try batch.add(parent_world_point, shape, col.timesAlpha(alpha));
        // batch.draw(camera);
        self.canvas.fillShape(
            camera,
            parent_world_point,
            shape,
            col.timesAlpha(alpha),
        );
    }

    if (stroke) |col| {
        // TODO: wouldnt be needed if Rect had rotation
        const world_points = try self.canvas.frame_arena.allocator().alloc(Vec2, shape.local_points.len + 1);
        for (shape.local_points, world_points[1..]) |p, *dst| {
            dst.* = parent_world_point.applyToLocalPosition(p);
        }
        world_points[0] = world_points[world_points.len - 1];
        self.canvas.line(camera, world_points, pixelWidth(camera), col.timesAlpha(alpha));
    }
}

pub fn drawEatingPattern(
    drawer: *Drawer,
    camera: Rect,
    point: Point,
    binding: core.Binding,
    t: f32,
    alpha: f32,
) !void {
    assert(in01(t));
    const visuals = try drawer.atom_visuals_cache.getAtomVisuals(binding.name, drawer.canvas.gl);
    try drawer.drawShapeV3(camera, point.applyToLocalPoint(.{ .turns = 0.5 }), AtomVisuals.Geometry.template_placeholder, null, visuals.color, alpha * t);
    try drawer.drawSexpr(camera, .{
        .is_pattern = 0,
        .pos = point.applyToLocalPoint(.{ .pos = .new(-3, 0) }),
        .value = binding.value,
    }, alpha);
}

pub fn drawTemplatePairHolder(drawer: *Drawer, camera: Rect, point: Point, alpha: f32) !void {
    try drawShapeV3(
        drawer,
        camera,
        point,
        AtomVisuals.Geometry.template_pair_holder,
        .black,
        .gray(3.0 / 8.0),
        alpha,
    );
}

fn drawTemplateAtom(drawer: *Drawer, camera: Rect, point: Point, visuals: AtomVisuals, alpha: f32) !void {
    try drawer.drawShapeV3(camera, point, visuals.geometry.template, .black, visuals.color, alpha);

    if (visuals.display) |d| {
        const p = point.applyToLocalPosition(.new(0.25, 0));
        try drawer.canvas.drawText(0, camera, d, .centeredAt(p), point.scale, .blackAlpha(alpha));
    }
}

fn drawTemplateVariable(drawer: *Drawer, camera: Rect, point: Point, visuals: AtomVisuals, alpha: f32) !void {
    try drawer.drawShapeV3(camera, point, AtomVisuals.Geometry.template_variable, .black, visuals.color, alpha);
}

pub fn drawPatternPairHolder(drawer: *Drawer, camera: Rect, world_point: Point, alpha: f32) !void {
    try drawer.drawShapeV3(
        camera,
        world_point,
        AtomVisuals.Geometry.pattern_pair_holder,
        .black,
        .gray(3.0 / 8.0),
        alpha,
    );
}

fn drawPatternAtom(drawer: *Drawer, camera: Rect, point: Point, visuals: AtomVisuals, alpha: f32) !void {
    try drawer.drawShapeV3(camera, point, visuals.geometry.pattern, .black, visuals.color, alpha);

    if (visuals.display) |d| {
        const p = point.applyToLocalPosition(.new(-0.25, 0));
        try drawer.canvas.drawText(0, camera, d, .centeredAt(p), point.scale, .blackAlpha(alpha));
    }
}

fn drawPatternVariable(drawer: *Drawer, camera: Rect, point: Point, visuals: AtomVisuals, alpha: f32) !void {
    try drawer.drawShapeV3(camera, point, AtomVisuals.Geometry.pattern_variable, .black, visuals.color, alpha);
}

pub fn drawTemplateSexprWithBindings(drawer: *Drawer, camera: Rect, world_point: Point, sexpr: *const Sexpr, bindings: BindingsState) !void {
    var out_particles: std.ArrayList(BindingParticle) = .init(drawer.canvas.frame_arena.allocator());
    try _drawTemplateSexprWithBindings(drawer, camera, world_point, sexpr, bindings, &out_particles);
    for (out_particles.items) |particle| {
        const visuals = try drawer.atom_visuals_cache.getAtomVisuals(particle.name);
        drawer.canvas.strokeRect(
            camera,
            .fromCenterAndSize(
                particle.point.applyToLocalPosition(.new(1, 0)),
                .both(lerp(7.5, 2.5, particle.t) * particle.point.scale),
            ),
            pixelWidth(camera),
            visuals.color.lighter().lighter(),
        );
    }
}

fn _drawTemplateSexprWithBindings(drawer: *Drawer, camera: Rect, world_point: Point, sexpr: *const Sexpr, bindings: BindingsState, out_particles: *std.ArrayList(BindingParticle)) !void {
    switch (sexpr.*) {
        .atom_lit => |lit| {
            try drawer.drawTemplateAtom(camera, world_point, try drawer.atom_visuals_cache.getAtomVisuals(lit.value));
        },
        .pair => |pair| {
            try _drawTemplateSexprWithBindings(drawer, camera, world_point.applyToLocalPoint(.{
                .pos = .new(0.5, -0.5),
                .scale = 0.5,
            }), pair.left, bindings, out_particles);
            try _drawTemplateSexprWithBindings(drawer, camera, world_point.applyToLocalPoint(.{
                .pos = .new(0.5, 0.5),
                .scale = 0.5,
            }), pair.right, bindings, out_particles);
            try drawer.drawTemplatePairHolder(camera, world_point);
            try drawer.drawTemplateWildcardLinesNonRecursive(camera, pair.left, pair.right, world_point, bindings);
        },
        .atom_var => |x| {
            // TODO: check that compiler skips the loop if anim_t is null
            for (bindings.new) |binding| {
                if (bindings.anim_t) |anim_t| {
                    if (std.mem.eql(u8, binding.name, x.value)) {
                        drawer.clipAtomRegion(camera, world_point);
                        const t = math.smoothstep(anim_t, 0, 0.4);
                        try drawer.drawTemplateSexpr(camera, binding.value, world_point.applyToLocalPoint(.{ .pos = .new(remap(t, 0, 1, -2.3, 0), 0) }));
                        drawer.endClip();

                        drawer.setTransparency(1 - anim_t);
                        try drawer.drawTemplateVariable(camera, world_point, try drawer.atom_visuals_cache.getAtomVisuals(x.value));
                        drawer.setTransparency(1);

                        if (anim_t < 0.5) {
                            try out_particles.append(.{ .point = world_point, .t = t, .name = binding.name });
                        }

                        break;
                    }
                }
            } else for (bindings.old) |binding| {
                if (std.mem.eql(u8, binding.name, x.value)) {
                    try drawer.drawTemplateSexpr(camera, binding.value, world_point);
                    break;
                }
            } else {
                try drawer.drawTemplateVariable(camera, world_point, try drawer.atom_visuals_cache.getAtomVisuals(x.value));
            }
        },
    }
}

pub fn clipAtomRegion(drawer: *Drawer, camera: Rect, world_point: Point) !void {
    drawer.canvas.gl.startStencil();
    try drawer.drawShapeV3(camera, world_point, AtomVisuals.Geometry.template_placeholder, .white, .white, 1);
    drawer.canvas.gl.doneStencil();
}

pub fn endClip(drawer: *Drawer) void {
    drawer.canvas.gl.stopStencil();
}

pub fn drawTemplateWildcardLinesNonRecursive(
    drawer: *Drawer,
    camera: Rect,
    left: *const Sexpr,
    right: *const Sexpr,
    point: Point,
    bindings: BindingsState,
    alpha: f32,
) !void {
    var left_names: std.ArrayList([]const u8) = .init(drawer.canvas.frame_arena.allocator());
    try left.getAllVarNames(&left_names);
    if (bindings.anim_t) |anim_t| if (anim_t >= 0.4) {
        try removeBoundNames(&left_names, bindings.new);
    };
    try removeBoundNames(&left_names, bindings.old);

    var right_names: std.ArrayList([]const u8) = .init(drawer.canvas.frame_arena.allocator());
    try right.getAllVarNames(&right_names);
    if (bindings.anim_t) |anim_t| if (anim_t >= 0.4) {
        try removeBoundNames(&right_names, bindings.new);
    };
    try removeBoundNames(&right_names, bindings.old);

    {
        // TODO: these numbers are not exact, issues when zooming in
        try drawer.drawWildcardsCable(camera, &([1]Vec2{
            point.applyToLocalPosition(.new(-0.5, 0)),
        } ++ funk.fromCountAndCtx(32, struct {
            pub fn anon(k: usize, p: Point) Vec2 {
                return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.5 + 0.25 / 2.0, 0.75, math.tof32(k) / 32)).scale(0.75).add(.new(0.25, 0.25)));
            }
        }.anon, point)), left_names.items, alpha);

        try drawer.drawWildcardsCable(camera, &([1]Vec2{
            point.applyToLocalPosition(.new(-0.5, 0)),
        } ++ funk.fromCountAndCtx(32, struct {
            pub fn anon(k: usize, p: Point) Vec2 {
                return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.5 - 0.25 / 2.0, 0.25, math.tof32(k) / 32)).scale(0.75).add(.new(0.25, -0.25)));
            }
        }.anon, point)), right_names.items, alpha);
    }
}

pub fn drawPatternWildcardLinesNonRecursive(
    drawer: *Drawer,
    camera: Rect,
    left: *const Sexpr,
    right: *const Sexpr,
    point: Point,
    alpha: f32,
) !void {
    var left_names: std.ArrayList([]const u8) = .init(drawer.canvas.frame_arena.allocator());
    try left.getAllVarNames(&left_names);
    var right_names: std.ArrayList([]const u8) = .init(drawer.canvas.frame_arena.allocator());
    try right.getAllVarNames(&right_names);

    const magic_1 = @sqrt(2.0) * 2.0 / 4.0;
    const magic_2 = @sqrt(2.0) * 3.0 / 4.0;
    try drawer.drawWildcardsCable(camera, &(funk.fromCountAndCtx(32, struct {
        pub fn anon(k: usize, p: Point) Vec2 {
            return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(-0.25, -0.25 / 2.0, math.tof32(k) / 32)).scale(magic_1).add(.new(-0.75, -0.5)).addY(magic_1));
        }
    }.anon, point) ++ funk.fromCountAndCtx(32, struct {
        pub fn anon(k: usize, p: Point) Vec2 {
            return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.25 + 0.25 / 2.0, 0.25, math.tof32(k) / 32)).scale(magic_2).add(.new(0.5, 0)).addY(-magic_2));
        }
    }.anon, point)), left_names.items, alpha);
    try drawer.drawWildcardsCable(camera, &(funk.fromCountAndCtx(32, struct {
        pub fn anon(k: usize, p: Point) Vec2 {
            return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(0.25, 0.25 / 2.0, math.tof32(k) / 32)).scale(magic_1).add(.new(-0.75, 0.5)).addY(-magic_1));
        }
    }.anon, point) ++ funk.fromCountAndCtx(32, struct {
        pub fn anon(k: usize, p: Point) Vec2 {
            return p.applyToLocalPosition(Vec2.fromTurns(math.lerp(-0.25 - 0.25 / 2.0, -0.25, math.tof32(k) / 32)).scale(magic_2).add(.new(0.5, 0)).addY(magic_2));
        }
    }.anon, point)), right_names.items, alpha);
}

fn drawWildcardsCable(drawer: *Drawer, camera: Rect, points: []const Vec2, names: []const []const u8, alpha: f32) !void {
    var visuals: std.ArrayList(AtomVisuals) = try .initCapacity(drawer.canvas.frame_arena.allocator(), names.len);
    for (names) |name| {
        visuals.appendAssumeCapacity(try drawer.atom_visuals_cache.getAtomVisuals(name, drawer.canvas.gl));
    }
    for (visuals.items) |v| {
        drawer.canvas.line(camera, points, pixelWidth(camera), v.color.timesAlpha(alpha));
    }
}

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
pub const FColor = math.FColor;
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
const Canvas = kommon.Canvas;

const core = @import("core.zig");
const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const PhysicalSexpr = @import("physical.zig").PhysicalSexpr;
const ViewHelper = @import("physical.zig").ViewHelper;
const BindingsState = @import("physical.zig").BindingsState;

const BindingParticle = struct {
    point: Point,
    t: f32,
    name: []const u8,
};

// TODO: clean these up

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
