pub const Drawer = @This();

canvas: *Canvas,
atom_visuals_cache: AtomVisualCache,

pub fn init(usual: *kommon.Usual) !Drawer {
    return .{
        .canvas = &usual.canvas,
        .atom_visuals_cache = try .init(usual.mem.forever.allocator()),
    };
}

// TODO: also have the border
pub const AtomVisuals = struct {
    profile: []const Vec2,
    color: FColor,
    display: ?[]const u8 = null,
    shape: Canvas.PrecomputedShape,

    fn computeShape(mem: std.mem.Allocator, profile: []const Vec2) !Canvas.PrecomputedShape {
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
        return try .fromOwnedPoints(mem, local_positions);
    }
};

const AtomVisualCache = struct {
    arena: std.mem.Allocator,
    visuals_cache: std.StringHashMap(AtomVisuals),

    const HardcodedAtomVisuals = struct {
        profile: ?[]const Vec2,
        color: FColor,
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

    pub fn init(arena: std.mem.Allocator) !AtomVisualCache {
        var res: AtomVisualCache = .{
            .visuals_cache = .init(arena),
            .arena = arena,
        };

        inline for (std.meta.fields(@TypeOf(hardcoded_visuals))) |field| {
            const atom_name = field.name;
            const input = @field(hardcoded_visuals, field.name);
            const profile = input.profile orelse try res.newAtomProfile(atom_name);
            const atom_visuals: AtomVisuals = .{
                .color = input.color,
                .profile = profile,
                .display = input.display,
                .shape = try AtomVisuals.computeShape(arena, profile),
            };
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

    fn newAtomColor(name: []const u8) FColor {
        const seed = std.array_hash_map.hashString(name);
        var rnd_state = std.Random.DefaultPrng.init(seed);
        var rnd = Random{ .rnd = rnd_state.random() };
        return rnd.fcolor();
    }

    pub fn getAtomVisuals(cache: *AtomVisualCache, name: []const u8) !AtomVisuals {
        const v = try cache.visuals_cache.getOrPut(name);
        if (!v.found_existing) {
            const profile = try cache.newAtomProfile(name);
            const res = AtomVisuals{
                .color = newAtomColor(name),
                .profile = profile,
                .shape = try AtomVisuals.computeShape(cache.arena, profile),
            };
            v.value_ptr.* = res;
        }
        return v.value_ptr.*;
    }
};

pub fn drawSexpr(drawer: *Drawer, camera: Rect, sexpr: PhysicalSexpr) !void {
    assert(in01(sexpr.is_pattern));
    if (sexpr.is_pattern <= 0.5) {
        try drawer.drawTemplateSexpr(camera, sexpr.value, sexpr.pos.applyToLocalPoint(.{ .turns = remap(
            sexpr.is_pattern,
            0.5,
            0,
            -0.25,
            0,
        ) }));
    } else {
        try drawer.drawPatternSexpr(camera, sexpr.value, sexpr.pos.applyToLocalPoint(.{ .turns = remap(
            sexpr.is_pattern,
            0.5,
            1,
            0.25,
            0,
        ) }));
    }
}

fn drawTemplateSexpr(drawer: *Drawer, camera: Rect, sexpr: *const Sexpr, point: Point) !void {
    switch (sexpr.*) {
        .atom_lit => |lit| {
            const visuals = try drawer.atom_visuals_cache.getAtomVisuals(lit.value);
            try drawer.drawTemplateAtom(camera, point, visuals);
        },
        .atom_var => |v| {
            _ = v;
            @panic("TODO");
            // try drawVariable(camera, point, v.value);
        },
        .pair => |pair| {
            try drawer.drawTemplatePairHolder(camera, point);
            // try drawTemplateWildcardLinesNonRecursive(camera, pair.left, pair.right, world_point, .none);
            try drawer.drawTemplateSexpr(camera, pair.left, point.applyToLocalPoint(ViewHelper.OFFSET_TEMPLATE_PAIR_LEFT));
            try drawer.drawTemplateSexpr(camera, pair.right, point.applyToLocalPoint(ViewHelper.OFFSET_TEMPLATE_PAIR_RIGHT));
        },
    }
}

fn drawPatternSexpr(drawer: *Drawer, camera: Rect, sexpr: *const Sexpr, point: Point) !void {
    switch (sexpr.*) {
        .atom_lit => |lit| {
            const visuals = try drawer.atom_visuals_cache.getAtomVisuals(lit.value);
            try drawer.drawPatternAtom(camera, point, visuals);
        },
        .atom_var => |v| {
            _ = v;
            @panic("TODO");
            // try drawPatternVariable(camera, point, v.value);
        },
        .pair => |pair| {
            try drawer.drawPatternPairHolder(camera, point);
            // try drawPatternWildcardLinesNonRecursive(camera, pair.left, pair.right, world_point);
            try drawer.drawPatternSexpr(camera, pair.left, point.applyToLocalPoint(ViewHelper.OFFSET_PATTERN_PAIR_LEFT));
            try drawer.drawPatternSexpr(camera, pair.right, point.applyToLocalPoint(ViewHelper.OFFSET_PATTERN_PAIR_RIGHT));
        },
    }
}

fn pixelWidth(camera: Rect) f32 {
    // const pixel_width = camera.height / window_size.y;
    return camera.size.y / 400.0;
}

// TODO: remove
fn drawShapeV2(self: *Drawer, camera: Rect, parent_world_point: Point, local_points: []const Vec2, stroke: ?FColor, fill: ?FColor) !void {
    if (fill) |col| {
        self.canvas.fillShape(
            camera,
            parent_world_point,
            try self.canvas.tmpShape(local_points),
            col,
        );
    }

    if (stroke) |col| {
        // TODO: wouldnt be needed if Rect had rotation
        const world_points = try self.canvas.frame_arena.allocator().alloc(Vec2, local_points.len + 1);
        for (local_points, world_points[1..]) |p, *dst| {
            dst.* = parent_world_point.applyToLocalPosition(p);
        }
        world_points[0] = world_points[world_points.len - 1];
        self.canvas.line(camera, world_points, pixelWidth(camera), col);
    }
}

fn drawTemplatePairHolder(drawer: *Drawer, camera: Rect, point: Point) !void {
    const local_positions = funk.fromCount(32, struct {
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
    }.anon);

    try drawShapeV2(drawer, camera, point, &local_positions, .black, .gray(3.0 / 8.0));
}

fn drawTemplateAtom(drawer: *Drawer, camera: Rect, point: Point, visuals: AtomVisuals) !void {
    // TODO: no alloc, precompute this
    var screen_positions: []Vec2 = try drawer.canvas.frame_arena.allocator().alloc(Vec2, visuals.shape.local_points.len + 1);

    for (screen_positions[0 .. screen_positions.len - 1], visuals.shape.local_points) |*dst, p| {
        dst.* = point.applyToLocalPosition(p);
    }
    screen_positions[screen_positions.len - 1] = screen_positions[0];

    drawer.canvas.fillShape(
        camera,
        point,
        visuals.shape,
        visuals.color,
    );

    drawer.canvas.line(camera, screen_positions, pixelWidth(camera), .black);

    if (visuals.display) |d| {
        const p = point.applyToLocalPosition(.new(0.25, 0));
        try drawer.canvas.drawText(0, camera, d, .centeredAt(p), point.scale, .black);
    }
}

fn drawPatternPairHolder(drawer: *Drawer, camera: Rect, world_point: Point) !void {
    const local_positions =
        funk.fromCount(32, struct {
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
        }.anon);
    try drawer.drawShapeV2(camera, world_point, &local_positions, .black, .gray(3.0 / 8.0));
}

fn drawPatternAtom(drawer: *Drawer, camera: Rect, point: Point, visuals: AtomVisuals) !void {
    const profile = visuals.profile;
    const skeleton_positions =
        [1]Vec2{.new(-1, -1)} ++
        funk.fromCount(32, struct {
            pub fn anon(k: usize) Vec2 {
                return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
            }
        }.anon) ++ [1]Vec2{.new(-1, 1)};
    // TODO: no allocations
    var local_positions: []Vec2 = drawer.canvas.frame_arena.allocator().alloc(Vec2, skeleton_positions.len + profile.len * 2) catch @panic("TODO");
    for (skeleton_positions, 0..) |pos, i| {
        local_positions[i] = pos;
    }
    for (profile, 0..) |pos, i| {
        local_positions[skeleton_positions.len + i] = Vec2.new(-1.0 - pos.y, 1.0 - pos.x);
        local_positions[skeleton_positions.len + profile.len * 2 - i - 1] = Vec2.new(-1.0 + pos.y, -1.0 + pos.x);
    }
    try drawer.drawShapeV2(camera, point, local_positions, .black, visuals.color);

    if (visuals.display) |d| {
        const p = point.applyToLocalPosition(.new(-0.25, 0));
        try drawer.canvas.drawText(0, camera, d, .centeredAt(p), point.scale, .black);
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
