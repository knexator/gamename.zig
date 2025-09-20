const std = @import("std");
const assert = std.debug.assert;

pub fn inRange(value: anytype, min_inclusive: anytype, max_exclusive: anytype) bool {
    return min_inclusive <= value and value < max_exclusive;
}

pub fn projectToRange(value: anytype, min_inclusive: anytype, max_inclusive: anytype) @TypeOf(value, min_inclusive, max_inclusive) {
    if (value < min_inclusive) return min_inclusive;
    if (value > max_inclusive) return max_inclusive;
    return value;
}

pub fn in01(value: f32) bool {
    return 0 <= value and value <= 1;
}

pub fn inverse_lerp(min: f32, max: f32, value: f32) f32 {
    return (value - min) / (max - min);
}

pub fn remap(value: f32, old_min: f32, old_max: f32, new_min: f32, new_max: f32) f32 {
    return lerp(new_min, new_max, inverse_lerp(old_min, old_max, value));
}

pub fn remapClamped(value: f32, old_min: f32, old_max: f32, new_min: f32, new_max: f32) f32 {
    return lerp(new_min, new_max, clamp01(inverse_lerp(old_min, old_max, value)));
}

/// same as lerp
pub fn remapFrom01(value: f32, new_min: f32, new_max: f32) f32 {
    assert(in01(value));
    return lerp(new_min, new_max, value);
}

pub fn remapTo01Clamped(value: f32, old_min: f32, old_max: f32) f32 {
    return remapClamped(value, old_min, old_max, 0, 1);
}

pub fn towards(v: *f32, goal: f32, max_delta: f32) void {
    if (max_delta <= 0) return;
    if (@abs(v.* - goal) <= max_delta) {
        v.* = goal;
    } else if (v.* < goal) {
        v.* += max_delta;
    } else {
        v.* -= max_delta;
    }
}

pub fn clampPtr(v: *f32, min_inclusive: f32, max_inclusive: f32) void {
    v.* = clamp(v.*, min_inclusive, max_inclusive);
}

const lerp_towards_float = lerp_towards;
pub fn lerp_towards(v: *f32, goal: f32, ratio: f32, delta_seconds: f32) void {
    // TODO: make this framerate independent
    _ = delta_seconds;
    v.* = std.math.lerp(v.*, goal, ratio);
}

pub fn lerp_towards_range(v: *f32, min: f32, max: f32, ratio: f32, delta_seconds: f32) void {
    assert(min <= max);
    if (v.* < min) {
        lerp_towards(v, min, ratio, delta_seconds);
    } else if (v.* > max) {
        lerp_towards(v, max, ratio, delta_seconds);
    }
}

pub fn maybeMirror(v: anytype, mirror: bool) switch (@TypeOf(v)) {
    comptime_float => f32,
    comptime_int => i32,
    else => |x| x,
} {
    return if (mirror) -v else v;
}

pub const clamp = std.math.clamp;
pub const lerp = std.math.lerp;

pub fn clamp01(value: anytype) @TypeOf(value, 0.0) {
    return std.math.clamp(value, 0.0, 1.0);
}

pub fn smoothstep(x: anytype, edge0: anytype, edge1: anytype) @TypeOf(x, edge0, edge1) {
    const y = std.math.clamp((x - edge0) / (edge1 - edge0), 0.0, 1.0);
    return y * y * (3.0 - 2.0 * y);
}

pub fn tof32(value: anytype) f32 {
    const T = @TypeOf(value);
    return switch (@typeInfo(T)) {
        .float, .comptime_float => value,
        .int, .comptime_int => @floatFromInt(value),
        .bool => if (value) 1.0 else 0.0,
        else => @compileError("Expected an int, float, bool, or vector of one, found " ++ @typeName(T)),
    };
}

pub const UVec2 = ZVec2(usize);
pub const IVec2 = ZVec2(isize);

pub inline fn signed(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .int => |i| i.signedness == .signed,
        .float, .comptime_int, .comptime_float => true,
        else => @compileError("bad"),
    };
}

pub fn ZVec2(T: type) type {
    return extern struct {
        pub const Scalar = T;

        x: Scalar,
        y: Scalar,

        const Self = @This();

        pub const zero = new(0, 0);
        pub const one = new(1, 1);
        pub const e1 = new(1, 0);
        pub const e2 = new(0, 1);
        pub const ne1 = if (signed(T)) new(-1, 0) else undefined;
        pub const ne2 = if (signed(T)) new(0, -1) else undefined;

        pub const cardinal_directions = [4]IVec2{
            .new(1, 0),
            .new(0, 1),
            .new(-1, 0),
            .new(0, -1),
        };

        pub const diagonal_directions = [4]IVec2{
            .new(1, 1),
            .new(-1, 1),
            .new(-1, -1),
            .new(1, -1),
        };

        pub const eight_directions = cardinal_directions ++ diagonal_directions;

        pub fn isCardinalDirection(v: Self) bool {
            return (v.x == 0 and @abs(v.y) == 1) or
                (v.y == 0 and @abs(v.x) == 1);
        }

        pub fn isEven(v: Self) bool {
            return @mod(v.x + v.y, 2) == 0;
        }

        pub fn tof32(v: Self) Vec2 {
            return .new(@floatFromInt(v.x), @floatFromInt(v.y));
        }

        pub fn cast(v: Self, S: type) ZVec2(S) {
            return .new(@intCast(v.x), @intCast(v.y));
        }

        pub fn equals(a: Self, b: Self) bool {
            return a.x == b.x and a.y == b.y;
        }

        pub fn new(x: Scalar, y: Scalar) Self {
            return .{ .x = x, .y = y };
        }

        pub fn both(v: Scalar) Self {
            return new(v, v);
        }

        pub fn addInPlace(a: *Self, b: Self) void {
            a.* = a.add(b);
        }

        pub fn add(a: Self, b: Self) Self {
            return new(a.x + b.x, a.y + b.y);
        }

        pub fn addSigned(a: UVec2, b: IVec2) UVec2 {
            return a.cast(isize).add(b).cast(usize);
        }

        pub fn addUnsigned(a: IVec2, b: UVec2) IVec2 {
            return a.add(b.cast(isize));
        }

        pub fn subToSigned(a: UVec2, b: UVec2) IVec2 {
            return a.cast(isize).sub(b.cast(isize));
        }

        pub fn decX(v: Self) Self {
            return new(v.x - 1, v.y);
        }

        pub fn decY(v: Self) Self {
            return new(v.x, v.y - 1);
        }

        pub fn sub(a: Self, b: Self) Self {
            return new(a.x - b.x, a.y - b.y);
        }

        pub fn scale(v: Self, s: Scalar) Self {
            return new(v.x * s, v.y * s);
        }

        pub fn mul(a: Self, b: Self) Self {
            return new(a.x * b.x, a.y * b.y);
        }

        pub fn addX(a: Self, b: Scalar) Self {
            return new(a.x + b, a.y);
        }

        pub fn addY(a: Self, b: Scalar) Self {
            return new(a.x, a.y + b);
        }

        pub fn magSq(v: Self) Scalar {
            return dot(v, v);
        }

        pub fn neg(v: Self) Self {
            if (!signed(T)) unreachable;
            return new(-v.x, -v.y);
        }

        pub fn dot(a: Self, b: Self) Scalar {
            return a.x * b.x + a.y * b.y;
        }

        pub fn rotateOnce(v: Self) Self {
            if (!signed(T)) unreachable;
            return new(-v.y, v.x);
        }

        pub fn perpTo(v: Self, o: Self) bool {
            assert(!v.equals(.zero));
            assert(!o.equals(.zero));
            return v.dot(o) == 0;
        }

        pub fn rotate(v: Self, k: isize) Self {
            var res = v;
            for (0..@abs(k)) |_| {
                res = rotateOnce(res);
            }
            if (k < 0) res = res.neg();
            return res;
        }

        pub fn mod(a: Self, m: UVec2) Self {
            return new(
                @mod(a.x, m.cast(Scalar).x),
                @mod(a.y, m.cast(Scalar).y),
            );
        }

        pub fn aspectRatio(v: Self) f32 {
            assert(T == usize);
            const f = v.tof32();
            return f.x / f.y;
        }
    };
}

const GrowOrShrink = enum { grow, shrink };
pub const Vec2 = extern struct {
    pub const Scalar = f32;

    x: Scalar,
    y: Scalar,

    const Self = @This();

    pub const zero = new(0, 0);
    pub const one = new(1, 1);
    pub const half = new(0.5, 0.5);
    pub const e1 = new(1, 0);
    pub const e2 = new(0, 1);
    pub const ne1 = new(-1, 0);
    pub const ne2 = new(0, -1);

    pub const xpos = new(1, 0);
    pub const ypos = new(0, 1);
    pub const xneg = new(-1, 0);
    pub const yneg = new(0, -1);

    pub const Coord = enum { x, y };
    pub const coords: [2]Coord = .{ .x, .y };

    pub const cardinal_directions = [4]Self{
        .xpos,
        .ypos,
        .xneg,
        .yneg,
    };

    pub fn new(x: Scalar, y: Scalar) Self {
        return .{ .x = x, .y = y };
    }

    pub fn get(v: *Self, which: Coord) Scalar {
        return switch (which) {
            .x => v.x,
            .y => v.y,
        };
    }

    pub fn setInPlace(v: *Self, which: Coord, value: Scalar) void {
        switch (which) {
            .x => v.x = value,
            .y => v.y = value,
        }
    }

    pub fn towardsPure(v: Self, goal: Self, max_delta: f32) Self {
        const delta = goal.sub(v);
        const delta_mag = delta.mag();
        if (delta_mag <= max_delta) {
            return goal;
        } else {
            return v.add(delta.scale(max_delta / delta_mag));
        }
    }

    pub fn towards(v: *Self, goal: Self, max_delta: f32) void {
        v.* = towardsPure(v.*, goal, max_delta);
    }

    pub fn lerpTowardsPure(v: Self, goal: Self, ratio: f32, delta_seconds: f32) Self {
        // TODO: make this framerate independent
        _ = delta_seconds;
        return .lerp(v, goal, ratio);
    }

    pub fn lerpTowards(v: *Self, goal: Self, ratio: f32, delta_seconds: f32) void {
        v.* = lerpTowardsPure(v.*, goal, ratio, delta_seconds);
    }

    pub fn awayFrom(v: Self, goal: Self, min_dist: f32) Self {
        const delta = v.sub(goal);
        const delta_mag = delta.mag();
        if (delta_mag >= min_dist) {
            return v;
        } else if (delta_mag == 0) {
            return goal.addX(min_dist);
        } else {
            return goal.add(delta.scale(min_dist / delta_mag));
        }
    }

    pub fn toInt(v: Self, S: type) ZVec2(S) {
        const v2: Vec2 = switch (@typeInfo(S).int.signedness) {
            .unsigned => v,
            .signed => .new(@floor(v.x), @floor(v.y)),
        };
        return .new(@intFromFloat(v2.x), @intFromFloat(v2.y));
    }

    pub fn both(v: Scalar) Self {
        return new(v, v);
    }

    pub fn withZ(v: Self, z: Scalar) Vec3 {
        return .new(v.x, v.y, z);
    }

    pub fn add(a: Self, b: Self) Self {
        return new(a.x + b.x, a.y + b.y);
    }

    pub fn addInPlace(a: *Self, b: Self) void {
        a.* = a.add(b);
    }

    pub fn sub(a: Self, b: Self) Self {
        return new(a.x - b.x, a.y - b.y);
    }

    pub fn scale(v: Self, s: Scalar) Self {
        return new(v.x * s, v.y * s);
    }

    pub fn neg(v: Self) Self {
        return new(-v.x, -v.y);
    }

    pub fn mul(a: Self, b: Self) Self {
        return new(a.x * b.x, a.y * b.y);
    }

    pub fn div(a: Self, b: Self) Self {
        return new(a.x / b.x, a.y / b.y);
    }

    pub fn addX(a: Self, b: Scalar) Self {
        return new(a.x + b, a.y);
    }

    pub fn addY(a: Self, b: Scalar) Self {
        return new(a.x, a.y + b);
    }

    /// deprecated
    pub fn perpCW(v: Self) Self {
        return v.rotPosQuarter();
    }

    pub fn rotPosQuarter(v: Self) Self {
        return new(-v.y, v.x);
    }

    pub fn rotNegQuarter(v: Self) Self {
        return new(-v.y, v.x);
    }

    test "rotate quarter" {
        try Vec2.expectApproxEqAbs(Vec2.e2, Vec2.e1.rotPosQuarter(), 0.001);
        try Vec2.expectApproxEqAbs(Vec2.e1.neg(), Vec2.e2.rotPosQuarter(), 0.001);
    }

    pub fn rotQuarters(v: Self, k: isize) Self {
        var res = v;
        for (0..@as(usize, @intCast(@mod(k, 4)))) |_| {
            res = rotPosQuarter(res);
        }
        return res;
    }

    test "rotate quarters" {
        try Vec2.expectApproxEqAbs(Vec2.e1, Vec2.e1.rotQuarters(4), 0.001);
        try Vec2.expectApproxEqAbs(Vec2.e1, Vec2.e1.rotQuarters(-4), 0.001);
        try Vec2.expectApproxEqAbs(Vec2.e2, Vec2.e1.rotQuarters(1), 0.001);
        try Vec2.expectApproxEqAbs(Vec2.e2.neg(), Vec2.e1.rotQuarters(-1), 0.001);
    }

    pub fn rotate(v: Self, turns: f32) Self {
        const c = @cos(turns * std.math.tau);
        const s = @sin(turns * std.math.tau);
        return new(
            v.x * c - v.y * s,
            v.x * s + v.y * c,
        );
    }

    test "rotate" {
        try Vec2.expectApproxEqAbs(Vec2.e2, rotate(Vec2.e1, 0.25), 0.001);
    }

    pub fn rotateAround(v: Self, center: Self, turns: f32) Self {
        const delta = v.sub(center);
        const rotated_delta = delta.rotate(turns);
        return center.add(rotated_delta);
    }

    pub fn fromTurns(turns: f32) Self {
        return e1.rotate(turns);
    }

    pub fn fromPolar(length: f32, turns: f32) Self {
        return fromTurns(turns).scale(length);
    }

    pub fn normalized(v: Self) Self {
        return v.scale(1 / v.mag());
    }

    pub fn mag(v: Self) Scalar {
        return @sqrt(v.magSq());
    }

    pub fn magSq(v: Self) Scalar {
        return dot(v, v);
    }

    pub fn dot(a: Self, b: Self) Scalar {
        return a.x * b.x + a.y * b.y;
    }

    /// The area of the paralelogram with a,b as sides
    pub fn cross(a: Self, b: Self) Scalar {
        return a.x * b.y - a.y * b.x;
    }

    test "cross" {
        try std.testing.expectEqual(1, cross(.e1, .e2));
        try std.testing.expectEqual(-1, cross(.e2, .e1));
        try std.testing.expectEqual(3, cross(Vec2.e1.scale(3), .e2));
        try std.testing.expectEqual(0, cross(.e1, .e1));
        try std.testing.expectEqual(0, cross(.e2, .e2));
    }

    pub fn lerp(a: Self, b: Self, t: f32) Self {
        return new(
            std.math.lerp(a.x, b.x, t),
            std.math.lerp(a.y, b.y, t),
        );
    }

    pub fn atBezier(p0: Self, p1: Self, p2: Self, t: f32) Self {
        const pA: Self = .lerp(p0, p1, t);
        const pB: Self = .lerp(p1, p2, t);
        return .lerp(pA, pB, t);
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.x == b.x and a.y == b.y;
    }

    pub fn aspectRatio(v: Self) f32 {
        return v.x / v.y;
    }

    pub fn withAspectRatio(original: Vec2, target_ratio: f32, mode: GrowOrShrink) Vec2 {
        const actual_ratio = original.x / original.y;
        if (actual_ratio < target_ratio) {
            return switch (mode) {
                .grow => original.mul(.new(target_ratio / actual_ratio, 1)),
                .shrink => original.mul(.new(1, actual_ratio / target_ratio)),
            };
        } else if (actual_ratio > target_ratio) {
            return switch (mode) {
                .grow => original.mul(.new(1, actual_ratio / target_ratio)),
                .shrink => original.mul(.new(target_ratio / actual_ratio, 1)),
            };
        } else return original;
    }

    test "withAspectRatio" {
        try expectApproxEqAbs(.new(2, 1), Vec2.one.withAspectRatio(2, .grow), 0.0001);
        try expectApproxEqAbs(.new(1, 0.5), Vec2.one.withAspectRatio(2, .shrink), 0.0001);
        try expectApproxEqAbs(.new(1, 2), Vec2.one.withAspectRatio(0.5, .grow), 0.0001);
        try expectApproxEqAbs(.new(0.5, 1), Vec2.one.withAspectRatio(0.5, .shrink), 0.0001);

        try expectApproxEqAbs(.new(6, 3), Vec2.new(4, 3).withAspectRatio(2, .grow), 0.0001);
        try expectApproxEqAbs(.new(4, 2), Vec2.new(4, 3).withAspectRatio(2, .shrink), 0.0001);
    }

    pub fn minEach(a: Vec2, b: Vec2) Vec2 {
        return .new(
            @min(a.x, b.x),
            @min(a.y, b.y),
        );
    }

    pub fn maxEach(a: Vec2, b: Vec2) Vec2 {
        return .new(
            @max(a.x, b.x),
            @max(a.y, b.y),
        );
    }

    pub fn expectApproxEqRel(expected: Vec2, actual: Vec2, tolerance: anytype) !void {
        try std.testing.expectApproxEqRel(expected.x, actual.x, tolerance);
        try std.testing.expectApproxEqRel(expected.y, actual.y, tolerance);
    }

    pub fn expectApproxEqAbs(expected: Vec2, actual: Vec2, tolerance: anytype) !void {
        try std.testing.expectApproxEqAbs(expected.x, actual.x, tolerance);
        try std.testing.expectApproxEqAbs(expected.y, actual.y, tolerance);
    }

    pub fn format(value: Vec2, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
        std.debug.assert(std.mem.eql(u8, fmt, ""));
        std.debug.assert(std.meta.eql(options, .{}));
        try writer.print("Vec2({d},{d})", .{ value.x, value.y });
    }
};

pub fn square(x: anytype) @TypeOf(x) {
    return x * x;
}

pub fn sin(turns: f32) f32 {
    return @sin(turns * std.math.tau);
}
pub fn cos(turns: f32) f32 {
    return @cos(turns * std.math.tau);
}

pub const Vec3 = extern struct {
    pub const Scalar = f32;

    x: Scalar,
    y: Scalar,
    z: Scalar,

    const Self = @This();

    pub const zero = new(0, 0, 0);
    pub const one = new(1, 1, 1);
    pub const half = new(0.5, 0.5, 0.5);
    pub const e1 = new(1, 0, 0);
    pub const e2 = new(0, 1, 0);
    pub const e3 = new(0, 0, 1);

    pub fn new(x: Scalar, y: Scalar, z: Scalar) Self {
        return .{ .x = x, .y = y, .z = z };
    }

    pub fn XY(v: Self) Vec2 {
        return .new(v.x, v.y);
    }

    pub fn add(a: Self, b: Self) Self {
        return new(a.x + b.x, a.y + b.y, a.z + b.z);
    }

    pub fn addInPlace(a: *Self, b: Self) void {
        a.* = a.add(b);
    }

    pub fn sub(a: Self, b: Self) Self {
        return new(a.x - b.x, a.y - b.y, a.z - b.z);
    }

    pub fn scale(v: Self, s: Scalar) Self {
        return new(v.x * s, v.y * s, v.z * s);
    }

    pub fn neg(v: Self) Self {
        return new(-v.x, -v.y, -v.z);
    }

    pub fn mul(a: Self, b: Self) Self {
        return new(a.x * b.x, a.y * b.y, a.z * b.z);
    }

    pub fn div(a: Self, b: Self) Self {
        return new(a.x / b.x, a.y / b.y, a.z / b.z);
    }

    pub fn normalized(v: Self) Self {
        return v.scale(1 / v.mag());
    }

    pub fn mag(v: Self) Scalar {
        return @sqrt(v.magSq());
    }

    pub fn magSq(v: Self) Scalar {
        return dot(v, v);
    }

    pub fn isNormal(self: Self) bool {
        return std.math.approxEqAbs(Scalar, self.magSq(), 1.0, 0.001);
    }

    pub fn dot(a: Self, b: Self) Scalar {
        return a.x * b.x + a.y * b.y + a.z * b.z;
    }

    pub fn lerp(a: Self, b: Self, t: f32) Self {
        return new(
            std.math.lerp(a.x, b.x, t),
            std.math.lerp(a.y, b.y, t),
            std.math.lerp(a.z, b.z, t),
        );
    }

    pub fn sampleBasis(v: Self, basis: [3]Self) Self {
        return Vec3.add(
            basis[0].scale(v.x),
            Vec3.add(
                basis[1].scale(v.y),
                basis[2].scale(v.z),
            ),
        );
    }

    pub fn equals(a: Self, b: Self) bool {
        return a.x == b.x and a.y == b.y and a.z == b.z;
    }

    pub fn expectApproxEqRel(expected: Vec2, actual: Vec2, tolerance: anytype) !void {
        try std.testing.expectApproxEqRel(expected.x, actual.x, tolerance);
        try std.testing.expectApproxEqRel(expected.y, actual.y, tolerance);
        try std.testing.expectApproxEqRel(expected.z, actual.z, tolerance);
    }

    pub fn expectApproxEqAbs(expected: Vec2, actual: Vec2, tolerance: anytype) !void {
        try std.testing.expectApproxEqAbs(expected.x, actual.x, tolerance);
        try std.testing.expectApproxEqAbs(expected.y, actual.y, tolerance);
        try std.testing.expectApproxEqAbs(expected.z, actual.z, tolerance);
    }
};

pub const Bivec3 = extern struct {
    pub const Scalar = f32;
    const Self = @This();

    xy: Scalar,
    yz: Scalar,
    zx: Scalar,

    pub const zero = new(0, 0, 0);
    // pub const one = new(1, 1, 1);
    // pub const half = new(0.5, 0.5, 0.5);
    // pub const e1 = new(1, 0, 0);
    // pub const e2 = new(0, 1, 0);
    // pub const e3 = new(0, 0, 1);

    pub fn new(xy: Scalar, yz: Scalar, zx: Scalar) Self {
        return .{ .xy = xy, .yz = yz, .zx = zx };
    }

    pub fn wedge(u: Vec3, v: Vec3) Self {
        return .{
            .xy = u.x * v.y - u.y * v.x,
            .yz = u.y * v.z - u.z * v.y,
            .zx = u.z * v.x - u.x * v.z,
        };
    }

    fn magSq(self: Bivec3) Scalar {
        return self.xy * self.xy + self.yz * self.yz + self.zx * self.zx;
    }

    fn scale(self: Bivec3, s: Scalar) Bivec3 {
        return new(
            self.xy * s,
            self.yz * s,
            self.zx * s,
        );
    }

    pub fn isNormal(self: Self) bool {
        return std.math.approxEqAbs(Scalar, self.magSq(), 1.0, 0.001);
    }
};

// TODO: rename
pub const General3 = struct {
    pub const Scalar = f32;
    const Self = @This();

    // TODO: rename 'rank'
    rank0: Scalar,
    rank1: Vec3,
    rank2: Bivec3,

    pub fn vectorProduct(a: Vec3, b: Vec3) Self {
        return .{
            .rank0 = Vec3.dot(a, b),
            .rank1 = .zero,
            .rank2 = Bivec3.wedge(a, b),
        };
    }

    // https://marctenbosch.com/quaternions/#h_13
    // pub fn reflect(plane_unit_normal: Vec3, pos: Vec3) Vec3 {
    //     const asdf = vectorProduct(pos, plane_unit_normal);
    // }
};

// from https://marctenbosch.com/quaternions/code.htm
pub const Rotor3 = extern struct {
    pub const Scalar = f32;

    bivec: Bivec3 = .zero,
    scalar: Scalar = 1.0,

    pub fn fromTwoVecs(from: Vec3, to: Vec3) Rotor3 {
        assert(from.isNormal());
        assert(to.isNormal());
        return normalized(.{
            .scalar = 1 + Vec3.dot(to, from),
            .bivec = Bivec3.wedge(to, from),
        });
    }
    pub fn fromAngleAndPlane(plane: Bivec3, turns: f32) Rotor3 {
        assert(plane.isNormal());
        return .{
            .scalar = cos(turns / 2.0),
            .bivec = plane.scale(-sin(turns / 2.0)),
        };
    }

    pub fn rotate(rotor: Rotor3, v: Vec3) Vec3 {
        const b = rotor.bivec;

        const q = Vec3.add(
            v.scale(rotor.scalar),
            .new(
                v.y * b.xy - v.z * b.zx,
                v.z * b.yz - v.x * b.xy,
                v.y * b.zx - v.y * b.yz,
            ),
        );

        // trivector!
        const q_t =
            v.x * b.yz +
            v.y * b.zx +
            v.z * b.xy;

        return Vec3.add(
            q.scale(rotor.scalar),
            .new(
                // zig fmt: off
                 q.y * b.xy - q.z * b.zx + q_t * b.yz,
                -q.x * b.xy + q_t * b.zx + q.z * b.yz,
                 q_t * b.xy + q.x * b.zx - q.y * b.yz,
                // zig fmt: on
            ),
        );
    }

    fn normalized(self: Rotor3) Rotor3 {
        const s = 1.0 / self.mag();
        return .{
            .scalar = self.scalar * s,
            .bivec = self.bivec.scale(s),
        };
    }

    fn magSq(self: Rotor3) Scalar {
        return square(self.scalar) + self.bivec.magSq();
    }

    fn mag(self: Rotor3) Scalar {
        return @sqrt(self.magSq());
    }
};

pub const URect = struct {
    top_left: UVec2,
    inner_size: UVec2,

    pub fn get(self: Rect, which: enum { top_center, top_right }) Vec2 {
        return switch (which) {
            .top_center => self.top_left.addX(self.size.x / 2),
            .top_right => self.top_left.addX(self.size.x),
        };
    }

    pub fn fromCorners(top_left: UVec2, bottom_right: UVec2) URect {
        assert(top_left.x <= bottom_right.x);
        assert(top_left.y <= bottom_right.y);
        return .{
            .top_left = top_left,
            .inner_size = .sub(bottom_right, top_left),
        };
    }
};

/// unitialized if inner_size is zero
pub const IBounds = struct {
    top_left: IVec2,
    inner_size: UVec2,

    pub const empty: IBounds = .{ .inner_size = .zero, .top_left = undefined };

    pub fn plusTile(original: *IBounds, p: IVec2) void {
        original.* = original.bounding(p);
    }

    pub fn bounding(original: IBounds, p: IVec2) IBounds {
        if (original.inner_size.equals(.zero)) return .{
            .top_left = p,
            .inner_size = .one,
        };
        var top_left = original.top_left;
        var bottom_right = original.top_left.add(original.inner_size.cast(isize));
        top_left.x = @min(top_left.x, p.x);
        top_left.y = @min(top_left.y, p.y);
        bottom_right.x = @max(bottom_right.x, p.x + 1);
        bottom_right.y = @max(bottom_right.y, p.y + 1);

        return .fromCorners(top_left, bottom_right);
    }

    pub fn fromCorners(top_left: IVec2, bottom_right: IVec2) IBounds {
        assert(top_left.x < bottom_right.x);
        assert(top_left.y < bottom_right.y);
        return .{
            .top_left = top_left,
            .inner_size = IVec2.sub(bottom_right, top_left).cast(usize),
        };
    }

    pub fn asRect(self: IBounds) Rect {
        return .{
            .top_left = self.top_left.tof32(),
            .size = self.inner_size.tof32(),
        };
    }

    pub fn fromRect(rect: Rect) IBounds {
        const top_left: IVec2 = .new(
            @intFromFloat(@floor(rect.top_left.x)),
            @intFromFloat(@floor(rect.top_left.y)),
        );
        const bottom_right: IVec2 = .new(
            @intFromFloat(@ceil(rect.top_left.x + rect.size.x)),
            @intFromFloat(@ceil(rect.top_left.y + rect.size.y)),
        );
        return .fromCorners(top_left, bottom_right);
    }

    pub fn contains(self: IBounds, p: IVec2) bool {
        return inRange(p.x, self.top_left.x, self.top_left.x + self.inner_size.cast(isize).x) and
            inRange(p.y, self.top_left.y, self.top_left.y + self.inner_size.cast(isize).y);
    }

    pub fn move(original: IBounds, delta: IVec2) IBounds {
        return .{
            .top_left = original.top_left.add(delta),
            .inner_size = original.inner_size,
        };
    }

    pub fn bottomRight(self: IBounds) IVec2 {
        return self.top_left.addUnsigned(self.inner_size);
    }

    pub fn plusMargin(original: IBounds, v: isize) IBounds {
        return .{
            .top_left = original.top_left.sub(.both(v)),
            .inner_size = original.inner_size.addSigned(.both(v * 2)),
        };
    }
};

pub const Rect = extern struct {
    top_left: Vec2,
    size: Vec2,

    pub const unit: Rect = .{ .top_left = .zero, .size = .one };

    pub fn lerp(a: Rect, b: Rect, t: f32) Rect {
        return .{
            .top_left = .lerp(a.top_left, b.top_left, t),
            .size = .lerp(a.size, b.size, t),
        };
    }

    pub fn intersect(a: Rect, b: Rect) ?Rect {
        // TODO NOW
        _ = b;
        return a;
    }

    pub fn getCenter(self: Rect) Vec2 {
        return self.top_left.add(self.size.scale(0.5));
    }

    pub fn fromCorners(top_left: Vec2, bottom_right: Vec2) Rect {
        return .{ .top_left = top_left, .size = bottom_right.sub(top_left) };
    }

    pub fn fromRanges(x_range: [2]f32, y_range: [2]f32) Rect {
        assert(x_range[0] <= x_range[1]);
        assert(y_range[0] <= y_range[1]);
        return .fromCorners(
            .new(x_range[0], y_range[0]),
            .new(x_range[1], y_range[1]),
        );
    }

    pub const Side = enum {
        right,
        bottom,
        left,
        top,

        pub const all: [4]Side = .{ .right, .bottom, .left, .top };

        pub fn sign(self: Side) f32 {
            return switch (self) {
                .right, .bottom => -1,
                .left, .top => 1,
            };
        }
    };

    pub const MeasureKind = enum {
        top_left,
        top_center,
        top_right,
        center_left,
        center,
        center_right,
        bottom_left,
        bottom_center,
        bottom_right,
        size,

        pub const all9: [9]MeasureKind = .{
            .top_left,
            .top_center,
            .top_right,
            .center_left,
            .center,
            .center_right,
            .bottom_left,
            .bottom_center,
            .bottom_right,
        };

        pub fn asPivot(m: MeasureKind) Vec2 {
            return switch (m) {
                .size => @panic("can't convert size to pivot"),
                .top_left => .new(0, 0),
                .top_center => .new(0.5, 0),
                .top_right => .new(1, 0),
                .center_left => .new(0, 0.5),
                .center => .new(0.5, 0.5),
                .center_right => .new(1, 0.5),
                .bottom_left => .new(0, 1),
                .bottom_center => .new(0.5, 1),
                .bottom_right => .new(1, 1),
            };
        }
    };
    // TODO: autogen from enum
    /// They are all Vec2 since otherwise .get wouldn't know what to return
    pub const Measure = union(MeasureKind) {
        top_left: Vec2,
        top_center: Vec2,
        top_right: Vec2,
        center_left: Vec2,
        center: Vec2,
        center_right: Vec2,
        bottom_left: Vec2,
        bottom_center: Vec2,
        bottom_right: Vec2,
        size: Vec2,
    };

    pub fn getAt(self: Rect, local_pos: Vec2) Vec2 {
        return self.worldFromLocal(local_pos);
    }

    pub fn get(self: Rect, which: MeasureKind) Vec2 {
        return switch (which) {
            .top_left => self.top_left,
            .top_center => self.top_left.addX(self.size.x / 2.0),
            .top_right => self.top_left.addX(self.size.x),
            .size => self.size,
            .bottom_center => self.top_left.add(self.size.mul(.new(0.5, 1))),
            .bottom_left => self.top_left.addY(self.size.y),
            .bottom_right => self.top_left.add(self.size),
            .center => self.top_left.add(self.size.scale(0.5)),
            .center_left => self.top_left.addY(self.size.y / 2.0),
            .center_right => self.top_left.add(self.size.mul(.new(1, 0.5))),
        };
    }

    pub fn contains(self: Rect, p: Vec2) bool {
        return inRange(p.x, self.top_left.x, self.top_left.x + self.size.x) and
            inRange(p.y, self.top_left.y, self.top_left.y + self.size.y);
    }

    pub fn projectPos(self: Rect, p: Vec2) Vec2 {
        return .new(
            projectToRange(p.x, self.top_left.x, self.top_left.x + self.size.x),
            projectToRange(p.y, self.top_left.y, self.top_left.y + self.size.y),
        );
    }

    pub fn plusMargin(self: Rect, v: f32) Rect {
        return self.plusMargin2(.both(v));
    }

    pub fn plusMargin2(self: Rect, v: Vec2) Rect {
        return .{
            .top_left = self.top_left.sub(v),
            .size = self.size.add(v.scale(2)),
        };
    }

    pub fn plusMargin3(self: Rect, which: enum { left, right, top, bottom }, amount: f32) Rect {
        return switch (which) {
            else => @panic("TODO"),
            .top => .{
                .top_left = self.top_left.addY(-amount),
                .size = self.size.addY(amount),
            },
        };
    }

    pub fn fromCenterAndSize(center: Vec2, size: Vec2) Rect {
        return .{ .top_left = center.sub(size.scale(0.5)), .size = size };
    }

    pub fn fromPivotAndSize(pos: Vec2, pivot: Vec2, size: Vec2) Rect {
        return .{ .top_left = pos.sub(size.mul(pivot)), .size = size };
    }

    pub fn fromMeasureAndSizeV2(measure_kind: MeasureKind, measure_value: Vec2, size: Vec2) Rect {
        return .fromPivotAndSize(measure_value, measure_kind.asPivot(), size);
    }

    pub fn fromMeasureAndSize(measure: Measure, size: Vec2) Rect {
        return switch (measure) {
            .top_left => |v| .fromPivotAndSize(v, MeasureKind.top_left.asPivot(), size),
            .top_center => |v| .fromPivotAndSize(v, MeasureKind.top_center.asPivot(), size),
            .top_right => |v| .fromPivotAndSize(v, MeasureKind.top_right.asPivot(), size),
            .center_left => |v| .fromPivotAndSize(v, MeasureKind.center_left.asPivot(), size),
            .center => |v| .fromPivotAndSize(v, MeasureKind.center.asPivot(), size),
            .center_right => |v| .fromPivotAndSize(v, MeasureKind.center_right.asPivot(), size),
            .bottom_left => |v| .fromPivotAndSize(v, MeasureKind.bottom_left.asPivot(), size),
            .bottom_center => |v| .fromPivotAndSize(v, MeasureKind.bottom_center.asPivot(), size),
            .bottom_right => |v| .fromPivotAndSize(v, MeasureKind.bottom_right.asPivot(), size),
            .size => unreachable,
        };
    }

    pub fn fromV3(measure1_kind: MeasureKind, measure1_value: Vec2, measure2_kind: MeasureKind, measure2_value: Vec2) Rect {
        if (measure1_kind == .size or measure2_kind == .size) {
            if (measure1_kind == .size and measure2_kind == .size) @panic("bad params (2 sizes)");
            if (measure1_kind == .size) return .fromV3(measure2_kind, measure2_value, measure1_kind, measure1_value);
            assert(measure1_kind != .size);
            assert(measure2_kind == .size);
            return .fromPivotAndSize(measure1_value, measure1_kind.asPivot(), measure2_value);
        } else {
            @panic("TODO");
        }
    }

    pub fn fromV2(measure1: Measure, measure2: Measure) Rect {
        return .from(.{ measure1, measure2 });
    }

    // TODO: delete
    pub fn from2(measure1: Measure, measure2: Measure) Rect {
        return .fromV2(measure1, measure2);
    }

    pub fn from(measures: [2]Measure) Rect {
        switch (measures[1]) {
            .size => |size| return .fromMeasureAndSize(measures[0], size),
            else => switch (measures[0]) {
                else => std.debug.panic("TODO: measure[0] {s}", .{@tagName(measures[0])}),
                .top_left => |top_left| {
                    switch (measures[1]) {
                        else => |x| std.debug.panic("TODO: from {s}", .{@tagName(x)}),
                        .bottom_right => |bottom_right| {
                            return .{ .size = bottom_right.sub(top_left), .top_left = top_left };
                        },
                    }
                },
            },
        }
    }

    pub fn zoom(original: Rect, fixed_world_pos: Vec2, s: f32) Rect {
        const new_size = original.size.scale(s);
        return .fromPivotAndSize(fixed_world_pos, original.localFromWorldPosition(fixed_world_pos), new_size);
    }

    pub fn setMaxSize(original: Rect, max_size: Vec2, keep: MeasureKind) Rect {
        return .fromPivotAndSize(original.get(keep), keep.asPivot(), .minEach(original.size, max_size));
    }

    pub fn setMinSize(original: Rect, min_size: Vec2, keep: MeasureKind) Rect {
        return .fromPivotAndSize(original.get(keep), keep.asPivot(), .maxEach(original.size, min_size));
    }

    pub fn withSize(original: Rect, size: Vec2, keep: MeasureKind) Rect {
        return .fromPivotAndSize(original.get(keep), keep.asPivot(), size);
    }

    pub fn with2(original: Rect, change_kind: MeasureKind, change_value: Vec2, keep: MeasureKind) Rect {
        if (change_kind == .size) {
            return .fromPivotAndSize(original.get(keep), keep.asPivot(), change_value);
        } else @panic("TODO");
    }

    pub fn with(original: Rect, change: Measure, keep: MeasureKind) Rect {
        return switch (change) {
            else => std.debug.panic("TODO: change {s}", .{@tagName(change)}),
            .top_left => |top_left| switch (keep) {
                else => std.debug.panic("TODO: keep {s}", .{@tagName(keep)}),
                .size => .from(.{ .{ .top_left = top_left }, .{ .size = original.get(.size) } }),
            },
            .bottom_center => |bottom_center| switch (keep) {
                else => std.debug.panic("TODO: keep {s}", .{@tagName(keep)}),
                .size => .from(.{ .{ .bottom_center = bottom_center }, .{ .size = original.get(.size) } }),
            },
            .center => |center| switch (keep) {
                else => std.debug.panic("TODO: keep {s}", .{@tagName(keep)}),
                .size => .from(.{ .{ .center = center }, .{ .size = original.get(.size) } }),
            },
            .bottom_left => |bottom_left| switch (keep) {
                else => std.debug.panic("TODO: keep {s}", .{@tagName(keep)}),
                .size => .from(.{ .{ .bottom_left = bottom_left }, .{ .size = original.get(.size) } }),
            },
            .size => |size| .fromV3(keep, original.get(keep), .size, size),
            // switch (keep) {
            //     else => std.debug.panic("TODO: keep {s}", .{@tagName(keep)}),
            //     .center => .fromCenterAndSize(original.get(.center), size),
            //     .top_left => .from(.{ .{ .top_left = original.get(.top_left) }, .{ .size = size } }),
            //     .top_center => .from(.{ .{ .top_center = original.get(.top_center) }, .{ .size = size } }),
            //     .bottom_center => .from(.{ .{ .bottom_center = original.get(.bottom_center) }, .{ .size = size } }),
            //     .top_right => .from(.{ .{ .top_right = original.get(.top_right) }, .{ .size = size } }),
            // },
        };
    }

    pub fn resizeRel(original: Rect, s: f32, keep: MeasureKind) Rect {
        return original.with(.{ .size = original.size.scale(s) }, keep);
    }

    pub fn scale(original: Rect, s: f32, keep: MeasureKind) Rect {
        return original.resizeRel(s, keep);
    }

    pub fn move(original: Rect, delta: Vec2) Rect {
        return .{
            .top_left = original.top_left.add(delta),
            .size = original.size,
        };
    }

    pub fn boundingOOP(comptime T: type, objs: []const T, comptime prop: []const u8) Rect {
        assert(objs.len > 0);
        var bounds = @field(objs[0], prop);
        for (objs[1..]) |q| bounds = Rect.bounding(&.{ bounds, @field(q, prop) });
        return bounds;
    }

    pub fn deltaToAchieve(current: Rect, target: Measure) Vec2 {
        assert(std.meta.activeTag(target) != .size);
        return current.with(target, .size).top_left.sub(current.top_left);
    }

    pub fn bounding(rects: []const Rect) Rect {
        assert(rects.len > 0);
        var top_left = rects[0].get(.top_left);
        var bottom_right = rects[0].get(.bottom_right);
        for (rects[1..]) |r| {
            top_left.x = @min(top_left.x, r.get(.top_left).x);
            top_left.y = @min(top_left.y, r.get(.top_left).y);
            bottom_right.x = @max(bottom_right.x, r.get(.bottom_right).x);
            bottom_right.y = @max(bottom_right.y, r.get(.bottom_right).y);
        }
        return .from(.{
            .{ .top_left = top_left },
            .{ .bottom_right = bottom_right },
        });
    }

    pub fn boundingPoints(points: []const Vec2) Rect {
        assert(points.len > 0);
        var top_left = points[0];
        var bottom_right = points[0];
        for (points[1..]) |p| {
            top_left.x = @min(top_left.x, p.x);
            top_left.y = @min(top_left.y, p.y);
            bottom_right.x = @max(bottom_right.x, p.x);
            bottom_right.y = @max(bottom_right.y, p.y);
        }
        return .from(.{
            .{ .top_left = top_left },
            .{ .bottom_right = bottom_right },
        });
    }

    pub fn fromPoint(point: Point, which: MeasureKind, base_size: Vec2) Rect {
        assert(point.turns == 0.0);
        const size = base_size.scale(point.scale);
        const top_left: Vec2 = point.pos.sub(switch (which) {
            .center => size.scale(0.5),
            else => @panic("TODO"),
        });
        return .{ .top_left = top_left, .size = size };
    }

    pub fn withAspectRatio(original: Rect, target_ratio: f32, mode: GrowOrShrink, keep: MeasureKind) Rect {
        const new_size = original.size.withAspectRatio(target_ratio, mode);
        return original.with(.{ .size = new_size }, keep);
    }

    test "withAspectRatio" {
        try expectApproxEqRel(Rect.fromCenterAndSize(.half, .new(2, 1)), Rect.unit.withAspectRatio(2, .grow, .center), 0.0001);
        try expectApproxEqRel(Rect.fromCenterAndSize(.half, .new(1, 0.5)), Rect.unit.withAspectRatio(2, .shrink, .center), 0.0001);
    }

    pub fn moveToBeInsideRect(original: Rect, bounds: Rect) Rect {
        assert(original.size.x <= bounds.size.x and
            original.size.y <= bounds.size.y);

        var result = original;
        inline for ([2]Side{ .left, .top }) |s| {
            if (original.getSide(s) < bounds.getSide(s)) {
                result = result.withSide(s, bounds.getSide(s));
            }
        }
        inline for ([2]Side{ .right, .bottom }) |s| {
            if (original.getSide(s) > bounds.getSide(s)) {
                result = result.withSide(s, bounds.getSide(s));
            }
        }
        return result;
    }

    test "moveToBeInsideRect" {
        const bounds: Rect = .fromCenterAndSize(.zero, .both(10));
        const movable: Rect = .fromCenterAndSize(.zero, .both(5));

        try Rect.expectApproxEqAbs(movable, movable.moveToBeInsideRect(bounds), 0.001);
    }

    pub fn getSide(self: Rect, side: Side) f32 {
        return switch (side) {
            .left => self.top_left.x,
            .right => self.top_left.x + self.size.x,
            .top => self.top_left.y,
            .bottom => self.top_left.y + self.size.y,
        };
    }

    pub fn withSide(original: Rect, which: Side, v: f32) Rect {
        return switch (which) {
            .left => .fromMeasureAndSizeV2(.top_left, .new(v, original.getSide(.top)), original.size),
            .right => .fromMeasureAndSizeV2(.top_right, .new(v, original.getSide(.top)), original.size),
            .top => .fromMeasureAndSizeV2(.top_left, .new(original.getSide(.left), v), original.size),
            .bottom => .fromMeasureAndSizeV2(.bottom_left, .new(original.getSide(.left), v), original.size),
        };
    }

    pub fn expectApproxEqRel(expected: Rect, actual: Rect, tolerance: anytype) !void {
        try Vec2.expectApproxEqRel(expected.top_left, actual.top_left, tolerance);
        try Vec2.expectApproxEqRel(expected.size, actual.size, tolerance);
    }

    pub fn expectApproxEqAbs(expected: Rect, actual: Rect, tolerance: anytype) !void {
        try Vec2.expectApproxEqAbs(expected.top_left, actual.top_left, tolerance);
        try Vec2.expectApproxEqAbs(expected.size, actual.size, tolerance);
    }

    /// renamed to worldFromLocal
    pub fn applyToLocalPosition(self: Rect, p: Vec2) Vec2 {
        return self.worldFromLocal(p);
    }

    /// takes p in the 0..1 range
    pub fn worldFromLocal(self: Rect, p: Vec2) Vec2 {
        return self.top_left.add(p.mul(self.size));
    }

    /// takes p in the -1..1 range
    pub fn worldFromCenterLocal(self: Rect, p: Vec2) Vec2 {
        return self.worldFromLocal(.new(
            remap(p.x, -1, 1, 0, 1),
            remap(p.y, -1, 1, 0, 1),
        ));
    }

    pub fn localFromWorldPosition(self: Rect, p: Vec2) Vec2 {
        return p.sub(self.top_left).div(self.size);
    }

    pub fn fromSpriteSheet(which: UVec2, count: UVec2, uv_margin: Vec2) Rect {
        const rect: Rect = .{
            .top_left = which.tof32().div(count.tof32()),
            .size = Vec2.one.div(count.tof32()),
        };
        return rect.plusMargin2(uv_margin.neg());
    }

    pub fn format(value: Rect, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
        std.debug.assert(std.mem.eql(u8, fmt, ""));
        std.debug.assert(std.meta.eql(options, .{}));
        try writer.print("Rect(tl: {any}, size: {any})", .{ value.top_left, value.size });
    }
};

pub const FColor = extern struct {
    r: f32,
    g: f32,
    b: f32,
    a: f32 = 1,

    pub const white = UColor.white.toFColor();
    pub const black = UColor.black.toFColor();
    pub const cyan = UColor.cyan.toFColor();
    pub const green = UColor.green.toFColor();
    pub const red = UColor.red.toFColor();
    pub const magenta = UColor.magenta.toFColor();

    pub fn new(r: f32, g: f32, b: f32) FColor {
        return .{ .r = r, .g = g, .b = b };
    }

    pub fn gray(v: f32) FColor {
        return new(v, v, v);
    }

    pub fn toArray(c: FColor) [4]f32 {
        return .{ c.r, c.g, c.b, c.a };
    }

    pub fn fromArray(arr: [4]f32) FColor {
        return .{ .r = arr[0], .g = arr[1], .b = arr[2], .a = arr[3] };
    }

    pub fn withAlpha(c: FColor, a: f32) FColor {
        return FColor{ .r = c.r, .g = c.g, .b = c.b, .a = a };
    }

    pub fn lerp(a: FColor, b: FColor, t: f32) FColor {
        return fromArray(.{
            std.math.lerp(a.r, b.r, t),
            std.math.lerp(a.g, b.g, t),
            std.math.lerp(a.b, b.b, t),
            std.math.lerp(a.a, b.a, t),
        });
    }

    pub fn fromHex(comptime str: []const u8) FColor {
        return UColor.fromHex(str).toFColor();
    }

    pub fn gradient(comptime steps: usize, comptime start: FColor, comptime end: FColor) [steps]FColor {
        const funk = @import("funktional.zig");
        return funk.map(struct {
            pub fn anon(t: f32) FColor {
                return FColor.lerp(start, end, t);
            }
        }.anon, &funk.linspace01(steps, true));
    }

    // TODO: use something fancier than HSV
    pub const HSV = struct {
        h: f32,
        s: f32,
        v: f32,
        a: f32,

        pub fn toRgb(hsv: HSV) FColor {
            if (hsv.s == 0.0) {
                return .{ .r = hsv.v, .g = hsv.v, .b = hsv.v, .a = hsv.a }; // achromatic (gray)
            }

            const h = hsv.h * 6.0;
            const i: u32 = @intFromFloat(std.math.floor(h));
            const f = h - @as(f32, @floatFromInt(i));
            const p = hsv.v * (1.0 - hsv.s);
            const q = hsv.v * (1.0 - hsv.s * f);
            const t = hsv.v * (1.0 - hsv.s * (1.0 - f));

            return switch (i % 6) {
                0 => .{ .r = hsv.v, .g = t, .b = p, .a = hsv.a },
                1 => .{ .r = q, .g = hsv.v, .b = p, .a = hsv.a },
                2 => .{ .r = p, .g = hsv.v, .b = t, .a = hsv.a },
                3 => .{ .r = p, .g = q, .b = hsv.v, .a = hsv.a },
                4 => .{ .r = t, .g = p, .b = hsv.v, .a = hsv.a },
                else => .{ .r = hsv.v, .g = p, .b = q, .a = hsv.a },
            };
        }
    };

    pub fn fromHsv(h: f32, s: f32, v: f32) FColor {
        return HSV.toRgb(.{ .h = h, .s = s, .v = v, .a = 1 });
    }

    pub fn toHsv(color: FColor) HSV {
        const max = @max(color.r, color.g, color.b);
        const min = @min(color.r, color.g, color.b);
        const delta = max - min;

        var h: f32 = 0.0;
        const s: f32 = if (max == 0.0) 0.0 else delta / max;
        const v: f32 = max;

        if (delta != 0.0) {
            if (max == color.r) {
                h = (color.g - color.b) / delta;
                if (color.g < color.b) h += 6.0;
            } else if (max == color.g) {
                h = (color.b - color.r) / delta + 2.0;
            } else {
                h = (color.r - color.g) / delta + 4.0;
            }
            h /= 6.0;
        }

        return .{ .h = h, .s = s, .v = v, .a = color.a };
    }

    pub fn scaleRGBClamped(color: FColor, v: f32) FColor {
        return .{
            .r = clamp01(color.r * v),
            .g = clamp01(color.g * v),
            .b = clamp01(color.b * v),
            .a = color.a,
        };
    }

    pub fn toUColor(color: FColor) UColor {
        return UColor.fromFColor(color);
    }

    // TODO
    pub fn lighter(color: FColor) FColor {
        return color.scaleRGBClamped(1.5);
        // const delta_s = 0.2;
        // const delta_v = 0.2;
        // const hsv = color.toHsv();
        // const new_v = std.math.clamp(hsv.v + delta_v, 0.0, 1.0);
        // const new_s = std.math.clamp(hsv.s + delta_s, 0.0, 1.0);
        // return (HSV{ .h = hsv.h, .s = new_s, .v = new_v, .a = color.a }).toRgb();
    }
};

pub const UColor = extern struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8 = 255,

    pub const white = new(255, 255, 255);
    pub const black = new(0, 0, 0);
    pub const cyan = new(0, 255, 255);
    pub const green = new(0, 255, 0);
    pub const red = new(255, 0, 0);
    pub const magenta = new(255, 0, 255);

    pub fn new(r: u8, g: u8, b: u8) UColor {
        return .{ .r = r, .g = g, .b = b };
    }

    pub fn from01(r: f32, g: f32, b: f32) UColor {
        assert(in01(r) and in01(g) and in01(b));
        return UColor.new(
            @intFromFloat(r * 255),
            @intFromFloat(g * 255),
            @intFromFloat(b * 255),
        );
    }

    pub fn fromHex(comptime str: []const u8) UColor {
        @setEvalBranchQuota(10000);
        const error_message = std.fmt.comptimePrint("bad format for str {s}", .{str});
        if (str.len != 7 or str[0] != '#') @compileError(error_message);
        errdefer @compileError(error_message);
        return comptime UColor{
            .r = try std.fmt.parseInt(u8, str[1..3], 16),
            .g = try std.fmt.parseInt(u8, str[3..5], 16),
            .b = try std.fmt.parseInt(u8, str[5..7], 16),
        };
    }

    pub fn fromHexAtRunTime(str: []const u8) !UColor {
        if (str.len != 7 or str[0] != '#') return error.BadHexCode;
        var it = std.mem.window(u8, str[1..], 2, 2);
        return UColor{
            .r = std.fmt.parseInt(u8, it.next().?, 16) catch return error.BadHexCode,
            .g = std.fmt.parseInt(u8, it.next().?, 16) catch return error.BadHexCode,
            .b = std.fmt.parseInt(u8, it.next().?, 16) catch return error.BadHexCode,
        };
    }

    pub fn gray(v: u8) UColor {
        return new(v, v, v);
    }

    pub fn withAlpha(c: UColor, a: u8) UColor {
        return UColor{ .r = c.r, .g = c.g, .b = c.b, .a = a };
    }

    pub fn withAlpha01(c: UColor, a: f32) UColor {
        return withAlpha(c, @intFromFloat(a * 255));
    }

    pub fn toFColor(c: UColor) FColor {
        return .{
            .r = tof32(c.r) / 255.0,
            .g = tof32(c.g) / 255.0,
            .b = tof32(c.b) / 255.0,
            .a = tof32(c.a) / 255.0,
        };
    }

    pub fn fromFColor(c: FColor) UColor {
        return .{
            .r = @intFromFloat(c.r * 255.0),
            .g = @intFromFloat(c.g * 255.0),
            .b = @intFromFloat(c.b * 255.0),
            .a = @intFromFloat(c.a * 255.0),
        };
    }

    pub fn lerp(a: UColor, b: UColor, t: f32) UColor {
        return fromFColor(.lerp(a.toFColor(), b.toFColor(), t));
    }

    pub fn equals(a: UColor, b: UColor) bool {
        return std.meta.eql(a, b);
    }

    pub fn gradient(comptime steps: usize, comptime start: UColor, comptime end: UColor) [steps]UColor {
        const funk = @import("funktional.zig");
        return funk.map(struct {
            pub fn anon(t: f32) UColor {
                return UColor.lerp(start, end, t);
            }
        }.anon, &funk.linspace01(steps, true));
    }
};

pub const Point = extern struct {
    pos: Vec2 = .zero,
    turns: f32 = 0,
    scale: f32 = 1,

    pub fn lerp(a: Point, b: Point, t: f32) Point {
        // TODO: properly handle rotation
        return .{
            .pos = Vec2.lerp(a.pos, b.pos, t),
            .scale = std.math.lerp(a.scale, b.scale, t),
            .turns = std.math.lerp(a.turns, b.turns, t),
        };
    }

    pub fn lerp_towards(self: *Point, goal: Point, ratio: f32, delta_seconds: f32) void {
        lerp_towards_float(&self.pos.x, goal.pos.x, ratio, delta_seconds);
        lerp_towards_float(&self.pos.y, goal.pos.y, ratio, delta_seconds);
        lerp_towards_float(&self.turns, goal.turns, ratio, delta_seconds);
        lerp_towards_float(&self.scale, goal.scale, ratio, delta_seconds);
    }

    pub fn applyToLocalPosition(parent: Point, local: Vec2) Vec2 {
        return local.scale(parent.scale).rotate(parent.turns).add(parent.pos);
    }

    pub fn applyToLocalPoint(parent: Point, local: Point) Point {
        return .{
            .pos = parent.applyToLocalPosition(local.pos),
            .scale = parent.scale * local.scale,
            .turns = parent.turns + local.turns,
        };
    }

    pub fn expectApproxEqRel(expected: Point, actual: Point, tolerance: anytype) !void {
        try std.testing.expectApproxEqRel(expected.scale, actual.scale, tolerance);
        try std.testing.expectApproxEqRel(expected.turns, actual.turns, tolerance);
        try Vec2.expectApproxEqRel(expected.pos, actual.pos, tolerance);
    }

    pub fn expectApproxEqAbs(expected: Point, actual: Point, tolerance: anytype) !void {
        try std.testing.expectApproxEqAbs(expected.scale, actual.scale, tolerance);
        try std.testing.expectApproxEqAbs(expected.turns, actual.turns, tolerance);
        try Vec2.expectApproxEqAbs(expected.pos, actual.pos, tolerance);
    }

    // TODO: document these
    pub fn inverseApplyToLocalPoint(applied: Point, local: Point) Point {
        const scale = applied.scale / local.scale;
        const turns = applied.turns - local.turns;
        return .{
            .pos = applied.pos.sub(local.pos.scale(scale).rotate(turns)),
            .scale = scale,
            .turns = turns,
        };
    }

    pub fn inverseApplyGetLocal(parent: Point, applied: Point) Point {
        return .{
            .pos = applied.pos.sub(parent.pos).rotate(-parent.turns).scale(1 / parent.scale),
            .scale = applied.scale / parent.scale,
            .turns = applied.turns - parent.turns,
        };
    }

    test "inverse apply" {
        const parent: Point = .{ .pos = .zero, .scale = 2, .turns = 0.25 };
        const local: Point = .{ .pos = .e1 };
        const applied = parent.applyToLocalPoint(local);
        try expectApproxEqAbs(.{ .pos = .new(0, 2), .scale = 2, .turns = 0.25 }, applied, 0.0001);
        try expectApproxEqAbs(parent, applied.inverseApplyToLocalPoint(local), 0.0001);
        try expectApproxEqAbs(local, parent.inverseApplyGetLocal(applied), 0.0001);
    }

    pub fn inverseApplyGetLocalPosition(parent: Point, applied: Vec2) Vec2 {
        return inverseApplyGetLocal(parent, .{ .pos = applied }).pos;
    }
};

// TODO: delete this and move the stuff into Rect
pub const Camera = struct {
    pub const aspect_ratio: f32 = 16.0 / 9.0;

    center: Vec2,
    /// how many world units fit between the top and bottom of the camera view
    height: f32,

    pub fn fromTopleftAndHeight(top_left: Vec2, height: f32) Camera {
        return .{ .center = top_left.add(
            Vec2.new(aspect_ratio, 1).scale(height).scale(0.5),
        ), .height = height };
    }

    pub fn toRect(self: Camera) Rect {
        const size = Vec2.new(self.height * aspect_ratio, self.height);
        const top_left = self.center.sub(size.scale(0.5));
        return Rect{ .top_left = top_left, .size = size };
    }

    pub fn fromRect(rect: Rect) Camera {
        assert(std.math.approxEqRel(f32, rect.size.y, rect.size.x * aspect_ratio, 0.001));
        return fromTopleftAndHeight(rect.top_left, rect.size.y);
    }

    pub fn lerp(a: Camera, b: Camera, t: f32) Camera {
        return Camera{
            .center = Vec2.lerp(a.center, b.center, t),
            .height = std.math.lerp(a.height, b.height, t),
        };
    }

    pub fn remap(old_cam: Camera, old_pos: Point, new_cam: Camera) Point {
        return worldFromScreen(new_cam, old_cam.screenFromWorld(old_pos));
    }

    /// relative_screen_pos is in ([0..aspect_ratio], [0..1])
    pub fn worldFromScreenPosition(self: Camera, relative_screen_pos: Vec2) Vec2 {
        const rect = self.toRect();
        return rect.top_left.add(relative_screen_pos.scale(self.height));
    }

    /// relative_screen_point.pos is in ([0..aspect_ratio], [0..1])
    pub fn worldFromScreen(camera: Camera, relative_screen_point: Point) Point {
        return .{
            .pos = camera.worldFromScreenPosition(relative_screen_point.pos),
            .scale = relative_screen_point.scale * camera.height,
            .turns = relative_screen_point.turns,
        };
    }

    /// assumes the screen as a height of 1
    pub fn screenFromWorld(camera: Camera, world_point: Point) Point {
        const window_height = 1;
        const rect = camera.toRect();
        const local = Point.inverseApplyGetLocal(Point{
            .pos = rect.top_left,
            .scale = rect.size.y,
        }, world_point);
        const screen = Point{ .pos = .zero, .scale = window_height };
        return screen.applyToLocalPoint(local);
    }

    pub fn screenFromWorldPosition(camera: Camera, world_position: Vec2) Vec2 {
        return screenFromWorld(camera, .{ .pos = world_position }).pos;
    }

    test "convert between screen and world" {
        // TODO: maybe change to fuzz
        var rnd_state = std.Random.DefaultPrng.init(std.testing.random_seed);
        var rnd = Random.init(rnd_state.random());

        const big_rect = Rect{ .top_left = .both(1000), .size = .both(2000) };

        const camera = Camera.fromTopleftAndHeight(
            rnd.inRect(big_rect),
            rnd.between(0.01, 100),
        );

        const world_pos = rnd.inRect(big_rect);
        try Vec2.expectApproxEqAbs(
            world_pos,
            camera.worldFromScreenPosition(camera.screenFromWorldPosition(world_pos)),
            0.001,
        );

        const screen_pos = rnd.inRect(big_rect);
        try Vec2.expectApproxEqAbs(
            screen_pos,
            camera.screenFromWorldPosition(camera.worldFromScreenPosition(screen_pos)),
            0.001,
        );
    }

    pub fn zoom(original: Camera, fixed_world_pos: Vec2, new_height: f32) Camera {
        const fixed_screen_pos = original.screenFromWorldPosition(fixed_world_pos);
        return Camera.fromMapping(
            .{ .pos = fixed_world_pos, .scale = 1.0 },
            .{ .pos = fixed_screen_pos, .scale = 1.0 / new_height },
        );
    }

    /// Transform the camera such that the changed screen pos corresponds to the same world pos
    pub fn drag(original: Camera, old_screen_pos: Vec2, new_screen_pos: Vec2) Camera {
        return Camera.fromMapping(
            original.worldFromScreen(.{ .pos = old_screen_pos }),
            .{ .pos = new_screen_pos, .scale = 1 },
        );
    }

    pub fn fromMapping(source_world_point: Point, target_screen_point: Point) Camera {
        const camera_height = source_world_point.scale / target_screen_point.scale;
        const camera_top_left = source_world_point.pos.sub(target_screen_point.pos.scale(camera_height));

        return Camera.fromTopleftAndHeight(
            camera_top_left,
            camera_height,
        );
    }

    test "fromMapping" {
        // TODO: maybe change to fuzz
        var rnd_state = std.Random.DefaultPrng.init(std.testing.random_seed);
        var rnd = Random.init(rnd_state.random());

        const big_rect = Rect{ .top_left = .both(1000), .size = .both(2000) };

        const world_point = Point{
            .pos = rnd.inRect(big_rect),
            .scale = rnd.between(0.01, 100),
        };
        const screen_point = Point{
            .pos = rnd.inRect(big_rect),
            .scale = rnd.between(0.01, 100),
        };

        const camera = Camera.fromMapping(world_point, screen_point);

        try Point.expectApproxEqRel(
            screen_point,
            camera.screenFromWorld(world_point),
            0.001,
        );
    }

    test "zoom" {
        var rnd_state = std.Random.DefaultPrng.init(std.testing.random_seed);
        var rnd = Random.init(rnd_state.random());

        const big_rect = Rect{ .top_left = .both(1000), .size = .both(2000) };

        const original_camera = Camera.fromTopleftAndHeight(
            rnd.inRect(big_rect),
            rnd.between(0.01, 100),
        );
        const fixed_world_position = rnd.inRect(big_rect);
        const new_height = rnd.between(0.01, 100);

        const new_camera = original_camera.zoom(fixed_world_position, new_height);

        try Vec2.expectApproxEqRel(
            original_camera.screenFromWorldPosition(fixed_world_position),
            new_camera.screenFromWorldPosition(fixed_world_position),
            0.001,
        );

        try std.testing.expectApproxEqRel(new_height, new_camera.height, 0.001);
    }
};

pub const Random = struct {
    rnd: std.Random,

    pub fn init(rnd: std.Random) Random {
        return .{ .rnd = rnd };
    }

    pub fn alphanumeric_bytes(this: Random, buf: []u8) void {
        this.rnd.bytes(buf);
        for (buf) |*v| {
            while (!std.ascii.isAlphanumeric(v.*)) {
                v.* = this.rnd.int(u8);
            }
        }
    }

    pub fn between(this: Random, at_least: f32, less_than: f32) f32 {
        return this.rnd.float(f32) * (less_than - at_least) + at_least;
    }

    pub fn inRect(this: Random, rect: Rect) Vec2 {
        return .new(
            this.between(rect.top_left.x, rect.top_left.x + rect.size.x),
            this.between(rect.top_left.y, rect.top_left.y + rect.size.y),
        );
    }

    pub fn inURect(this: Random, rect: URect) UVec2 {
        return .new(
            rect.top_left.x + this.rnd.intRangeLessThan(usize, 0, rect.inner_size.x),
            rect.top_left.y + this.rnd.intRangeLessThan(usize, 0, rect.inner_size.y),
        );
    }

    pub fn around0(this: Random, radius: f32) f32 {
        return this.between(-radius, radius);
    }

    pub fn direction(this: Random) Vec2 {
        return Vec2.e1.rotate(this.rnd.float(f32));
    }

    // TODO: better
    pub fn direction3D(this: Random) Vec3 {
        return Vec3.new(
            this.between(-1, 1),
            this.between(-1, 1),
            this.between(-1, 1),
        ).normalized();
    }

    pub fn color(this: Random) UColor {
        return UColor.new(
            this.rnd.int(u8),
            this.rnd.int(u8),
            this.rnd.int(u8),
        );
    }

    pub fn fcolor(this: Random) FColor {
        return FColor.new(
            this.rnd.float(f32),
            this.rnd.float(f32),
            this.rnd.float(f32),
        );
    }
};

// https://easings.net/
pub const easings = struct {
    pub fn linear(t: f32) f32 {
        return t;
    }

    pub fn easeInOutCubic(x: f32) f32 {
        return if (x < 0.5) 4 * x * x * x else 1 - std.math.pow(f32, -2 * x + 2, 3) / 2;
    }

    pub fn easeOutCubic(x: f32) f32 {
        return 1 - std.math.pow(f32, 1 - x, 3);
    }

    pub fn easeInCubic(x: f32) f32 {
        return x * x * x;
    }

    pub fn easeInQuad(x: f32) f32 {
        return x * x;
    }
};

pub fn smoothstepEased(x: f32, edge0: f32, edge1: f32, comptime easing: std.meta.DeclEnum(easings)) f32 {
    const easing_fn = @field(easings, @tagName(easing));
    return easing_fn(clamp01(inverse_lerp(edge0, edge1, x)));
}

// pub fn segmentCircleIntersection(segment_a: Vec2, segment_b: Vec2, circle_center: Vec2, circle_radius: f32) ?Vec2 {
pub fn lineCircleIntersection(line_pos: Vec2, line_dir: Vec2, circle_center: Vec2, circle_radius: f32) ?f32 {
    // TODO: revise
    const a = line_dir.x * line_dir.x + line_dir.y * line_dir.y;
    const b = 2 * (line_dir.x * (line_pos.x - circle_center.x) + line_dir.y * (line_pos.y - circle_center.y));
    const c = (line_pos.x - circle_center.x) * (line_pos.x - circle_center.x) +
        (line_pos.y - circle_center.y) * (line_pos.y - circle_center.y) -
        circle_radius * circle_radius;

    const discriminant = b * b - 4 * a * c;

    if (discriminant < 0) {
        return null; // no intersection
    }

    const sqrt_discriminant = std.math.sqrt(discriminant);
    const t1 = (-b + sqrt_discriminant) / (2 * a);
    const t2 = (-b - sqrt_discriminant) / (2 * a);

    // Return the closest intersection point
    return @min(t1, t2);
}

pub fn projectPointOnLine(p: Vec2, line_origin: Vec2, line_dir: Vec2) f32 {
    assert(std.math.approxEqAbs(f32, line_dir.mag(), 1.0, 0.001));
    const line_to_point = p.sub(line_origin);
    const projection_length = line_to_point.dot(line_dir);
    return projection_length / line_dir.mag();
}

test "projectPointOnLine" {
    try std.testing.expectEqual(1.5, projectPointOnLine(.new(3.5, 2), .new(2, 2), .new(1, 0)));
}
