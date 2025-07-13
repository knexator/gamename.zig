// TODO: maybe use @Vector(3, i32)
pub const Hex = struct {
    q: i32,
    r: i32,
    s: i32,

    pub const zero = new(0, 0, 0);
    pub const directions: [6]Hex = .{
        .new(1, 0, -1), .new(1, -1, 0), .new(0, -1, 1),
        .new(-1, 0, 1), .new(-1, 1, 0), .new(0, 1, -1),
    };
    pub const diagonals: [6]Hex = .{
        .new(2, -1, -1), .new(1, -2, 1),  .new(-1, -1, 2),
        .new(-2, 1, 1),  .new(-1, 2, -1), .new(1, 1, -2),
    };

    pub fn new(q: i32, r: i32, s: i32) Hex {
        std.debug.assert(q + r + s == 0);
        return .{ .q = q, .r = r, .s = s };
    }

    pub fn fromQR(q: i32, r: i32) Hex {
        return new(q, r, -q - r);
    }

    pub fn len(self: Hex) u32 {
        return @divExact(@abs(self.q) + @abs(self.r) + @abs(self.s), 2);
    }

    pub fn add(self: Hex, other: Hex) Hex {
        return new(self.q + other.q, self.r + other.r, self.s + other.s);
    }

    pub fn sub(self: Hex, other: Hex) Hex {
        return new(self.q - other.q, self.r - other.r, self.s - other.s);
    }

    pub fn scale(self: Hex, k: i32) Hex {
        return new(self.q * k, self.r * k, self.s * k);
    }

    pub fn neighbor(self: Hex, direction: usize) Hex {
        return self.add(Hex.directions[direction]);
    }

    pub fn fract(self: Hex) HexFract {
        return .new(
            @floatFromInt(self.q),
            @floatFromInt(self.r),
            @floatFromInt(self.s),
        );
    }
};

pub const HexFract = struct {
    q: f32,
    r: f32,
    s: f32,

    pub fn new(q: f32, r: f32, s: f32) HexFract {
        std.debug.assert(std.math.approxEqAbs(f32, 0, q + r + s, 0.0001));
        return .{ .q = q, .r = r, .s = s };
    }

    pub fn round(this: HexFract) Hex {
        var qi: f32 = std.math.round(this.q);
        var ri: f32 = std.math.round(this.r);
        var si: f32 = std.math.round(this.s);
        const q_diff: f32 = @abs(qi - this.q);
        const r_diff: f32 = @abs(ri - this.r);
        const s_diff: f32 = @abs(si - this.s);
        if (q_diff > r_diff and q_diff > s_diff) {
            qi = -ri - si;
        } else if (r_diff > s_diff) {
            ri = -qi - si;
        } else {
            si = -qi - ri;
        }
        return .new(
            @intFromFloat(qi),
            @intFromFloat(ri),
            @intFromFloat(si),
        );
    }
};

const Iterator = @import("itertools.zig").Iterator;
const iteratorLen = @import("itertools.zig").iteratorLen;

pub const RingIterator = struct {
    radius: usize,
    cur_hex: Hex,
    i: Iterator(std.math.IntFittingRange(0, 5)),
    j: Iterator(usize),

    pub fn init(radius: usize) RingIterator {
        return .{
            .radius = radius,
            .cur_hex = Hex.directions[4].scale(@intCast(radius)),
            .i = .init(0, 5),
            .j = .init(0, if (radius > 0) radius - 1 else 0),
        };
    }

    pub fn next(self: *RingIterator) ?Hex {
        // special case
        if (self.radius == 0) {
            if (!self.i.done) {
                self.i.done = true;
                return .zero;
            } else {
                return null;
            }
        }

        // ideal:
        // for (0..6) |i| {
        //     for (0..self.radius) |j| {
        //         yield cur_hex;
        //         cur_hex = cur_hex.neighbor(i)
        //     }
        // }

        if (self.i.cur()) |i| {
            if (self.j.next()) |_| {
                const hex = self.cur_hex;
                self.cur_hex = self.cur_hex.neighbor(i);
                return hex;
            } else {
                self.i.advance();
                self.j.reset();
                return self.next();
            }
        } else {
            return null;
        }
    }

    test "ring 0" {
        var it: RingIterator = .init(0);
        try std.testing.expectEqual(Hex.zero, it.next().?);
        try std.testing.expectEqual(null, it.next());
    }

    test "ring 1" {
        const it: RingIterator = .init(1);
        try std.testing.expectEqual(6, iteratorLen(it));
    }

    test "ring 2" {
        const it: RingIterator = .init(2);
        try std.testing.expectEqual(12, iteratorLen(it));
    }

    test "ring 3" {
        const it: RingIterator = .init(3);
        try std.testing.expectEqual(18, iteratorLen(it));
    }
};

pub const SpiralIterator = struct {
    cur_ring: RingIterator = .init(0),

    pub const init: SpiralIterator = .{};
    pub fn next(self: *SpiralIterator) Hex {
        if (self.cur_ring.next()) |hex| {
            return hex;
        } else {
            self.cur_ring = .init(self.cur_ring.radius + 1);
            return self.next();
        }
    }
};

// TODO: revise Orientation & Layout
const Orientation = struct {
    f0: f32,
    f1: f32,
    f2: f32,
    f3: f32,
    b0: f32,
    b1: f32,
    b2: f32,
    b3: f32,
    start_angle: f32,

    pub const pointy: Orientation = .{
        .f0 = @sqrt(3.0),
        .f1 = @sqrt(3.0) / 2.0,
        .f2 = 0.0,
        .f3 = 3.0 / 2.0,
        .b0 = @sqrt(3.0) / 3.0,
        .b1 = -1.0 / 3.0,
        .b2 = 0.0,
        .b3 = 2.0 / 3.0,
        .start_angle = 0.5,
    };
    // pub const flat: Orientation = .{3.0 / 2.0, 0.0, @sqrt(3.0) / 2.0, @sqrt(3.0), 2.0 / 3.0, 0.0, -1.0 / 3.0, @sqrt(3.0) / 3.0, 0.0,};
};

pub const Layout = struct {
    orientation: Orientation,
    size: Vec2,
    origin: Vec2,

    pub fn pointPixelOffset(h: Hex, size: f32, origin: Vec2) Vec2 {
        const M: Orientation = Orientation.pointy;
        const x: f32 = (M.f0 * tof32(h.q) + M.f1 * tof32(h.r));
        const y: f32 = (M.f2 * tof32(h.q) + M.f3 * tof32(h.r));
        return Vec2.new(x, y).scale(size).add(origin);
    }

    pub fn hexToPixel(self: Layout, h: HexFract) Vec2 {
        const M = self.orientation;
        return self.origin.add(Vec2.new(
            M.f0 * h.q + M.f1 * h.r,
            M.f2 * h.q + M.f3 * h.r,
        ).mul(self.size));
    }

    pub fn pixelToHex(self: Layout, p: Vec2) HexFract {
        const M: Orientation = self.orientation;
        const size: Vec2 = self.size;
        const origin: Vec2 = self.origin;
        const pt: Vec2 = .new((p.x - origin.x) / size.x, (p.y - origin.y) / size.y);
        const q: f32 = M.b0 * pt.x + M.b1 * pt.y;
        const r: f32 = M.b2 * pt.x + M.b3 * pt.y;
        return .new(q, r, -q - r);
    }

    // public hexCornerOffset(corner: number): Point {
    //     var M: Orientation = this.orientation;
    //     var size: Point = this.size;
    //     var angle: number = 2.0 * Math.PI * (M.start_angle - corner) / 6.0;
    //     return new Point(size.x * Math.cos(angle), size.y * Math.sin(angle));
    // }

    // public polygonCorners(h: Hex): Point[] {
    //     var corners: Point[] = [];
    //     var center: Point = this.hexToPixel(h);
    //     for (var i = 0; i < 6; i++) {
    //         var offset: Point = this.hexCornerOffset(i);
    //         corners.push(new Point(center.x + offset.x, center.y + offset.y));
    //     }
    //     return corners;
    // }

};

pub const DoubledCoord = struct {
    col: i32,
    row: i32,

    pub fn new(col: i32, row: i32) DoubledCoord {
        return .{ .col = col, .row = row };
    }

    // public static qdoubledFromCube(h: Hex): DoubledCoord {
    //     var col: number = h.q;
    //     var row: number = 2 * h.r + h.q;
    //     return new DoubledCoord(col, row);
    // }

    // public qdoubledToCube(): Hex {
    //     var q: number = this.col;
    //     var r: number = (this.row - this.col) / 2;
    //     var s: number = -q - r;
    //     return new Hex(q, r, s);
    // }

    // public static rdoubledFromCube(h: Hex): DoubledCoord {
    //     var col: number = 2 * h.q + h.r;
    //     var row: number = h.r;
    //     return new DoubledCoord(col, row);
    // }

    pub fn rdoubledToCube(self: DoubledCoord) Hex {
        const q: i32 = @divExact(self.col - self.row, 2);
        const r: i32 = self.row;
        return .fromQR(q, r);
    }
};

const Vec2 = @import("math.zig").Vec2;
const tof32 = @import("math.zig").tof32;

const std = @import("std");
