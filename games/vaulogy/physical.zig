pub const PhysicalSexpr = struct {
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

pub const ViewHelper = struct {
    pub fn overlapsTemplateAtom(atom_point: Point, needle_pos: Vec2, kind: enum { atom, pair }) bool {
        const p = atom_point.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;
        return inRange(p.y, -1, 1) and switch (kind) {
            .pair => inRange(p.x, -0.5 * (1 - @abs(p.y)), 0.5 - 0.25 * (1 - @abs(@abs(p.y) - 0.5) / 0.5)),
            .atom => inRange(p.x, -0.5 * (1 - @abs(p.y)), 2),
        };
    }

    pub fn overlapsPatternAtom(atom_point: Point, needle_pos: Vec2, kind: enum { atom, pair }) bool {
        const p = atom_point.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;
        return inRange(p.y, -1, 1) and switch (kind) {
            .atom => inRange(p.x, -1, 0.5 * (1 - @abs(p.y))),
            .pair => inRange(p.x, -1 + 0.25 * (1 - @abs(@abs(p.y) - 0.5) / 0.5), 0.5 * (1 - @abs(p.y))),
        };
    }

    pub fn overlapsTemplateSexpr(alloc: std.mem.Allocator, sexpr: *const Sexpr, sexpr_pos: Point, needle_pos: Vec2) !?core.SexprAddress {
        var result = std.ArrayList(core.SexprAddressItem).init(alloc);
        defer result.deinit();
        // TODO (low priority): probably can be made more efficient by using less changes of coordinates

        var cur_sexpr_pos = sexpr_pos;
        var cur_sexpr = sexpr;
        while (true) {
            switch (cur_sexpr.*) {
                .atom_lit, .atom_var => {
                    if (overlapsTemplateAtom(cur_sexpr_pos, needle_pos, .atom)) {
                        return try result.toOwnedSlice();
                    } else {
                        return null;
                    }
                },
                .pair => |pair| {
                    const p = cur_sexpr_pos.inverseApplyGetLocal(.{ .pos = needle_pos }).pos;

                    if (overlapsTemplateAtom(cur_sexpr_pos, needle_pos, .pair)) {
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

    pub fn sexprTemplateChildView(parent: Point, address: core.SexprAddress) Point {
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

    pub const OFFSET_TEMPLATE_PAIR_LEFT: Point = .{ .pos = .new(0.5, -0.5), .scale = 0.5 };
    pub const OFFSET_TEMPLATE_PAIR_RIGHT: Point = .{ .pos = .new(0.5, 0.5), .scale = 0.5 };
    pub const OFFSET_PATTERN_PAIR_LEFT: Point = .{ .pos = .new(-1, -0.5), .scale = 0.5 };
    pub const OFFSET_PATTERN_PAIR_RIGHT: Point = .{ .pos = .new(-1, 0.5), .scale = 0.5 };
};

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
