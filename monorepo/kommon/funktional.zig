const std = @import("std");

fn indexOfScalar(comptime T: type, slice: []const T, value: T, comptime eql: ?fn (a: T, b: T) bool) ?usize {
    const equal = eql orelse std.meta.eql;
    for (slice, 0..) |c, j| {
        if (equal(c, value)) return j;
    }
    return null;
}

pub fn indexOfString(slice: []const []const u8, value: []const u8) ?usize {
    return indexOfScalar([]const u8, slice, value, struct {
        // TODO: currying helper
        pub fn anon(a: []const u8, b: []const u8) bool {
            return std.mem.eql(u8, a, b);
        }
    }.anon);
}

pub fn single(arr: anytype) std.meta.Elem(@TypeOf(arr)) {
    std.debug.assert(arr.len == 1);
    return arr[0];
}

pub fn map(comptime map_fn: anytype, comptime in: []const SingleInputOf(map_fn)) [in.len]ReturnOf(map_fn) {
    var result: [in.len]ReturnOf(map_fn) = undefined;
    for (in, &result) |v, *target| {
        target.* = map_fn(v);
    }
    return result;
}

fn SingleInputOf(map_fn: anytype) type {
    return single(@typeInfo(@TypeOf(map_fn)).@"fn".params).type.?;
}

pub fn ReturnOf(map_fn: anytype) type {
    return @typeInfo(@TypeOf(map_fn)).@"fn".return_type.?;
}

pub fn mapWithIndex(comptime map_fn: anytype, comptime in: []const FirstInputOf(map_fn)) [in.len]ReturnOf(map_fn) {
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params.len == 2);
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params[1].type.? == usize);
    var result: [in.len]ReturnOf(map_fn) = undefined;
    for (in, &result, 0..) |v, *target, k| {
        target.* = map_fn(v, k);
    }
    return result;
}

fn FirstInputOf(map_fn: anytype) type {
    return @typeInfo(@TypeOf(map_fn)).@"fn".params[0].type.?;
}

fn SecondInputOf(map_fn: anytype) type {
    return @typeInfo(@TypeOf(map_fn)).@"fn".params[1].type.?;
}

fn ParamsLen(map_fn: anytype) usize {
    return @typeInfo(@TypeOf(map_fn)).@"fn".params.len;
}

pub fn fromCount(comptime n: usize, comptime map_fn: anytype) [n]ReturnOf(map_fn) {
    @setEvalBranchQuota(n * 100);
    std.debug.assert(SingleInputOf(map_fn) == usize);
    var result: [n]ReturnOf(map_fn) = undefined;
    for (0..n, &result) |k, *target| {
        target.* = map_fn(k);
    }
    return result;
}

pub fn fromCountAndCtx(comptime n: usize, comptime map_fn: anytype, ctx: SecondInputOf(map_fn)) [n]ReturnOf(map_fn) {
    std.debug.assert(FirstInputOf(map_fn) == usize);
    std.debug.assert(ParamsLen(map_fn) == 2);
    var result: [n]ReturnOf(map_fn) = undefined;
    for (0..n, &result) |k, *target| {
        target.* = map_fn(k, ctx);
    }
    return result;
}

pub fn mapWithIndexAndCtx(comptime map_fn: anytype, comptime in: []const FirstInputOf(map_fn), ctx: anytype) [in.len]ReturnOf(map_fn) {
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params.len == 3);
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params[1].type.? == usize);
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params[2].type.? == @TypeOf(ctx));
    var result: [in.len]ReturnOf(map_fn) = undefined;
    for (in, &result, 0..) |v, *target, k| {
        target.* = map_fn(v, k, ctx);
    }
    return result;
}

pub fn mapWithCtx(comptime map_fn: anytype, comptime in: []const SecondInputOf(map_fn), ctx: FirstInputOf(map_fn)) [in.len]ReturnOf(map_fn) {
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params.len == 2);
    var result: [in.len]ReturnOf(map_fn) = undefined;
    for (in, &result) |v, *target| {
        target.* = map_fn(ctx, v);
    }
    return result;
}

pub fn mapOOP(
    ctx: anytype,
    comptime map_fn_name: std.meta.DeclEnum(@TypeOf(ctx)),
    comptime in: []const SecondInputOf(@field(@TypeOf(ctx), @tagName(map_fn_name))),
) [in.len]ReturnOf(@field(@TypeOf(ctx), @tagName(map_fn_name))) {
    const map_fn = @field(@TypeOf(ctx), @tagName(map_fn_name));
    std.debug.assert(@typeInfo(@TypeOf(map_fn)).@"fn".params.len == 2);
    std.debug.assert(FirstInputOf(map_fn) == @TypeOf(ctx));
    var result: [in.len]ReturnOf(map_fn) = undefined;
    for (in, &result) |v, *target| {
        target.* = map_fn(ctx, v);
    }
    return result;
}

pub fn allOOP(
    values: anytype,
    comptime field_name: std.meta.FieldEnum(std.meta.Elem(@TypeOf(values))),
) bool {
    for (values) |value| {
        if (!@field(value, @tagName(field_name))) return false;
    } else return true;
}

pub fn linspace01(comptime n_steps: usize, comptime include_endpoint: bool) [n_steps]f32 {
    return linspace(0, 1, n_steps, include_endpoint);
}

pub fn linspace(comptime min: f32, comptime max: f32, comptime n_steps: usize, comptime include_endpoint: bool) [n_steps]f32 {
    const denominator: f32 = @floatFromInt(if (include_endpoint) (n_steps - 1) else n_steps);
    return fromCount(n_steps, struct {
        pub fn anon(n: usize) f32 {
            return std.math.lerp(min, max, kommon.math.tof32(n) / denominator);
        }
    }.anon);
}

pub fn sum(comptime T: type, values: []const T) T {
    var result: T = 0;
    for (values) |v| {
        result += v;
    }
    return result;
}

pub fn concatComptime(comptime strs: []const []const u8) []const u8 {
    var result: []const u8 = "";
    for (strs) |s| {
        result = result ++ s;
    }
    return result;
}

pub fn Fn(params: []const std.builtin.Type.Fn.Param, return_type: type) type {
    return @Type(.{
        .@"fn" = .{
            .params = params,
            .return_type = return_type,
            // not confident in these values
            .calling_convention = .auto,
            .is_generic = false,
            .is_var_args = false,
        },
    });
}

pub fn chain(comptime functions: anytype) Fn(
    @typeInfo(@TypeOf(functions[0])).@"fn".params,
    ReturnOf(functions[functions.len - 1]),
) {
    var intermediate_types: [functions.len]type = undefined;
    inline for (functions, 0..) |f, k| {
        intermediate_types[k] = ReturnOf(f);
    }
    const intermediate_values_type = std.meta.Tuple(&intermediate_types);

    if (ParamsLen(functions[0]) == 0) {
        const S = struct {
            pub fn anon() ReturnOf(functions[functions.len - 1]) {
                var values: intermediate_values_type = undefined;
                inline for (functions, 0..) |f, k| {
                    if (k == 0) {
                        values[0] = f();
                    } else {
                        values[k] = f(values[k - 1]);
                    }
                }
                return values[functions.len - 1];
            }
        };
        return S.anon;
    } else if (ParamsLen(functions[1]) == 1) {
        @compileError("TODO");
    } else {
        @compileError("nope");
    }
}

// from joseph mansfield
pub fn Closure(
    comptime f: anytype,
    comptime Capture: type,
) type {
    return struct {
        const Self = @This();
        capture: Capture,
        pub fn call(self: Self, args: anytype) @typeInfo(@TypeOf(f)).Fn.return_type.? {
            return @call(.auto, f, self.capture ++ args);
        }
    };
}

pub fn closure(
    comptime f: anytype,
    capture: anytype,
) Closure(f, @TypeOf(capture)) {
    return Closure(f, @TypeOf(capture)){ .capture = capture };
}

const kommon = @import("./kommon.zig");
