/// similar to std.EnumSet, oops
pub fn BoolFlags(fields: type, @"packed": bool) type {
    return StructFromEnum(fields, bool, @"packed");
}

test "BoolFlags" {
    const Foo = enum { a, b, c };
    const Expected = struct {
        a: bool,
        b: bool,
        c: bool,
    };
    const Actual = BoolFlags(Foo, false);

    comptime try std.testing.expectEqualDeep(@typeInfo(Expected), @typeInfo(Actual));
}

pub fn StructFromEnum(fields: type, T: type, @"packed": bool) type {
    const enum_info = @typeInfo(fields).@"enum";
    assert(enum_info.is_exhaustive);

    return @Type(.{ .@"struct" = .{
        .layout = if (@"packed") .@"packed" else .auto,
        .backing_integer = null,
        .is_tuple = false,
        .decls = &.{},
        .fields = &funktional.map(struct {
            pub fn anon(f: std.builtin.Type.EnumField) std.builtin.Type.StructField {
                return .{
                    .name = f.name,
                    .type = T,
                    .default_value_ptr = null,
                    .is_comptime = false,
                    .alignment = 0,
                };
            }
        }.anon, enum_info.fields),
    } });
}

const std = @import("std");
const assert = std.debug.assert;

const funktional = @import("kommon.zig").funktional;
