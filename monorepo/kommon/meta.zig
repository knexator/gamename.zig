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

pub fn initDefaultFields(comptime T: type) T {
    switch (@typeInfo(T)) {
        .@"struct" => |struct_info| {
            var value: T = undefined;

            inline for (struct_info.fields) |field| {
                if (field.is_comptime) {
                    continue;
                }

                if (field.defaultValue()) |val| {
                    @field(value, field.name) = val;
                }
            }

            return value;
        },
        else => {
            @compileError("Can't default init a " ++ @typeName(T));
        },
    }
}

/// Compares two of any type for equality. Containers that do not support comparison
/// on their own are compared on a field-by-field basis. Pointers **are** followed.
pub fn eql(a: anytype, b: @TypeOf(a)) bool {
    const T = @TypeOf(a);

    switch (@typeInfo(T)) {
        .@"struct" => |info| {
            if (info.layout == .@"packed") return a == b;

            inline for (info.fields) |field_info| {
                if (!eql(@field(a, field_info.name), @field(b, field_info.name))) return false;
            }
            return true;
        },
        .error_union => {
            if (a) |a_p| {
                if (b) |b_p| return eql(a_p, b_p) else |_| return false;
            } else |a_e| {
                if (b) |_| return false else |b_e| return a_e == b_e;
            }
        },
        .@"union" => |info| {
            if (info.tag_type) |UnionTag| {
                const tag_a: UnionTag = a;
                const tag_b: UnionTag = b;
                if (tag_a != tag_b) return false;

                return switch (a) {
                    inline else => |val, tag| return eql(val, @field(b, @tagName(tag))),
                };
            }

            @compileError("cannot compare untagged union type " ++ @typeName(T));
        },
        .array => {
            if (a.len != b.len) return false;
            for (a, 0..) |e, i|
                if (!eql(e, b[i])) return false;
            return true;
        },
        .vector => |info| {
            var i: usize = 0;
            while (i < info.len) : (i += 1) {
                if (!eql(a[i], b[i])) return false;
            }
            return true;
        },
        .pointer => |info| {
            return switch (info.size) {
                .one => a == b or eql(a.*, b.*),
                .many, .c => @compileError("Many/C pointers not supported"),
                .slice => a.len == b.len and (a.ptr == b.ptr or for (0..a.len) |k| {
                    if (!eql(a[k], b[k])) break false;
                } else true),
            };
        },
        .optional => {
            if (a == null and b == null) return true;
            if (a == null or b == null) return false;
            return eql(a.?, b.?);
        },
        else => return a == b,
    }
}

/// can this be safely copied by value?
pub fn isPlainOldData(T: type) bool {
    return switch (@typeInfo(T)) {
        .pointer => false,
        .float, .int, .void, .bool, .@"enum" => true,
        .@"struct" => |info| inline for (info.fields) |field_info| {
            if (!isPlainOldData(field_info.type)) break false;
        } else true,
        inline .array, .vector => |info| isPlainOldData(info.child),
        .error_union => |info| isPlainOldData(info.payload),
        else => @compileError(std.fmt.comptimePrint("TODO: type {any}", .{T})),
    };
}

const std = @import("std");
const assert = std.debug.assert;

const funktional = @import("kommon.zig").funktional;
