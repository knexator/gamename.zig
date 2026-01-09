pub const Sample = struct { input: *const Sexpr, expected: *const Sexpr };

const core = @import("core.zig");
const Sexpr = core.Sexpr;
const std = @import("std");
const assert = std.debug.assert;

const SexprPool = std.heap.MemoryPool(Sexpr);
const kommon = @import("kommon");

fn safeAt(arr: []const *const Sexpr, index: usize) ?*const Sexpr {
    return kommon.safeAt(*const Sexpr, arr, index);
}

pub const Level = struct {
    fnk_name: *const Sexpr,
    description: []const u8,
    initial_definition: ?core.FnkBodyV2,
    generate_sample: *const fn (k: usize, pool: *SexprPool, arena: std.mem.Allocator) core.OoM!?Sample,

    pub fn samplesIterator(level: Level) SamplesIterator {
        return .{ .k = 0, .level = level };
    }

    pub const SamplesIterator = struct {
        k: usize,
        level: Level,

        pub fn next(self: *SamplesIterator, pool: *SexprPool, arena: std.mem.Allocator) !?Sample {
            const result = try self.level.generate_sample(self.k, pool, arena);
            if (result) |r| {
                self.k += 1;
                return r;
            } else return null;
        }
    };
};

const Solutions = struct {
    fn isVowel(in: *const Sexpr) *const Sexpr {
        return Sexpr.fromBool(Helpers.isVowel(in));
    }
};

const Helpers = struct {
    fn isVowel(in: *const Sexpr) bool {
        return (in.equals(Vals.lowercase[0]) or in.equals(Vals.lowercase[4]) or
            in.equals(Vals.uppercase[0]) or in.equals(Vals.uppercase[4]));
    }

    fn isB(in: *const Sexpr) bool {
        return in.equals(Vals.uppercase[1]) or in.equals(Vals.lowercase[1]);
    }

    fn hasSomeB(in: *const Sexpr) bool {
        if (in.isPair()) {
            return hasSomeB(in.pair.left) or hasSomeB(in.pair.right);
        } else return isB(in);
    }
};

pub const levels: []const Level = &.{
    // .{
    //     .fnk_name = &Sexpr.doLit("changeAtoBandBtoA"),
    //     .description = "Turn 'a' into 'b', and 'b' into 'a'",
    //     .initial_definition = .{ .cases = &.{
    //         .{ .pattern = Vals.lowercase[0], .template = Vals.lowercase[1], .fnk_name = Sexpr.builtin.empty, .next = null },
    //         .{ .pattern = Vals.lowercase[1], .template = Vals.lowercase[0], .fnk_name = Sexpr.builtin.empty, .next = null },
    //     } },
    //     .generate_sample = struct {
    //         fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
    //             if (k == 0) {
    //                 return .{ .input = Vals.lowercase[0], .expected = Vals.lowercase[1] };
    //             } else if (k == 1) {
    //                 return .{ .input = Vals.lowercase[1], .expected = Vals.lowercase[0] };
    //             } else return null;
    //         }
    //     }.generate_sample,
    // },
    .{
        .fnk_name = &Sexpr.doLit("uppercase"),
        .description = "Get the uppercase version of each atom",
        .initial_definition = .{ .cases = &.{
            .{ .pattern = Vals.lowercase[0], .template = Vals.uppercase[0], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[1], .template = Vals.uppercase[1], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[2], .template = Vals.uppercase[2], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[3], .template = Vals.uppercase[3], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[4], .template = Vals.uppercase[4], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[5], .template = Vals.uppercase[5], .fnk_name = Sexpr.builtin.empty, .next = null },
        } },
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const order: [Vals.lowercase.len]usize = .{ 2, 0, 3, 5, 4, 1 };
                if (kommon.safeAt(usize, &order, k)) |k2| {
                    return .{ .input = Vals.lowercase[k2], .expected = Vals.uppercase[k2] };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("lowercase"),
        .description = "Get the lowercase version of each atom",
        .initial_definition = .{ .cases = &.{
            .{ .pattern = Vals.uppercase[0], .template = Vals.lowercase[0], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[1], .template = Vals.uppercase[1], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.uppercase[2], .template = Vals.uppercase[1], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.uppercase[3], .template = Vals.lowercase[3], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Sexpr.builtin.empty, .template = Vals.lowercase[4], .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.uppercase[5], .template = Vals.lowercase[5], .fnk_name = Sexpr.builtin.empty, .next = null },
        } },
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const order: [Vals.lowercase.len]usize = .{ 2, 0, 3, 5, 4, 1 };
                if (kommon.safeAt(usize, &order, k)) |k2| {
                    return .{ .input = Vals.uppercase[k2], .expected = Vals.lowercase[k2] };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("isVowel"),
        .description = "Return true if the input letter is a vowel",
        .initial_definition = .{ .cases = &.{
            .{ .pattern = Vals.lowercase[0], .template = Sexpr.builtin.true, .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.uppercase[0], .template = Sexpr.builtin.true, .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[4], .template = Sexpr.builtin.true, .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = &Sexpr.doVar("other"), .template = Sexpr.builtin.false, .fnk_name = Sexpr.builtin.empty, .next = null },
        } },
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const both = Vals.lowercase ++ Vals.uppercase;
                if (k < both.len) {
                    return .{
                        .input = both[k],
                        .expected = Solutions.isVowel(both[k]),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("swap"),
        .description = "Swap both values",
        .initial_definition = .{ .cases = &.{.{
            .pattern = &.doPair(Sexpr.builtin.empty, Vals.vars.down),
            .template = &.doPair(Vals.vars.down, Vals.vars.up),
            .fnk_name = Sexpr.builtin.empty,
            .next = null,
        }} },
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const some_samples: []const [2]*const Sexpr = &.{
                    .{ Vals.uppercase[0], Vals.lowercase[0] },
                    .{ Vals.uppercase[0], Vals.lowercase[2] },
                    .{ Vals.uppercase[1], Vals.uppercase[5] },
                    .{
                        Vals.uppercase[0],
                        try store(pool, .doPair(Vals.uppercase[1], Vals.lowercase[5])),
                    },
                    .{
                        try store(pool, .doPair(Vals.uppercase[0], Vals.lowercase[2])),
                        Vals.uppercase[1],
                    },
                    .{
                        try store(pool, .doPair(Vals.uppercase[0], Vals.lowercase[2])),
                        try store(pool, .doPair(Vals.uppercase[1], Vals.uppercase[5])),
                    },
                };
                if (kommon.safeAt([2]*const Sexpr, some_samples, k)) |pair| {
                    const left = pair[0];
                    const right = pair[1];
                    return .{
                        .input = try store(pool, Sexpr.doPair(left, right)),
                        .expected = try store(pool, Sexpr.doPair(right, left)),
                    };
                } else if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    const left = try randomSexpr(pool, &(Vals.lowercase ++ Vals.uppercase), random, if (k < 10) 2 else 4);
                    const right = try randomSexpr(pool, &(Vals.lowercase ++ Vals.uppercase), random, if (k < 10) 2 else 4);
                    return .{
                        .input = try store(pool, Sexpr.doPair(left, right)),
                        .expected = try store(pool, Sexpr.doPair(right, left)),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("firstAsUppercase"),
        .description = "Get the pair's top half in uppercase.",
        .initial_definition = .{ .cases = &.{
            .{
                .pattern = &.doPair(Vals.lowercase[0], Sexpr.builtin.empty),
                .template = Vals.lowercase[0],
                .fnk_name = &.doLit("uppercase"),
                .next = null,
            },
            .{
                .pattern = &.doPair(Vals.lowercase[1], Sexpr.builtin.empty),
                .template = Vals.lowercase[1],
                .fnk_name = &.doLit("uppercase"),
                .next = null,
            },
        } },
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const k1 = @mod(k, Vals.lowercase.len);
                const k2 = @divFloor(k, Vals.lowercase.len);
                if (k2 < Vals.lowercase.len) {
                    return .{
                        .input = try store(pool, Sexpr.doPair(Vals.lowercase[k1], Vals.lowercase[k2])),
                        .expected = Vals.uppercase[k1],
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("startWithB"),
        .description = "Check if pair's top half is a B.",
        .initial_definition = .{ .cases = &.{
            .{
                .pattern = &.doPair(&.doVar("left"), Sexpr.builtin.empty),
                .template = &.doVar("left"),
                .fnk_name = Sexpr.builtin.empty,
                .next = &.{
                    .{
                        .pattern = Vals.lowercase[1],
                        .template = Sexpr.builtin.true,
                        .fnk_name = Sexpr.builtin.empty,
                        .next = null,
                    },
                    .{
                        .pattern = &.doVar("else"),
                        .template = Sexpr.builtin.false,
                        .fnk_name = Sexpr.builtin.empty,
                        .next = null,
                    },
                },
            },
        } },
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const both = Vals.lowercase ++ Vals.uppercase;
                const k1 = @mod(k, both.len);
                const k2 = @divFloor(k, both.len);
                if (k2 < both.len) {
                    return .{
                        .input = try store(pool, Sexpr.doPair(both[k1], both[k2])),
                        .expected = Sexpr.fromBool(Helpers.isB(both[k1])),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("uppercaseIfVowel"),
        .description = "Change only the vowels to uppercase.",
        .initial_definition = .{ .cases = &.{
            .{
                .pattern = &.doVar("letter"),
                .template = &.doVar("letter"),
                .fnk_name = &.doLit("isVowel"),
                .next = &.{
                    .{
                        .pattern = Sexpr.builtin.true,
                        .template = &.doVar("letter"),
                        .fnk_name = &.doLit("uppercase"),
                        .next = null,
                    },
                    .{
                        .pattern = Sexpr.builtin.false,
                        .template = &.doVar("letter"),
                        .fnk_name = Sexpr.builtin.empty,
                        .next = null,
                    },
                },
            },
        } },
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    const in = Vals.lowercase[k];
                    return .{
                        .input = in,
                        .expected = if (Helpers.isVowel(in))
                            Vals.uppercase[k]
                        else
                            in,
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("withFirstUppercased"),
        .description = "Change the top half to be uppercase.",
        .initial_definition = null,
        // .{ .cases = &.{
        //     .{
        //         .pattern = &.doPair(Vals.lowercase[0], Vals.lowercase[0]),
        //         .template = &.doPair(Vals.uppercase[0], Vals.lowercase[0]),
        //         .fnk_name = Sexpr.builtin.empty,
        //         .next = null,
        //     },
        //     .{
        //         .pattern = &.doPair(Vals.lowercase[0], Vals.lowercase[1]),
        //         .template = Vals.lowercase[0],
        //         .fnk_name = &.doLit("uppercase"),
        //         .next = &.{
        //             .{
        //                 .pattern = Vals.uppercase[0],
        //                 .template = &.doPair(Vals.uppercase[0], Vals.lowercase[1]),
        //                 .fnk_name = Sexpr.builtin.empty,
        //                 .next = null,
        //             },
        //         },
        //     },
        //     .{
        //         .pattern = &.doPair(&.doVar("first"), Vals.lowercase[2]),
        //         .template = &.doVar("first"),
        //         .fnk_name = &.doLit("uppercase"),
        //         .next = &.{
        //             .{
        //                 .pattern = &.doVar("FIRST"),
        //                 .template = &.doPair(&.doVar("FIRST"), Vals.lowercase[2]),
        //                 .fnk_name = Sexpr.builtin.empty,
        //                 .next = null,
        //             },
        //         },
        //     },
        // } },
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const k1 = @mod(k, Vals.lowercase.len);
                const k2 = @divFloor(k, Vals.lowercase.len);
                if (k2 < Vals.lowercase.len) {
                    return .{
                        .input = try store(pool, Sexpr.doPair(Vals.lowercase[k1], Vals.lowercase[k2])),
                        .expected = try store(pool, Sexpr.doPair(Vals.uppercase[k1], Vals.lowercase[k2])),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("pairToUppercase"),
        .description = "Make both halves into uppercase.",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                const k1 = @mod(k, Vals.lowercase.len);
                const k2 = @divFloor(k, Vals.lowercase.len);
                if (k2 < Vals.lowercase.len) {
                    return .{
                        .input = try store(pool, Sexpr.doPair(Vals.lowercase[k1], Vals.lowercase[k2])),
                        .expected = try store(pool, Sexpr.doPair(Vals.uppercase[k1], Vals.uppercase[k2])),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("changeCase"),
        .description = "Change the casing of the given letter.",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, _: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    return .{
                        .input = Vals.lowercase[k],
                        .expected = Vals.uppercase[k],
                    };
                } else if (k < Vals.lowercase.len * 2) {
                    return .{
                        .input = Vals.uppercase[k - Vals.lowercase.len],
                        .expected = Vals.lowercase[k - Vals.lowercase.len],
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("letterToBothCases"),
        .description = "Get the lowercase and uppercase versions of the given letter.",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < Vals.lowercase.len) {
                    return .{
                        .input = Vals.lowercase[k],
                        .expected = try store(pool, Sexpr.doPair(Vals.lowercase[k], Vals.uppercase[k])),
                    };
                } else if (k < Vals.lowercase.len * 2) {
                    const k2 = k - Vals.lowercase.len;
                    return .{
                        .input = Vals.uppercase[k2],
                        .expected = try store(pool, Sexpr.doPair(Vals.lowercase[k2], Vals.uppercase[k2])),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("hasSomeB"),
        // intro to recursion
        .description = "Check if the input has a 'B' anywhere.",
        .initial_definition = .{ .cases = &.{
            .{ .pattern = Vals.lowercase[0], .template = Sexpr.builtin.false, .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[1], .template = Sexpr.builtin.true, .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[2], .template = Sexpr.builtin.false, .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[3], .template = Sexpr.builtin.false, .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[4], .template = Sexpr.builtin.false, .fnk_name = Sexpr.builtin.empty, .next = null },
            .{ .pattern = Vals.lowercase[5], .template = Sexpr.builtin.false, .fnk_name = Sexpr.builtin.empty, .next = null },
            .{
                .pattern = &.doPair(&.doVar("any"), Vals.lowercase[1]),
                .template = Sexpr.builtin.true,
                .fnk_name = Sexpr.builtin.empty,
                .next = null,
            },
            .{
                .pattern = &.doPair(Vals.lowercase[1], &.doVar("any")),
                .template = Sexpr.builtin.true,
                .fnk_name = Sexpr.builtin.empty,
                .next = null,
            },
        } },
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    const input = try randomSexpr(pool, &Vals.lowercase, random, 5);
                    return .{
                        .input = input,
                        .expected = Sexpr.fromBool(Helpers.hasSomeB(input)),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("second"),
        // simplest intro to lists
        .description = "Get the second element of the list.",
        .initial_definition = .{ .cases = &.{
            .{ .pattern = &.doPair(
                Vals.lowercase[0],
                &.doPair(
                    Vals.lowercase[1],
                    Sexpr.builtin.nil,
                ),
            ), .template = Vals.uppercase[0], .fnk_name = Vals.lowercase[1], .next = null },
            .{ .pattern = &.doPair(
                Vals.lowercase[0],
                &.doPair(
                    Vals.lowercase[1],
                    &.doPair(
                        Vals.lowercase[2],
                        Sexpr.builtin.nil,
                    ),
                ),
            ), .template = Vals.uppercase[0], .fnk_name = Vals.lowercase[1], .next = null },
            .{ .pattern = &.doPair(
                Vals.lowercase[0],
                &.doPair(
                    Vals.lowercase[1],
                    &.doPair(
                        Vals.lowercase[2],
                        &.doPair(
                            Vals.lowercase[3],
                            Sexpr.builtin.nil,
                        ),
                    ),
                ),
            ), .template = Vals.uppercase[0], .fnk_name = Vals.lowercase[1], .next = null },
        } },
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    const first = randomChoice(&Vals.lowercase, random);
                    const second = randomChoice(&Vals.lowercase, random);
                    const rest = try randomList(pool, &Vals.lowercase, random, random.intRangeAtMost(usize, 0, 7));
                    return .{
                        .input = try toListWithSentinel(pool, &.{ first, second }, rest),
                        .expected = second,
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("listHasSomeB"),
        // real intro to lists
        .description = "Check if the given list has a 'B' anywhere.",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                // TODO: builtin samples:
                // nil -> false;
                // (f . nil) -> false;
                // (a . (b . nil)) -> true;
                // (c . (d . (e . nil))) -> false;
                // // The first element of this list is '(b . b)',
                // // which is not 'b'!
                // ((b . b) . (a . nil)) -> false;
                if (k == 0) {
                    return .{
                        .input = Sexpr.builtin.nil,
                        .expected = Sexpr.builtin.false,
                    };
                } else if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    var remaining_len = 1 + random.uintLessThan(usize, @min(k, 9));
                    // long samples
                    if (k > 90) remaining_len += 50;
                    var input = Sexpr.builtin.nil;
                    var has_b = false;
                    while (remaining_len > 0) : (remaining_len -= 1) {
                        const v = if (random.float(f32) < 0.2)
                            Vals.lowercase[1]
                        else
                            try randomSexpr(pool, &Vals.lowercase, random, 3);
                        has_b = has_b or Helpers.isB(v);
                        input = try store(pool, Sexpr.doPair(v, input));
                    }
                    return .{
                        .input = input,
                        .expected = Sexpr.fromBool(has_b),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("uppercaseEachElement"),
        .description = "Uppercase each element in the list.",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k == 0) {
                    return .{
                        .input = Sexpr.builtin.nil,
                        .expected = Sexpr.builtin.nil,
                    };
                } else if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    var remaining_len = 1 + random.uintLessThan(usize, @min(k, 9));
                    // long samples
                    if (k > 90) remaining_len += 50;
                    var input = Sexpr.builtin.nil;
                    var output = Sexpr.builtin.nil;
                    while (remaining_len > 0) : (remaining_len -= 1) {
                        const i = random.uintLessThan(usize, Vals.lowercase.len);
                        input = try store(pool, Sexpr.doPair(Vals.lowercase[i], input));
                        output = try store(pool, Sexpr.doPair(Vals.uppercase[i], output));
                    }
                    return .{
                        .input = input,
                        .expected = output,
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("hasAnyVowel"),
        .description = "Return true if the list contains any vowel",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                if (k == 0) {
                    return .{
                        .input = Sexpr.builtin.nil,
                        .expected = Sexpr.builtin.false,
                    };
                } else if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    var remaining_len = 1 + random.uintLessThan(usize, @min(k, 9));
                    // long samples
                    if (k > 90) remaining_len += 50;
                    var input = Sexpr.builtin.nil;
                    var output = false;
                    while (remaining_len > 0) : (remaining_len -= 1) {
                        const i = random.uintLessThan(usize, Vals.lowercase.len);
                        input = try store(pool, Sexpr.doPair(Vals.lowercase[i], input));
                        output = output or Helpers.isVowel(Vals.lowercase[i]);
                    }
                    return .{
                        .input = input,
                        .expected = Sexpr.fromBool(output),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("reverse"),
        .description = "Reverse the given list.",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, arena: std.mem.Allocator) core.OoM!?Sample {
                const some_samples: []const []const *const Sexpr = &.{
                    &.{},
                    &.{Vals.lowercase[0]},
                    &.{ Vals.lowercase[0], Vals.lowercase[1] },
                    &.{ Vals.lowercase[0], Vals.lowercase[1], Vals.lowercase[2] },
                    &.{ Vals.uppercase[0], Vals.uppercase[1] },
                    &.{ Vals.lowercase[3], Vals.lowercase[4], Vals.lowercase[4] },
                };
                if (kommon.safeAt([]const *const Sexpr, some_samples, k)) |input| {
                    const output = try arena.dupe(*const Sexpr, input);
                    std.mem.reverse(*const Sexpr, output);
                    return .{
                        .input = try toList(pool, input),
                        .expected = try toList(pool, output),
                    };
                } else if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    var remaining_len = 1 + random.uintLessThan(usize, switch (k) {
                        0...19 => 4,
                        20...59 => 7,
                        60...100 => 50,
                        else => unreachable,
                    });
                    var input = Sexpr.builtin.nil;
                    var output: std.ArrayListUnmanaged(*const Sexpr) = try .initCapacity(arena, remaining_len);
                    while (remaining_len > 0) : (remaining_len -= 1) {
                        const i = random.uintLessThan(usize, Vals.lowercase.len);
                        input = try store(pool, Sexpr.doPair(Vals.lowercase[i], input));
                        output.appendAssumeCapacity(Vals.lowercase[i]);
                    }
                    return .{
                        .input = input,
                        .expected = try toList(pool, output.items),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("mostCommonBoolean"),
        .description = "Return the most common element of the list.",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, arena: std.mem.Allocator) core.OoM!?Sample {
                const t = Sexpr.builtin.true;
                const f = Sexpr.builtin.false;
                const premade_samples: []const struct { input: []const *const Sexpr, expected: *const Sexpr } = &.{
                    .{
                        .input = &.{ t, f, t },
                        .expected = t,
                    },
                    .{
                        .input = &.{ t, f, f, t, f },
                        .expected = f,
                    },
                    .{
                        .input = &.{ t, t, t, f, f, f, f },
                        .expected = f,
                    },
                    .{
                        .input = &.{ t, f, t, f, t, f, t },
                        .expected = t,
                    },
                };
                if (k < premade_samples.len) {
                    return .{
                        .input = try toList(pool, premade_samples[k].input),
                        .expected = premade_samples[k].expected,
                    };
                } else if (k < 100) {
                    var random_instance: std.Random.DefaultPrng = .init(@intCast(k));
                    const random = random_instance.random();
                    var num_true = 1 + random.uintLessThan(usize, 10);
                    var num_false = 1 + random.uintLessThan(usize, 10);
                    // long samples
                    if (k > 90) num_false += 50;
                    if (k > 90) num_true += 50;
                    if (num_true == num_false) {
                        if (random.boolean()) {
                            num_true += 1;
                        } else {
                            num_false += 1;
                        }
                    }
                    const all_elements = try arena.alloc(*const Sexpr, num_true + num_false);
                    @memset(all_elements, f);
                    for (0..num_true) |_| {
                        var index = random.uintLessThan(usize, all_elements.len);
                        while (all_elements[index] == t) {
                            index = random.uintLessThan(usize, all_elements.len);
                        }
                        all_elements[index] = t;
                    }
                    return .{
                        .input = try toList(pool, all_elements),
                        .expected = Sexpr.fromBool(num_true > num_false),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        .fnk_name = &Sexpr.doLit("brainfuck"),
        .description = "Implement a BrainF*ck interpreter.",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, _: std.mem.Allocator) core.OoM!?Sample {
                // TODO: infinite samples
                const prev = Vals.BF.prev;
                const next = Vals.BF.next;
                const inc = Vals.BF.inc;
                const dec = Vals.BF.dec;
                const in = Vals.BF.in;
                const out = Vals.BF.out;
                const loop = Vals.BF.loop;
                const end = Vals.BF.end;
                const premade_samples: []const struct {
                    code: []const *const Sexpr,
                    stdin: []const usize,
                    output: []const usize,
                } = &.{ .{
                    .code = &.{ in, in, out, in, out },
                    .stdin = &.{ 1, 2, 3, 4 },
                    .output = &.{ 2, 3 },
                }, .{
                    .code = &.{
                        inc, inc, out,
                        inc, inc, inc,
                        out, dec, dec,
                        dec, dec, out,
                    },
                    .stdin = &.{},
                    .output = &.{ 2, 5, 1 },
                }, .{
                    .code = &.{ inc, next, next, prev, prev, out },
                    .stdin = &.{},
                    .output = &.{1},
                }, .{
                    .code = &.{
                        inc,  inc,  inc,
                        loop, dec,  next,
                        inc,  inc,  prev,
                        end,  next, out,
                    },
                    .stdin = &.{},
                    .output = &.{6},
                } };
                if (k < premade_samples.len) {
                    const sample = premade_samples[k];
                    return .{
                        .input = try store(pool, Sexpr.doPair(
                            try toList(pool, sample.code),
                            try toListOfPeano(pool, sample.stdin),
                        )),
                        .expected = try toListOfPeano(pool, sample.output),
                    };
                } else return null;
            }
        }.generate_sample,
    },
    .{
        // TODO
        .fnk_name = &Sexpr.doLit("interpreter"),
        .description = "Implement a Vaulogy interpreter.",
        .initial_definition = null,
        .generate_sample = struct {
            fn generate_sample(k: usize, pool: *SexprPool, arena: std.mem.Allocator) core.OoM!?Sample {
                // TODO: should be an argument?
                const strings_pool = pool.arena.allocator();
                if (k != 0) return null;
                var mem = core.VeryPermamentGameStuff.init(arena);
                defer mem.deinit();
                var scoring = try core.ScoringRun.init(
                    \\ bubbleUp {
                    \\      (A . @rest) -> (A . @rest);
                    \\      (@a . @b) -> bubbleUp: @b {
                    \\          (A . @rest) -> (A . (@a . @rest));
                    \\      }
                    \\ }
                , &mem);
                defer scoring.deinit(true);
                const input = try toList(pool, &.{ Vals.uppercase[1], Vals.uppercase[2], Vals.uppercase[0], Vals.uppercase[3] });
                const output = try toList(pool, &.{ Vals.uppercase[0], Vals.uppercase[1], Vals.uppercase[2], Vals.uppercase[3] });
                const fnk_name = try store(pool, Sexpr.doLit("bubbleUp"));
                const fnk_def = scoring.all_fnks.get(fnk_name).?;
                const fnk_def_sexpr = try store(pool, Sexpr.doPair(
                    fnk_name,
                    try core.deepCloneSexpr(
                        true,
                        try core.sexprFromCases(fnk_def.cases.items, pool),
                        pool,
                        strings_pool,
                    ),
                ));
                return .{
                    .input = try store(pool, Sexpr.doPair(
                        input,
                        try store(pool, Sexpr.doPair(
                            fnk_name,
                            try toList(pool, &.{fnk_def_sexpr}),
                        )),
                    )),
                    .expected = output,
                };
            }
        }.generate_sample,
    },
};

fn store(pool: *SexprPool, s: Sexpr) !*const Sexpr {
    const res = try pool.create();
    res.* = s;
    return res;
}

fn toList(pool: *SexprPool, items: []const *const Sexpr) !*const Sexpr {
    return toListWithSentinel(pool, items, Sexpr.builtin.nil);
}

fn toListWithSentinel(pool: *SexprPool, items: []const *const Sexpr, sentinel: *const Sexpr) !*const Sexpr {
    var result = sentinel;
    for (0..items.len) |k| {
        result = try store(pool, Sexpr.doPair(items[items.len - 1 - k], result));
    }
    return result;
}

fn toPeano(pool: *SexprPool, n: usize) !*const Sexpr {
    var result = Sexpr.builtin.nil;
    for (0..n) |_| {
        result = try store(pool, Sexpr.doPair(Vals.peano_succ, result));
    }
    return result;
}

fn toListOfPeano(pool: *SexprPool, ns: []const usize) !*const Sexpr {
    var result = Sexpr.builtin.nil;
    for (0..ns.len) |k| {
        const n = ns[ns.len - k - 1];
        result = try store(pool, Sexpr.doPair(try toPeano(pool, n), result));
    }
    return result;
}

fn randomList(pool: *SexprPool, options: []const *const Sexpr, random: std.Random, len: usize) !*const Sexpr {
    if (len == 0) {
        return Sexpr.builtin.nil;
    } else {
        const first = randomChoice(options, random);
        const rest = try randomList(pool, options, random, len - 1);
        return try store(pool, Sexpr.doPair(first, rest));
    }
}

fn randomChoice(options: []const *const Sexpr, random: std.Random) *const Sexpr {
    assert(options.len > 0);
    return options[random.uintLessThan(usize, options.len)];
}

fn randomSexpr(pool: *SexprPool, atoms: []const *const Sexpr, random: std.Random, max_depth: usize) !*const Sexpr {
    if (max_depth == 0 or random.float(f32) < 0.3) {
        return randomChoice(atoms, random);
    } else {
        return try store(pool, Sexpr.doPair(
            try randomSexpr(pool, atoms, random, max_depth - 1),
            try randomSexpr(pool, atoms, random, max_depth - 1),
        ));
    }
}

const Vals = struct {
    const peano_succ: *const Sexpr = &Sexpr.doLit("N");

    const lowercase: [6]*const Sexpr = .{
        &Sexpr.doLit("a"),
        &Sexpr.doLit("b"),
        &Sexpr.doLit("c"),
        &Sexpr.doLit("d"),
        &Sexpr.doLit("e"),
        &Sexpr.doLit("f"),
    };
    const uppercase: [6]*const Sexpr = .{
        &Sexpr.doLit("A"),
        &Sexpr.doLit("B"),
        &Sexpr.doLit("C"),
        &Sexpr.doLit("D"),
        &Sexpr.doLit("E"),
        &Sexpr.doLit("F"),
    };

    const vars: struct {
        down: *const Sexpr = &Sexpr.doVar("down"),
        up: *const Sexpr = &Sexpr.doVar("up"),
        other: *const Sexpr = &Sexpr.doVar("other"),
    } = .{};

    const BF: struct {
        prev: *const Sexpr = &Sexpr.doLit("prev"),
        next: *const Sexpr = &Sexpr.doLit("next"),
        inc: *const Sexpr = &Sexpr.doLit("inc"),
        dec: *const Sexpr = &Sexpr.doLit("dec"),
        in: *const Sexpr = &Sexpr.doLit("in"),
        out: *const Sexpr = &Sexpr.doLit("out"),
        loop: *const Sexpr = &Sexpr.doLit("loop"),
        end: *const Sexpr = &Sexpr.doLit("end"),
    } = .{};
};
