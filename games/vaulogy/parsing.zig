const std = @import("std");
const MemoryPool = std.heap.MemoryPool;

const core = @import("core.zig");
const Sexpr = core.Sexpr;
const Pair = core.Pair;
const Atom = core.Atom;
const Fnk = core.Fnk;
const FnkCollection = core.FnkCollection;
const MatchCases = core.MatchCases;
const MatchCaseDefinition = core.MatchCaseDefinition;

fn parseSexpr(input: *[]const u8, pool: *MemoryPool(Sexpr)) !*const Sexpr {
    const result = try parseSexprTrue(input.*, pool);
    input.* = result.rest;
    return result.sexpr;
}

pub fn parseSingleSexpr(input: []const u8, pool: *MemoryPool(Sexpr)) !*const Sexpr {
    var result = try parseSexprTrue(input, pool);
    skipWhitespace(&result.rest);
    if (result.rest.len > 0) return error.BAD_INPUT;
    return result.sexpr;
}

fn parseSexprTrue(input: []const u8, pool: *MemoryPool(Sexpr)) error{ OutOfMemory, BAD_INPUT }!struct { sexpr: *const Sexpr, rest: []const u8 } {
    var rest = input;
    skipWhitespace(&rest);
    if (rest[0] == '(') {
        const asdf = try parseSexprInsideParens(rest[1..], pool);
        return .{ .sexpr = asdf.sexpr, .rest = asdf.rest };
    }

    // TODO: remove this special case
    if (std.mem.startsWith(u8, input, "<empty>")) {
        return .{
            .sexpr = Sexpr.builtin.empty,
            .rest = rest["<empty>".len..],
        };
    }

    const asdf = try parseAtom(rest);
    const res = try pool.create();
    if (asdf.is_var) {
        res.* = Sexpr{ .atom_var = asdf.atom };
    } else {
        res.* = Sexpr{ .atom_lit = asdf.atom };
    }
    return .{ .sexpr = res, .rest = asdf.rest };
}

fn parseSexprInsideParens(input: []const u8, pool: *MemoryPool(Sexpr)) !struct { sexpr: *const Sexpr, rest: []const u8 } {
    var rest = input;
    skipWhitespace(&rest);
    if (rest.len == 0) return error.BAD_INPUT;
    if (rest[0] == ')') {
        return .{ .sexpr = Sexpr.builtin.nil, .rest = rest[1..] };
    } else if (rest[0] == '.') {
        const final_asdf = try parseSexprTrue(rest[1..], pool);
        rest = final_asdf.rest;
        skipWhitespace(&rest);
        if (rest.len == 0 or rest[0] != ')') return error.BAD_INPUT;
        return .{ .sexpr = final_asdf.sexpr, .rest = rest[1..] };
    }
    const first_asdf = try parseSexprTrue(rest, pool);
    const rest_asdf = try parseSexprInsideParens(first_asdf.rest, pool);

    const res = try pool.create();
    res.* = Sexpr{ .pair = Pair{ .left = first_asdf.sexpr, .right = rest_asdf.sexpr } };

    return .{ .sexpr = res, .rest = rest_asdf.rest };
}

fn parseAtom(input: []const u8) !struct { atom: Atom, is_var: bool, rest: []const u8 } {
    const word_breaks = .{ '(', ')', ':', '.', ';' } ++ std.ascii.whitespace;
    const rest = std.mem.trimLeft(u8, input, &std.ascii.whitespace);
    const word_end = std.mem.indexOfAnyPos(u8, rest, 0, &word_breaks) orelse rest.len;
    const is_variable = rest[0] == '@';
    return .{
        .atom = Atom{ .value = rest[(if (is_variable) 1 else 0)..word_end] },
        .is_var = is_variable,
        .rest = rest[word_end..],
    };
}

pub fn parseFnk(input: *[]const u8, pool: *MemoryPool(Sexpr), allocator_for_cases: std.mem.Allocator) !Fnk {
    const result = try parseFnkTrue(input.*, pool, allocator_for_cases);
    input.* = result.rest;
    return result.fnk;
}

pub fn parseSingleFnk(input: []const u8, pool: *MemoryPool(Sexpr), allocator_for_cases: std.mem.Allocator) !Fnk {
    var result = try parseFnkTrue(input, pool, allocator_for_cases);
    skipWhitespace(&result.rest);
    if (result.rest.len > 0) {
        std.log.err("unexpected remaining input: \"{s}\"", .{result.rest});
        return error.TooMuchInput;
    }
    return result.fnk;
}

fn parseFnkTrue(input: []const u8, pool: *MemoryPool(Sexpr), allocator_for_cases: std.mem.Allocator) !struct { fnk: Fnk, rest: []const u8 } {
    var rest = input;
    skipWhitespace(&rest);
    const name = try parseSexpr(&rest, pool);
    skipWhitespace(&rest);
    // try parseChar(&rest, ':');
    // skipWhitespace(&rest);
    try parseChar(&rest, '{');
    const cases = try parseMatchCases(&rest, pool, allocator_for_cases);
    skipWhitespace(&rest);
    return .{ .fnk = Fnk{ .name = name, .body = .{ .cases = cases } }, .rest = rest };
}

fn parseMatchCases(input: *[]const u8, pool: *MemoryPool(Sexpr), allocator_for_cases: std.mem.Allocator) !MatchCases {
    var list = std.ArrayListUnmanaged(MatchCaseDefinition){};
    skipWhitespace(input);
    while (!parseCharIfPossible(input, '}')) {
        const pattern = try parseSexpr(input, pool);
        skipWhitespace(input);
        try parseChar(input, '-');
        try parseChar(input, '>');
        skipWhitespace(input);
        const fnk_name_or_template = try parseSexpr(input, pool);
        skipWhitespace(input);
        var fnk_name: *const Sexpr = undefined;
        var template: *const Sexpr = undefined;
        if (parseCharIfPossible(input, ':')) {
            fnk_name = fnk_name_or_template;
            template = try parseSexpr(input, pool);
            skipWhitespace(input);
        } else {
            fnk_name = Sexpr.builtin.identity;
            template = fnk_name_or_template;
        }
        var next: ?MatchCases = undefined;
        if (parseCharIfPossible(input, ';')) {
            next = null;
        } else {
            try parseChar(input, '{');
            next = try parseMatchCases(input, pool, allocator_for_cases);
        }
        skipWhitespace(input);

        try list.append(allocator_for_cases, .{
            .pattern = pattern,
            .fnk_name = fnk_name,
            .template = template,
            .next = next,
        });
    }
    return list;
}

pub fn skipWhitespace(input: *[]const u8) void {
    input.* = std.mem.trimLeft(u8, input.*, &std.ascii.whitespace);
    while (std.mem.startsWith(u8, input.*, "//")) {
        if (std.mem.indexOfScalar(u8, input.*, '\n')) |end| {
            input.* = input.*[(end + 1)..];
            input.* = std.mem.trimLeft(u8, input.*, &std.ascii.whitespace);
        } else {
            input.* = input.*[input.len..];
        }
    }
}

fn parseChar(input: *[]const u8, comptime expected: u8) !void {
    if (input.*[0] != expected) return error.BAD_INPUT;
    input.* = input.*[1..];
}

fn parseCharIfPossible(input: *[]const u8, comptime expected: u8) bool {
    if (input.*[0] != expected) return false;
    input.* = input.*[1..];
    return true;
}

// TODO: this, but much better
pub const Parser = struct {
    remaining_text: []const u8,

    pub fn parseFnkCollection(this: *Parser, result: *FnkCollection, pool: *MemoryPool(Sexpr), allocator_for_cases: std.mem.Allocator) !void {
        while (true) {
            this.skipWhitespaceNew();
            if (this.remaining_text.len == 0) break;
            const fnk = try this.parseFnkNew(pool, allocator_for_cases);
            try result.put(fnk.name, fnk.body);
        }
    }

    fn skipWhitespaceNew(this: *Parser) void {
        skipWhitespace(&this.remaining_text);
    }

    pub fn maybeParseSexpr(this: *Parser, pool: *MemoryPool(Sexpr)) !?*const Sexpr {
        this.skipWhitespaceNew();
        if (this.remaining_text.len == 0) return null;
        const name = try parseSexpr(&this.remaining_text, pool);
        this.skipWhitespaceNew();
        return name;
    }

    pub fn parseFnkNew(this: *Parser, pool: *MemoryPool(Sexpr), allocator_for_cases: std.mem.Allocator) !Fnk {
        this.skipWhitespaceNew();
        const name = try parseSexpr(&this.remaining_text, pool);
        this.skipWhitespaceNew();
        if (!consumeChar(this, '{')) {
            std.log.err("ERROR: No body found for fnk with name {any}\n", .{name});
            return error.BAD_INPUT;
        }
        const cases = parseMatchCases(&this.remaining_text, pool, allocator_for_cases) catch |err| switch (err) {
            error.BAD_INPUT => {
                std.log.err("ERROR: Bad input on fnk with name {any}\n", .{name});
                return err;
            },
            else => return err,
        };
        this.skipWhitespaceNew();
        return Fnk{ .name = name, .body = .{ .cases = cases } };
    }

    fn consumeChar(this: *Parser, comptime expected: u8) bool {
        if (this.remaining_text.len == 0) return false;
        if (this.remaining_text[0] != expected) return false;
        this.remaining_text = this.remaining_text[1..];
        return true;
    }
};
