//! must not import anything from main.zig, the platform layer

// https://nullprogram.com/blog/2014/12/23/

pub const PlatformGives = struct {
    gpa: std.mem.Allocator,
    stdout: std.fs.File,
};

pub const GameState = struct {
    n: usize = 0,
    n2: usize = 0,

    // pub fn init(alloc: std.mem.Allocator) *GameState {
    //     var res = alloc.create(GameState) catch unreachable;
    //     res.n = 0;
    //     return res;
    // }
    pub fn init() GameState {
        return .{};
    }

    pub fn deinit(self: *GameState) void {
        _ = self;
    }

    pub fn update(self: *GameState, platform_gives: PlatformGives) !void {
        _ = platform_gives;
        // platform_gives.stdout.writeAll("aaa123\n") catch unreachable;
        std.log.debug("Update, n is {d} and n2 is {d}", .{ self.n, self.n2 });
        self.n += 1;
        self.n2 += 1;
        // return error.hola;
    }
};

pub const CApi = extern struct {
    // TODO: init should take an allocator
    // init: *const fn () *GameState,
    update: *const @TypeOf(_update),
    // TODO: unload/reload

    fn _update(game: *GameState, platform_gives: *const PlatformGives) callconv(.c) void {
        game.update(platform_gives.*) catch unreachable;
    }
};

pub export const game_api: CApi = .{
    .update = CApi._update,
};

test "foo" {
    try std.testing.expect(true);
}

const std = @import("std");
