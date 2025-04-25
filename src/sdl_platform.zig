const PlatformGives = @import("game.zig").PlatformGives;
const game = @import("game.zig");

var my_game: if (@import("build_options").game_dynlib_path) |game_dynlib_path| struct {
    const Self = @This();

    api: *const game.CApi = &game.game_api,
    dyn_lib: ?std.DynLib = null,
    state: game.GameState,

    fn init() Self {
        return .{ .state = .init() };
    }

    fn update(self: *Self, platform_gives: PlatformGives) !void {
        try self.maybeReloadApi();
        self.api.update(&self.state, &platform_gives);
    }

    fn maybeReloadApi(self: *Self) !void {
        if (self.dyn_lib) |*dyn_lib| dyn_lib.close();
        // const path = "./zig-out/lib/libgame.so";
        const path = if (@import("builtin").os.tag == .windows) blk: {
            try std.fs.copyFileAbsolute(game_dynlib_path, game_dynlib_path ++ ".tmp", .{});
            break :blk game_dynlib_path ++ ".tmp";
        } else game_dynlib_path;
        self.dyn_lib = try .open(path);
        self.api = self.dyn_lib.?.lookup(*const game.CApi, "game_api") orelse return error.LookupFail;
    }
} else game.GameState = undefined;

comptime {
    std.testing.refAllDeclsRecursive(game);
}

pub fn main() !void {
    const windows_platform: PlatformGives = .{
        .stdout = std.io.getStdOut(),
        .gpa = std.heap.smp_allocator,
    };

    my_game = .init();

    for (0..100) |_| {
        try my_game.update(windows_platform);
        windows_platform.stdout.writeAll("bbb123\n") catch unreachable;
        std.Thread.sleep(std.time.ns_per_s);
    }
}

const std = @import("std");
