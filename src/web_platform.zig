const PlatformGives = @import("game.zig").PlatformGives;
const game = @import("game.zig");
comptime {
    std.testing.refAllDeclsRecursive(game);
}

pub const std_options = std.Options{
    // wasm-freestanding has no stderr, so we have to override this function
    .logFn = myLogFn,
};
fn myLogFn(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = message_level;
    _ = scope;
    _ = format;
    _ = args;
    // const level_txt = comptime message_level.asText();
    // const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";

    // var buf: [1000]u8 = undefined;
    // const res = std.fmt.bufPrint(&buf, level_txt ++ prefix2 ++ format ++ "\n", args) catch {
    //     js_better.debug.logString("RAN OUT OF LOG BUFFER! the log started with:\n");
    //     js_better.debug.logString(&buf);
    //     return;
    // };
    // js_better.debug.logString(res);
}

// TODO: hot reloading
var my_game: game.GameState = undefined;

const js = struct {
    pub const debug = struct {
        extern fn logInt(arg: i32) void;
    };
};

export fn init() void {
    my_game = .init();
}

export fn update() void {
    my_game.update(undefined) catch unreachable;
    js.debug.logInt(@intCast(my_game.n));
}

const std = @import("std");
