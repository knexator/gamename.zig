const std = @import("std");
const KeyboardButton = @import("kommon").input.KeyboardButton;

pub fn main() !void {
    var arena_state = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const args = try std.process.argsAlloc(arena);
    const output_file_path = args[1];

    var output_file = try std.fs.cwd().createFile(output_file_path, .{});
    defer output_file.close();

    var writer = output_file.writer();
    try writer.writeAll("export const keys = {\n");
    inline for (std.meta.fields(KeyboardButton)) |field| {
        try writer.print("\t{s}: {d},\n", .{ field.name, field.value });
    }
    try writer.writeAll("};\n");

    return std.process.cleanExit();
}
