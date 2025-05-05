const js = struct {
    pub const debug = struct {
        extern fn logInt(arg: u32) void;
        extern fn logFloat(arg: f32) void;
        extern fn logString(ptr: [*]const u8, len: usize) void;
    };

    pub const canvas = struct {
        extern fn beginPath() void;
        extern fn moveTo(x: f32, y: f32) void;
        extern fn lineTo(x: f32, y: f32) void;
        extern fn closePath() void;
        extern fn fill() void;
        extern fn stroke() void;
        extern fn setLineWidth(w: f32) void;
        extern fn setFillColor(r: u8, g: u8, b: u8, a: u8) void;
        extern fn setStrokeColor(r: u8, g: u8, b: u8, a: u8) void;
        extern fn getWidth() u32;
        extern fn getHeight() u32;
    };
};

const js_better = struct {
    pub const debug = struct {
        pub fn logString(s: []const u8) void {
            js.debug.logString(s.ptr, s.len);
        }
    };

    pub const canvas = struct {
        pub fn getSize() UVec2 {
            return UVec2.new(@intCast(js.canvas.getWidth()), @intCast(js.canvas.getHeight()));
        }

        pub fn getRect() Rect {
            return .{
                .top_left = .zero,
                .size = getSize().tof32(),
            };
        }

        pub fn setFillColor(c: Color) void {
            js.canvas.setFillColor(c.r, c.g, c.b, c.a);
        }

        pub fn setStrokeColor(c: Color) void {
            js.canvas.setStrokeColor(c.r, c.g, c.b, c.a);
        }

        pub fn moveTo(p: Vec2) void {
            js.canvas.moveTo(p.x, p.y);
        }

        pub fn lineTo(p: Vec2) void {
            js.canvas.lineTo(p.x, p.y);
        }

        // pub fn clear(color: Color) void {
        //     setFillColor(color);
        //     const size = getSize();
        //     js.canvas.fillRect(0, 0, size.x, size.y);
        // }

        pub fn pathLoop(all_positions: []const Vec2) void {
            if (all_positions.len < 3) unreachable;
            js.canvas.beginPath();
            moveTo(all_positions[0]);
            for (all_positions[1..]) |pos| {
                lineTo(pos);
            }
            js.canvas.closePath();
        }

        pub fn path(all_positions: []const Vec2) void {
            assert(all_positions.len >= 2);
            moveTo(all_positions[0]);
            for (all_positions[1..]) |pos| {
                lineTo(pos);
            }
        }
    };
};

const game = @import("game.zig");
const PlatformGives = game.PlatformGives;
comptime {
    std.testing.refAllDeclsRecursive(game);
}

// TODO: hot reloading is not working :(

var my_game: if (@import("build_options").hot_reloadable) *game.GameState else game.GameState = undefined;

const gpa = std.heap.wasm_allocator;
var render_queue: game.RenderQueue = .init(gpa);

var web_platform: PlatformGives = .{
    .gpa = gpa,
    .render_queue = &render_queue,
    .getMouse = struct {
        pub fn anon(camera: Rect) Mouse {
            var result = mouse;
            result.cur.position = camera.applyToLocalPosition(result.cur.position);
            result.prev.position = camera.applyToLocalPosition(result.prev.position);
            // TODO: delete these fields
            result.cur.client_pos = .zero;
            result.prev.client_pos = .zero;
            return result;
        }
    }.anon,
    .keyboard = undefined,
    .delta_seconds = 0,
    .aspect_ratio = undefined,
    .global_seconds = 0,
};

fn render(cmd: game.RenderQueue.Command) !void {
    switch (cmd) {
        .clear => |color| {
            const size = js_better.canvas.getSize().tof32();
            js_better.canvas.pathLoop(&.{
                .zero, .new(size.x, 0), size, .new(0, size.y),
            });
            js_better.canvas.setFillColor(color);
            js.canvas.fill();
        },
        .shape => |shape| {
            // TODO: use another allocator
            // or maybe just reuse that mem?
            const screen_positions = try gpa.alloc(Vec2, shape.local_points.len);
            defer gpa.free(screen_positions);

            const screen = js_better.canvas.getRect();

            for (shape.local_points, screen_positions) |local_pos, *screen_pos| {
                screen_pos.* = screen.applyToLocalPosition(
                    shape.camera.localFromWorldPosition(
                        shape.parent_world_point.applyToLocalPosition(local_pos),
                    ),
                );
            }

            js_better.canvas.pathLoop(screen_positions);

            if (shape.fill) |color| {
                js_better.canvas.setFillColor(color);
                js.canvas.fill();
            }
            if (shape.stroke) |color| {
                js.canvas.setLineWidth(1);
                js_better.canvas.setStrokeColor(color);
                js.canvas.stroke();
            }
        },
    }
}

export fn init() void {
    if (@import("build_options").hot_reloadable) {
        my_game = std.heap.wasm_allocator.create(game.GameState) catch unreachable;
        my_game.* = .init();
    } else {
        my_game = .init();
    }
}

export fn getDesiredAspectRatio() f32 {
    return game.metadata.desired_aspect_ratio;
}

export fn getTitle() [*:0]const u8 {
    return game.metadata.name;
}

export fn update(delta_seconds: f32) void {
    web_platform.aspect_ratio = js_better.canvas.getSize().aspectRatio();
    web_platform.delta_seconds = delta_seconds;
    web_platform.global_seconds += delta_seconds;
    web_platform.keyboard = keyboard;
    _ = my_game.update(web_platform) catch unreachable;
    mouse.prev = mouse.cur;
    mouse.cur.scrolled = .none;
    keyboard.prev = keyboard.cur;
    defer web_platform.render_queue.pending_commands.clearRetainingCapacity();
    var it = web_platform.render_queue.pending_commands.constIterator(0);
    while (it.next()) |cmd| render(cmd.*) catch unreachable;
}

/// positions are in [0..1]x[0..1]
var mouse: Mouse = .{ .cur = .init, .prev = .init };
export fn pointermove(x: f32, y: f32) void {
    mouse.cur.position = js_better.canvas.getRect().localFromWorldPosition(.new(x, y));
}

const MouseButton = enum(u8) {
    left = 0,
    middle = 1,
    right = 2,
    _,
};

export fn pointerup(button: MouseButton) void {
    switch (button) {
        .left => mouse.cur.buttons.left = false,
        .middle => mouse.cur.buttons.middle = false,
        .right => mouse.cur.buttons.right = false,
        _ => {},
    }
}

export fn pointerdown(button: MouseButton) void {
    switch (button) {
        .left => mouse.cur.buttons.left = true,
        .middle => mouse.cur.buttons.middle = true,
        .right => mouse.cur.buttons.right = true,
        _ => {},
    }
}

export fn wheel(delta_y: i32) void {
    mouse.cur.scrolled = if (delta_y == 0)
        .none
    else if (delta_y > 0)
        .down
    else
        .up;
}

var keyboard = Keyboard{ .cur = .init, .prev = .init };
const KeyCode = KeyboardButton;

export fn keydown(code: KeyCode) void {
    keychanged(code, true);
}

export fn keyup(code: KeyCode) void {
    keychanged(code, false);
}

fn keychanged(key: KeyCode, is_pressed: bool) void {
    switch (key) {
        inline else => |x| @field(keyboard.cur.keys, @tagName(x)) = is_pressed,
    }
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
    const level_txt = comptime message_level.asText();
    const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";

    var buf: [1000]u8 = undefined;
    const res = std.fmt.bufPrint(&buf, level_txt ++ prefix2 ++ format ++ "\n", args) catch {
        js_better.debug.logString("RAN OUT OF LOG BUFFER! the log started with:\n");
        js_better.debug.logString(&buf);
        return;
    };
    js_better.debug.logString(res);
}

const std = @import("std");
const assert = std.debug.assert;

const kommon = @import("kommon");
const math = kommon.math;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const Color = math.Color;
const Camera = math.Camera;
const Point = math.Point;
const Rect = math.Rect;
const Mouse = game.Mouse;
const Keyboard = game.Keyboard;
const KeyboardButton = game.KeyboardButton;
