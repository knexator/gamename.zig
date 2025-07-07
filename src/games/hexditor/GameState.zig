const InputState = struct {
    command: ?Hexditor.Instruction.Kind = null,
    operands: Hexditor.Instruction.Operands = .{ .len = 0 },

    pub fn update(self: *InputState, platform: PlatformGives) ?Hexditor.Instruction {
        if (self.command) |cmd| {
            for ([_]KeyboardButton{ .KeyJ, .KeyK, .KeyL, .Semicolon }, 0..) |key, k| {
                if (platform.keyboard.wasPressed(key)) {
                    self.operands.append(.{
                        .byte = @intCast(k),
                        // TODO: support further indirection?
                        .indirection = if (platform.keyboard.cur.isShiftDown()) 1 else 0,
                    }) catch unreachable;

                    if (self.operands.len >= cmd.nOperands()) {
                        const result: Hexditor.Instruction = .{
                            .kind = cmd,
                            .operands = self.operands,
                        };
                        self.command = null;
                        self.operands.clear();
                        return result;
                    }
                }
            }
        } else {
            if (platform.keyboard.wasPressed(.KeyQ)) {
                self.command = .inc;
            } else if (platform.keyboard.wasPressed(.KeyA)) {
                self.command = .add;
            } else if (platform.keyboard.wasPressed(.KeyW)) {
                self.command = .neg;
            }
        }
        return null;
    }
};

const Hexditor = struct {
    values: [0x100]u8 = @splat(0),

    input_state: InputState = .{},

    fn at(self: *Hexditor, operand: Instruction.Operand) *u8 {
        var result = operand.byte;
        for (0..operand.indirection) |_| {
            result = self.values[result];
        }
        return &self.values[result];
    }

    pub const Instruction = struct {
        kind: Kind,
        operands: Operands,

        pub const Kind = enum {
            inc,
            add,
            neg,

            pub fn nOperands(self: @This()) usize {
                return switch (self) {
                    .inc => 1,
                    .add => 2,
                    .neg => 1,
                };
            }
        };

        pub const Operand = struct {
            byte: u8,
            indirection: u8,
        };

        pub const Operands = std.BoundedArray(Operand, 4);

        comptime {
            for (@typeInfo(Kind).@"enum".fields) |field| {
                const kind = @field(Kind, field.name);
                assert(kind.nOperands() <= @as(Operands, undefined).capacity());
            }
        }
    };

    pub fn process(self: *Hexditor, instruction: Instruction) void {
        assert(instruction.operands.len == instruction.kind.nOperands());
        switch (instruction.kind) {
            .inc => {
                self.at(instruction.operands.get(0)).* +%= 1;
            },
            .add => {
                const a = self.at(instruction.operands.get(0));
                const b = self.at(instruction.operands.get(1));
                a.* +%= b.*;
            },
            .neg => {
                const v = self.at(instruction.operands.get(0));
                v.* = @as(u8, @bitCast(-@as(i8, @bitCast(v.*))));
            },
        }
    }

    pub fn draw(self: Hexditor, canvas: *Canvas, camera: Rect) !void {
        const COLORS = struct {
            text: FColor = .white,
            bg_panel: FColor = .fromHex("#222222"),
            bg_checker: [2]FColor = .{ .fromHex("#332211"), .fromHex("#112233") },
        }{};

        var bg_rects: std.ArrayList(Canvas.FilledRect) = .init(canvas.frame_arena.allocator());
        var fg_text = canvas.textBatch(0);

        for (0..16) |k| {
            try bg_rects.append(.{ .pos = .{
                .top_left = .new(tof32(k), 0),
                .size = .one,
            }, .color = COLORS.bg_panel });
            // }, .color = COLORS.bg_checker[@mod(k, 2)] });
            try fg_text.addFmt(
                "{x}",
                .{k},
                .{
                    .hor = .center,
                    .ver = .median,
                    .pos = bg_rects.getLast().pos.getCenter(),
                },
                0.75,
                COLORS.text,
            );
        }

        for (0..16) |line_index| {
            for (0..16) |k| {
                try bg_rects.append(.{ .pos = .{
                    .top_left = .new(tof32(k), tof32(line_index + 1)),
                    .size = .one,
                }, .color = COLORS.bg_checker[@mod(k + line_index, 2)] });
                try fg_text.addFmt(
                    "{x:02}",
                    .{self.values[16 * line_index + k]},
                    .{
                        .hor = .center,
                        .ver = .median,
                        .pos = bg_rects.getLast().pos.getCenter(),
                    },
                    0.75,
                    COLORS.text,
                );
            }
        }

        canvas.fillRects(camera, bg_rects.items);
        fg_text.draw(camera);
    }
};

pub const GameState = @This();
const PlatformGives = @import("../../game.zig").PlatformGives;

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "hexditor",
        .author = "knexator",
        .desired_aspect_ratio = 16.0 / 17.0,
    },

    .sounds = .{},

    .loops = .{},

    .preloaded_images = .{
        .arial_atlas = "fonts/Arial.png",
    },
};

pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

canvas: Canvas,
mem: Mem,

core: Hexditor,

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
) !void {
    dst.mem = .init(gpa);
    dst.canvas = try .init(gl, gpa, &.{@embedFile("../../fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)});
    dst.core = .{};
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    self.canvas.deinit(undefined, gpa);
    // self.mem.deinit();
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    _ = self;
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    _ = self.mem.frame.reset(.retain_capacity);
    _ = self.mem.scratch.reset(.retain_capacity);

    if (self.core.input_state.update(platform)) |instruction| {
        self.core.process(instruction);
    }

    try self.core.draw(&self.canvas, (Rect{
        .top_left = .zero,
        .size = .new(16, 17),
    }).withAspectRatio(
        platform.aspect_ratio,
        .grow,
        .top_center,
    ));
    return false;
}

const std = @import("std");
const assert = std.debug.assert;
const panic = std.debug.panic;

const kommon = @import("kommon");
const Triangulator = kommon.Triangulator;
const math = kommon.math;
const tof32 = math.tof32;
const Color = math.UColor;
const FColor = math.FColor;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const IVec2 = math.IVec2;
const funk = kommon.funktional;
const maybeMirror = math.maybeMirror;
const Noise = kommon.Noise;
const last = kommon.last;
pub const Mouse = kommon.input.Mouse;
pub const Keyboard = kommon.input.Keyboard;
pub const KeyboardButton = kommon.input.KeyboardButton;
pub const PrecomputedShape = kommon.renderer.PrecomputedShape;
pub const RenderableInfo = kommon.renderer.RenderableInfo;
pub const Gl = kommon.Gl;
pub const Canvas = kommon.Canvas;
pub const TextRenderer = Canvas.TextRenderer;
pub const Mem = @import("../tres_undos/GameState.zig").Mem;
pub const Key = @import("../akari/GameState.zig").Key;
pub const LazyState = @import("../akari/GameState.zig").LazyState;
pub const EdgePos = kommon.grid2D.EdgePos;
