pub const GameState = @This();
pub const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
pub export const game_api: kommon.engine.CApiFor(GameState) = .{};

const core = @import("core.zig");
const Drawer = @import("Drawer.zig");

// TODO: type
pub const stuff = .{
    .metadata = .{
        .name = "vaulogy",
        .author = "knexator",
        .desired_aspect_ratio = 16.0 / 9.0,
    },
    .sounds = .{},
    .loops = .{},
    .preloaded_images = .{
        // TODO: don't require this here
        .arial_atlas = "fonts/Arial.png",
    },
};
pub const Images = std.meta.FieldEnum(@FieldType(@TypeOf(stuff), "preloaded_images"));

var COLORS: struct {
    bg: FColor = .gray(0.5),
} = .{};

usual: kommon.Usual,

drawer: Drawer,
camera: Rect = .fromCenterAndSize(.zero, .new(16, 9)),
core_mem: core.VeryPermamentGameStuff,
scoring_run: core.ScoringRun,
execution_thread: core.ExecutionThread,
anim_t: f32 = 0.99,
result: ?core.ExecutionThread.Result = null,

pub fn init(
    dst: *GameState,
    gpa: std.mem.Allocator,
    gl: Gl,
    loaded_images: std.EnumArray(Images, *const anyopaque),
    random_seed: u64,
    // tweakable: type,
    // tweakable: struct {
    //     fcolor: fn (name: []const u8, value: *FColor) void,
    // },
) !void {
    dst.* = kommon.meta.initDefaultFields(GameState);

    dst.usual.init(
        gpa,
        random_seed,
        try .init(gl, gpa, &.{@embedFile("fonts/Arial.json")}, &.{loaded_images.get(.arial_atlas)}),
    );

    // tweakable.fcolor("bg", &COLORS.bg);

    dst.drawer = try .init(&dst.usual);

    dst.core_mem = .init(gpa);
    dst.scoring_run = try .init(
        \\ peanoSum {
        \\     (@a . nil) -> @a;
        \\     (@a . (true . @b)) -> peanoSum: ((true . @a) . @b);
        \\ }
        \\
        \\ peanoMul {
        \\     (@a . nil) -> nil;
        \\     (@a . (true . @b)) -> peanoMul: (@a . @b) {
        \\         @ab -> peanoSum: (@ab . @a);
        \\     }
        \\ }
    , &dst.core_mem);
    dst.execution_thread = try .initFromText("((true . (true . nil)) . (true . (true . nil)))", "peanoMul", &dst.scoring_run);
}

// TODO: take gl parameter
pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
    _ = gpa;
    self.usual.deinit(undefined);
}

pub fn beforeHotReload(self: *GameState) !void {
    _ = self;
}

pub fn afterHotReload(self: *GameState) !void {
    _ = self;
}

/// returns true if should quit
pub fn update(self: *GameState, platform: PlatformGives) !bool {
    self.usual.frameStarted(platform);
    // const mem = &self.usual.mem;
    // const canvas = &self.usual.canvas;

    const camera = self.camera.withAspectRatio(platform.aspect_ratio, .grow, .top_left);
    const mouse = platform.getMouse(camera);
    defer self.camera = moveCamera(self.camera, platform.delta_seconds, platform.keyboard, mouse);

    platform.gl.clear(COLORS.bg);

    if (platform.keyboard.cur.isDown(.KeyE)) {
        self.anim_t += platform.delta_seconds;
    }
    if (platform.keyboard.cur.isDown(.KeyQ)) {
        self.anim_t -= platform.delta_seconds;
    }
    self.anim_t = math.clamp01(self.anim_t);

    while (self.anim_t >= 1 and self.result == null) {
        self.anim_t -= 1;
        self.result = try self.execution_thread.advanceTinyStep(&self.scoring_run);
    }

    try self.draw(camera);

    return false;
}

fn drawCase(drawer: *Drawer, camera: Rect, template_point: Point, case: core.MatchCaseDefinition, unfolded: f32) !void {
    assert(math.in01(unfolded));

    try drawer.drawSexpr(camera, .{
        .is_pattern = 1,
        .value = case.pattern,
        .pos = template_point,
    });

    if (unfolded > 0) {
        try drawer.drawSexpr(camera, .{
            .is_pattern = 0,
            .value = case.template,
            .pos = template_point.applyToLocalPoint(.{ .pos = .new(2, 0) }),
        });
    }
}

fn draw(self: *GameState, camera: Rect) !void {
    if (self.result != null) return;

    const template_point: Point = .{};
    var it = std.mem.reverseIterator(self.execution_thread.stack.items);
    switch (self.execution_thread.last_visual_state) {
        .failed_to_match => |discarded_case| {
            try self.drawer.drawSexpr(camera, .{
                .is_pattern = 0,
                .value = self.execution_thread.active_value,
                .pos = .{},
            });

            const match_dist = math.remapClamped(self.anim_t, 0, 0.2, 1, 0);

            const fly_away = math.remapClamped(self.anim_t, 0.2, 0.8, 0, 1);
            const pattern_point_floating_away = template_point.applyToLocalPoint(Point.lerp(
                .{ .pos = .new(math.remap01(match_dist, 3, 4), 0) },
                .{ .pos = .new(8, -8), .scale = 0, .turns = -0.65 },
                fly_away,
            ));
            try drawCase(&self.drawer, camera, pattern_point_floating_away, discarded_case, 1);

            const offset = math.remapClamped(self.anim_t, 0.2, 1, 1, 0);
            const active_stack: core.StackThing = it.next().?;
            for (active_stack.cur_cases, 0..) |case, k| {
                try drawCase(&self.drawer, camera, .{ .pos = .new(4, (tof32(k) + offset) * 3) }, case, if (k > 0) 0 else 1.0 - offset);
            }
        },
        .matched => |matched| {
            try self.drawer.drawSexpr(camera, .{
                .is_pattern = 0,
                .value = matched.old_active_value,
                .pos = .{},
            });

            const match_dist = math.remapClamped(self.anim_t, 0, 0.2, 1, 0);
            try drawCase(&self.drawer, camera, template_point.applyToLocalPoint(
                .{ .pos = .new(math.remap01(match_dist, 3, 4), 0) },
            ), matched.case, 1);
            // const match_dist = math.remapClamped(self.anim_t, 0, 0.2, 1, 0);
            // _ = matched;
            // const active_stack: core.StackThing = it.next().?;
            // for (active_stack.cur_cases, 0..) |case, k| {
            //     try drawCase(&self.drawer, camera, .{ .pos = .new(4, tof32(k) * 3) }, case, if (k == 0) 1 else 0);
            // }
        },
        else => {},
        // inline else => |_, t| std.log.debug("unhandled: {s}", .{@tagName(t)}),
    }

    // try self.drawer.drawSexpr(camera, .{
    //     .is_pattern = 0,
    //     .value = self.execution_thread.active_value,
    //     .pos = .{},
    // });
    // const asdf = self.execution_thread.stack.getLast();
    // for (asdf.cur_cases, 0..) |case, k| {
    //     try self.drawer.drawSexpr(camera, .{
    //         .is_pattern = 1,
    //         .value = case.pattern,
    //         .pos = .{ .pos = .new(4, tof32(k * 3)) },
    //     });

    //     try self.drawer.drawSexpr(camera, .{
    //         .is_pattern = 0,
    //         .value = case.template,
    //         .pos = .{ .pos = .new(6, tof32(k * 3)) },
    //     });

    //     if (k == 1) {
    //         if (case.next) |next| {
    //             for (next.items) |case2| {
    //                 try self.drawer.drawSexpr(camera, .{
    //                     .is_pattern = 1,
    //                     .value = case2.pattern,
    //                     .pos = .{ .pos = .new(10, tof32(k * 3)) },
    //                 });

    //                 try self.drawer.drawSexpr(camera, .{
    //                     .is_pattern = 0,
    //                     .value = case2.template,
    //                     .pos = .{ .pos = .new(12, tof32(k * 3)) },
    //                 });
    //             }
    //         }
    //     }
    // }
}

fn moveCamera(camera: Rect, delta_seconds: f32, keyboard: Keyboard, mouse: Mouse) Rect {
    var result = camera;
    const mouse_pos = mouse.cur.position;

    result = result.zoom(mouse_pos, switch (mouse.cur.scrolled) {
        .none => 1.0,
        .down => 1.1,
        .up => 0.9,
    });

    inline for (KeyboardButton.directional_keys) |kv| {
        for (kv.keys) |key| {
            if (keyboard.cur.isDown(key)) {
                result.top_left.addInPlace(kv.dir.scale(delta_seconds * camera.size.y));
            }
        }
    }

    if (mouse.cur.isDown(.middle) and mouse.prev.isDown(.middle)) {
        result.top_left.addInPlace(mouse.deltaPos());
    }

    return result;
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
pub const Mem = kommon.Mem;
pub const Key = kommon.Key;
pub const LazyState = kommon.LazyState;
pub const EdgePos = kommon.grid2D.EdgePos;
// pub const LocalDecisions = @import("../chesstory/GameState.zig").LocalDecisions;

const Atom = core.Atom;
const Pair = core.Pair;
const Sexpr = core.Sexpr;
const Fnk = core.Fnk;
const FnkBody = core.FnkBody;
const FnkCollection = core.FnkCollection;
const VeryPermamentGameStuff = core.VeryPermamentGameStuff;
const parsing = @import("parsing.zig");

const PhysicalSexpr = @import("physical.zig").PhysicalSexpr;
const ViewHelper = @import("physical.zig").ViewHelper;
