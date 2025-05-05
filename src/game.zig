//! must not import anything from main.zig, the platform layer

// https://nullprogram.com/blog/2014/12/23/

pub const metadata = .{
    .name = "Snakanake",
    .author = "knexator",
    .desired_aspect_ratio = 1.0,
};

pub const sounds = .{
    .apple = "sounds/apple.wav",
};

pub const PlatformGives = struct {
    gpa: std.mem.Allocator,
    render_queue: *RenderQueue,
    getMouse: *const fn (camera: Rect) Mouse,
    keyboard: Keyboard,
    aspect_ratio: f32,
    delta_seconds: f32,
    // idk if this should be given by the platform
    global_seconds: f32,
    sound_queue: *std.EnumSet(std.meta.FieldEnum(@TypeOf(sounds))),
};

pub const GameState = struct {
    const BOARD_SIZE: UVec2 = .new(16, 16);
    const MAX_TURNS = 96;
    const TURN_DURATION = 0.15;
    const SNAKE_LENGTH = 4;

    const COLORS = struct {
        BACKGROUND: Color = .fromHex("#1a1c2c"),
        CLOCK: struct {
            NORMAL: Color = .fromHex("#333c57"),
            DANGER: Color = .fromHex("#29366f"),
            ACTIVE: Color = .fromHex("#5d275d"),
            DANGER_ACTIVE: Color = .fromHex("#b13e53"),
        } = .{},
        APPLE: Color = .fromHex("#a7f070"),
        APPLE_WARNING: Color = .fromHex("#38b764"),
        TEXT: Color = .fromHex("#f4f4f4"),
        SNAKE: struct {
            PASSIVE: [SNAKE_LENGTH]Color = Color.gradient(
                SNAKE_LENGTH,
                .fromHex("#566c86"),
                .fromHex("#94b0c2"),
            ),
            ACTIVE: [SNAKE_LENGTH]Color = Color.gradient(
                SNAKE_LENGTH,
                .fromHex("#41a6f6"),
                .fromHex("#3b5dc9"),
            ),
        } = .{},
    }{};

    const initial_turn = 96 / 3;
    const initial_pos: IVec2 = .new(8, 8);

    state: enum { waiting, main, lost } = .waiting,
    score: i32 = 0,
    turn: i32 = initial_turn,
    /// always in -1..1
    turn_offset: f32 = 0,
    time_reversed: bool = false,
    remaining_skip_turns: usize = 0,
    input_queue: kommon.CircularBuffer(IVec2, 32) = .init,
    cur_turn_duration: f32 = TURN_DURATION,
    body: std.SegmentedList(BodyPart, BOARD_SIZE.x * BOARD_SIZE.y) = .{},
    // blk: {
    //     var result: @FieldType(GameState, "body") = .{};
    //     result.append(undefined, .{
    //         .pos = initial_pos,
    //         .t = initial_turn,
    //         .dir = .zero,
    //         .time_reversed = false,
    //     }) catch unreachable;
    //     break :blk result;
    // },
    changes: std.SegmentedList(Change, 32) = .{},
    cur_apple: IVec2 = undefined,
    rnd_instance: std.Random.DefaultPrng,
    cur_screen_shake: struct { pos: Vec2 = .zero, target_mag: f32 = 0, actual_mag: f32 = 0 } = .{},
    cam_noise: Noise = .{},

    const BodyPart = struct { pos: IVec2, t: i32, dir: IVec2, time_reversed: bool };
    const Change = struct { pos: IVec2, t: i32, time_reversed: bool };

    // TODO: should take an allocator?
    pub fn init() GameState {
        // TODO: get random seed as param?
        var result: GameState = .{
            .rnd_instance = .init(0),
        };
        result.restart();
        return result;
    }

    pub fn deinit(self: *GameState, gpa: std.mem.Allocator) void {
        self.body.deinit(gpa);
    }

    fn findApplePlace(self: *GameState) IVec2 {
        const last = self.body.at(self.body.count() - 1);
        while (true) {
            // TODO: decide the semantics of URect
            const cur = math.Random.init(self.rnd_instance.random()).inURect(.{
                .top_left = .zero,
                .inner_size = BOARD_SIZE,
            }).cast(isize);
            const valid = !cur.equals(last.pos) and !cur.equals(last.pos.add(last.dir));
            if (valid) return cur;
        }
    }

    fn restart(self: *GameState) void {
        self.state = .waiting;
        self.input_queue.clear();
        self.turn = initial_turn;
        self.body.clearRetainingCapacity();
        self.body.append(undefined, .{
            .pos = initial_pos,
            .t = initial_turn,
            .dir = .zero,
            .time_reversed = false,
        }) catch unreachable;
        self.turn_offset = 0;
        self.time_reversed = false;
        self.cur_screen_shake.target_mag = 0;
        self.cur_turn_duration = TURN_DURATION;
        self.remaining_skip_turns = 0;
        self.changes.clearRetainingCapacity();
        self.score = 0;
        self.cur_apple = self.findApplePlace();
    }

    /// returns true if should quit
    pub fn update(self: *GameState, platform: PlatformGives) !bool {
        if (platform.keyboard.wasPressed(.KeyR))
            self.restart();

        if (platform.keyboard.wasPressed(.Escape))
            return true;

        // TODO: better
        if (platform.keyboard.wasPressed(.KeyW)) {
            self.input_queue.append(.new(0, -1)) catch {};
            if (self.state == .lost) self.restart();
            if (self.state == .waiting) self.state = .main;
        }
        if (platform.keyboard.wasPressed(.KeyS)) {
            self.input_queue.append(.new(0, 1)) catch {};
            if (self.state == .lost) self.restart();
            if (self.state == .waiting) self.state = .main;
        }
        if (platform.keyboard.wasPressed(.KeyD)) {
            self.input_queue.append(.new(1, 0)) catch {};
            if (self.state == .lost) self.restart();
            if (self.state == .waiting) self.state = .main;
        }
        if (platform.keyboard.wasPressed(.KeyA)) {
            self.input_queue.append(.new(-1, 0)) catch {};
            if (self.state == .lost) self.restart();
            if (self.state == .waiting) self.state = .main;
        }

        if (self.state == .main)
            self.turn_offset += maybeMirror(platform.delta_seconds, self.time_reversed) / self.cur_turn_duration;

        while (@abs(self.turn_offset) >= 1) {
            assert((self.turn_offset < 0) == self.time_reversed);
            self.turn_offset -= maybeMirror(@as(f32, 1.0), self.time_reversed);
            self.turn += maybeMirror(@as(i32, 1), self.time_reversed);
            // stepSound.play();

            if (self.remaining_skip_turns > 0) {
                self.remaining_skip_turns -= 1;
                if (self.remaining_skip_turns == 0) {
                    self.time_reversed = !self.time_reversed;
                    self.cur_apple = self.findApplePlace();
                    self.cur_turn_duration = TURN_DURATION;
                }
            } else {
                const last = self.body.at(self.body.len - 1);
                const d: IVec2 = get_next_dir: {
                    while (self.input_queue.popFirst()) |d| process_input: {
                        if (d.magSq() != 1) break :process_input;
                        if (d.neg().equals(last.dir)) break :process_input;
                        break :get_next_dir d;
                    }
                    if (last.dir.equals(.zero)) break :get_next_dir .new(1, 0);
                    break :get_next_dir last.dir;
                };

                const new_head_pos = last.pos.add(d).mod(BOARD_SIZE);
                try self.body.append(platform.gpa, .{
                    .pos = new_head_pos,
                    .dir = d,
                    .t = self.turn,
                    .time_reversed = self.time_reversed,
                });

                const collision: bool = blk: {
                    for (0..SNAKE_LENGTH) |k| {
                        if (self.body.count() - 1 < k) break;
                        const active_part = self.body.at(self.body.count() - k - 1);
                        if (active_part.time_reversed != self.time_reversed) break;

                        var it = self.body.constIterator(self.body.count() - k - 1);
                        _ = it.prev();
                        while (it.prev()) |other_part| {
                            const dt = maybeMirror(self.turn - other_part.t, other_part.time_reversed);
                            if (other_part.pos.equals(active_part.pos) and
                                math.inRange(dt, 0, SNAKE_LENGTH))
                            {
                                break :blk true;
                            }
                        }
                    }
                    break :blk false;
                };
                if (collision) {
                    // crashSound.play();
                    self.state = .lost;
                }

                if (new_head_pos.equals(self.cur_apple)) {
                    self.score += 1;
                    platform.sound_queue.insert(.apple);
                    self.cur_screen_shake.actual_mag = 100.0;
                    self.remaining_skip_turns = SNAKE_LENGTH - 1;
                    self.cur_apple = .new(-1, -1);
                    self.cur_turn_duration = TURN_DURATION / math.tof32(SNAKE_LENGTH + 1);
                    try self.changes.append(platform.gpa, .{
                        .pos = new_head_pos,
                        .t = self.turn,
                        .time_reversed = self.time_reversed,
                    });
                }
            }
        }

        try platform.render_queue.clear(COLORS.BACKGROUND);

        const cur_shake_mag = self.cur_screen_shake.actual_mag * (1.0 + @cos(platform.global_seconds * 0.25) * 0.25) / 32.0;
        const cur_shake_phase = self.cam_noise.genNoise2D(platform.global_seconds * 100, 0);
        self.cur_screen_shake.pos = .fromPolar(cur_shake_mag, cur_shake_phase);
        if (self.state != .main) self.cur_screen_shake.target_mag = 0;
        math.towards(&self.cur_screen_shake.actual_mag, self.cur_screen_shake.target_mag, platform.delta_seconds * 1000);

        const camera: Rect = (Rect{
            .top_left = self.cur_screen_shake.pos,
            .size = BOARD_SIZE.tof32(),
        }).withAspectRatio(platform.aspect_ratio, .grow, .center);

        const renderer: Renderer = .{
            .render_queue = platform.render_queue,
            .camera = camera,
        };

        const screen_center = BOARD_SIZE.tof32().scale(0.5);
        const half_side = math.tof32(BOARD_SIZE.x / 2);
        const t = (math.tof32(self.turn) + self.turn_offset) / math.tof32(MAX_TURNS);
        const danger_size = 0.03;
        const distance_to_danger = @min(t, 1.0 - t);

        if (distance_to_danger <= 0 and self.state == .main) self.state = .lost;

        {
            const arc_color = if (distance_to_danger > 0.2) blk: {
                self.cur_screen_shake.target_mag = 0;
                // alarmSound.volume(0)
                break :blk COLORS.CLOCK.DANGER;
            } else blk: {
                const val = std.math.lerp(1.0, 3.0 / 16.0, distance_to_danger / 0.2) * 255;
                self.cur_screen_shake.target_mag = val * 0.05;
                // alarmSound.volume((1. - distance_to_danger / .2) * .5)
                break :blk Color.lerp(COLORS.CLOCK.DANGER_ACTIVE, COLORS.CLOCK.DANGER, distance_to_danger / 0.2);
            };

            try renderer.fillArc(
                screen_center,
                half_side - 1.5,
                0.25 - danger_size,
                0.25 + danger_size,
                arc_color,
            );
        }

        {
            const clock_color = if (distance_to_danger > 0.15)
                COLORS.CLOCK.NORMAL
            else
                Color.lerp(COLORS.CLOCK.ACTIVE, COLORS.CLOCK.NORMAL, distance_to_danger / 0.15);

            try renderer.fillCrown(screen_center, half_side - 1.5, 1.5, clock_color);

            try platform.render_queue.drawShape(camera, .{
                .pos = screen_center,
                .turns = math.lerp(-0.75 + danger_size, 0.25 - danger_size, t),
            }, &.{
                .new(0, -1),
                .new(half_side - 3, 0),
                .new(0, 1),
            }, null, clock_color);

            try renderer.fillCircle(screen_center, 1.5, clock_color);
        }

        {
            var it = self.changes.constIterator(0);
            while (it.next()) |change| {
                const dt = maybeMirror(self.turn - change.t, change.time_reversed);
                if (math.inRange(dt, 0, SNAKE_LENGTH)) {
                    try renderer.fillRect(.{ .top_left = change.pos.tof32().sub(.half), .size = .both(2) }, COLORS.APPLE_WARNING);
                }
            }
        }

        {
            var it = self.body.constIterator(self.body.count());
            var in_active_snake = true;
            while (it.prev()) |part| {
                const dt = maybeMirror(self.turn - part.t, part.time_reversed);
                if (part.time_reversed != self.time_reversed) in_active_snake = false;
                if (dt >= SNAKE_LENGTH) in_active_snake = false;

                if (in_active_snake) {
                    try renderer.fillTile(part.pos, COLORS.SNAKE.ACTIVE[@intCast(dt)]);
                } else if (math.inRange(dt, 0, SNAKE_LENGTH)) {
                    try renderer.fillTile(part.pos, COLORS.SNAKE.PASSIVE[@intCast(dt)]);
                }
            }
        }

        try renderer.fillTile(self.cur_apple, COLORS.APPLE);

        return false;
    }
};

const Renderer = struct {
    render_queue: *RenderQueue,
    camera: Rect,

    const CIRCLE_RESOLUTION = 128;

    fn fillRect(self: Renderer, rect: Rect, color: Color) !void {
        try self.render_queue.drawShape(self.camera, .{
            .pos = rect.top_left,
        }, &.{
            .new(0, 0),
            .new(rect.size.x, 0),
            .new(rect.size.x, rect.size.y),
            .new(0, rect.size.y),
        }, null, color);
    }

    fn fillTile(self: Renderer, pos: IVec2, color: Color) !void {
        try self.fillRect(.{ .top_left = pos.tof32(), .size = .one }, color);
    }

    fn fillArc(self: Renderer, center: Vec2, radius: f32, turns_start: f32, turns_end: f32, color: Color) !void {
        try self.render_queue.drawShape(self.camera, .{
            .pos = center,
            .scale = radius,
            .turns = turns_start,
        }, &(funk.fromCountAndCtx(CIRCLE_RESOLUTION, struct {
            // TODO: maybe use linspace
            pub fn anon(n: usize, angle_delta: f32) Vec2 {
                return Vec2.fromTurns(angle_delta * math.tof32(n) / math.tof32(CIRCLE_RESOLUTION));
            }
        }.anon, turns_end - turns_start) ++ [1]Vec2{.zero}), null, color);
    }

    fn fillCircle(self: Renderer, center: Vec2, radius: f32, color: Color) !void {
        try self.render_queue.drawShape(self.camera, .{
            .pos = center,
            .scale = radius,
        }, &funk.map(Vec2.fromTurns, &funk.linspace01(CIRCLE_RESOLUTION, false)), null, color);
    }

    fn fillCrown(self: Renderer, center: Vec2, radius: f32, width: f32, color: Color) !void {
        try self.render_queue.drawShape(self.camera, .{
            .pos = center,
        }, &(funk.mapWithCtx(
            Vec2.fromPolar,
            &funk.linspace(1, 0, CIRCLE_RESOLUTION, true),
            radius - width / 2,
        ) ++
            funk.mapWithCtx(
                Vec2.fromPolar,
                &funk.linspace(0, 1, CIRCLE_RESOLUTION, true),
                radius + width / 2,
            )), null, color);
    }
};

pub const CApi = extern struct {
    update: *const @TypeOf(_update),
    reload: *const @TypeOf(_reload),

    fn _update(game: *GameState, platform_gives: *const PlatformGives) callconv(.c) bool {
        return game.update(platform_gives.*) catch unreachable;
    }

    fn _reload(dst: *GameState, gpa: *const std.mem.Allocator) callconv(.c) void {
        dst.deinit(gpa.*);
        dst.* = .init();
    }
};

pub export const game_api: CApi = .{
    .reload = CApi._reload,
    .update = CApi._update,
};

test "foo" {
    try std.testing.expect(true);
}

const std = @import("std");
const assert = std.debug.assert;

const kommon = @import("kommon");
const math = kommon.math;
const Color = math.Color;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const UVec2 = math.UVec2;
const IVec2 = math.IVec2;
const funk = kommon.funktional;
const maybeMirror = math.maybeMirror;
const Noise = kommon.Noise;
pub const Mouse = kommon.input.Mouse;
pub const Keyboard = kommon.input.Keyboard;
pub const KeyboardButton = kommon.input.KeyboardButton;
pub const RenderQueue = @import("renderer.zig").RenderQueue;
