//! must not import anything from main.zig, the platform layer

// https://nullprogram.com/blog/2014/12/23/

pub const metadata = .{
    .name = "Snakanake",
    .author = "knexator",
    .desired_aspect_ratio = 1.0,
};

// TODO(eternal): support formats other than .wav
pub const sounds = .{
    .apple = "sounds/apple.wav",
    .crash = "sounds/crash.wav",
    .step = "sounds/step1.wav",
};

pub const loops = .{
    .alarm = "sounds/alarm.wav",
    .music = "sounds/music.wav",
};

pub const preloaded_images = .{
    .arial_atlas = "fonts/Arial.png",
};

pub const Images = std.meta.FieldEnum(@TypeOf(preloaded_images));

const fill_shape_info: RenderableInfo = .{
    .VertexData = extern struct { position: Vec2 },
    .IndexType = u16,
    .UniformTypes = struct {
        color: FColor,
        rect: Rect,
        point: Point,
    },
    .vertex =
    \\uniform vec4 u_rect; // as top_left, size
    \\uniform vec4 u_point; // as pos, turns, scale
    \\
    \\in vec2 a_position;
    \\#define TAU 6.283185307179586
    \\void main() {
    \\  float c = cos(u_point.z * TAU);
    \\  float s = sin(u_point.z * TAU);
    \\  vec2 world_position = u_point.xy + u_point.w * (mat2x2(c,s,-s,c) * a_position);
    \\  vec2 camera_position = (world_position - u_rect.xy) / u_rect.zw;
    \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
    \\}
    ,
    .fragment =
    \\precision highp float;
    \\out vec4 out_color;
    \\
    \\uniform vec4 u_color;
    \\void main() {
    \\  out_color = u_color;
    \\}
    ,
};

pub const renderables = .{
    .fill_shape = fill_shape_info,
};

// TODO: small text looks worse on the web version!
const TextRenderer = struct {
    atlas_texture: Gl.Texture,
    renderable: Gl.Renderable,
    font_info: std.json.Parsed(FontJsonInfo),

    const RectSides = struct {
        left: f32,
        bottom: f32,
        right: f32,
        top: f32,
    };
    const FontJsonInfo = struct {
        atlas: struct {
            type: enum { msdf },
            distanceRange: f32,
            distanceRangeMiddle: f32,
            /// pixels per em
            size: f32,
            width: usize,
            height: usize,
            yOrigin: enum { top },
        },
        metrics: struct {
            emSize: f32,
            lineHeight: f32,
            /// ??
            ascender: f32,
            /// ??
            descender: f32,
            /// ??
            underlineY: f32,
            /// ??
            underlineThickness: f32,
        },
        glyphs: []struct {
            unicode: u8,
            advance: f32,
            planeBounds: ?RectSides = null,
            atlasBounds: ?RectSides = null,
        },
        kerning: []struct {
            unicode1: u8,
            unicode2: u8,
            advance: f32,
        },
    };

    pub fn init(comptime font_name: []const u8, gpa: std.mem.Allocator, gl: Gl, atlas_image: *const anyopaque) !TextRenderer {
        return .{
            .atlas_texture = gl.buildTexture2D(atlas_image),
            // TODO: parse the font data at comptime
            .font_info = try std.json.parseFromSlice(
                FontJsonInfo,
                gpa,
                @embedFile("./fonts/" ++ font_name ++ ".json"),
                .{},
            ),
            .renderable = try gl.buildRenderable(
                \\in vec2 a_position;
                \\in vec2 a_texcoord;
                \\
                \\out vec2 v_texcoord;
                \\
                \\// 0,0 => -1,1 (top left)
                \\// 1,0 => 1,1 (top right)
                \\vec2 clipFromCam(vec2 p) {
                \\  return (p * 2.0 - 1.0) * vec2(1, -1);
                \\}
                \\
                \\void main() {
                \\  v_texcoord = a_texcoord;
                \\  gl_Position = vec4(clipFromCam(a_position), 0, 1);
                \\}
            ,
                \\precision highp float;
                \\out vec4 out_color;
                \\
                \\// for some reason, on desktop, the fwidth value is half of what it should.
                \\#ifdef GL_ES // WebGL2
                \\  #define FWIDTH(x) fwidth(x)
                \\#else // Desktop
                \\  #define FWIDTH(x) (2.0 * fwidth(x))
                \\#endif
                \\
                \\in vec2 v_texcoord;
                \\uniform sampler2D u_texture;
                \\
                \\float median(float r, float g, float b) {
                \\  return max(min(r, g), min(max(r, g), b));
                \\}
                \\
                \\float inverseLerp(float a, float b, float t) {
                \\  return (t - a) / (b - a);
                \\}
                \\
                \\void main() {
                \\  // assume square texture
                \\  float sdf_texture_size = float(textureSize(u_texture, 0).x);
                \\  // the values in the sdf texture should be remapped to (-sdf_pxrange/2, +sdf_pxrange/2)
                // TODO: get sdf_pxrange from the font data
                \\  float sdf_pxrange = 2.0;
                \\  vec3 raw = texture(u_texture, v_texcoord).rgb;
                \\  float distance_in_texels = (median(raw.r, raw.g, raw.b) - 0.5) * sdf_pxrange;
                \\  // density of the texture on screen; assume uniform scaling.
                \\  float texels_per_pixel = FWIDTH(v_texcoord.x) * sdf_texture_size;
                \\  float distance_in_pixels = distance_in_texels / texels_per_pixel;
                \\  // over how many screen pixels do the transition
                \\  float transition_pixels = 1.0;
                \\  float alpha = clamp(inverseLerp(-transition_pixels / 2.0, transition_pixels / 2.0, distance_in_pixels), 0.0, 1.0);
                \\  // TODO: premultiply alpha?
                \\  out_color = vec4(vec3(1.0), alpha);
                \\}
            ,
                .{ .attribs = &.{
                    .{ .name = "a_position", .kind = .Vec2 },
                    .{ .name = "a_texcoord", .kind = .Vec2 },
                } },
                &.{},
            ),
        };
    }

    pub fn deinit(self: *TextRenderer) void {
        self.font_info.deinit();
        // TODO
        // gl.destroyRenderable(self.renderable);
        // TODO
        // gl.DeleteTextures(1, @ptrCast(&self.texture));
    }

    // TODO: kerning
    // TODO: single draw call, maybe
    pub fn drawText(self: TextRenderer, gl: Gl, camera: Rect, bottom_left: Vec2, text: []const u8, em: f32) void {
        var cursor: Vec2 = bottom_left;
        for (text) |char| {
            cursor = self.drawLetter(gl, camera, cursor, char, em);
        }
    }

    // TODO: use a map
    pub fn drawLetter(self: TextRenderer, gl: Gl, camera: Rect, bottom_left: Vec2, letter: u8, em: f32) Vec2 {
        const glyph_info = blk: {
            for (self.font_info.value.glyphs) |glyph| {
                if (glyph.unicode == letter) break :blk glyph;
            } else unreachable;
        };
        if (glyph_info.atlasBounds) |b| {
            const s = UVec2.new(
                self.font_info.value.atlas.width,
                self.font_info.value.atlas.height,
            ).tof32();
            const p = glyph_info.planeBounds orelse unreachable;

            // TODO: use a better api
            const VertexData = extern struct { position: Vec2, texcoord: Vec2 };
            gl.useRenderable(self.renderable, &[4]VertexData{
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.left, p.bottom).scale(em))),
                    .texcoord = Vec2.new(b.left, b.bottom).div(s),
                },
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.right, p.bottom).scale(em))),
                    .texcoord = Vec2.new(b.right, b.bottom).div(s),
                },
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.left, p.top).scale(em))),
                    .texcoord = Vec2.new(b.left, b.top).div(s),
                },
                .{
                    .position = camera.localFromWorldPosition(bottom_left
                        .add(Vec2.new(p.right, p.top).scale(em))),
                    .texcoord = Vec2.new(b.right, b.top).div(s),
                },
            }, 4 * @sizeOf(VertexData), &.{ .{ 0, 1, 2 }, .{ 3, 2, 1 } }, &.{}, self.atlas_texture);
        }

        return bottom_left.addX(em * glyph_info.advance);
    }
};

pub const PlatformGives = struct {
    gpa: std.mem.Allocator,
    getMouse: *const fn (camera: Rect) Mouse,
    keyboard: Keyboard,
    aspect_ratio: f32,
    delta_seconds: f32,
    // idk if this should be given by the platform
    global_seconds: f32,
    sound_queue: *std.EnumSet(std.meta.FieldEnum(@TypeOf(sounds))),
    loop_volumes: *std.EnumArray(std.meta.FieldEnum(@TypeOf(loops)), f32),
    gl: Gl,
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
    changes: std.SegmentedList(Change, 32) = .{},
    cur_apple: IVec2 = undefined,
    rnd_instance: std.Random.DefaultPrng,
    cur_screen_shake: struct { pos: Vec2 = .zero, target_mag: f32 = 0, actual_mag: f32 = 0 } = .{},
    cam_noise: Noise = .{},

    SHAPES: struct {
        circle: PrecomputedShape,
    },

    fill_shape: Gl.Renderable,
    text_renderer: TextRenderer,
    debug_fwidth: Gl.Renderable,

    const BodyPart = struct { pos: IVec2, t: i32, dir: IVec2, time_reversed: bool };
    const Change = struct { pos: IVec2, t: i32, time_reversed: bool };

    pub fn init(
        gpa: std.mem.Allocator,
        gl: Gl,
        loaded_images: std.EnumArray(Images, *const anyopaque),
    ) !GameState {
        // TODO: get random seed as param?
        var result: GameState = .{
            .rnd_instance = .init(0),
            .SHAPES = .{
                .circle = try .fromPoints(gpa, &funk.map(
                    Vec2.fromTurns,
                    &funk.linspace01(
                        128,
                        false,
                    ),
                )),
            },
            .fill_shape = try gl.buildRenderable(
                fill_shape_info.vertex,
                fill_shape_info.fragment,
                .{ .attribs = &.{
                    .{ .name = "a_position", .kind = .Vec2 },
                } },
                &.{
                    .{ .name = "u_rect", .kind = .Rect },
                    .{ .name = "u_point", .kind = .Point },
                    .{ .name = "u_color", .kind = .FColor },
                },
            ),
            // TODO: store this in kommon for later use
            .debug_fwidth = try gl.buildRenderable(
                \\in vec2 a_position;
                \\in vec2 a_texcoord;
                \\out vec2 v_texcoord;
                \\void main() {
                \\  v_texcoord = a_texcoord;
                \\  gl_Position = vec4(a_position, 0, 1);
                \\}
            ,
                \\precision highp float;
                \\out vec4 out_color;
                \\
                \\#ifdef GL_ES // WebGL2
                \\  #define FWIDTH(x) fwidth(x)
                \\#else // Desktop
                \\  #define FWIDTH(x) (2.0 * fwidth(x))
                \\#endif
                \\
                \\in vec2 v_texcoord;
                \\void main() {
                \\  float v = FWIDTH(v_texcoord.x) * 256.0;
                \\  out_color = vec4(vec3(v), 1.0);
                \\}
            ,
                .{ .attribs = &.{
                    .{ .name = "a_position", .kind = .Vec2 },
                    .{ .name = "a_texcoord", .kind = .Vec2 },
                } },
                &.{},
            ),
            .text_renderer = try .init("Arial", gpa, gl, loaded_images.get(.arial_atlas)),
        };
        result.restart();
        return result;
    }

    // TODO: take gl parameter
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
        platform.loop_volumes.set(.music, 1);

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
            platform.sound_queue.insert(.step);

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
                    platform.sound_queue.insert(.crash);
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

        platform.gl.clear(COLORS.BACKGROUND.toFColor());

        const cur_shake_mag = self.cur_screen_shake.actual_mag * (1.0 + @cos(platform.global_seconds * 0.25) * 0.25) / 32.0;
        const cur_shake_phase = self.cam_noise.genNoise2D(platform.global_seconds * 100, 0);
        self.cur_screen_shake.pos = .fromPolar(cur_shake_mag, cur_shake_phase);
        if (self.state != .main) self.cur_screen_shake.target_mag = 0;
        math.towards(&self.cur_screen_shake.actual_mag, self.cur_screen_shake.target_mag, platform.delta_seconds * 1000);

        // TODO: bug here, doesn't work properly for sdl fullscreen
        const camera: Rect = (Rect{
            .top_left = self.cur_screen_shake.pos,
            .size = BOARD_SIZE.tof32(),
        }).withAspectRatio(platform.aspect_ratio, .grow, .center);

        const renderer: Renderer = .{
            // TODO
            .scratch = platform.gpa,
            .camera = camera,
            .SHAPES = &self.SHAPES,
            .gl = platform.gl,
            .fill_shape = self.fill_shape,
        };

        if (false) {
            // TODO: store in kommon for later use
            // TODO: use a better api
            const VertexData = extern struct { position: Vec2, texcoord: Vec2 };
            defer platform.gl.useRenderable(self.debug_fwidth, &[4]VertexData{
                .{
                    .position = .new(-1, 1),
                    .texcoord = .new(0, 0),
                },
                .{
                    .position = .new(1, 1),
                    .texcoord = .new(1, 0),
                },
                .{
                    .position = .new(-1, -1),
                    .texcoord = .new(0, 1),
                },
                .{
                    .position = .new(1, -1),
                    .texcoord = .new(1, 1),
                },
            }, 4 * @sizeOf(VertexData), &.{ .{ 0, 1, 2 }, .{ 3, 2, 1 } }, &.{}, null);
        }

        const screen_center = BOARD_SIZE.tof32().scale(0.5);
        const half_side = math.tof32(BOARD_SIZE.x / 2);
        const t = (math.tof32(self.turn) + self.turn_offset) / math.tof32(MAX_TURNS);
        const danger_size = 0.03;
        const distance_to_danger = @min(t, 1.0 - t);

        if (distance_to_danger <= 0 and self.state == .main) self.state = .lost;

        {
            const arc_color = if (distance_to_danger > 0.2) blk: {
                self.cur_screen_shake.target_mag = 0;
                platform.loop_volumes.set(.alarm, 0);
                break :blk COLORS.CLOCK.DANGER;
            } else blk: {
                const val = std.math.lerp(1.0, 3.0 / 16.0, distance_to_danger / 0.2) * 255;
                self.cur_screen_shake.target_mag = val * 0.05;
                platform.loop_volumes.set(.alarm, (1.0 - distance_to_danger / 0.2) * 0.5);
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

            try renderer.fillShape(.{
                .pos = screen_center,
                .turns = math.lerp(-0.75 + danger_size, 0.25 - danger_size, t),
            }, &.{
                .new(0, -1),
                .new(half_side - 3, 0),
                .new(0, 1),
            }, clock_color);

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

        switch (self.state) {
            .waiting => {
                self.text_renderer.drawText(
                    platform.gl,
                    renderer.camera,
                    // TODO: set text's bottom center
                    BOARD_SIZE.tof32().mul(.new(0.5, 0.25)).addX(-6.25),
                    "WASD or Arrow Keys to move",
                    30.0 / 32.0,
                );
            },
            else => {},
        }

        return false;
    }
};

const Renderer = struct {
    camera: Rect,
    SHAPES: *const @FieldType(GameState, "SHAPES"),
    gl: Gl,
    fill_shape: Gl.Renderable,
    scratch: std.mem.Allocator,

    const CIRCLE_RESOLUTION = 128;

    fn fillRect(self: Renderer, rect: Rect, color: Color) !void {
        self.gl.useRenderable(self.fill_shape, &[4]Vec2{
            .new(0, 0),
            .new(rect.size.x, 0),
            .new(0, rect.size.y),
            .new(rect.size.x, rect.size.y),
        }, 4 * @sizeOf(Vec2), &.{ .{ 0, 1, 2 }, .{ 3, 2, 1 } }, &.{
            .{ .name = "u_color", .value = .{ .FColor = color.toFColor() } },
            .{ .name = "u_point", .value = .{ .Point = .{ .pos = rect.top_left } } },
            .{ .name = "u_rect", .value = .{ .Rect = self.camera } },
        }, null);
    }

    fn fillTile(self: Renderer, pos: IVec2, color: Color) !void {
        try self.fillRect(.{ .top_left = pos.tof32(), .size = .one }, color);
    }

    fn fillShape(
        self: Renderer,
        parent: Point,
        local_points: []const Vec2,
        color: Color,
    ) !void {
        // TODO: cache triangulation
        assert(local_points.len >= 3);
        const indices = try Triangulator.triangulate(Gl.IndexType, self.scratch, local_points);
        defer self.scratch.free(indices);

        self.gl.useRenderable(self.fill_shape, local_points.ptr, local_points.len * @sizeOf(Vec2), indices, &.{
            .{ .name = "u_color", .value = .{ .FColor = color.toFColor() } },
            .{ .name = "u_point", .value = .{ .Point = parent } },
            .{ .name = "u_rect", .value = .{ .Rect = self.camera } },
        }, null);
    }

    fn fillArc(self: Renderer, center: Vec2, radius: f32, turns_start: f32, turns_end: f32, color: Color) !void {
        try self.fillShape(.{
            .pos = center,
            .scale = radius,
            .turns = turns_start,
        }, &(funk.fromCountAndCtx(CIRCLE_RESOLUTION, struct {
            // TODO: maybe use linspace
            pub fn anon(n: usize, angle_delta: f32) Vec2 {
                return Vec2.fromTurns(angle_delta * math.tof32(n) / math.tof32(CIRCLE_RESOLUTION));
            }
        }.anon, turns_end - turns_start) ++ [1]Vec2{.zero}), color);
    }

    fn fillCircle(self: Renderer, center: Vec2, radius: f32, color: Color) !void {
        self.gl.useRenderable(
            self.fill_shape,
            self.SHAPES.circle.local_points.ptr,
            self.SHAPES.circle.local_points.len * @sizeOf(Vec2),
            self.SHAPES.circle.triangles,
            &.{
                .{ .name = "u_color", .value = .{ .FColor = color.toFColor() } },
                .{ .name = "u_point", .value = .{ .Point = .{ .pos = center, .scale = radius } } },
                .{ .name = "u_rect", .value = .{ .Rect = self.camera } },
            },
            null,
        );
    }

    fn fillCrown(self: Renderer, center: Vec2, radius: f32, width: f32, color: Color) !void {
        try self.fillShape(.{
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
            )), color);
    }
};

pub const CApi = extern struct {
    update: *const @TypeOf(_update),
    reload: *const @TypeOf(_reload),

    fn _update(game: *GameState, platform_gives: *const PlatformGives) callconv(.c) bool {
        return game.update(platform_gives.*) catch unreachable;
    }

    fn _reload(dst: *GameState, gpa: *const std.mem.Allocator, gl: *const Gl) callconv(.c) void {
        dst.deinit(gpa.*);
        // TODO
        dst.* = GameState.init(gpa.*, gl.*, undefined) catch unreachable;
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
const Triangulator = kommon.Triangulator;
const math = kommon.math;
const Color = math.Color;
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
pub const Mouse = kommon.input.Mouse;
pub const Keyboard = kommon.input.Keyboard;
pub const KeyboardButton = kommon.input.KeyboardButton;
pub const PrecomputedShape = @import("renderer.zig").PrecomputedShape;
pub const RenderableInfo = @import("renderer.zig").RenderableInfo;
pub const Gl = @import("./Gl.zig");
