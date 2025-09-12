const std = @import("std");
const assert = std.debug.assert;
const gl = @import("gl");
const zstbi = @import("zstbi");

const kommon = @import("kommon");
const funk = kommon.funktional;
const math = kommon.math;
const model = @import("main.zig");
const presenter = @import("presenter.zig");
const DESIGN = presenter.DESIGN;

const IndexType = Gl.IndexType;

const SdlPlatform = struct {
    // TODO: improve
    pub fn getPlayerData(mem: *model.VeryPermamentGameStuff) !?presenter.PlayerData {
        if (false) {
            return presenter.PlayerData.fromAscii(
                @embedFile("./my_save/vaulogy_player_data"),
                @embedFile("./my_save/vaulogy_player_data_custom_samples"),
                @embedFile("./my_save/vaulogy_player_data_fav_fnks"),
                mem,
            ) catch return null;
        } else {
            return presenter.PlayerData.fromAscii(
                std.fs.cwd().readFileAlloc(
                    mem.gpa,
                    "ungit/my_save/vaulogy_player_data",
                    std.math.maxInt(usize),
                ) catch return null,
                std.fs.cwd().readFileAlloc(
                    mem.gpa,
                    "ungit/my_save/vaulogy_player_data_custom_samples",
                    std.math.maxInt(usize),
                ) catch return null,
                std.fs.cwd().readFileAlloc(
                    mem.gpa,
                    "ungit/my_save/vaulogy_player_data_fav_fnks",
                    std.math.maxInt(usize),
                ) catch return null,
                mem,
            ) catch return null;
        }
    }

    pub fn setPlayerData(player_data: presenter.PlayerData, mem: *model.VeryPermamentGameStuff) !void {
        if (true) return;
        std.fs.cwd().makePath("ungit/my_save") catch unreachable;
        const ascii = try player_data.toAscii(mem.gpa);
        defer mem.gpa.free(ascii.fnks);
        defer mem.gpa.free(ascii.samples);
        defer mem.gpa.free(ascii.fav_fnks);
        std.fs.cwd().writeFile(.{ .sub_path = "ungit/my_save/vaulogy_player_data", .data = ascii.fnks }) catch unreachable;
        std.fs.cwd().writeFile(.{ .sub_path = "ungit/my_save/vaulogy_player_data_custom_samples", .data = ascii.samples }) catch unreachable;
        std.fs.cwd().writeFile(.{ .sub_path = "ungit/my_save/vaulogy_player_data_fav_fnks", .data = ascii.fav_fnks }) catch unreachable;
    }

    pub fn downloadPlayerData(player_data: presenter.PlayerData, alloc: std.mem.Allocator) !void {
        // TODO
        _ = player_data;
        _ = alloc;
    }

    pub fn uploadPlayerData() std.io.AnyReader {
        @panic("TODO");
    }

    pub fn getMouse() presenter.Mouse {
        return mouse;
    }

    pub fn getKeyboard() presenter.Keyboard {
        return keyboard;
    }

    pub fn setCursor(cursor: presenter.Platform.Cursor) void {
        _ = cursor;
        // TODO
    }
};

var gl_vtable: Gl = undefined;
var canvas: Canvas = undefined;

const Gl = kommon.Gl;
const Canvas = kommon.Canvas;
const tof32 = math.tof32;
const Camera = presenter.Camera;
const Point = presenter.Point;
const Vec2 = presenter.Vec2;
const Vec3 = math.Vec3;
const Rotor3 = math.Rotor3;
const Color = presenter.Color;
const FColor = math.FColor;
const Rect = presenter.Rect;
const Noise = kommon.Noise;
const SdlDrawer = struct {
    pub fn asdfBackground() void {
        const camera: Rect = .{ .top_left = .zero, .size = window_size.scale(10.0 / window_size.y) };
        // const color: FColor = .fromHex("#557119");
        const pixel_width = camera.size.y / window_size.y;

        const S = struct {
            pub const noise: Noise = .{};
            pub var t: f32 = 0;

            pub fn randDir(offset: f32) Vec3 {
                return Vec3.new(
                    noise.genNoise3D(t, 0, offset),
                    noise.genNoise3D(t, 50, offset),
                    noise.genNoise3D(t, 100, offset),
                ).normalized();
            }
        };
        S.t += 30.0 * 1.0 / 60.0;

        var rnd_instance: std.Random.DefaultPrng = .init(1);
        const rnd: math.Random = .init(rnd_instance.random());

        const N_THINGS = 10;

        var gpu_points = std.ArrayList(Canvas.InstancedShapeInfo).initCapacity(
            canvas.frame_arena.allocator(),
            700 * N_THINGS,
        ) catch @panic("OoM");

        const focus_dist = math.lerp(-1, 1, mouse.cur.client_pos.y);

        for (0..N_THINGS) |k| {
            const rotor: Rotor3 = .fromTwoVecs(
                rnd.direction3D().add(S.randDir(0 + tof32(k) * 100).scale(0.1)).normalized(),
                rnd.direction3D().add(S.randDir(50 + tof32(k) * 100).scale(0.1)).normalized(),
            );

            // https://en.wikipedia.org/wiki/Depth_of_field
            // https://en.wikipedia.org/wiki/Circle_of_confusion
            // https://developer.nvidia.com/gpugems/gpugems/part-iv-image-processing/chapter-23-depth-field-survey-techniques

            const center = rnd.inRect(camera).withZ(rnd.between(-1, 1));

            for (0..700) |_| {
                // const b = rnd.between(0, 1);

                const t = rnd.between(0, 1);
                const trefoil_pos: Vec3 = Vec3.new(
                    math.sin(t) + 2 * math.sin(2 * t),
                    math.cos(t) - 2 * math.cos(2 * t),
                    -math.sin(3 * t),
                ).scale(0.3).add(Vec3.new(
                    rnd.between(-1, 1),
                    rnd.between(-1, 1),
                    rnd.between(-1, 1),
                ).scale(0.05));

                const local_pos: Vec3 = rotor.rotate(trefoil_pos);

                // const local_pos: Vec3 = rotor.rotate(.new(
                //     rnd.between(-1, 1),
                //     rnd.between(-1, 1),
                //     rnd.between(-1, 1),
                // ));

                const pos = center.add(local_pos);

                const color: FColor = .lerp(
                    .fromHex("#557119"),
                    .fromHex("#646629"),
                    @abs(t * 2 - 1),
                );

                // points.appendAssumeCapacity(.{
                //     .pos = local_pos.add(center),
                //     .color = color,
                // });
                {
                    const dist_to_focus = @abs(pos.z - focus_dist);
                    const radius = dist_to_focus * 10 + 1;
                    const area = math.square(radius);
                    const op = 1.0 / area;
                    gpu_points.appendAssumeCapacity(.{
                        .color = color.withAlpha(op),
                        .point = .{
                            .pos = pos.XY(),
                            .turns = 0.0,
                            .scale = radius * pixel_width * 2,
                        },
                    });
                }
            }
        }
        // TODO: generate the points in order, ideally
        // std.mem.sort(AsdfPoint, points.items, {}, struct {
        //     pub fn anon(_: void, a: AsdfPoint, b: AsdfPoint) bool {
        //         return a.pos.z < b.pos.z;
        //     }
        // }.anon);
        canvas.fillShapesInstanced(
            camera,
            canvas.DEFAULT_SHAPES.circle_8,
            gpu_points.items,
        );
    }

    fn screenFromWorld(camera: Camera, world_point: Point) Point {
        const rect = camera.toRect();
        const local = Point.inverseApplyGetLocal(Point{
            .pos = rect.top_left,
            .scale = rect.size.y,
        }, world_point);
        const screen = Point{ .pos = .zero, .scale = window_size.y };
        return screen.applyToLocalPoint(local);
    }

    fn screenFromWorldPosition(camera: Camera, world_position: Vec2) Vec2 {
        return screenFromWorld(camera, .{ .pos = world_position }).pos;
    }

    fn screenFromWorldScale(camera: Camera, world_scale: f32) f32 {
        return screenFromWorld(camera, .{ .scale = world_scale }).scale;
    }

    fn screenFromWorldSize(camera: Camera, world_size: Vec2) Vec2 {
        return Vec2.new(
            screenFromWorldScale(camera, world_size.x),
            screenFromWorldScale(camera, world_size.y),
        );
    }

    pub fn clear(color: Color) void {
        gl_vtable.clear(color.toFColor());
    }

    pub fn setTransparency(alpha: f32) void {
        // TODO
        _ = alpha;
    }

    pub fn clipAtomRegion(camera: Camera, world_point: Point) void {
        // TODO
        _ = camera;
        _ = world_point;
    }

    pub fn endClip() void {
        // TODO
    }

    pub fn drawShape(camera: Camera, points: []const Vec2, stroke: ?Color, fill: ?Color) void {
        // TODO
        _ = camera;
        _ = points;
        _ = stroke;
        _ = fill;
    }

    pub fn drawShapeV2(camera: Camera, parent_world_point: Point, local_points: []const Vec2, stroke: ?Color, fill: ?Color) void {
        if (fill) |col| {
            canvas.fillShape(
                camera.toRect(),
                parent_world_point,
                canvas.tmpShape(local_points) catch @panic("OoM"),
                col.toFColor(),
            );
        }

        if (stroke) |col| {
            // TODO: wouldnt be needed if Rect had rotation
            const world_points = canvas.frame_arena.allocator().alloc(Vec2, local_points.len + 1) catch @panic("OoM");
            for (local_points, world_points[1..]) |p, *dst| {
                dst.* = parent_world_point.applyToLocalPosition(p);
            }
            world_points[0] = world_points[world_points.len - 1];
            const pixel_width = camera.height / window_size.y;
            canvas.line(camera.toRect(), world_points, pixel_width, col.toFColor());
        }
    }

    pub fn drawLine(camera: Camera, points: []const Vec2, color: Color) void {
        const pixel_width = camera.height / window_size.y;
        canvas.line(camera.toRect(), points, pixel_width, color.toFColor());
    }

    pub fn drawRect(camera: Camera, rect: Rect, stroke: ?Color, fill: ?Color) void {
        if (fill) |col| {
            canvas.fillRect(camera.toRect(), rect, col.toFColor());
        }
        if (stroke) |col| {
            const pixel_width = camera.height / window_size.y;
            canvas.strokeRect(camera.toRect(), rect, pixel_width, col.toFColor());
        }
    }

    pub fn drawDebugText(camera: Camera, center: Point, text: [:0]const u8, color: Color) void {
        var it = std.mem.splitScalar(u8, text, '\n');
        const n_lines = blk: {
            var result: usize = 0;
            while (it.next()) |_| result += 1;
            it.reset();
            break :blk result;
        };
        var k: f32 = -(tof32(n_lines) - 1) / 2;
        while (it.next()) |line| {
            defer k += 1.0;
            canvas.text_renderers[0].drawLine(
                gl_vtable,
                camera.toRect(),
                .{ .center = center.applyToLocalPosition(.new(
                    -tof32(line.len) * 0.158,
                    0.22583334 + k * 0.725,
                )) },
                line,
                center.scale * 0.7,
                color.toFColor(),
                canvas.frame_arena.allocator(),
            ) catch @panic("TODO");
        }
        // const screen_point = screenFromWorld(camera, center);
        // // TODO: scale
        // // panickify(c.SDL_SetRenderScale(sdl_renderer, screen_point.scale / 8, screen_point.scale / 8));
        // // defer panickify(c.SDL_SetRenderScale(sdl_renderer, 1, 1));
        // setRenderDrawColor(color);
        // var it = std.mem.splitScalar(u8, text, '\n');
        // var y: f32 = -4 - 8 * (tof32(std.mem.count(u8, text, "\n")) / 2);
        // while (it.next()) |line| {
        //     const lineZ = gpa.allocator().dupeZ(u8, line) catch @panic("OoM");
        //     defer gpa.allocator().free(lineZ);
        //     panickify(c.SDL_RenderDebugText(sdl_renderer, screen_point.pos.x - tof32(line.len) * 4, screen_point.pos.y + y, lineZ));
        //     y += 8;
        // }
    }

    fn polygon(screen_positions: []const Vec2, triangles: []const [3]IndexType, outline_points: []const usize, fill: Color, stroke: Color) void {
        const camera: Rect = .{ .top_left = .zero, .size = window_size };
        canvas.fillShape(
            camera,
            .{},
            .{ .local_points = screen_positions, .triangles = triangles },
            fill.toFColor(),
        );

        for (0..outline_points.len) |i| {
            const from = screen_positions[outline_points[i]];
            const to = screen_positions[outline_points[(i + 1) % outline_points.len]];
            canvas.line(camera, &.{ from, to }, 1, stroke.toFColor());
        }
    }

    pub fn drawPatternAtomDebug(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = [_]Vec2{
            Vec2.new(0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(-1, 1),
            Vec2.new(-0.8, 1.0 / 3.0),
            Vec2.new(-1.2, -1.0 / 3.0),
            Vec2.new(-1, -1),
            Vec2.new(0, -1),
        };
        const indices = [_][3]IndexType{
            .{ 1, 2, 3 },
            .{ 0, 1, 3 },
            .{ 0, 3, 4 },
            .{ 0, 4, 6 },
            .{ 4, 5, 6 },
        };
        const outline = [_]usize{ 0, 1, 2, 3, 4, 5, 6 };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        polygon(&screen_positions, &indices, &outline, Color.white, Color.black);
    }

    pub fn drawPatternAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const profile = visuals.profile;
        const screen_point = screenFromWorld(camera, world_point);
        if (screen_point.scale < 0.1) return;
        const skeleton_positions = if (DESIGN.round_data)
            [1]Vec2{.new(-1, -1)} ++
                funk.fromCount(32, struct {
                    pub fn anon(k: usize) Vec2 {
                        return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                    }
                }.anon) ++ [1]Vec2{.new(-1, 1)}
        else
            [_]Vec2{
                Vec2.new(-1, -1),
                Vec2.new(0, -1),
                Vec2.new(0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(-1, 1),
            };
        // TODO: no allocations
        var local_positions: []Vec2 = canvas.frame_arena.allocator().alloc(Vec2, skeleton_positions.len + profile.len * 2) catch @panic("TODO");
        for (skeleton_positions, 0..) |pos, i| {
            local_positions[i] = pos;
        }
        for (profile, 0..) |pos, i| {
            local_positions[skeleton_positions.len + i] = Vec2.new(-1.0 - pos.y, 1.0 - pos.x);
            local_positions[skeleton_positions.len + profile.len * 2 - i - 1] = Vec2.new(-1.0 + pos.y, -1.0 + pos.x);
        }
        drawShapeV2(camera, world_point, local_positions, .black, visuals.color);
    }

    pub fn drawVariable(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).mul(.new(0.5, 1)).addX(0.5);
                }
            }.anon)
        else
            [_]Vec2{
                Vec2.new(-0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(0.5, 1),
                Vec2.new(0, 0),
                Vec2.new(0.5, -1),
                Vec2.new(0, -1),
            };
        drawShapeV2(camera, world_point, &local_positions, .black, visuals.color);
    }

    pub fn drawPatternVariable(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, -0.25, math.tof32(k) / 32)).mul(.new(0.5, 1)).addX(-0.5);
                }
            }.anon)
        else
            [_]Vec2{
                Vec2.new(0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(-0.5, 1),
                Vec2.new(0, 0),
                Vec2.new(-0.5, -1),
                Vec2.new(0, -1),
            };
        drawShapeV2(camera, world_point, &local_positions, .black, visuals.color);
    }

    pub fn drawPairHolder(camera: Camera, world_point: Point) void {
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).scale(0.5).add(.new(0.75, 0.5));
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, 0.75, math.tof32(k) / 32)).scale(0.5).add(.new(0.75, -0.5));
                }
            }.anon)
        else
            [_]Vec2{
                Vec2.new(-0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(0.5, 1),
                Vec2.new(0.25, 0.5),
                Vec2.new(0.5, 0),
                Vec2.new(0.25, -0.5),
                Vec2.new(0.5, -1),
                Vec2.new(0, -1),
            };

        drawShapeV2(camera, world_point, &local_positions, .black, .gray(96));
    }

    pub fn drawPatternPairHolder(camera: Camera, world_point: Point) void {
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(-0.25, 0.25, math.tof32(k) / 32)).addX(-0.5);
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, -0.25, math.tof32(k) / 32)).scale(0.5).add(.new(-1.25, 0.5));
                }
            }.anon) ++ funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.25, -0.25, math.tof32(k) / 32)).scale(0.5).add(.new(-1.25, -0.5));
                }
            }.anon)

                // [_]Vec2{ .new(-1.5, 1), .new(-1, 0.5), .new(-1.5, 0), .new(-1, -0.5), .new(-1.5, -1) }
        else
            [_]Vec2{
                Vec2.new(0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(-1, 1),
                Vec2.new(-0.75, 0.5),
                Vec2.new(-1, 0),
                Vec2.new(-0.75, -0.5),
                Vec2.new(-1, -1),
                Vec2.new(0, -1),
            };
        drawShapeV2(camera, world_point, &local_positions, .black, .gray(96));
    }

    pub fn drawAtomDebug(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = [_]Vec2{
            Vec2.new(-0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(2, 1),
            Vec2.new(2.2, 1.0 / 3.0),
            Vec2.new(1.8, -1.0 / 3.0),
            Vec2.new(2, -1),
            Vec2.new(0, -1),
        };
        const indices = [_][3]IndexType{
            .{ 1, 2, 3 },
            .{ 0, 1, 3 },
            .{ 0, 3, 4 },
            .{ 0, 4, 6 },
            .{ 4, 5, 6 },
        };
        const outline = [_]usize{ 0, 1, 2, 3, 4, 5, 6 };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        polygon(&screen_positions, &indices, &outline, Color.white, Color.black);
    }

    pub fn drawPatternAtomOutline(camera: Camera, world_point: Point) void {
        _ = camera;
        _ = world_point;
        @panic("not implemented");
    }

    pub fn drawCable(camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void {
        _ = offset;
        const pixel_width = camera.height / window_size.y;
        canvas.line(camera.toRect(), &.{ world_from, world_to }, world_scale * pixel_width, .black);
    }

    pub fn drawFnkHolder(camera: Camera, world_point: Point) void {
        const pixel_width = camera.height / window_size.y;
        canvas.strokeCircle(
            32,
            camera.toRect(),
            world_point.pos,
            world_point.scale / 2.0,
            pixel_width,
            .black,
        );

        canvas.line(
            camera.toRect(),
            &.{
                world_point.applyToLocalPosition(.new(0, -0.5)),
                world_point.applyToLocalPosition(.new(0, -1.5)),
            },
            pixel_width,
            .black,
        );
    }

    fn strokePoints(N: comptime_int, screen_positions: *const [N]Vec2) void {
        // TODO NOW
        _ = screen_positions;
        // var sdl_points: [N]c.SDL_FPoint = undefined;
        // for (&sdl_points, screen_positions) |*target, source| {
        //     target.* = c.SDL_FPoint{ .x = source.x, .y = source.y };
        // }
        // panickify(c.SDL_RenderLines(sdl_renderer, &sdl_points, sdl_points.len));
    }

    pub fn drawCaseHolder(camera: Camera, world_point: Point) void {
        const pixel_width = camera.height / window_size.y;
        canvas.strokeCircle(
            32,
            camera.toRect(),
            world_point.pos,
            world_point.scale / 2.0,
            pixel_width,
            .white,
        );
    }

    fn drawNilAtomCool(camera: Rect, world_point: Point, visuals: presenter.AtomVisuals, dist_to_focus: f32) !void {
        try canvas.drawDirty(
            .{
                .vertex_src =
                \\uniform vec4 u_camera; // as top_left, size
                \\uniform vec4 u_point; // as pos, turns, scale
                \\
                \\in vec2 a_position;
                \\out vec2 v_uv;
                \\#define TAU 6.283185307179586
                \\void main() {
                \\  float c = cos(u_point.z * TAU);
                \\  float s = sin(u_point.z * TAU);
                \\  vec2 world_position = u_point.xy + u_point.w * (mat2x2(c,s,-s,c) * a_position);
                \\  vec2 camera_position = (world_position - u_camera.xy) / u_camera.zw;
                \\  gl_Position = vec4((camera_position * 2.0 - 1.0) * vec2(1, -1), 0, 1);
                \\  v_uv = a_position;
                \\}
                ,
                .fragment_src =
                \\precision highp float;
                \\out vec4 out_color;
                \\in vec2 v_uv;
                \\uniform vec4 u_color;
                \\uniform vec4 u_color_lighter;
                \\uniform float u_px_per_uv;
                \\uniform float u_abs_dist_to_focus_plane;
                \\float clamp01(float v) {
                \\  return clamp(v, 0.0, 1.0);
                \\}
                \\float inverseLerp(float a, float b, float t) {
                \\  return (t - a) / (b - a);
                \\}
                \\vec4 fill(vec4 color, float dist_in_px, float coc_radius_in_px) {
                \\  return mix(color, vec4(color.xyz, 0), clamp01(inverseLerp(-coc_radius_in_px, coc_radius_in_px, dist_in_px)));
                \\}
                \\vec4 addColors(vec4 a, vec4 b) {
                \\  return vec4(mix(a.xyz, b.xyz, b.w / (a.w + b.w)), a.w + b.w);
                \\}
                \\float rawAtomProfile(float t) {
                \\  // valid for Nil atom
                \\  if (t < 0.25) {
                \\    return mix(0, -0.25, t / 0.25);
                \\  } else {
                \\    return mix(-0.25, 0, (t - 0.25) / 0.75);
                \\  }
                \\}
                \\// y in -1..1
                \\float atomProfile(float y) {
                \\  if (y >= 0) {
                \\      return -rawAtomProfile(y);
                \\  } else {
                \\      return rawAtomProfile(-y);
                \\  }
                \\}
                \\void main() {
                \\  float dist = length(vec2(min(v_uv.x, 0), v_uv.y));
                \\  dist = max(dist, v_uv.x - 0.5 - atomProfile(v_uv.y));
                \\  float coc_radius_in_px = 0.5 + 1.0 * u_abs_dist_to_focus_plane * abs(u_px_per_uv);
                \\  float interior_size = 0.9; // mix(0.9, 1.0, smoothstep(0, 0.2, u_abs_dist_to_focus_plane));
                \\  vec4 border_color = mix(vec4(u_color_lighter.rgb,0.7), u_color, smoothstep(0, 2, u_abs_dist_to_focus_plane));
                \\  float dist_to_interior_in_px = (dist - interior_size) * u_px_per_uv;
                \\  float dist_to_border_in_px = max(dist - 1.0, interior_size - dist) * u_px_per_uv;
                \\  out_color = addColors(
                \\      fill(u_color, dist_to_interior_in_px, coc_radius_in_px), 
                \\      fill(border_color, dist_to_border_in_px, coc_radius_in_px)
                \\  );
                \\}
                ,
                .attributes = .{ .attribs = &.{
                    .{ .name = "a_position", .kind = .Vec2 },
                } },
                .uniforms = &.{
                    .{ .name = "u_camera", .kind = .Rect },
                    .{ .name = "u_point", .kind = .Point },
                    .{ .name = "u_color", .kind = .FColor },
                    .{ .name = "u_color_lighter", .kind = .FColor },
                    .{ .name = "u_px_per_uv", .kind = .f32 },
                    .{ .name = "u_abs_dist_to_focus_plane", .kind = .f32 },
                },
            },
            .{
                .vertices_ptr = &.{
                    Vec2.new(-2, -2),
                    Vec2.new(3, -2),
                    Vec2.new(-2, 2),
                    Vec2.new(3, 2),
                },
                .vertices_len_bytes = 4 * @sizeOf(Vec2),
                .triangles = &.{
                    .{ 0, 1, 2 },
                    .{ 3, 2, 1 },
                },
                .uniforms = &.{
                    .{ .name = "u_point", .value = .{
                        .Point = world_point.applyToLocalPoint(.{ .pos = .new(0.5, 0) }),
                    } },
                    .{ .name = "u_camera", .value = .{ .Rect = camera } },
                    .{ .name = "u_color", .value = .{ .FColor = visuals.color.toFColor() } },
                    .{ .name = "u_color_lighter", .value = .{ .FColor = visuals.color.toFColor().lighter() } },
                    .{ .name = "u_px_per_uv", .value = .{ .f32 = world_point.scale * window_size.y / camera.size.y } },
                    .{ .name = "u_abs_dist_to_focus_plane", .value = .{ .f32 = @abs(dist_to_focus) } },
                },
                .texture = null,
            },
        );
    }

    pub fn drawAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        // if (true or visuals.profile.len == 1 and visuals.color.equals(.from01(0.45, 0.45, 0.45))) {
        if (false) {
            drawNilAtomCool(camera.toRect(), world_point, visuals, math.lerp(-1, 1, mouse.cur.client_pos.y)) catch unreachable;
            return;
        }
        const profile = visuals.profile;
        const screen_point = screenFromWorld(camera, world_point);
        if (screen_point.scale < 0.1) return;
        const skeleton_positions = if (DESIGN.round_data)
            [1]Vec2{.new(2, -1)} ++
                funk.fromCount(32, struct {
                    pub fn anon(k: usize) Vec2 {
                        return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                    }
                }.anon) ++ [1]Vec2{.new(2, 1)}
        else
            [_]Vec2{
                Vec2.new(2, -1),
                Vec2.new(0, -1),
                Vec2.new(-0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(2, 1),
            };

        // TODO: no allocations
        var local_positions: []Vec2 = canvas.frame_arena.allocator().alloc(Vec2, skeleton_positions.len + profile.len * 2) catch @panic("TODO");
        for (skeleton_positions, 0..) |pos, i| {
            local_positions[i] = pos;
        }
        for (profile, 0..) |pos, i| {
            local_positions[skeleton_positions.len + i] = Vec2.new(2.0 - pos.y, 1.0 - pos.x);
            local_positions[skeleton_positions.len + profile.len * 2 - i - 1] = Vec2.new(2.0 + pos.y, -1.0 + pos.x);
        }
        drawShapeV2(camera, world_point, local_positions, .black, visuals.color);
    }

    pub fn drawWildcardsCable(camera: Camera, points: []const Vec2, visuals: []const presenter.AtomVisuals) void {
        // TODO: line width
        // TODO: multiple visuals
        if (visuals.len == 0) return;
        drawLine(camera, points, visuals[0].color);
    }
};

var gl_procs: gl.ProcTable = undefined;

var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
const sdl_platform = presenter.Platform{
    .gpa = gpa.allocator(),
    .getPlayerData = SdlPlatform.getPlayerData,
    .setPlayerData = SdlPlatform.setPlayerData,
    .downloadPlayerData = SdlPlatform.downloadPlayerData,
    .uploadPlayerData = SdlPlatform.uploadPlayerData,
    .getMouse = SdlPlatform.getMouse,
    .getKeyboard = SdlPlatform.getKeyboard,
    .setCursor = SdlPlatform.setCursor,
};
const sdl_drawer = presenter.Drawer{
    .asdfBackground = SdlDrawer.asdfBackground,
    .clear = SdlDrawer.clear,
    .setTransparency = SdlDrawer.setTransparency,
    .clipAtomRegion = SdlDrawer.clipAtomRegion,
    .endClip = SdlDrawer.endClip,
    .drawLine = SdlDrawer.drawLine,
    .drawRect = SdlDrawer.drawRect,
    .drawShape = SdlDrawer.drawShape,
    .drawShapeV2 = SdlDrawer.drawShapeV2,
    .drawDebugText = SdlDrawer.drawDebugText,
    .drawAtom = SdlDrawer.drawAtom,
    .drawPatternAtom = SdlDrawer.drawPatternAtom,
    .drawVariable = SdlDrawer.drawVariable,
    .drawPatternVariable = SdlDrawer.drawPatternVariable,
    .drawAtomDebug = SdlDrawer.drawAtomDebug,
    .drawPatternAtomOutline = SdlDrawer.drawPatternAtomOutline,
    .drawPairHolder = SdlDrawer.drawPairHolder,
    .drawPatternPairHolder = SdlDrawer.drawPatternPairHolder,
    .drawPatternAtomDebug = SdlDrawer.drawPatternAtomDebug,
    .drawCable = SdlDrawer.drawCable,
    .drawCaseHolder = SdlDrawer.drawCaseHolder,
    .drawFnkHolder = SdlDrawer.drawFnkHolder,
    .drawWildcardsCable = SdlDrawer.drawWildcardsCable,
};

const window_size = Vec2.new(1280, 720);
const MouseState = presenter.MouseState;
var mouse = presenter.Mouse{ .cur = .init, .prev = .init, .cur_time = undefined };
var keyboard = presenter.Keyboard{ .cur = .init, .prev = .init, .cur_time = undefined };
var game: presenter.Presenter(sdl_platform, sdl_drawer) = undefined;

const c = @cImport({
    @cDefine("SDL_DISABLE_OLD_NAMES", {});
    @cInclude("SDL3/SDL.h");
    @cInclude("SDL3/SDL_revision.h");
    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_MAIN_HANDLED' should be defined before including 'SDL_main.h'.
    @cDefine("SDL_MAIN_HANDLED", {});
    @cInclude("SDL3/SDL_main.h");
});

pub fn main() !void {
    errdefer |err| if (err == error.SdlError) std.log.err("SDL error: {s}", .{c.SDL_GetError()});

    std.log.debug("SDL build time version: {d}.{d}.{d}", .{
        c.SDL_MAJOR_VERSION,
        c.SDL_MINOR_VERSION,
        c.SDL_MICRO_VERSION,
    });
    std.log.debug("SDL build time revision: {s}", .{c.SDL_REVISION});
    {
        const version = c.SDL_GetVersion();
        std.log.debug("SDL runtime version: {d}.{d}.{d}", .{
            c.SDL_VERSIONNUM_MAJOR(version),
            c.SDL_VERSIONNUM_MINOR(version),
            c.SDL_VERSIONNUM_MICRO(version),
        });
        const revision: [*:0]const u8 = c.SDL_GetRevision();
        std.log.debug("SDL runtime revision: {s}", .{revision});
    }

    // For programs that provide their own entry points instead of relying on SDL's main function
    // macro magic, 'SDL_SetMainReady' should be called before calling 'SDL_Init'.
    c.SDL_SetMainReady();

    try errify(c.SDL_SetAppMetadata("Vaulogy", "0.0.0", "com.knexator.vaulogy"));

    try errify(c.SDL_Init(c.SDL_INIT_VIDEO));
    defer c.SDL_Quit();

    errify(c.SDL_SetHint(c.SDL_HINT_RENDER_VSYNC, "1")) catch {};

    try errify(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MAJOR_VERSION, gl.info.version_major));
    try errify(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_MINOR_VERSION, gl.info.version_minor));
    if (gl.info.profile) |profile| try errify(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_PROFILE_MASK, switch (profile) {
        .core => c.SDL_GL_CONTEXT_PROFILE_CORE,
        else => @compileError("TODO"),
    }));
    try errify(c.SDL_GL_SetAttribute(c.SDL_GL_CONTEXT_FLAGS, c.SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG));

    if (true) {
        try errify(c.SDL_GL_SetAttribute(c.SDL_GL_MULTISAMPLEBUFFERS, 1));
        try errify(c.SDL_GL_SetAttribute(c.SDL_GL_MULTISAMPLESAMPLES, 16));
    }

    std.log.debug("SDL video drivers: {}", .{fmtSdlDrivers(
        c.SDL_GetCurrentVideoDriver().?,
        c.SDL_GetNumVideoDrivers(),
        c.SDL_GetVideoDriver,
    )});

    const sdl_window: *c.SDL_Window = try errify(c.SDL_CreateWindow(
        "Vaulogy",
        @intFromFloat(window_size.x),
        @intFromFloat(window_size.y),
        c.SDL_WINDOW_OPENGL,
    ));
    defer c.SDL_DestroyWindow(sdl_window);

    const gl_context = try errify(c.SDL_GL_CreateContext(sdl_window));
    defer errify(c.SDL_GL_DestroyContext(gl_context)) catch {};
    try errify(c.SDL_GL_MakeCurrent(sdl_window, gl_context));
    defer errify(c.SDL_GL_MakeCurrent(sdl_window, null)) catch {};
    if (!gl_procs.init(c.SDL_GL_GetProcAddress)) return error.GlInitFailed;
    gl.makeProcTableCurrent(&gl_procs);
    defer gl.makeProcTableCurrent(null);

    gl.Viewport(0, 0, @intCast(window_size.toInt(c_int).x), @intCast(window_size.toInt(c_int).y));
    gl.Enable(gl.BLEND);
    gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    const sdl_gl = struct {
        pub const vtable: Gl = .{
            .clear = clear,
            .buildRenderable = buildRenderable,
            .useRenderable = useRenderable,
            .buildTexture2D = buildTexture2D,
            .buildInstancedRenderable = buildInstancedRenderable,
            .useInstancedRenderable = useInstancedRenderable,
            .loadTextureDataFromBase64 = undefined,
            .loadTextureDataFromFilename = undefined,
        };

        pub fn clear(color: FColor) void {
            gl.ClearBufferfv(gl.COLOR, 0, &color.toArray());
        }

        pub fn buildTexture2D(data: *const anyopaque, pixelart: bool) Gl.Texture {
            const image: *const zstbi.Image = @alignCast(@ptrCast(data));

            const has_alpha = switch (image.num_components) {
                3 => false,
                4 => true,
                else => unreachable,
            };

            var texture: c_uint = undefined;
            gl.GenTextures(1, @ptrCast(&texture));
            gl.BindTexture(gl.TEXTURE_2D, texture);
            gl.TexImage2D(
                gl.TEXTURE_2D,
                0,
                if (has_alpha) gl.RGBA else gl.RGB,
                @intCast(image.width),
                @intCast(image.height),
                0,
                if (has_alpha) gl.RGBA else gl.RGB,
                gl.UNSIGNED_BYTE,
                image.data.ptr,
            );
            if (pixelart) {
                gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
                gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
            } else {
                gl.GenerateMipmap(gl.TEXTURE_2D);
                gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
                // TODO: let user choose quality
                gl.TexParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR_MIPMAP_LINEAR);
            }

            return .{ .id = texture, .resolution = .new(image.width, image.height) };
        }

        pub fn buildRenderable(
            vertex_src: [:0]const u8,
            fragment_src: [:0]const u8,
            attributes: Gl.VertexInfo.Collection,
            uniforms: []const Gl.UniformInfo.In,
        ) !Gl.Renderable {
            const program = try (ProgramInfo{
                .vertex = vertex_src,
                .fragment = fragment_src,
            }).load();

            var vao: c_uint = undefined;
            gl.GenVertexArrays(1, @ptrCast(&vao));

            gl.BindVertexArray(vao);

            var vbo: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&vbo));
            var ebo: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&ebo));

            gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, ebo);
            defer gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);

            gl.BindBuffer(gl.ARRAY_BUFFER, vbo);
            defer gl.BindBuffer(gl.ARRAY_BUFFER, 0);

            defer gl.BindVertexArray(0);

            for (attributes.attribs, 0..) |attribute, k| {
                const index: gl.uint = @intCast(gl.GetAttribLocation(program, attribute.name));
                if (index == -1) return error.AttributeLocationError;
                gl.EnableVertexAttribArray(index);
                gl.VertexAttribPointer(
                    index,
                    @intFromEnum(attribute.kind.count()),
                    @intFromEnum(attribute.kind.type()),
                    if (attribute.kind.normalized()) gl.TRUE else gl.FALSE,
                    // TODO: check in debugger if this is computed once
                    @intCast(attributes.getStride()),
                    attributes.getOffset(k),
                );
            }

            var uniforms_data = std.BoundedArray(Gl.UniformInfo, 8).init(0) catch unreachable;
            for (uniforms) |uniform| {
                const location = gl.GetUniformLocation(program, uniform.name);
                if (location == -1) {
                    std.log.debug("bad uniform: {s}", .{uniform.name});
                    return error.UniformLocationError;
                }
                uniforms_data.append(.{
                    .location = location,
                    .name = uniform.name,
                    .kind = uniform.kind,
                }) catch return error.TooManyUniforms;
            }

            return .{
                .program = @enumFromInt(program),
                .vao = @enumFromInt(vao),
                .vbo = @enumFromInt(vbo),
                .ebo = @enumFromInt(ebo),
                .uniforms = uniforms_data,
            };
        }

        pub fn useRenderable(
            renderable: Gl.Renderable,
            vertices_ptr: *const anyopaque,
            vertices_len_bytes: usize,
            // vertices: []const anyopaque,
            // TODO: make triangles optional, since they could be precomputed
            triangles: []const [3]Gl.IndexType,
            uniforms: []const Gl.UniformInfo.Runtime,
            // TODO: multiple textures
            texture: ?Gl.Texture,
        ) void {
            {
                gl.BindBuffer(gl.ARRAY_BUFFER, @intFromEnum(renderable.vbo));
                gl.BufferData(
                    gl.ARRAY_BUFFER,
                    @intCast(vertices_len_bytes),
                    @ptrCast(vertices_ptr),
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ARRAY_BUFFER, 0);
            }

            {
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, @intFromEnum(renderable.ebo));
                gl.BufferData(
                    gl.ELEMENT_ARRAY_BUFFER,
                    @intCast(@sizeOf([3]Gl.IndexType) * triangles.len),
                    triangles.ptr,
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);
            }

            if (texture) |t| gl.BindTexture(gl.TEXTURE_2D, t.id);
            defer if (texture) |_| gl.BindTexture(gl.TEXTURE_2D, 0);

            gl.BindVertexArray(@intFromEnum(renderable.vao));
            defer gl.BindVertexArray(0);

            gl.UseProgram(@intFromEnum(renderable.program));
            defer gl.UseProgram(0);

            for (uniforms) |uniform| {
                // const u = uniform.location;
                // TODO
                const u = blk: {
                    for (renderable.uniforms.slice()) |u| {
                        if (std.mem.eql(u8, u.name, uniform.name)) break :blk u.location;
                    } else unreachable;
                };
                switch (uniform.value) {
                    .FColor => |v| gl.Uniform4f(u, v.r, v.g, v.b, v.a),
                    .Rect => |v| gl.Uniform4f(u, v.top_left.x, v.top_left.y, v.size.x, v.size.y),
                    .Point => |v| gl.Uniform4f(u, v.pos.x, v.pos.y, v.turns, v.scale),
                    .f32 => |v| gl.Uniform1f(u, v),
                }
            }

            gl.DrawElements(gl.TRIANGLES, @intCast(3 * triangles.len), switch (Gl.IndexType) {
                u16 => gl.UNSIGNED_SHORT,
                u32 => gl.UNSIGNED_INT,
                else => @compileError("not implemented"),
            }, 0);
        }

        pub fn buildInstancedRenderable(
            vertex_src: [:0]const u8,
            fragment_src: [:0]const u8,
            per_vertex_attributes: Gl.VertexInfo.Collection,
            per_instance_attributes: Gl.VertexInfo.Collection,
            uniforms: []const Gl.UniformInfo.In,
        ) !Gl.InstancedRenderable {
            const program = try (ProgramInfo{
                .vertex = vertex_src,
                .fragment = fragment_src,
            }).load();

            var vao: c_uint = undefined;
            gl.GenVertexArrays(1, @ptrCast(&vao));

            gl.BindVertexArray(vao);

            // TODO: single GenBuffers call
            var vbo_vertices: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&vbo_vertices));
            var vbo_instances: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&vbo_instances));
            var ebo: c_uint = undefined;
            gl.GenBuffers(1, @ptrCast(&ebo));

            gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, ebo);
            // defer gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);

            defer gl.BindVertexArray(0);

            // https://webgl2fundamentals.org/webgl/lessons/webgl-instanced-drawing.html

            {
                gl.BindBuffer(gl.ARRAY_BUFFER, vbo_vertices);
                // defer gl.BindBuffer(gl.ARRAY_BUFFER, 0);

                const attributes = per_vertex_attributes;
                for (attributes.attribs, 0..) |attribute, k| {
                    const index: gl.uint = @intCast(gl.GetAttribLocation(program, attribute.name));
                    if (index == -1) return error.AttributeLocationError;
                    gl.EnableVertexAttribArray(index);
                    gl.VertexAttribPointer(
                        index,
                        @intFromEnum(attribute.kind.count()),
                        @intFromEnum(attribute.kind.type()),
                        if (attribute.kind.normalized()) gl.TRUE else gl.FALSE,
                        // TODO: check in debugger if this is computed once
                        @intCast(attributes.getStride()),
                        attributes.getOffset(k),
                    );
                }
            }

            {
                gl.BindBuffer(gl.ARRAY_BUFFER, vbo_instances);
                // defer gl.BindBuffer(gl.ARRAY_BUFFER, 0);

                const attributes = per_instance_attributes;
                for (attributes.attribs, 0..) |attribute, k| {
                    const index: gl.uint = blk: {
                        const index = gl.GetAttribLocation(program, attribute.name);
                        if (index == -1) {
                            std.log.err("bad attribute: {s}", .{attribute.name});
                            return error.AttributeLocationError;
                        }
                        break :blk @intCast(index);
                    };
                    gl.EnableVertexAttribArray(index);
                    gl.VertexAttribPointer(
                        index,
                        @intFromEnum(attribute.kind.count()),
                        @intFromEnum(attribute.kind.type()),
                        if (attribute.kind.normalized()) gl.TRUE else gl.FALSE,
                        // TODO: check in debugger if this is computed once
                        @intCast(attributes.getStride()),
                        attributes.getOffset(k),
                    );
                    gl.VertexAttribDivisor(index, 1);
                }
            }

            var uniforms_data = std.BoundedArray(Gl.UniformInfo, 8).init(0) catch unreachable;
            for (uniforms) |uniform| {
                const location = gl.GetUniformLocation(program, uniform.name);
                if (location == -1) return error.UniformLocationError;
                uniforms_data.append(.{
                    .location = location,
                    .name = uniform.name,
                    .kind = uniform.kind,
                }) catch return error.TooManyUniforms;
            }

            return .{
                .program = @enumFromInt(program),
                .vao = @enumFromInt(vao),
                .vbo_vertices = @enumFromInt(vbo_vertices),
                .vbo_instances = @enumFromInt(vbo_instances),
                .ebo = @enumFromInt(ebo),
                .uniforms = uniforms_data,
            };
        }

        pub fn useInstancedRenderable(
            renderable: Gl.InstancedRenderable,
            // TODO: make the vertex data optional, since it could be precomputed
            vertex_data_ptr: *const anyopaque,
            vertex_data_len_bytes: usize,
            // TODO: make triangles optional, since they could be precomputed
            triangles: []const [3]Gl.IndexType,
            instance_data_ptr: *const anyopaque,
            instance_data_len_bytes: usize,
            instance_count: usize,
            uniforms: []const Gl.UniformInfo.Runtime,
            // TODO: multiple textures
            texture: ?Gl.Texture,
        ) void {
            {
                gl.BindBuffer(gl.ARRAY_BUFFER, @intFromEnum(renderable.vbo_vertices));
                gl.BufferData(
                    gl.ARRAY_BUFFER,
                    @intCast(vertex_data_len_bytes),
                    @ptrCast(vertex_data_ptr),
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ARRAY_BUFFER, 0);
            }

            {
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, @intFromEnum(renderable.ebo));
                gl.BufferData(
                    gl.ELEMENT_ARRAY_BUFFER,
                    @intCast(@sizeOf([3]Gl.IndexType) * triangles.len),
                    triangles.ptr,
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ELEMENT_ARRAY_BUFFER, 0);
            }

            {
                gl.BindBuffer(gl.ARRAY_BUFFER, @intFromEnum(renderable.vbo_instances));
                gl.BufferData(
                    gl.ARRAY_BUFFER,
                    @intCast(instance_data_len_bytes),
                    @ptrCast(instance_data_ptr),
                    gl.DYNAMIC_DRAW,
                );
                gl.BindBuffer(gl.ARRAY_BUFFER, 0);
            }

            // TODO
            assert(texture == null);
            // if (texture) |t| gl.BindTexture(gl.TEXTURE_2D, t.id);
            // defer if (texture) |_| gl.BindTexture(gl.TEXTURE_2D, 0);

            gl.BindVertexArray(@intFromEnum(renderable.vao));
            defer gl.BindVertexArray(0);

            gl.UseProgram(@intFromEnum(renderable.program));
            defer gl.UseProgram(0);

            for (uniforms) |uniform| {
                // const u = uniform.location;
                // TODO
                const u = blk: {
                    for (renderable.uniforms.slice()) |u| {
                        if (std.mem.eql(u8, u.name, uniform.name)) break :blk u.location;
                    } else unreachable;
                };
                switch (uniform.value) {
                    .FColor => |v| gl.Uniform4f(u, v.r, v.g, v.b, v.a),
                    .Rect => |v| gl.Uniform4f(u, v.top_left.x, v.top_left.y, v.size.x, v.size.y),
                    .Point => |v| gl.Uniform4f(u, v.pos.x, v.pos.y, v.turns, v.scale),
                    .f32 => |v| gl.Uniform1f(u, v),
                }
            }

            gl.DrawElementsInstanced(gl.TRIANGLES, @intCast(3 * triangles.len), switch (Gl.IndexType) {
                u16 => gl.UNSIGNED_SHORT,
                u32 => gl.UNSIGNED_INT,
                else => @compileError("not implemented"),
            }, null, @intCast(instance_count));
        }
    };
    gl_vtable = sdl_gl.vtable;

    zstbi.init(gpa.allocator());
    defer zstbi.deinit();
    var arial_atlas = try zstbi.Image.loadFromMemory(@embedFile("./fonts/Arial.png"), 0);
    defer arial_atlas.deinit();

    canvas = try .init(gl_vtable, gpa.allocator(), &.{@embedFile("./fonts/Arial.json")}, &.{&arial_atlas});
    defer canvas.deinit(gl_vtable, gpa.allocator());

    try @TypeOf(game).init(&game);

    // var timekeeper: Timekeeper = .{ .tocks_per_s = c.SDL_GetPerformanceFrequency() };

    var timer: std.time.Timer = try .start();
    main_loop: while (true) {
        _ = canvas.frame_arena.reset(.retain_capacity);
        const lap: f32 = @floatFromInt(timer.lap());
        if (lap / std.time.ns_per_ms > 17) {
            std.log.warn("slow frame: {d}ms", .{lap / std.time.ns_per_ms});
        } else if (lap / std.time.ns_per_ms < 15) {
            std.log.warn("fast frame: {d}ms", .{lap / std.time.ns_per_ms});
        }

        // Process SDL events
        {
            var event: c.SDL_Event = undefined;
            while (c.SDL_PollEvent(&event)) {
                switch (event.type) {
                    c.SDL_EVENT_QUIT => {
                        break :main_loop;
                    },
                    c.SDL_EVENT_MOUSE_BUTTON_DOWN, c.SDL_EVENT_MOUSE_BUTTON_UP => {
                        const is_pressed = event.button.down;
                        switch (event.button.button) {
                            c.SDL_BUTTON_LEFT => mouse.cur.buttons.left = is_pressed,
                            c.SDL_BUTTON_RIGHT => mouse.cur.buttons.right = is_pressed,
                            c.SDL_BUTTON_MIDDLE => mouse.cur.buttons.middle = is_pressed,
                            else => {},
                        }
                    },
                    c.SDL_EVENT_MOUSE_MOTION => {
                        mouse.cur.client_pos = Vec2.new(event.motion.x, event.motion.y).scale(1.0 / window_size.y);
                    },
                    c.SDL_EVENT_MOUSE_WHEEL => {
                        mouse.cur.scrolled = if (event.wheel.y == 0)
                            .none
                        else if (event.wheel.y < 0)
                            .down
                        else
                            .up;
                    },
                    c.SDL_EVENT_KEY_DOWN, c.SDL_EVENT_KEY_UP => {
                        const is_pressed = event.type == c.SDL_EVENT_KEY_DOWN;
                        switch (event.key.key) {
                            c.SDLK_D, c.SDLK_RIGHT => keyboard.cur.keys.right = is_pressed,
                            c.SDLK_A, c.SDLK_LEFT => keyboard.cur.keys.left = is_pressed,
                            c.SDLK_W, c.SDLK_UP => keyboard.cur.keys.up = is_pressed,
                            c.SDLK_S, c.SDLK_DOWN => keyboard.cur.keys.down = is_pressed,
                            else => {},
                        }
                    },
                    else => {},
                }
            }
        }

        // Update the game state
        // while (timekeeper.consume()) {
        {
            // frame logic
            try game.update(1.0 / 60.0);
            mouse.prev = mouse.cur;
            mouse.cur.scrolled = .none;
            keyboard.prev = keyboard.cur;
        }

        // Draw
        try game.draw();
        try errify(c.SDL_GL_SwapWindow(sdl_window));

        // timekeeper.produce(c.SDL_GetPerformanceCounter());
    }
}

/// Facilitates updating the game logic at a fixed rate.
/// Inspired <https://github.com/TylerGlaiel/FrameTimingControl> and the linked article.
const Timekeeper = struct {
    const updates_per_s = 60;
    const max_accumulated_updates = 8;
    const snap_frame_rates = .{ updates_per_s, 30, 120, 144 };
    const ticks_per_tock = 720; // Least common multiple of 'snap_frame_rates'
    const snap_tolerance_us = 200;
    const us_per_s = 1_000_000;

    tocks_per_s: u64,
    accumulated_ticks: u64 = 0,
    previous_timestamp: ?u64 = null,

    fn consume(timekeeper: *Timekeeper) bool {
        const ticks_per_s: u64 = timekeeper.tocks_per_s * ticks_per_tock;
        const ticks_per_update: u64 = @divExact(ticks_per_s, updates_per_s);
        if (timekeeper.accumulated_ticks >= ticks_per_update) {
            timekeeper.accumulated_ticks -= ticks_per_update;
            return true;
        } else {
            return false;
        }
    }

    fn produce(timekeeper: *Timekeeper, current_timestamp: u64) void {
        if (timekeeper.previous_timestamp) |previous_timestamp| {
            const ticks_per_s: u64 = timekeeper.tocks_per_s * ticks_per_tock;
            const elapsed_ticks: u64 = (current_timestamp -% previous_timestamp) *| ticks_per_tock;
            const snapped_elapsed_ticks: u64 = inline for (snap_frame_rates) |snap_frame_rate| {
                const target_ticks: u64 = @divExact(ticks_per_s, snap_frame_rate);
                const abs_diff = @max(elapsed_ticks, target_ticks) - @min(elapsed_ticks, target_ticks);
                if (abs_diff *| us_per_s <= snap_tolerance_us *| ticks_per_s) {
                    break target_ticks;
                }
            } else elapsed_ticks;
            const ticks_per_update: u64 = @divExact(ticks_per_s, updates_per_s);
            const max_accumulated_ticks: u64 = max_accumulated_updates * ticks_per_update;
            timekeeper.accumulated_ticks = @min(timekeeper.accumulated_ticks +| snapped_elapsed_ticks, max_accumulated_ticks);
        }
        timekeeper.previous_timestamp = current_timestamp;
    }
};

fn fmtSdlDrivers(
    current_driver: [*:0]const u8,
    num_drivers: c_int,
    getDriver: *const fn (c_int) callconv(.C) ?[*:0]const u8,
) std.fmt.Formatter(formatSdlDrivers) {
    return .{ .data = .{
        .current_driver = current_driver,
        .num_drivers = num_drivers,
        .getDriver = getDriver,
    } };
}

fn formatSdlDrivers(
    context: struct {
        current_driver: [*:0]const u8,
        num_drivers: c_int,
        getDriver: *const fn (c_int) callconv(.C) ?[*:0]const u8,
    },
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    var i: c_int = 0;
    while (i < context.num_drivers) : (i += 1) {
        if (i != 0) {
            try writer.writeAll(", ");
        }
        const driver = context.getDriver(i).?;
        try writer.writeAll(std.mem.span(driver));
        if (std.mem.orderZ(u8, context.current_driver, driver) == .eq) {
            try writer.writeAll(" (current)");
        }
    }
}

/// Converts the return value of an SDL function to an error union.
inline fn errify(value: anytype) error{SdlError}!switch (@typeInfo(@TypeOf(value))) {
    .bool => void,
    .pointer, .optional => @TypeOf(value.?),
    .int => |info| switch (info.signedness) {
        .signed => @TypeOf(@max(0, value)),
        .unsigned => @TypeOf(value),
    },
    else => @compileError("unerrifiable type: " ++ @typeName(@TypeOf(value))),
} {
    return switch (@typeInfo(@TypeOf(value))) {
        .bool => if (!value) error.SdlError,
        .pointer, .optional => value orelse error.SdlError,
        .int => |info| switch (info.signedness) {
            .signed => if (value >= 0) @max(0, value) else error.SdlError,
            .unsigned => if (value != 0) value else error.SdlError,
        },
        else => comptime unreachable,
    };
}

// same as errify but panics instead of returning an error
inline fn panickify(value: anytype) switch (@typeInfo(@TypeOf(value))) {
    .bool => void,
    .pointer, .optional => @TypeOf(value.?),
    .int => |info| switch (info.signedness) {
        .signed => @TypeOf(@max(0, value)),
        .unsigned => @TypeOf(value),
    },
    else => @compileError("unerrifiable type: " ++ @typeName(@TypeOf(value))),
} {
    return errify(value) catch {
        std.debug.panic("SDL error: {s}", .{c.SDL_GetError()});
    };
}

// TODO: delete/improve this
pub const ProgramInfo = struct {
    const preamble: [:0]const u8 =
        \\#version 330 core
        \\
    ;
    vertex: [:0]const u8,
    fragment: [:0]const u8,

    fn doShader(stage: enum { vertex, fragment }, source: [:0]const u8) !gl.uint {
        assert(!std.mem.startsWith(u8, source, "#version"));
        const result = gl.CreateShader(switch (stage) {
            .vertex => gl.VERTEX_SHADER,
            .fragment => gl.FRAGMENT_SHADER,
        });
        gl.ShaderSource(
            result,
            2,
            &.{ preamble.ptr, source.ptr },
            &.{ @intCast(preamble.len), @intCast(source.len) },
        );
        gl.CompileShader(result);

        var success: gl.int = undefined;
        gl.GetShaderiv(result, gl.COMPILE_STATUS, &success);
        if (success == 0) {
            var info_log: [512]u8 = undefined;
            var info_len: gl.sizei = undefined;
            gl.GetShaderInfoLog(result, info_log.len, &info_len, &info_log);
            std.log.err("Failed to compile shader: {s}", .{info_log[0..@intCast(info_len)]});
            return error.ShaderCreationError;
        }

        return result;
    }

    pub fn load(self: ProgramInfo) !gl.uint {
        const vertex_shader = try doShader(.vertex, self.vertex);
        defer gl.DeleteShader(vertex_shader);

        const fragment_shader = try doShader(.fragment, self.fragment);
        defer gl.DeleteShader(fragment_shader);

        const program = gl.CreateProgram();

        gl.AttachShader(program, vertex_shader);
        gl.AttachShader(program, fragment_shader);
        gl.LinkProgram(program);

        var success: gl.int = undefined;
        gl.GetProgramiv(program, gl.LINK_STATUS, &success);
        if (success == 0) {
            var info_log: [512]u8 = undefined;
            var info_len: gl.sizei = undefined;
            gl.GetProgramInfoLog(program, info_log.len, &info_len, &info_log);
            std.log.err("Failed to link shader: {s}", .{info_log[0..@intCast(info_len)]});
            return error.ShaderCreationError;
        }

        return program;
    }
};
