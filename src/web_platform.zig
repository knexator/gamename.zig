const js = struct {
    pub const debug = struct {
        extern fn logInt(arg: u32) void;
        extern fn logFloat(arg: f32) void;
        extern fn logString(ptr: [*]const u8, len: usize) void;
    };

    pub const canvas = struct {
        extern fn getWidth() u32;
        extern fn getHeight() u32;
    };

    // current direction: closely matching the webgl2 API
    pub const webgl2 = struct {
        extern fn clearColor(r: f32, g: f32, b: f32, a: f32) void;
        // TODO: clear(mask: GLbitfield) gl.DEPTH_BUFFER_BIT | gl.COLOR_BUFFER_BIT | gl.STENCIL_CLEAR_VALUE
        extern fn clear() void;
        extern fn enable(capability: Capability) void;
        extern fn disable(capability: Capability) void;
        extern fn blendFunc(sfactor: BlendFactor, dfactor: BlendFactor) void;
        extern fn createShader(@"type": ShaderType) Shader;
        extern fn shaderSource(shader: Shader, source_ptr: [*]const u8, source_len: usize) void;
        extern fn compileShader(shader: Shader) void;
        extern fn getShaderParameter(shader: Shader, pname: ShaderParameter) extern union {
            type: ShaderType,
            delete_status: GLboolean,
            compile_status: GLboolean,
        };
        extern fn getShaderInfoLog(shader: Shader, buf_ptr: [*]u8, buf_len: usize) usize;
        extern fn createProgram() Program;
        extern fn attachShader(program: Program, shader: Shader) void;
        extern fn linkProgram(program: Program) void;
        extern fn getProgramParameter(program: Program, pname: ProgramParameter) extern union {
            link_status: GLboolean,
            validate_status: GLboolean,
            delete_status: GLboolean,
            attached_shaders: GLint,
            active_attributes: GLint,
            active_uniforms: GLint,
        };
        extern fn getProgramInfoLog(program: Program, buf_ptr: [*]u8, buf_len: usize) usize;
        extern fn useProgram(program: Program) void;
        extern fn deleteProgram(program: Program) void;
        extern fn deleteShader(shader: Shader) void;
        extern fn getUniformLocation(program: Program, name_ptr: [*]const u8, name_len: usize) UniformLocation;
        extern fn createVertexArray() VertexArrayObject;
        extern fn bindVertexArray(vao: VertexArrayObject) void;
        extern fn deleteVertexArray(vao: VertexArrayObject) void;
        extern fn bindBuffer(target: BufferTarget, buffer: Buffer) void;
        extern fn createBuffer() Buffer;
        extern fn deleteBuffer(buffer: Buffer) void;
        extern fn getAttribLocation(program: Program, name_ptr: [*]const u8, name_len: usize) GLint;
        extern fn enableVertexAttribArray(index: GLuint) void;
        extern fn disableVertexAttribArray(index: GLuint) void;
        extern fn bufferData_size(target: BufferTarget, size: GLsizeiptr, usage: Usage) void;
        extern fn bufferData(target: BufferTarget, data_ptr: [*]const u8, data_len: usize, usage: Usage) void;
        extern fn vertexAttribPointer(
            index: GLuint,
            /// 1, 2, 3, or 4
            size: GLint,
            @"type": VertexDataType,
            normalized: GLboolean,
            stride: GLsizei,
            offset: GLintptr,
        ) void;
        extern fn drawArrays(mode: DrawMode, first: GLint, count: GLsizei) void;
        extern fn drawElements(mode: DrawMode, count: GLsizei, @"type": IndexDataType, offset: GLintptr) void;
        extern fn uniform4f(location: UniformLocation, v0: f32, v1: f32, v2: f32, v3: f32) void;
        // TODO: uniform[1234][uif][v]

        pub const better = struct {
            pub fn buildShader(src: []const u8, kind: ShaderType) !Shader {
                const shader = webgl2.createShader(kind);
                errdefer webgl2.deleteShader(shader);
                webgl2.shaderSource(shader, src.ptr, src.len);
                webgl2.compileShader(shader);
                // TODO: fix this
                if (false and !webgl2.getShaderParameter(shader, .COMPILE_STATUS).compile_status) {
                    var buf: [2048]u8 = undefined;
                    const len = webgl2.getShaderInfoLog(shader, &buf, buf.len);
                    std.log.err("Failed to compile shader: {s}", .{buf[0..len]});
                    return error.ShaderCreationError;
                }
                return shader;
            }

            pub fn buildProgram(vertex_src: []const u8, fragment_src: []const u8) !Program {
                const program = webgl2.createProgram();
                errdefer webgl2.deleteProgram(program);
                const vertex_shader = try webgl2.better.buildShader(vertex_src, .VERTEX_SHADER);
                defer webgl2.deleteShader(vertex_shader);
                const fragment_shader = try webgl2.better.buildShader(fragment_src, .FRAGMENT_SHADER);
                defer webgl2.deleteShader(fragment_shader);
                webgl2.attachShader(program, vertex_shader);
                webgl2.attachShader(program, fragment_shader);
                webgl2.linkProgram(program);
                // TODO: fix this
                if (false and webgl2.getProgramParameter(program, .LINK_STATUS).link_status == false) {
                    var buf: [2048]u8 = undefined;
                    const len = webgl2.getProgramInfoLog(program, &buf, buf.len);
                    std.log.err("Failed to link program: {s}", .{buf[0..len]});
                    return error.ProgramCreationError;
                }
                return program;
            }

            pub fn getAttribLocation(program: Program, name: []const u8) !GLuint {
                const location = webgl2.getAttribLocation(program, name.ptr, name.len);
                if (location == -1) {
                    return error.AttributeLocationError;
                }
                return @intCast(location);
            }

            pub fn getUniformLocation(program: Program, name: []const u8) UniformLocation {
                // TODO: check that the name exists
                return webgl2.getUniformLocation(program, name.ptr, name.len);
            }
        };

        const GLenum = u32;
        const GLboolean = bool;
        const GLbitfield = u32;
        const GLbyte = i8;
        const GLshort = i16;
        const GLint = i32;
        const GLsizei = i32;
        // TODO(zig): c_longlong turns into BigNumber and fails
        const GLintptr = i32;
        const GLsizeiptr = i32;
        // const GLintptr = c_longlong;
        // const GLsizeiptr = c_longlong;
        const GLubyte = u8;
        const GLushort = u16;
        const GLuint = u32;
        const GLfloat = f32;
        const GLclampf = f32;
        const GLint64 = u64;

        const GLObject = u16;
        const Shader = enum(GLObject) { _ };
        const Program = enum(GLObject) { null = 0, _ };
        const VertexArrayObject = enum(GLObject) { null = 0, _ };
        const Buffer = enum(GLObject) { null = 0, _ };
        const UniformLocation = enum(GLObject) { _ };

        pub const Capability = enum(GLenum) {
            BLEND = 0x0BE2,
            CULL_FACE = 0x0B44,
            DEPTH_TEST = 0x0B71,
            DITHER = 0x0BD0,
            POLYGON_OFFSET_FILL = 0x8037,
            SAMPLE_ALPHA_TO_COVERAGE = 0x809E,
            SAMPLE_COVERAGE = 0x80A0,
            SCISSOR_TEST = 0x0C11,
            STENCIL_TEST = 0x0B90,
            RASTERIZER_DISCARD = 0x8C89,
        };

        pub const BlendFactor = enum(GLenum) {
            ZERO = 0,
            ONE = 1,
            SRC_COLOR = 0x0300,
            ONE_MINUS_SRC_COLOR = 0x0301,
            SRC_ALPHA = 0x0302,
            ONE_MINUS_SRC_ALPHA = 0x0303,
            DST_ALPHA = 0x0304,
            ONE_MINUS_DST_ALPHA = 0x0305,
            DST_COLOR = 0x0306,
            ONE_MINUS_DST_COLOR = 0x0307,
            SRC_ALPHA_SATURATE = 0x0308,
            CONSTANT_COLOR = 0x8001,
            ONE_MINUS_CONSTANT_COLOR = 0x8002,
            CONSTANT_ALPHA = 0x8003,
            ONE_MINUS_CONSTANT_ALPHA = 0x8004,
        };

        pub const ShaderType = enum(GLenum) {
            VERTEX_SHADER = 0x8B31,
            FRAGMENT_SHADER = 0x8B30,
        };

        pub const ShaderParameter = enum(GLenum) {
            COMPILE_STATUS = 0x8B81,
            DELETE_STATUS = 0x8B80,
            SHADER_TYPE = 0x8B4F,
        };

        pub const ProgramParameter = enum(GLenum) {
            DELETE_STATUS = 0x8B80,
            LINK_STATUS = 0x8B82,
            VALIDATE_STATUS = 0x8B83,
            ATTACHED_SHADERS = 0x8B85,
            ACTIVE_ATTRIBUTES = 0x8B89,
            ACTIVE_UNIFORMS = 0x8B86,
        };

        pub const BufferTarget = enum(GLenum) {
            ARRAY_BUFFER = 0x8892,
            ELEMENT_ARRAY_BUFFER = 0x8893,
            COPY_READ_BUFFER = 0x8F36,
            COPY_WRITE_BUFFER = 0x8F37,
            TRANSFORM_FEEDBACK_BUFFER = 0x8C8E,
            UNIFORM_BUFFER = 0x8A11,
            PIXEL_PACK_BUFFER = 0x88EB,
            PIXEL_UNPACK_BUFFER = 0x88EC,
        };

        pub const Usage = enum(GLenum) {
            STREAM_DRAW = 0x88E0,
            STATIC_DRAW = 0x88E4,
            DYNAMIC_DRAW = 0x88E8,
            STREAM_READ = 0x88E1,
            STATIC_READ = 0x88E5,
            DYNAMIC_READ = 0x88E9,
            STREAM_COPY = 0x88E2,
            STATIC_COPY = 0x88E6,
            DYNAMIC_COPY = 0x88EA,
        };

        pub const VertexDataType = enum(GLenum) {
            BYTE = 0x1400,
            UNSIGNED_BYTE = 0x1401,
            SHORT = 0x1402,
            UNSIGNED_SHORT = 0x1403,
            INT = 0x1404,
            UNSIGNED_INT = 0x1405,
            FLOAT = 0x1406,
            HALF_FLOAT = 0x140B,
            INT_2_10_10_10_REV = 0x8D9F,
            UNSIGNED_INT_2_10_10_10_REV = 0x8368,
        };

        pub const IndexDataType = enum(GLenum) {
            UNSIGNED_BYTE = 0x1401,
            UNSIGNED_SHORT = 0x1403,
            UNSIGNED_INT = 0x1405,
        };

        pub const DrawMode = enum(GLenum) {
            POINTS = 0x0000,
            LINES = 0x0001,
            LINE_LOOP = 0x0002,
            LINE_STRIP = 0x0003,
            TRIANGLES = 0x0004,
            TRIANGLE_STRIP = 0x0005,
            TRIANGLE_FAN = 0x0006,
        };
    };

    pub const audio = struct {
        extern fn loadSound(url_ptr: [*]const u8, url_len: usize) usize;
        // extern fn isSoundLoaded(sound_id: usize) bool;
        extern fn playSound(sound_id: usize) void;
        extern fn loadAndStartLoop(url_ptr: [*]const u8, url_len: usize) usize;
        extern fn setLoopVolume(loop_id: usize, volume: f32) void;
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
    };

    pub const audio = struct {
        pub fn loadAndStartLoop(url: []const u8) usize {
            return js.audio.loadAndStartLoop(url.ptr, url.len);
        }

        pub fn loadSound(url: []const u8) usize {
            return js.audio.loadSound(url.ptr, url.len);
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

var asdf_renderer: ArbitraryShapeRenderer = undefined;

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
    .sound_queue = &sound_queue,
    .loop_volumes = &loop_volumes,
};

fn render(cmd: game.RenderQueue.Command) !void {
    switch (cmd) {
        .clear => |color| {
            const fcolor = color.toFColor();
            js.webgl2.clearColor(fcolor.r, fcolor.g, fcolor.b, fcolor.a);
            js.webgl2.clear();
        },
        .shape => |shape| {
            // TODO: cache triangulation
            if (shape.fill) |color| {
                // TODO: use another allocator
                const indices = try Triangulator.triangulate(gpa, shape.local_points);
                defer gpa.free(indices);
                assert(shape.local_points.len >= 3);

                try asdf_renderer.draw(
                    gpa,
                    shape.camera,
                    shape.parent_world_point,
                    shape.local_points,
                    indices,
                    color,
                );
            }

            if (shape.stroke) |color| {
                _ = color;
                @panic("TODO");
            }
        },
    }
}

const Sounds = std.meta.FieldEnum(@TypeOf(game.sounds));
var sound_ids: std.EnumArray(Sounds, usize) = .initUndefined();
var sound_queue: std.EnumSet(Sounds) = .initEmpty();

const Loops = std.meta.FieldEnum(@TypeOf(game.loops));
var loop_ids: std.EnumArray(Loops, usize) = .initUndefined();
var loop_volumes: std.EnumArray(Loops, f32) = .initFill(0);

export fn init() void {
    if (@import("build_options").hot_reloadable) {
        my_game = std.heap.wasm_allocator.create(game.GameState) catch unreachable;
        my_game.* = .init();
    } else {
        my_game = .init();
    }

    asdf_renderer = ArbitraryShapeRenderer.init() catch unreachable;

    inline for (comptime std.enums.values(Sounds)) |sound| {
        const path = @field(game.sounds, @tagName(sound));
        sound_ids.set(sound, js_better.audio.loadSound(path));
    }

    inline for (comptime std.enums.values(Loops)) |loop| {
        const path = @field(game.loops, @tagName(loop));
        loop_ids.set(loop, js_better.audio.loadAndStartLoop(path));
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
    web_platform.sound_queue.* = .initEmpty();
    _ = my_game.update(web_platform) catch unreachable;
    mouse.prev = mouse.cur;
    mouse.cur.scrolled = .none;
    keyboard.prev = keyboard.cur;

    // graphics
    {
        defer web_platform.render_queue.pending_commands.clearRetainingCapacity();
        var it = web_platform.render_queue.pending_commands.constIterator(0);
        while (it.next()) |cmd| render(cmd.*) catch unreachable;
    }

    // sounds
    {
        var it = web_platform.sound_queue.iterator();
        while (it.next()) |id| {
            js.audio.playSound(sound_ids.get(id));
        }
    }

    // loops
    {
        var it = web_platform.loop_volumes.iterator();
        while (it.next()) |entry| {
            const id = loop_ids.get(entry.key);
            js.audio.setLoopVolume(id, entry.value.*);
        }
    }
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
const Triangulator = kommon.Triangulator;
const Mouse = game.Mouse;
const Keyboard = game.Keyboard;
const KeyboardButton = game.KeyboardButton;

// TODO: remove this
pub const ProgramInfo = struct {
    // TODO: remove
    preamble: [:0]const u8 =
        \\#version 300 es
        \\
    ,
    vertex: [:0]const u8,
    fragment: [:0]const u8,
};

// TODO: remove duplication
const ArbitraryShapeRenderer = struct {
    const program_info: ProgramInfo = .{
        .vertex =
        \\uniform vec4 u_rect; // as top_left, size
        \\uniform vec4 u_point; // as pos, turns, scale
        \\
        \\in vec2 a_position;
        \\
        \\void main() {
        \\  float c = cos(u_point.z * 6.283185307179586);
        \\  float s = sin(u_point.z * 6.283185307179586);
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

    program: js.webgl2.Program,
    vao: js.webgl2.VertexArrayObject, // vertex array object: the draw call state, roughly
    vbo: js.webgl2.Buffer, // vertex buffer object: the vertex data itself
    ebo: js.webgl2.Buffer, // element buffer object: the triangle indices

    color_uniform: js.webgl2.UniformLocation,
    rect_uniform: js.webgl2.UniformLocation,
    point_uniform: js.webgl2.UniformLocation,

    // TODO: cleaning
    const VertexData = extern struct { position: Vec2 };

    pub fn init() !ArbitraryShapeRenderer {
        const program = try js.webgl2.better.buildProgram(
            program_info.preamble ++ program_info.vertex,
            program_info.preamble ++ program_info.fragment,
        );

        const vao = js.webgl2.createVertexArray();
        js.webgl2.bindVertexArray(vao);

        const vbo = js.webgl2.createBuffer();
        const ebo = js.webgl2.createBuffer();

        js.webgl2.bindBuffer(.ARRAY_BUFFER, vbo);
        defer js.webgl2.bindBuffer(.ARRAY_BUFFER, .null);

        js.webgl2.bindBuffer(.ELEMENT_ARRAY_BUFFER, ebo);
        defer js.webgl2.bindBuffer(.ELEMENT_ARRAY_BUFFER, .null);

        defer js.webgl2.bindVertexArray(.null);

        inline for (@typeInfo(VertexData).@"struct".fields) |field| {
            const attrib = try js.webgl2.better.getAttribLocation(program, "a_" ++ "position");
            js.webgl2.enableVertexAttribArray(attrib);
            js.webgl2.vertexAttribPointer(
                attrib,
                howManyElementsIn(field.type),
                getVertexAttribType(field.type),
                isVertexAttribNormalized(field.type),
                @sizeOf(VertexData),
                @offsetOf(VertexData, field.name),
            );
        }

        const color_uniform = js.webgl2.better.getUniformLocation(program, "u_color");
        const rect_uniform = js.webgl2.better.getUniformLocation(program, "u_rect");
        const point_uniform = js.webgl2.better.getUniformLocation(program, "u_point");

        return .{
            .program = program,
            .vao = vao,
            .vbo = vbo,
            .ebo = ebo,
            .color_uniform = color_uniform,
            .rect_uniform = rect_uniform,
            .point_uniform = point_uniform,
        };
    }

    pub fn deinit(self: *ArbitraryShapeRenderer) void {
        defer js.webgl2.deleteProgram(self.program);
        defer js.webgl2.deleteVertexArray(self.vao);
        defer js.webgl2.deleteBuffer(self.vbo);
        defer js.webgl2.deleteBuffer(self.ebo);
    }

    pub fn draw(
        self: ArbitraryShapeRenderer,
        scratch: std.mem.Allocator,
        camera: Rect,
        parent: Point,
        positions: []const Vec2,
        triangles: []const [3]usize,
        color: Color,
    ) !void {
        const cpu_buffer: []const VertexData = @ptrCast(positions);

        js.webgl2.bindBuffer(.ARRAY_BUFFER, self.vbo);
        js.webgl2.bufferData(
            .ARRAY_BUFFER,
            @ptrCast(cpu_buffer.ptr),
            @sizeOf(VertexData) * cpu_buffer.len,
            .DYNAMIC_DRAW,
        );
        defer js.webgl2.bindBuffer(.ARRAY_BUFFER, .null);

        const IndexType = u16;
        const indices: []IndexType = try scratch.alloc(IndexType, 3 * triangles.len);
        defer scratch.free(indices);

        for (triangles, 0..) |tri, i| {
            for (0..3) |k| {
                indices[i * 3 + k] = @intCast(tri[k]);
            }
        }

        js.webgl2.bindBuffer(.ELEMENT_ARRAY_BUFFER, self.ebo);
        js.webgl2.bufferData(
            .ELEMENT_ARRAY_BUFFER,
            @ptrCast(indices.ptr),
            @sizeOf(IndexType) * indices.len,
            .DYNAMIC_DRAW,
        );
        defer js.webgl2.bindBuffer(.ELEMENT_ARRAY_BUFFER, .null);

        js.webgl2.bindVertexArray(self.vao);
        defer js.webgl2.bindVertexArray(.null);

        js.webgl2.useProgram(self.program);
        defer js.webgl2.useProgram(.null);

        const fcol = color.toFColor();
        js.webgl2.uniform4f(self.color_uniform, fcol.r, fcol.g, fcol.b, fcol.a);

        js.webgl2.uniform4f(self.rect_uniform, camera.top_left.x, camera.top_left.y, camera.size.y, camera.size.y);
        js.webgl2.uniform4f(self.point_uniform, parent.pos.x, parent.pos.y, parent.turns, parent.scale);

        js.webgl2.drawElements(.TRIANGLES, @intCast(3 * triangles.len), switch (IndexType) {
            u16 => .UNSIGNED_SHORT,
            else => @compileError("not implemented"),
        }, 0);
    }

    fn getVertexAttribType(x: type) js.webgl2.VertexDataType {
        return switch (x) {
            f32 => .FLOAT,
            Vec2 => .FLOAT,
            Color => .UNSIGNED_BYTE,
            else => @compileError(std.fmt.comptimePrint("unhandled type: {any}", .{x})),
        };
    }

    fn howManyElementsIn(x: type) comptime_int {
        return switch (x) {
            f32 => 1,
            Vec2 => 2,
            Color => 4,
            else => @compileError(std.fmt.comptimePrint("unhandled type: {any}", .{x})),
        };
    }

    fn isVertexAttribNormalized(x: type) js.webgl2.GLboolean {
        return switch (x) {
            f32 => false,
            Vec2 => false,
            Color => true,
            else => @compileError(std.fmt.comptimePrint("unhandled type: {any}", .{x})),
        };
    }
};
