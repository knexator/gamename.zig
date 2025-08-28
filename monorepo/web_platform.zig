const JsReader = struct {
    file_index: usize,

    pub fn isReady(self: JsReader) bool {
        return js.reader.isLoaded(self.file_index);
    }

    pub fn isNull(self: JsReader) bool {
        return js.reader.isNull(self.file_index);
    }

    pub const Error = error{FileNotReady};
    pub fn read(self: *JsReader, buf: []u8) Error!usize {
        if (buf.len == 0) return 0;
        if (!self.isReady()) return error.FileNotReady;
        const bytes_readed = js.reader.readInto(self.file_index, buf.ptr, buf.len);
        return bytes_readed;
    }

    pub fn reader(self: *JsReader) std.io.Reader(*JsReader, Error, read) {
        log_for_load_save_bug.debug("getting reader for self with index {d}", .{self.file_index});
        return .{ .context = self };
    }
};

const js = struct {
    extern fn setCursor(cursor: Mouse.Cursor) void;

    pub const debug = struct {
        extern fn logInt(arg: u32) void;
        extern fn logFloat(arg: f32) void;
        extern fn logString(ptr: [*]const u8, len: usize) void;
    };

    pub const canvas = struct {
        extern fn getWidth() u32;
        extern fn getHeight() u32;
    };

    pub const images = struct {
        extern fn preloadImage(url_ptr: [*]const u8, url_len: usize) usize;
        extern fn preloadImageFromBase64Data(base64_ptr: [*]const u8, base64_len: usize) usize;
        extern fn imageWidth(image_id: usize) usize;
        extern fn imageHeight(image_id: usize) usize;
    };

    // current direction: closely matching the webgl2 API
    pub const webgl2 = struct {
        extern fn clearColor(r: f32, g: f32, b: f32, a: f32) void;
        // TODO: clear(mask: GLbitfield) gl.DEPTH_BUFFER_BIT | gl.COLOR_BUFFER_BIT | gl.STENCIL_CLEAR_VALUE
        extern fn clear() void;
        extern fn enable(capability: Capability) void;
        extern fn disable(capability: Capability) void;
        extern fn blendFunc(sfactor: BlendFactor, dfactor: BlendFactor) void;
        extern fn blendFuncSeparate(srcRGB: BlendFactor, dstRGB: BlendFactor, srcAlpha: BlendFactor, dstAlpha: BlendFactor) void;
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
        extern fn vertexAttribDivisor(index: GLuint, divisor: GLuint) void;
        extern fn drawArrays(mode: DrawMode, first: GLint, count: GLsizei) void;
        extern fn drawElements(mode: DrawMode, count: GLsizei, @"type": IndexDataType, offset: GLintptr) void;
        extern fn drawElementsInstanced(mode: DrawMode, count: GLsizei, @"type": IndexDataType, offset: GLintptr, instanceCount: GLsizei) void;
        extern fn uniform4f(location: UniformLocation, v0: f32, v1: f32, v2: f32, v3: f32) void;
        extern fn uniform1f(location: UniformLocation, v0: f32) void;
        // uniform1i
        // TODO: uniform[1234][uif][v]
        extern fn createTexture() Texture;
        extern fn deleteTexture(texture: Texture) void;
        extern fn bindTexture(target: TextureBindPointGeneral, texture: Texture) void;
        extern fn texParameteri(target: TextureBindPointGeneral, pname: TexParameter, param: TexParameterValue) void;
        // TODO: more variants
        extern fn texImage2D_basic(
            target: TextureBindPointSpecific,
            level: GLint,
            // TODO
            internalformat: enum(GLenum) { RGBA = 0x1908, RGB = 0x1907 },
            // TODO
            format: enum(GLenum) { RGBA = 0x1908, RGB = 0x1907 },
            type: enum(GLenum) { UNSIGNED_BYTE = 0x1401 },
            /// index of a ImageData, HTMLImageElement, HTMLCanvasElement, HTMLVideoElement, or ImageBitmap.
            pixels: usize,
        ) void;
        extern fn generateMipmap(target: TextureBindPointGeneral) void;

        pub const better = struct {
            pub fn buildShader(src: []const u8, kind: ShaderType) !Shader {
                assert(!std.mem.startsWith(u8, src, "#version"));
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
                if (location == -1) return error.AttributeLocationError;
                return @intCast(location);
            }

            pub fn getUniformLocation(program: Program, name: []const u8) !UniformLocation {
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

        const GLObject = c_uint;
        const Shader = enum(GLObject) { _ };
        const Program = enum(GLObject) { null = 0, _ };
        const VertexArrayObject = enum(GLObject) { null = 0, _ };
        const Buffer = enum(GLObject) { null = 0, _ };
        const UniformLocation = enum(GLObject) { _ };
        const Texture = enum(GLObject) { null = 0, _ };

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

        pub const TextureBindPointGeneral = enum(GLenum) {
            TEXTURE_2D = 0x0DE1,
            TEXTURE_CUBE_MAP = 0x8513,
            TEXTURE_3D = 0x806F,
            TEXTURE_2D_ARRAY = 0x8C1A,
        };

        pub const TextureBindPointSpecific = enum(GLenum) {
            TEXTURE_2D = 0x0DE1,
            // TODO: numbers
            // TEXTURE_CUBE_MAP_POSITIVE_X = 0x0000,
            // TEXTURE_CUBE_MAP_NEGATIVE_X = 0x0000,
            // TEXTURE_CUBE_MAP_POSITIVE_Y = 0x0000,
            // TEXTURE_CUBE_MAP_NEGATIVE_Y = 0x0000,
            // TEXTURE_CUBE_MAP_POSITIVE_Z = 0x0000,
            // TEXTURE_CUBE_MAP_NEGATIVE_Z = 0x0000,
        };

        pub const TexParameter = enum(GLenum) {
            TEXTURE_MAG_FILTER = 0x2800,
            TEXTURE_MIN_FILTER = 0x2801,
            // TODO: https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/texParameter
        };

        // TODO: somehow, separate these by param?
        pub const TexParameterValue = enum(GLint) {
            NEAREST = 0x2600,
            LINEAR = 0x2601,
            NEAREST_MIPMAP_NEAREST = 0x2700,
            LINEAR_MIPMAP_NEAREST = 0x2701,
            NEAREST_MIPMAP_LINEAR = 0x2702,
            LINEAR_MIPMAP_LINEAR = 0x2703,
            _,
        };
    };

    pub const audio = struct {
        extern fn loadSound(url_ptr: [*]const u8, url_len: usize) usize;
        // extern fn isSoundLoaded(sound_id: usize) bool;
        extern fn playSound(sound_id: usize) void;
        extern fn loadAndStartLoop(url_ptr: [*]const u8, url_len: usize) usize;
        extern fn setLoopVolume(loop_id: usize, volume: f32) void;
        extern fn enqueueSamples(src_ptr: [*]const f32, src_len: usize) void;
        extern fn queuedSeconds() f32;
    };

    pub const storage = struct {
        extern fn downloadAsFile(filename_ptr: [*]const u8, filename_len: usize, mime_ptr: [*]const u8, mime_len: usize, contents_ptr: [*]const u8, contents_len: usize) void;
        /// returns index of reader
        extern fn askUserForFile() usize;
    };

    pub const reader = struct {
        extern fn isLoaded(file_index: usize) bool;
        extern fn isNull(file_index: usize) bool;
        /// returns bytes readed
        extern fn readInto(file_index: usize, dst_ptr: [*]u8, dst_len: usize) usize;
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

    pub const images = struct {
        pub fn preloadImage(url: []const u8) usize {
            return js.images.preloadImage(url.ptr, url.len);
        }
        pub fn preloadImageFromBase64Data(data: []const u8) usize {
            return js.images.preloadImageFromBase64Data(data.ptr, data.len);
        }
        pub fn resolution(image_id: usize) UVec2 {
            return .new(
                js.images.imageWidth(image_id),
                js.images.imageHeight(image_id),
            );
        }
    };

    pub const audio = struct {
        pub fn loadAndStartLoop(url: []const u8) usize {
            return js.audio.loadAndStartLoop(url.ptr, url.len);
        }

        pub fn loadSound(url: []const u8) usize {
            return js.audio.loadSound(url.ptr, url.len);
        }

        pub fn enqueueSamples(src: []const f32) void {
            return js.audio.enqueueSamples(src.ptr, src.len * @sizeOf(f32));
        }

        // to avoid the callconv(.c) thing
        pub fn queuedSeconds() f32 {
            return js.audio.queuedSeconds();
        }
    };

    pub const storage = struct {
        pub fn downloadAsFile(filename: []const u8, mime: enum { txt }, contents: []const u8) void {
            const mime_str = switch (mime) {
                .txt => "text/plain",
            };
            js.storage.downloadAsFile(filename.ptr, filename.len, mime_str.ptr, mime_str.len, contents.ptr, contents.len);
        }
    };
};

const GameState = @import("GameState");
const PlatformGives = kommon.engine.PlatformGivesFor(GameState);
const stuff = GameState.stuff;

const log_for_load_save_bug = std.log.scoped(.load_save_bug);

comptime {
    std.testing.refAllDeclsRecursive(GameState);
}

// TODO: hot reloading is not working :(

var my_game: GameState = undefined;

const gpa = std.heap.wasm_allocator;

// TODO: remove this
var global_gpa_BAD: std.mem.Allocator = gpa;

var web_platform: PlatformGives = .{
    .gpa = gpa,
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
    .setKeyChanged = setKeyChanged,
    .setButtonChanged = setButtonChanged,
    .delta_seconds = 0,
    .aspect_ratio = undefined,
    .global_seconds = 0,
    .sound_queue = &sound_queue,
    .loop_volumes = &loop_volumes,
    .sample_rate = 48000,
    .enqueueSamples = js_better.audio.enqueueSamples,
    .queuedSeconds = js_better.audio.queuedSeconds,
    .gl = web_gl.vtable,
    .downloadAsFile = struct {
        fn anon(filename: []const u8, contents: []const u8) void {
            return js_better.storage.downloadAsFile(filename, .txt, contents);
        }
    }.anon,
    .askUserForFile = struct {
        fn anon() void {
            // JS doesn't correctly know if the file is null :(
            // assert(user_uploaded_file == null or user_uploaded_file.?.isNull());
            const index = js.storage.askUserForFile();
            user_uploaded_file = .{ .file_index = index };
            log_for_load_save_bug.debug("in web_platform.askUserForFile, index was set to {d} / {d}", .{ user_uploaded_file.?.file_index, index });

            // hack since we miss any events that happened while the user was picking the file
            // TODO: make robust by querying JS for the whole state, maybe
            mouse.cur.scrolled = .none;
            mouse.cur.buttons = .{
                .left = false,
                .middle = false,
                .right = false,
            };
        }
    }.anon,
    .userUploadedFile = struct {
        fn anon() ?std.io.AnyReader {
            if (user_uploaded_file == null) return null;
            log_for_load_save_bug.debug("in web_platform.userUploadedFile, index is {d}", .{user_uploaded_file.?.file_index});
            if (!user_uploaded_file.?.isReady()) return null;
            // TODO(zig): removing the log causes a bug!
            log_for_load_save_bug.debug("second log: index is {d}", .{user_uploaded_file.?.file_index});
            const reader = user_uploaded_file.?.reader();
            log_for_load_save_bug.info("reader is: {any}", .{reader});
            const reader_any = reader.any();
            log_for_load_save_bug.debug("anyreader is: {any}", .{reader_any});
            return reader_any;
        }
    }.anon,
    .forgetUserUploadedFile = struct {
        fn anon() void {
            assert(user_uploaded_file != null);
            user_uploaded_file = null;
        }
    }.anon,
    .setCursor = struct {
        fn anon(cursor: Mouse.Cursor) void {
            js.setCursor(cursor);
        }
    }.anon,
};

var user_uploaded_file: ?JsReader = null;

const Sounds = std.meta.FieldEnum(@TypeOf(stuff.sounds));
var sound_ids: std.EnumArray(Sounds, usize) = .initUndefined();
var sound_queue: std.EnumSet(Sounds) = .initEmpty();

const Loops = std.meta.FieldEnum(@TypeOf(stuff.loops));
var loop_ids: std.EnumArray(Loops, usize) = .initUndefined();
var loop_volumes: std.EnumArray(Loops, f32) = .initFill(0);

// TODO: allow loading images during the game
const PreloadedImages = std.meta.FieldEnum(@TypeOf(stuff.preloaded_images));
var preloaded_image_ids: std.EnumArray(PreloadedImages, usize) = .initUndefined();
var images_pointers: std.EnumArray(GameState.Images, *const anyopaque) = .initUndefined();

var other_images: std.SegmentedList(usize, 16) = .{};

const web_gl = struct {
    pub const vtable: Gl = .{
        .clear = clear,
        .buildRenderable = buildRenderable,
        .useRenderable = useRenderable,
        .buildTexture2D = buildTexture2D,
        .buildInstancedRenderable = buildInstancedRenderable,
        .useInstancedRenderable = useInstancedRenderable,
        .loadTextureDataFromBase64 = loadTextureDataFromBase64,
        .loadTextureDataFromFilename = loadTextureDataFromFilename,
    };

    pub fn clear(color: FColor) void {
        js.webgl2.clearColor(color.r, color.g, color.b, color.a);
        js.webgl2.clear();
    }

    pub fn loadTextureDataFromFilename(path: [:0]const u8) *const anyopaque {
        other_images.append(global_gpa_BAD, js_better.images.preloadImage(path)) catch @panic("TODO");
        return other_images.at(other_images.len - 1);
    }

    pub fn loadTextureDataFromBase64(base64: []const u8) *const anyopaque {
        other_images.append(global_gpa_BAD, js_better.images.preloadImageFromBase64Data(base64)) catch @panic("TODO");
        return other_images.at(other_images.len - 1);
    }

    pub fn buildTexture2D(data: *const anyopaque, pixelart: bool) Gl.Texture {
        const image_id: *const usize = @alignCast(@ptrCast(data));

        // TODO
        // const has_alpha = ...;

        const texture = js.webgl2.createTexture();
        js.webgl2.bindTexture(.TEXTURE_2D, texture);
        js.webgl2.texImage2D_basic(
            .TEXTURE_2D,
            0,
            .RGBA,
            .RGBA,
            .UNSIGNED_BYTE,
            image_id.*,
        );
        if (pixelart) {
            js.webgl2.texParameteri(.TEXTURE_2D, .TEXTURE_MAG_FILTER, .NEAREST);
            js.webgl2.texParameteri(.TEXTURE_2D, .TEXTURE_MIN_FILTER, .NEAREST);
        } else {
            js.webgl2.generateMipmap(.TEXTURE_2D);
            // TODO: let user choose quality
            js.webgl2.texParameteri(.TEXTURE_2D, .TEXTURE_MAG_FILTER, .LINEAR);
            // js.webgl2.texParameteri(.TEXTURE_2D, .TEXTURE_MIN_FILTER, .NEAREST_MIPMAP_LINEAR);
            js.webgl2.texParameteri(.TEXTURE_2D, .TEXTURE_MIN_FILTER, .LINEAR_MIPMAP_LINEAR);
        }

        return .{ .id = @intFromEnum(texture), .resolution = js_better.images.resolution(image_id.*) };
    }

    pub fn buildRenderable(
        vertex_src: [:0]const u8,
        fragment_src: [:0]const u8,
        attributes: Gl.VertexInfo.Collection,
        uniforms: []const Gl.UniformInfo.In,
    ) !Gl.Renderable {
        const program = try js.webgl2.better.buildProgram(
            vertex_src,
            fragment_src,
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

        for (attributes.attribs, 0..) |attribute, k| {
            const index = try js.webgl2.better.getAttribLocation(program, attribute.name);
            js.webgl2.enableVertexAttribArray(index);
            js.webgl2.vertexAttribPointer(
                index,
                @intFromEnum(attribute.kind.count()),
                @enumFromInt(@intFromEnum(attribute.kind.type())),
                attribute.kind.normalized(),
                // TODO: check in debugger if this is computed once
                @intCast(attributes.getStride()),
                @intCast(attributes.getOffset(k)),
            );
        }

        var uniforms_data = std.BoundedArray(Gl.UniformInfo, 8).init(0) catch unreachable;
        for (uniforms) |uniform| {
            const location = try js.webgl2.better.getUniformLocation(program, uniform.name);
            uniforms_data.append(.{
                .location = @intCast(@intFromEnum(location)),
                .name = uniform.name,
                .kind = uniform.kind,
            }) catch return error.TooManyUniforms;
        }

        return .{
            .program = @enumFromInt(@intFromEnum(program)),
            .vao = @enumFromInt(@intFromEnum(vao)),
            .vbo = @enumFromInt(@intFromEnum(vbo)),
            .ebo = @enumFromInt(@intFromEnum(ebo)),
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
            js.webgl2.bindBuffer(.ARRAY_BUFFER, @enumFromInt(@intFromEnum(renderable.vbo)));
            js.webgl2.bufferData(
                .ARRAY_BUFFER,
                @ptrCast(vertices_ptr),
                vertices_len_bytes,
                .DYNAMIC_DRAW,
            );
            js.webgl2.bindBuffer(.ARRAY_BUFFER, .null);
        }

        {
            js.webgl2.bindBuffer(.ELEMENT_ARRAY_BUFFER, @enumFromInt(@intFromEnum(renderable.ebo)));
            js.webgl2.bufferData(
                .ELEMENT_ARRAY_BUFFER,
                @ptrCast(triangles.ptr),
                @intCast(@sizeOf([3]Gl.IndexType) * triangles.len),
                .DYNAMIC_DRAW,
            );
            js.webgl2.bindBuffer(.ELEMENT_ARRAY_BUFFER, .null);
        }

        if (texture) |t| js.webgl2.bindTexture(.TEXTURE_2D, @enumFromInt(t.id));
        defer if (texture) |_| js.webgl2.bindTexture(.TEXTURE_2D, .null);

        js.webgl2.bindVertexArray(@enumFromInt(@intFromEnum(renderable.vao)));
        defer js.webgl2.bindVertexArray(.null);

        js.webgl2.useProgram(@enumFromInt(@intFromEnum(renderable.program)));
        defer js.webgl2.useProgram(.null);

        for (uniforms) |uniform| {
            // const u = uniform.location;
            // TODO
            const u: js.webgl2.UniformLocation = @enumFromInt(blk: {
                for (renderable.uniforms.slice()) |u| {
                    if (std.mem.eql(u8, u.name, uniform.name)) break :blk u.location;
                } else unreachable;
            });
            switch (uniform.value) {
                .FColor => |v| js.webgl2.uniform4f(u, v.r, v.g, v.b, v.a),
                .Rect => |v| js.webgl2.uniform4f(u, v.top_left.x, v.top_left.y, v.size.x, v.size.y),
                .Point => |v| js.webgl2.uniform4f(u, v.pos.x, v.pos.y, v.turns, v.scale),
                .f32 => |v| js.webgl2.uniform1f(u, v),
            }
        }

        js.webgl2.drawElements(.TRIANGLES, @intCast(3 * triangles.len), switch (Gl.IndexType) {
            u16 => .UNSIGNED_SHORT,
            u32 => .UNSIGNED_INT,
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
        const program = try js.webgl2.better.buildProgram(
            vertex_src,
            fragment_src,
        );

        const vao = js.webgl2.createVertexArray();
        js.webgl2.bindVertexArray(vao);
        defer js.webgl2.bindVertexArray(.null);

        const vbo_vertices = js.webgl2.createBuffer();
        const vbo_instances = js.webgl2.createBuffer();
        const ebo = js.webgl2.createBuffer();

        js.webgl2.bindBuffer(.ELEMENT_ARRAY_BUFFER, ebo);

        {
            js.webgl2.bindBuffer(.ARRAY_BUFFER, vbo_vertices);

            const attributes = per_vertex_attributes;
            for (attributes.attribs, 0..) |attribute, k| {
                const index = try js.webgl2.better.getAttribLocation(program, attribute.name);
                js.webgl2.enableVertexAttribArray(index);
                js.webgl2.vertexAttribPointer(
                    index,
                    @intFromEnum(attribute.kind.count()),
                    @enumFromInt(@intFromEnum(attribute.kind.type())),
                    attribute.kind.normalized(),
                    // TODO: check in debugger if this is computed once
                    @intCast(attributes.getStride()),
                    @intCast(attributes.getOffset(k)),
                );
            }
        }

        {
            js.webgl2.bindBuffer(.ARRAY_BUFFER, vbo_instances);

            const attributes = per_instance_attributes;
            for (attributes.attribs, 0..) |attribute, k| {
                const index = try js.webgl2.better.getAttribLocation(program, attribute.name);
                js.webgl2.enableVertexAttribArray(index);
                js.webgl2.vertexAttribPointer(
                    index,
                    @intFromEnum(attribute.kind.count()),
                    @enumFromInt(@intFromEnum(attribute.kind.type())),
                    attribute.kind.normalized(),
                    // TODO: check in debugger if this is computed once
                    @intCast(attributes.getStride()),
                    @intCast(attributes.getOffset(k)),
                );
                js.webgl2.vertexAttribDivisor(index, 1);
            }
        }

        var uniforms_data = std.BoundedArray(Gl.UniformInfo, 8).init(0) catch unreachable;
        for (uniforms) |uniform| {
            const location = try js.webgl2.better.getUniformLocation(program, uniform.name);
            uniforms_data.append(.{
                .location = @intCast(@intFromEnum(location)),
                .name = uniform.name,
                .kind = uniform.kind,
            }) catch return error.TooManyUniforms;
        }

        return .{
            .program = @enumFromInt(@intFromEnum(program)),
            .vao = @enumFromInt(@intFromEnum(vao)),
            .vbo_vertices = @enumFromInt(@intFromEnum(vbo_vertices)),
            .vbo_instances = @enumFromInt(@intFromEnum(vbo_instances)),
            .ebo = @enumFromInt(@intFromEnum(ebo)),
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
            js.webgl2.bindBuffer(.ARRAY_BUFFER, @enumFromInt(@intFromEnum(renderable.vbo_vertices)));
            js.webgl2.bufferData(
                .ARRAY_BUFFER,
                @ptrCast(vertex_data_ptr),
                vertex_data_len_bytes,
                .DYNAMIC_DRAW,
            );
            js.webgl2.bindBuffer(.ARRAY_BUFFER, .null);
        }

        {
            js.webgl2.bindBuffer(.ELEMENT_ARRAY_BUFFER, @enumFromInt(@intFromEnum(renderable.ebo)));
            js.webgl2.bufferData(
                .ELEMENT_ARRAY_BUFFER,
                @ptrCast(triangles.ptr),
                @intCast(@sizeOf([3]Gl.IndexType) * triangles.len),
                .DYNAMIC_DRAW,
            );
            js.webgl2.bindBuffer(.ELEMENT_ARRAY_BUFFER, .null);
        }

        {
            js.webgl2.bindBuffer(.ARRAY_BUFFER, @enumFromInt(@intFromEnum(renderable.vbo_instances)));
            js.webgl2.bufferData(
                .ARRAY_BUFFER,
                @ptrCast(instance_data_ptr),
                instance_data_len_bytes,
                .DYNAMIC_DRAW,
            );
            js.webgl2.bindBuffer(.ARRAY_BUFFER, .null);
        }

        // TODO
        assert(texture == null);
        // if (texture) |t| js.webgl2.bindTexture(.TEXTURE_2D, @enumFromInt(t.id));
        // defer if (texture) |_| js.webgl2.bindTexture(.TEXTURE_2D, .null);

        js.webgl2.bindVertexArray(@enumFromInt(@intFromEnum(renderable.vao)));
        defer js.webgl2.bindVertexArray(.null);

        js.webgl2.useProgram(@enumFromInt(@intFromEnum(renderable.program)));
        defer js.webgl2.useProgram(.null);

        for (uniforms) |uniform| {
            // const u = uniform.location;
            // TODO
            const u: js.webgl2.UniformLocation = @enumFromInt(blk: {
                for (renderable.uniforms.slice()) |u| {
                    if (std.mem.eql(u8, u.name, uniform.name)) break :blk u.location;
                } else unreachable;
            });
            switch (uniform.value) {
                .FColor => |v| js.webgl2.uniform4f(u, v.r, v.g, v.b, v.a),
                .Rect => |v| js.webgl2.uniform4f(u, v.top_left.x, v.top_left.y, v.size.x, v.size.y),
                .Point => |v| js.webgl2.uniform4f(u, v.pos.x, v.pos.y, v.turns, v.scale),
                .f32 => |v| js.webgl2.uniform1f(u, v),
            }
        }

        js.webgl2.drawElementsInstanced(.TRIANGLES, @intCast(3 * triangles.len), switch (Gl.IndexType) {
            u16 => .UNSIGNED_SHORT,
            u32 => .UNSIGNED_INT,
            else => @compileError("not implemented"),
        }, 0, @intCast(instance_count));
    }
};

export fn setSampleRate(new_rate: f32) void {
    web_platform.sample_rate = new_rate;
}

export fn preload() void {
    inline for (comptime std.enums.values(Sounds)) |sound| {
        const path = @field(stuff.sounds, @tagName(sound));
        sound_ids.set(sound, js_better.audio.loadSound(path));
    }

    inline for (comptime std.enums.values(Loops)) |loop| {
        const path = @field(stuff.loops, @tagName(loop));
        loop_ids.set(loop, js_better.audio.loadAndStartLoop(path));
    }

    inline for (comptime std.enums.values(PreloadedImages)) |image| {
        const path = @field(stuff.preloaded_images, @tagName(image));
        preloaded_image_ids.set(image, js_better.images.preloadImage(path));
        images_pointers.set(image, preloaded_image_ids.getPtrConst(image));
    }

    if (@hasDecl(GameState, "preload")) {
        my_game.preload(web_gl.vtable) catch @panic("TODO");
    }
}

export fn getDesiredAspectRatio() f32 {
    return stuff.metadata.desired_aspect_ratio;
}

export fn getTitle() [*:0]const u8 {
    return stuff.metadata.name;
}

export fn init(random_seed: u32) void {
    js.webgl2.enable(.BLEND);
    js.webgl2.blendFunc(.SRC_ALPHA, .ONE_MINUS_SRC_ALPHA);

    my_game.init(gpa, web_platform.gl, images_pointers, @intCast(random_seed)) catch unreachable;
}

export fn update(delta_seconds: f32) void {
    keyboard.cur_time = web_platform.global_seconds;
    mouse.cur_time = web_platform.global_seconds;
    web_platform.aspect_ratio = js_better.canvas.getSize().aspectRatio();
    web_platform.delta_seconds = delta_seconds;
    web_platform.global_seconds += delta_seconds;
    web_platform.keyboard = keyboard;
    web_platform.sound_queue.* = .initEmpty();

    // update and draw
    _ = my_game.update(web_platform) catch unreachable;

    mouse.prev = mouse.cur;
    mouse.cur.scrolled = .none;
    keyboard.prev = keyboard.cur;

    // sounds
    if (@typeInfo(Sounds).@"enum".fields.len > 0) {
        var it = web_platform.sound_queue.iterator();
        while (it.next()) |id| {
            js.audio.playSound(sound_ids.get(id));
        }
    }

    // loops
    if (@typeInfo(Loops).@"enum".fields.len > 0) {
        var it = web_platform.loop_volumes.iterator();
        while (it.next()) |entry| {
            const id = loop_ids.get(entry.key);
            js.audio.setLoopVolume(id, entry.value.*);
        }
    }
}

/// positions are in [0..1]x[0..1]
var mouse: Mouse = .{ .cur = .init, .prev = .init, .cur_time = 0 };
fn setButtonChanged(key: kommon.input.MouseButton) void {
    mouse.setChanged(key);
}

export fn pointermove(x: f32, y: f32) void {
    mouse.cur.position = js_better.canvas.getRect().localFromWorldPosition(.new(x, y));
}

const MouseButton = enum(u8) {
    left = 0,
    middle = 1,
    right = 2,
    _,
};

fn buttonchanged(button: MouseButton, is_pressed: bool) void {
    switch (button) {
        .left => {
            mouse.cur.buttons.left = is_pressed;
            mouse.last_change_at.left = web_platform.global_seconds;
        },
        .middle => {
            mouse.cur.buttons.middle = is_pressed;
            mouse.last_change_at.middle = web_platform.global_seconds;
        },
        .right => {
            mouse.cur.buttons.right = is_pressed;
            mouse.last_change_at.right = web_platform.global_seconds;
        },
        _ => {},
    }
}

export fn pointerup(button: MouseButton) void {
    buttonchanged(button, false);
}

export fn pointerdown(button: MouseButton) void {
    buttonchanged(button, true);
}

export fn wheel(delta_y: i32) void {
    mouse.cur.scrolled = if (delta_y == 0)
        .none
    else if (delta_y > 0)
        .down
    else
        .up;
}

var keyboard = Keyboard{ .cur = .init, .prev = .init, .cur_time = 0 };
fn setKeyChanged(key: KeyboardButton) void {
    keyboard.setChanged(key);
}
const KeyCode = KeyboardButton;

export fn keydown(code: KeyCode) void {
    keychanged(code, true);
}

export fn keyup(code: KeyCode) void {
    keychanged(code, false);
}

fn keychanged(key: KeyCode, is_pressed: bool) void {
    switch (key) {
        inline else => |x| {
            @field(keyboard.cur.keys, @tagName(x)) = is_pressed;
            @field(keyboard.last_change_at, @tagName(x)) = web_platform.global_seconds;
        },
    }
}

pub const std_options = std.Options{
    // wasm-freestanding has no stderr, so we have to override this function
    .logFn = myLogFn,
    .log_scope_levels = &.{.{ .scope = .load_save_bug, .level = .info }},
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
const FColor = math.FColor;
const Camera = math.Camera;
const Point = math.Point;
const Rect = math.Rect;
const Mouse = kommon.input.Mouse;
const Keyboard = kommon.input.Keyboard;
const KeyboardButton = kommon.input.KeyboardButton;
const Gl = kommon.Gl;
