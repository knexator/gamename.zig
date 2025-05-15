pub const RenderableInfo = struct {
    VertexData: type,
    IndexType: type,
    UniformTypes: type,
    vertex: [:0]const u8,
    fragment: [:0]const u8,

    // TODO: pub fn fromType(...)
};

pub const RenderQueue = struct {
    arena: std.heap.ArenaAllocator,
    pending_commands: std.SegmentedList(Command, 32),

    pub const PrecomputedShape = struct {
        pub const IndexType = u16;

        local_points: []const Vec2,
        triangles: []const [3]IndexType,

        pub fn fromPoints(gpa: std.mem.Allocator, points: []const Vec2) !PrecomputedShape {
            std.debug.assert(points.len >= 3);
            const triangles = try Triangulator.triangulate(IndexType, gpa, points);
            return .{
                // TODO: clarify ownership
                .local_points = try gpa.dupe(Vec2, points),
                .triangles = triangles,
            };
        }
    };

    pub const Command = union(enum) {
        clear: Color,
        shape: struct {
            camera: Rect,
            parent_world_point: Point,
            local_points: []const Vec2,
            stroke: ?Color,
            fill: ?Color,
        },
        precomputed_shape: struct {
            camera: Rect,
            parent_world_point: Point,
            data: *const PrecomputedShape,
            fill: Color,
        },
        text: struct {
            camera: Rect,
            // TODO: maybe bottom_left + em should be a Point?
            bottom_left: Vec2,
            em: f32,
            line: []const u8,
            // TODO: *const Font
            // TODO: color
        },
    };

    pub fn init(parent_allocator: std.mem.Allocator) RenderQueue {
        return .{
            .arena = .init(parent_allocator),
            .pending_commands = .{},
        };
    }

    pub fn deinit(self: RenderQueue) void {
        self.arena.deinit();
    }

    pub fn clear(self: *RenderQueue, color: Color) !void {
        try self.pending_commands.append(self.arena.allocator(), .{ .clear = color });
    }

    pub fn drawShape(
        self: *RenderQueue,
        camera: Rect,
        parent_world_point: Point,
        /// will be duplicated
        local_points: []const Vec2,
        stroke: ?Color,
        fill: ?Color,
    ) !void {
        const owned_points = try self.arena.allocator().dupe(Vec2, local_points);
        try self.pending_commands.append(self.arena.allocator(), .{
            .shape = .{
                .camera = camera,
                .parent_world_point = parent_world_point,
                .local_points = owned_points,
                .stroke = stroke,
                .fill = fill,
            },
        });
    }
};

const std = @import("std");
const kommon = @import("kommon");
const math = kommon.math;
const Color = math.Color;
const FColor = math.FColor;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
const Triangulator = kommon.Triangulator;
