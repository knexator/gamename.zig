pub const RenderQueue = struct {
    arena: std.heap.ArenaAllocator,
    pending_commands: std.SegmentedList(Command, 32),

    pub const Command = union(enum) {
        clear: Color,
        shape: struct {
            camera: Rect,
            parent_world_point: Point,
            local_points: []const Vec2,
            stroke: ?Color,
            fill: ?Color,
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
const math = @import("kommon").math;
const Color = math.Color;
const Camera = math.Camera;
const Rect = math.Rect;
const Point = math.Point;
const Vec2 = math.Vec2;
