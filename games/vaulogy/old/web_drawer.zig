const WebDrawer = struct {
    pub fn asdfBackground() void {}

    fn screenFromWorld(camera: Camera, world_point: Point) Point {
        const rect = camera.toRect();
        const local = Point.inverseApplyGetLocal(Point{
            .pos = rect.top_left,
            .scale = rect.size.y,
        }, world_point);
        const screen = Point{ .pos = .zero, .scale = js_better.canvas.getSize().y };
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

    pub fn setTransparency(alpha: f32) void {
        js.canvas.setGlobalAlpha(alpha);
    }

    pub fn clipAtomRegion(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ [2]Vec2{ .new(2.3, 1), .new(2.3, -1) }
        else
            [_]Vec2{
                Vec2.new(-0.5, 0),
                Vec2.new(0, 1),
                Vec2.new(2.3, 1),
                Vec2.new(2.3, -1),
                Vec2.new(0, -1),
            };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js.canvas.save();
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.clip();
    }

    pub fn endClip() void {
        js.canvas.restore();
    }

    pub fn drawLine(camera: Camera, points: []const Vec2, color: Color) void {
        const screen_positions = gpa.allocator().alloc(Vec2, points.len) catch @panic("OoM");
        defer gpa.allocator().free(screen_positions);

        for (points, screen_positions) |world_pos, *screen_pos| {
            screen_pos.* = screenFromWorldPosition(camera, world_pos);
        }

        js.canvas.beginPath();
        js_better.canvas.setStrokeColor(color);
        js_better.canvas.path(screen_positions);
        js.canvas.stroke();
    }

    pub fn drawShapeV2(camera: Camera, parent_world_point: Point, local_points: []const Vec2, stroke: ?Color, fill: ?Color) void {
        const screen_positions = gpa.allocator().alloc(Vec2, local_points.len) catch @panic("OoM");
        defer gpa.allocator().free(screen_positions);

        for (local_points, screen_positions) |local_pos, *screen_pos| {
            screen_pos.* = screenFromWorldPosition(camera, parent_world_point.applyToLocalPosition(local_pos));
        }

        js_better.canvas.pathLoop(screen_positions);

        if (fill) |col| {
            js_better.canvas.setFillColor(col);
            js.canvas.fill();
        }
        if (stroke) |col| {
            js.canvas.setLineWidth(1);
            js_better.canvas.setStrokeColor(col);
            js.canvas.stroke();
        }
    }

    pub fn drawShape(camera: Camera, points: []const Vec2, stroke: ?Color, fill: ?Color) void {
        const screen_positions = gpa.allocator().alloc(Vec2, points.len) catch @panic("OoM");
        defer gpa.allocator().free(screen_positions);

        for (points, screen_positions) |world_pos, *screen_pos| {
            screen_pos.* = screenFromWorldPosition(camera, world_pos);
        }

        js_better.canvas.pathLoop(screen_positions);

        if (stroke) |col| {
            js.canvas.setLineWidth(1);
            js_better.canvas.setStrokeColor(col);
            js.canvas.stroke();
        }
        if (fill) |col| {
            js_better.canvas.setFillColor(col);
            js.canvas.fill();
        }
    }

    pub fn drawRect(camera: Camera, rect: Rect, stroke: ?Color, fill: ?Color) void {
        const screen_top_left = screenFromWorldPosition(camera, rect.top_left);
        const screen_size = screenFromWorldSize(camera, rect.size);
        const screen_positions = [_]Vec2{
            screen_top_left,
            screen_top_left.addX(screen_size.x),
            screen_top_left.add(screen_size),
            screen_top_left.addY(screen_size.y),
        };
        js_better.canvas.pathLoop(&screen_positions);
        if (stroke) |col| {
            js.canvas.setLineWidth(1);
            js_better.canvas.setStrokeColor(col);
            js.canvas.stroke();
        }
        if (fill) |col| {
            js_better.canvas.setFillColor(col);
            js.canvas.fill();
        }
    }

    pub fn drawDebugText(camera: Camera, center: Point, text: [:0]const u8, color: Color) void {
        const screen_point = screenFromWorld(camera, center);
        js_better.canvas.setFillColor(color);
        // std.log.debug("scale: {d}", .{screen_point.scale});
        js.canvas.fillText(text.ptr, text.len, screen_point.pos.x, screen_point.pos.y, screen_point.scale * 0.7);
    }

    pub fn drawVariable(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const screen_point = screenFromWorld(camera, world_point);
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
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(visuals.color);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawPatternVariable(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const screen_point = screenFromWorld(camera, world_point);
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
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(visuals.color);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawAtomDebug(camera: Camera, world_point: Point) void {
        // std.debug.assert(!DESIGN.round_data);
        const local_positions = if (DESIGN.round_data)
            funk.fromCount(32, struct {
                pub fn anon(k: usize) Vec2 {
                    return Vec2.fromTurns(math.lerp(0.75, 0.25, math.tof32(k) / 32)).addX(0.5);
                }
            }.anon) ++ [_]Vec2{
                .new(2, 1),
                .new(2.2, 1.0 / 3.0),
                .new(1.8, -1.0 / 3.0),
                .new(2, -1),
            }
        else
            [_]Vec2{
                .new(-0.5, 0),
                .new(0, 1),
                .new(2, 1),
                .new(2.2, 1.0 / 3.0),
                .new(1.8, -1.0 / 3.0),
                .new(2, -1),
                .new(0, -1),
            };

        drawShapeV2(camera, world_point, &local_positions, .black, .white);
    }

    pub fn drawAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const profile = visuals.profile;
        const screen_point = screenFromWorld(camera, world_point);
        if (optimization_dont_draw_tiny and screen_point.scale < 0.1) return;
        const local_positions = if (DESIGN.round_data)
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
        var screen_positions: []Vec2 = gpa.allocator().alloc(Vec2, local_positions.len + profile.len * 2) catch @panic("TODO");
        defer gpa.allocator().free(screen_positions);
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        for (profile, 0..) |pos, i| {
            screen_positions[local_positions.len + i] = screen_point.applyToLocalPosition(
                Vec2.new(2.0 - pos.y, 1.0 - pos.x),
            );
            screen_positions[local_positions.len + profile.len * 2 - i - 1] = screen_point.applyToLocalPosition(
                Vec2.new(2.0 + pos.y, -1.0 + pos.x),
            );
        }
        js_better.canvas.pathLoop(screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(visuals.color);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
        if (visuals.display) |d| {
            const p = screen_point.applyToLocalPosition(.new(0.25, 0));
            js_better.canvas.setFillColor(.black);
            js.canvas.fillText(d.ptr, d.len, p.x, p.y, screen_point.scale);
        }
    }

    pub fn drawPatternAtom(camera: Camera, world_point: Point, visuals: presenter.AtomVisuals) void {
        const profile = visuals.profile;
        const screen_point = screenFromWorld(camera, world_point);
        if (optimization_dont_draw_tiny and screen_point.scale < 0.1) return;
        const local_positions = if (DESIGN.round_data)
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
        var screen_positions: []Vec2 = gpa.allocator().alloc(Vec2, local_positions.len + profile.len * 2) catch @panic("TODO");
        defer gpa.allocator().free(screen_positions);
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        for (profile, 0..) |pos, i| {
            screen_positions[local_positions.len + i] = screen_point.applyToLocalPosition(
                Vec2.new(-1.0 - pos.y, 1.0 - pos.x),
            );
            screen_positions[local_positions.len + profile.len * 2 - i - 1] = screen_point.applyToLocalPosition(
                Vec2.new(-1.0 + pos.y, -1.0 + pos.x),
            );
        }
        js_better.canvas.pathLoop(screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(visuals.color);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
        if (visuals.display) |d| {
            const p = screen_point.applyToLocalPosition(.new(-0.25, 0));
            js_better.canvas.setFillColor(.black);
            js.canvas.fillText(d.ptr, d.len, p.x, p.y, screen_point.scale);
        }
    }

    pub fn drawPairHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        if (optimization_dont_draw_tiny and screen_point.scale < 0.1) return;
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
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(Color.gray(96));
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawPatternPairHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        if (optimization_dont_draw_tiny and screen_point.scale < 0.1) return;
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
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(Color.gray(96));
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
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
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(1);
        js_better.canvas.setFillColor(Color.white);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.fill();
        js.canvas.stroke();
    }

    pub fn drawPatternAtomOutline(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);
        const local_positions = [_]Vec2{
            Vec2.new(0.5, 0),
            Vec2.new(0, 1),
            Vec2.new(-1, 1),
            Vec2.new(-1, -1),
            Vec2.new(0, -1),
        };
        var screen_positions: [local_positions.len]Vec2 = undefined;
        for (local_positions, 0..) |pos, i| {
            screen_positions[i] = screen_point.applyToLocalPosition(pos);
        }
        js_better.canvas.pathLoop(&screen_positions);
        js.canvas.setLineWidth(2);
        js_better.canvas.setStrokeColor(Color.cyan);
        js.canvas.stroke();
    }

    pub fn drawCable(camera: Camera, world_from: Vec2, world_to: Vec2, world_scale: f32, offset: f32) void {
        const screen_from = screenFromWorldPosition(camera, world_from);
        const screen_to = screenFromWorldPosition(camera, world_to);
        const scale = screenFromWorldScale(camera, world_scale);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.setLineWidth(1);
        js.canvas.beginPath();
        js_better.canvas.moveTo(screen_from);
        js_better.canvas.lineTo(screen_to);
        js.canvas.stroke();

        // TODO: only draw in visible bounds to avoid arbitrarily big cost on zoom
        if (true) return;
        js.canvas.setLineWidth(scale * 0.02);
        js.canvas.beginPath();
        const delta = screen_to.sub(screen_from);
        const length = delta.mag();
        const dir = delta.scale(1 / length);
        var done: f32 = 0;
        js_better.canvas.moveTo(screen_from);
        while (done < length) : (done += 1) {
            js_better.canvas.lineTo(screen_from.add(dir.scale(done)).add(dir.perpCW().scale(cableOffset(done + offset * scale, scale))));
        }
        js_better.canvas.lineTo(screen_to);
        js.canvas.stroke();
    }

    pub fn drawCaseHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);

        js_better.canvas.setStrokeColor(Color.white);
        js.canvas.beginPath();
        js_better.canvas.circle(screen_point.pos, screen_point.scale * 0.5);
        js.canvas.stroke();
    }

    pub fn drawFnkHolder(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);

        js.canvas.setLineWidth(1);
        js_better.canvas.setStrokeColor(Color.black);
        js.canvas.beginPath();
        js_better.canvas.circle(screen_point.pos, screen_point.scale * 0.5);
        js.canvas.stroke();

        js.canvas.beginPath();
        js_better.canvas.path(&.{
            screen_point.applyToLocalPosition(.new(0, -0.5)),
            screen_point.applyToLocalPosition(.new(0, -1.5)),
        });
        js.canvas.stroke();
    }

    pub fn drawWildcardsCable(camera: Camera, points: []const Vec2, visuals: []const presenter.AtomVisuals) void {
        js.canvas.setLineWidth(3);
        for (visuals) |v| {
            drawLine(camera, points, v.color);
            js.canvas.translate(3, 3);
        }
        js.canvas.resetTransform();
        js.canvas.setLineWidth(1);
    }

    pub fn drawAsdfDevice(camera: Camera, world_point: Point) void {
        const screen_point = screenFromWorld(camera, world_point);

        js_better.canvas.setStrokeColor(Color.white);
        js.canvas.beginPath();
        js.canvas.ellipse(screen_point.pos.x - screen_point.scale * 0.2 + 1, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 1.5, std.math.pi * 0.5, true);
        js.canvas.stroke();

        // Back face
        // layer1.setFillColor(Color.gray(128 - 32));
        // js.canvas.beginPath();
        // js.canvas.ellipse(screen_point.pos.x - screen_point.scale * 0.2, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 1.5, std.math.pi * 0.5, true);
        // js.canvas.lineTo(screen_point.pos.x + screen_point.scale * 0.2, screen_point.pos.y + screen_point.scale * 0.25);
        // js.canvas.ellipse(screen_point.pos.x + screen_point.scale * 0.2, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 0.5, std.math.pi * 1.5, false);
        // js.canvas.closePath();
        // js.canvas.fill();

        js_better.canvas.setFillColor(Color.white);
        js.canvas.beginPath();
        js.canvas.ellipse(screen_point.pos.x - screen_point.scale * 0.2, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 1.5, std.math.pi * 0.5, false);
        js.canvas.lineTo(screen_point.pos.x - screen_point.scale * 0.1, screen_point.pos.y + screen_point.scale * 0.25);
        js.canvas.lineTo(screen_point.pos.x - screen_point.scale * 0.05, screen_point.pos.y + screen_point.scale * 0.2);
        js.canvas.lineTo(screen_point.pos.x + screen_point.scale * 0.075, screen_point.pos.y + screen_point.scale * 0.15);
        js.canvas.lineTo(screen_point.pos.x + screen_point.scale * 0.15, screen_point.pos.y + screen_point.scale * 0.2);
        js.canvas.lineTo(screen_point.pos.x + screen_point.scale * 0.1, screen_point.pos.y + screen_point.scale * 0.25);
        js.canvas.ellipse(screen_point.pos.x + screen_point.scale * 0.2, screen_point.pos.y, screen_point.scale * 0.05, screen_point.scale * 0.25, 0, std.math.pi * 0.5, std.math.pi * 1.5, true);
        js.canvas.closePath();
        js.canvas.fill();
    }

    fn cableOffset(x: f32, scale: f32) f32 {
        const z = x * 20 / scale;
        const y = @sin(z) + 0.2 * @sin(z * 1.3) + 0.3 * @sin(z * 3.1);
        return y * 0.1 * scale;
    }
};
