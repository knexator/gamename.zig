const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const sdl_lib = b.dependency("sdl", .{
        .target = target,
        .optimize = optimize,
    }).artifact("SDL3");

    const kommon_module = b.dependency("kommon", .{
        .target = target,
        .optimize = optimize,
    }).module("kommon");

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_module.linkLibrary(sdl_lib);
    exe_module.addImport("kommon", kommon_module);

    const exe = b.addExecutable(.{
        .name = "game",
        .root_module = exe_module,
    });
    b.installArtifact(exe);

    {
        const run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        const run_step = b.step("run", "Run the app");
        run_step.dependOn(&run_cmd.step);
    }

    {
        const exe_unit_tests = b.addTest(.{
            .root_module = exe_module,
        });
        const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
        const test_step = b.step("test", "Run unit tests");
        test_step.dependOn(&run_exe_unit_tests.step);
    }

    {
        const exe_check = b.addExecutable(.{
            .name = "foo",
            .root_module = exe_module,
        });

        const check = b.step("check", "Check if the project compiles");
        check.dependOn(&exe_check.step);
    }
}
