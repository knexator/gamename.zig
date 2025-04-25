const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const hot_reloading = b.option(
        bool,
        "hot-reloadable",
        "If true, will compile the game logic as a separate dynamic library.",
    ) orelse (optimize == .Debug);

    const sdl_lib = b.dependency("sdl", .{
        .target = target,
        .optimize = optimize,
    }).artifact("SDL3");

    const kommon_module = b.dependency("kommon", .{
        .target = target,
        .optimize = optimize,
    }).module("kommon");

    const build_options = b.addOptions();
    if (hot_reloading) {
        const game_module = b.createModule(.{
            .root_source_file = b.path("src/game.zig"),
            .target = target,
            .optimize = optimize,
            .pic = true,
        });
        const game_lib = b.addLibrary(.{
            .name = "game",
            .root_module = game_module,
            .linkage = .dynamic,
            // waiting for https://github.com/ziglang/zig/issues/23442
            // .use_llvm = false,
        });
        b.installArtifact(game_lib);
        const install_game_lib = b.addInstallArtifact(game_lib, .{});
        b.getInstallStep().dependOn(&install_game_lib.step);

        // HACK
        // hot_reload_options.args.clearRetainingCapacity();
        // build_options.addOptionPath("lib_path", install_game_lib.emitted_bin.?);
        const path_to_lib = b.fmt("{s}/{s}/{s}", .{
            b.install_path,
            switch (install_game_lib.dest_dir.?) {
                .lib => "lib",
                .bin => "bin",
                .custom, .header, .prefix => unreachable,
            },
            install_game_lib.dest_sub_path,
        });
        build_options.addOption(?[]const u8, "game_dynlib_path", path_to_lib);
    } else {
        build_options.addOption(?[]const u8, "game_dynlib_path", null);
    }

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/sdl_platform.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_module.linkLibrary(sdl_lib);
    exe_module.addImport("kommon", kommon_module);
    exe_module.addOptions("build_options", build_options);

    const exe = b.addExecutable(.{
        .name = "game",
        .root_module = exe_module,
    });
    b.installArtifact(exe);

    if (b.option(bool, "emit-llvm-ir", "Emit LLVM IR") orelse false) {
        b.getInstallStep().dependOn(&b.addInstallFile(exe.getEmittedLlvmIr(), "platform.ir").step);
    }

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

    // TODO: wasm
    if (false) {
        const wasm_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = b.resolveTargetQuery(.{
                .cpu_arch = .wasm32,
                .os_tag = .freestanding,
            }),
            .optimize = optimize,
        });
        wasm_module.addImport("kommon", kommon_module);

        const wasm = b.addExecutable(.{
            .name = "game",
            .root_module = wasm_module,
        });
        b.installArtifact(wasm);
    }
}
