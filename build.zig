const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const hot_reloadable = b.option(
        bool,
        "hot-reloadable",
        "If true, will compile the game logic as a separate dynamic library.",
    ) orelse (optimize == .Debug);

    const emit_llvm_ir = b.option(bool, "emit-llvm-ir", "Emit LLVM IR") orelse false;

    const install_step = b.getInstallStep();
    const run_step = b.step("run", "Run the app");
    const test_step = b.step("test", "Run unit tests");
    const check_step = b.step("check", "Check if the project compiles");

    // TODO: delete this variable
    const kommon_module = b.dependency("kommon", .{
        .target = target,
        .optimize = optimize,
    }).module("kommon");

    build_for_desktop(b, .{
        .install = install_step,
        .run = run_step,
        .unit_test = test_step,
        .check = check_step,
    }, .{
        .target = target,
        .optimize = optimize,
        .hot_reloadable = hot_reloadable,
        .emit_llvm_ir = emit_llvm_ir,
    }, kommon_module);
}

fn build_for_desktop(
    b: *std.Build,
    steps: struct {
        run: *std.Build.Step,
        install: *std.Build.Step,
        check: *std.Build.Step,
        unit_test: *std.Build.Step,
    },
    options: struct {
        target: std.Build.ResolvedTarget,
        optimize: std.builtin.OptimizeMode,
        hot_reloadable: bool,
        emit_llvm_ir: bool,
    },
    kommon_module: *std.Build.Module,
) void {
    const sdl_lib = b.dependency("sdl", .{
        .target = options.target,
        .optimize = options.optimize,
    }).artifact("SDL3");

    const build_options = b.addOptions();
    if (options.hot_reloadable) {
        const game_module = b.createModule(.{
            .root_source_file = b.path("src/game.zig"),
            .target = options.target,
            .optimize = options.optimize,
            .pic = true,
        });
        const game_lib = b.addLibrary(.{
            .name = "game",
            .root_module = game_module,
            .linkage = .dynamic,
            // TODO(zig): uncomment this line after solving https://github.com/ziglang/zig/issues/23442
            // .use_llvm = false,
        });
        const install_game_lib = b.addInstallArtifact(game_lib, .{});
        steps.install.dependOn(&install_game_lib.step);

        if (options.emit_llvm_ir) {
            steps.install.dependOn(&b.addInstallFile(
                game_lib.getEmittedLlvmIr(),
                "game.ir",
            ).step);
        }

        {
            const game_unit_tests = b.addTest(.{
                .root_module = game_module,
            });
            const run_game_unit_tests = b.addRunArtifact(game_unit_tests);
            steps.unit_test.dependOn(&run_game_unit_tests.step);
        }
        {
            // TODO(zig): delete game_check after solving https://github.com/ziglang/zig/issues/18877
            const game_check = b.addLibrary(.{
                .name = "game",
                .root_module = game_module,
                .linkage = .dynamic,
            });
            steps.check.dependOn(&game_check.step);
        }

        const path_to_lib = b.getInstallPath(install_game_lib.dest_dir.?, install_game_lib.dest_sub_path);
        build_options.addOption(?[]const u8, "game_dynlib_path", path_to_lib);
    } else {
        build_options.addOption(?[]const u8, "game_dynlib_path", null);
    }

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/sdl_platform.zig"),
        .target = options.target,
        .optimize = options.optimize,
    });
    exe_module.linkLibrary(sdl_lib);
    exe_module.addImport("kommon", kommon_module);
    exe_module.addOptions("build_options", build_options);

    const exe = b.addExecutable(.{
        .name = "game",
        .root_module = exe_module,
    });
    steps.install.dependOn(&b.addInstallArtifact(exe, .{}).step);

    if (options.emit_llvm_ir) {
        steps.install.dependOn(&b.addInstallFile(
            exe.getEmittedLlvmIr(),
            "sdl_platform.ir",
        ).step);
    }

    {
        const run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(b.getInstallStep());
        if (b.args) |args| {
            run_cmd.addArgs(args);
        }

        steps.run.dependOn(&run_cmd.step);
    }

    {
        const exe_unit_tests = b.addTest(.{
            .root_module = exe_module,
        });
        const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
        steps.unit_test.dependOn(&run_exe_unit_tests.step);
    }

    {
        // TODO(zig): delete exe_check after solving https://github.com/ziglang/zig/issues/18877
        const exe_check = b.addExecutable(.{
            .name = "sdl_platform",
            .root_module = exe_module,
        });
        steps.check.dependOn(&exe_check.step);
    }
}

fn build_for_web(
    b: *std.Build,
    steps: struct {
        run: *std.Build.Step,
        install: *std.Build.Step,
        check: *std.Build.Step,
        unit_test: *std.Build.Step,
    },
    options: struct {
        target: std.Build.ResolvedTarget,
        optimize: std.builtin.OptimizeMode,
        hot_reloadable: bool,
        emit_llvm_ir: bool,
    },
    kommon_module: *std.Build.Module,
) void {
    _ = steps;
    // TODO: wasm
    if (false) {
        // const target = b.resolveTargetQuery(.{
        //     .cpu_arch = .wasm32,
        //     .os_tag = .freestanding,
        // });
        const wasm_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = b.resolveTargetQuery(.{
                .cpu_arch = .wasm32,
                .os_tag = .freestanding,
            }),
            .optimize = options.optimize,
        });
        wasm_module.addImport("kommon", kommon_module);

        const wasm = b.addExecutable(.{
            .name = "game",
            .root_module = wasm_module,
        });
        b.installArtifact(wasm);
    }
}
