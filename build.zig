// On two terminals:
// zig build run
// zig build --watch -Dhot-reloadable=only_lib

const active_folder = "chesstory";

const std = @import("std");

pub fn build(b: *std.Build) !void {
    // A compile error stack trace of 10 is arbitrary in size but helps with debugging.
    b.reference_trace = 10;

    // To use in other projects
    _ = b.addModule("kommon", .{
        .root_source_file = b.path("monorepo/kommon/kommon.zig"),
        // .target = target,
        // .optimize = optimize,
    });

    if (b.option(bool, "ghpages", "Build all games, for ghpages") orelse false) {
        try build_all_games_html(b);
    } else {
        try build_game(b, active_folder);
    }

    // TODO(eternal): delete this step by getting a build.zig for msdf
    const fonts_step = b.step("fonts", "Compile the fonts (only available on win64)");
    // TODO: make msdf an optional dependency
    const msdf = b.dependency("msdf", .{});

    const wf = b.addUpdateSourceFiles();
    // wf.addCopyFileToSource(msdf.path("msdf-atlas-gen.exe"), "ungit/msdf-atlas-gen.exe");
    inline for (&.{ "Arial", "Bokor" }) |font_name| {
        const run_msdf = std.Build.Step.Run.create(b, "run_msdf");
        run_msdf.addFileArg(msdf.path("msdf-atlas-gen.exe"));
        // TODO: -chars [0x20, 0x7e],ñ does not work, open github issue
        run_msdf.addArgs(&.{ "-type", "msdf", "-size", "32", "-yorigin", "top", "-outerpxpadding", "2", "-charset", "monorepo/tools/font_chars.txt" });
        run_msdf.addArg("-font");
        run_msdf.addFileArg(b.path("assets/fonts/" ++ font_name ++ ".ttf"));
        run_msdf.addArg("-json");
        const font_json = run_msdf.addOutputFileArg(font_name ++ ".json");
        run_msdf.addArg("-imageout");
        const font_atlas = run_msdf.addOutputFileArg(font_name ++ ".png");
        wf.addCopyFileToSource(font_json, "assets/fonts/" ++ font_name ++ ".json");
        wf.addCopyFileToSource(font_atlas, "assets/fonts/" ++ font_name ++ ".png");
    }
    fonts_step.dependOn(&wf.step);
    // TODO
    // const asdf = run_msdf.captureStdErr();
    // const expected_stderr =
    //     \\Atlas image file saved.
    //     \\Glyph layout and metadata written into JSON file.
    //     \\
    // ;
    // std.debug.assert(std.mem.eql(u8, expected_stderr, asdf.))
    // wf.addCopyFileToSource(asdf, "monorepo/fonts/" ++ font_name ++ ".txt");
}

const HotReloadableMode = enum {
    no,
    full,
    /// only compile the library part, useful for watch mode
    only_lib,
};

fn build_game(b: *std.Build, comptime game_folder: []const u8) !void {
    const optimize = b.standardOptimizeOption(.{});

    const hot_reloadable: HotReloadableMode = b.option(
        HotReloadableMode,
        "hot-reloadable",
        "Compile the game logic as a separate dynamic library.",
    ) orelse if (optimize == .Debug) .full else .no;

    const emit_llvm_ir = b.option(bool, "emit-llvm-ir", "Emit LLVM IR") orelse false;

    const web = b.option(bool, "web", "Target the web") orelse false;

    const install_step = b.getInstallStep();
    const run_step = b.step("run", "Run the app");
    const test_step = b.step("test", "Run unit tests");
    const check_step = b.step("check", "Check if the project compiles");

    if (web) {
        build_for_web(b, game_folder, .{
            .install = install_step,
            .run = run_step,
            .unit_test = test_step,
            .check = check_step,
        }, .{
            .target = b.resolveTargetQuery(.{
                .cpu_arch = .wasm32,
                .os_tag = .freestanding,
            }),
            .optimize = optimize,
            .hot_reloadable = hot_reloadable,
            .emit_llvm_ir = emit_llvm_ir,
        });
    } else {
        try build_for_desktop(b, game_folder, .{
            .install = install_step,
            .run = run_step,
            .unit_test = test_step,
            .check = check_step,
        }, .{
            .target = b.standardTargetOptions(.{}),
            .optimize = optimize,
            .hot_reloadable = hot_reloadable,
            .emit_llvm_ir = emit_llvm_ir,
        });
    }
}

fn build_for_desktop(
    b: *std.Build,
    comptime game_folder: []const u8,
    steps: struct {
        install: *std.Build.Step,
        run: *std.Build.Step,
        unit_test: *std.Build.Step,
        check: *std.Build.Step,
    },
    options: struct {
        target: std.Build.ResolvedTarget,
        optimize: std.builtin.OptimizeMode,
        hot_reloadable: HotReloadableMode,
        emit_llvm_ir: bool,
    },
) !void {
    const sdl_lib = b.dependency("sdl", .{
        .target = options.target,
        .optimize = options.optimize,
    }).artifact("SDL3");

    const kommon_module = b.addModule("kommon", .{
        .root_source_file = b.path("monorepo/kommon/kommon.zig"),
        .target = options.target,
        .optimize = options.optimize,
    });

    // TODO: remove 'game_module' and use this instead
    const game_module_asdf = b.createModule(.{
        .root_source_file = b.path("games/" ++ game_folder ++ "/GameState.zig"),
    });
    game_module_asdf.addImport("kommon", kommon_module);
    // TODO: better
    game_module_asdf.addAnonymousImport("assets/fonts/Bokor.json", .{ .root_source_file = b.path("assets/fonts/Bokor.json") });
    game_module_asdf.addAnonymousImport("assets/fonts/Arial.json", .{ .root_source_file = b.path("assets/fonts/Arial.json") });

    const build_options = b.addOptions();
    if (options.hot_reloadable != .no) {
        game_module_asdf.pic = true;
        game_module_asdf.resolved_target = options.target;
        game_module_asdf.optimize = options.optimize;

        // const game_module = b.createModule(.{
        //     .root_source_file = b.path("monorepo/game.zig"),
        //     .target = options.target,
        //     .optimize = options.optimize,
        //     .pic = true,
        // });
        // game_module.addImport("kommon", kommon_module);
        // game_module.addImport("GameState", game_module_asdf);
        const game_lib = b.addLibrary(.{
            .name = "game",
            .root_module = game_module_asdf,
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
                .root_module = game_module_asdf,
            });
            const run_game_unit_tests = b.addRunArtifact(game_unit_tests);
            steps.unit_test.dependOn(&run_game_unit_tests.step);
        }
        {
            // TODO(zig): delete game_check after solving https://github.com/ziglang/zig/issues/18877
            const game_check = b.addLibrary(.{
                .name = "game",
                .root_module = game_module_asdf,
                .linkage = .dynamic,
            });
            steps.check.dependOn(&game_check.step);
        }

        const path_to_lib = b.getInstallPath(install_game_lib.dest_dir.?, install_game_lib.dest_sub_path);
        build_options.addOption(?[]const u8, "game_dynlib_path", path_to_lib);
    } else {
        build_options.addOption(?[]const u8, "game_dynlib_path", null);
    }

    if (options.hot_reloadable == .only_lib) return;

    const exe_module = b.createModule(.{
        .root_source_file = b.path("monorepo/sdl_platform.zig"),
        .target = options.target,
        .optimize = options.optimize,
    });
    exe_module.linkLibrary(sdl_lib);
    exe_module.addImport("zstbi", b.dependency("zstbi", .{}).module("root"));
    exe_module.addImport("kommon", kommon_module);
    exe_module.addImport("GameState", game_module_asdf);

    // TODO: less hacky in general
    // TODO: only the actually used assets
    {
        var dir = try std.fs.cwd().openDir("assets", .{ .iterate = true });
        defer dir.close();

        var walk = try dir.walk(b.allocator);
        defer walk.deinit();

        while (try walk.next()) |entry| {
            // TODO: remove this particular
            if (!std.mem.eql(u8, "alchemy", game_folder) and
                std.mem.indexOf(u8, entry.path, "alchemy") != null) continue;

            if (entry.kind == .file) {
                // const corrected_path = try b.allocator.dupe(u8, entry.path);
                const corrected_path = b.pathJoin(&.{ "assets", entry.path });
                defer b.allocator.free(corrected_path);
                for (corrected_path) |*byte| {
                    switch (byte.*) {
                        '\\' => byte.* = '/',
                        else => {},
                    }
                }
                exe_module.addAnonymousImport(corrected_path, .{ .root_source_file = b.path(corrected_path) });
            }
        }
    }

    exe_module.addOptions("build_options", build_options);

    // The closest version to WebGL2
    exe_module.addImport("gl", @import("zigglgen").generateBindingsModule(b, .{
        .api = .gl,
        .version = .@"3.3",
        .profile = .core,
    }));

    const exe = b.addExecutable(.{
        .name = game_folder,
        .root_module = exe_module,
    });
    // Without this, the SDL game will open a console window
    if (options.target.result.os.tag == .windows and options.optimize != .Debug) exe.subsystem = .Windows;
    steps.install.dependOn(&b.addInstallArtifact(exe, .{}).step);

    if (options.emit_llvm_ir) {
        steps.install.dependOn(&b.addInstallFile(
            exe.getEmittedLlvmIr(),
            "sdl_platform.ir",
        ).step);
    }

    {
        const run_cmd = b.addRunArtifact(exe);
        run_cmd.step.dependOn(steps.install);
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

// inspiration from https://github.com/daneelsan/minimal-zig-wasm-canvas/blob/master/build.zig
fn build_for_web(
    b: *std.Build,
    comptime game_folder: []const u8,
    steps: struct {
        install: *std.Build.Step,
        run: *std.Build.Step,
        unit_test: *std.Build.Step,
        check: *std.Build.Step,
    },
    options: struct {
        target: std.Build.ResolvedTarget,
        optimize: std.builtin.OptimizeMode,
        hot_reloadable: HotReloadableMode,
        emit_llvm_ir: bool,
    },
) void {
    const web_install_dir = std.Build.InstallDir{ .custom = "web_static" };
    _build_for_web(b, game_folder, web_install_dir, steps, options);
}

fn _build_for_web(
    b: *std.Build,
    comptime game_folder: []const u8,
    web_install_dir: std.Build.InstallDir,
    // TODO
    steps: anytype,
    // TODO
    options: anytype,
) void {
    const kommon_module = b.addModule("kommon", .{
        .root_source_file = b.path("monorepo/kommon/kommon.zig"),
        .target = options.target,
        .optimize = options.optimize,
    });

    const game_module = b.createModule(.{
        .root_source_file = b.path("games/" ++ game_folder ++ "/GameState.zig"),
    });
    game_module.addImport("kommon", kommon_module);
    // TODO: better
    game_module.addAnonymousImport("assets/fonts/Bokor.json", .{ .root_source_file = b.path("assets/fonts/Bokor.json") });
    game_module.addAnonymousImport("assets/fonts/Arial.json", .{ .root_source_file = b.path("assets/fonts/Arial.json") });

    const wasm_module = b.createModule(.{
        .root_source_file = b.path("monorepo/web_platform.zig"),
        .target = options.target,
        .optimize = options.optimize,
        // TODO(zig): uncomment this line when zig's backend works
        // .use_llvm = options.optimize != .Debug,
    });
    wasm_module.addImport("kommon", kommon_module);
    wasm_module.addImport("GameState", game_module);

    const build_options = b.addOptions();
    build_options.addOption(bool, "hot_reloadable", options.hot_reloadable != .no);
    wasm_module.addOptions("build_options", build_options);

    const wasm = b.addExecutable(.{
        .name = "main",
        .root_module = wasm_module,
    });

    {
        wasm.entry = .disabled;
        wasm.import_memory = true;
        wasm.stack_size = std.wasm.page_size;
        // without this, the .js can't see the exported symbols
        wasm.rdynamic = true;
    }

    steps.install.dependOn(&b.addInstallArtifact(wasm, .{
        .dest_dir = .{ .override = web_install_dir },
    }).step);

    if (options.emit_llvm_ir) {
        steps.install.dependOn(&b.addInstallFile(
            wasm.getEmittedLlvmIr(),
            "web_platform.ir",
        ).step);
    }

    const copy_static_files = b.addInstallDirectory(.{
        .install_dir = web_install_dir,
        .install_subdir = "",
        .source_dir = b.path("static"),
    });
    steps.install.dependOn(&copy_static_files.step);

    // TODO: only copy the actually used assets
    {
        const copy_sound_files = b.addInstallDirectory(.{
            .install_dir = web_install_dir,
            .install_subdir = "assets",
            .source_dir = b.path("assets"),
        });
        steps.install.dependOn(&copy_sound_files.step);
    }

    const generate_keycodes = b.addExecutable(.{
        .name = "generate_keycodes",
        .root_source_file = b.path("monorepo/tools/generate_keycodes_js.zig"),
        .target = b.graph.host,
    });
    generate_keycodes.root_module.addImport("kommon", kommon_module);
    const generate_keycodes_step = b.addRunArtifact(generate_keycodes);
    steps.install.dependOn(&b.addInstallFileWithDir(
        generate_keycodes_step.addOutputFileArg("keycodes.js"),
        web_install_dir,
        "keycodes.js",
    ).step);

    {
        // dev server for testing the webgame, with WebSockets + hot reloading
        // TODO(eternal): remove this step if zig gets a fs.watch equivalent
        const run_dev_server_cmd = b.addSystemCommand(&.{"bun"});
        run_dev_server_cmd.addFileArg(b.path("monorepo/tools/dev_server.js"));
        run_dev_server_cmd.addArg(b.getInstallPath(web_install_dir, ""));
        run_dev_server_cmd.step.dependOn(steps.install);
        steps.run.dependOn(&run_dev_server_cmd.step);
    }

    // TODO(future): get unit tests working for the web version
    // {
    //     const wasm_unit_tests = b.addTest(.{
    //         .root_module = wasm_module,
    //     });
    //     const run_wasm_unit_tests = b.addRunArtifact(wasm_unit_tests);
    //     steps.unit_test.dependOn(&run_wasm_unit_tests.step);
    // }

    {
        // TODO(zig): delete wasm_check after solving https://github.com/ziglang/zig/issues/18877
        const wasm_check = b.addExecutable(.{
            .name = "wasm_platform",
            .root_module = wasm_module,
        });
        steps.check.dependOn(&wasm_check.step);
    }
}

fn build_all_games_html(b: *std.Build) !void {
    const optimize = b.standardOptimizeOption(.{});

    const install_step = b.getInstallStep();
    const run_step = b.step("run", "Run the app");
    const test_step = b.step("test", "Run unit tests");
    const check_step = b.step("check", "Check if the project compiles");

    inline for (.{
        "akari",
        "alchemy",
        "hexditor",
        "octopus",
        "snakanake",
        "tres_undos",
    }) |game_folder| {
        _build_for_web(
            b,
            game_folder,
            std.Build.InstallDir{ .custom = "web_static/" ++ game_folder },
            .{
                .install = install_step,
                .run = run_step,
                .unit_test = test_step,
                .check = check_step,
            },
            .{
                .target = b.resolveTargetQuery(.{
                    .cpu_arch = .wasm32,
                    .os_tag = .freestanding,
                }),
                .optimize = optimize,
                .hot_reloadable = .no,
                .emit_llvm_ir = false,
            },
        );
    }
}
