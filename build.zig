const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    // create exports for the wasm target
    if (target.result.isWasm()) {
        const lib = b.addSharedLibrary(.{
            .name = "honey",
            .root_source_file = b.path("src/wasm.zig"),
            .target = target,
            .optimize = .ReleaseSmall,
            .version = .{ .major = 0, .minor = 1, .patch = 0 },
        });
        // used to ensure exports
        lib.rdynamic = true;

        const install = b.addInstallArtifact(lib, .{});
        install.step.dependOn(&lib.step);
        b.default_step.dependOn(&install.step);
        return;
    }

    const clap = b.dependency("clap", .{
        .target = target,
        .optimize = optimize,
    });
    const exe = b.addExecutable(.{
        .name = "honey",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    // exe.use_lld = false;
    // exe.use_llvm = false;
    exe.root_module.addImport("clap", clap.module("clap"));

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // dedicated step for checking if the app compiles (ZLS)
    const exe_check = b.addExecutable(.{
        .name = "check",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_check.root_module.addImport("clap", clap.module("clap"));

    const check = b.step("check", "Check if the executable compiles");
    check.dependOn(&exe_check.step);

    // Step for running unit tests
    const unit_tests = b.addTest(.{
        .root_source_file = b.path("src/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    unit_tests.root_module.addImport("clap", clap.module("clap"));

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    // Step for generating markdown documentation
    const generate_md = b.addExecutable(.{
        .name = "generate-md",
        .root_source_file = b.path("src/compiler/generate_md.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_generate_md = b.addRunArtifact(generate_md);
    const generate_md_step = b.step("generate", "Generates markdown documentation for the compiler");
    generate_md_step.dependOn(&run_generate_md.step);
}
