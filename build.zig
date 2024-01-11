const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // create exports for the wasm target
    if (target.result.isWasm()) {
        const lib = b.addSharedLibrary(.{
            .name = "honey",
            .root_source_file = .{ .path = "src/wasm.zig" },
            .target = target,
            .optimize = .ReleaseSmall,
            .version = .{ .major = 0, .minor = 0, .patch = 1 },
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
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("clap", clap.module("clap"));

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    unit_tests.root_module.addImport("clap", clap.module("clap"));

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}
