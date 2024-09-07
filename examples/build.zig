const std = @import("std");

fn getProjectDir(b: *std.Build) [:0]const u8 {
    const path = b.build_root.path orelse
        std.fs.cwd().realpathAlloc(b.allocator, ".") catch
        @panic("Can't alloc space!");
    return b.allocator.dupeZ(u8, path) catch @panic("Can't alloc space!");
}

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "examples",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    // If you're linking libclang statically(libclang might be "static" but still
    // need to link with windows's msvc runtime), it might not work the intended way,
    // your final executable's linkage might need to be set as `.dynamic`.
    // https://github.com/ziglang/zig/issues/21292#issuecomment-2326787384
    if (target.result.os.tag == .windows and target.result.abi == .msvc)
        exe.linkage = .dynamic;

    const project_info_options = b.addOptions();
    exe.root_module.addOptions("project_info", project_info_options);
    project_info_options.addOption([:0]const u8, "project_dir", getProjectDir(b));

    const cpp2ast_dep = @import("cpp2ast").dependency(b, .{ .target = target, .optimize = optimize }, null);
    exe.root_module.addImport("cpp2ast", cpp2ast_dep.module("cpp2ast"));

    b.installArtifact(exe);
    const run_cmd_step = b.addRunArtifact(exe);
    run_cmd_step.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd_step.addArgs(args);
    }
    const run_step = b.step("run", "Runs it!");
    run_step.dependOn(&run_cmd_step.step);
}
