const std = @import("std");
const builtin = @import("builtin");
const ext = @import("zig-buildext");
const fluent = ext.fluent;

const PACKAGE_NAME = "cpp2ast";
// it's fine for most libclang-c functions till clang-17~16
const EXPECTED_LLVM_VERSION = std.SemanticVersion{ .major = 18, .minor = 1, .patch = 7 };

pub const DependencyOptions = struct {
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    dependency_name: []const u8 = PACKAGE_NAME,
    options_prefix: []const u8 = PACKAGE_NAME ++ "-",
};

// check examples/build.zig
pub fn dependency(b: *std.Build, options: DependencyOptions, overrides: anytype) *std.Build.Dependency {
    var build_options = BuildOptionsType.parse(b, options.options_prefix, .{});
    build_options.target = options.target;
    build_options.optimize = options.optimize;

    const OverridesTy = @TypeOf(overrides);
    const overrides_ty_info = @typeInfo(OverridesTy);
    if (overrides_ty_info != .null) {
        inline for (overrides_ty_info.@"struct".fields) |field| {
            const field_name = ext.comptimeScalarReplace(field.name, '-', '_');
            if (@hasField(@TypeOf(build_options), field_name)) {
                @field(build_options, field_name) = @field(overrides, field.name);
            }
        }
    }

    installLibClangDlls(b, build_options);

    return b.dependency(options.dependency_name, BuildOptionsType.toDependencyArgs(build_options));
}

const BuildOptionsType = ext.BuildOptions(.{
    .target = .{ std.Build.ResolvedTarget, "", undefined },
    .optimize = .{ std.builtin.OptimizeMode, "", undefined },
    // usually pre-built liblang static libs requires `stdc++` to be linked
    .use_zig_libcxx = .{ bool, "If libc++ is needed, use zig's bundled version, don't try to integrate with the system.", false },
    .read_symlink = .{ bool, "If a symlink is encountered when searching for files/a file, read through it instead of skipping.", false },
    .llvm_config_path = .{ []const u8, "Will use `llvm-config` to get the libs, bin and include dirs.", "" },
    // everything will be linked inside this dir
    .libclang_libs_dir = .{ []const u8, "Will search for libclang libs in this dir.", "" },
    .libclang_bin_dir = .{ []const u8, "On windows, bin dir to get the DLLs.", "" },
    .libclang_inc_dir = .{ []const u8, "User provided clang-c include header dir.", "" },
});

pub fn build(b: *std.Build) !void {
    const build_context = try BuildContext.fromBuild(b);

    try buildModule(build_context);
    try buildTests(build_context);
    try buildExamples(build_context);
}

fn buildModule(build_context: BuildContext) !void {
    const b = build_context.b;
    const lib = b.addStaticLibrary(.{
        .name = PACKAGE_NAME,
        .root_source_file = b.path("src/lib.zig"),
        .target = build_context.options.target,
        .optimize = build_context.options.optimize,
    });
    try linkLibClangToCompileStep(lib, build_context);
    b.installArtifact(lib);
    installLibClangDlls(b, build_context.options);

    const module = b.addModule(PACKAGE_NAME, .{
        .root_source_file = b.path("src/lib.zig"),
        .target = build_context.options.target,
        .optimize = build_context.options.optimize,
    });
    try linkLibClangToModule(module, build_context);
}

fn buildTests(build_context: BuildContext) !void {
    const b = build_context.b;
    const test_step = b.step("test", "Run all the avilable tests.");

    const test_comp = b.addTest(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = build_context.options.target,
        .optimize = build_context.options.optimize,
    });
    try linkLibClangToCompileStep(test_comp, build_context);

    const run_tests = b.addRunArtifact(test_comp);
    test_step.dependOn(&run_tests.step);
}

fn buildExamples(build_context: BuildContext) !void {
    const b = build_context.b;

    const examples_build_cmd = b.addSystemCommand(&.{ "zig", "build", "run" });
    examples_build_cmd.setCwd(b.path("examples"));

    examples_build_cmd.step.dependOn(&b.addRemoveDirTree(b.path("examples/zig-out")).step);
    examples_build_cmd.step.dependOn(&b.addRemoveDirTree(b.path("examples/.zig-cache")).step);

    for (b.available_options_list.items) |avail_option| {
        if (b.user_input_options.get(avail_option.name)) |option| {
            switch (option.value) {
                .flag => examples_build_cmd.addArg(b.fmt("-D" ++ PACKAGE_NAME ++ "-{s}", .{option.name})),
                .scalar => |v| {
                    if (std.mem.eql(u8, option.name, "target") or std.mem.eql(u8, option.name, "optimize")) {
                        examples_build_cmd.addArg(b.fmt("-D{s}={s}", .{ option.name, v }));
                        continue;
                    }
                    examples_build_cmd.addArg(b.fmt("-D" ++ PACKAGE_NAME ++ "-{s}={s}", .{ option.name, v }));
                },
                else => {},
            }
        }
    }

    if (b.verbose) {
        examples_build_cmd.addArg("--verbose");
    }

    if (b.args) |args| {
        examples_build_cmd.addArg("--");
        examples_build_cmd.addArgs(args);
    }

    const example_run_step = b.step("examples", "Builds and runs the examples executable. eg. zig build examples -- <specific example>");
    example_run_step.dependOn(&examples_build_cmd.step);
}

fn installLibClangDlls(b: *std.Build, build_options: BuildOptions) void {
    if (build_options.target.result.os.tag == .windows) {
        var native_paths = ext.getNativePaths(b.allocator, build_options.target.result) catch {
            return;
        };
        if (build_options.libclang_bin_dir.len != 0) {
            native_paths.addLibDir(build_options.libclang_bin_dir) catch @panic("OOM");
        }
        if (build_options.libclang_libs_dir.len != 0) {
            native_paths.addLibDir(build_options.libclang_libs_dir) catch @panic("OOM");
        }

        if (build_options.llvm_config_path.len > 0) {
            var code: u8 = undefined;
            if (b.runAllowFail(
                &.{ build_options.llvm_config_path, "--bindir" },
                &code,
                .Inherit,
            )) |out| {
                const bindir = std.mem.trim(u8, out, "\n");
                native_paths.addLibDir(bindir) catch @panic("OOM");
            } else |_| {}
        }
        ext.installDlls(b, &libclang_dlls, native_paths.lib_dirs.items, true) catch {};
    }
}

fn absolutePathToLazyPath(path: []const u8) ?std.Build.LazyPath {
    if (std.mem.eql(u8, path, "")) return null;
    return std.Build.LazyPath{ .cwd_relative = path };
}

const BuildOptions = BuildOptionsType.Type;

fn parseLLVMConfigPaths(b: *std.Build, options: *BuildOptions) !void {
    if (options.llvm_config_path.len == 0) return;
    var code: u8 = undefined;
    if (options.libclang_libs_dir.len == 0) {
        options.libclang_libs_dir = b.dupe(std.mem.trim(u8, try b.runAllowFail(
            &.{ options.llvm_config_path, "--libdir" },
            &code,
            .Inherit,
        ), "\n"));
    }
    if (options.libclang_inc_dir.len == 0) {
        options.libclang_inc_dir = b.dupe(std.mem.trim(u8, try b.runAllowFail(
            &.{ options.llvm_config_path, "--includedir" },
            &code,
            .Inherit,
        ), "\n"));
    }
    if (options.target.result.os.tag == .windows) {
        options.libclang_bin_dir = b.dupe(std.mem.trim(u8, try b.runAllowFail(
            &.{ options.llvm_config_path, "--bindir" },
            &code,
            .Inherit,
        ), "\n"));
    }
}

const BuildContext = struct {
    b: *std.Build,
    native_paths: std.zig.system.NativePaths,
    options: BuildOptions,

    const Self = @This();
    fn fromBuild(b: *std.Build) !Self {
        var options = BuildOptionsType.parse(b, "", .{
            .target = b.standardTargetOptions(.{}),
            .optimize = b.standardOptimizeOption(.{}),
        });
        parseLLVMConfigPaths(b, &options) catch |err| {
            std.log.warn("llvm-config returned error: {}", .{err});
        };
        const native_paths = try ext.getNativePaths(b.allocator, options.target.result);
        return Self{
            .b = b,
            .native_paths = native_paths,
            .options = options,
        };
    }

    fn getClangIncludeDir(self: Self) ?std.Build.LazyPath {
        if (absolutePathToLazyPath(self.options.libclang_inc_dir)) |inc_path| {
            std.debug.assert(inc_path == .cwd_relative);
            if (ext.hasFileIn(inc_path.cwd_relative, "clang-c/Index.h")) {
                return inc_path;
            }
        }
        const dep_name = self.b.fmt("clang-{}-headers", .{EXPECTED_LLVM_VERSION});
        if (self.b.lazyDependency(dep_name, .{})) |dep| {
            return dep.path("include");
        }
        return null;
    }
};

const CCXXIncludes = struct {
    libcxx: [:0]const u8,
    clang_rt: [:0]const u8,
    libc: []const [:0]const u8,

    const Self = @This();
    fn fromBuild(b: *std.Build, target: std.Target) !Self {
        const zig_lib_dir = b.graph.zig_lib_directory.handle;
        const zig_lib_dir_path = try zig_lib_dir.realpathAlloc(b.allocator, ".");
        const libc_dirs = try std.zig.LibCDirs.detect(b.allocator, zig_lib_dir_path, target, true, true, null);
        var libc_inc_dirs = std.ArrayList([:0]const u8).init(b.allocator);
        for (libc_dirs.libc_include_dir_list) |dir| {
            libc_inc_dirs.append(b.allocator.dupeZ(u8, dir) catch @panic("OOM")) catch @panic("OOM");
        }
        return Self{
            .libcxx = try b.allocator.dupeZ(
                u8,
                try zig_lib_dir.realpathAlloc(b.allocator, "libcxx/include"),
            ),
            .clang_rt = try b.allocator.dupeZ(
                u8,
                try zig_lib_dir.realpathAlloc(b.allocator, "include"),
            ),
            .libc = libc_inc_dirs.items,
        };
    }
};

fn addCCXXIncludes(module: *std.Build.Module) !void {
    const b = module.owner;
    const runtime_includes_options = b.addOptions();
    module.addOptions("c_cxx_includes", runtime_includes_options);

    const rt_includes = try CCXXIncludes.fromBuild(b, module.resolved_target.?.result);
    runtime_includes_options.addOption([:0]const u8, "clang_rt", rt_includes.clang_rt);
    runtime_includes_options.addOption([:0]const u8, "libcxx", rt_includes.libcxx);
    runtime_includes_options.addOption([]const [:0]const u8, "libc", rt_includes.libc);
}

const LinkingLibClangInfo = struct {
    version: ?std.SemanticVersion = null,
    guessed_linkage: std.builtin.LinkMode = .static,
    path: std.Build.LazyPath,
    is_found_in_system: bool = false,

    const Self = @This();

    fn parseVersion(str: []const u8) ?std.SemanticVersion {
        var tokens = std.mem.tokenizeScalar(u8, str, '.');
        while (tokens.peek()) |buff| {
            if (buff.len == 0 or std.mem.allEqual(u8, buff, ' ')) {
                _ = tokens.next();
            } else {
                break;
            }
        }
        const major = std.fmt.parseInt(usize, tokens.next() orelse return null, 10) catch return null;
        const minor = if (tokens.next()) |minor| std.fmt.parseInt(usize, minor, 10) catch return null else 0;
        const patch = if (tokens.next()) |patch| std.fmt.parseInt(usize, patch, 10) catch return null else 0;
        return std.SemanticVersion{ .major = major, .minor = minor, .patch = patch };
    }

    const MatchOrder = struct {
        optional: bool = false,
        op: enum {
            skip,
            extension,
            version,
            major,
            minor,
            patch,
        },
    };

    const extensions = std.StaticStringMap(std.builtin.LinkMode).initComptime(&.{
        .{ "so", .dynamic },
        .{ "dylib", .dynamic },
        .{ "tbd", .dynamic },
        .{ "a", .static },
        .{ "lib", .static },
        .{ "dll.a", .static },
    });

    fn match(
        comptime pattern: []const u8,
        str: []const u8,
        orders: []const MatchOrder,
    ) ?struct { std.builtin.LinkMode, ?std.SemanticVersion } {
        var linkage: std.builtin.LinkMode = .static;
        var version: ?std.SemanticVersion = null;

        var matches = fluent.match(pattern, str);
        var next_entry: @TypeOf(matches.next()) = null;
        for (orders) |order| {
            // current fluent doesn't have .peek yet..
            const entry = next_entry orelse matches.next() orelse {
                if (order.optional) {
                    continue;
                }
                return null;
            };
            const sucess = blk: {
                switch (order.op) {
                    .skip => break :blk true,
                    .extension => {
                        const exten = if (entry.items[0] == '.') entry.items[1..] else entry.items;
                        if (extensions.get(exten)) |ext_linkage| {
                            linkage = ext_linkage;
                            break :blk true;
                        }
                    },
                    .version => {
                        version = parseVersion(entry.items) orelse break :blk false;
                        break :blk true;
                    },
                    .major => {
                        const major = entry.cast(usize) catch break :blk false;
                        if (version) |*ver| {
                            ver.major = major;
                        } else {
                            version = std.SemanticVersion{ .major = major, .minor = 0, .patch = 0 };
                        }
                        break :blk true;
                    },
                    .minor => {
                        const minor = entry.cast(usize) catch break :blk false;
                        if (version) |*ver| {
                            ver.minor = minor;
                        } else {
                            version = std.SemanticVersion{ .major = 0, .minor = minor, .patch = 0 };
                        }
                        break :blk true;
                    },
                    .patch => {
                        const patch = entry.cast(usize) catch break :blk false;
                        if (version) |*ver| {
                            ver.patch = patch;
                        } else {
                            version = std.SemanticVersion{ .major = 0, .minor = 0, .patch = patch };
                        }
                        break :blk true;
                    },
                }
                break :blk false;
            };
            if (sucess) {
                next_entry = null;
                continue;
            }
            if (order.optional) {
                next_entry = entry;
            } else {
                return null;
            }
        }

        return .{ linkage, version };
    }

    fn getLibClangInfoRaw(allocator: std.mem.Allocator, files: *std.fs.Dir.Iterator) ?Self {
        var buf: [std.fs.max_path_bytes]u8 = undefined;

        while (files.next() catch return null) |file| {
            if (file.kind != .file and file.kind != .sym_link) {
                continue;
            }
            const extensions_pattern = "(\\.so|\\.a|\\.dylib|\\.tbd|\\.lib|.\\.dll.a)";
            var matches = fluent.match("clang(" ++ extensions_pattern ++ "|\\.(\\d+)|\\.|-\\d+).*", file.name);
            if (matches.next()) |entry| {
                if (Self.match(
                    extensions_pattern ++ "|(\\d+)",
                    entry.items,
                    &.{
                        .{ .op = .major, .optional = true },
                        .{ .op = .extension },
                        .{ .op = .major, .optional = true },
                        .{ .op = .minor, .optional = true },
                        .{ .op = .patch, .optional = true },
                    },
                )) |info| {
                    var path: ?[]const u8 = null;
                    if (file.kind == .sym_link) {
                        const tmp_file = files.dir.realpath(file.name, &buf) catch continue;
                        // testing if it's a file?
                        var file_h = std.fs.openFileAbsolute(tmp_file, .{}) catch continue;
                        file_h.close();

                        path = allocator.dupe(u8, tmp_file) catch @panic("OOM");
                    }
                    return Self{
                        .version = info[1],
                        .guessed_linkage = info[0],
                        .path = .{
                            .cwd_relative = path orelse files.dir.realpathAlloc(allocator, file.name) catch return null,
                        },
                    };
                }
            }
        }
        return null;
    }

    fn getLibClangInfo(b: *std.Build, lib_path: std.Build.LazyPath, last_libclang_info: *?Self) bool {
        var dir = lib_path.getPath3(b, null).openDir("", .{
            .iterate = true,
            .no_follow = true,
        }) catch return false;
        defer dir.close();
        var files = dir.iterate();
        while (getLibClangInfoRaw(b.allocator, &files)) |info| {
            if (last_libclang_info.* == null) {
                last_libclang_info.* = info;
            }
            if (last_libclang_info.*.?.version) |ver| {
                if (info.version) |new_ver| {
                    if (new_ver.order(ver) == .gt) {
                        last_libclang_info.* = info;
                    }
                }
            } else {
                last_libclang_info.* = info;
            }
        }
        return last_libclang_info.* != null;
    }

    fn from(module: *std.Build.Module, native_paths: std.zig.system.NativePaths) ?Self {
        const b = module.owner;

        var last_libclang_info: ?Self = null;

        for (module.lib_paths.items) |lib_path| {
            if (getLibClangInfo(b, lib_path, &last_libclang_info)) {
                return last_libclang_info;
            }
        }

        if (module.lib_paths.items.len > 0) {
            std.log.warn("Couldn't find libclang in the provided libs directory(or collected from user-provided `llvm-config`), searching in system paths...", .{});
        }

        for (native_paths.lib_dirs.items) |lib_dir| {
            const resolved_lib_dir = std.fs.path.resolve(b.allocator, &.{lib_dir}) catch return null;
            const lib_path = std.Build.LazyPath{ .cwd_relative = resolved_lib_dir };
            if (getLibClangInfo(b, lib_path, &last_libclang_info)) {
                break;
            }
        }

        for (native_paths.rpaths.items) |lib_dir| {
            const resolved_lib_dir = std.fs.path.resolve(b.allocator, &.{lib_dir}) catch return null;
            const lib_path = std.Build.LazyPath{ .cwd_relative = resolved_lib_dir };
            if (getLibClangInfo(b, lib_path, &last_libclang_info)) {
                break;
            }
        }

        if (last_libclang_info) |*info| {
            info.*.is_found_in_system = true;
        }
        return last_libclang_info;
    }
};

fn linkLibClangToModule(module: *std.Build.Module, build_context: BuildContext) !void {
    const b = module.owner;
    const target = build_context.options.target;

    try addCCXXIncludes(module);

    const exclude_files: ?[]const []const u8 = &.{
        "libMLIRMlirOptMain.a",
        "libbolt_rt_hugify.a",
        "libbolt_rt_instr.a",
        "LLVMPolly.so",
    };

    module.link_libc = true;

    var static_libclang = false;
    if (absolutePathToLazyPath(build_context.options.libclang_libs_dir)) |lib_path| {
        module.addLibraryPath(lib_path);
    }

    if (build_context.getClangIncludeDir()) |dir| {
        module.addIncludePath(dir);
    }
    module.addIncludePath(b.path("src/clang-c/"));
    module.addCSourceFile(.{ .file = b.path("src/clang-c/glue.c") });

    if (target.result.os.tag.isDarwin()) {
        // adding some homebrew and macport lib paths, we are adding these before trying to find libclang.so
        module.addLibraryPath(.{ .cwd_relative = "/usr/local/lib" });
        module.addLibraryPath(.{ .cwd_relative = "/opt/homebrew/lib" });
        module.addLibraryPath(.{ .cwd_relative = "/opt/local/lib" });
    }

    const native_paths = build_context.native_paths;
    if (LinkingLibClangInfo.from(module, native_paths)) |libclang_info| {
        static_libclang = libclang_info.guessed_linkage != .dynamic;
        if (libclang_info.version) |ver| {
            if (!static_libclang) {
                if (ver.order(EXPECTED_LLVM_VERSION) == .lt) {
                    std.log.warn(
                        "The libclang library is older({}) than the expected version({}).",
                        .{ ver, EXPECTED_LLVM_VERSION },
                    );
                }
            }
        }
        if (libclang_info.is_found_in_system and !static_libclang) {
            module.addObjectFile(libclang_info.path);
        } else {
            try ext.linkAllLibsOf(
                module,
                libclang_info.path.dirname(),
                static_libclang,
                build_context.options.read_symlink,
                exclude_files,
            );
        }
    } else {
        module.linkSystemLibrary("clang", .{});
    }

    if (!build_context.options.use_zig_libcxx) {
        const libstdcxx_dyn_names: []const []const u8 = &.{
            "libstdc++.so",
            "libc++.so",
            "libgcc_eh.so",
            "libc++abi.so",
        };
        const libstdcxx_static_names: []const []const u8 = &.{
            "libstdc++.a",
            "libc++.a",
            "libgcc_eh.a",
            "libc++abi.a",
        };
        var found_atleast_one = false;
        for (libstdcxx_dyn_names) |libstdcxx_name| {
            const libstdcxx_path = ext.getObjSystemPath(native_paths, libstdcxx_name) catch continue;
            module.addObjectFile(.{ .cwd_relative = libstdcxx_path });
            found_atleast_one = true;
        }
        // just try everything and finger crossed...
        if (!found_atleast_one) {
            for (libstdcxx_static_names) |libstdcxx_name| {
                const libstdcxx_path = ext.getObjSystemPath(native_paths, libstdcxx_name) catch continue;
                module.addObjectFile(.{ .cwd_relative = libstdcxx_path });
                found_atleast_one = true;
            }
        }

        if (found_atleast_one) {
            if (target.result.os.tag == .linux) {
                module.linkSystemLibrary("unwind", .{});
            }
        } else {
            module.link_libcpp = true;
        }
    } else {
        switch (target.result.os.tag) {
            .windows => {
                if (target.result.abi != .msvc) {
                    module.link_libcpp = true;
                }
            },
            else => module.link_libcpp = true,
        }
    }

    if (target.result.os.tag == .windows) {
        if (target.result.abi == .msvc) {
            // `getNativePaths` will actually find <Visual Studio Place>/Tools/LLVM/*
            if (build_context.options.libclang_libs_dir.len == 0) {
                for (native_paths.lib_dirs.items) |lib_dir| {
                    module.addLibraryPath(.{ .cwd_relative = lib_dir });
                }
            }
            module.link_libcpp = false;
        }
        module.linkSystemLibrary("Winmm", .{});
        module.linkSystemLibrary("Version", .{});
        module.linkSystemLibrary("Ws2_32", .{});
        module.linkSystemLibrary("advapi32", .{});
        module.linkSystemLibrary("uuid", .{});
        module.linkSystemLibrary("ole32", .{});
    }
}

fn linkLibClangToCompileStep(exe: *std.Build.Step.Compile, build_context: BuildContext) !void {
    const target = build_context.options.target;
    try linkLibClangToModule(&exe.root_module, build_context);
    if (target.result.os.tag == .windows) {
        if (target.result.abi == .gnu) {
            // https://github.com/ziglang/zig/blob/28383d4d985cd04c897f6b6a63bd2107d8e2a8e9/build.zig#L216
            exe.want_lto = false;
        }

        // If you're linking libclang statically(libclang might be "static" but still
        // need to link with windows's msvc runtime), it might not work the intended way,
        // your final executable's linkage might need to be set as `.dynamic`.
        // https://github.com/ziglang/zig/issues/21292#issuecomment-2326787384
        if (target.result.abi == .msvc) {
            exe.linkage = .dynamic;
        }
    }
}

const libclang_dlls = .{
    "libclang.dll",
    "LLVM-C.dll",
};
