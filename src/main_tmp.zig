const std = @import("std");
const clang_runtime = @import("clang_runtime");
const proj_info = @import("project_info");
const common = @import("common.zig");

const c = @cImport({
    @cInclude("clang-c/Index.h");
    @cInclude("stdio.h");
});

const BasicString = common.BasicString;
const log = std.log;

const TEMP_FILE_NAME = "generate_temp.h";

fn checkClangRet(ret: c_int) error{ClangError}!void {
    if (ret == 0) return;
    log.err("Error code: {}", .{ret});
    return error.ClangError;
}

fn checkClangRetWithMsg(ret: c_int, comptime msg: []const u8) error{ClangError}!void {
    if (ret == 0) return;
    log.err("{s} Error code: {}", .{ msg, ret });
    return error.ClangError;
}

const Context = struct {
    tu: c.CXTranslationUnit,
    msg: []const u8,
};

fn c_func(
    _: c.CXCursor,
    _: c.CXCursor,
    user_data: ?*anyopaque,
) callconv(.C) c.CXChildVisitResult {
    const data = user_data orelse @panic("Shouldn't receive null context!");
    const context: *Context = @ptrCast(@alignCast(data));

    std.log.debug("MSG: {s}", .{context.msg});

    return c.CXChildVisit_Continue;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        if (gpa.deinit() != .ok) @panic("leak");
    }

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    _ = args.skip();
    const cpp_file_path = args.next() orelse @panic("Need at least a c++ file path");

    const index = c.clang_createIndex(0, 0);
    defer c.clang_disposeIndex(index);

    var argv = std.ArrayList([*:0]const u8).init(allocator);
    defer argv.deinit();
    try argv.append("-isystem");
    try argv.append(clang_runtime.include_dir.ptr);
    try argv.append("-x");
    try argv.append("c++");
    try argv.append("-std=c++20");
    try argv.append("-DWIN32_LEAN_AND_MEAN");
    try argv.append("-fparse-all-comments");

    var temp_file_builder = BasicString.init(allocator);
    defer temp_file_builder.deinit();
    try temp_file_builder.appendFmt("#include \"{s}\"\n", .{cpp_file_path});

    var temp_files = try allocator.alloc(c.CXUnsavedFile, 1);
    defer allocator.free(temp_files);
    temp_files[0] = .{
        .Filename = TEMP_FILE_NAME,
        .Contents = temp_file_builder.c_str(),
        .Length = @intCast(temp_file_builder.len),
    };

    try argv.append(TEMP_FILE_NAME);

    var translation_unit: c.CXTranslationUnit = undefined;
    try checkClangRetWithMsg(c.clang_parseTranslationUnit2(
        index,
        null,
        argv.items.ptr,
        @intCast(argv.items.len),
        temp_files.ptr,
        @intCast(temp_files.len),
        c.CXTranslationUnit_DetailedPreprocessingRecord | c.CXTranslationUnit_SkipFunctionBodies,
        &translation_unit,
    ), "Parsing translation unit failed!");
    defer c.clang_disposeTranslationUnit(translation_unit);

    var err: c_int = 0;
    const num_diagnostics = c.clang_getNumDiagnostics(translation_unit);
    if (num_diagnostics > 0) {
        log.info("Clang says:", .{});
        for (0..num_diagnostics) |i| {
            const diag = c.clang_getDiagnostic(translation_unit, @intCast(i));
            const severity = c.clang_getDiagnosticSeverity(diag);

            if (severity == c.CXDiagnostic_Fatal or severity == c.CXDiagnostic_Error) {
                err = 1;
            }

            const @"struct" = c.clang_formatDiagnostic(diag, c.clang_defaultDiagnosticDisplayOptions());
            std.debug.print("{s}\n", .{c.clang_getCString(@"struct")});
            c.clang_disposeString(@"struct");
        }
    }

    try checkClangRet(err);

    var context: Context = .{
        .tu = translation_unit,
        .msg = "Test msg",
    };

    const cursor = c.clang_getTranslationUnitCursor(translation_unit);
    _ = c.clang_visitChildren(cursor, c_func, @as(*anyopaque, &context));
}
