const std = @import("std");
const cpp2ast = @import("cpp2ast");
const index_zig = @import("index_zig.zig");
const index_glue = @import("index_glue.zig");

fn default(allocator: std.mem.Allocator) !void {
    const cpp_file_path = "../test-sources/test_basic.h";

    var translation_unit = try cpp2ast.traversers.createDummyTranslationUnit(
        allocator,
        cpp_file_path,
    );
    defer translation_unit.deinit();
    try translation_unit.printAndCheckDiags();

    var root_cursor = translation_unit.cursor();

    const ns_childs = try root_cursor.collectChildren(allocator);
    defer ns_childs.deinit();
    std.debug.assert(ns_childs.items.len > 0);

    for (ns_childs.items) |cursor| {
        if (cursor.isInSystemHeader()) continue;
        const spelling = try cursor.spelling(allocator);
        defer allocator.free(spelling);
        std.debug.print("[{s}] kind: {}\n", .{ spelling, cursor.kind() });
    }
}

fn writeBytesToExeDir(file_name: []const u8, bytes: []const u8) !void {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const out_dir_path = try std.fs.selfExeDirPath(&path_buf);
    var out_dir = try std.fs.openDirAbsolute(out_dir_path, .{});
    defer out_dir.close();

    var file = try out_dir.createFile(file_name, .{});
    defer file.close();
    try file.writeAll(bytes);

    std.debug.print("Saved as {s}\n", .{try out_dir.realpath(file_name, &path_buf)});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        if (gpa.deinit() != .ok) @panic("leak");
    }
    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();
    if (!args.skip()) @panic("Should receive the process name...");

    if (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "index_zig")) {
            const inc_header_path = args.next() orelse @panic("Need header include path!");

            const basic_str = try index_zig.bindings(allocator, inc_header_path);
            defer basic_str.deinit();

            const save_file_name = "index.zig";
            try writeBytesToExeDir(save_file_name, basic_str.str());

            return;
        }
        if (std.mem.eql(u8, arg, "index_glue")) {
            const inc_header_path = args.next() orelse @panic("Need header include path!");

            const header = try index_glue.generate_header(allocator, inc_header_path);
            defer header.deinit();

            const header_name = "glue.h";
            try writeBytesToExeDir(header_name, header.str());

            const source = try index_glue.generate_source(allocator, inc_header_path, header_name);
            defer source.deinit();

            const source_name = "glue.c";
            try writeBytesToExeDir(source_name, source.str());

            return;
        }
    }
    try default(allocator);
}
