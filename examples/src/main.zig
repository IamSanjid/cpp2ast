const std = @import("std");
const cpp2ast = @import("cpp2ast");
const index_enum_to_zig = @import("index_enum_to_zig.zig");

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
        if (std.mem.eql(u8, arg, "index_enum_to_zig")) {
            const inc_header_path = args.next() orelse @panic("Need header include path!");
            
            const basic_str = try index_enum_to_zig.bindings(allocator, inc_header_path);
            defer basic_str.deinit();

            const out_dir_path = try std.fs.selfExeDirPathAlloc(allocator);
            defer allocator.free(out_dir_path);
            var out_dir = try std.fs.openDirAbsolute(out_dir_path, .{});
            defer out_dir.close();
            
            const save_file_name = "index.zig";
            var file = try out_dir.createFile(save_file_name, .{});
            defer file.close();
            try file.writeAll(basic_str.str());
            const save_file_path = try out_dir.realpathAlloc(allocator, save_file_name);
            defer allocator.free(save_file_path);
            
            std.debug.print("Saved as {s}\n", .{save_file_path});
            return;
        }
    }
    try default(allocator);
}
