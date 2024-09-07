const cpp2ast = @import("cpp2ast");
const std = @import("std");

pub fn main() !void {
    const cpp_file_path = "../test-sources/test_basic.h";

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        if (gpa.deinit() != .ok) @panic("leak");
    }

    var translation_unit = try cpp2ast.traversers.createDummyTranslationUnit(
        allocator,
        cpp_file_path,
    );
    defer translation_unit.deinit();
    try translation_unit.printDiags();

    var root_cursor = translation_unit.cursor();

    const ns_childs = try root_cursor.collectChildren(allocator);
    defer ns_childs.deinit();
    std.debug.assert(ns_childs.items.len > 0);

    for (ns_childs.items) |cursor| {
        if (cursor.isInSystemHeader()) continue;
        const spelling = try cursor.spelling(allocator);
        defer allocator.free(spelling);
        std.debug.print("[{s}] {}\n", .{spelling, cursor});
    }
}