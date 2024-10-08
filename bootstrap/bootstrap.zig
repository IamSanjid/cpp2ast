// To generate glue.c/glue.h/index.zig

const std = @import("std");
const index_zig = @import("index_zig.zig");
const index_glue = @import("index_glue.zig");

fn writeBytesTo(out_path: []const u8, bytes: []const u8) !void {
    var file = try std.fs.createFileAbsolute(out_path, .{});
    defer file.close();
    try file.writeAll(bytes);
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
        if (std.mem.eql(u8, arg, "index.zig")) {
            const inc_header_path = args.next() orelse @panic("Need header include path!");
            const target_arch_os_abi = if (args.next()) |str| blk: {
                const query = std.zig.parseTargetQueryOrReportFatalError(allocator, .{
                    .arch_os_abi = str,
                });
                break :blk std.zig.resolveTargetQueryOrFatal(query);
            } else @panic("Need target zig triple!");
            const out_path = args.next() orelse @panic("Need output path");

            var src = try index_zig.bindings(allocator, inc_header_path, target_arch_os_abi);
            defer src.deinit();
            try src.prepend("// Auto generated by `bootstrap index.zig <path/to/clang-c/Index.h>`\n");

            try writeBytesTo(out_path, src.str());
            return;
        }

        const header_name = "glue.h";
        // const source_name = "glue.c";
        if (std.mem.eql(u8, arg, "glue.h")) {
            const inc_header_path = args.next() orelse @panic("Need header include path!");
            const target_arch_os_abi = if (args.next()) |str| blk: {
                const query = std.zig.parseTargetQueryOrReportFatalError(allocator, .{
                    .arch_os_abi = str,
                });
                break :blk std.zig.resolveTargetQueryOrFatal(query);
            } else @panic("Need target zig triple!");
            const out_path = args.next() orelse @panic("Need output path");

            var header = try index_glue.generate_header(allocator, inc_header_path, target_arch_os_abi);
            defer header.deinit();
            try header.prepend("// Auto generated by `bootstrap glue.h <path/to/clang-c/Index.h>`\n");

            try writeBytesTo(out_path, header.str());
            return;
        }
        if (std.mem.eql(u8, arg, "glue.c")) {
            const inc_header_path = args.next() orelse @panic("Need header include path!");
            const target_arch_os_abi = if (args.next()) |str| blk: {
                const query = std.zig.parseTargetQueryOrReportFatalError(allocator, .{
                    .arch_os_abi = str,
                });
                break :blk std.zig.resolveTargetQueryOrFatal(query);
            } else @panic("Need target zig triple!");
            const out_path = args.next() orelse @panic("Need output path");

            var source = try index_glue.generate_source(allocator, inc_header_path, header_name, target_arch_os_abi);
            defer source.deinit();
            try source.prepend("// Auto generated by `bootstrap glue.c <path/to/clang-c/Index.h>`\n");

            try writeBytesTo(out_path, source.str());
            return;
        }
    }
    @panic("Bootstrap shouldn't be called/ran like this way.");
}
