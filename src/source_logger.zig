const root = @import("root");

const std = @import("std");
const builtin = @import("builtin");

const DebugInfo = std.debug.SelfInfo;

pub const Options = struct {
    exclude_source_for: []const std.log.Level = &.{},
    strip_dir: []const u8 = "",
};

pub var options: Options = if (@hasDecl(root, "sl_options")) root.sl_options else .{};

fn writeSourceLocation(li: std.debug.SourceLocation, out_stream: anytype) !void {
    var stripped_file_name = li.file_name;
    if (options.strip_dir.len > 0) {
        if (std.mem.indexOf(u8, li.file_name, options.strip_dir)) |_| {
            stripped_file_name = li.file_name[options.strip_dir.len + 1 ..];
        }
    }
    try out_stream.print("[{s}:{d}:{d}] ", .{ stripped_file_name, li.line, li.column });
}

fn writeSourceLocationOfAddress(debug_info: *DebugInfo, address: usize, out_stream: anytype) !void {
    const module = try debug_info.getModuleForAddress(address);
    const symbol_info = try module.getSymbolAtAddress(debug_info.allocator, address);

    defer symbol_info.deinit(debug_info.allocator);

    if (symbol_info.line_info) |li| {
        try writeSourceLocation(li, out_stream);
    }
}

fn writeSourceLocationOfAddress2(debug_info: *DebugInfo, address: usize, out_stream: anytype) !bool {
    const module = try debug_info.getModuleForAddress(address);
    const symbol_info = try module.getSymbolAtAddress(debug_info.allocator, address);

    defer symbol_info.deinit(debug_info.allocator);

    if (symbol_info.line_info) |li| {
        // if file name doesn't contain "std/log.zig" we are porbably at the actual call location.
        if (std.mem.indexOf(u8, li.file_name, "std" ++ std.fs.path.sep_str ++ "log.zig") != null) return false;
        try writeSourceLocation(li, out_stream);
        return true;
    }
    return false;
}

fn isLevelExcluded(level: std.log.Level) bool {
    for (options.exclude_source_for) |opt| {
        if (opt == level) return true;
    }
    return false;
}

pub fn auto(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    const level_txt = comptime message_level.asText();
    const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";
    const stderr = std.io.getStdErr().writer();
    var bw = std.io.bufferedWriter(stderr);
    const writer = bw.writer();

    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    nosuspend {
        if (!isLevelExcluded(message_level)) {
            const debug_info = std.debug.getSelfDebugInfo() catch return;
            var context: std.debug.ThreadContext = undefined;
            const has_context = std.debug.getContext(&context);

            if (builtin.os.tag == .windows) {
                // usually call stack looks like:
                //      std.log.debug(main_call_loc) -> std.log.scoped.debug -> std.log.log -> theLogFn(we're here)
                const HIGHEST_STACK_RET_ADDR = 3;
                var addr_buf: [HIGHEST_STACK_RET_ADDR]usize = undefined;

                const n = std.debug.walkStackWindows(addr_buf[0..], &context);
                const addrs = addr_buf[0..n];
                for (addrs) |addr| {
                    const success = writeSourceLocationOfAddress2(debug_info, addr - 1, writer) catch return;
                    if (success) break;
                }
            } else {
                var it = (if (has_context) blk: {
                    break :blk std.debug.StackIterator.initWithContext(null, debug_info, &context) catch null;
                } else null) orelse std.debug.StackIterator.init(null, null);
                defer it.deinit();

                while (it.next()) |return_address| {
                    const address = if (return_address == 0) return_address else return_address - 1;
                    const success = writeSourceLocationOfAddress2(debug_info, address, writer) catch return;
                    if (success) break;
                }
            }
        }
        writer.print(level_txt ++ prefix2 ++ format ++ "\n", args) catch return;
        bw.flush() catch return;
    }
}

const ReturnAddress = struct {
    usize,
};

pub fn wrapped(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.enum_literal),
    comptime format: []const u8,
    args_with_ret_addr: anytype,
) void {
    const level_txt = comptime message_level.asText();
    const prefix2 = if (scope == .default) ": " else "(" ++ @tagName(scope) ++ "): ";
    const stderr = std.io.getStdErr().writer();
    var bw = std.io.bufferedWriter(stderr);
    const writer = bw.writer();

    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    nosuspend {
        const ArgsType = @TypeOf(args_with_ret_addr);
        const args_type_info = @typeInfo(ArgsType);

        const fields_info = args_type_info.Struct.fields;
        //@compileLog("Fields len", fields_info.len);

        const args = if (fields_info.len == 2 and fields_info[0].type == ReturnAddress and @typeInfo(fields_info[1].type) == .Struct) blk: {
            if (!isLevelExcluded(message_level)) {
                const debug_info = std.debug.getSelfDebugInfo() catch return;
                writeSourceLocationOfAddress(debug_info, args_with_ret_addr[0][0], writer) catch return;
            }

            break :blk args_with_ret_addr[1];
        } else args_with_ret_addr;

        writer.print(level_txt ++ prefix2 ++ format ++ "\n", args) catch return;
        bw.flush() catch return;
    }
}

pub fn scoped(comptime scope: @Type(.enum_literal)) type {
    const std_log_scoped = std.log.scoped(scope);

    if (@hasDecl(root, "std_options") and root.std_options.logFn == wrapped) {
        return struct {
            pub noinline fn err(
                comptime format: []const u8,
                args: anytype,
            ) void {
                std_log_scoped.err(format, .{ ReturnAddress{@returnAddress() - 1}, args });
            }

            pub noinline fn warn(
                comptime format: []const u8,
                args: anytype,
            ) void {
                std_log_scoped.warn(format, .{ ReturnAddress{@returnAddress() - 1}, args });
            }

            pub noinline fn info(
                comptime format: []const u8,
                args: anytype,
            ) void {
                std_log_scoped.info(format, .{ ReturnAddress{@returnAddress() - 1}, args });
            }

            pub noinline fn debug(
                comptime format: []const u8,
                args: anytype,
            ) void {
                std_log_scoped.debug(format, .{ ReturnAddress{@returnAddress() - 1}, args });
            }

            pub noinline fn errRet(
                addr: usize,
                comptime format: []const u8,
                args: anytype,
            ) void {
                std_log_scoped.err(format, .{ ReturnAddress{addr - 1}, args });
            }

            pub noinline fn warnRet(
                addr: usize,
                comptime format: []const u8,
                args: anytype,
            ) void {
                std_log_scoped.warn(format, .{ ReturnAddress{addr - 1}, args });
            }

            pub noinline fn infoRet(
                addr: usize,
                comptime format: []const u8,
                args: anytype,
            ) void {
                std_log_scoped.info(format, .{ ReturnAddress{addr - 1}, args });
            }

            pub noinline fn debugRet(
                addr: usize,
                comptime format: []const u8,
                args: anytype,
            ) void {
                std_log_scoped.debug(format, .{ ReturnAddress{addr - 1}, args });
            }
        };
    } else {
        return std_log_scoped;
    }
}

pub const default_log = scoped(.default);

pub const err = default_log.err;
pub const warn = default_log.warn;
pub const info = default_log.info;
pub const debug = default_log.debug;
