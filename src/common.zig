const std = @import("std");

pub const BasicString = struct {
    buffer: std.ArrayList(u8),
    len: usize,

    const Self = @This();

    pub fn init(init_allocator: std.mem.Allocator) Self {
        return .{
            .buffer = std.ArrayList(u8).init(init_allocator),
            .len = 0,
        };
    }

    pub fn append(self: *Self, value: []const u8) !void {
        if (value.len == 0) return;
        if (self.buffer.getLastOrNull()) |v| {
            if (v == 0) _ = self.buffer.pop();
        }
        try self.buffer.appendSlice(value);
        self.len = self.buffer.items.len;
    }

    pub fn appendFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const newBuf = try std.fmt.allocPrint(self.buffer.allocator, fmt, args);
        defer self.buffer.allocator.free(newBuf);
        try self.append(newBuf);
    }

    pub fn prepend(self: *Self, value: []const u8) !void {
        if (self.buffer.getLastOrNull()) |v| {
            if (v == 0) _ = self.buffer.pop();
        }
        try self.buffer.insertSlice(0, value);
        self.len = self.buffer.items.len;
    }

    pub fn prependFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const newBuf = try std.fmt.allocPrint(self.buffer.allocator, fmt, args);
        defer self.buffer.allocator.free(newBuf);
        try self.prepend(newBuf);
    }

    pub fn replace(self: *Self, needle: []const u8, replacement: []const u8) !void {
        const needle_len: isize = @intCast(needle.len);
        const replacement_len: isize = @intCast(replacement.len);
        const extra_size = @abs(needle_len - replacement_len);
        try self.buffer.ensureTotalCapacity(self.buffer.items.len + extra_size);
        _ = std.mem.replace(u8, self.buffer.items, needle, replacement, self.buffer.items[0..]);
    }

    pub fn str(self: Self) []const u8 {
        if (self.len == 0) return "";
        return self.buffer.items[0..self.len];
    }

    pub fn c_str(self: *Self) [:0]const u8 {
        if (self.buffer.items[self.len - 1] != 0) {
            self.buffer.append(0) catch @panic("Couldn't append extra 0.");
        }
        // slice from 0th to our len without extra 0, :0 says we will find a 0
        // after the `self.len`th item.
        return self.buffer.items[0..self.len :0];
    }

    pub fn contains(self: Self, substr: []const u8) bool {
        return std.mem.indexOf(u8, self.buffer.items, substr) != null;
    }

    pub fn allocator(self: Self) std.mem.Allocator {
        return self.buffer.allocator;
    }

    pub fn deinit(self: Self) void {
        self.buffer.deinit();
    }
};
