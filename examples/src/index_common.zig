const std = @import("std");
const cpp2ast = @import("cpp2ast");
const clang = cpp2ast.clang;
const t = cpp2ast.traversers;

pub const glue_cast_define = "GLUE_CAST";

pub const ToZig = struct {
    name: []const u8,
    variant_trim_left: []const u8 = "",
    glue: bool = false,
};

pub const interested_cursor_spelling = std.StaticStringMap(ToZig).initComptime(&.{
    .{ "CXCursorKind", .{ .name = "CursorKind" } },
    .{ "CXTypeKind", .{ .name = "TypeKind" } },
    .{ "CX_CXXAccessSpecifier", .{ .name = "CXXAccessSpecifier", .variant_trim_left = "CX" } },
    .{ "CXIndexOptions", .{ .name = "IndexOptions", .glue = true } },
});

pub const isCursorInteresting = struct {
    fn func(cur: clang.Cursor, space_creator: std.mem.Allocator, to_enum_zig: ?*ToZig) bool {
        const cur_spelling = cur.spelling(space_creator) catch return false;
        defer space_creator.free(cur_spelling);
        if (interested_cursor_spelling.get(cur_spelling)) |tez| {
            if (to_enum_zig) |ret| {
                ret.* = tez;
            }
            return true;
        }
        return false;
    }
}.func;

pub fn CNumberToZigTypeStr(c_type: clang.Type, type_tag: t.TypeTag) []const u8 {
    return switch (c_type.kind()) {
        .SChar, .Char_S, .UChar, .Char_U => "u8",
        else => switch (type_tag) {
            .UInt => "c_uint",
            .Int => "c_int",
            // it could be f16 but for simplicity...
            // TODO: support precise type according to size, eg. introduce Float32/Float64/Float16
            // or tagged enum with size...
            .Float => if (c_type.c_size() > 32) "f32" else "f64",
            else => unreachable,
        },
    };
}

// includes `c.` prefix for non primitive types
pub const toZigTypeStr = struct {
    fn func(c_type: clang.Type, space_creator: std.mem.Allocator, to_zig: *?ToZig) ![]const u8 {
        to_zig.* = null;
        const tag_type = t.TypeTag.fromType(c_type);
        return switch (tag_type) {
            .UInt, .Int, .Float => space_creator.dupe(u8, CNumberToZigTypeStr(c_type, tag_type)),
            .Pointer, .Array => blk: {
                const child_c_type = c_type.pointeeType() orelse c_type.elemType() orelse unreachable;
                const child_type_str = try @This().func(child_c_type, space_creator, to_zig);
                defer space_creator.free(child_type_str);
                if (c_type.isConst()) {
                    break :blk std.fmt.allocPrint(space_creator, "[*c]const {s}", .{child_type_str});
                } else {
                    break :blk std.fmt.allocPrint(space_creator, "[*c]{s}", .{child_type_str});
                }
            },
            else => blk: {
                // arena allocator is probably best for such cases...
                if (t.c_typeToCanonTypeDefFallible(c_type)) |cur| {
                    var tez: ToZig = undefined;
                    if (isCursorInteresting(cur, space_creator, &tez)) {
                        to_zig.* = tez;
                        break :blk space_creator.dupe(u8, tez.name);
                    }
                }
                const type_spelling = @constCast(try c_type.spelling(space_creator));
                defer space_creator.free(type_spelling);
                std.mem.replaceScalar(u8, type_spelling, ' ', '_');
                break :blk std.fmt.allocPrint(space_creator, "c.{s}", .{type_spelling});
            },
        };
    }
}.func;

// includes `c.` prefix for non primitive types
pub const toCTypeStr = struct {
    fn func(c_type: clang.Type, space_creator: std.mem.Allocator) ![]const u8 {
        const tag_type = t.TypeTag.fromType(c_type);
        return switch (tag_type) {
            .UInt, .Int, .Float => space_creator.dupe(u8, CNumberToZigTypeStr(c_type, tag_type)),
            .Pointer, .Array => blk: {
                const child_c_type = c_type.pointeeType() orelse c_type.elemType() orelse unreachable;
                const child_type_str = try @This().func(child_c_type, space_creator);
                defer space_creator.free(child_type_str);
                if (c_type.isConst()) {
                    break :blk std.fmt.allocPrint(space_creator, "[*c]const {s}", .{child_type_str});
                } else {
                    break :blk std.fmt.allocPrint(space_creator, "[*c]{s}", .{child_type_str});
                }
            },
            else => blk: {
                // arena allocator is probably best for such cases...
                const type_spelling = @constCast(try c_type.spelling(space_creator));
                defer space_creator.free(type_spelling);
                std.mem.replaceScalar(u8, type_spelling, ' ', '_');
                break :blk std.fmt.allocPrint(space_creator, "c.{s}", .{type_spelling});
            },
        };
    }
}.func;
