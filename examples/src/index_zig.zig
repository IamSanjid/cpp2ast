const std = @import("std");
const index_common = @import("index_common.zig");
const cpp2ast = @import("cpp2ast");

const ToZig = index_common.ToZig;
const isCursorInteresting = index_common.isCursorInteresting;
const toZigTypeStr = index_common.toZigTypeStr;
const toCTypeStr = index_common.toCTypeStr;
const interested_cursor_spelling = index_common.interested_cursor_spelling;

const clang = cpp2ast.clang;
const t = cpp2ast.traversers;
const BasicString = cpp2ast.common.BasicString;

pub fn bindings(allocator: std.mem.Allocator, header_path: []const u8) !BasicString {
    // c_cxx_includes module is generated by cpp2ast build.zig and is exposed as zig_c_cxx_includes
    // it's just the absolute path to zig's bundled clang runtime include dirs and libc include dirs.
    const c_cxx_includes = cpp2ast.zig_c_cxx_includes;
    var sys_include_dirs = std.ArrayList([]const u8).init(allocator);
    defer sys_include_dirs.deinit();
    for (c_cxx_includes.libc) |libc| {
        try sys_include_dirs.append(libc);
    }
    try sys_include_dirs.append(c_cxx_includes.clang_rt);
    try sys_include_dirs.append(c_cxx_includes.libcxx);

    const index_header = try std.fs.path.join(allocator, &.{ header_path, "clang-c", "Index.h" });
    defer allocator.free(index_header);

    // `createTranslationUnitDefault` will treat every source as c++ by passing `-x c++` option to libclang,
    // if need to control everything `clang.TranslationUnit.parse` can be used
    var translation_unit = try cpp2ast.traversers.createTranslationUnitDefault(
        allocator,
        &.{index_header},
        &.{header_path},
        sys_include_dirs.items,
    );
    defer translation_unit.deinit();
    try translation_unit.printAndCheckDiags();

    var root_cursor = translation_unit.cursor();

    const root_childs = try root_cursor.collectChildren(allocator);
    defer root_childs.deinit();
    std.debug.assert(root_childs.items.len > 0);

    var gen_builder = BasicString.init(allocator);
    var glue_needed = false;

    for (root_childs.items) |root_child| {
        if (root_child.isInSystemHeader()) continue;

        switch (root_child.kind()) {
            // since it's a c++ source and by default clang-c/Index.h does `extern "C"` stuff
            clang.CursorKind.LinkageSpec => {
                const current_context = .{ .allocator = allocator, .gen_builder = &gen_builder, .glue_needed = &glue_needed };
                const visitor = struct {
                    fn func(context: @TypeOf(current_context), child: clang.Cursor, _: clang.Cursor) !void {
                        switch (child.kind()) {
                            clang.CursorKind.EnumDecl => {
                                try generateEnumBindings(child, context.allocator, context.gen_builder);
                            },
                            clang.CursorKind.FunctionDecl => {
                                try generateFuncBindings(child, context.allocator, context.gen_builder);
                            },
                            clang.CursorKind.StructDecl => {
                                try generateStructBindings(child, context.allocator, context.gen_builder, context.glue_needed);
                            },
                            else => {},
                        }
                    }
                }.func;
                _ = root_child.visit(current_context, visitor);
            },
            else => {},
        }
    }

    if (glue_needed) {
        try gen_builder.append(
            \\pub const c = @cImport({
            \\    @cInclude("clang-c/Index.h");
            \\    @cInclude("stdio.h");
            \\
        );
        try gen_builder.append("    @cDefine(\"" ++ index_common.glue_cast_define ++ "\", {});\n");
        try gen_builder.append("    @cInclude(\"glue.h\");\n");
        try gen_builder.append("    @cUndef(\"" ++ index_common.glue_cast_define ++ "\");\n");
        try gen_builder.append("});\n");
    } else {
        try gen_builder.append(
            \\pub const c = @cImport({
            \\    @cInclude("clang-c/Index.h");
            \\    @cInclude("stdio.h");
            \\});
            \\
        );
    }

    return gen_builder;
}

fn generateEnumBindings(cursor: clang.Cursor, allocator: std.mem.Allocator, gen_builder: *BasicString) !void {
    const c_type = t.cursorToDeclType(cursor);

    const enum_ty = try t.EnumType.fromType(allocator, c_type);
    defer enum_ty.deinit();

    var to_zig_enum: ToZig = undefined;
    if (!isCursorInteresting(enum_ty.cursor, allocator, &to_zig_enum)) return;

    const tag_type_str = try toCTypeStr(enum_ty.cursor.c_type(), allocator);
    defer allocator.free(tag_type_str);

    var used_variant_values: [800]bool = std.mem.zeroes([800]bool);
    var decl_variants = std.ArrayList([]const u8).init(allocator);
    defer {
        for (decl_variants.items) |decl| {
            allocator.free(decl);
        }
        decl_variants.deinit();
    }
    const enum_name = to_zig_enum.name;
    try gen_builder.appendFmt("pub const {s} = enum({s}) {{\n", .{ enum_name, tag_type_str });
    for (enum_ty.variants.items) |variant| {
        const variant_spelling = try variant[0].spelling(allocator);
        defer allocator.free(variant_spelling);
        var variant_name = variant_spelling;
        if (std.mem.indexOf(u8, variant_name, "_")) |pos| {
            variant_name = variant_name[pos + 1 ..];
        }
        if (to_zig_enum.variant_trim_left.len > 0) {
            variant_name = std.mem.trimLeft(u8, variant_name, to_zig_enum.variant_trim_left);
        }
        const variant_name_starts_first = std.mem.startsWith(u8, variant_name, "First");
        const variant_name_starts_last = std.mem.startsWith(u8, variant_name, "Last");
        switch (variant[1].?) {
            .UInt => |v| {
                if (variant_name_starts_first or variant_name_starts_last or used_variant_values[v]) {
                    try decl_variants.append(
                        try std.fmt.allocPrint(allocator, "pub const {s}: {s} = @enumFromInt({});", .{ variant_name, enum_name, v }),
                    );
                    continue;
                }
                used_variant_values[v] = true;
                try gen_builder.appendFmt("    {s} = {},\n", .{ variant_name, v });
            },
            .Int => |v| {
                const v_as_index: usize = @intCast(v);
                if (variant_name_starts_first or variant_name_starts_last or used_variant_values[v_as_index]) {
                    try decl_variants.append(
                        try std.fmt.allocPrint(allocator, "pub const {s}: {s} = @enumFromInt({});", .{ variant_name, enum_name, v }),
                    );
                    continue;
                }
                used_variant_values[v_as_index] = true;
                try gen_builder.appendFmt("    {s} = {},\n", .{ variant_name, v });
            },
            else => unreachable,
        }
    }
    try gen_builder.appendFmt("    pub const TagType = {s};\n", .{tag_type_str});
    for (decl_variants.items) |decl| {
        try gen_builder.appendFmt("    {s}\n", .{decl});
    }
    try gen_builder.append("};\n");
}

fn generateFuncBindings(cursor: clang.Cursor, allocator: std.mem.Allocator, gen_builder: *BasicString) !void {
    const func_info = try t.FunctionInfo.fromCur(cursor);

    var interested = false;
    var is_raw_enum = false;

    if (t.c_typeToCanonTypeDefFallible(func_info.ret_c_type)) |cur| {
        is_raw_enum = cur.c_type().kind() == .Enum;
        interested = isCursorInteresting(cur, allocator, null);
    }
    const args = try func_info.args(allocator);
    defer args.deinit();

    if (!interested) {
        for (args.items) |arg| {
            // will fail for pointer types, but that's ok for `clang-c/Index.h` since it's "enum"
            if (t.c_typeToCanonTypeDefFallible(arg.c_type())) |cur| {
                if (isCursorInteresting(cur, allocator, null)) {
                    is_raw_enum = cur.c_type().kind() == .Enum;
                    interested = true;
                    break;
                }
            }
        }
    }

    if (!interested) return;

    if (is_raw_enum) {
        // if our generated glue struct needs to be passed as pointer or value, it will fail...
        return generateEnumFuncBindings(func_info, args.items, allocator, gen_builder);
    } else {
        // TODO: possiblities...
    }
}

fn generateStructBindings(cursor: clang.Cursor, allocator: std.mem.Allocator, gen_builder: *BasicString, glue_needed: *bool) !void {
    const struct_info = t.CompoundType.fromType(allocator, t.cursorToDeclType(cursor), cursor) catch return;
    defer struct_info.deinit();

    const c_struct_name = try struct_info.cursor.spelling(allocator);
    defer allocator.free(c_struct_name);

    var to_zig: ToZig = undefined;
    if (!isCursorInteresting(struct_info.cursor, allocator, &to_zig)) return;
    const c_type = struct_info.cursor.c_type();

    try gen_builder.appendFmt("pub const {s} = struct{{\n", .{to_zig.name});
    // TODO: Should be using `TranslationUnit.typeSize` and `TranslationUnit.typeAlign`
    try gen_builder.appendFmt("    pub const Size: usize = {};\n", .{c_type.c_size()});
    try gen_builder.appendFmt("    pub const Align: usize = {};\n", .{c_type.c_align()});
    try gen_builder.append("    const Native = extern struct { __opaque: [Size]u8 align(Align) = @import(\"std\").mem.zeroes([Size]u8) };\n");

    var glue_func_builder = BasicString.init(allocator);
    defer glue_func_builder.deinit();

    var zig_fields_builder = BasicString.init(allocator);
    defer zig_fields_builder.deinit();

    var zig_native_builder = BasicString.init(allocator);
    defer zig_native_builder.deinit();

    var size_field_name: ?[]const u8 = null;
    if (to_zig.glue) {
        glue_needed.* = true;
        for (struct_info.fields.items) |field| {
            const field_name = try field.spelling(allocator);
            defer allocator.free(field_name);
            if (field_name.len == 0) continue;
            var interesting_zig_field = false;
            if (std.ascii.eqlIgnoreCase(field_name, "size")) {
                size_field_name = try allocator.dupe(u8, field_name);
            } else {
                interesting_zig_field = true;
            }
            var field_type_str: ?[]const u8 = null;
            var instance_str: []const u8 = undefined;
            if (field.isBitField()) {
                if (field.bitWidth()) |width| {
                    if (width == 1) {
                        try glue_func_builder.appendFmt("    inline fn set_{s}(x: *Native, value: bool) void {{\n", .{field_name});
                        try glue_func_builder.appendFmt(
                            "        return c.{s}_set_{s}(@as(*anyopaque, x), @intFromBool(value));\n",
                            .{ c_struct_name, field_name },
                        );
                        field_type_str = try allocator.dupe(u8, "bool");
                        instance_str = try allocator.dupe(u8, "false");
                    } else {
                        try glue_func_builder.appendFmt("    inline fn set_{s}(x: *Native, value: u{}) void {{\n", .{ field_name, width });
                        try glue_func_builder.appendFmt(
                            "        return c.{s}_set_{s}(@as(*anyopaque, x), @intCast(value));\n",
                            .{ c_struct_name, field_name },
                        );
                        field_type_str = try std.fmt.allocPrint(allocator, "u{}", .{width});
                        instance_str = try allocator.dupe(u8, "0");
                    }
                    try glue_func_builder.append("    }\n");
                }
            } else {
                var wrap: ?ToZig = null; // ignored..
                const field_c_type = t.cursorToDeclType(field);
                const field_type_tag = t.TypeTag.fromType(field_c_type);
                const type_str = try toZigTypeStr(field_c_type, allocator, &wrap);
                defer allocator.free(type_str);

                field_type_str = try allocator.dupe(u8, type_str);
                if (field_type_tag.isInteger()) {
                    instance_str = try allocator.dupe(u8, "0");
                } else if (field_type_tag.isReal()) {
                    instance_str = try allocator.dupe(u8, "0.0");
                } else {
                    instance_str = try std.fmt.allocPrint(allocator, "@import(\"std\").mem.zeroes({s})", .{type_str});
                }

                try glue_func_builder.appendFmt("    inline fn set_{s}(x: *Native, value: {s}) void {{\n", .{ field_name, type_str });
                try glue_func_builder.appendFmt(
                    "        return c.{s}_set_{s}(@as(*anyopaque, x), value);\n",
                    .{ c_struct_name, field_name },
                );
                try glue_func_builder.append("    }\n");
            }
            if (field_type_str) |type_str| {
                defer allocator.free(type_str);
                defer allocator.free(instance_str);
                if (interesting_zig_field) {
                    try zig_fields_builder.appendFmt("    {s}: {s} = {s},\n", .{ field_name, type_str, instance_str });
                    try zig_native_builder.appendFmt("        set_{s}(&x, self.{s});\n", .{ field_name, field_name });
                }
            }
        }
    } else {
        @panic("TODO");
    }
    try gen_builder.append(zig_fields_builder.str());
    try gen_builder.append(
        \\    pub fn native(self: @This()) Native {
        \\        var x = Native{};
        \\
        ,
    );
    if (size_field_name) |f_name| {
        defer allocator.free(f_name);
        try gen_builder.appendFmt("        set_{s}(&x, @This().Size);\n", .{f_name});
    }
    try gen_builder.append(zig_native_builder.str());
    try gen_builder.append(
        \\        return x;
        \\    }
        \\
    );
    try gen_builder.append(glue_func_builder.str());
    try gen_builder.append("};\n");
}

fn generateEnumFuncBindings(func_info: t.FunctionInfo, args: []const clang.Cursor, allocator: std.mem.Allocator, gen_builder: *BasicString) !void {
    var wrap_ret: ?ToZig = null;
    const return_str = try toZigTypeStr(func_info.ret_c_type, allocator, &wrap_ret);
    defer allocator.free(return_str);

    const func_name = try func_info.name(allocator);
    defer allocator.free(func_name);

    var forward_builder = BasicString.init(allocator);
    defer forward_builder.deinit();

    try gen_builder.appendFmt("pub inline fn {s}(", .{func_name});
    for (args, 0..) |arg, i| {
        var arg_name = try arg.spelling(allocator);
        defer allocator.free(arg_name);
        if (arg_name.len == 0) {
            allocator.free(arg_name);
            arg_name = try std.fmt.allocPrint(allocator, "arg{}", .{i});
        }
        var wrap_type: ?ToZig = null;
        const type_str = try toZigTypeStr(arg.c_type(), allocator, &wrap_type);
        defer allocator.free(type_str);
        if (i > 0) {
            try gen_builder.appendFmt(", {s}: {s}", .{ arg_name, type_str });
            if (wrap_type) |_| {
                try forward_builder.appendFmt(", @intFromEnum({s})", .{arg_name});
            } else {
                try forward_builder.appendFmt(", {s}", .{arg_name});
            }
        } else {
            try gen_builder.appendFmt("{s}: {s}", .{ arg_name, type_str });
            if (wrap_type) |_| {
                try forward_builder.appendFmt("@intFromEnum({s})", .{arg_name});
            } else {
                try forward_builder.appendFmt("{s}", .{arg_name});
            }
        }
    }
    try gen_builder.appendFmt(") {s} {{\n", .{return_str});
    if (wrap_ret) |_| {
        try gen_builder.appendFmt("    return @enumFromInt(c.{s}({s}));\n", .{ func_name, forward_builder.str() });
    } else {
        try gen_builder.appendFmt("    return c.{s}({s});\n", .{ func_name, forward_builder.str() });
    }
    try gen_builder.append("}\n");
}
