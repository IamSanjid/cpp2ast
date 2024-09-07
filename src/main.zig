const std = @import("std");
const clang_runtime = @import("clang_runtime");
const proj_info = @import("project_info");
const sl = @import("source_logger.zig");

const c = @cImport({
    @cInclude("clang-c/Index.h");
    @cInclude("stdio.h");
});

pub const std_options = .{
    .log_level = .debug,
    .logFn = sl.wrapped,
};

pub const sl_options = .{
    .strip_dir = proj_info.project_dir,
};

const log = sl.default_log;

const TEMP_FILE_NAME = "generate_temp.h";

const BasicString = struct {
    buffer: std.ArrayList(u8),
    len: usize,

    const Self = @This();

    fn init(allocator: std.mem.Allocator) Self {
        return .{
            .buffer = std.ArrayList(u8).init(allocator),
            .len = 0,
        };
    }

    fn append(self: *Self, value: []const u8) !void {
        if (value.len == 0) return;
        if (self.buffer.getLastOrNull()) |v| {
            if (v == 0) _ = self.buffer.pop();
        }
        try self.buffer.appendSlice(value);
        self.len = self.buffer.items.len;
    }

    fn appendFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const newBuf = try std.fmt.allocPrint(self.buffer.allocator, fmt, args);
        defer self.buffer.allocator.free(newBuf);
        try self.append(newBuf);
    }

    fn prepend(self: *Self, value: []const u8) !void {
        if (self.buffer.getLastOrNull()) |v| {
            if (v == 0) _ = self.buffer.pop();
        }
        try self.buffer.insertSlice(0, value);
        self.len = self.buffer.items.len;
    }

    fn prependFmt(self: *Self, comptime fmt: []const u8, args: anytype) !void {
        const newBuf = try std.fmt.allocPrint(self.buffer.allocator, fmt, args);
        defer self.buffer.allocator.free(newBuf);
        try self.prepend(newBuf);
    }

    fn replace(self: *Self, needle: []const u8, replacement: []const u8) !void {
        const needle_len: isize = @intCast(needle.len);
        const replacement_len: isize = @intCast(replacement.len);
        const extra_size = @abs(needle_len - replacement_len);
        try self.buffer.ensureTotalCapacity(self.buffer.items.len + extra_size);
        _ = std.mem.replace(u8, self.buffer.items, needle, replacement, self.buffer.items[0..]);
    }

    fn str(self: Self) []const u8 {
        if (self.len == 0) return "";
        return self.buffer.items[0..self.len];
    }

    fn c_str(self: *Self) [:0]const u8 {
        if (self.buffer.items[self.len - 1] != 0) {
            self.buffer.append(0) catch @panic("Couldn't append extra 0.");
        }
        // slice from 0th to our len without extra 0, :0 says we will find a 0
        // after the `self.len`th item.
        return self.buffer.items[0..self.len :0];
    }

    fn contains(self: Self, substr: []const u8) bool {
        return std.mem.indexOf(u8, self.buffer.items, substr) != null;
    }

    fn deinit(self: *Self) void {
        self.buffer.deinit();
    }
};

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

fn CursorMap(comptime T: type, comptime create_fn: anytype) type {
    return struct {
        map: InnerMap,

        const InnerMap = std.AutoHashMap(c_uint, T);
        const Self = @This();

        fn init(allocator: std.mem.Allocator) Self {
            return .{
                .map = InnerMap.init(allocator),
            };
        }

        fn putFetchWithTU(self: *Self, curr: c.CXCursor, tu: c.CXTranslationUnit) !T {
            const instance = try @call(.always_inline, create_fn, .{ self.map.allocator, curr, tu });
            try self.map.put(c.clang_hashCursor(curr), instance);
            return instance;
        }

        fn putFetch(self: *Self, curr: c.CXCursor) !T {
            const instance = try @call(.always_inline, create_fn, .{ self.map.allocator, curr });
            try self.map.put(c.clang_hashCursor(curr), instance);
            return instance;
        }

        fn put(self: *Self, curr: c.CXCursor, value: T) !void {
            try self.map.put(c.clang_hashCursor(curr), value);
        }

        fn get(self: Self, curr: c.CXCursor) ?T {
            return self.map.get(c.clang_hashCursor(curr));
        }

        fn contains(self: Self, curr: c.CXCursor) bool {
            return self.map.contains(c.clang_hashCursor(curr));
        }

        fn iterator(self: *const Self) InnerMap.Iterator {
            return self.map.iterator();
        }

        fn deinit(self: *Self) void {
            var classes_iterator = self.map.iterator();
            const type_info = @typeInfo(T);

            // T.destroy must accept the allocator.
            if (type_info == .Pointer) {
                while (classes_iterator.next()) |entry| {
                    entry.value_ptr.*.destroy(self.map.allocator);
                }
            } else if (@hasDecl(T, "destroy")) {
                while (classes_iterator.next()) |entry| {
                    entry.value_ptr.*.destroy(self.map.allocator);
                }
            } else if (@hasDecl(T, "deinit")) {
                while (classes_iterator.next()) |entry| {
                    entry.value_ptr.*.deinit();
                }
            } else {
                @compileError("Couldn't find 'deinit' or 'destroy' for type " ++ @typeName(T));
            }
            self.map.deinit();
        }
    };
}

const Context = struct {
    allocator: std.mem.Allocator,
    translation_unit: c.CXTranslationUnit,
    tree: CursorList,
    classes: VisitedClassMap,
    vars: VarList,
    enums: EnumMap,
    exposes_as_is: CursorList,
    class_tree: ClassList,
    working_enum: *EnumInfo = undefined, // TODO: Find better way of doing it?

    const Self = @This();
    const VisitedClassMap = CursorMap(*ClassInfo, ClassInfo.create);
    const ClassList = std.ArrayList(*ClassInfo);
    const VarList = std.ArrayList(*VarInfo);
    const EnumMap = CursorMap(*EnumInfo, EnumInfo.create_debug);

    fn create(allocator: std.mem.Allocator, translation_unit: c.CXTranslationUnit) !*Self {
        var instance = try allocator.create(Self);

        instance.allocator = allocator;
        instance.translation_unit = translation_unit;
        instance.tree = CursorList.init(allocator);
        instance.classes = VisitedClassMap.init(allocator);
        instance.vars = VarList.init(allocator);
        instance.enums = EnumMap.init(allocator);
        instance.exposes_as_is = CursorList.init(allocator);
        instance.class_tree = ClassList.init(allocator);

        return instance;
    }

    fn init(allocator: std.mem.Allocator, translation_unit: c.CXTranslationUnit) Self {
        return .{
            .allocator = allocator,
            .translation_unit = translation_unit,
            .tree = CursorList.init(allocator),
            .classes = VisitedClassMap.init(allocator),
            .vars = VarList.init(allocator),
            .enums = EnumMap.init(allocator),
            .exposes_as_is = CursorList.init(allocator),
            .class_tree = ClassList.init(allocator),
        };
    }

    fn deinit(self: *Self) void {
        self.tree.deinit();
        for (0..self.vars.items.len) |i| {
            var @"var" = self.vars.items[i];
            @"var".destroy(self.allocator);
        }
        self.classes.deinit();
        self.vars.deinit();
        self.enums.deinit();
        self.exposes_as_is.deinit();
        self.class_tree.deinit();
    }

    fn destroy(self: *Self) void {
        self.deinit();
        self.allocator.destroy(self);
    }
};

const ReverseCursorIterator = struct {
    current: c.CXCursor,
    reached_end: bool = false,

    fn init(current: c.CXCursor) @This() {
        return .{ .current = current };
    }

    fn next(self: *@This()) ?c.CXCursor {
        if (self.reached_end) return null;

        const parent = c.clang_getCursorSemanticParent(self.current);
        const parent_kind = c.clang_getCursorKind(parent);

        if (parent_kind == c.CXCursor_TranslationUnit) {
            self.reached_end = true;
            return null;
        }

        self.current = parent;
        return self.current;
    }

    fn nextTillTU(self: *@This()) ?c.CXCursor {
        if (self.reached_end) return null;

        const parent = c.clang_getCursorSemanticParent(self.current);
        const parent_kind = c.clang_getCursorKind(parent);

        self.reached_end = parent_kind == c.CXCursor_TranslationUnit;

        self.current = parent;
        return self.current;
    }
};

fn getCursorNamespaceWitSep(
    allocator: std.mem.Allocator,
    cursor: c.CXCursor,
    sep: []const u8,
) !BasicString {
    var namespace_builder = BasicString.init(allocator);
    var reverse_iter = ReverseCursorIterator.init(cursor);
    var is_first = true;
    while (reverse_iter.next()) |parent| {
        const parent_spelling = c.clang_getCursorSpelling(parent);
        defer c.clang_disposeString(parent_spelling);

        if (!is_first) {
            try namespace_builder.prependFmt("{s}{s}", .{ c.clang_getCString(parent_spelling), sep });
        } else {
            try namespace_builder.prependFmt("{s}", .{c.clang_getCString(parent_spelling)});
            is_first = false;
        }
    }

    return namespace_builder;
}

fn getCursorFullNamespaceWithSep(
    allocator: std.mem.Allocator,
    cursor: c.CXCursor,
    sep: []const u8,
) !BasicString {
    var namespace_builder = try getCursorNamespaceWitSep(allocator, cursor, sep);

    const spelling = c.clang_getCursorSpelling(cursor);
    defer c.clang_disposeString(spelling);
    if (namespace_builder.len > 0) {
        try namespace_builder.appendFmt("{s}{s}", .{ sep, c.clang_getCString(spelling) });
    } else {
        try namespace_builder.appendFmt("{s}", .{c.clang_getCString(spelling)});
    }

    return namespace_builder;
}

fn getCursorNamespace(allocator: std.mem.Allocator, cursor: c.CXCursor) !BasicString {
    return getCursorNamespaceWitSep(allocator, cursor, "_");
}

const CursorList = std.ArrayList(c.CXCursor);
const ClassInfo = struct {
    cursor: c.CXCursor,
    // Debug
    size: usize,
    namespace: BasicString,
    spelling: []const u8,
    // ======
    expose_cursors: CursorList, // methods, public fields
    base_classes: BaseClassList,

    const BaseClassList = std.ArrayList(*Self);

    const Self = @This();

    fn create(allocator: std.mem.Allocator, cur: c.CXCursor) !*Self {
        const size = c.clang_Type_getSizeOf(c.clang_getCursorType(cur));

        var instance = try allocator.create(Self);
        instance.cursor = cur;
        instance.size = if (size < 0) 0 else @intCast(size);
        instance.namespace = try getCursorNamespace(allocator, cur);
        instance.spelling = try copyCXStringAndDispose(allocator, c.clang_getCursorSpelling(cur));
        instance.expose_cursors = CursorList.init(allocator);
        instance.base_classes = BaseClassList.init(allocator);
        return instance;
    }

    fn create_with_tree(allocator: std.mem.Allocator, cur: c.CXCursor, tree: CursorList) !*Self {
        var namespace_builder = BasicString.init(allocator);

        for (0..tree.items.len - 1) |i| {
            const parent = tree.items[i];
            const parent_kind = c.clang_getCursorKind(parent);

            if (parent_kind == c.CXCursor_TranslationUnit or c.clang_equalCursors(parent, cur) != 0) {
                continue;
            }

            const parent_spelling = c.clang_getCursorSpelling(parent);
            defer c.clang_disposeString(parent_spelling);

            if (i < tree.items.len - 1) {
                try namespace_builder.appendFmt("{s}::", .{c.clang_getCString(parent_spelling)});
            } else {
                try namespace_builder.appendFmt("{s}", .{c.clang_getCString(parent_spelling)});
            }
        }

        var instance = try allocator.create(Self);
        instance.cursor = cur;
        instance.namespace = namespace_builder;
        instance.spelling = try copyCXStringAndDispose(allocator, c.clang_getCursorSpelling(cur));
        instance.expose_cursors = CursorList.init(allocator);
        instance.base_classes = BaseClassList.init(allocator);
        return instance;
    }

    fn destroy(self: *Self, allocator: std.mem.Allocator) void {
        self.namespace.deinit();
        allocator.free(self.spelling);
        self.expose_cursors.deinit();
        self.base_classes.deinit();

        allocator.destroy(self);
    }
};

//CXChildVisitResult var_decl_visitor(
//    CXCursor cursor, CXCursor parent, CXClientData data) {
//  auto kind = clang_getCursorKind(cursor);
//
//  switch (kind) {
//  case CXCursor_IntegerLiteral: {
//    auto res = clang_Cursor_Evaluate(cursor);
//    auto value = clang_EvalResult_getAsInt(res);
//    clang_EvalResult_dispose(res);
//
//    std::cout << "IntegerLiteral " << value << std::endl;
//
//    break;
//  }
//  default:
//    break;
//  }
//
//  return CXChildVisit_Recurse;
//}
//if (kind == CXCursor_IntegerLiteral)
//{
//    CXSourceRange range = clang_getCursorExtent(cursor);
//    CXToken *tokens = 0;
//    unsigned int nTokens = 0;
//    clang_tokenize(tu, range, &tokens, &nTokens);
//    for (unsigned int i = 0; i < nTokens; i++)
//    {
//        CXString spelling = clang_getTokenSpelling(tu, tokens[i]);
//        printf("token = %s\n", clang_getCString(spelling));
//        clang_disposeString(spelling);
//    }
//    clang_disposeTokens(tu, tokens, nTokens);
//}
//

fn getCursorRawTokensStr(allocator: std.mem.Allocator, tu: c.CXTranslationUnit, cursor: c.CXCursor) !BasicString {
    var raw_token_builder = BasicString.init(allocator);

    const range: c.CXSourceRange = c.clang_getCursorExtent(cursor);
    var tokens: [*c]c.CXToken = undefined;
    var num_tokens: c_uint = 0;
    _ = c.clang_tokenize(tu, range, &tokens, &num_tokens);
    defer c.clang_disposeTokens(tu, tokens, num_tokens);
    for (0..num_tokens) |i| {
        const spelling = c.clang_getTokenSpelling(tu, tokens[i]);
        defer c.clang_disposeString(spelling);
        if (i < num_tokens - 1) {
            try raw_token_builder.appendFmt("{s} ", .{c.clang_getCString(spelling)});
        } else {
            try raw_token_builder.appendFmt("{s}", .{c.clang_getCString(spelling)});
        }
    }

    return raw_token_builder;
}

const LiteralTag = enum { Int, Float, Char, String, Type, InitList };
const Literal = union(LiteralTag) {
    Int: u64,
    Float: f64,
    Char: u8,
    String: []const u8,
    Type: c.CXCursor, // typically CXCursor_DeclRefExpr
    InitList: std.ArrayList(@This()),

    fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        switch (self) {
            .InitList => |arr| {
                for (arr.items) |*c_l| {
                    c_l.deinit(allocator);
                }
                arr.deinit();
            },
            .String => |val| {
                allocator.free(val);
            },
            else => {},
        }
    }

    fn append_to_string(self: @This(), builder: *BasicString) !void {
        switch (self) {
            .Int,
            .Char,
            => |val| {
                try builder.appendFmt("{}", .{val});
            },
            .Float => |val| {
                try builder.appendFmt("{}", .{val});
            },
            .String => |val| {
                try builder.appendFmt("{s}", .{val});
            },
            .Type => |val| {
                const spelling = c.clang_getCursorSpelling(val);
                defer c.clang_disposeString(spelling);
                try builder.appendFmt("{s}", .{c.clang_getCString(spelling)});
            },
            .InitList => |val_arr| {
                for (val_arr.items) |val| {
                    try val.append_to_string(builder);
                }
            },
        }
    }
};

// TODO: Inline constexpr/const literals, fetch initialization data... instead of "get/set" overhead
const VarInfo = struct {
    cursor: c.CXCursor,
    // Debug
    namespace: BasicString,
    spelling: []const u8,
    raw_tokens: BasicString,
    // ======
    literal: ?Literal = null,

    const Self = @This();

    fn create_debug(allocator: std.mem.Allocator, cur: c.CXCursor, tu: c.CXTranslationUnit) !*Self {
        var instance = try allocator.create(Self);
        instance.cursor = cur;
        instance.namespace = try getCursorNamespace(allocator, cur);
        instance.spelling = try copyCXStringAndDispose(allocator, c.clang_getCursorSpelling(cur));
        instance.raw_tokens = try getCursorRawTokensStr(allocator, tu, cur);
        instance.literal = null;
        return instance;
    }

    fn addOrSetLiteral(self: *Self, new_literal: Literal) !void {
        if (self.literal) |*literal| {
            if (literal.* != .InitList) {
                @panic("New literal's shouldn't be assigned to already set VarInfo.");
            }
            try literal.InitList.append(new_literal);
        } else {
            self.literal = new_literal;
        }
    }

    fn destroy(self: *Self, allocator: std.mem.Allocator) void {
        self.namespace.deinit();
        allocator.free(self.spelling);
        self.raw_tokens.deinit();
        if (self.literal) |*literal| {
            literal.deinit(allocator);
        }
        allocator.destroy(self);
    }
};

fn create_var_info(args: anytype) !*VarInfo {
    const ArgsType = @TypeOf(args);
    const args_type_info = @typeInfo(ArgsType);
    if (args_type_info != .Struct) {
        @compileError("expected tuple or struct argument, found " ++ @typeName(ArgsType));
    }

    const fields_info = args_type_info.Struct.fields;
    if (fields_info.len != 3) {
        @compileError("requires only 3 arguments");
    }

    return VarInfo.create_debug(args[0], args[1], args[2]);
}

const EnumInfo = struct {
    cursor: c.CXCursor,
    // Debug
    namespace: BasicString,
    spelling: []const u8,
    raw_tokens: BasicString,
    // =====
    decls: DeclsType,

    const Self = @This();
    const DeclsType = std.ArrayList(struct { c.CXCursor, ?Literal });

    fn create_debug(allocator: std.mem.Allocator, cur: c.CXCursor, tu: c.CXTranslationUnit) !*Self {
        var instance = try allocator.create(Self);
        instance.cursor = cur;
        instance.namespace = try getCursorNamespace(allocator, cur);
        instance.spelling = try copyCXStringAndDispose(allocator, c.clang_getCursorSpelling(cur));
        instance.raw_tokens = try getCursorRawTokensStr(allocator, tu, cur);
        instance.decls = DeclsType.init(allocator);
        return instance;
    }

    fn addLiteral(self: *Self, new_literal: Literal) void {
        if (new_literal == .InitList) @panic("Enum decl as array?");
        if (self.decls.items[self.decls.items.len - 1][1] != null) {
            @panic("We shouldn't be replacing enum decls");
        }
        self.decls.items[self.decls.items.len - 1][1] = new_literal;
    }

    fn addDecl(self: *Self, cur: c.CXCursor) !void {
        try self.decls.append(.{ cur, null });
    }

    fn destroy(self: *Self, allocator: std.mem.Allocator) void {
        self.namespace.deinit();
        allocator.free(self.spelling);
        self.raw_tokens.deinit();
        for (0..self.decls.items.len) |i| {
            var literal = self.decls.items[i][1] orelse continue;
            literal.deinit(allocator);
        }
        self.decls.deinit();
        allocator.destroy(self);
    }
};

fn copyCXStringAndDispose(allocator: std.mem.Allocator, string: c.CXString) ![]const u8 {
    const c_str = c.clang_getCString(string);
    defer c.clang_disposeString(string);
    return std.fmt.allocPrint(allocator, "{s}", .{c_str});
}

const CursorChildVisitError = error{ ContinueAsRecurse, ForcedBreak };

fn C_CursorChildVisitorWrapper(comptime T: type, comptime visitor: fn (*T, c.CXCursor, c.CXCursor) anyerror!void) type {
    return struct {
        fn c_func(
            current: c.CXCursor,
            parent: c.CXCursor,
            user_data: ?*anyopaque,
        ) callconv(.C) c.CXChildVisitResult {
            const data = user_data orelse @panic("Shouldn't receive null context!");
            const context: *T = @ptrCast(@alignCast(data));

            visitor(context, current, parent) catch |err| {
                switch (err) {
                    CursorChildVisitError.ContinueAsRecurse => {
                        return c.CXChildVisit_Recurse;
                    },
                    CursorChildVisitError.ForcedBreak => {
                        return c.CXChildVisit_Break;
                    },
                    else => {
                        log.err("{}", .{err});
                        return c.CXChildVisit_Break;
                    },
                }
            };

            return c.CXChildVisit_Continue;
        }
    };
}

fn ToCursorChildVisitorT(
    comptime T: type,
    comptime visitor: fn (*T, c.CXCursor, c.CXCursor) anyerror!void,
) @TypeOf(C_CursorChildVisitorWrapper(T, visitor).c_func) {
    return C_CursorChildVisitorWrapper(T, visitor).c_func;
}

fn ToCursorChildVisitor(
    comptime visitor: fn (*Context, c.CXCursor, c.CXCursor) anyerror!void,
) @TypeOf(C_CursorChildVisitorWrapper(Context, visitor).c_func) {
    return C_CursorChildVisitorWrapper(Context, visitor).c_func;
}

fn printCursorTree(tree: CursorList) void {
    for (0..tree.items.len - 1) |i| {
        const parent = tree.items[i];
        const parent_kind = c.clang_getCursorKind(parent);
        if (parent_kind != c.CXCursor_TranslationUnit) {
            const parent_spelling = c.clang_getCursorSpelling(parent);
            defer c.clang_disposeString(parent_spelling);
            std.debug.print("{s}_", .{c.clang_getCString(parent_spelling)});
        }
    }
}

fn printCursorNamespace(cur: c.CXCursor, curr_tree: ?CursorList) void {
    if (curr_tree) |tree| {
        printCursorTree(tree);
    }
    const kind = c.clang_getCursorKind(cur);
    if (kind == c.CXCursor_Constructor) {
        std.debug.print("new\n", .{});
    } else if (kind == c.CXCursor_Destructor) {
        std.debug.print("delete\n", .{});
    } else {
        const spelling = c.clang_getCursorSpelling(cur);
        defer c.clang_disposeString(spelling);
        const spelling_cstr = c.clang_getCString(spelling);
        if (kind == c.CXCursor_VarDecl or kind == c.CXCursor_FieldDecl) {
            std.debug.print("get_{s}\n", .{spelling_cstr});
            if (curr_tree) |tree| {
                printCursorTree(tree);
            }
            std.debug.print("set_{s}\n", .{spelling_cstr});
        } else {
            std.debug.print("{s}\n", .{spelling_cstr});
        }
    }
}

fn printCursorNamespace2(allocator: std.mem.Allocator, cur: c.CXCursor) !void {
    var namespace_string = try getCursorNamespace(allocator, cur);
    defer namespace_string.deinit();

    std.debug.print("{s}", .{namespace_string.str()});
    const kind = c.clang_getCursorKind(cur);
    if (kind == c.CXCursor_Constructor) {
        std.debug.print("_new\n", .{});
    } else if (kind == c.CXCursor_Destructor) {
        std.debug.print("_delete\n", .{});
    } else {
        const spelling = c.clang_getCursorSpelling(cur);
        defer c.clang_disposeString(spelling);
        const spelling_cstr = c.clang_getCString(spelling);
        if (kind == c.CXCursor_VarDecl or kind == c.CXCursor_FieldDecl) {
            std.debug.print("_get_{s}\n", .{spelling_cstr});
            std.debug.print("_{s}", .{namespace_string.str()});
            std.debug.print("_set_{s}\n", .{spelling_cstr});
        } else {
            std.debug.print("_{s}\n", .{spelling_cstr});
        }
    }
}

fn printDebugCursor(cursor: c.CXCursor) void {
    const kind = c.clang_getCursorKind(cursor);
    const c_type = c.clang_getCursorType(cursor);
    const access_specifier = c.clang_getCXXAccessSpecifier(cursor);
    log.debugRet(@returnAddress(), "[{}] {} ({})", .{ access_specifier, kind, c_type });
}

fn handleDummyVisits(
    context: *Context,
    current: c.CXCursor,
    _: c.CXCursor,
) !void {
    std.debug.print("[DummyVisit] ", .{});
    printDebugCursor(current);
    _ = c.clang_visitChildren(current, ToCursorChildVisitor(handleDummyVisits), @as(*anyopaque, context));
}

fn handleTopLevelCursors(
    context: *Context,
    current: c.CXCursor,
    parent: c.CXCursor,
) !void {
    _ = parent;

    const kind = c.clang_getCursorKind(current);
    var ignore = false;
    if (c.clang_Location_isInSystemHeader(c.clang_getCursorLocation(current)) != 0) {
        ignore = true;
    }

    if (!ignore) {
        try context.tree.append(current);

        switch (kind) {
            c.CXCursor_Namespace => {
                _ = c.clang_visitChildren(current, ToCursorChildVisitor(handleChildLevelCursors), @as(*anyopaque, context));
            },
            c.CXCursor_ClassDecl,
            c.CXCursor_StructDecl,
            => {
                _ = try createOrGetClassInfo(context, current);
            },
            c.CXCursor_VarDecl => {
                _ = try createFetchVarInfo(context, current);
            },
            c.CXCursor_FunctionDecl,
            c.CXCursor_TypeRef,
            c.CXCursor_TypedefDecl,
            => {
                try context.exposes_as_is.append(current);
            },
            c.CXCursor_EnumDecl => {
                _ = try createOrGetEnumInfo(context, current);
            },
            else => {
                // ignore..
            },
        }
        _ = context.tree.pop();
    }
}

fn handleChildLevelCursors(
    context: *Context,
    current: c.CXCursor,
    _: c.CXCursor,
) !void {
    try context.tree.append(current);
    defer _ = context.tree.pop();
    const kind = c.clang_getCursorKind(current);

    printDebugCursor(current);

    const access_specifier = c.clang_getCXXAccessSpecifier(current);
    if (access_specifier == c.CX_CXXInvalidAccessSpecifier or access_specifier == c.CX_CXXPublic) {
        switch (kind) {
            c.CXCursor_Namespace => {
                _ = c.clang_visitChildren(current, ToCursorChildVisitor(handleChildLevelCursors), @as(*anyopaque, context));
            },
            c.CXCursor_ClassDecl,
            c.CXCursor_StructDecl,
            => {
                _ = try createOrGetClassInfo(context, current);
            },
            c.CXCursor_VarDecl => {
                _ = try createFetchVarInfo(context, current);
            },
            c.CXCursor_FunctionDecl,
            c.CXCursor_TypeAliasDecl,
            c.CXCursor_TypedefDecl,
            => {
                try context.exposes_as_is.append(current);
            },
            c.CXCursor_EnumDecl => {
                _ = try createOrGetEnumInfo(context, current);
            },
            else => {},
        }
    }
}

inline fn createOrGetEnumInfo(context: *Context, cur: c.CXCursor) !*EnumInfo {
    if (context.enums.get(cur)) |@"enum"| return @"enum";
    const info = try context.enums.putFetchWithTU(cur, context.translation_unit);
    context.working_enum = info;
    _ = c.clang_visitChildren(cur, ToCursorChildVisitor(handleEnumDecls), @as(*anyopaque, context));
    return info;
}

fn handleEnumDecls(
    context: *Context,
    current: c.CXCursor,
    _: c.CXCursor,
) !void {
    const kind = c.clang_getCursorKind(current);
    if (kind == c.CXCursor_EnumConstantDecl) {
        try printCursorNamespace2(context.allocator, current);
        try context.working_enum.addDecl(current);
    } else if (try convertCursorToLiteral(context.allocator, current)) |literal| {
        //log.debug("Cursor -> Literal: {}", .{literal});
        context.working_enum.addLiteral(literal);
    }
    return error.ContinueAsRecurse;
}

fn convertCursorToLiteral(allocator: std.mem.Allocator, cur: c.CXCursor) !?Literal {
    const kind = c.clang_getCursorKind(cur);

    const eval_res = c.clang_Cursor_Evaluate(cur);
    defer c.clang_EvalResult_dispose(eval_res);

    var literal: ?Literal = null;
    switch (kind) {
        c.CXCursor_IntegerLiteral => {
            if (c.clang_EvalResult_isUnsignedInt(eval_res) == 0) {
                literal = Literal{ .Int = @intCast(c.clang_EvalResult_getAsLongLong(eval_res)) };
            } else {
                literal = Literal{ .Int = @intCast(c.clang_EvalResult_getAsUnsigned(eval_res)) };
            }
        },
        c.CXCursor_FloatingLiteral => {
            literal = Literal{ .Float = @floatCast(c.clang_EvalResult_getAsDouble(eval_res)) };
        },
        //c.CXCursor_ImaginaryLiteral,
        c.CXCursor_StringLiteral => {
            literal = Literal{ .String = try std.fmt.allocPrint(allocator, "{s}", .{c.clang_EvalResult_getAsStr(eval_res)}) };
        },
        c.CXCursor_CharacterLiteral => {
            literal = Literal{ .Char = @intCast(c.clang_EvalResult_getAsInt(eval_res)) };
        },
        c.CXCursor_DeclRefExpr => {
            literal = Literal{ .Type = cur };
        },
        c.CXCursor_InitListExpr => {
            literal = Literal{ .InitList = std.ArrayList(Literal).init(allocator) };
        },
        else => {},
    }
    return literal;
}

inline fn createFetchVarInfo(context: *Context, current: c.CXCursor) !*VarInfo {
    const var_info = try VarInfo.create_debug(context.allocator, current, context.translation_unit);
    try context.vars.append(var_info);

    // TODO: Inline constexpr/const literals, fetch initialization data... instead of "get/set" overhead
    //const init_cursor = c.clang_Cursor_getVarDeclInitializer(current);
    //printDebugCursor(init_cursor);

    //if (c.clang_Cursor_isNull(init_cursor) == 0) {
    //    std.debug.print("[InitLiteral] {s}_{s}\n", .{ var_info.namespace.str(), var_info.spelling });
    //    _ = c.clang_visitChildren(current, ToCursorChildVisitor(handleVarDeclCursors), @as(*anyopaque, context));
    //    if (context.vars.getLast().literal == null) {
    //        log.warn("Couldn't find any proper initializer for variable decleration.\n  init:{}", .{init_cursor});
    //    }
    //} else {
    //    printCursorNamespace(current, context.tree);
    //    //_ = c.clang_visitChildren(current, ToCursorChildVisitor(handleDummyVisits), @as(*anyopaque, context));
    //}

    _ = c.clang_visitChildren(current, ToCursorChildVisitor(handleVarDeclCursors), @as(*anyopaque, context));
    return var_info;
}

// TODO: Inline constexpr/const literals, fetch initialization data... instead of "get/set" overhead
fn handleVarDeclCursors(
    context: *Context,
    current: c.CXCursor,
    _: c.CXCursor,
) !void {
    const kind = c.clang_getCursorKind(current);
    //printDebugCursor(current);

    switch (kind) {
        c.CXCursor_EnumDecl => {
            //var namespace = try getCursorNamespace(context.allocator, current);
            //defer namespace.deinit();
            //log.debug("VarDecl Enum => {s}", .{namespace.str()});
            //_ = c.clang_visitChildren(current, ToCursorChildVisitor(handleDummyVisits), @as(*anyopaque, context));
            _ = try createOrGetEnumInfo(context, current);
        },
        c.CXCursor_ClassDecl,
        c.CXCursor_StructDecl,
        => {
            if (!context.classes.contains(current)) {
                _ = try createOrGetClassInfo(context, current);
            }
        },
        else => {
            //if (try convertCursorToLiteral(context.allocator, current)) |literal| {
            //log.debug("Cursor -> Literal: {}", .{literal});
            //var var_info = context.vars.getLast();
            //try var_info.addOrSetLiteral(literal);
            //}
            return error.ContinueAsRecurse;
        },
    }
}

fn createOrGetClassInfo(context: *Context, cur: c.CXCursor) !*ClassInfo {
    const decl_cur = c.clang_getTypeDeclaration(c.clang_getCursorType(cur));
    const decl_hash = c.clang_hashCursor(decl_cur);
    const cur_hash = c.clang_hashCursor(cur);

    // probably typedef struct SomeStruct ss;
    const parsing_cur = if (decl_hash != cur_hash) decl_cur else cur;

    if (context.classes.get(parsing_cur)) |class| return class;
    var info = try context.classes.putFetch(parsing_cur);

    log.debug("~~Start CS {s}_{s}({}): {}bytes~~", .{
        info.namespace.str(),
        info.spelling,
        c.clang_hashCursor(parsing_cur),
        info.size,
    });

    try context.class_tree.append(info);
    defer {
        log.debug("==EndOf CS==", .{});
        _ = context.class_tree.pop();
    }
    _ = c.clang_visitChildren(parsing_cur, ToCursorChildVisitor(handleClassDeclCursors), @as(*anyopaque, context));
    return info;
}

fn handleClassDeclCursors(
    context: *Context,
    current: c.CXCursor,
    _: c.CXCursor,
) !void {
    try context.tree.append(current);
    defer _ = context.tree.pop();

    printDebugCursor(current);

    const kind = c.clang_getCursorKind(current);
    const access_specifier = c.clang_getCXXAccessSpecifier(current);
    if (access_specifier == c.CX_CXXInvalidAccessSpecifier or access_specifier == c.CX_CXXPublic) {
        switch (kind) {
            c.CXCursor_VarDecl => {
                _ = try createFetchVarInfo(context, current);
            },
            c.CXCursor_Constructor,
            c.CXCursor_Destructor,
            c.CXCursor_CXXMethod,
            c.CXCursor_FunctionDecl,
            c.CXCursor_FieldDecl,
            c.CXCursor_TypeAliasDecl,
            c.CXCursor_TypedefDecl,
            => {
                printCursorNamespace(current, context.tree);
                try context.class_tree.getLast().expose_cursors.append(current);
            },
            c.CXCursor_CXXBaseSpecifier => {
                const spelling = c.clang_getCursorSpelling(current);
                defer c.clang_disposeString(spelling);
                log.debug("BaseSpecifier: {s}({})", .{ c.clang_getCString(spelling), c.clang_hashCursor(current) });

                _ = c.clang_visitChildren(current, ToCursorChildVisitor(handleBaseSpecifierCursors), @as(*anyopaque, context));
            },
            c.CXCursor_ClassDecl,
            c.CXCursor_StructDecl,
            => {
                _ = try createOrGetClassInfo(context, current);
            },
            c.CXCursor_EnumDecl => {
                _ = try createOrGetEnumInfo(context, current);
            },
            else => {
                // ignore
            },
        }
    }
}

inline fn ignoreBaseSpecifierType(kind: c.CXTypeKind) bool {
    return kind == c.CXType_Elaborated;
}

inline fn ignoreBaseSpecifierCursor(kind: c.CXCursorKind) bool {
    return kind == c.CXCursor_NamespaceRef;
}

fn handleBaseSpecifierCursors(
    context: *Context,
    current: c.CXCursor,
    _: c.CXCursor,
) !void {
    const kind = c.clang_getCursorKind(current);
    const c_type = c.clang_getCursorType(current);

    var success = false;

    if (kind == c.CXCursor_TypeRef) {
        if (c_type.kind == c.CXType_Record) {
            const type_decl_cursor = c.clang_getTypeDeclaration(c_type);
            if (c.clang_getCursorKind(type_decl_cursor) == c.CXCursor_ClassDecl) {
                const base_class = try createOrGetClassInfo(context, type_decl_cursor);
                try context.class_tree.getLast().base_classes.append(base_class);
                success = true;
            }
        }
    } else {
        // ignoring some types
        success = ignoreBaseSpecifierCursor(kind);
    }

    if (!success) {
        const format =
            \\Unhandled base specifier type:
            \\  CXType: {}
            \\  CXCursor: {}
        ;
        log.warn(format, .{ c_type, current });
    } else {
        // we don't want to continue...
        return error.ForcedBreak;
    }
}

fn getAlignCTypeStr(align_sz: c_longlong) []const u8 {
    return switch (align_sz) {
        1 => "uint8_t",
        2 => "uint16_t",
        4 => "uint32_t",
        8 => "uint64_t",
        else => "long double",
    };
}

const GenerationEnv = struct {
    context: *Context,
    type_map: TypeMap,
    bindings_builder: BasicString,
    indention: []const u8,
    ns_sep: []const u8,

    const TypeMap = CursorMap(BasicString, BasicString.init);

    fn init(context: *Context) @This() {
        return .{
            .context = context,
            .type_map = TypeMap.init(context.allocator),
            .bindings_builder = BasicString.init(context.allocator),
            .indention = "",
            .ns_sep = "_",
        };
    }

    fn deinit(self: *@This()) void {
        self.bindings_builder.deinit();
        self.type_map.deinit();
    }
};

const FieldParamInfo = struct {
    is_pointer: bool,
    is_type_primitive: bool,
    cursor: c.CXCursor,
};

fn handleFieldParamVisit(
    info: *FieldParamInfo,
    current: c.CXCursor,
    _: c.CXCursor,
) !void {
    const kind = c.clang_getCursorKind(current);
    if (kind != c.CXCursor_TypeRef) {
        log.err("\ncur: {}", .{current});
        @panic("Received unexpected cursor for field param visit.");
    }
    const c_type = c.clang_getCursorType(current);
    const decl_cursor = c.clang_getTypeDeclaration(c_type);

    const decl_kind = c.clang_getCursorKind(current);
    if (decl_kind == c.CXCursor_InvalidFile or decl_kind == c.CXCursor_NoDeclFound) {
        log.err("\ndecl: {}\ncur: {}", .{ decl_cursor, current });
        @panic("Unexpected decl kind received.");
    }

    info.cursor = decl_cursor;
    info.is_type_primitive = false;
    return CursorChildVisitError.ForcedBreak;
}

fn getFieldParamInfo(allocator: std.mem.Allocator, cursor: c.CXCursor) !*FieldParamInfo {
    const c_type = c.clang_getCursorType(cursor);
    var info = try allocator.create(FieldParamInfo);
    info.is_pointer = c_type.kind == c.CXType_Pointer;
    info.cursor = cursor;
    info.is_type_primitive = true;
    _ = c.clang_visitChildren(cursor, ToCursorChildVisitorT(FieldParamInfo, handleFieldParamVisit), @as(*anyopaque, info));
    const spelling = c.clang_getTypeSpelling(c.clang_getCursorType(info.cursor));
    defer c.clang_disposeString(spelling);
    log.debug("Field/Param: {s}", .{c.clang_getCString(spelling)});
    return info;
}

fn generateExposeCBindings(env: *GenerationEnv, cursor: c.CXCursor, parrent: ?c.CXCursor) !void {
    const kind = c.clang_getCursorKind(cursor);
    const c_type = c.clang_getCursorType(cursor);
    var full_name = try getCursorFullNamespaceWithSep(env.context.allocator, cursor, env.ns_sep);

    switch (kind) {
        c.CXCursor_FieldDecl => {
            const parent_cur = parrent.?;
            var parent_full_name = try getCursorFullNamespaceWithSep(env.context.allocator, parent_cur, env.ns_sep);
            defer {
                parent_full_name.deinit();
                full_name.deinit();
            }
            try env.bindings_builder.append(env.indention);

            const info = try getFieldParamInfo(env.context.allocator, cursor);
            defer env.context.allocator.destroy(info);

            if (info.is_type_primitive) {
                const spelling = c.clang_getTypeSpelling(c.clang_getCursorType(info.cursor));
                defer c.clang_disposeString(spelling);

                // getter
                try full_name.append("_get");

                try env.bindings_builder.appendFmt("{s} {s}({s} * self);\n", .{
                    c.clang_getCString(spelling),
                    full_name.str(),
                    parent_full_name.str(),
                });

                // setter
                try full_name.replace("_get", "_set");

                try env.bindings_builder.appendFmt("void {s}({s} * self, {s} value);\n", .{
                    full_name.str(),
                    parent_full_name.str(),
                    c.clang_getCString(spelling),
                });
            } else {
                var ref_full_name = try getCursorFullNamespaceWithSep(env.context.allocator, info.cursor, env.ns_sep);
                defer ref_full_name.deinit();

                // getter
                try full_name.append("_get");

                try env.bindings_builder.appendFmt("{s} ", .{ref_full_name.str()});
                if (info.is_pointer) {
                    try env.bindings_builder.append("* ");
                }

                try env.bindings_builder.appendFmt("{s}({s} * self);\n", .{
                    full_name.str(),
                    parent_full_name.str(),
                });

                // setter
                try full_name.replace("_get", "_set");
                try env.bindings_builder.appendFmt(
                    "void {s}({s} * self, ",
                    .{ full_name.str(), parent_full_name.str() },
                );

                try env.bindings_builder.appendFmt("{s}", .{ref_full_name.str()});
                if (info.is_pointer) {
                    try env.bindings_builder.append(" *");
                }
                try env.bindings_builder.append(" value);\n");
            }
        },
        c.CXCursor_TypeAliasDecl,
        c.CXCursor_TypedefDecl,
        => {
            try env.bindings_builder.append(env.indention);
            try env.bindings_builder.append("typedef ");
            const actual_type = c.clang_getCanonicalType(c_type);
            const actual_cursor = c.clang_getTypeDeclaration(actual_type);

            const is_enum_type = actual_type.kind == c.CXType_Enum;

            if (env.type_map.get(actual_cursor)) |cached_name| {
                if (is_enum_type) {
                    try env.bindings_builder.append("enum ");
                } else if (actual_type.kind == c.CXType_Record) {
                    try env.bindings_builder.append("struct ");
                }
                try env.bindings_builder.appendFmt("{s} ", .{cached_name.str()});
            } else if (is_enum_type) {
                try env.bindings_builder.append("unsigned ");
            } else {
                const actual_size = c.clang_Type_getSizeOf(actual_type);
                const actual_align = c.clang_Type_getAlignOf(actual_type);
                const align_type = getAlignCTypeStr(actual_align);
                try env.bindings_builder.appendFmt(
                    "struct {{ {s} _pad; uint8_t data[{} - sizeof({s})]; }} ",
                    .{ align_type, actual_size, align_type },
                );
            }

            try env.bindings_builder.appendFmt("{s};\n", .{full_name.str()});
            try env.type_map.put(cursor, full_name);
        },
        c.CXCursor_Constructor,
        c.CXCursor_Destructor,
        c.CXCursor_CXXBaseSpecifier,
        c.CXCursor_FunctionDecl,
        => {
            defer full_name.deinit();
            try env.bindings_builder.appendFmt("//TODO: func: {s}\n", .{full_name.str()});
        },
        else => unreachable,
    }
}

fn generateCBindgins(context: *Context) !void {
    var env = GenerationEnv.init(context);
    defer env.deinit();

    // handling enums..
    var enums = env.context.enums.iterator();
    while (enums.next()) |entry| {
        const cpp_enum = entry.value_ptr.*;
        var full_name = try getCursorFullNamespaceWithSep(env.context.allocator, cpp_enum.cursor, env.ns_sep);

        try env.bindings_builder.appendFmt("typedef enum {{\n", .{});
        for (cpp_enum.decls.items) |decl_tuple| {
            const decl = decl_tuple[0];
            const decl_spelling = c.clang_getCursorSpelling(decl);
            defer c.clang_disposeString(decl_spelling);

            try env.bindings_builder.appendFmt("  {s}", .{c.clang_getCString(decl_spelling)});
            if (decl_tuple[1]) |literal| {
                try env.bindings_builder.append(" = ");
                try literal.append_to_string(&env.bindings_builder);
            }
            try env.bindings_builder.appendFmt(",\n", .{});
        }
        try env.bindings_builder.appendFmt("}} {s};\n", .{full_name.str()});

        try env.type_map.put(cpp_enum.cursor, full_name);
    }

    try env.bindings_builder.append("\n");

    // handling classes, structs
    var classes = context.classes.iterator();
    while (classes.next()) |entry| {
        const class = entry.value_ptr.*;
        const c_type = c.clang_getCursorType(class.cursor);
        const size = c.clang_Type_getSizeOf(c_type);
        const c_align = c.clang_Type_getAlignOf(c_type);
        var full_name = try getCursorFullNamespaceWithSep(context.allocator, class.cursor, env.ns_sep);
        const align_type = getAlignCTypeStr(c_align);

        // incomplete type? skipping
        if (size > 0 and c_align > 0) {
            try env.bindings_builder.appendFmt(
                "typedef struct {{ {s} _pad; uint8_t data[{} - sizeof({s})]; }} {s};\n",
                .{ align_type, size, align_type, full_name.str() },
            );
            for (class.expose_cursors.items) |cursor| {
                try generateExposeCBindings(&env, cursor, class.cursor);
            }
        }
        try env.type_map.put(class.cursor, full_name);
    }

    try env.bindings_builder.append("\n");

    // handling typedefs, type aliases, global funcs
    for (context.exposes_as_is.items) |cursor| {
        try generateExposeCBindings(&env, cursor, null);
    }

    std.debug.print("====Generated Bindings====\n", .{});
    std.debug.print("{s}", .{env.bindings_builder.str()});
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

    var context = Context.init(allocator, translation_unit);
    defer context.deinit();

    const cursor = c.clang_getTranslationUnitCursor(translation_unit);
    const ret_visit_child = c.clang_visitChildren(cursor, ToCursorChildVisitor(handleTopLevelCursors), @as(*anyopaque, &context));

    try checkClangRetWithMsg(@intCast(ret_visit_child), "Traversing AST went wrong...");
    log.info("Visit children: {}", .{ret_visit_child});

    try generateCBindgins(&context);
}
