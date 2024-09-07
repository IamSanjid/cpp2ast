const std = @import("std");
const common = @import("common.zig");
const clang = @import("clang.zig");
const c = struct {
    usingnamespace clang.c;
};

const BasicString = common.BasicString;

const TEMP_FILE_NAME = "generate_temp.h";

// TODO: Accept options?
pub fn createTranslationUnitDefault(
    allocator: std.mem.Allocator,
    cpp_files: []const []const u8,
    inc_dirs: []const []const u8,
) !clang.TranslationUnit {
    var args = std.ArrayList([*:0]const u8).init(allocator);
    var copied_inc_dirZ = std.ArrayList([:0]const u8).init(allocator);
    defer {
        for (copied_inc_dirZ.items) |inc_dirZ| {
            allocator.free(inc_dirZ);
        }
        copied_inc_dirZ.deinit();
        args.deinit();
    }
    for (inc_dirs) |inc_dir| {
        try args.append("-isystem");

        const inc_dirz = try allocator.dupeZ(u8, inc_dir);
        errdefer allocator.free(inc_dirz);

        try copied_inc_dirZ.append(inc_dirz);
        try args.append(inc_dirz);
    }
    try args.append("-x");
    try args.append("c++");
    try args.append("-std=c++20");
    try args.append("-DWIN32_LEAN_AND_MEAN");
    try args.append("-fparse-all-comments");

    var temp_file_builder = BasicString.init(allocator);
    defer temp_file_builder.deinit();
    for (cpp_files) |cpp_file| {
        try temp_file_builder.appendFmt("#include \"{s}\"\n", .{cpp_file});
    }

    var temp_files = try allocator.alloc(c.CXUnsavedFile, 1);
    defer allocator.free(temp_files);
    temp_files[0] = .{
        .Filename = TEMP_FILE_NAME,
        .Contents = temp_file_builder.c_str(),
        .Length = @intCast(temp_file_builder.len),
    };

    try args.append(TEMP_FILE_NAME);

    return clang.TranslationUnit.parse(.{
        .args = args.items,
        .unsaved_files = temp_files,
        .skip_function_bodies = true,
    });
}

// TODO: Make it not dummy?
pub fn createDummyTranslationUnit(
    allocator: std.mem.Allocator,
    cpp_file_path: []const u8,
) !clang.TranslationUnit {
    const clang_include_dir = @import("clang_runtime").include_dir;

    return createTranslationUnitDefault(
        allocator,
        &.{cpp_file_path},
        &.{clang_include_dir},
    );

    //var argv = std.ArrayList([*:0]const u8).init(allocator);
    //defer argv.deinit();
    //try argv.append("-isystem");
    //try argv.append(clang_runtime.include_dir.ptr);
    //try argv.append("-x");
    //try argv.append("c++");
    //try argv.append("-std=c++20");
    //try argv.append("-DWIN32_LEAN_AND_MEAN");
    //try argv.append("-fparse-all-comments");

    //var temp_file_builder = BasicString.init(allocator);
    //defer temp_file_builder.deinit();
    //try temp_file_builder.appendFmt("#include \"{s}\"\n", .{cpp_file_path});

    //var temp_files = try allocator.alloc(c.CXUnsavedFile, 1);
    //defer allocator.free(temp_files);
    //temp_files[0] = .{
    //    .Filename = TEMP_FILE_NAME,
    //    .Contents = temp_file_builder.c_str(),
    //    .Length = @intCast(temp_file_builder.len),
    //};

    //try argv.append(TEMP_FILE_NAME);

    //return clang.TranslationUnit.parse(.{
    //    .argv = argv.items,
    //    .unsaved_files = temp_files,
    //    .skip_function_bodies = true,
    //});

    //var translation_unit: c.CXTranslationUnit = undefined;
    //try clang.checkRetWithMsg(c.clang_parseTranslationUnit2(
    //    index,
    //    null,
    //    argv.items.ptr,
    //    @intCast(argv.items.len),
    //    temp_files.ptr,
    //    @intCast(temp_files.len),
    //    c.CXTranslationUnit_DetailedPreprocessingRecord | c.CXTranslationUnit_SkipFunctionBodies,
    //    &translation_unit,
    //), "Parsing translation unit failed...");

    //var err: c_int = 0;
    //const num_diagnostics = c.clang_getNumDiagnostics(translation_unit);
    //if (num_diagnostics > 0) {
    //    std.debug.print("\nClang says:\n", .{});
    //    for (0..num_diagnostics) |i| {
    //        const diag = c.clang_getDiagnostic(translation_unit, @intCast(i));
    //        const severity = c.clang_getDiagnosticSeverity(diag);

    //        if (severity == c.CXDiagnostic_Fatal or severity == c.CXDiagnostic_Error) {
    //            err = 1;
    //        }

    //        const @"struct" = c.clang_formatDiagnostic(diag, c.clang_defaultDiagnosticDisplayOptions());
    //        std.debug.print("{s}\n", .{c.clang_getCString(@"struct")});
    //        c.clang_disposeString(@"struct");
    //    }
    //}

    //try clang.checkRet(err);

    //return translation_unit;
}

const CursorList = std.ArrayList(clang.Cursor);

// Just normalizing "CXType"(s) to our interested format.
pub const TypeTag = enum {
    Void,
    Bool,
    Int,
    UInt,
    UInt128,
    Float,
    Float128,
    Array,
    Pointer,
    Function,
    Raw,

    pub fn fromType(c_type: clang.Type) @This() {
        return switch (c_type.kind()) {
            c.CXType_Void => .Void,
            c.CXType_Bool => .Bool,
            c.CXType_Short...c.CXType_LongLong,
            c.CXType_SChar,
            c.CXType_WChar,
            c.CXType_Char16,
            c.CXType_Char32,
            c.CXType_Char_S,
            => .Int,
            c.CXType_UShort...c.CXType_ULongLong,
            c.CXType_Char_U,
            c.CXType_UChar,
            => .UInt,
            c.CXType_UInt128 => .UInt128,
            c.CXType_Float...c.CXType_LongDouble => .Float,
            c.CXType_Float128 => .Float128,
            c.CXType_Half,
            c.CXType_Float16,
            => .Float,
            c.CXType_Complex => @panic("TODO: Handle complex type"),
            c.CXType_ConstantArray...c.CXType_DependentSizedArray => .Array,
            c.CXType_Pointer,
            c.CXType_RValueReference,
            c.CXType_LValueReference,
            c.CXType_MemberPointer,
            c.CXType_BlockPointer,
            c.CXType_ObjCObjectPointer,
            => .Pointer,
            c.CXType_FunctionProto, c.CXType_FunctionNoProto => .Function,
            else => .Raw,
        };
    }

    pub inline fn isInteger(self: @This()) bool {
        return switch (self) {
            .Int,
            .UInt,
            .UInt128,
            .Bool,
            => true,
            else => false,
        };
    }

    pub inline fn isReal(self: @This()) bool {
        return switch (self) {
            .Float, .Float128 => true,
            else => false,
        };
    }

    pub inline fn isPrimitive(self: @This()) bool {
        return self == .Void or self.isInteger() or self.isReal();
    }
};

const ConstLiteral = union(TypeTag) {
    Void: void,
    Bool: bool,
    Int: i64,
    UInt: u64,
    UInt128: u128,
    Float: f64,
    Float128: f128,
    Array: std.ArrayList(Self),
    Pointer: void,
    Function: void,
    Raw: void,

    const Self = @This();
    fn initTag(comptime tagName: []const u8, value: anytype) Self {
        return @unionInit(Self, tagName, value);
    }

    fn fromTag(tag: TypeTag, value: anytype) Self {
        const ValueType = @TypeOf(value);
        const value_type_info = @typeInfo(ValueType);

        return switch (value_type_info) {
            .void => if (tag != TypeTag.Void)
                @panic("Mistmatched tag for type " ++ @typeName(ValueType))
            else
                Self{ .Void = value },
            .bool => if (tag != TypeTag.Bool)
                @panic("Mistmatched tag for type " ++ @typeName(ValueType))
            else
                Self{ .Bool = value },
            .int => switch (tag) {
                .Bool => Self{ .Bool = value != 0 },
                .Int => Self{ .Int = @intCast(value) },
                .UInt => Self{ .UInt = @intCast(value) },
                .UInt128 => Self{ .UInt128 = @intCast(value) },
                else => {
                    @panic("Mistmatched tag for type " ++ @typeName(ValueType));
                },
            },
            .float => switch (tag) {
                .Float => Self{ .Float = @floatCast(value) },
                .Float128 => Self{ .Float128 = @floatCast(value) },
                else => {
                    @panic("Mistmatched tag for type " ++ @typeName(ValueType));
                },
            },
            else => if (tag == TypeTag.Array)
                Self{ .Array = value }
            else
                @panic("No valid `TypeTag` was provided to construct `ConstLiteral` for type " ++
                    @typeName(ValueType)),
        };
    }
};

pub const EnumType = struct {
    cursor: clang.Cursor,
    variants: std.ArrayList(Variant),

    const Self = @This();

    const Variant = struct { clang.Cursor, ?ConstLiteral };

    pub fn fromType(allocator: std.mem.Allocator, c_type: clang.Type) !Self {
        if (c_type.kind() != c.CXType_Enum) @panic("Didn't receive enum type");
        const declaration = c_type.declaration().canonical();
        const enum_type = declaration.enumType().?;
        const definition = declaration.definition() orelse declaration;
        const type_tag = TypeTag.fromType(enum_type);

        var childs = try definition.collectChildren(allocator);
        defer childs.deinit();

        var instance = Self{
            .cursor = definition,
            .variants = std.ArrayList(Variant).init(allocator),
        };
        for (childs.items) |child| {
            if (child.kind() == c.CXCursor_EnumConstantDecl) {
                const variant_value: ConstLiteral = switch (type_tag) {
                    .Bool, .Int => ConstLiteral.fromTag(
                        type_tag,
                        child.enumValue().?,
                    ),
                    .UInt => ConstLiteral.fromTag(
                        type_tag,
                        child.enumUnsignedValue().?,
                    ),
                    else => @panic("Enum shouldn't have any other types as their base other than int"),
                };

                try instance.variants.append(.{ child, variant_value });
            }
        }

        return instance;
    }

    pub fn deinit(self: *Self) void {
        self.variants.deinit();
    }
};

// Lazy info collector

pub inline fn c_typeToCanonTypeDef(c_type: clang.Type) clang.Cursor {
    const canonical_def = c_type.canonicalType().declaration().definition();
    return canonical_def orelse c_type.declaration();
}

pub inline fn c_typeToCanonTypeDefFallible(c_type: clang.Type) ?clang.Cursor {
    const result = c_typeToCanonTypeDef(c_type);

    return if (result.isInvalid() or result.kind() == c.CXCursor_NoDeclFound)
        null
    else
        result;
}

pub inline fn cursorToDeclType(cursor: clang.Cursor) clang.Type {
    const CurTypeTester = struct {
        original_cur: clang.Cursor,

        const Self = @This();
        fn init(cur: clang.Cursor) Self {
            return Self{ .original_cur = cur };
        }

        inline fn falliblePerform(self: Self) ?clang.Type {
            var current_cur = self.original_cur;
            var last_c_type: ?clang.Type = null;

            return blk: while (true) {
                const c_type = current_cur.c_type();

                const new_c_type = switch (c_type.kind()) {
                    c.CXType_Elaborated => c_type.namedType(),
                    c.CXType_Typedef => current_cur.typedefType() orelse @panic("Typedef invalid?"),
                    else => {
                        break :blk last_c_type;
                    },
                };

                const next_cur = c_typeToCanonTypeDefFallible(new_c_type) orelse {
                    break :blk new_c_type;
                };
                current_cur = if (!current_cur.equal(next_cur))
                    next_cur
                else
                    break :blk new_c_type;

                last_c_type = new_c_type;
            };
        }

        inline fn perform(self: Self) clang.Type {
            return self.falliblePerform() orelse self.original_cur.c_type();
        }
    };

    return CurTypeTester.init(cursor).perform();
}

// Some common things for most "Decl" cursor kinds...
pub const DeclInfo = struct {
    ref_c_type: clang.Type,
    ref_type: TypeTag,
    cursor: clang.Cursor,

    const Self = @This();

    pub fn fromCur(cur: clang.Cursor) Self {
        const c_type = cursorToDeclType(cur);

        std.debug.assert(cur.isDeclaration());

        return Self{
            .ref_c_type = c_type,
            .ref_type = TypeTag.fromType(c_type),
            //.init_val = cursor.varInitializer() != null,
            .cursor = cur,
        };
    }

    // irrelevant for function parameters
    pub fn isRefTypePub(self: Self) bool {
        return self.cursor.isPubliclyAccessible();
    }

    pub fn isRefTypeConst(self: Self) bool {
        return self.ref_c_type.isConst();
    }

    pub fn isTemplateLike(self: Self) bool {
        if (self.cursor.isTemplateLike()) return true;
        if (FunctionInfo.isCurTemplate(self.cursor)) return true;

        var is_template_fn: bool = false;
        const template_checker = struct {
            fn func(ret_val: *bool, current: clang.Cursor, _: clang.Cursor) !void {
                if (FunctionInfo.isCurTemplate(current)) {
                    ret_val.* = true;
                    return error.ForcedBreak;
                }
            }
        }.func;
        _ = self.cursor.visit(bool, template_checker, &is_template_fn);

        return is_template_fn;
    }

    fn hasTemplateInstantiationImpl(cur: clang.Cursor) bool {
        if (cur.kind() == c.CXCursor_TemplateRef) return true;

        const checker = struct {
            fn func(found: *bool, current: clang.Cursor, _: clang.Cursor) !void {
                if (Self.hasTemplateInstantiationImpl(current)) {
                    found.* = true;
                    return error.ForcedBreak;
                }
            }
        }.func;
        var found_temp_ins = false;
        _ = cur.visit(bool, checker, &found_temp_ins);
        return found_temp_ins;
    }

    pub fn hasTemplateInstantiation(self: Self) bool {
        return Self.hasTemplateInstantiationImpl(self.cursor);
    }

    pub fn isTopLevel(self: Self) bool {
        return self.cursor.isTopLevel();
    }

    pub fn name(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        return self.cursor.spelling(allocator);
    }
};

pub const FunctionInfo = struct {
    ret_c_type: clang.Type,
    ret_type: TypeTag,
    cursor: clang.Cursor,

    const Self = @This();
    const Error = error{ InvalidFuncCursor, TemplateFunc };

    pub fn isCurTemplate(cur: clang.Cursor) bool {
        const kind = cur.kind();
        if (kind == c.CXCursor_FunctionTemplate) return true;

        if (kind == c.CXCursor_Constructor or kind == c.CXCursor_Destructor) {
            const spelling_raw = cur.spellingRaw();
            defer spelling_raw.deinit();

            return std.mem.containsAtLeast(u8, spelling_raw.asBytes(), 1, "<");
        }

        return false;
    }

    pub fn fromCur(cur: clang.Cursor) Error!Self {
        var result_c_type = cur.c_type().retType() orelse return Error.InvalidFuncCursor;
        if (c_typeToCanonTypeDefFallible(result_c_type)) |res_type_decl| {
            result_c_type = cursorToDeclType(res_type_decl);
        }

        if (Self.isCurTemplate(cur)) return Error.TemplateFunc;

        return Self{
            .ret_c_type = result_c_type,
            .ret_type = TypeTag.fromType(result_c_type),
            .cursor = cur,
        };
    }

    // Should be used when CXCursor_VarDecl or CXCursor_TypedefDecl is encountered...
    // The generator/binder should keep track of all the declerations, this function
    // should never be used actually...
    pub fn fromType(ty: clang.Type, cur: clang.Cursor) Error!Self {
        var result_c_type = ty.retType() orelse cur.c_type().retType() orelse
            return Error.InvalidFuncCursor;
        if (c_typeToCanonTypeDefFallible(result_c_type)) |res_type_decl| {
            result_c_type = cursorToDeclType(res_type_decl);
        }

        // could be a CXCursor_VarDecl with type of CXType_Typedef...
        // typedef Type(*TypedefMethodPtr)(void);
        // TypedefMethodPtr VarDecl;
        // TODO: Probably not needed...
        var cursor = cur;
        var cur_c_type = cursor.c_type();
        while (true) {
            const tag = TypeTag.fromType(cur_c_type);
            if (tag == .Function) break;
            if (tag.isPrimitive()) return Error.InvalidFuncCursor;

            const new_c_type = switch (tag) {
                .Pointer => cur_c_type.pointeeType() orelse unreachable,
                .Array => cur_c_type.elemType() orelse unreachable,
                else => blk: {
                    break :blk switch (cur_c_type.kind()) {
                        c.CXType_Elaborated => cur_c_type.namedType(),
                        c.CXType_Typedef => cursor.typedefType() orelse
                            return Error.InvalidFuncCursor,
                        else => cur_c_type,
                    };
                },
            };

            cursor = c_typeToCanonTypeDefFallible(new_c_type) orelse break;
            cur_c_type = new_c_type;
        }

        if (Self.isCurTemplate(cur)) return Error.TemplateFunc;
        var is_template: bool = false;
        const template_checker = struct {
            fn func(ret_val: *bool, current: clang.Cursor, _: clang.Cursor) !void {
                if (Self.isCurTemplate(current)) {
                    ret_val.* = true;
                    return error.ForcedBreak;
                }
            }
        }.func;
        _ = cursor.visit(bool, template_checker, &is_template);

        if (is_template) return Error.TemplateFunc;

        return Self{
            .ret_c_type = result_c_type,
            .ret_type = TypeTag.fromType(result_c_type),
            .cursor = cursor,
        };
    }

    pub fn name(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        return self.cursor.spelling(allocator);
    }

    pub fn args(self: Self, allocator: std.mem.Allocator) !std.ArrayList(clang.Cursor) {
        return switch (self.cursor.kind()) {
            c.CXCursor_FunctionDecl,
            c.CXCursor_Constructor,
            c.CXCursor_CXXMethod,
            c.CXCursor_ObjCInstanceMethodDecl,
            c.CXCursor_ObjCClassMethodDecl,
            => {
                return self.cursor.arguments(allocator) orelse Error.InvalidFuncCursor;
            },
            c.CXCursor_Destructor => std.ArrayList(clang.Cursor).init(allocator),
            else => blk: {
                var args_list = std.ArrayList(clang.Cursor).init(allocator);
                const args_parser = struct {
                    fn func(
                        list: *std.ArrayList(clang.Cursor),
                        cur: clang.Cursor,
                        _: clang.Cursor,
                    ) !void {
                        switch (cur.kind()) {
                            c.CXCursor_ParmDecl => {
                                try list.append(cur);
                            },
                            else => {},
                        }
                    }
                }.func;
                _ = self.cursor.visit(std.ArrayList(clang.Cursor), args_parser, &args_list);
                if (args_list.items.len == 0) {
                    if (self.cursor.arguments(allocator)) |fallback_args| {
                        args_list = fallback_args;
                    }
                }
                break :blk args_list;
            },
        };
    }

    pub fn isStatic(self: Self) bool {
        return self.cursor.isMethodStatic();
    }

    pub fn isVirtual(self: Self) bool {
        return self.cursor.isMethodVirtual();
    }

    pub fn isPureVirtual(self: Self) bool {
        return self.cursor.isMethodPureVirtual();
    }

    pub fn isConstructor(self: Self) bool {
        return self.cursor.kind() == c.CXCursor_Constructor;
    }

    pub fn isDestructor(self: Self) bool {
        return self.cursor.kind() == c.CXCursor_Destructor;
    }

    pub fn isCXXMethod(self: Self) bool {
        return self.cursor.kind() == c.CXCursor_CXXMethod;
    }

    pub fn isOperator(self: Self) bool {
        return self.cursor.isMethodOperator();
    }
};

pub const CompoundType = struct {
    // TODO: Mostly "publicly accessed" things are "parsed"
    constructors: CursorList, // multiple constructors?
    destructor: ?clang.Cursor, // should be one destructor right?
    fields: CursorList,
    methods: CursorList, // base class methods are **not** included
    base_specifiers: CursorList,
    inner_decls: CursorList,
    cursor: clang.Cursor,

    const Self = @This();

    pub fn isValidCursor(cur: clang.Cursor) bool {
        return switch (cur.kind()) {
            c.CXCursor_UnionDecl => true,
            c.CXCursor_ClassDecl, c.CXCursor_StructDecl => true,
            c.CXCursor_CXXBaseSpecifier => true,
            // TODO: Parse template classes?
            c.CXCursor_ClassTemplatePartialSpecialization,
            c.CXCursor_ClassTemplate,
            => false,
            else => false,
        };
    }

    pub fn fromType(
        allocator: std.mem.Allocator,
        c_type: clang.Type,
        cur: clang.Cursor,
    ) error{InvalidArgsForCompType}!Self {
        var cursor = c_type.declaration();
        if (!isValidCursor(cursor)) {
            if (!isValidCursor(cur)) return error.InvalidArgsForCompType;
            cursor = cur;
        }

        var comp_type = Self{
            .cursor = cursor,
            .constructors = CursorList.init(allocator),
            .destructor = null,
            .fields = CursorList.init(allocator),
            .methods = CursorList.init(allocator),
            .base_specifiers = CursorList.init(allocator),
            .inner_decls = CursorList.init(allocator),
        };

        _ = cursor.visit(Self, Self.performParsing, &comp_type);

        return comp_type;
    }

    fn performParsing(self: *Self, cur: clang.Cursor, _: clang.Cursor) !void {
        // TODO: Parse all private fields/methods, templates?
        if (!cur.isPubliclyAccessible()) return;
        switch (cur.kind()) {
            c.CXCursor_Constructor => {
                try self.constructors.append(cur);
            },
            c.CXCursor_Destructor => {
                std.debug.assert(self.destructor == null);
                self.destructor = cur;
            },
            c.CXCursor_CXXMethod => {
                try self.methods.append(cur);
            },
            c.CXCursor_VarDecl, c.CXCursor_FieldDecl => {
                try self.fields.append(cur);
            },
            c.CXCursor_CXXBaseSpecifier => {
                // TODO: private,virtual ones too?
                if (cur.accessSpecifier() == c.CX_CXXPublic)
                    try self.base_specifiers.append(c_typeToCanonTypeDef(cur.c_type()));
            },
            c.CXCursor_TypedefDecl, c.CXCursor_TypeAliasDecl => {
                try self.inner_decls.append(cur);
            },
            else => {
                if (Self.isValidCursor(cur)) {
                    try self.inner_decls.append(cur);
                }
            },
        }
    }

    pub fn isTemplateLike(self: Self) bool {
        return self.cursor.isTemplateLike();
    }

    pub fn isTopLevel(self: Self) bool {
        return self.cursor.isTopLevel();
    }

    pub fn name(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        return self.cursor.spelling(allocator);
    }

    pub fn deinit(self: *Self) void {
        self.constructors.deinit();
        self.fields.deinit();
        self.methods.deinit();
        self.base_specifiers.deinit();
        self.inner_decls.deinit();
    }
};

test "enum type info test" {
    const cpp_file_path = "./test-sources/test_enums.h";

    const allocator = std.testing.allocator;

    var translation_unit = try createDummyTranslationUnit(
        allocator,
        cpp_file_path,
    );
    defer translation_unit.deinit();
    try translation_unit.printDiags();

    var root_cursor = translation_unit.cursor();

    const ns_childs = try root_cursor.collectChildren(allocator);
    defer ns_childs.deinit();
    try std.testing.expect(ns_childs.items.len > 0);

    for (ns_childs.items) |child| {
        if (child.isInSystemHeader() or child.kind() != c.CXCursor_Namespace) continue;
        root_cursor = child;
        break;
    }

    const childs = try root_cursor.collectChildren(allocator);
    defer childs.deinit();
    try std.testing.expect(childs.items.len > 0);

    var found_enum_decl = false;
    var encounters: usize = 0;
    for (childs.items) |child| {
        if (child.isInSystemHeader() or child.kind() != c.CXCursor_EnumDecl) continue;
        encounters += 1;

        var enum_decl = try EnumType.fromType(allocator, child.c_type());
        defer enum_decl.deinit();
        try std.testing.expect(enum_decl.variants.items.len > 0);

        // checking the namespace of enum declaration's
        try std.testing.expectEqual(root_cursor.hash(), enum_decl.cursor.semanticParent().hash());
        // checking the namespace of the enum varint which suppose to be the enum declaration
        try std.testing.expectEqual(enum_decl.cursor.hash(), enum_decl.variants.items[0][0].semanticParent().hash());

        var spelling: []const u8 = undefined;
        switch (encounters) {
            1 => {
                try std.testing.expectEqual(3, enum_decl.variants.items.len);

                {
                    spelling = try enum_decl.variants.items[1][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("Enum2", spelling);
                    try std.testing.expect(enum_decl.variants.items[1][1] != null);
                    try std.testing.expectEqual(2, enum_decl.variants.items[1][1].?.Int);
                }
                {
                    spelling = try enum_decl.variants.items[2][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("Enum3", spelling);
                    try std.testing.expect(enum_decl.variants.items[2][1] != null);
                    try std.testing.expectEqual(1, enum_decl.variants.items[2][1].?.Int);
                }
            },
            2 => {
                try std.testing.expectEqual(3, enum_decl.variants.items.len);

                {
                    spelling = try enum_decl.variants.items[1][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("Enum5", spelling);
                    try std.testing.expect(enum_decl.variants.items[1][1] != null);
                    try std.testing.expectEqual(4, enum_decl.variants.items[1][1].?.Int);
                }
                {
                    spelling = try enum_decl.variants.items[2][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("Enum6", spelling);
                    try std.testing.expect(enum_decl.variants.items[2][1] != null);
                    try std.testing.expectEqual(enum_decl.variants.items[0][1].?.Int, enum_decl.variants.items[2][1].?.Int);
                }
            },
            3 => {
                try std.testing.expectEqual(3, enum_decl.variants.items.len);

                {
                    spelling = try enum_decl.variants.items[1][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("Enum8", spelling);
                    try std.testing.expect(enum_decl.variants.items[1][1] != null);
                    try std.testing.expectEqual(6, enum_decl.variants.items[1][1].?.Int);
                }
                {
                    spelling = try enum_decl.variants.items[2][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("Enum9", spelling);
                    try std.testing.expect(enum_decl.variants.items[2][1] != null);
                    try std.testing.expectEqual(enum_decl.variants.items[0][1].?.Int, enum_decl.variants.items[2][1].?.Int);
                }
            },
            4 => {
                try std.testing.expectEqual(2, enum_decl.variants.items.len);

                {
                    spelling = try enum_decl.variants.items[0][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("EnumFalse", spelling);
                    try std.testing.expect(enum_decl.variants.items[0][1] != null);
                    try std.testing.expectEqual(false, enum_decl.variants.items[0][1].?.Bool);
                }
                {
                    spelling = try enum_decl.variants.items[1][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("EnumTrue", spelling);
                    try std.testing.expect(enum_decl.variants.items[1][1] != null);
                    try std.testing.expectEqual(true, enum_decl.variants.items[1][1].?.Bool);
                }
            },
            5 => {
                try std.testing.expectEqual(3, enum_decl.variants.items.len);

                {
                    spelling = try enum_decl.variants.items[0][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("VariantEnum1", spelling);
                    try std.testing.expect(enum_decl.variants.items[0][1] != null);
                    try std.testing.expectEqual(0, enum_decl.variants.items[0][1].?.Int);
                }
                {
                    spelling = try enum_decl.variants.items[1][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("VariantEnum2", spelling);
                    try std.testing.expect(enum_decl.variants.items[1][1] != null);
                    try std.testing.expectEqual(1, enum_decl.variants.items[1][1].?.Int);
                }
                {
                    spelling = try enum_decl.variants.items[2][0].spelling(allocator);
                    defer allocator.free(spelling);
                    try std.testing.expectEqualStrings("VariantEnum3", spelling);
                    try std.testing.expect(enum_decl.variants.items[2][1] != null);
                    try std.testing.expectEqual(enum_decl.variants.items[2][1].?.Int, enum_decl.variants.items[0][1].?.Int);
                }
            },
            else => {},
        }

        found_enum_decl = true;
    }
    try std.testing.expect(found_enum_decl);
}

test "basic method/function info" {
    const cpp_file_path = "./test-sources/test_functions.h";

    const allocator = std.testing.allocator;

    var translation_unit = try createDummyTranslationUnit(
        allocator,
        cpp_file_path,
    );
    defer translation_unit.deinit();
    try translation_unit.printDiags();

    var root_cursor = translation_unit.cursor();

    const ns_childs = try root_cursor.collectChildren(allocator);
    defer ns_childs.deinit();
    try std.testing.expect(ns_childs.items.len > 0);

    for (ns_childs.items) |child| {
        if (child.isInSystemHeader() or child.kind() != c.CXCursor_Namespace) continue;
        root_cursor = child;
        break;
    }

    const childs = try root_cursor.collectChildren(allocator);
    defer childs.deinit();
    try std.testing.expect(childs.items.len > 0);

    var fn_decl_cases: usize = 0;
    var var_decl_cases: usize = 0;
    for (childs.items) |child| {
        switch (child.kind()) {
            c.CXCursor_FunctionDecl,
            c.CXCursor_Constructor,
            c.CXCursor_CXXMethod,
            c.CXCursor_ObjCInstanceMethodDecl,
            c.CXCursor_ObjCClassMethodDecl,
            => {
                fn_decl_cases += 1;
                const func = try FunctionInfo.fromCur(child);

                switch (fn_decl_cases) {
                    1 => {
                        var args = try func.args(allocator);
                        // int Method1(void); `void` is not detected as a arg type...
                        try std.testing.expect(args.items.len == 0);
                        defer args.deinit();
                    },
                    else => {},
                }
            },
            c.CXCursor_VarDecl => {
                const vi = DeclInfo.fromCur(child);
                var_decl_cases += 1;

                switch (var_decl_cases) {
                    1 => {
                        try std.testing.expectEqual(.Pointer, vi.ref_type);
                        const vi_pointee_tag = TypeTag.fromType(vi.ref_c_type.pointeeType().?);
                        try std.testing.expectEqual(.Function, vi_pointee_tag);
                        try std.testing.expect(c_typeToCanonTypeDefFallible(vi.ref_c_type) == null);

                        const vi_name = try vi.name(allocator);
                        defer allocator.free(vi_name);
                        try std.testing.expectEqualStrings("VarMethod1", vi_name);

                        const func = try FunctionInfo.fromType(vi.ref_c_type.pointeeType().?, child);
                        try std.testing.expectEqual(.Int, func.ret_type);

                        var args = try func.args(allocator);
                        defer args.deinit();
                        try std.testing.expect(args.items.len > 0);
                    },
                    2 => {
                        try std.testing.expectEqual(.Pointer, vi.ref_type);
                        const vi_pointee_tag = TypeTag.fromType(vi.ref_c_type.pointeeType().?);
                        try std.testing.expectEqual(.Function, vi_pointee_tag);
                        try std.testing.expect(c_typeToCanonTypeDefFallible(vi.ref_c_type) == null);

                        const vi_name = try vi.name(allocator);
                        defer allocator.free(vi_name);
                        try std.testing.expectEqualStrings("VarMethod2", vi_name);

                        const func = try FunctionInfo.fromType(vi.ref_c_type.pointeeType().?, child);
                        try std.testing.expectEqual(.Int, func.ret_type);

                        var child_c_type_def = c_typeToCanonTypeDefFallible(child.c_type());
                        try std.testing.expectEqual(c.CXCursor_TypeAliasDecl, child_c_type_def.?.kind());
                        child_c_type_def = c_typeToCanonTypeDefFallible(child_c_type_def.?.typedefType().?);
                        try std.testing.expectEqual(c.CXCursor_TypedefDecl, child_c_type_def.?.kind());
                        //child_c_type_def = c_typeToCanonTypeDef(child_c_type_def.?.typedefType().?);
                        try std.testing.expectEqual(c.CXType_Pointer, child_c_type_def.?.typedefType().?.kind());
                        const child_c_type_def_pointee_tag = TypeTag.fromType(child_c_type_def.?.typedefType().?.pointeeType().?);
                        try std.testing.expectEqual(.Function, child_c_type_def_pointee_tag);

                        var args = try func.args(allocator);
                        defer args.deinit();
                        try std.testing.expect(args.items.len > 0);
                    },
                    else => {},
                }
            },
            c.CXCursor_TypedefDecl => {
                const vi = DeclInfo.fromCur(child);
                try std.testing.expectEqual(.Pointer, vi.ref_type);
                const vi_pointee_tag = TypeTag.fromType(vi.ref_c_type.pointeeType().?);
                try std.testing.expectEqual(.Function, vi_pointee_tag);

                const func = try FunctionInfo.fromType(vi.ref_c_type.pointeeType().?, child);
                try std.testing.expectEqual(.Int, func.ret_type);

                var args = try func.args(allocator);
                defer args.deinit();
                try std.testing.expect(args.items.len > 0);
            },
            else => {},
        }
    }

    try std.testing.expectEqual(1, fn_decl_cases);
}

test "struct/class type info" {
    const cpp_file_path = "./test-sources/test_structs.h";

    const allocator = std.testing.allocator;

    var translation_unit = try createDummyTranslationUnit(
        allocator,
        cpp_file_path,
    );
    defer translation_unit.deinit();
    try translation_unit.printDiags();

    var root_cursor = translation_unit.cursor();

    const ns_childs = try root_cursor.collectChildren(allocator);
    defer ns_childs.deinit();
    try std.testing.expect(ns_childs.items.len > 0);

    for (ns_childs.items) |child| {
        if (child.isInSystemHeader() or child.kind() != c.CXCursor_Namespace) continue;
        root_cursor = child;
        break;
    }

    const childs = try root_cursor.collectChildren(allocator);
    defer childs.deinit();
    try std.testing.expect(childs.items.len > 0);

    var def_types = clang.CursorHashMap(CompoundType).init(allocator);
    defer def_types.deinit();

    var cases: usize = 0;
    for (childs.items) |child| {
        // ignoring CXCursor_TypedefDecl!
        if (!CompoundType.isValidCursor(child)) continue;
        cases += 1;

        var struct_ty = try CompoundType.fromType(allocator, cursorToDeclType(child), child);
        defer struct_ty.deinit();
        try def_types.put(struct_ty.cursor, struct_ty);

        const decl_spelling = try struct_ty.cursor.spelling(allocator);
        defer allocator.free(decl_spelling);

        switch (cases) {
            0 => {
                try std.testing.expect(struct_ty.fields.items.len > 0);
                try std.testing.expectEqualStrings("Struct3", decl_spelling);

                const fi = DeclInfo.fromCur(struct_ty.fields.items[0]);
                try std.testing.expectEqual(.Raw, fi.ref_type);

                // We haven't seen it yet but the TranslationUnit knows about it.
                const fi_def = c_typeToCanonTypeDef(fi.ref_c_type);
                try std.testing.expect(def_types.get(fi_def) == null);
            },
            1 => {
                try std.testing.expect(struct_ty.fields.items.len > 0);
                try std.testing.expectEqualStrings("Struct1", decl_spelling);

                var fi_cases: usize = 0;
                for (struct_ty.fields.items) |field| {
                    fi_cases += 1;
                    const fi = DeclInfo.fromCur(field);

                    switch (fi_cases) {
                        1 => {
                            try std.testing.expectEqual(.Int, fi.ref_type);

                            const fi_name = try fi.name(allocator);
                            defer allocator.free(fi_name);
                            try std.testing.expectEqualStrings("Field1", fi_name);
                        },
                        4 => {
                            try std.testing.expectEqual(.Pointer, fi.ref_type);

                            const fi_name = try fi.name(allocator);
                            defer allocator.free(fi_name);
                            try std.testing.expectEqualStrings("Field4", fi_name);

                            // Haven't seen the decleration yet for parsing...
                            const fi_def = c_typeToCanonTypeDefFallible(fi.ref_c_type.pointeeType().?);
                            try std.testing.expect(fi_def != null);
                            // isDefinition saves the day? for incomplete types?
                            try std.testing.expect(fi_def.?.isDefinition());
                            try std.testing.expect(def_types.get(fi_def.?) == null);
                            var fi_ty = try CompoundType.fromType(allocator, fi_def.?.c_type(), fi_def.?);
                            defer fi_ty.deinit();
                            try std.testing.expect(fi_ty.fields.items.len > 0);
                        },
                        5 => {
                            try std.testing.expectEqual(.Pointer, fi.ref_type);

                            const fi_name = try fi.name(allocator);
                            defer allocator.free(fi_name);
                            try std.testing.expectEqualStrings("Field5", fi_name);

                            // Non-existing... the TU hasn't seen it yet..
                            const fi_def = c_typeToCanonTypeDefFallible(fi.ref_c_type.pointeeType().?);
                            try std.testing.expect(fi_def != null);
                            // TU not found it's definition..
                            try std.testing.expect(!fi_def.?.isDefinition());
                            try std.testing.expect(fi_def.?.isDeclaration());
                            var fi_ty = try CompoundType.fromType(allocator, fi_def.?.c_type(), fi_def.?);
                            defer fi_ty.deinit();
                            try std.testing.expectEqual(0, fi_ty.fields.items.len);
                        },
                        else => {},
                    }
                }
            },
            2 => {
                try std.testing.expect(struct_ty.fields.items.len > 0);
                try std.testing.expectEqualStrings("Struct2", decl_spelling);

                var fi_cases: usize = 0;
                for (struct_ty.fields.items) |field| {
                    fi_cases += 1;
                    const fi = DeclInfo.fromCur(field);

                    switch (fi_cases) {
                        2 => {
                            try std.testing.expectEqual(.Raw, fi.ref_type);

                            const fi_name = try fi.name(allocator);
                            defer allocator.free(fi_name);
                            try std.testing.expectEqualStrings("Field2", fi_name);

                            const fi_def = c_typeToCanonTypeDef(fi.ref_c_type);
                            try std.testing.expect(def_types.get(fi_def) != null);
                        },
                        3 => {
                            try std.testing.expectEqual(.Pointer, fi.ref_type);

                            const fi_name = try fi.name(allocator);
                            defer allocator.free(fi_name);
                            try std.testing.expectEqualStrings("Field3", fi_name);

                            const fi_def = c_typeToCanonTypeDefFallible(fi.ref_c_type.pointeeType().?);
                            try std.testing.expect(fi_def != null);
                            try std.testing.expect(def_types.get(fi_def.?) != null);
                        },
                        else => {},
                    }
                }
            },
            3 => {
                try std.testing.expect(struct_ty.fields.items.len > 0);
                try std.testing.expect(struct_ty.methods.items.len > 0);

                try std.testing.expectEqualStrings("Struct3", decl_spelling);

                var method_cases: usize = 0;
                for (struct_ty.methods.items) |method| {
                    const func = try FunctionInfo.fromCur(method);

                    switch (method_cases) {
                        0 => {
                            try std.testing.expectEqual(.Void, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method0", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(0, args.items.len);
                        },
                        1 => {
                            try std.testing.expectEqual(.Void, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method1", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(1, args.items.len);

                            const pi = DeclInfo.fromCur(args.items[0]);
                            try std.testing.expectEqual(.Int, pi.ref_type);
                        },
                        2 => {
                            try std.testing.expectEqual(.Void, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method2", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(1, args.items.len);

                            const pi = DeclInfo.fromCur(args.items[0]);
                            try std.testing.expectEqual(.Pointer, pi.ref_type);
                            try std.testing.expectEqual(.Int, TypeTag.fromType(pi.ref_c_type.pointeeType().?));
                        },
                        3 => {
                            try std.testing.expectEqual(.Void, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method3", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(1, args.items.len);

                            const pi = DeclInfo.fromCur(args.items[0]);
                            try std.testing.expectEqual(.Pointer, pi.ref_type);
                            try std.testing.expect(pi.ref_c_type.isROrLValueRef());
                        },
                        4 => {
                            try std.testing.expectEqual(.Void, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method4", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(1, args.items.len);

                            const pi = DeclInfo.fromCur(args.items[0]);
                            try std.testing.expectEqual(.Pointer, pi.ref_type);
                            try std.testing.expect(pi.ref_c_type.isROrLValueRef());
                        },
                        5 => {
                            try std.testing.expectEqual(.Void, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method5", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(1, args.items.len);

                            const pi = DeclInfo.fromCur(args.items[0]);
                            try std.testing.expectEqual(.Raw, pi.ref_type);

                            const pi_def = c_typeToCanonTypeDef(pi.ref_c_type);
                            try std.testing.expect(def_types.get(pi_def) != null);
                        },
                        6 => {
                            try std.testing.expectEqual(.Void, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method6", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(1, args.items.len);

                            const pi = DeclInfo.fromCur(args.items[0]);
                            try std.testing.expectEqual(.Pointer, pi.ref_type);

                            const pi_def = c_typeToCanonTypeDef(pi.ref_c_type.pointeeType().?);
                            try std.testing.expect(def_types.get(pi_def) != null);
                        },
                        7 => {
                            try std.testing.expectEqual(.Void, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method7", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(1, args.items.len);

                            const pi = DeclInfo.fromCur(args.items[0]);
                            try std.testing.expectEqual(.Pointer, pi.ref_type);
                            try std.testing.expect(pi.ref_c_type.isROrLValueRef());

                            const pi_def = c_typeToCanonTypeDef(pi.ref_c_type.pointeeType().?);
                            try std.testing.expect(def_types.get(pi_def) != null);
                        },
                        8 => {
                            try std.testing.expectEqual(.Raw, func.ret_type);

                            const ri_def = c_typeToCanonTypeDef(func.ret_c_type);
                            try std.testing.expect(def_types.get(ri_def) != null);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method8", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(0, args.items.len);
                        },
                        9 => {
                            try std.testing.expectEqual(.Pointer, func.ret_type);

                            const ri_def = c_typeToCanonTypeDef(func.ret_c_type.pointeeType().?);
                            try std.testing.expect(def_types.get(ri_def) != null);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method9", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(0, args.items.len);
                        },
                        10 => {
                            try std.testing.expectEqual(.Pointer, func.ret_type);
                            try std.testing.expect(func.ret_c_type.isROrLValueRef());

                            const ri_def = c_typeToCanonTypeDef(func.ret_c_type.pointeeType().?);
                            try std.testing.expect(def_types.get(ri_def) != null);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("Method10", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(0, args.items.len);
                        },
                        11 => {
                            try std.testing.expectEqual(.Void, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("StaticMethod1", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(1, args.items.len);

                            const pi = DeclInfo.fromCur(args.items[0]);
                            try std.testing.expectEqual(.Int, pi.ref_type);
                            try std.testing.expect(func.isStatic());
                            try std.testing.expect(!func.isOperator());
                        },
                        12 => {
                            try std.testing.expectEqual(.Pointer, func.ret_type);

                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            try std.testing.expectEqualStrings("operator->", func_name);

                            var args = try func.args(allocator);
                            defer args.deinit();

                            try std.testing.expectEqual(0, args.items.len);
                            try std.testing.expect(func.isOperator());
                        },
                        else => {},
                    }
                    method_cases += 1;
                }
            },
            else => {},
        }
    }

    try std.testing.expectEqual(3, cases);
}

test "basic cursor manipulatons" {
    const cpp_file_path = "./test-sources/test_basic.h";

    const allocator = std.testing.allocator;

    var translation_unit = try createDummyTranslationUnit(
        allocator,
        cpp_file_path,
    );
    defer translation_unit.deinit();
    try translation_unit.printDiags();

    var root_cursor = translation_unit.cursor();

    const ns_childs = try root_cursor.collectChildren(allocator);
    defer ns_childs.deinit();
    try std.testing.expect(ns_childs.items.len > 0);

    for (ns_childs.items) |child| {
        if (child.isInSystemHeader() or child.kind() != c.CXCursor_Namespace) continue;
        root_cursor = child;
        break;
    }

    const childs = try root_cursor.collectChildren(allocator);
    defer childs.deinit();
    try std.testing.expect(childs.items.len > 0);

    var comp_cases: usize = 0;
    var typedef_cases: usize = 0;
    var var_decl_cases: usize = 0;

    var comp_defs = std.ArrayList(clang.Cursor).init(allocator);
    defer comp_defs.deinit();

    for (childs.items) |child| {
        if (child.isInSystemHeader()) continue;
        switch (child.kind()) {
            c.CXCursor_StructDecl => {
                comp_cases += 1;
                switch (comp_cases) {
                    1 => {
                        try comp_defs.append(child.definition().?);
                        try std.testing.expectEqual(child.hash(), child.definition().?.hash());
                    },
                    else => {},
                }
            },
            c.CXCursor_TypedefDecl => {
                typedef_cases += 1;
                switch (typedef_cases) {
                    1 => {},
                    2 => {
                        const def = child.typedefType().?.declaration().definition().?;
                        try std.testing.expectEqual(comp_defs.items[0].hash(), def.hash());
                    },
                    else => {},
                }
            },
            c.CXCursor_VarDecl => {
                var_decl_cases += 1;
                switch (var_decl_cases) {
                    1 => {
                        const c_type = child.c_type();
                        const type_tag = TypeTag.fromType(c_type);
                        try std.testing.expectEqual(.Pointer, type_tag);

                        const pointee_type = c_type.pointeeType();
                        try std.testing.expect(pointee_type != null);

                        const decl_cur = c_typeToCanonTypeDef(pointee_type.?);
                        const is_not_decl = decl_cur.isInvalid() or decl_cur.kind() == c.CXCursor_NoDeclFound;
                        // `int` is a built-in type not any declaraton...
                        try std.testing.expect(is_not_decl);

                        const decl_spelling = try child.spelling(allocator);
                        defer allocator.free(decl_spelling);
                        try std.testing.expectEqualStrings("Var_Ptr_1", decl_spelling);
                    },
                    2 => {
                        const c_type = child.c_type();
                        const type_tag = TypeTag.fromType(c_type);
                        try std.testing.expectEqual(.Pointer, type_tag);

                        const pointee_type = c_type.pointeeType();
                        try std.testing.expect(pointee_type != null);

                        const decl_cur = c_typeToCanonTypeDef(pointee_type.?);
                        const is_not_decl = decl_cur.isInvalid() or decl_cur.kind() == c.CXCursor_NoDeclFound;
                        // points to typedef declaration...
                        try std.testing.expect(!is_not_decl);
                        try std.testing.expectEqual(c.CXCursor_TypedefDecl, decl_cur.kind());

                        const decl_spelling = try child.spelling(allocator);
                        defer allocator.free(decl_spelling);
                        try std.testing.expectEqualStrings("Var_Ptr_2", decl_spelling);
                    },
                    3 => {
                        const c_type = child.c_type();
                        const type_tag = TypeTag.fromType(c_type);
                        try std.testing.expectEqual(.Pointer, type_tag);

                        var pointee_type = c_type.pointeeType();
                        try std.testing.expect(pointee_type != null);
                        try std.testing.expectEqual(c.CXType_Pointer, pointee_type.?.kind());

                        // the `Type *`
                        pointee_type = pointee_type.?.pointeeType();
                        const decl_cur = c_typeToCanonTypeDef(pointee_type.?);
                        const is_not_decl = decl_cur.isInvalid() or decl_cur.kind() == c.CXCursor_NoDeclFound;
                        // points to typedef declaration...
                        try std.testing.expect(!is_not_decl);
                        try std.testing.expectEqual(c.CXCursor_TypedefDecl, decl_cur.kind());
                    },
                    else => {},
                }
            },
            else => {},
        }
    }

    try std.testing.expectEqual(3, var_decl_cases);
    try std.testing.expectEqual(2, typedef_cases);
}
