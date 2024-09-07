const std = @import("std");
const sl = @import("source_logger.zig");

pub const c = @cImport({
    @cInclude("clang-c/Index.h");
    @cInclude("stdio.h");
});

const log = sl.scoped(.clang);

pub fn checkRet(code: anytype) error{ClangErrorCode}!void {
    const RetType = @TypeOf(code);
    const ret_type_info = @typeInfo(RetType);
    if (ret_type_info != .int) @compileError("Expected int like argument.");
    if (code == 0) return;
    log.err("Error code: {}", .{code});
    return error.ClangErrorCode;
}

pub fn checkRetWithMsg(
    code: anytype,
    comptime msg: []const u8,
) error{ClangErrorCode}!void {
    const RetType = @TypeOf(code);
    const ret_type_info = @typeInfo(RetType);
    if (ret_type_info != .int) @compileError("Expected int like argument.");
    if (code == 0) return;
    log.err("{s} Error code: {}", .{ msg, code });
    return error.ClangErrorCode;
}

pub fn copyCXStringAndDispose(
    allocator: std.mem.Allocator,
    string: c.CXString,
) ![]const u8 {
    const c_str = c.clang_getCString(string);
    defer c.clang_disposeString(string);
    return std.fmt.allocPrint(allocator, "{s}", .{c_str});
}

pub fn ReverseCursorIterator(
    comptime getParent: fn (c.CXCursor) callconv(.C) c.CXCursor,
) type {
    return struct {
        current: c.CXCursor,
        reached_end: bool = false,

        fn init(current: c.CXCursor) @This() {
            return .{ .current = current };
        }

        pub fn next(self: *@This()) ?c.CXCursor {
            if (self.reached_end) return null;

            const parent = getParent(self.current);
            const parent_kind = c.clang_getCursorKind(parent);

            if (parent_kind == c.CXCursor_TranslationUnit) {
                self.reached_end = true;
                return null;
            }

            self.current = parent;
            return self.current;
        }

        pub fn nextTillTU(self: *@This()) ?c.CXCursor {
            if (self.reached_end) return null;

            const parent = getParent(self.current);
            const parent_kind = c.clang_getCursorKind(parent);

            self.reached_end = parent_kind == c.CXCursor_TranslationUnit;

            self.current = parent;
            return self.current;
        }
    };
}

pub const CursorChildVisitError = error{
    ContinueAsRecurse,
    ForcedBreak,
    TraverseError,
};

fn C_CursorChildVisitorWrapper(
    comptime T: type,
    comptime visitor: fn (*T, Cursor, Cursor) anyerror!void,
) type {
    return struct {
        fn c_func(
            current: c.CXCursor,
            parent: c.CXCursor,
            user_data: ?*anyopaque,
        ) callconv(.C) c.CXChildVisitResult {
            const data = user_data orelse @panic("Shouldn't receive null context!");
            const context: *T = @ptrCast(@alignCast(data));

            visitor(
                context,
                Cursor{ .native = current },
                Cursor{ .native = parent },
            ) catch |err| {
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
    comptime visitor: fn (*T, Cursor, Cursor) anyerror!void,
) @TypeOf(C_CursorChildVisitorWrapper(T, visitor).c_func) {
    return C_CursorChildVisitorWrapper(T, visitor).c_func;
}

inline fn validOrNull(value: anytype) ?@TypeOf(value) {
    return if (value.isInvalid()) null else value;
}

pub fn isValidIdent(str: []const u8) bool {
    const is_first_valid = std.ascii.isAlphabetic(str[0]) or str[0] == '_';
    return is_first_valid and blk: {
        for (str) |ch| {
            if (!std.ascii.isAlphanumeric(ch) and ch != '_') break :blk false;
        }
        break :blk true;
    };
}

pub const String = struct {
    native: c.CXString,

    const Self = @This();

    pub fn asBytes(self: Self) []const u8 {
        return std.mem.span(c.clang_getCString(self.native));
    }

    pub fn toBytes(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        const c_str = c.clang_getCString(self.native);
        return std.fmt.allocPrint(allocator, "{s}", .{c_str});
    }

    pub fn deinit(self: Self) void {
        c.clang_disposeString(self.native);
    }
};

pub const Cursor = struct {
    native: c.CXCursor,

    const Self = @This();
    const KindType = @TypeOf(c.CXCursor_ClassDecl);

    pub fn isInvalid(self: Self) bool {
        return self.isNull() or c.clang_isInvalid(@intCast(self.kind())) != 0;
    }

    pub fn isNull(self: Self) bool {
        return c.clang_Cursor_isNull(self.native) != 0;
    }

    pub fn isDeclaration(self: Self) bool {
        return c.clang_isDeclaration(@intCast(self.kind())) != 0;
    }

    pub fn isDefinition(self: Self) bool {
        return c.clang_isCursorDefinition(self.native) != 0;
    }

    pub fn isInvalidDeclaration(self: Self) bool {
        return c.clang_isInvalidDeclaration(self.native) != 0;
    }

    pub fn isReference(self: Self) bool {
        return c.clang_isReference(@intCast(self.kind())) != 0;
    }

    pub fn isAnonymous(self: Self) bool {
        return c.clang_Cursor_isAnonymous(self.native) != 0;
    }

    pub fn isMethodVariadic(self: Self) bool {
        return c.clang_Cursor_isVariadic(self.native) != 0;
    }

    pub fn isConvertingConstructor(self: Self) bool {
        return c.clang_CXXConstructor_isConvertingConstructor(self.native) != 0;
    }

    pub fn isCopyConstructor(self: Self) bool {
        return c.clang_CXXConstructor_isCopyConstructor(self.native) != 0;
    }

    pub fn isDefaultConstructor(self: Self) bool {
        return c.clang_CXXConstructor_isDefaultConstructor(self.native) != 0;
    }

    pub fn isMoveConstructor(self: Self) bool {
        return c.clang_CXXConstructor_isMoveConstructor(self.native) != 0;
    }

    pub fn isMethodVirtual(self: Self) bool {
        return c.clang_CXXMethod_isVirtual(self.native) != 0;
    }

    pub fn isMethodPureVirtual(self: Self) bool {
        return c.clang_CXXMethod_isPureVirtual(self.native) != 0;
    }

    pub fn isMethodConst(self: Self) bool {
        return c.clang_CXXMethod_isConst(self.native) != 0;
    }

    pub fn isMethodDefaulted(self: Self) bool {
        return c.clang_CXXMethod_isDefaulted(self.native) != 0;
    }

    pub fn isMethodDeleted(self: Self) bool {
        return c.clang_CXXMethod_isDeleted(self.native) != 0;
    }

    pub fn isMethodStatic(self: Self) bool {
        return c.clang_CXXMethod_isStatic(self.native) != 0;
    }

    pub fn isMethodOperator(self: Self) bool {
        if (self.kind() != c.CXCursor_CXXMethod) return false;
        if (c.clang_CXXMethod_isCopyAssignmentOperator(self.native) != 0) return true;
        if (c.clang_CXXMethod_isMoveAssignmentOperator(self.native) != 0) return true;

        const spelling_raw = self.spellingRaw();
        defer spelling_raw.deinit();

        return std.mem.startsWith(u8, spelling_raw.asBytes(), "operator") and
            !isValidIdent(spelling_raw.asBytes());
    }

    pub fn isInSystemHeader(self: Self) bool {
        return c.clang_Location_isInSystemHeader(
            c.clang_getCursorLocation(self.native),
        ) != 0;
    }

    pub fn isTopLevel(self: Self) bool {
        var semantic_parent = self.fallibleSemanticParent();

        while (semantic_parent != null and
            (semantic_parent.?.kind() == c.CXCursor_Namespace or
            semantic_parent.?.kind() == c.CXCursor_NamespaceAlias or
            semantic_parent.?.kind() == c.CXCursor_NamespaceRef))
        {
            semantic_parent = semantic_parent.?.fallibleSemanticParent();
        }

        const tu_sm_par = self.translationUnit().cursor().fallibleSemanticParent();
        if (semantic_parent) |sm_p| {
            if (tu_sm_par) |tu_sm_p| {
                return sm_p.equal(tu_sm_p);
            }
            return false;
        }
        return tu_sm_par == null;
    }

    pub fn isTemplateLike(self: Self) bool {
        return switch (self.kind()) {
            c.CXCursor_ClassTemplate,
            c.CXCursor_ClassTemplatePartialSpecialization,
            c.CXCursor_TypeAliasTemplateDecl,
            => true,
            else => false,
        };
    }

    pub fn isPubliclyAccessible(self: Self) bool {
        const accessible = self.accessSpecifier();
        return accessible == c.CX_CXXPublic or
            accessible == c.CX_CXXInvalidAccessSpecifier;
    }

    pub fn translationUnit(self: Self) TranslationUnit {
        const tu = c.clang_Cursor_getTranslationUnit(self.native);
        return TranslationUnit.init_with_native(tu, null);
    }

    pub fn equal(self: Self, other: Self) bool {
        return c.clang_equalCursors(self.native, other.native) == 1;
    }

    pub fn kind(self: Self) KindType {
        return @intCast(c.clang_getCursorKind(self.native));
    }

    pub fn templateKind(self: Self) KindType {
        return @intCast(c.clang_getTemplateCursorKind(self.native));
    }

    pub fn spellingRaw(self: Self) String {
        return String{ .native = c.clang_getCursorSpelling(self.native) };
    }

    pub fn spelling(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        return copyCXStringAndDispose(allocator, c.clang_getCursorSpelling(self.native));
    }

    pub fn lexicalParent(self: Self) Self {
        return Self{ .native = c.clang_getCursorLexicalParent(self.native) };
    }

    pub fn fallibleSemanticParent(self: Self) ?Self {
        const parent = Self{ .native = c.clang_getCursorSemanticParent(self.native) };

        return validOrNull(parent);
    }

    pub fn semanticParent(self: Self) Self {
        return self.fallibleSemanticParent().?;
    }

    pub fn varInitializer(self: Self) ?Self {
        const cursor = Self{ .native = c.clang_Cursor_getVarDeclInitializer(self.native) };

        return validOrNull(cursor);
    }

    pub fn referenced(self: Self) ?Self {
        const cursor = Self{ .native = c.clang_getCursorReferenced(self.native) };

        return validOrNull(cursor);
    }

    pub fn definition(self: Self) ?Self {
        const cursor = Self{ .native = c.clang_getCursorDefinition(self.native) };

        return if (cursor.isInvalid() or cursor.kind() == c.CXCursor_NoDeclFound)
            null
        else
            cursor;
    }

    pub fn canonical(self: Self) Self {
        const cursor = Self{ .native = c.clang_getCanonicalCursor(self.native) };

        return cursor;
    }

    pub fn specialized(self: Self) ?Self {
        const cursor = Self{ .native = c.clang_getSpecializedCursorTemplate(self.native) };

        return validOrNull(cursor);
    }

    pub fn typedefType(self: Self) ?Type {
        const inner = Type{ .native = c.clang_getTypedefDeclUnderlyingType(self.native) };

        return validOrNull(inner);
    }

    pub fn c_type(self: Self) Type {
        return Type{ .native = c.clang_getCursorType(self.native) };
    }

    pub fn enumType(self: Self) ?Type {
        const inner = Type{ .native = c.clang_getEnumDeclIntegerType(self.native) };

        return validOrNull(inner);
    }

    pub fn enumValue(self: Self) ?i64 {
        return if (self.kind() == c.CXCursor_EnumConstantDecl)
            @intCast(c.clang_getEnumConstantDeclValue(self.native))
        else
            null;
    }

    pub fn enumUnsignedValue(self: Self) ?u64 {
        return if (self.kind() == c.CXCursor_EnumConstantDecl)
            @intCast(c.clang_getEnumConstantDeclUnsignedValue(self.native))
        else
            null;
    }

    pub fn accessSpecifier(self: Self) c.CX_CXXAccessSpecifier {
        return c.clang_getCXXAccessSpecifier(self.native);
    }

    pub fn hash(self: Self) c_uint {
        return c.clang_hashCursor(self.native);
    }

    pub fn argumentsNum(self: Self) ?usize {
        const n = c.clang_Cursor_getNumArguments(self.native);

        return if (n < 0) null else @intCast(n);
    }

    const ArgumentList = std.ArrayList(Self);
    pub fn arguments(self: Self, allocator: std.mem.Allocator) ?ArgumentList {
        const n = self.argumentsNum() orelse return null;
        var list = ArgumentList.init(allocator);
        for (0..@intCast(n)) |i| {
            list.append(Self{
                .native = c.clang_Cursor_getArgument(self.native, @intCast(i)),
            }) catch return null;
        }

        return list;
    }

    const ReverseSemanticIterator = ReverseCursorIterator(c.clang_getCursorSemanticParent);
    pub fn reverseSemanticIterator(self: Self) ReverseSemanticIterator {
        return ReverseSemanticIterator.init(self.native);
    }

    const ReverseLexicalIterator = ReverseCursorIterator(c.clang_getCursorLexicalParent);
    pub fn reverseLexicalIterator(self: Self) ReverseLexicalIterator {
        return ReverseLexicalIterator.init(self.native);
    }

    pub fn visit(
        self: Self,
        comptime T: type,
        comptime visitor: fn (*T, Self, Self) anyerror!void,
        user_data: *T,
    ) bool {
        return c.clang_visitChildren(
            self.native,
            ToCursorChildVisitorT(T, visitor),
            @as(*anyopaque, user_data),
        ) == 0;
    }

    const SelfArrayList = std.ArrayList(Self);
    pub fn collectChildren(self: Self, allocator: std.mem.Allocator) !SelfArrayList {
        var list = SelfArrayList.init(allocator);
        const collect_fn = struct {
            fn func(ret_list: *SelfArrayList, current: Cursor, _: Cursor) !void {
                try ret_list.append(current);
            }
        }.func;

        if (!self.visit(SelfArrayList, collect_fn, &list)) {
            return error.TraverseError;
        }
        return list;
    }

    pub fn hasChild(self: Self) bool {
        const checker_fn = struct {
            fn func(ret: *bool, _: Cursor, _: Cursor) !void {
                ret.* = true;
                return error.ForcedBreak;
            }
        }.func;

        var found = false;
        _ = self.visit(bool, checker_fn, &found);
        return found;
    }
};

pub const CursorContext = struct {
    pub fn hash(self: @This(), cursor: Cursor) u64 {
        _ = self;
        return @intCast(cursor.hash());
    }
    pub fn eql(self: @This(), a: Cursor, b: Cursor) bool {
        _ = self;
        return a.equal(b);
    }
};

pub fn CursorHashMap(comptime V: type) type {
    return std.HashMap(Cursor, V, CursorContext, std.hash_map.default_max_load_percentage);
}

pub const Type = struct {
    native: c.CXType,

    const KindType = @TypeOf(c.CXType_Record);
    const Self = @This();

    pub fn isInvalid(self: Self) bool {
        return self.kind() == c.CXType_Invalid;
    }

    pub fn isConst(self: Self) bool {
        return c.clang_isConstQualifiedType(self.native) != 0;
    }

    pub fn isROrLValueRef(self: Self) bool {
        return switch (self.kind()) {
            c.CXType_RValueReference, c.CXType_LValueReference => true,
            else => false,
        };
    }

    pub fn isNonDeducibleAutoType(self: Self) bool {
        std.debug.assert(self.kind() == c.CXType_Auto);
        return self.canonicalType().equal(self);
    }

    pub fn equal(self: Self, other: Self) bool {
        return c.clang_equalTypes(self.native, other.native) != 0;
    }

    pub fn kind(self: Self) KindType {
        return @intCast(self.native.kind);
    }

    pub fn spellingFull(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        return copyCXStringAndDispose(allocator, c.clang_getTypeSpelling(self.native));
    }

    pub fn spellingRaw(self: Self) String {
        return String{ .native = c.clang_getTypeSpelling(self.native) };
    }

    pub fn spelling(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        // TODO: Remove double allocation
        const str = try copyCXStringAndDispose(allocator, c.clang_getTypeSpelling(self.native));
        defer allocator.free(str);
        var split = std.mem.splitBackwardsSequence(u8, str, "::");
        return allocator.dupe(u8, split.first());
    }

    pub fn typedefName(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        return copyCXStringAndDispose(allocator, c.clang_getTypedefName(self.native));
    }

    pub fn c_size(self: Self) isize {
        return @intCast(c.clang_Type_getSizeOf(self.native));
    }

    pub fn c_align(self: Self) isize {
        return @intCast(c.clang_Type_getAlignOf(self.native));
    }

    pub fn pointeeType(self: Self) ?Self {
        switch (self.kind()) {
            c.CXType_Pointer,
            c.CXType_RValueReference,
            c.CXType_LValueReference,
            c.CXType_MemberPointer,
            c.CXType_BlockPointer,
            c.CXType_ObjCObjectPointer,
            => {
                const inner = Self{ .native = c.clang_getPointeeType(self.native) };

                return validOrNull(inner);
            },
            else => {
                return null;
            },
        }
    }

    pub fn elemType(self: Self) ?Self {
        const inner = Self{ .native = c.clang_getElementType(self.native) };

        return validOrNull(inner);
    }

    pub fn retType(self: Self) ?Self {
        const inner = Self{ .native = c.clang_getResultType(self.native) };

        return validOrNull(inner);
    }

    pub fn canonicalType(self: Self) Self {
        return Self{ .native = c.clang_getCanonicalType(self.native) };
    }

    pub fn namedType(self: Self) Self {
        return Self{ .native = c.clang_Type_getNamedType(self.native) };
    }

    pub fn argumentsNum(self: Self) ?usize {
        const n = c.clang_getNumArgTypes(self.native);

        return if (n < 0) return null else @intCast(n);
    }

    const ArgumentList = std.ArrayList(Self);
    pub fn arguments(self: Self, allocator: std.mem.Allocator) ?ArgumentList {
        const n = self.argumentsNum() orelse return null;
        var list = ArgumentList.init(allocator);
        for (0..@intCast(n)) |i| {
            list.append(Self{
                .native = c.clang_getArgType(self.native, @intCast(i)),
            }) catch return null;
        }

        return list;
    }

    pub fn declaration(self: Self) Cursor {
        return Cursor{ .native = c.clang_getTypeDeclaration(self.native) };
    }

    pub fn canonicalDeclaration(self: Self, loc_cur_opt: ?Cursor) ?Cursor {
        var cur_decleration = self.declaration();
        if (cur_decleration.isInvalid()) {
            if (loc_cur_opt) |loc_cur| {
                const location = loc_cur.referenced() orelse loc_cur;
                if (location.isTemplateLike()) {
                    cur_decleration = location;
                }
            }
        }

        const cur_canonical = cur_decleration.canonical();
        if (!cur_canonical.isInvalid() and cur_canonical.kind() != c.CXCursor_NoDeclFound) {
            return cur_canonical;
        } else {
            return null;
        }
    }
};

pub const TranslationUnitOptions = struct {
    args: [][*:0]const u8,
    unsaved_files: ?[]c.CXUnsavedFile = null,
    record_detailed_preproessing: bool = true,
    skip_function_bodies: bool = false,
    exclude_pch: bool = false,
    display_diag: bool = false,
    // TODO: add more options related to how the parsing goes, CXIndexOptions,

    const Self = @This();

    fn createCXIndex(self: Self) c.CXIndex {
        return c.clang_createIndex(
            @intFromBool(self.exclude_pch),
            @intFromBool(self.display_diag),
        );
    }

    fn parseOptions(self: Self) c_uint {
        var parse_options: c_uint = 0;
        if (self.record_detailed_preproessing) {
            parse_options |= c.CXTranslationUnit_DetailedPreprocessingRecord;
        }
        if (self.skip_function_bodies) {
            parse_options |= c.CXTranslationUnit_SkipFunctionBodies;
        }

        return parse_options;
    }

    fn unsavedFilesData(self: Self) struct { [*c]c.CXUnsavedFile, c_uint } {
        var unsaved_files_ptr: [*c]c.CXUnsavedFile = undefined;
        var unsaved_files_len: c_uint = 0;
        if (self.unsaved_files) |uf| {
            unsaved_files_ptr = uf.ptr;
            unsaved_files_len = @intCast(uf.len);
        }

        return .{ unsaved_files_ptr, unsaved_files_len };
    }
};

pub const TranslationUnit = struct {
    index: ?c.CXIndex,
    native: c.CXTranslationUnit,
    pointer_width: usize,

    const Self = @This();
    // TODO: Options?
    pub fn parse(opt: TranslationUnitOptions) !Self {
        const index = opt.createCXIndex();

        const unsaved_files = opt.unsavedFilesData();

        var translation_unit: c.CXTranslationUnit = undefined;
        try checkRetWithMsg(c.clang_parseTranslationUnit2(
            index,
            null,
            opt.args.ptr,
            @intCast(opt.args.len),
            unsaved_files[0],
            unsaved_files[1],
            opt.parseOptions(),
            &translation_unit,
        ), "Parsing translation unit failed...");

        return init_with_native(translation_unit, index);
    }

    fn init_with_native(translation_unit: c.CXTranslationUnit, idx: ?c.CXIndex) Self {
        const ti = c.clang_getTranslationUnitTargetInfo(translation_unit);
        defer c.clang_TargetInfo_dispose(ti);
        const pointer_width = c.clang_TargetInfo_getPointerWidth(ti);

        return Self{
            .index = idx,
            .native = translation_unit,
            .pointer_width = @intCast(pointer_width),
        };
    }

    pub fn printDiags(self: Self) !void {
        var err: c_int = 0;
        const num_diagnostics = c.clang_getNumDiagnostics(self.native);
        if (num_diagnostics > 0) {
            log.warn("\nClang says:", .{});
            for (0..num_diagnostics) |i| {
                const diag = c.clang_getDiagnostic(self.native, @intCast(i));
                const severity = c.clang_getDiagnosticSeverity(diag);

                if (severity == c.CXDiagnostic_Fatal or severity == c.CXDiagnostic_Error) {
                    err = 1;
                }

                const @"struct" = c.clang_formatDiagnostic(diag, c.clang_defaultDiagnosticDisplayOptions());
                std.debug.print("{s}\n", .{c.clang_getCString(@"struct")});
                c.clang_disposeString(@"struct");
            }
        }

        try checkRet(err);
    }

    pub fn cursor(self: Self) Cursor {
        return Cursor{ .native = c.clang_getTranslationUnitCursor(self.native) };
    }

    pub fn spelling(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        return copyCXStringAndDispose(allocator, c.clang_getTranslationUnitSpelling(self.native));
    }

    fn clangTypeSize(self: Self, ty: Type) isize {
        return switch (ty.kind()) {
            c.CXType_RValueReference, c.CXType_LValueReference => @intCast(self.pointer_width / 8),
            c.CXType_Auto => if (ty.isNonDeducibleAutoType()) -6 else ty.c_size(),
            else => ty.c_size(),
        };
    }

    fn clangTypeAlign(self: Self, ty: Type) isize {
        return switch (ty.kind()) {
            c.CXType_RValueReference, c.CXType_LValueReference => @intCast(self.pointer_width / 8),
            c.CXType_Auto => if (ty.isNonDeducibleAutoType()) -6 else ty.c_align(),
            else => ty.c_align(),
        };
    }

    pub fn typeSize(self: Self, ty: Type) usize {
        const val = self.clangTypeSize(ty);
        return if (val < 0) 0 else @intCast(val);
    }

    pub fn typeAlign(self: Self, ty: Type) usize {
        const val = self.clangTypeAlign(ty);
        return if (val < 0) 0 else @intCast(val);
    }

    pub fn triple(self: Self, allocator: std.mem.Allocator) ![]const u8 {
        const ti = c.clang_getTranslationUnitTargetInfo(self.native);
        defer c.clang_TargetInfo_dispose(ti);

        return copyCXStringAndDispose(allocator, c.clang_TargetInfo_getTriple(ti));
    }

    pub fn deinit(self: *Self) void {
        c.clang_disposeTranslationUnit(self.native);
        if (self.index) |idx| {
            c.clang_disposeIndex(idx);
        }
    }
};
