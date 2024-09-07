const std = @import("std");
const sl = @import("source_logger.zig");

const log = sl.scoped(.simple_c_bindgen);

const common = @import("common.zig");
const clang = @import("clang.zig");
const t = @import("traversers.zig");
const c = struct {
    usingnamespace clang.c;
};

const BasicString = common.BasicString;

const Type = union(enum) {
    Primitive: PrimitiveTypeInfo,
    Compound: t.CompoundType,
    Enum: t.EnumType,
    Indirect: IndirectTypeInfo,

    const PrimitiveTypeInfo = struct {
        c_type: clang.Type,
        tag: t.TypeTag,
    };
    const IndirectTypeInfo = struct {
        cursor: ?clang.Cursor,
        tag: t.TypeTag,
        ref_type: TypeId,
    };
    const Self = @This();

    fn from(
        bindgen_info: *BindgenInfo,
        ty: clang.Type,
        owner: ?clang.Cursor,
    ) anyerror!Self {
        const tag = t.TypeTag.fromType(ty);
        if (tag.isPrimitive()) {
            return Self{
                .Primitive = .{ .c_type = ty, .tag = tag },
            };
        }

        if (tag == .Pointer) {
            return Self{
                .Indirect = .{
                    .tag = tag,
                    .cursor = owner,
                    .ref_type = try bindgen_info.getOrAddTyFromCTy(ty.pointeeType().?),
                },
            };
        } else if (tag == .Array) {
            return Self{
                .Indirect = .{
                    .tag = tag,
                    .cursor = owner,
                    .ref_type = try bindgen_info.getOrAddTyFromCTy(ty.elemType().?),
                },
            };
        }

        if (ty.kind() == c.CXType_Enum) {
            return Self{
                .Enum = try t.EnumType.fromType(bindgen_info.allocator, ty),
            };
        }

        return Self{
            .Compound = try t.CompoundType.fromType(
                bindgen_info.allocator,
                ty,
                owner.?,
            ),
        };
    }

    fn deinit(self: *Self) void {
        switch (self.*) {
            .Compound => |*info| info.deinit(),
            .Enum => |*info| info.deinit(),
            else => {},
        }
    }
};

const TypeHash = c_uint;
const TypeId = usize;
const BindgenInfo = struct {
    type_map: TypeMap,
    type_list: TypeList,
    allocator: std.mem.Allocator,
    tu: clang.TranslationUnit,
    next_id: TypeId,

    const TypeMap = std.AutoHashMap(TypeHash, TypeId);
    const TypeList = std.ArrayList(*Type);

    fn init(allocator: std.mem.Allocator, tu: clang.TranslationUnit) Self {
        return Self{
            .type_map = TypeMap.init(allocator),
            .type_list = TypeList.init(allocator),
            .allocator = allocator,
            .tu = tu,
            .next_id = 0,
        };
    }

    const Self = @This();

    fn getTypeHashFromCur(cur: clang.Cursor) TypeHash {
        const c_type = t.cursorToDeclType(cur);
        const type_tag = t.TypeTag.fromType(c_type);
        return switch (type_tag) {
            .Raw => (t.c_typeToCanonTypeDefFallible(c_type) orelse cur).hash(),
            // the cursor who's type is CXType_Pointer/Array.
            .Pointer, .Array => cur.hash(),
            else => if (type_tag.isPrimitive())
                @intFromEnum(type_tag)
            else
                @panic("TODO"),
        };
    }

    fn pushTy(self: *Self, hash_opt: ?TypeHash, ty: *Type) !TypeId {
        const id = self.next_id;
        try self.type_list.append(ty);
        if (hash_opt) |hash| try self.type_map.put(hash, id);
        self.next_id = self.type_list.items.len;
        return id;
    }

    fn getTy(self: Self, ty_id: TypeId) ?*Type {
        return if (ty_id >= self.type_list.items.len)
            null
        else
            self.type_list.items[ty_id];
    }

    fn getTyId(self: Self, ty_hash: TypeHash) ?TypeId {
        return self.type_map.get(ty_hash);
    }

    fn getTyFromCur(self: Self, cur: clang.Cursor) ?*Type {
        const hash = Self.getTypeHashFromCur(cur);
        const ty_id = self.getTyId(hash) orelse return null;
        return self.getTy(ty_id);
    }

    fn getTyFromCTy(
        self: Self,
        c_type: clang.Type,
        owner: ?clang.Cursor,
    ) ?*Type {
        const type_tag = t.TypeTag.fromType(c_type);
        if (type_tag.isPrimitive()) {
            const hash = @intFromEnum(type_tag);
            const id = self.getTyId(hash) orelse return null;
            return self.getTy(id);
        }
        const def = t.c_typeToCanonTypeDefFallible(c_type) orelse
            owner orelse return null;
        return self.getTyFromCur(def);
    }

    fn addTyFromCur(self: *Self, cur: clang.Cursor) !void {
        const hash = Self.getTypeHashFromCur(cur);
        if (self.type_map.contains(hash)) return;

        const ty = try self.allocator.create(Type);
        ty.* = try Type.from(self, t.cursorToDeclType(cur), cur);

        _ = try self.pushTy(hash, ty);
    }

    fn addTyFromCTy(self: *Self, c_type: clang.Type) !void {
        if (t.c_typeToCanonTypeDefFallible(c_type)) |cur| {
            return self.addTyFromCur(cur);
        }

        const type_tag = t.TypeTag.fromType(c_type);
        if (type_tag == .Raw) {
            @panic("Not sure how to handle it");
        }

        if (type_tag.isPrimitive()) {
            const hash = @intFromEnum(type_tag);
            if (self.type_map.contains(hash)) return;

            const ty = try self.allocator.create(Type);
            ty.* = try Type.from(self, c_type, null);
            _ = try self.pushTy(hash, ty);
        } else if (type_tag == .Pointer or type_tag == .Array) {
            const ty = try self.allocator.create(Type);
            ty.* = try Type.from(self, c_type, null);

            _ = try self.pushTy(null, ty);
        } else {
            @panic("Can't handle TODO?");
        }
    }

    fn getOrAddTyFromCur(self: *Self, cur: clang.Cursor) !TypeId {
        const hash = Self.getTypeHashFromCur(cur);
        if (self.type_map.get(hash)) |ty_id| return ty_id;

        const ty = try self.allocator.create(Type);
        ty.* = try Type.from(self, t.cursorToDeclType(cur), cur);

        return try self.pushTy(hash, ty);
    }

    fn getOrAddTyFromCTy(self: *Self, c_type: clang.Type) !TypeId {
        if (t.c_typeToCanonTypeDefFallible(c_type)) |cur| {
            return self.getOrAddTyFromCur(cur);
        }

        const type_tag = t.TypeTag.fromType(c_type);
        if (type_tag == .Raw) {
            @panic("Not sure how to handle it");
        }

        if (type_tag.isPrimitive()) {
            const hash = @intFromEnum(type_tag);
            return self.getTyId(hash) orelse blk: {
                const ty = try self.allocator.create(Type);
                ty.* = try Type.from(self, c_type, null);
                break :blk try self.pushTy(hash, ty);
            };
        }
        if (type_tag == .Pointer or type_tag == .Array) {
            const ty = try self.allocator.create(Type);
            ty.* = try Type.from(self, c_type, null);

            return try self.pushTy(null, ty);
        }
        @panic("Can't handle TODO?");
    }

    fn deinit(self: *Self) void {
        // translation_unit deinit?

        for (self.type_list.items) |ty| {
            ty.deinit();
            self.allocator.destroy(ty);
        }

        self.type_map.deinit();
        self.type_list.deinit();
    }
};

test "bindgen info parse" {
    const cpp_file_path = "./test-sources/test_simple_gen.h";

    const allocator = std.testing.allocator;

    var translation_unit = try t.createDummyTranslationUnit(
        allocator,
        cpp_file_path,
    );
    defer translation_unit.deinit();
    try translation_unit.printDiags();

    var root_cursor = translation_unit.cursor();

    const childs = try root_cursor.collectChildren(allocator);
    defer childs.deinit();
    try std.testing.expect(childs.items.len > 0);

    var bindgen_info = BindgenInfo.init(allocator, translation_unit);
    defer bindgen_info.deinit();

    var normal_cases: usize = 0;
    var typedef_cases: usize = 0;
    for (childs.items) |child| {
        if (child.isInSystemHeader()) continue;
        switch (child.kind()) {
            c.CXCursor_StructDecl,
            c.CXCursor_ClassDecl,
            c.CXCursor_EnumDecl,
            c.CXCursor_UnionDecl,
            => {
                const ty_id = try bindgen_info.getOrAddTyFromCur(child);
                const ty = bindgen_info.getTy(ty_id).?;

                normal_cases += 1;

                switch (normal_cases) {
                    1 => {
                        try std.testing.expect(ty.* == .Compound);

                        const name = try ty.Compound.name(allocator);
                        defer allocator.free(name);
                        try std.testing.expectEqualStrings("Base", name);
                        try std.testing.expectEqual(0, ty.Compound.constructors.items.len);

                        var func_cases: usize = 0;
                        for (ty.Compound.methods.items) |method| {
                            const func = try t.FunctionInfo.fromCur(method);
                            func_cases += 1;

                            var args = try func.args(allocator);
                            defer args.deinit();

                            switch (func_cases) {
                                5 => {
                                    try std.testing.expect(!func.isPureVirtual());
                                    try std.testing.expect(func.isVirtual());
                                },
                                6 => {
                                    try std.testing.expect(func.isPureVirtual());
                                    try std.testing.expect(func.isVirtual());
                                },
                                7 => {
                                    try std.testing.expect(args.items.len > 0);

                                    const param1 = t.DeclInfo.fromCur(args.items[0]);
                                    try std.testing.expect(param1.hasTemplateInstantiation());
                                    try std.testing.expectEqual(.Pointer, param1.ref_type);
                                    const param1_ref_pointee_type = param1.ref_c_type.pointeeType();
                                    try std.testing.expect(param1_ref_pointee_type != null);
                                    const param1_ty = bindgen_info.getTyFromCTy(param1_ref_pointee_type.?, null);
                                    try std.testing.expect(param1_ty != null);
                                },
                                else => {},
                            }

                            for (args.items) |param| {
                                const param_info = t.DeclInfo.fromCur(param);
                                try bindgen_info.addTyFromCTy(param_info.ref_c_type);
                            }
                        }

                        try std.testing.expect(ty.Compound.fields.items.len > 0);
                        var field_cases: usize = 0;
                        for (ty.Compound.fields.items) |field_cur| {
                            const field = t.DeclInfo.fromCur(field_cur);
                            const field_name = try field.name(allocator);
                            defer allocator.free(field_name);

                            field_cases += 1;

                            switch (field_cases) {
                                1 => {
                                    try std.testing.expect(field.hasTemplateInstantiation());
                                    try std.testing.expectEqualStrings("outer_int_vec1_", field_name);
                                    const field_ty = bindgen_info.getTyFromCTy(field.ref_c_type, null);
                                    try std.testing.expect(field_ty != null);

                                    const field_c_ty = field_ty.?.Compound.cursor.c_type();
                                    try std.testing.expect(field_c_ty.c_size() > 0);
                                    try std.testing.expect(field_c_ty.c_align() > 0);

                                    const field_c_ty_spelling = try field_c_ty.spellingFull(allocator);
                                    defer allocator.free(field_c_ty_spelling);
                                    try std.testing.expectEqualStrings("std::vector<int>", field_c_ty_spelling);
                                },
                                else => {},
                            }
                        }
                    },
                    2 => {
                        try std.testing.expect(ty.* == .Compound);

                        const name = try ty.Compound.name(allocator);
                        defer allocator.free(name);
                        try std.testing.expectEqualStrings("Child", name);

                        try std.testing.expectEqual(1, ty.Compound.base_specifiers.items.len);
                        try std.testing.expectEqual(1, ty.Compound.constructors.items.len);

                        const bs_cur = ty.Compound.base_specifiers.items[0];
                        const base_ty = bindgen_info.getTyFromCur(bs_cur);
                        try std.testing.expect(base_ty != null);
                        try std.testing.expect(base_ty.?.* == .Compound);
                        try std.testing.expect(base_ty.?.Compound.methods.items.len > 0);

                        const base_name = try base_ty.?.Compound.name(allocator);
                        defer allocator.free(base_name);
                        try std.testing.expectEqualStrings("Base", base_name);

                        var func_cases: usize = 0;
                        for (ty.Compound.methods.items) |method| {
                            const func = try t.FunctionInfo.fromCur(method);
                            const func_name = try func.name(allocator);
                            defer allocator.free(func_name);
                            func_cases += 1;

                            switch (func_cases) {
                                1 => {
                                    try std.testing.expectEqual(.Float, func.ret_type);
                                    try std.testing.expectEqualStrings("GetInnerFloat1", func_name);
                                },
                                2 => {
                                    try std.testing.expectEqual(.Float, func.ret_type);
                                    try std.testing.expectEqualStrings("GetInnerFloat1", func_name);
                                },
                                else => {},
                            }
                        }
                    },
                    else => {},
                }
            },
            c.CXCursor_TypedefDecl,
            c.CXCursor_TypeAliasDecl,
            => {
                typedef_cases += 1;

                switch (typedef_cases) {
                    1 => {
                        try bindgen_info.addTyFromCur(child);

                        const child_c_type = t.cursorToDeclType(child);
                        const comp_ty_info = try t.CompoundType.fromType(
                            allocator,
                            child_c_type,
                            child,
                        );

                        const decl_name = try comp_ty_info.name(allocator);
                        defer allocator.free(decl_name);
                        try std.testing.expectEqualStrings("vector", decl_name);

                        // typically if we really wanted to parse the Class Template Definition..
                        const visitor = struct {
                            fn func(ret: *clang.Cursor, current: clang.Cursor, _: clang.Cursor) !void {
                                if (current.kind() == c.CXCursor_TemplateRef) {
                                    ret.* = current;
                                    return error.ForcedBreak;
                                }
                            }
                        }.func;

                        var ref_cur: clang.Cursor = undefined;
                        try std.testing.expect(!child.visit(clang.Cursor, visitor, &ref_cur));
                        const ref_cur_decl = ref_cur.referenced();
                        try std.testing.expect(ref_cur_decl != null);

                        try std.testing.expect(t.DeclInfo.fromCur(child).hasTemplateInstantiation());

                        try std.testing.expect(child_c_type.c_size() > 0);
                        try std.testing.expect(child_c_type.c_align() > 0);
                    },
                    else => {},
                }
            },
            else => {},
        }
    }
}
