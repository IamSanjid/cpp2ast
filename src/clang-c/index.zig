pub const IndexOptions = struct {
    pub const Size: usize = 24;
    pub const Align: usize = 8;
    const Native = extern struct { __opaque: [Size]u8 align(Align) = @import("std").mem.zeroes([Size]u8) };
    ThreadBackgroundPriorityForIndexing: u8 = 0,
    ThreadBackgroundPriorityForEditing: u8 = 0,
    ExcludeDeclarationsFromPCH: bool = false,
    DisplayDiagnostics: bool = false,
    StorePreamblesInMemory: bool = false,
    PreambleStoragePath: [*c]u8 = @import("std").mem.zeroes([*c]u8),
    InvocationEmissionPath: [*c]u8 = @import("std").mem.zeroes([*c]u8),
    pub fn native(self: @This()) Native {
        var x = Native{};
        set_Size(&x, @This().Size);
        set_ThreadBackgroundPriorityForIndexing(&x, self.ThreadBackgroundPriorityForIndexing);
        set_ThreadBackgroundPriorityForEditing(&x, self.ThreadBackgroundPriorityForEditing);
        set_ExcludeDeclarationsFromPCH(&x, self.ExcludeDeclarationsFromPCH);
        set_DisplayDiagnostics(&x, self.DisplayDiagnostics);
        set_StorePreamblesInMemory(&x, self.StorePreamblesInMemory);
        set_PreambleStoragePath(&x, self.PreambleStoragePath);
        set_InvocationEmissionPath(&x, self.InvocationEmissionPath);
        return x;
    }
    inline fn set_Size(x: *Native, value: c_uint) void {
        return c.CXIndexOptions_set_Size(@as(*anyopaque, x), value);
    }
    inline fn set_ThreadBackgroundPriorityForIndexing(x: *Native, value: u8) void {
        return c.CXIndexOptions_set_ThreadBackgroundPriorityForIndexing(@as(*anyopaque, x), value);
    }
    inline fn set_ThreadBackgroundPriorityForEditing(x: *Native, value: u8) void {
        return c.CXIndexOptions_set_ThreadBackgroundPriorityForEditing(@as(*anyopaque, x), value);
    }
    inline fn set_ExcludeDeclarationsFromPCH(x: *Native, value: bool) void {
        return c.CXIndexOptions_set_ExcludeDeclarationsFromPCH(@as(*anyopaque, x), @intFromBool(value));
    }
    inline fn set_DisplayDiagnostics(x: *Native, value: bool) void {
        return c.CXIndexOptions_set_DisplayDiagnostics(@as(*anyopaque, x), @intFromBool(value));
    }
    inline fn set_StorePreamblesInMemory(x: *Native, value: bool) void {
        return c.CXIndexOptions_set_StorePreamblesInMemory(@as(*anyopaque, x), @intFromBool(value));
    }
    inline fn set_PreambleStoragePath(x: *Native, value: [*c]u8) void {
        return c.CXIndexOptions_set_PreambleStoragePath(@as(*anyopaque, x), value);
    }
    inline fn set_InvocationEmissionPath(x: *Native, value: [*c]u8) void {
        return c.CXIndexOptions_set_InvocationEmissionPath(@as(*anyopaque, x), value);
    }
};
pub const CursorKind = enum(c_uint) {
    UnexposedDecl = 1,
    StructDecl = 2,
    UnionDecl = 3,
    ClassDecl = 4,
    EnumDecl = 5,
    FieldDecl = 6,
    EnumConstantDecl = 7,
    FunctionDecl = 8,
    VarDecl = 9,
    ParmDecl = 10,
    ObjCInterfaceDecl = 11,
    ObjCCategoryDecl = 12,
    ObjCProtocolDecl = 13,
    ObjCPropertyDecl = 14,
    ObjCIvarDecl = 15,
    ObjCInstanceMethodDecl = 16,
    ObjCClassMethodDecl = 17,
    ObjCImplementationDecl = 18,
    ObjCCategoryImplDecl = 19,
    TypedefDecl = 20,
    CXXMethod = 21,
    Namespace = 22,
    LinkageSpec = 23,
    Constructor = 24,
    Destructor = 25,
    ConversionFunction = 26,
    TemplateTypeParameter = 27,
    NonTypeTemplateParameter = 28,
    TemplateTemplateParameter = 29,
    FunctionTemplate = 30,
    ClassTemplate = 31,
    ClassTemplatePartialSpecialization = 32,
    NamespaceAlias = 33,
    UsingDirective = 34,
    UsingDeclaration = 35,
    TypeAliasDecl = 36,
    ObjCSynthesizeDecl = 37,
    ObjCDynamicDecl = 38,
    CXXAccessSpecifier = 39,
    ObjCSuperClassRef = 40,
    ObjCProtocolRef = 41,
    ObjCClassRef = 42,
    TypeRef = 43,
    CXXBaseSpecifier = 44,
    TemplateRef = 45,
    NamespaceRef = 46,
    MemberRef = 47,
    LabelRef = 48,
    OverloadedDeclRef = 49,
    VariableRef = 50,
    InvalidFile = 70,
    NoDeclFound = 71,
    NotImplemented = 72,
    InvalidCode = 73,
    UnexposedExpr = 100,
    DeclRefExpr = 101,
    MemberRefExpr = 102,
    CallExpr = 103,
    ObjCMessageExpr = 104,
    BlockExpr = 105,
    IntegerLiteral = 106,
    FloatingLiteral = 107,
    ImaginaryLiteral = 108,
    StringLiteral = 109,
    CharacterLiteral = 110,
    ParenExpr = 111,
    UnaryOperator = 112,
    ArraySubscriptExpr = 113,
    BinaryOperator = 114,
    CompoundAssignOperator = 115,
    ConditionalOperator = 116,
    CStyleCastExpr = 117,
    CompoundLiteralExpr = 118,
    InitListExpr = 119,
    AddrLabelExpr = 120,
    StmtExpr = 121,
    GenericSelectionExpr = 122,
    GNUNullExpr = 123,
    CXXStaticCastExpr = 124,
    CXXDynamicCastExpr = 125,
    CXXReinterpretCastExpr = 126,
    CXXConstCastExpr = 127,
    CXXFunctionalCastExpr = 128,
    CXXTypeidExpr = 129,
    CXXBoolLiteralExpr = 130,
    CXXNullPtrLiteralExpr = 131,
    CXXThisExpr = 132,
    CXXThrowExpr = 133,
    CXXNewExpr = 134,
    CXXDeleteExpr = 135,
    UnaryExpr = 136,
    ObjCStringLiteral = 137,
    ObjCEncodeExpr = 138,
    ObjCSelectorExpr = 139,
    ObjCProtocolExpr = 140,
    ObjCBridgedCastExpr = 141,
    PackExpansionExpr = 142,
    SizeOfPackExpr = 143,
    LambdaExpr = 144,
    ObjCBoolLiteralExpr = 145,
    ObjCSelfExpr = 146,
    OMPArraySectionExpr = 147,
    ObjCAvailabilityCheckExpr = 148,
    FixedPointLiteral = 149,
    OMPArrayShapingExpr = 150,
    OMPIteratorExpr = 151,
    CXXAddrspaceCastExpr = 152,
    ConceptSpecializationExpr = 153,
    RequiresExpr = 154,
    CXXParenListInitExpr = 155,
    UnexposedStmt = 200,
    LabelStmt = 201,
    CompoundStmt = 202,
    CaseStmt = 203,
    DefaultStmt = 204,
    IfStmt = 205,
    SwitchStmt = 206,
    WhileStmt = 207,
    DoStmt = 208,
    ForStmt = 209,
    GotoStmt = 210,
    IndirectGotoStmt = 211,
    ContinueStmt = 212,
    BreakStmt = 213,
    ReturnStmt = 214,
    GCCAsmStmt = 215,
    ObjCAtTryStmt = 216,
    ObjCAtCatchStmt = 217,
    ObjCAtFinallyStmt = 218,
    ObjCAtThrowStmt = 219,
    ObjCAtSynchronizedStmt = 220,
    ObjCAutoreleasePoolStmt = 221,
    ObjCForCollectionStmt = 222,
    CXXCatchStmt = 223,
    CXXTryStmt = 224,
    CXXForRangeStmt = 225,
    SEHTryStmt = 226,
    SEHExceptStmt = 227,
    SEHFinallyStmt = 228,
    MSAsmStmt = 229,
    NullStmt = 230,
    DeclStmt = 231,
    OMPParallelDirective = 232,
    OMPSimdDirective = 233,
    OMPForDirective = 234,
    OMPSectionsDirective = 235,
    OMPSectionDirective = 236,
    OMPSingleDirective = 237,
    OMPParallelForDirective = 238,
    OMPParallelSectionsDirective = 239,
    OMPTaskDirective = 240,
    OMPMasterDirective = 241,
    OMPCriticalDirective = 242,
    OMPTaskyieldDirective = 243,
    OMPBarrierDirective = 244,
    OMPTaskwaitDirective = 245,
    OMPFlushDirective = 246,
    SEHLeaveStmt = 247,
    OMPOrderedDirective = 248,
    OMPAtomicDirective = 249,
    OMPForSimdDirective = 250,
    OMPParallelForSimdDirective = 251,
    OMPTargetDirective = 252,
    OMPTeamsDirective = 253,
    OMPTaskgroupDirective = 254,
    OMPCancellationPointDirective = 255,
    OMPCancelDirective = 256,
    OMPTargetDataDirective = 257,
    OMPTaskLoopDirective = 258,
    OMPTaskLoopSimdDirective = 259,
    OMPDistributeDirective = 260,
    OMPTargetEnterDataDirective = 261,
    OMPTargetExitDataDirective = 262,
    OMPTargetParallelDirective = 263,
    OMPTargetParallelForDirective = 264,
    OMPTargetUpdateDirective = 265,
    OMPDistributeParallelForDirective = 266,
    OMPDistributeParallelForSimdDirective = 267,
    OMPDistributeSimdDirective = 268,
    OMPTargetParallelForSimdDirective = 269,
    OMPTargetSimdDirective = 270,
    OMPTeamsDistributeDirective = 271,
    OMPTeamsDistributeSimdDirective = 272,
    OMPTeamsDistributeParallelForSimdDirective = 273,
    OMPTeamsDistributeParallelForDirective = 274,
    OMPTargetTeamsDirective = 275,
    OMPTargetTeamsDistributeDirective = 276,
    OMPTargetTeamsDistributeParallelForDirective = 277,
    OMPTargetTeamsDistributeParallelForSimdDirective = 278,
    OMPTargetTeamsDistributeSimdDirective = 279,
    BuiltinBitCastExpr = 280,
    OMPMasterTaskLoopDirective = 281,
    OMPParallelMasterTaskLoopDirective = 282,
    OMPMasterTaskLoopSimdDirective = 283,
    OMPParallelMasterTaskLoopSimdDirective = 284,
    OMPParallelMasterDirective = 285,
    OMPDepobjDirective = 286,
    OMPScanDirective = 287,
    OMPTileDirective = 288,
    OMPCanonicalLoop = 289,
    OMPInteropDirective = 290,
    OMPDispatchDirective = 291,
    OMPMaskedDirective = 292,
    OMPUnrollDirective = 293,
    OMPMetaDirective = 294,
    OMPGenericLoopDirective = 295,
    OMPTeamsGenericLoopDirective = 296,
    OMPTargetTeamsGenericLoopDirective = 297,
    OMPParallelGenericLoopDirective = 298,
    OMPTargetParallelGenericLoopDirective = 299,
    OMPParallelMaskedDirective = 300,
    OMPMaskedTaskLoopDirective = 301,
    OMPMaskedTaskLoopSimdDirective = 302,
    OMPParallelMaskedTaskLoopDirective = 303,
    OMPParallelMaskedTaskLoopSimdDirective = 304,
    OMPErrorDirective = 305,
    OMPScopeDirective = 306,
    TranslationUnit = 350,
    UnexposedAttr = 400,
    IBActionAttr = 401,
    IBOutletAttr = 402,
    IBOutletCollectionAttr = 403,
    CXXFinalAttr = 404,
    CXXOverrideAttr = 405,
    AnnotateAttr = 406,
    AsmLabelAttr = 407,
    PackedAttr = 408,
    PureAttr = 409,
    ConstAttr = 410,
    NoDuplicateAttr = 411,
    CUDAConstantAttr = 412,
    CUDADeviceAttr = 413,
    CUDAGlobalAttr = 414,
    CUDAHostAttr = 415,
    CUDASharedAttr = 416,
    VisibilityAttr = 417,
    DLLExport = 418,
    DLLImport = 419,
    NSReturnsRetained = 420,
    NSReturnsNotRetained = 421,
    NSReturnsAutoreleased = 422,
    NSConsumesSelf = 423,
    NSConsumed = 424,
    ObjCException = 425,
    ObjCNSObject = 426,
    ObjCIndependentClass = 427,
    ObjCPreciseLifetime = 428,
    ObjCReturnsInnerPointer = 429,
    ObjCRequiresSuper = 430,
    ObjCRootClass = 431,
    ObjCSubclassingRestricted = 432,
    ObjCExplicitProtocolImpl = 433,
    ObjCDesignatedInitializer = 434,
    ObjCRuntimeVisible = 435,
    ObjCBoxable = 436,
    FlagEnum = 437,
    ConvergentAttr = 438,
    WarnUnusedAttr = 439,
    WarnUnusedResultAttr = 440,
    AlignedAttr = 441,
    PreprocessingDirective = 500,
    MacroDefinition = 501,
    MacroExpansion = 502,
    InclusionDirective = 503,
    ModuleImportDecl = 600,
    TypeAliasTemplateDecl = 601,
    StaticAssert = 602,
    FriendDecl = 603,
    ConceptDecl = 604,
    OverloadCandidate = 700,
    pub const TagType = c_uint;
    pub const FirstDecl: CursorKind = @enumFromInt(1);
    pub const LastDecl: CursorKind = @enumFromInt(39);
    pub const FirstRef: CursorKind = @enumFromInt(40);
    pub const LastRef: CursorKind = @enumFromInt(50);
    pub const FirstInvalid: CursorKind = @enumFromInt(70);
    pub const LastInvalid: CursorKind = @enumFromInt(73);
    pub const FirstExpr: CursorKind = @enumFromInt(100);
    pub const LastExpr: CursorKind = @enumFromInt(155);
    pub const FirstStmt: CursorKind = @enumFromInt(200);
    pub const AsmStmt: CursorKind = @enumFromInt(215);
    pub const LastStmt: CursorKind = @enumFromInt(306);
    pub const FirstAttr: CursorKind = @enumFromInt(400);
    pub const LastAttr: CursorKind = @enumFromInt(441);
    pub const MacroInstantiation: CursorKind = @enumFromInt(502);
    pub const FirstPreprocessing: CursorKind = @enumFromInt(500);
    pub const LastPreprocessing: CursorKind = @enumFromInt(503);
    pub const FirstExtraDecl: CursorKind = @enumFromInt(600);
    pub const LastExtraDecl: CursorKind = @enumFromInt(604);
};
pub inline fn clang_getCursorKind(arg0: c.CXCursor) CursorKind {
    return @enumFromInt(c.clang_getCursorKind(arg0));
}
pub inline fn clang_isDeclaration(arg0: CursorKind) c_uint {
    return c.clang_isDeclaration(@intFromEnum(arg0));
}
pub inline fn clang_isReference(arg0: CursorKind) c_uint {
    return c.clang_isReference(@intFromEnum(arg0));
}
pub inline fn clang_isExpression(arg0: CursorKind) c_uint {
    return c.clang_isExpression(@intFromEnum(arg0));
}
pub inline fn clang_isStatement(arg0: CursorKind) c_uint {
    return c.clang_isStatement(@intFromEnum(arg0));
}
pub inline fn clang_isAttribute(arg0: CursorKind) c_uint {
    return c.clang_isAttribute(@intFromEnum(arg0));
}
pub inline fn clang_isInvalid(arg0: CursorKind) c_uint {
    return c.clang_isInvalid(@intFromEnum(arg0));
}
pub inline fn clang_isTranslationUnit(arg0: CursorKind) c_uint {
    return c.clang_isTranslationUnit(@intFromEnum(arg0));
}
pub inline fn clang_isPreprocessing(arg0: CursorKind) c_uint {
    return c.clang_isPreprocessing(@intFromEnum(arg0));
}
pub inline fn clang_isUnexposed(arg0: CursorKind) c_uint {
    return c.clang_isUnexposed(@intFromEnum(arg0));
}
pub const TypeKind = enum(c_uint) {
    Invalid = 0,
    Unexposed = 1,
    Void = 2,
    Bool = 3,
    Char_U = 4,
    UChar = 5,
    Char16 = 6,
    Char32 = 7,
    UShort = 8,
    UInt = 9,
    ULong = 10,
    ULongLong = 11,
    UInt128 = 12,
    Char_S = 13,
    SChar = 14,
    WChar = 15,
    Short = 16,
    Int = 17,
    Long = 18,
    LongLong = 19,
    Int128 = 20,
    Float = 21,
    Double = 22,
    LongDouble = 23,
    NullPtr = 24,
    Overload = 25,
    Dependent = 26,
    ObjCId = 27,
    ObjCClass = 28,
    ObjCSel = 29,
    Float128 = 30,
    Half = 31,
    Float16 = 32,
    ShortAccum = 33,
    Accum = 34,
    LongAccum = 35,
    UShortAccum = 36,
    UAccum = 37,
    ULongAccum = 38,
    BFloat16 = 39,
    Ibm128 = 40,
    Complex = 100,
    Pointer = 101,
    BlockPointer = 102,
    LValueReference = 103,
    RValueReference = 104,
    Record = 105,
    Enum = 106,
    Typedef = 107,
    ObjCInterface = 108,
    ObjCObjectPointer = 109,
    FunctionNoProto = 110,
    FunctionProto = 111,
    ConstantArray = 112,
    Vector = 113,
    IncompleteArray = 114,
    VariableArray = 115,
    DependentSizedArray = 116,
    MemberPointer = 117,
    Auto = 118,
    Elaborated = 119,
    Pipe = 120,
    OCLImage1dRO = 121,
    OCLImage1dArrayRO = 122,
    OCLImage1dBufferRO = 123,
    OCLImage2dRO = 124,
    OCLImage2dArrayRO = 125,
    OCLImage2dDepthRO = 126,
    OCLImage2dArrayDepthRO = 127,
    OCLImage2dMSAARO = 128,
    OCLImage2dArrayMSAARO = 129,
    OCLImage2dMSAADepthRO = 130,
    OCLImage2dArrayMSAADepthRO = 131,
    OCLImage3dRO = 132,
    OCLImage1dWO = 133,
    OCLImage1dArrayWO = 134,
    OCLImage1dBufferWO = 135,
    OCLImage2dWO = 136,
    OCLImage2dArrayWO = 137,
    OCLImage2dDepthWO = 138,
    OCLImage2dArrayDepthWO = 139,
    OCLImage2dMSAAWO = 140,
    OCLImage2dArrayMSAAWO = 141,
    OCLImage2dMSAADepthWO = 142,
    OCLImage2dArrayMSAADepthWO = 143,
    OCLImage3dWO = 144,
    OCLImage1dRW = 145,
    OCLImage1dArrayRW = 146,
    OCLImage1dBufferRW = 147,
    OCLImage2dRW = 148,
    OCLImage2dArrayRW = 149,
    OCLImage2dDepthRW = 150,
    OCLImage2dArrayDepthRW = 151,
    OCLImage2dMSAARW = 152,
    OCLImage2dArrayMSAARW = 153,
    OCLImage2dMSAADepthRW = 154,
    OCLImage2dArrayMSAADepthRW = 155,
    OCLImage3dRW = 156,
    OCLSampler = 157,
    OCLEvent = 158,
    OCLQueue = 159,
    OCLReserveID = 160,
    ObjCObject = 161,
    ObjCTypeParam = 162,
    Attributed = 163,
    OCLIntelSubgroupAVCMcePayload = 164,
    OCLIntelSubgroupAVCImePayload = 165,
    OCLIntelSubgroupAVCRefPayload = 166,
    OCLIntelSubgroupAVCSicPayload = 167,
    OCLIntelSubgroupAVCMceResult = 168,
    OCLIntelSubgroupAVCImeResult = 169,
    OCLIntelSubgroupAVCRefResult = 170,
    OCLIntelSubgroupAVCSicResult = 171,
    OCLIntelSubgroupAVCImeResultSingleReferenceStreamout = 172,
    OCLIntelSubgroupAVCImeResultDualReferenceStreamout = 173,
    OCLIntelSubgroupAVCImeSingleReferenceStreamin = 174,
    OCLIntelSubgroupAVCImeDualReferenceStreamin = 175,
    ExtVector = 176,
    Atomic = 177,
    BTFTagAttributed = 178,
    pub const TagType = c_uint;
    pub const FirstBuiltin: TypeKind = @enumFromInt(2);
    pub const LastBuiltin: TypeKind = @enumFromInt(40);
    pub const OCLIntelSubgroupAVCImeResultSingleRefStreamout: TypeKind = @enumFromInt(172);
    pub const OCLIntelSubgroupAVCImeResultDualRefStreamout: TypeKind = @enumFromInt(173);
    pub const OCLIntelSubgroupAVCImeSingleRefStreamin: TypeKind = @enumFromInt(174);
    pub const OCLIntelSubgroupAVCImeDualRefStreamin: TypeKind = @enumFromInt(175);
};
pub inline fn clang_getTypeKindSpelling(K: TypeKind) c.CXString {
    return c.clang_getTypeKindSpelling(@intFromEnum(K));
}
pub const CXXAccessSpecifier = enum(c_uint) {
    InvalidAccessSpecifier = 0,
    Public = 1,
    Protected = 2,
    Private = 3,
    pub const TagType = c_uint;
};
pub inline fn clang_getCXXAccessSpecifier(arg0: c.CXCursor) CXXAccessSpecifier {
    return @enumFromInt(c.clang_getCXXAccessSpecifier(arg0));
}
pub inline fn clang_getTemplateCursorKind(C: c.CXCursor) CursorKind {
    return @enumFromInt(c.clang_getTemplateCursorKind(C));
}
pub inline fn clang_getCursorKindSpelling(Kind: CursorKind) c.CXString {
    return c.clang_getCursorKindSpelling(@intFromEnum(Kind));
}
pub inline fn clang_codeCompleteGetContainerKind(Results: [*c]c.CXCodeCompleteResults, IsIncomplete: [*c]c_uint) CursorKind {
    return @enumFromInt(c.clang_codeCompleteGetContainerKind(Results, IsIncomplete));
}
pub const c = @cImport({
    @cInclude("clang-c/Index.h");
    @cInclude("stdio.h");
    @cDefine("GLUE_CAST", {});
    @cInclude("glue.h");
    @cUndef("GLUE_CAST");
});
