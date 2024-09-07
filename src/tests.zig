pub const cpp2ast = @import("cpp2ast");

test {
    @setEvalBranchQuota(2001);
    @import("std").testing.refAllDeclsRecursive(cpp2ast.traversers);
}
