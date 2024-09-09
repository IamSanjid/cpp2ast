const std = @import("std");
const builtin = @import("builtin");
const arch = builtin.cpu.arch;
const abi = builtin.abi;

comptime {
    if (builtin.os.tag == .windows) {
        // some mingw compiled libs tries to find these, but actually doesn't use them...
        if (builtin.target.isMinGW() and (arch == .x86 or arch == .x86_64)) {
            @export(&___chkstk, .{ .name = "___chkstk", .linkage = .weak });
            @export(&___chkstk, .{ .name = "__alloca", .linkage = .weak });
        }
    }
}

extern "C" fn _alloca() void;
// This is wrong, but probably nothing uses `___chkstk` or `__alloca`
pub fn ___chkstk() callconv(.C) void {
    @setRuntimeSafety(false);
    _alloca();
}
