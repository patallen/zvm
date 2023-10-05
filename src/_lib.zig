pub const testing = @import("std").testing;
pub const Chunk = @import("./Chunk.zig");
pub const VM = @import("./VM.zig");
pub const debug = @import("./debug.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
