const std = @import("std");
const object = @import("./object.zig");
const Obj = object.Obj;

const ObjStringContext = struct {
    pub fn hash(self: @This(), s: *Obj.String) u64 {
        _ = self;
        return s.hash;
    }
    pub fn eql(self: @This(), a: *Obj.String, b: *Obj.String) bool {
        _ = self;
        return std.hash_map.eqlString(a.bytes, b.bytes);
    }
};

pub fn ObjStringHashMap(comptime V: type) type {
    return std.HashMap(
        *Obj.String,
        V,
        ObjStringContext,
        std.hash_map.default_max_load_percentage,
    );
}

test ObjStringHashMap {
    const copyString = object.copyString;
    const MashHap = ObjStringHashMap(usize);
    var hm = MashHap.init(std.testing.allocator);
    defer hm.deinit();

    var stringobj: *Obj.String = try copyString(std.testing.allocator, "this is a key");
    defer stringobj.deinit(std.testing.allocator);

    try hm.put(stringobj, 69);
    try std.testing.expectEqual(hm.unmanaged.size, 1);
    try std.testing.expectEqual(hm.get(stringobj).?, 69);
}
