const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

var allocator: std.mem.Allocator = undefined;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example), 140);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

const ListErrors = error{
    OutOfMemory,
};

const List = struct {
    data: std.ArrayList(Elt),
    fn addOne(l: *List) !*Elt {
        return try l.data.addOne();
    }
    fn init(reader: anytype) !*List {
        var l = try allocator.create(List);
        l.data = std.ArrayList(Elt).init(allocator);
        while (true) {
            const c = reader.readByte() catch break;
            switch (c) {
                '[' => {
                    var e = try l.data.addOne();
                    e.* = Elt{ .list = try List.init(reader) };
                },
                ']' => {
                    return l;
                },
                ',' => { // comma after a list ending, ignore
                },
                else => { // a digit
                    var e = try l.data.addOne();
                    e.* = Elt{ .int = c - '0' };
                    while (true) {
                        const d = reader.readByte() catch unreachable;
                        if (d == ']') {
                            return l;
                        } else if (d == ',') {
                            break;
                        }
                        e.int = e.int * 10 + d;
                    }
                },
            }
        }
        return l;
    }
    fn initEmpty() !*List {
        var l = try allocator.create(List);
        l.data = std.ArrayList(Elt).init(allocator);
        return l;
    }
    fn initSingleton(n: u8) !*List {
        var l = try allocator.create(List);
        l.data = std.ArrayList(Elt).init(allocator);
        var e = try l.data.addOne();
        e.* = Elt{ .int = n };
        return l;
    }
    fn isLowerThan(l: *List, o: *List) ListErrors!?bool {
        var i: usize = 0;
        while (true) : (i += 1) {
            if (o.len() <= i) {
                if (l.len() <= i) {
                    return null;
                }
                return false;
            }
            if (l.len() <= i) {
                return true;
            }
            const t = try l.data.items[i].isLowerThan(&(o.data.items[i]));
            if (t != null) {
                return t;
            }
        }
    }
    inline fn len(l: *List) usize {
        return l.data.items.len;
    }
    fn print(l: *List) void {
        for (l.data.items) |e| {
            switch (e) {
                Elt.int => |i| std.debug.print("{d} ", .{i}),
                Elt.list => |o| {
                    std.debug.print("[", .{});
                    o.print();
                    std.debug.print("]", .{});
                },
            }
        }
    }
};

const EltType = enum {
    int,
    list,
};

const Elt = union(EltType) {
    int: u8,
    list: *List,
    fn isLowerThan(e: *Elt, f: *Elt) !?bool {
        switch (e.*) {
            Elt.int => |i| switch (f.*) {
                Elt.int => |j| {
                    if (i == j) {
                        return null;
                    }
                    return i < j;
                },
                Elt.list => |o| {
                    var l = try List.initSingleton(i);
                    return l.isLowerThan(o);
                },
            },
            Elt.list => |l| switch (f.*) {
                Elt.int => |j| {
                    var ll = Elt{ .list = try List.initSingleton(j) };
                    return e.isLowerThan(&ll);
                },
                Elt.list => |o| {
                    return l.isLowerThan(o);
                },
            },
        }
    }
};

fn lesserThan(context: void, a: *Elt, b: *Elt) bool {
    _ = context;
    var tt = a.isLowerThan(b) catch unreachable;
    if (tt) |t| {
        if (t) {
            return true;
        }
    }
    return false;
}

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var list = std.ArrayList(*Elt).init(allocator);
    var divider2Line = std.io.fixedBufferStream("[[2]]");
    var div2 = try list.addOne();
    div2.* = try allocator.create(Elt);
    div2.*.* = Elt{ .list = try List.init(divider2Line.reader()) };
    var divider6Line = std.io.fixedBufferStream("[[6]]");
    var div6 = try list.addOne();
    div6.* = try allocator.create(Elt);
    div6.*.* = Elt{ .list = try List.init(divider6Line.reader()) };
    // process input
    while (it.next()) |line| {
        var bs = std.io.fixedBufferStream(line);
        var elt = try list.addOne();
        elt.* = try allocator.create(Elt);
        elt.*.* = Elt{ .list = try List.init(bs.reader()) };
    }
    // sort the list
    std.sort.sort(*Elt, list.items, {}, lesserThan);
    //for (list.items) |l| {
    //    l.list.print();
    //    std.debug.print("\n", .{});
    //}
    // compute output
    var ret: usize = 1;
    for (list.items) |l, i| {
        switch (l.*) {
            Elt.int => {},
            Elt.list => |ll| {
                if (ll.len() != 1) {
                    continue;
                }
                switch (ll.data.items[0]) {
                    Elt.int => {},
                    Elt.list => |lll| {
                        if (lll.len() != 1) {
                            continue;
                        }
                        switch (lll.data.items[0]) {
                            Elt.int => {},
                            Elt.list => |llll| {
                                if (llll.len() != 1) {
                                    continue;
                                }
                                switch (llll.data.items[0]) {
                                    Elt.int => |n| {
                                        if (n == 2 or n == 6) {
                                            ret *= (i + 1);
                                        }
                                    },
                                    Elt.list => {},
                                }
                            },
                        }
                    },
                }
            },
        }
    }
    return ret;
}
