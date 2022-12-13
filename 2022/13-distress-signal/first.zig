const std = @import("std");

const example = @embedFile("example");
const input = @embedFile("input");

var allocator: std.mem.Allocator = undefined;

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example), 13);
    const result = try solve(input);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

const ListErrors = error{
    OutOfMemory,
};

const List = struct {
    data: std.ArrayList(Elt),
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
                    f.* = Elt{ .list = try List.initSingleton(j) };
                    return e.isLowerThan(f);
                },
                Elt.list => |o| {
                    return l.isLowerThan(o);
                },
            },
        }
    }
};

fn solve(puzzle: []const u8) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var i: usize = 1;
    var tot: usize = 0;
    while (it.next()) |line| {
        var firstLine = std.io.fixedBufferStream(line);
        var first = try List.init(firstLine.reader());
        var secondLine = std.io.fixedBufferStream(it.next() orelse unreachable);
        var second = try List.init(secondLine.reader());
        const t = try first.isLowerThan(second);
        //first.print();
        //std.debug.print("\n", .{});
        //second.print();
        //std.debug.print("\n{d}: {?}\n", .{ i, t });
        if (t) |s| {
            if (s) {
                tot += i;
            }
        }
        i += 1;
    }
    return tot;
}
