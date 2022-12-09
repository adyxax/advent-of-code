const std = @import("std");

const example = @embedFile("example");
const example2 = @embedFile("example2");
const input = @embedFile("input");

pub fn main() anyerror!void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    try std.testing.expectEqual(try solve(example, allocator), 1);
    try std.testing.expectEqual(try solve(example2, allocator), 36);
    const result = try solve(input, allocator);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}

const Line = struct {
    data: std.ArrayList(bool),
    x: i64,
    fn init(allocator: std.mem.Allocator) !*Line {
        var l = try allocator.create(Line);
        l.data = std.ArrayList(bool).init(allocator);
        return l;
    }
    inline fn len(l: Line) usize {
        return l.data.items.len;
    }
    fn set(l: *Line, x: i64) !void {
        if (l.len() == 0) { // this is en empty line
            l.x = x;
            try l.data.append(true);
            return;
        }
        const lx = @intCast(i64, l.len());
        if (x >= l.x) {
            if (x < l.x + lx) { // just set the value
                l.data.items[@intCast(usize, x - l.x)] = true;
            } else { // we need to add trailing spaces
                var i: usize = l.len();
                while (i < x - l.x) : (i += 1) {
                    try l.data.append(false);
                }
                try l.data.append(true);
            }
        } else { // we need to shift right and add leading spaces
            const oldLen = l.len();
            l.data.items.len += @intCast(usize, l.x - x);
            try l.data.ensureUnusedCapacity(l.len());
            std.mem.copyBackwards(bool, l.data.items[@intCast(usize, l.x - x)..], l.data.items[0..oldLen]);
            l.data.items[0] = true;
            var i: usize = 1;
            while (i < @intCast(usize, l.x - x)) : (i += 1) {
                l.data.items[i] = false;
            }
            l.x = x;
        }
    }
    pub fn visited(l: Line) u64 {
        var tot: u64 = 0;
        var i: usize = 0;
        while (i < l.len()) : (i += 1) {
            if (l.data.items[i]) {
                tot += 1;
            }
        }
        return tot;
    }
};

pub const Field = struct {
    allocator: std.mem.Allocator,
    x: i64 = 0,
    y: i64 = 0,
    lines: std.ArrayList(*Line),
    lx: usize = 0,
    fn init(allocator: std.mem.Allocator) !*Field {
        var f = try allocator.create(Field);
        f.allocator = allocator;
        f.x = undefined;
        f.y = 0;
        f.lines = std.ArrayList(*Line).init(allocator);
        var l = try f.lines.addOne();
        l.* = try Line.init(allocator);
        f.lx = 0;
        return f;
    }
    inline fn len(f: Field) usize {
        return f.lines.items.len;
    }
    pub fn set(f: *Field, x: i64, y: i64) !void {
        if (y >= f.y) {
            if (y < f.y + @intCast(i64, f.lines.items.len)) { // the line exists
                try f.lines.items[@intCast(usize, y - f.y)].set(x);
            } else { // append lines
                var i: usize = f.lines.items.len;
                while (i < y - f.y) : (i += 1) {
                    try f.lines.append(try Line.init(f.allocator));
                }
                var l = try Line.init(f.allocator);
                try l.set(x);
                try f.lines.append(l);
            }
        } else { // preprend lines
            const oldLen = f.lines.items.len;
            f.lines.items.len += @intCast(usize, f.y - y);
            try f.lines.ensureUnusedCapacity(f.lines.items.len);
            std.mem.copyBackwards(*Line, f.lines.items[@intCast(usize, f.y - y)..], f.lines.items[0..oldLen]);
            var l = try Line.init(f.allocator);
            try l.set(x);
            f.lines.items[0] = l;
            var i: usize = 1;
            while (i < @intCast(usize, f.y - y)) : (i += 1) {
                f.lines.items[i] = try Line.init(f.allocator);
            }
            f.y = y;
        }
        if (x < f.x or x >= f.x + @intCast(i64, f.lx)) { // recalculate boundaries
            f.x = std.math.maxInt(i64);
            var x2: i64 = std.math.minInt(i64);
            for (f.lines.items) |line| {
                if (line.len() == 0) continue;
                if (f.x > line.x) f.x = line.x;
                if (x2 < line.x + @intCast(i64, line.len())) x2 = line.x + @intCast(i64, line.len());
            }
            f.lx = @intCast(usize, x2 - f.x);
        }
        return;
    }
    pub fn visited(f: Field) u64 {
        var tot: u64 = 0;
        var i: usize = 0;
        while (i < f.len()) : (i += 1) {
            tot += f.lines.items[i].visited();
        }
        return tot;
    }
};

const Rope = struct {
    hx: i64 = 0,
    hy: i64 = 0,
    tx: i64 = 0,
    ty: i64 = 0,
    fn stepH(r: *Rope, direction: u8) void {
        switch (direction) {
            'D' => {
                r.hy += 1;
            },
            'L' => {
                r.hx -= 1;
            },
            'R' => {
                r.hx += 1;
            },
            'U' => {
                r.hy -= 1;
            },
            else => unreachable,
        }
    }
    fn stepT(r: *Rope) void {
        if (r.tx == r.hx) { // same line
            if (r.ty + 2 == r.hy) { // to the left
                r.ty += 1;
            } else if (r.ty - 2 == r.hy) { // to the right
                r.ty -= 1;
            }
        } else if (r.ty == r.hy) { // same column
            if (r.tx + 2 == r.hx) { // to the top
                r.tx += 1;
            } else if (r.tx - 2 == r.hx) { // to the bottom
                r.tx -= 1;
            }
        } else if (r.tx + 1 == r.hx) { // on the left by one
            if (r.ty + 2 == r.hy) { // above by two
                r.tx += 1;
                r.ty += 1;
            } else if (r.ty - 2 == r.hy) { // bellow by two
                r.tx += 1;
                r.ty -= 1;
            }
        } else if (r.tx - 1 == r.hx) { // on the right by one
            if (r.ty + 2 == r.hy) { // above by two
                r.tx -= 1;
                r.ty += 1;
            } else if (r.ty - 2 == r.hy) { // bellow by two
                r.tx -= 1;
                r.ty -= 1;
            }
        } else if (r.ty + 1 == r.hy) { // above by one
            if (r.tx + 2 == r.hx) { // two to the left
                r.tx += 1;
                r.ty += 1;
            } else if (r.tx - 2 == r.hx) { // two to the right
                r.tx -= 1;
                r.ty += 1;
            }
        } else if (r.ty - 1 == r.hy) { // bellow by one
            if (r.tx + 2 == r.hx) { // two to the left
                r.tx += 1;
                r.ty -= 1;
            } else if (r.tx - 2 == r.hx) { // two to the right
                r.tx -= 1;
                r.ty -= 1;
            }
        } else {
            if (r.tx + 2 == r.hx) { // far left
                r.tx += 1;
            } else if (r.tx - 2 == r.hx) { // far right
                r.tx -= 1;
            }
            if (r.ty + 2 == r.hy) { // far above
                r.ty += 1;
            } else if (r.ty - 2 == r.hy) { // far bellow
                r.ty -= 1;
            }
        }
    }
};

fn solve(puzzle: []const u8, allocator: std.mem.Allocator) !u64 {
    var it = std.mem.tokenize(u8, puzzle, "\n");
    var rope: [9]Rope = undefined;
    for (rope) |*r| {
        r.* = Rope{};
    }
    var field = try Field.init(allocator);
    // process input
    while (it.next()) |line| {
        var elts = std.mem.split(u8, line, " ");
        _ = elts.next() orelse unreachable; // the move word
        var n = try std.fmt.parseInt(u8, elts.next() orelse unreachable, 10);
        while (n > 0) : (n -= 1) {
            rope[0].stepH(line[0]);
            rope[0].stepT();
            var i: usize = 1;
            while (i < rope.len) : (i += 1) {
                rope[i].hx = rope[i - 1].tx;
                rope[i].hy = rope[i - 1].ty;
                rope[i].stepT();
            }
            try field.set(rope[rope.len - 1].tx, rope[rope.len - 1].ty);
        }
    }
    return field.visited();
}
