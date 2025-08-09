#!/usr/bin/env raku

# output of the ./first.raku run places my monitoring station in (11, 11) where
# it sees 221 asteroids. Since we have more than 200 asteroids, there is no need
# to account for drill rotation.
my $msx = 11;
my $msy = 11;

my Str @lines = "input".IO.lines;
my @asteroids;
for @lines.kv -> $y, $line {
    for $line.comb.kv -> $x, $c {
        next unless $c eq "#";
        next if $x == $msx && $y == $msy;
        # we translate all coordinates so that the monitoring station is the
        # origin of our system
        @asteroids.push(($x - $msx, $y - $msy));
    }
}

my @visible;
outer: for @asteroids -> ($x2, $y2) {
    for @asteroids -> ($x3, $y3) {
        next if $x2 == $x3 && $y2 == $y3;
        next unless min(0, $x2) ≤ $x3 ≤ max(0, $x2) && min(0, $y2) ≤ $y3 ≤ max(0, $y2);
        next unless $x2 × $y3 - $y2 × $x3 == 0;
        next outer;
    }
    # I am adding the calculation of the polar angle for easier sorting later.
    # Since the laser is rotating clockwise and we are measuring from the
    # y-axis, the atan2 arguments are inverted. This gives us the angle in
    # [-pi;pi) as if a pi/4 symetry was applied which is perfect because in this
    # system the laser starts rotating from pi towards decreasing angles.
    my $theta = atan2($x2, $y2);
    push @visible, ($x2, $y2, $theta);
}

my @sorted = @visible.sort(-> $a, $b {
        # sort in decreasing order
        $b[2] <=> $a[2];
    }
);

my $two-hundredth-x = @sorted[199][0] + $msx;
my $two-hundredth-y = @sorted[199][1] + $msy;

say $two-hundredth-x × 100 + $two-hundredth-y;
