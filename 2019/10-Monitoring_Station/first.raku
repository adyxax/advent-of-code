#!/usr/bin/env raku

sub check(Str :$filename, Int :@expected) {
    my @output = compute :$filename;
    unless @expected ~~ @output {
        say "$filename error: expected " ~ @expected ~ " but got " ~ @output;
        exit 1;
    }
}

my $best-score = 0;

sub compute(Str :$filename --> List) {
    my Str @lines = $filename.IO.lines;
    my @asteroids;
    for @lines.kv -> $y, $line {
        for $line.comb.kv -> $x, $c {
            next unless $c eq "#";
            @asteroids.push(($x, $y));
        }
    }
    my @best;
    $best-score = 0;
    for @asteroids -> ($x1, $y1) {
        my $visible = 0;
        outer: for @asteroids -> ($x2, $y2) {
            next if $x1 == $x2 && $y1 == $y2;
            for @asteroids -> ($x3, $y3) {
                next if $x1 == $x3 && $y1 == $y3;
                next if $x2 == $x3 && $y2 == $y3;
                # we check if we are within the segment bounds
                next unless min($x1, $x2) ≤ $x3 ≤ max($x1, $x2) && min($y1, $y2) ≤ $y3 ≤ max($y1, $y2);
                # The three points are collinear if the area of the triangle
                # they form is 0, which can be checked using the cross product
                next unless ($x2 - $x1) × ($y3 - $y1) - ($y2 - $y1) × ($x3 - $x1) == 0;
                next outer;
            }
            $visible++;
        }
        next if $visible < $best-score;
        $best-score = $visible;
        @best = ($x1, $y1);
    }

    return @best;
}

check :filename("example1") :expected(Array[Int].new(3,4));
check :filename("example2") :expected(Array[Int].new(5,8));
check :filename("example3") :expected(Array[Int].new(1,2));
check :filename("example4") :expected(Array[Int].new(6,3));
check :filename("example5") :expected(Array[Int].new(11,13));

say compute(:filename("input"));
say $best-score;
