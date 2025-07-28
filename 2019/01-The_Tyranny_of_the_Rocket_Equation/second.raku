#!/usr/bin/env raku

my Int $total = 0;

sub compute(Int $n --> Int) {
    my $fuel = ( $n / 3 ).floor - 2;
    if $fuel > 0 {
        return $fuel + compute($fuel);
    } else {
        return 0;
    }
}

for "input".IO.lines>>.Int -> $n {
    $total += compute($n)
}

say $total;
