#!/usr/bin/env raku

my Int $total = 0;

for "input".IO.lines>>.Int -> $n {
    $total += ( $n / 3 ).floor - 2;
}

say $total;
