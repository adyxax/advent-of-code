#!/usr/bin/env raku

sub hasTwoConsecutiveEqual(@digits --> Bool) {
    for ^5 -> $i {
        return True if @digits[$i] == @digits[$i+1];
    }
    return False;
}

sub isMonotonousIncrease(@digits --> Bool) {
    for ^5 -> $i {
        return False unless @digits[$i] ≤ @digits[$i+1];
    }
    return True;
}

sub compute(Str:D $filename --> Int) {
    my ($a, $b)  = $filename.IO.slurp.trim.split('-')>>.Int;
    my Int $c = 0;
    while $a < $b {
        my @digits = $a.Str.comb>>.Int;
        $a++;
        next unless hasTwoConsecutiveEqual(@digits);
        next unless isMonotonousIncrease(@digits);
        $c++;
    }
    return $c;
}

say compute "input";
