#!/usr/bin/env raku

use Data::Tree;

class Body is RTree {
    method countOrbits(Int $depth --> Int) {
        return $depth + [+] @.children>>.countOrbits($depth+1);
    }
}

sub check(Str $filename, Int $expected) {
    my $output = compute($filename);
    if $expected != $output {
        say "$filename error: expected $expected but got $output";
        exit 1;
    }
}

sub compute(Str $filename --> Int) {
    my @input = $filename.IO.lines>>.split(')');
    my %nodes{Str} of Body;
    for @input -> ($a, $b) {
        my $orbitee = %nodes{$a} //= Body.new :data($a);
        my $orbiter = %nodes{$b} //= Body.new :data($b);
        $orbitee.children.push($orbiter);
    }
    return %nodes{"COM"}.countOrbits(0);
}

check "example", 42;

say compute "input";
