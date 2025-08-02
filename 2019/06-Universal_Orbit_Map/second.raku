#!/usr/bin/env raku

use Data::Tree;

class Body is RTree {
    method countOrbits(Int $depth --> Int) {
        return $depth + [+] @.children>>.countOrbits($depth+1);
    }
    method walk-to(Str $dest --> Array) {
        return [] if $dest eq $.data;
        for @.children -> $child {
            with $child.walk-to($dest) {
                return @_.unshift($.data);
            }
        }
        fail "not found";
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
    my Str @COM-to-YOU = %nodes{"COM"}.walk-to("YOU");
    my Str @COM-to-SAN = %nodes{"COM"}.walk-to("SAN");
    while @COM-to-YOU[0] eq @COM-to-SAN[0] {
        shift @COM-to-YOU;
        shift @COM-to-SAN;
    }
    return @COM-to-YOU.elems + @COM-to-SAN.elems;
}

check "example2", 4;

say compute "input";
