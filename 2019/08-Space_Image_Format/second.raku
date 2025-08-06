#!/usr/bin/env raku

sub compute(Str :$filename, Int :$width, Int :$height --> Array[Str]) {
    my Int @input = $filename.IO.split('')>>.Int;
    my Array[Int] @layers = @input.rotor($width × $height).map({Array[Int].new($_)});
    for ^$width -> $x {
        for ^$height -> $y {
            for ^@layers.elems -> $i {
                with @layers[$i][$x + $y * $width] {
                    next if $_ == 2;
                    @layers[0][$x + $y * $width] = $_;
                    last;
                }
            }
        }
    }
    my Str @output = @layers[0]>>.Str;
    return @output;
}

my Str @msg = compute(:filename("input") :width(25) :height(6));
for @msg.rotor(25) {
    my $line = $_.map({ $_ eq '0' ?? ' ' !! $_ }).map({ ($_, $_) }).flat.join('');
    for 1..2 { say $line; }
}
