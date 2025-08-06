#!/usr/bin/env raku

sub compute(Str :$filename, Int :$width, Int :$height --> Int) {
    my Int @input = $filename.IO.split('')>>.Int;
    my Array[Int] @layers = @input.rotor($width × $height).map({Array[Int].new($_)});
    my Int $zeroes;
    my Int $score;
    for @layers -> @layer {
        my %bags = @layer.Bag;
        say %bags;
        if (!$zeroes.defined || $zeroes > %bags{0}) {
            $zeroes = %bags{0};
            $score = %bags{1} × %bags{2};
        }
    }
    return $score;
}

say compute(:filename("input") :width(25) :height(6));
