#!/usr/bin/env raku

sub run(Int @input, Int $noun, Int $verb --> Int) {
    my Int @memory = @input;
    @memory[1] = $noun;
    @memory[2] = $verb;
    my $ip = 0;
    while True {
        given @memory[$ip] {
            when 99 { last; }
            my ($a, $b, $c) = @memory[$ip+1..$ip+3];
            when 1 { @memory[$c] = @memory[$a] + @memory[$b]; $ip += 4; }
            when 2 { @memory[$c] = @memory[$a] × @memory[$b]; $ip += 4; }
            default { THROW "invalid op $_"; }
        }
    }
    return @memory[0];
}

sub compute(Str $filename --> Int) {
    my Int @input = $filename.IO.split(',')>>.Int;
    for 0..99 -> $i {
        for 0..99 -> $j {
            return $i × 100 + $j if run(@input, $i, $j) == 19690720;
        }
    }
}

say compute "input";
