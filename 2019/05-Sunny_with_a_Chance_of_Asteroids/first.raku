#!/usr/bin/env raku

sub run(Int @inputMemory --> Str) {
    my Int @memory = @inputMemory;
    my Int @input = (1);
    my Int @output;
    my $ip = 0;
    while True {
        my @digits = @memory[$ip].Str.comb>>.Int;
        my ($A, $B, $C, $D, $E) = (0 xx (5 - @digits.elems)).Array.append(@digits);
        given $D * 10 + $E {
            # 0 parameters instructions
            $ip++;
            when 99 { last; }
            # 1 parameters instructions
            my $c = @memory[$ip];
            $ip++;
            when 3 { @memory[$c] = shift @input; }
            when 4 { push @output, ($C == 0 ?? @memory[$c] !! $c); }
            # 3 parameters instructions
            my $b = @memory[$ip];
            $ip++;
            my $a = @memory[$ip];
            $ip++;
            when 1 { @memory[$a] = ($C == 0 ?? @memory[$c] !! $c) + ($B == 0 ?? @memory[$b] !! $b); }
            when 2 { @memory[$a] = ($C == 0 ?? @memory[$c] !! $c) × ($B == 0 ?? @memory[$b] !! $b); }
            default { die "invalid op $_"; }
        }
    }
    return @output.join('');
}

sub compute(Str $filename --> Str) {
    my Int @input = $filename.IO.split(',')>>.Int;
    return run(@input);
}

say compute "input";
