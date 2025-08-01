#!/usr/bin/env raku

sub run(Int @inputMemory --> Str) {
    my Int @memory = @inputMemory;
    my Int @input = (5);
    my Int @output;
    my $ip = 0;
    while True {
        my @digits = @memory[$ip].Str.comb>>.Int;
        my ($A, $B, $C, $D, $E) = (0 xx (5 - @digits.elems)).Array.append(@digits);
        given $D * 10 + $E {
            # 0 parameter instructions
            $ip++;
            when 99 { last; }
            # 1 parameter instructions
            my $c = @memory[$ip];
            $ip++;
            when 3 { @memory[$c] = shift @input; }
            $c = @memory[$c] if $C == 0; # after this, the first parameter is never an output
            when 4 { push @output, $c; }
            # 2 parameters instructions
            my $b = @memory[$ip];
            $ip++;
            $b = @memory[$b] if $B == 0; # after this, the second parameter is never an output
            when 5 { $ip = $b if $c != 0; }
            when 6 { $ip = $b if $c == 0; }
            # 3 parameters instructions
            my $a = @memory[$ip];
            $ip++;
            when 1 { @memory[$a] = $c + $b; }
            when 2 { @memory[$a] = $c × $b; }
            when 7 { @memory[$a] = $c < $b ?? 1 !! 0; }
            when 8 { @memory[$a] = $c == $b ?? 1 !! 0; }
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
