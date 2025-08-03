#!/usr/bin/env raku

sub run(Int @inputMemory, Int @input --> Array[Int]) {
    my Int @memory = @inputMemory;
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
    return @output;
}

sub check(Str $filename, Int $expected) {
    my $output = compute($filename);
    if $expected != $output {
        say "$filename error: expected $expected but got $output";
        exit 1;
    }
}


sub compute(Str $filename --> Int) {
    my Int @input = $filename.IO.split(',')>>.Int;
    my Int $max = 0;
    for [0..4].permutations -> $phase {
        my Int @o= (0);
        my Int @a = run(@input, @o.unshift($phase[0]));
        my Int @b = run(@input, @a.unshift($phase[1]));
        my Int @c = run(@input, @b.unshift($phase[2]));
        my Int @d = run(@input, @c.unshift($phase[3]));
        my Int @e = run(@input, @d.unshift($phase[4]));
        with @e[0] { $max = $_ if $max < $_ };
    }
    return $max;
}

check "example1", 43210;
check "example2", 54321;
check "example3", 65210;

say compute "input";
