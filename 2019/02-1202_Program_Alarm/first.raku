#!/usr/bin/env raku

sub check(Str $filename, Int $expected) {
    my $output = compute($filename);
    if $expected != $output {
        say "$filename error: expected $expected but got $output";
        exit 1;
    }
}

sub compute(Str $filename, Bool :$tweak-before-running --> Int) {
    my @input = $filename.IO.split(',')>>.Int;
    if $tweak-before-running {
        @input[1] = 12;
        @input[2] = 2;
    }
    my $i = 0;
    while True {
        given @input[$i] {
            when 99 { last; }
            my ($a, $b, $c) = @input[$i+1..$i+3];
            when 1 { @input[$c] = @input[$a] + @input[$b]; }
            when 2 { @input[$c] = @input[$a] × @input[$b]; }
            default { THROW "invalid op $_"; }
        }
        $i += 4;
    }
    return @input[0];
}

check "example1", 3500;
check "example2", 2;
check "example3", 0;
check "example4", 30;

say compute "input", :tweak-before-running;
