#!/usr/bin/env raku

class Computer {
    has Int @!memory is built is required;
    has Int @!input is built is required;
    has Int $!ip = 0;
    has Bool $.halted = False;
    method run(Int @input --> Array[Int]) {
        @!input.append(@input);
        my Int @output;
        while True {
            my Int @digits = @!memory[$!ip].Str.comb>>.Int;
            my Int ($A, $B, $C, $D, $E) = (0 xx (5 - @digits.elems)).Array.append(@digits);
            given $D * 10 + $E {
                # 0 parameter instructions
                $!ip++;
                when 99 { $!halted = True; last; }
                # 1 parameter instructions
                my $c = @!memory[$!ip];
                $!ip++;
                when 3 {
                    if @!input.elems > 0 {
                        @!memory[$c] = shift @!input;
                    } else {
                        $!ip -= 2;
                        last;
                    }
                }
                $c = @!memory[$c] if $C == 0; # after this, the first parameter is never an output
                when 4 { push @output, $c; }
                # 2 parameters instructions
                my $b = @!memory[$!ip];
                $!ip++;
                $b = @!memory[$b] if $B == 0; # after this, the second parameter is never an output
                when 5 { $!ip = $b if $c != 0; }
                when 6 { $!ip = $b if $c == 0; }
                # 3 parameters instructions
                my $a = @!memory[$!ip];
                $!ip++;
                when 1 { @!memory[$a] = $c + $b; }
                when 2 { @!memory[$a] = $c × $b; }
                when 7 { @!memory[$a] = $c < $b ?? 1 !! 0; }
                when 8 { @!memory[$a] = $c == $b ?? 1 !! 0; }
                default { die "invalid op $_"; }
            }
        }
        return @output;
    }
}


sub check(Str $filename, Int $expected) {
    my Int $output = compute :$filename;
    if $expected != $output {
        die "$filename error: expected $expected but got $output";
    }
}

sub compute(Str :$filename --> Int) {
    my Int @input = $filename.IO.split(',')>>.Int;
    my Int $max = 0;
    for [5..9].permutations -> $phase {
        my Computer $A = Computer.new :memory(@input.clone) :input($phase[0]);
        my Computer $B = Computer.new :memory(@input.clone) :input($phase[1]);
        my Computer $C = Computer.new :memory(@input.clone) :input($phase[2]);
        my Computer $D = Computer.new :memory(@input.clone) :input($phase[3]);
        my Computer $E = Computer.new :memory(@input.clone) :input($phase[4]);
        my Int @output = (0);
        until $A.halted || $B.halted || $C.halted || $D.halted || $E.halted {
            @output = $A.run(@output);
            @output = $B.run(@output);
            @output = $C.run(@output);
            @output = $D.run(@output);
            @output = $E.run(@output);
        }
        with @output[0] { $max = $_ if $max < $_ };
    }
    return $max;
}

check "example4", 139629729;
check "example5", 18216;

say compute :filename("input");
