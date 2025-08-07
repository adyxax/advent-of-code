#!/usr/bin/env raku

class Computer {
    has Bool $.halted = False;
    has Int @.input;
    has Int $.ip = 0;
    has Int %.memory is built is required;
    has Int $.relative-base = 0;
    method run(Int @input --> Array[Int]) {
        @!input.append(@input);
        my Int @output;
        while True {
            my Int @digits = %!memory{$!ip}.Str.comb>>.Int;
            my Int ($A, $B, $C, $D, $E) = (0 xx (5 - @digits.elems)).Array.append(@digits);
            given $D * 10 + $E {
                # 0 parameter instructions
                $!ip++;
                when 99 { $!halted = True; last; }
                # 1 parameter instructions
                my $c = %!memory{$!ip};
                $!ip++;
                given $C {
                    when 0 { $c := %!memory{$c} //= 0; }
                    when 2 { $c := %!memory{$c + $!relative-base} //= 0; }
                }
                when 3 {
                    if @!input.elems > 0 {
                        $c = shift @!input;
                    } else {
                        $!ip -= 2;
                        last;
                    }
                }
                when 4 { push @output, $c; }
                when 9 { $!relative-base += $c; }
                # 2 parameters instructions
                my $b = %!memory{$!ip};
                $!ip++;
                given $B {
                    when 0 { $b := %!memory{$b} //= 0; }
                    when 2 { $b := %!memory{$b + $!relative-base} //= 0; }
                }
                when 5 { $!ip = $b if $c != 0; }
                when 6 { $!ip = $b if $c == 0; }
                # 3 parameters instructions
                my $a = %!memory{$!ip};
                $!ip++;
                given $A {
                    when 0 { $a := %!memory{$a} //= 0; }
                    when 2 { $a := %!memory{$a + $!relative-base} //= 0; }
                }
                when 1 { $a = $c + $b; }
                when 2 { $a = $c × $b; }
                when 7 { $a = $c < $b ?? 1 !! 0; }
                when 8 { $a = $c == $b ?? 1 !! 0; }
                default { die "invalid op $_"; }
            }
        }
        return @output;
    }
}

sub compute(Str :$filename, Int :@initial-input --> Str) {
    my Int @input = $filename.IO.split(',')>>.Int;
    my Int %memory = @input.kv;
    my Computer $A = Computer.new :memory(%memory.clone);
    my Str $output = $A.run(@initial-input)>>.Str.join(',');
    return $output;
}

say compute :filename("input") :initial-input(Array[Int].new(2));
