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

enum Color <BLACK WHITE>;
enum Heading <N S E W>;
enum Rotation <LEFT RIGHT>;

sub step(Int $x is rw, Int $y is rw, Heading $h) {
    given $h {
        when N { $y--; }
        when S { $y++; }
        when E { $x++; }
        when W { $x--; }
    }
}

sub rotate(Heading $h, Rotation $r --> Heading) {
    given $h {
        when N { $r == LEFT ?? W !! E }
        when S { $r == LEFT ?? E !! W }
        when E { $r == LEFT ?? N !! S }
        when W { $r == LEFT ?? S !! N }
    }
}

class Robot {
    has Int $.x = 0;
    has Int $.y = 0;
    has Heading $.h = N;
    has Hash %.hull;
    method print-tiles() {
        my Int @ykeys = %!hull.keys>>.Int;
        my Int $y0 = @ykeys.min;
        my Int $y1 = @ykeys.max;
        my Int $x0 = 0;
        my Int $x1 = 0;
        for %!hull.values -> %y {
            my Int @keys = %y.keys>>.Int;
            my Int $min = @keys.min;
            my Int $max = @keys.max;
            $x0 = $min if $x0 > $min;
            $x1 = $max if $x1 < $max;
        }
        my Str $out;
        for $y0..$y1 -> $y {
            for $x0..$x1 -> $x {
                my $c = " ";
                $c = "#" if %!hull{$y;$x}:exists && %!hull{$y;$x} eq WHITE;
                $out ~= $c;
            }
            $out ~= "\n";
        }
        say $out;
    }
    method step(Int @input --> Int) {
        my Color $color = @input[0] == 0 ?? BLACK !! WHITE;
        my Rotation $r = @input[1] == 0 ?? LEFT !! RIGHT;
        %!hull{$!y;$!x} = $color;
        $!h = rotate($!h, $r);
        step($!x, $!y, $!h);
        my $ret = %!hull{$!y;$!x} // BLACK;
        return $ret == WHITE ?? 1 !! 0;
    }
}


sub compute(Str :$filename) {
    my Int %memory = $filename.IO.split(',')>>.Int.kv;
    my Computer $A .= new :memory(%memory);
    my Robot $r .= new;
    my Int @input = (1);
    while !$A.halted {
        my Int @output = $A.run(@input);
        my Int $nextColor = $r.step(@output);
        @input = ($nextColor);
    }
    $r.print-tiles;
}

compute :filename("input");
