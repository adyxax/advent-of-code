#!/usr/bin/env raku

enum MODE <IMMEDIATE POSITION RELATIVE>;
sub to-mode(Int $mode --> MODE) {
    given $mode {
        when 0 { POSITION }
        when 1 { IMMEDIATE }
        when 2 { RELATIVE }
        default { die "invalid mode: $mode" }
    }
}

enum OP <ADD EQ HALT INPUT JNZ JZ LT MUL OUTPUT REBASE>;
sub to-op(Int $op --> OP) {
    given $op {
        when 1 { ADD }
        when 2 { MUL }
        when 3 { INPUT }
        when 4 { OUTPUT }
        when 5 { JNZ }
        when 6 { JZ }
        when 7 { LT }
        when 8 { EQ }
        when 9 { REBASE }
        when 99 { HALT }
        default { die "invalid op: $op" }
    }
}

class Computer {
    has Bool $.halted = False;
    has Int @.input;
    has Int $.ip = 0;
    has Int %.memory is built is required;
    has Int $.relative-base = 0;
    method get(Int $i --> Int) { %!memory{$i} // 0 }
    method getInstruction(--> List) {
        my Int @d = 0 xx 5;
        my $i = 4;
        loop (my $n = self.get($!ip); $n > 0; do { $i--; $n div= 10; }) {
            @d[$i] = $n % 10;
        }
        return (to-mode(@d[0]), to-mode(@d[1]), to-mode(@d[2]), to-op(@d[3]×10 + @d[4]));
    }
    method getWithMode(Int $i, MODE $m --> Int) {
        my $n = self.get($i);
        given $m {
            when IMMEDIATE { return $n };
            when POSITION { return self.get($n) };
            when RELATIVE { return self.get($n + $!relative-base) };
        }
    }
    method run(Int @input = Array[Int].new --> Array[Int]) {
        @!input.append(@input);
        my Int @output;
        while True {
            my ($A, $B, $C, $op) = self.getInstruction();
            given $op {
                when ADD {
                    self.setWithMode(
                        $!ip+3,
                        self.getWithMode($!ip+1, $C) + self.getWithMode($!ip+2, $B),
                        $A);
                    $!ip += 4;
                }
                when EQ {
                    my $v = self.getWithMode($!ip+1, $C) == self.getWithMode($!ip+2, $B) ?? 1 !! 0;
                    self.setWithMode($!ip+3, $v, $A);
                    $!ip += 4;
                }
                when HALT { $!halted = True; return @output; }
                when INPUT {
                    if @!input.elems > 0 {
                        my $v = shift @!input;
                        self.setWithMode($!ip+1, $v, $C)
                    } else {
                        return @output;
                    }
                    $!ip += 2;
                }
                when JNZ {
                    if self.getWithMode($!ip+1, $C) ≠ 0 {
                        $!ip = self.getWithMode($!ip+2, $B);
                    } else {
                        $!ip += 3;
                    }
                }
                when JZ {
                    if self.getWithMode($!ip+1, $C) == 0 {
                        $!ip = self.getWithMode($!ip+2, $B);
                    } else {
                        $!ip += 3;
                    }
                }
                when LT {
                    my $v = self.getWithMode($!ip+1, $C) < self.getWithMode($!ip+2, $B) ?? 1 !! 0;
                    self.setWithMode($!ip+3, $v, $A);
                    $!ip += 4;
                }
                when OUTPUT { push @output, self.getWithMode($!ip+1, $C); $!ip +=2; }
                when REBASE { $!relative-base += self.getWithMode($!ip+1, $C); $!ip += 2; }
                when MUL {
                    self.setWithMode(
                        $!ip+3,
                        self.getWithMode($!ip+1, $C) × self.getWithMode($!ip+2, $B),
                        $A);
                    $!ip += 4;
                }
            }
        }
    }
    method set(Int $i, Int $v) { %!memory{$i} = $v }
    method setWithMode(Int $i, Int $v, MODE $m) {
        given $m {
            when IMMEDIATE { self.set($i, $v) };
            when POSITION { self.set(self.get($i), $v) };
            when RELATIVE { self.set(self.get($i) + $!relative-base, $v) };
        }
    }
}

enum Tile <BALL BLOCK EMPTY PADDLE WALL>;
my subset TileId of Int where {0 ≤ $_ ≤ 4};

sub to-tile(TileId $t --> Tile) {
    given $t {
        when 0 { EMPTY }
        when 1 { WALL }
        when 2 { BLOCK }
        when 3 { PADDLE }
        when 4 { BALL }
    }
}

class Point {
    has Int $.x;
    has Int $.y;
}

sub compute(Str :$filename --> Int) {
    my Int %memory = $filename.IO.split(',')>>.Int.kv;
    my Computer $A .= new :memory(%memory);
    $A.set(0, 2);
    my Tile %screen;
    my Int $score = 0;
    my Int @input;
    while !$A.halted {
        my Int @output = $A.run(@input);
        for @output -> $x, $y, $t {
            if $x == -1 {
                $score = $t;
            } else {
                %screen{"$x,$y"} = to-tile($t);
            }
        }
        my $blocks = 0;
        my Int $paddle;
        my Int $ball;
        for %screen -> (:$key, :$value) {
            my $x = $key.split(',')[0].Int;
            given $value {
                when BALL { $ball = $x }
                when BLOCK { $blocks++ }
                when PADDLE { $paddle = $x }
            }
        }
        last if $blocks == 0;
        when $paddle < $ball { @input = (1) }
        when $paddle > $ball { @input = (-1) }
        when $paddle == $ball { @input = (0) }
    }
    return $score;
}

say compute :filename("input");
