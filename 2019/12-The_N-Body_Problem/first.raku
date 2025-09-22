#!/usr/bin/env raku

sub gravityAdjust(Int $ax, Int $avx is rw, Int $bx) {
    if $ax < $bx {
        $avx++;
    } elsif $ax > $bx {
        $avx--;
    }
}

class Moon {
    has Int $.x is required;
    has Int $.y is required;
    has Int $.z is required;
    has Int $.vx = 0;
    has Int $.vy = 0;
    has Int $.vz = 0;
    method energy(--> Int) {
        ($!x.abs + $!y.abs + $!z.abs) × ($!vx.abs + $!vy.abs + $!vz.abs)
    }
    method gravity(Moon $b) {
        gravityAdjust($!x, $!vx, $b.x);
        gravityAdjust($!y, $!vy, $b.y);
        gravityAdjust($!z, $!vz, $b.z);
    }
    method velocity() {
        $!x += $!vx;
        $!y += $!vy;
        $!z += $!vz;
    }
}

grammar Input {
    rule TOP { ^ [ <moon> \n* ]+ $ }
    token moon { '<x=' <int> ', y=' <int> ', z=' <int> '>' }
    token int { \-? \d+ }
}

class Actions {
    method TOP($/)  { make $<moon>».made.List }
    method int($/)  { make $/.Int }
    method moon($/) {
        my ($x, $y, $z) = $<int>».made;
        make Moon.new(:$x, :$y, :$z);
    }
}

sub check(Str :$filename, Int :$steps, Int :$expected) {
    my $output = compute :$filename :$steps;
    unless $expected ~~ $output {
        say "$filename error: expected " ~ $expected ~ " but got " ~ $output;
        exit 1;
    }
}

sub compute(Str :$filename, Int :$steps = 1000 --> Int) {
    my Str $input = $filename.IO.slurp;

    my Moon @moons = Input.parse($input, :actions(Actions.new)).made or die "Parse failed";

    for 1..$steps {
        for @moons -> $m {
            for @moons -> $n {
                $m.gravity($n)
            }
        }
        for @moons -> $m {
            $m.velocity();
        }
    }

    return [+] @moons».energy;
}

check :filename("example1") :steps(10) :expected(179);
check :filename("example2") :steps(100) :expected(1940);

say compute(:filename("input"));
