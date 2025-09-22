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

sub check(Str :$filename, Int :$expected) {
    my $output = compute :$filename;
    unless $expected ~~ $output {
        say "$filename error: expected " ~ $expected ~ " but got " ~ $output;
        exit 1;
    }
}

sub compute(Str :$filename --> Int) {
    my Str $input = $filename.IO.slurp;

    my Moon @moons = Input.parse($input, :actions(Actions.new)).made or die "Parse failed";

    my %x;
    my %y;
    my %z;
    my Int $i = 0;
    my Int $px = 0;
    my Int $py = 0;
    my Int $pz = 0;

    while $px == 0 || $py == 0 || $pz == 0 {
        my ($x1, $x2, $x3, $x4) = @moons».x;
        my ($vx1, $vx2, $vx3, $vx4) = @moons».vx;
        my ($y1, $y2, $y3, $y4) = @moons».y;
        my ($vy1, $vy2, $vy3, $vy4) = @moons».vy;
        my ($z1, $z2, $z3, $z4) = @moons».z;
        my ($vz1, $vz2, $vz3, $vz4) = @moons».vz;
        if $px == 0 && (%x{$x1;$x2;$x3;$x4;$vx1;$vx2;$vx3;$vx4}:exists) {
            $px = $i - %x{$x1;$x2;$x3;$x4;$vx1;$vx2;$vx3;$vx4};
        } else {
            %x{$x1;$x2;$x3;$x4;$vx1;$vx2;$vx3;$vx4} = $i;
        }
        if $py == 0 && (%y{$y1;$y2;$y3;$y4;$vy1;$vy2;$vy3;$vy4}:exists) {
            $py = $i - %y{$y1;$y2;$y3;$y4;$vy1;$vy2;$vy3;$vy4};
        } else {
            %y{$y1;$y2;$y3;$y4;$vy1;$vy2;$vy3;$vy4} = $i;
        }
        if $pz == 0 && (%z{$z1;$z2;$z3;$z4;$vz1;$vz2;$vz3;$vz4}:exists) {
            $pz = $i - %z{$z1;$z2;$z3;$z4;$vz1;$vz2;$vz3;$vz4};
        } else {
            %z{$z1;$z2;$z3;$z4;$vz1;$vz2;$vz3;$vz4} = $i;
        }
        for @moons -> $m {
            for @moons -> $n {
                $m.gravity($n)
            }
        }
        for @moons -> $m {
            $m.velocity();
        }
        $i++;
    }
    say "$i $px $py $pz";
    return [lcm] ($px, $py, $pz);
}

check :filename("example1") :expected(2772);
check :filename("example2") :expected(4686774924);

say compute(:filename("input"));
