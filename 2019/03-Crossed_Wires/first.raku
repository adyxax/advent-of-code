#!/usr/bin/env raku

class Point {
    has Int:D $.x is required;
    has Int:D $.y is required;
    method distance(--> Int) { return $!x.abs + $!y.abs }
}

class Segment {
    has Point:D $.a is required;
    has Point:D $.b is required;
    has Bool:D $.horizontal is required;
    has Str:D $.path is required;
    method new(Point:D $a, Str:D $path --> Segment) {
        my $horizontal = False;
        my Point $b;
        my $len = $path.substr(1).Int;
        given $path.substr(0, 1) {
            when "U" { $b = Point.new :x($a.x) :y($a.y - $len); }
            when "D" { $b = Point.new :x($a.x) :y($a.y + $len); }
            when "L" { $b = Point.new :x($a.x - $len) :y($a.y); $horizontal = True; }
            when "R" { $b = Point.new :x($a.x + $len) :y($a.y); $horizontal = True; }
            default { THROW "invalid direction $_"; }
        }
        return self.bless(:$a, :$b, :$horizontal, :$path);
    }
}

class Wire {
    has Segment:D @.segments is required;
    method new(Str:D @moves --> Wire) {
        my Segment @segments;
        my $origin = Point.new :x(0) :y(0);
        for @moves -> $move {
            my $s = Segment.new($origin, $move);
            push @segments, $s;
            $origin = $s.b;
        }
        return self.bless(:@segments);
    }
}

sub segmentsIntersection(Segment:D $ab, Segment:D $cd --> Point) {
    return Nil if $ab.horizontal == $cd.horizontal;
    return samewith $cd, $ab if $ab.horizontal;
    if $cd.a.x < $cd.b.x {
        return Nil unless $cd.a.x < $ab.a.x <= $cd.b.x;
    } else {
        return Nil unless $cd.b.x <= $ab.a.x < $cd.a.x;
    }
    if $ab.a.y < $ab.b.y {
        return Nil unless $ab.a.y < $cd.a.y <= $ab.b.y;
    } else {
        return Nil unless $ab.b.y <= $cd.a.y < $ab.a.y;
    }
    return Point.new :x($ab.a.x) :y($cd.a.y);
}

sub wiresIntersection(Wire:D $ab, Wire:D $cd --> Point) {
    my Point $crossing;
    for $ab.segments -> $sab {
        for $cd.segments -> $scd {
            my $p = segmentsIntersection($sab, $scd);
            next unless $p.defined;
            next if $p.x == 0 && $p.y == 0;
            $crossing = $p if !$crossing.defined || $crossing.distance > $p.distance;
        }
    }
    return $crossing;
}

sub compute(Str:D $filename --> Int) {
    my Wire @wires;
    for $filename.IO.lines>>.split(',') -> @inputWire {
        my Str @input = @inputWire;
        push @wires, Wire.new(@input);
    }
    return wiresIntersection(@wires[0], @wires[1]).distance;
}

sub check(Str:D $filename, Int:D $expected) {
    my $output = compute($filename);
    if $expected != $output {
        say "$filename error: expected $expected but got $output";
        exit 1;
    }
}

check "example1", 6;
check "example2", 159;
check "example3", 135;
say compute "input";
