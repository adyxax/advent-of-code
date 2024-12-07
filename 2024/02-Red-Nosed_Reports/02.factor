! Copyright (C) 2024 Julien (adyxax) Dessaux.
! See https://factorcode.org/license.txt for BSD license.
USING: combinators.short-circuit.smart grouping io io.encodings.utf8 io.files
kernel math math.order math.parser prettyprint sequences splitting ;
IN: aoc.2024.02

<PRIVATE

: load_input ( filename -- reports ) ! reports are a sequence of levels
    "~/git/adyxax/aoc/2024/02-Red-Nosed_Reports/"
    swap append utf8 file-lines
    [ split-words [ string>number ] map ] map ;

: bounded? ( levels -- ? )
    [ - abs 1 3 between? ] monotonic? ;

: any_monotonic? ( levels -- ? )
    { [ [ > ] monotonic? ]
      [ [ < ] monotonic? ]
    } || ;

: safe? ( levels -- ? )
    { [ any_monotonic? ] [ bounded? ] } && ;

: part1 ( filename -- n )
    load_input
    [ safe? ] count ;

: candidates ( report -- reports )
    dup length <iota> [ over remove-nth ] map nip ;

: safish? ( report -- ? )
    candidates [ safe? ] any? ;

: part2 ( filename -- n )
    load_input
    [ safish? ] count ;

PRIVATE>

: aoc202402 ( -- )
    "input" part1 .
    "input" part2 . ;

MAIN: aoc202402
