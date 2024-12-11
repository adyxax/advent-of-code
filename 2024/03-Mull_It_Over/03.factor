! Copyright (C) 2024 Julien (adyxax) Dessaux.
! See https://git.adyxax.org/adyxax/advent-of-code/tree/LICENSE for EUPL license.
USING: accessors io.encodings.utf8 io.files kernel make math math.parser peg
peg.parsers prettyprint regexp sequences ;
IN: aoc.2024.03

<PRIVATE

: load_input ( filename -- string )
    "~/git/adyxax/aoc/2024/03-Mull_It_Over/"
    swap append utf8 file-contents ;

! ----- Let's do part1 with regexes --------------------------------------------
: get_muls ( string -- instructions )
    R/ mul\(\d+,\d+\)/ all-matching-subseqs ;

: mul>result ( string -- n )
    R/ \d+/ all-matching-subseqs
    [ string>number ] map
    product ;

: part1 ( filename -- n )
    load_input
    get_muls
    [ mul>result ] map
    sum ;

! ----- And part2 with a real parser -------------------------------------------
TUPLE: computer total multiplying? ;

GENERIC: compute ( computer item -- computer )
TUPLE: do ;
M: do compute
    drop
    t >>multiplying? ;

TUPLE: dont ;
M: dont compute
    drop
    f >>multiplying? ;

TUPLE: mul opX opY ;
M: mul compute
    over multiplying?>>
    [ [ opX>> ] [ opY>> ] bi *
      over total>> + >>total ]
    [ drop ]
    if ;

TUPLE: nop ;
M: nop compute
    drop ;

: parse_do ( -- parser )
    "do()" token hide [ drop do boa ] action ;

: parse_dont ( -- parser )
    "don't()" token hide [ drop dont boa ] action ;

: parse_mul ( -- parser )
    [ "mul(" token hide ,
      integer-parser ,
      "," token hide ,
      integer-parser ,
      ")" token hide ,
    ] seq*
    [ first2 mul boa ] action ;

: parse_nop ( -- parser )
    any-char hide [ drop nop boa ] action ;

PEG: parse_input ( string -- ast )
    parse_do
    parse_dont
    parse_mul
    parse_nop
    4choice
    repeat1 ;

: part2 ( filename -- n )
    load_input
    parse_input
    0 t computer boa
    swap
    [ compute ] each
    total>> ;

PRIVATE>

: aoc202403 ( -- )
    "input" part1 .
    "input" part2 . ;

MAIN: aoc202403
