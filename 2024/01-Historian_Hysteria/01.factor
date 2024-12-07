! Copyright (C) 2024 Julien (adyxax) Dessaux.
! See https://git.adyxax.org/adyxax/advent-of-code/tree/LICENSE for EUPL license.
USING: assocs io io.encodings.utf8 io.files kernel math math.parser
math.statistics namespaces prettyprint sequences sorting splitting ;
IN: aoc.2024.01

<PRIVATE

: load_input ( filename -- array array )
    "~/git/adyxax/aoc/2024/01-Historian_Hysteria/"
    swap append utf8 file-lines
    [ split-words harvest ] map unzip
    [ [ string>number ] map ] bi@ ;

: part1 ( filename -- n )
    load_input
    [ sort ] bi@
    [ - abs ] 2map
    sum ;

: part2 ( filename -- n )
    load_input
    histogram ! turns an array into a hashtable of { value occurrences }
    '[ dup _ at 0 or * ] map
    sum ;

PRIVATE>

: aoc202401 ( -- )
    "input" part1 pprint nl
    "input" part2 pprint nl ;

MAIN: aoc202401
