package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

func sortStrings(strs []string) {
	for i, v := range strs {
		s := strings.Split(v, "")
		sort.Strings(s)
		strs[i] = strings.Join(s, "")
	}
}

func deduceSix(one string, zns []string) string { // zns == zero, nine and six
	for _, v := range zns {
		for _, c := range one {
			if !strings.Contains(v, string(c)) {
				return v
			}
		}
	}
	log.Fatalf("deduce(%+v, %+v) failed!", one, zns)
	return ""
}

func deduceTwoThreeFive(ttf []string, topRight, bottomRight byte) (two string, three string, five string) {
	for _, s := range ttf {
		if !strings.Contains(s, string(topRight)) {
			five = s
		} else if !strings.Contains(s, string(bottomRight)) {
			two = s
		} else {
			three = s
		}
	}
	return
}

func deduceRight(one, six string) (byte, byte) {
	if strings.Contains(six, string(one[0])) {
		return one[1], one[0]
	} else {
		return one[0], one[1]
	}
}

func deduceBottomLeft(one, two, five string) byte {
	for _, ch := range two {
		s := string(ch)
		if strings.Contains(one, s) || strings.Contains(five, s) {
			continue
		}
		return s[0]
	}
	log.Fatalf("deduceBottomLeft(%+v, %+v, %+v) failed!", one, two, five)
	return 0 // cannot happen
}

func deduceZeroNine(zsn []string, six string, bottomLeft byte) (zero string, nine string) {
	for _, s := range zsn {
		if s == six {
			continue
		}
		if strings.Contains(s, string(bottomLeft)) {
			zero = s
		} else {
			nine = s
		}
	}
	return
}

func main() {
	score := 0

	s := bufio.NewScanner(os.Stdin)
	for s.Scan() {
		inputs := strings.Split(s.Text(), " | ")
		hints := strings.Split(inputs[0], " ")
		outputs := strings.Split(inputs[1], " ")
		sort.Slice(hints, func(i, j int) bool {
			return len(hints[i]) < len(hints[j])
		})
		sortStrings(hints)
		sortStrings(outputs)

		// some digits are straightforward :
		one := hints[0]
		seven := hints[1]
		four := hints[2]
		eight := hints[9]

		// We can deduce number 6 by being in len 6 and missing a component from 1
		six := deduceSix(one, hints[6:9])
		// the right segments can be deduced from the one in 1 that is in the 6
		topRight, bottomRight := deduceRight(one, six)
		// We can deduce numbers two, three and five from the right segments
		two, three, five := deduceTwoThreeFive(hints[3:6], topRight, bottomRight)
		// We can deduce the bottom left segment from 1 2 and 5
		bottomLeft := deduceBottomLeft(one, two, five)
		// We can deduce numbers 0 and 9 from the bottom left segment and the fact we already know 6
		zero, nine := deduceZeroNine(hints[6:9], six, bottomLeft)

		key := make([]byte, 4)
		for i, s := range outputs {
			switch s {
			case zero:
				key[i] = '0'
			case one:
				key[i] = '1'
			case two:
				key[i] = '2'
			case three:
				key[i] = '3'
			case four:
				key[i] = '4'
			case five:
				key[i] = '5'
			case six:
				key[i] = '6'
			case seven:
				key[i] = '7'
			case eight:
				key[i] = '8'
			case nine:
				key[i] = '9'
			default:
				fmt.Println(zero, one, two, three, four, five, six, seven, eight, nine)
				log.Fatalf("Did not find %+v", s)
			}
		}
		v, _ := strconv.Atoi(string(key))
		score += v
	}
	fmt.Println(score)
}
