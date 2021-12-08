package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func abs(a int) int {
	if a > 0 {
		return a
	} else {
		return -a
	}
}

func splitOnCommas(data []byte, atEOF bool) (advance int, token []byte, err error) {
	var ch byte
	for advance, ch = range data {
		if ch < '0' || ch > '9' {
			break
		}
	}
	if advance > 0 {
		token = data[0:advance]
		advance++ // skip the comma
	}
	return
}

func main() {
	positions := make([]int, 0)

	s := bufio.NewScanner(os.Stdin)
	s.Split(splitOnCommas)
	for s.Scan() {
		n, _ := strconv.Atoi(s.Text())
		positions = append(positions, n)
	}

	sort.Ints(positions)
	medianPosition := len(positions) / 2
	median := 0
	if len(positions)%2 == 0 {
		median = positions[medianPosition]
	} else {
		median = (positions[medianPosition] + positions[medianPosition+1]) / 2
	}

	total := 0
	for _, i := range positions {
		total += abs(i - median)
	}
	fmt.Println(total)
}
