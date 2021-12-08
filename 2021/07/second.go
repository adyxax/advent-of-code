package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func min(a, b int) int {
	if a < b {
		return a
	} else {
		return b
	}
}
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

	sum := 0
	for _, i := range positions {
		sum += i
	}
	mean := sum / len(positions)

	// doing a mean with integers sucks
	total := 0
	total2 := 0
	for _, i := range positions {
		diff := abs(i - mean)
		total += diff * (diff + 1) / 2
		diff = abs(i - mean - 1)
		total2 += diff * (diff + 1) / 2
	}
	fmt.Println(min(total, total2))
}
