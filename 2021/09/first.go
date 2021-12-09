package main

import (
	"bufio"
	"fmt"
	"os"
)

// DOES NOT WORK!!!

func countMins(prev []byte, prevS []bool) (score int) {
	for i, v := range prevS {
		if v {
			fmt.Println(prev[i])
			score += int(prev[i]) + 1
		}
	}
	return
}

func main() {
	score := 0

	s := bufio.NewScanner(os.Stdin)
	s.Scan()
	width := len(s.Text())
	prevS := make([]bool, width)
	prev := make([]byte, width)
	for i, ch := range s.Text() {
		prev[i] = byte(ch) - '0'
		if i == 0 {
			prevS[i] = true
		} else if prev[i] < prev[i-1] {
			prevS[i-1] = false
			prevS[i] = true
		}
	}

	curS := make([]bool, width)
	cur := make([]byte, width)
	fmt.Println(s.Text())
	for s.Scan() {
		for i, ch := range s.Text() {
			cur[i] = byte(ch) - '0'
			if i > 0 && cur[i] < cur[i-1] {
				curS[i-1] = false
				if cur[i] < prev[i] {
					prevS[i] = false
					curS[i] = true
				} else {
					curS[i] = false
				}
			} else {
				if cur[i] < prev[i] {
					prevS[i] = false
				}
				curS[i] = false
			}
		}
		fmt.Printf("%+v\n", prevS)
		fmt.Println(s.Text())
		score += countMins(prev, prevS)
		copy(prev, cur)
		copy(prevS, curS)
	}
	score += countMins(prev, prevS)
	fmt.Printf("%+v\n", prevS)
	fmt.Println(score)
}
