package main

import (
	"bufio"
	"fmt"
	"os"

	"git.adyxax.org/aoc/2021/16/bits"
)

var (
	score = 0
)

func computeScore(b *bits.Bits) {
	score += int(b.Version)
	for _, sub := range b.Operators {
		computeScore(sub)
	}
}

func main() {
	scanner := bits.NewScanner(bufio.NewReader(os.Stdin))
	for {
		b := scanner.Scan()
		fmt.Println(b)
		if b == nil {
			break
		}
		computeScore(b)
	}
	fmt.Println(score)
}
