package main

import (
	"bufio"
	"fmt"
	"os"

	"git.adyxax.org/aoc/2021/05/line"
)

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func computeDelta(a, b int) int {
	if a < b {
		return 1
	} else if a > b {
		return -1
	}
	return 0
}

func main() {
	matrix := make([][]int, 1000)
	for i := 0; i < 1000; i++ {
		matrix[i] = make([]int, 1000)
	}

	parser := line.NewParser(bufio.NewReader(os.Stdin))
	for {
		l, err := parser.Parse()
		if err != nil {
			break
		}
		length := abs(l.X2 - l.X1)
		if length == 0 {
			length = abs(l.Y2 - l.Y1)
		}
		dx := computeDelta(l.X1, l.X2)
		dy := computeDelta(l.Y1, l.Y2)
		for i := 0; i <= length; i++ {
			matrix[l.X1+i*dx][l.Y1+i*dy]++
		}
	}
	score := 0
	for i := 0; i < 1000; i++ {
		for j := 0; j < 1000; j++ {
			if matrix[i][j] >= 2 {
				score++
			}
		}
	}
	fmt.Println(score)
}
