package main

import (
	"bufio"
	"fmt"
	"os"

	"git.adyxax.org/aoc/2021/05/line"
)

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
		if l.X1 == l.X2 {
			if l.Y1 > l.Y2 {
				l.Y1, l.Y2 = l.Y2, l.Y1
			}
			for i := l.Y1; i <= l.Y2; i++ {
				matrix[l.X1][i]++
			}
		} else if l.Y1 == l.Y2 {
			if l.X1 > l.X2 {
				l.X1, l.X2 = l.X2, l.X1
			}
			for i := l.X1; i <= l.X2; i++ {
				matrix[i][l.Y1]++
			}
		} else {
			if l.X1 > l.X2 {
				l.X1, l.X2, l.Y1, l.Y2 = l.X2, l.X1, l.Y2, l.Y1
			}
			if l.Y1 < l.Y2 {
				for i := 0; i <= l.X2-l.X1; i++ {
					matrix[l.X1+i][l.Y1+i]++
				}
			} else {
				for i := 0; i <= l.X2-l.X1; i++ {
					matrix[l.X1+i][l.Y1-i]++
				}
			}
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
