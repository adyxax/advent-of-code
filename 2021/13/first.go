package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var (
	dots   [][]bool
	width  = -1
	height = -1
)

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func foldx(n int) {
	for y := 0; y < height; y++ {
		if len(dots[y]) < n {
			continue
		}
		if len(dots[y]) < width {
			dots[y] = append(dots[y], make([]bool, width-len(dots[y])+1)...)
		}
		for i := 1; i <= n; i++ {
			if dots[y][n+i] {
				dots[y][n-i] = true
			}
		}
	}
	width = n
}

func foldy(n int) {
	for i := 1; i <= n; i++ {
		dots[n-i] = append(dots[n-i], make([]bool, width-len(dots[n-i])+1)...)
		for x := 0; x < len(dots[n+i]); x++ {
			if dots[n+i][x] {
				dots[n-i][x] = true
			}
		}
	}
	height = n
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), ",")
		if len(line) < 2 {
			break
		}
		x, _ := strconv.Atoi(line[0])
		y, _ := strconv.Atoi(line[1])
		if y >= height {
			dots = append(dots, make([][]bool, y-height+1)...)
			height = y + 1
		}
		if x >= len(dots[y]) {
			dots[y] = append(dots[y], make([]bool, x-len(dots[y])+1)...)
			if x >= width {
				width = x + 1
			}
		}
		dots[y][x] = true
	}
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), "=")
		letter := line[0][11]
		n, _ := strconv.Atoi(line[1])
		if letter == 'y' {
			foldy(n)
		} else {
			foldx(n)
		}
		for y := 0; y < height; y++ {
			w := min(width, len(dots[y]))
			for x := 0; x < w; x++ {
				if dots[y][x] {
					fmt.Printf("#")
				} else {
					fmt.Printf(".")
				}
			}
			fmt.Println()
		}
		fmt.Println()
		fmt.Println("---")
		break
	}
	score := 0
	for y := 0; y < height; y++ {
		w := min(width, len(dots[y]))
		for x := 0; x < w; x++ {
			if dots[y][x] {
				score++
			}
		}
	}
	fmt.Println(score)
}
