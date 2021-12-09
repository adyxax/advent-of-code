package main

import (
	"bufio"
	"fmt"
	"os"
)

var (
	grid          [][]byte = make([][]byte, 0)
	width, height int
)

func isMin(x, y int) bool {
	var left, right, top, bottom byte
	if x == 0 {
		left = '9'
	} else {
		left = grid[x-1][y]
	}
	if x == height-1 {
		right = '9'
	} else {
		right = grid[x+1][y]
	}
	if y == 0 {
		top = '9'
	} else {
		top = grid[x][y-1]
	}
	if y == width-1 {
		bottom = '9'
	} else {
		bottom = grid[x][y+1]
	}
	return grid[x][y] < top && grid[x][y] < bottom && grid[x][y] < left && grid[x][y] < right
}

func main() {
	s := bufio.NewScanner(os.Stdin)
	s.Scan()
	width = len(s.Text())
	for {
		line := make([]byte, width)
		for i, ch := range s.Text() {
			line[i] = byte(ch)
		}
		grid = append(grid, line)
		if !s.Scan() {
			break
		}
	}
	height = len(grid)

	score := 0
	for y := 0; y < width; y++ {
		for x := 0; x < height; x++ {
			if isMin(x, y) {
				score += int(grid[x][y]-'0') + 1
			}
		}
	}
	fmt.Println(score)
}
