package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
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

type Min struct {
	x, y int
}

var minimums []Min

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

	for y := 0; y < width; y++ {
		for x := 0; x < height; x++ {
			if isMin(x, y) {
				minimums = append(minimums, Min{x, y})
			}
		}
	}

	basins := make([]int, 0)
	for _, m := range minimums {
		basins = append(basins, computeBasinSize(m.x, m.y))
	}
	sort.Ints(basins)
	basins = basins[len(basins)-3:]
	fmt.Println(basins[0] * basins[1] * basins[2])
}

func computeBasinSize(x, y int) int {
	if x < 0 || x >= height || y < 0 || y >= width || grid[x][y] == '9' {
		return 0
	}
	grid[x][y] = '9'
	return 1 + computeBasinSize(x-1, y) + computeBasinSize(x+1, y) + computeBasinSize(x, y-1) + computeBasinSize(x, y+1)
}
