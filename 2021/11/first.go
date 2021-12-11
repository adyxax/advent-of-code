package main

import (
	"bufio"
	"fmt"
	"os"
)

var (
	grid  = make([][]byte, 0)
	score = 0
)

func powerup(x, y int) {
	if x < 0 || x > 9 || y < 0 || y > 9 || grid[x][y] > 9 {
		return
	}
	grid[x][y]++
	if grid[x][y] > 9 {
		score++
		powerup(x-1, y-1)
		powerup(x, y-1)
		powerup(x+1, y-1)
		powerup(x-1, y)
		powerup(x+1, y)
		powerup(x-1, y+1)
		powerup(x, y+1)
		powerup(x+1, y+1)
	}
}
func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		l := make([]byte, len(line))
		for i := 0; i < len(line); i++ {
			l[i] = line[i] - '0'
		}
		grid = append(grid, l)
	}

	for i := 0; i < 100; i++ {
		for y := 0; y < 10; y++ {
			for x := 0; x < 10; x++ {
				powerup(x, y)
			}
		}
		for y := 0; y < 10; y++ {
			for x := 0; x < 10; x++ {
				if grid[x][y] > 9 {
					grid[x][y] = 0
				}
			}
		}
	}
	fmt.Println(score)
}
