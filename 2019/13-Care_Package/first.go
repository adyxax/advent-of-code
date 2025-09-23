package main

import (
	"fmt"
)

func compute1(filename string) error {
	c, err := parseInputFile(filename)
	if err != nil {
		return fmt.Errorf("failed to parse input file: %w", err)
	}
	out := c.Run(nil)
	type point struct {
		x int
		y int
	}
	screen := make(map[point]tile)
	for i := 0; i < len(out); i += 3 {
		screen[point{out[i], out[i+1]}] = tile(out[i+2])
	}
	i := 0
	for _, tile := range screen {
		if tile == BLOCK {
			i++
		}
	}
	fmt.Printf("%d\n", i)
	return nil
}
