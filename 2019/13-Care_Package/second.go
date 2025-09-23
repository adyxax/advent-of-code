package main

import (
	"fmt"
	"math"
)

func compute2(filename string) error {
	c, err := parseInputFile(filename)
	if err != nil {
		return fmt.Errorf("failed to parse input file: %w", err)
	}
	c.Set(0, 2)
	type point struct {
		x int
		y int
	}
	screen := make(map[point]tile)
	score := 0
	var in []int = nil
	for {
		out := c.Run(in)
		for i := 0; i < len(out); i += 3 {
			if out[i] == -1 {
				score = out[i+2]
			} else {
				screen[point{out[i], out[i+1]}] = tile(out[i+2])
			}
		}
		x0 := math.MaxInt
		x1 := -math.MaxInt
		y0 := math.MaxInt
		y1 := -math.MaxInt
		for p, _ := range screen {
			if p.x < x0 {
				x0 = p.x
			}
			if x1 < p.x {
				x1 = p.x
			}
			if p.y < y0 {
				y0 = p.y
			}
			if y1 < p.y {
				y1 = p.y
			}
		}
		s := ""
		for j := y0; j <= y1; j++ {
			for i := x0; i <= x1; i++ {
				if t, ok := screen[point{i, j}]; ok {
					s += t.String()
				} else {
					s += " "
				}
			}
			s += "\n"
		}
		fmt.Println(s)

		blocks := 0
		var paddle point
		var ball point
		for p, tile := range screen {
			switch tile {
			case BALL:
				ball = p
			case BLOCK:
				blocks++
			case PADDLE:
				paddle = p
			}
		}
		if blocks == 0 {
			break
		}
		switch {
		case paddle.x < ball.x:
			in = []int{1}
		case paddle.x > ball.x:
			in = []int{-1}
		default:
			in = []int{0}
		}
	}
	fmt.Printf("%d\n", score)
	return fmt.Errorf("not implemented")
}
