package robot

import (
	"fmt"
	"math"
)

type Robot struct {
	h     heading
	tiles map[point]Color
	p     point
}

func NewRobot() *Robot {
	return &Robot{
		tiles: make(map[point]Color),
	}
}

func (r *Robot) GetPaintedTiles() map[point]Color {
	return r.tiles
}

func (r *Robot) PrintTiles() {
	x0 := math.MaxInt
	x1 := -math.MaxInt
	y0 := math.MaxInt
	y1 := -math.MaxInt
	for k, _ := range r.tiles {
		if k.x < x0 {
			x0 = k.x
		}
		if k.x > x1 {
			x1 = k.x
		}
		if k.y < y0 {
			y0 = k.y
		}
		if k.y > y1 {
			y1 = k.y
		}
	}
	out := ""
	for j := y0; j <= y1; j++ {
		for i := x0; i <= x1; i++ {
			c := " "
			if t, ok := r.tiles[point{x: i, y: j}]; ok && t == WHITE {
				c = "#"
			}
			out += c
		}
		out += "\n"
	}
	fmt.Print(out)
}

func (r *Robot) Step(c Color, rot Rotation) Color {
	r.tiles[r.p] = c
	r.h = r.h.rotate(rot)
	r.p.step(r.h)
	if c, ok := r.tiles[r.p]; ok {
		return c
	}
	return BLACK
}
