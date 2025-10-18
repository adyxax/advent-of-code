package robot

import "fmt"

type point struct {
	x int
	y int
}

func (p *point) step(h Movement) point {
	switch h {
	case EAST:
		p.x++
	case NORTH:
		p.y--
	case SOUTH:
		p.y++
	case WEST:
		p.x--
	default:
		panic(fmt.Errorf("invalid heading %d", h))
	}
	return *p
}
