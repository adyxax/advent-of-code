package robot

import "fmt"

type point struct {
	x int
	y int
}

func (p *point) step(h heading) {
	switch h {
	case N:
		p.y--
	case S:
		p.y++
	case E:
		p.x++
	case W:
		p.x--
	default:
		panic(fmt.Errorf("invalid heading %d", h))
	}
}
