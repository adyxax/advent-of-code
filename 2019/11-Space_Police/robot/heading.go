package robot

import "fmt"

type heading int

const (
	N heading = 0
	E heading = 1
	S heading = 2
	W heading = 3
)

func (h heading) rotate(r Rotation) heading {
	if r == LEFT {
		return (h + 3) % 4
	} else if r == RIGHT {
		return (h + 1) % 4
	}
	panic(fmt.Errorf("invalid rotation %d", r))
}
