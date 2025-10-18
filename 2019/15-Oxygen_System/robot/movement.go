package robot

import "fmt"

type Movement int

const (
	NONE  Movement = 0
	EAST  Movement = 4
	NORTH Movement = 1
	SOUTH Movement = 2
	WEST  Movement = 3
)

func (m Movement) reverse() Movement {
	switch m {
	case EAST:
		return WEST
	case NORTH:
		return SOUTH
	case SOUTH:
		return NORTH
	case WEST:
		return EAST
	}
	panic(fmt.Errorf("invalid movement: %d", m))
}

func (m Movement) String() string {
	switch m {
	case EAST:
		return "E"
	case NORTH:
		return "N"
	case SOUTH:
		return "S"
	case WEST:
		return "W"
	}
	panic(fmt.Errorf("invalid movement: %d", m))
}
