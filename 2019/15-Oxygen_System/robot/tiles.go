package robot

import "fmt"

type Tile int

const (
	FLOOR  Tile = 1
	OXYGEN Tile = 2
	WALL   Tile = 0
)

func (t Tile) String() string {
	switch t {
	case FLOOR:
		return "."
	case OXYGEN:
		return "O"
	case WALL:
		return "#"
	}
	panic(fmt.Errorf("invalid tile: %d", t))
}
