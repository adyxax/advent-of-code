package main

import "fmt"

type tile int

const (
	BALL   tile = 4
	BLOCK  tile = 2
	EMPTY  tile = 0
	PADDLE tile = 3
	WALL   tile = 1
)

func (t tile) String() string {
	switch t {
	case BALL:
		return "@"
	case BLOCK:
		return "B"
	case EMPTY:
		return " "
	case PADDLE:
		return "-"
	case WALL:
		return "#"
	}
	panic(fmt.Errorf("invalid tile: %d", t))
}
