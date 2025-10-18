package robot

import (
	"container/heap"
	"fmt"
	"math"
)

type Robot struct {
	end   point
	pos   point
	tiles map[point]Tile
	trail []Movement
}

func NewRobot() *Robot {
	return &Robot{
		pos:   point{0, 0},
		trail: make([]Movement, 0),
		tiles: make(map[point]Tile),
	}
}

func (r *Robot) move(m Movement) {
	r.pos.step(m)
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
			if t, ok := r.tiles[point{x: i, y: j}]; ok {
				switch t {
				case FLOOR:
					c = "."
				case OXYGEN:
					c = "O"
				case WALL:
					c = "#"
				}
			}
			out += c
		}
		out += "\n"
	}
	fmt.Print(out)
}

func (r *Robot) Record(last Movement, result Tile) {
	r.tiles[r.pos] = result
	switch result {
	case OXYGEN:
		r.end = r.pos
	case WALL:
		r.move(last.reverse())
		r.trail = r.trail[:len(r.trail)-1]
	}
}

func (r *Robot) DistanceToOxygen() int {
	costs := make(map[point]int)
	pq := make(NodeQueue, 1)
	pq[0] = &Node{
		cost: 0,
		p:    point{0, 0},
	}
	heap.Init(&pq)
	for pq.Len() > 0 {
		n := heap.Pop(&pq).(*Node)
		switch r.tiles[n.p] {
		case OXYGEN:
			return n.cost
		case WALL:
			continue
		}
		if _, ok := costs[n.p]; ok {
			continue
		}
		costs[n.p] = n.cost
		var m Movement = 1
		for ; m <= 4; m++ {
			p := n.p
			p.step(m)
			if t, ok := r.tiles[p]; ok && t != WALL {
				heap.Push(&pq, &Node{
					cost: n.cost + 1,
					p:    p,
				})
			}
		}
	}
	panic("unreachable")
}

func (r *Robot) Fill() int {
	costs := make(map[point]int)
	maxcost := 0
	pq := make(NodeQueue, 1)
	pq[0] = &Node{
		cost: 0,
		p:    r.end,
	}
	heap.Init(&pq)
	for pq.Len() > 0 {
		n := heap.Pop(&pq).(*Node)
		if r.tiles[n.p] == WALL {
			continue
		}
		if _, ok := costs[n.p]; ok {
			continue
		}
		maxcost = n.cost
		costs[n.p] = n.cost
		var m Movement = 1
		for ; m <= 4; m++ {
			p := n.p
			p.step(m)
			if t, ok := r.tiles[p]; ok && t != WALL {
				heap.Push(&pq, &Node{
					cost: n.cost + 1,
					p:    p,
				})
			}
		}
	}
	return maxcost
}

func (r *Robot) Step() Movement {
	// are there unexplored tiles around us?
	var m Movement = 1
	for ; m <= 4; m++ {
		p := r.pos
		if _, ok := r.tiles[p.step(m)]; !ok {
			r.move(m)
			r.trail = append(r.trail, m)
			return m
		}
	}
	// Let's backtrack
	l := len(r.trail)
	if l > 0 {
		m := r.trail[l-1].reverse()
		r.move(m)
		r.trail = r.trail[:l-1]
		return m
	}
	return NONE
}
