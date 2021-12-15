package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
)

type Node struct {
	x, y, cost int
	index      int
}
type NodeQueue []*Node

func (pq NodeQueue) Len() int           { return len(pq) }
func (pq NodeQueue) Less(i, j int) bool { return pq[i].cost < pq[j].cost }
func (pq NodeQueue) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].index = i
	pq[j].index = j
}
func (pq *NodeQueue) Push(x interface{}) {
	item := x.(*Node)
	item.index = len(*pq)
	*pq = append(*pq, item)
}
func (pq *NodeQueue) Pop() interface{} {
	p := *pq
	l := len(p) - 1
	item := p[l]
	p[l] = nil
	*pq = p[0:l]
	return item
}
func (pq *NodeQueue) Update(n *Node, cost int) {
	n.cost = cost
	heap.Fix(pq, n.index)
}

var (
	grid  [][]byte
	costs [][]Node
	nq    = make(NodeQueue, 1)
	size  int
)

func calculateTentativeDistance(x, y, cost int) {
	if x < 0 || x >= size || y < 0 || y >= size {
		return
	}
	cost += int(grid[y][x])
	if costs[y][x].cost == 0 {
		costs[y][x].cost = cost
		heap.Push(&nq, &costs[y][x])
	} else if cost < costs[y][x].cost {
		nq.Update(&costs[y][x], cost)
	}
}
func main() {
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Scan()
	line := scanner.Text()
	size = len(line)

	y := 0
	for {
		l := make([]byte, size)
		for i := 0; i < size; i++ {
			l[i] = line[i] - '0'
		}
		grid = append(grid, l)
		c := make([]Node, size)
		for j := 0; j < size; j++ {
			c[j] = Node{x: j, y: y}
		}
		costs = append(costs, c)
		if !scanner.Scan() {
			break
		}
		line = scanner.Text()
		y++
	}
	grid[0][0] = 0
	nq[0] = &costs[0][0]
	heap.Init(&nq)

	for {
		n := heap.Pop(&nq).(*Node)
		if n.x == size-1 && n.y == size-1 {
			fmt.Println(n.cost)
			break
		}
		calculateTentativeDistance(n.x-1, n.y, n.cost)
		calculateTentativeDistance(n.x+1, n.y, n.cost)
		calculateTentativeDistance(n.x, n.y-1, n.cost)
		calculateTentativeDistance(n.x, n.y+1, n.cost)
	}
}
