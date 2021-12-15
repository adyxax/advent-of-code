package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"os"
)

type Node struct {
	risk       byte
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
	item.index = -1
	p[l] = nil
	*pq = p[0:l]
	return item
}
func (pq *NodeQueue) Update(n *Node, cost int) {
	n.cost = cost
	heap.Fix(pq, n.index)
}

var (
	costs    [][]Node
	nq       = make(NodeQueue, 1)
	size     int
	fullsize int
)

func calculateTentativeDistance(x, y, cost int) {
	if x < 0 || x >= size || y < 0 || y >= size {
		return
	}
	cost += int(costs[y][x].risk)
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
	fullsize = size * 5
	for y := 0; y < fullsize; y++ {
		costs = append(costs, make([]Node, fullsize))
		for x := 0; x < fullsize; x++ {
			costs[y][x].x = x
			costs[y][x].y = y
			costs[y][x].index = -1
		}
	}

	for y := 0; y < size; y++ {
		for x := 0; x < size; x++ {
			costs[y][x].risk = line[x] - '0'
			for i := 1; i < 5; i++ {
				costs[y][x+i*size].risk = (costs[y][x+(i-1)*size].risk % 9) + 1
			}
			for j := 1; j < 5; j++ {
				for i := 0; i < 5; i++ {
					costs[y+j*size][x+i*size].risk = (costs[y+(j-1)*size][x+i*size].risk % 9) + 1
				}
			}
		}
		scanner.Scan()
		line = scanner.Text()
	}
	size = fullsize
	costs[0][0].cost = 0
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
