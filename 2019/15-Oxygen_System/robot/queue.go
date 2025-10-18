package robot

import "container/heap"

type Node struct {
	cost  int
	p     point
	index int
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
