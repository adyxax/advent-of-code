package main

import (
	"fmt"
	"os"

	"git.adyxax.org/aoc/2021/18/pairs"
)

func main() {
	max := 0
	parser := pairs.NewParser(os.Stdin)
	pairs := make([]*pairs.Pair, 0)
	for {
		pair, err := parser.Parse()
		if err != nil {
			break
		}
		pairs = append(pairs, pair)
	}
	l := len(pairs)
	for i := 0; i < l; i++ {
		for j := 0; j < l; j++ {
			if i == j {
				continue
			}
			p1 := pairs[i].DeepCopy()
			p2 := pairs[j].DeepCopy()
			if m := p1.Add(p2).Magnitude(); max < m {
				max = m
			}
		}
	}
	fmt.Println(max)
}
