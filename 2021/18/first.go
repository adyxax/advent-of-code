package main

import (
	"fmt"
	"log"
	"os"

	"git.adyxax.org/aoc/2021/18/pairs"
)

func main() {
	parser := pairs.NewParser(os.Stdin)
	pair, err := parser.Parse()
	if err != nil {
		log.Fatalf("%w", err)
	}
	for {
		pair2, err := parser.Parse()
		if err != nil {
			break
		}
		pair = pair.Add(pair2)
	}
	fmt.Println(pair)
	pair.Reduce()
	fmt.Println(pair)
	fmt.Println(pair.Magnitude())
}
