package main

import (
	"fmt"
)

func computeOnly1(factories factories, symbols []string, expect int) int {
	materials := make(map[string]int)
	materials["FUEL"] = expect
	for _, symbol := range symbols {
		need, ok := materials[symbol]
		if !ok {
			continue
		}
		f := factories[symbol]
		for _, dep := range f.dependencies {
			n := need / f.units * dep.units
			if need%f.units > 0 {
				n += dep.units
			}
			if d, ok := materials[dep.symbol]; ok {
				materials[dep.symbol] = d + n
			} else {
				materials[dep.symbol] = n
			}
		}
		delete(materials, symbol)
	}
	return materials["ORE"]
}

func compute1(filename string) (int, error) {
	factories, err := parseInput(filename)
	if err != nil {
		return 0, fmt.Errorf("failed to parse factories: %w", err)
	}

	symbols := orderFactorySymbols(factories)

	return computeOnly1(factories, symbols, 1), nil
}
