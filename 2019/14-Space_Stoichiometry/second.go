package main

import "fmt"

func compute2(filename string, ore int) (int, error) {
	factories, err := parseInput(filename)
	if err != nil {
		return 0, fmt.Errorf("failed to parse factories: %w", err)
	}

	symbols := orderFactorySymbols(factories)

	t := 1000000000000
	l := t / ore
	h := t
	for n := h / 2; h-l > 1; n = l + (h-l)/2 {
		if computeOnly1(factories, symbols, n) > t {
			h = n
		} else {
			l = n
		}
	}

	return l, nil
}
