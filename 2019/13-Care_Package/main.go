package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"git.adyxax.org/adyxax/advent-of-code/2019/13-Care_Package/computer"
)

func parseInputFile(filename string) (*computer.Computer, error) {
	inputStr, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to read file: %w", err)
	}
	inputStrs := strings.Split(strings.TrimSpace(string(inputStr)), ",")
	memory := make(map[int]int, len(inputStrs))
	for i, p := range inputStrs {
		n, err := strconv.Atoi(p)
		if err != nil {
			return nil, fmt.Errorf("failed to parse %s int at %d: %w", p, i, err)
		}
		memory[i] = n
	}
	c := computer.Computer{
		Memory: memory,
	}
	return &c, nil
}

func main() {
	err := compute1("input")
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		os.Exit(1)
	}
	err = compute2("input")
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		os.Exit(1)
	}
}
