package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"git.adyxax.org/adyxax/advent-of-code/2019/15-Oxygen_System/computer"
	"git.adyxax.org/adyxax/advent-of-code/2019/15-Oxygen_System/robot"
)

func parseInputFile(filename string) (*robot.Robot, error) {
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
	r := robot.NewRobot()
	for {
		m := r.Step()
		if m == robot.NONE {
			break
		}
		out := c.Run([]int{int(m)})
		r.Record(m, robot.Tile(out[0]))
	}
	r.PrintTiles()
	return r, nil
}

func main() {
	r, err := parseInputFile("input")
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to parse input file: %w", err)
		os.Exit(1)
	}
	fmt.Printf("%d\n", r.DistanceToOxygen())
	fmt.Printf("%d\n", r.Fill())
}
