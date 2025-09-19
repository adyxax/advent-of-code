package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"git.adyxax.org/adyxax/advent-of-code/2019/11-Space_Police/computer"
	"git.adyxax.org/adyxax/advent-of-code/2019/11-Space_Police/robot"
)

func compute(filename string) error {
	inputStr, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("failed to read file: %w", err)
	}
	inputStrs := strings.Split(strings.TrimSpace(string(inputStr)), ",")
	memory := make(map[int]int, len(inputStrs))
	for i, p := range inputStrs {
		n, err := strconv.Atoi(p)
		if err != nil {
			return fmt.Errorf("failed to parse %s int at %d: %w", p, i, err)
		}
		memory[i] = n
	}
	c := computer.Computer{
		Memory: memory,
	}
	input := []int{0}
	r := robot.NewRobot()
	for !c.Halted {
		output := c.Run(input)
		input[0] = int(r.Step(robot.Color(output[0]), robot.Rotation(output[1])))
	}
	fmt.Printf("%d\n", len(r.GetPaintedTiles()))
	return nil
}

func main() {
	if err := compute("input"); err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		os.Exit(1)
	}
}
