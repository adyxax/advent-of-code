package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"git.adyxax.org/adyxax/advent-of-code/2019/12-The_N-Body_Problem/moon"
)

func parseMoonsFromFile(filename string) ([]moon.Moon, error) {
	f, err := os.Open(filename)
	defer f.Close()
	if err != nil {
		return nil, fmt.Errorf("failed to open file: %w", err)
	}
	r := bufio.NewReader(f)
	sc := bufio.NewScanner(r)
	var moons []moon.Moon
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			break
		}
		m, err := moon.NewMoon(line)
		if err != nil {
			return nil, fmt.Errorf("failed to create moon: %w", err)
		}
		moons = append(moons, *m)
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("failed to scan: %w", err)
	}
	return moons, nil
}

func main() {
	e, err := compute1("input", 1000)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		os.Exit(1)
	}
	fmt.Printf("%d\n", e)
	i, err := compute2("input")
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		os.Exit(1)
	}
	fmt.Printf("%d\n", i)
}
