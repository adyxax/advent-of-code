package main

import (
	"bufio"
	"cmp"
	"fmt"
	"maps"
	"os"
	"slices"
	"strconv"
)

type item struct {
	symbol string
	units  int
}

type factory struct {
	dependencies []item
	symbol       string
	units        int
}

type factories map[string]*factory

func orderFactorySymbols(factories factories) []string {
	levels := make(map[string]int)
	levels["ORE"] = 0
outer1:
	for l := 0; len(levels) <= len(factories); l++ {
	outer2:
		for symbol, f := range factories {
			if _, ok := levels[symbol]; ok {
				continue
			}
			for _, d := range f.dependencies {
				if d.symbol == "ORE" {
					continue
				}
				if _, ok := levels[d.symbol]; !ok {
					continue outer2
				}
			}
			levels[symbol] = l
			continue outer1
		}
	}
	return slices.SortedFunc(maps.Keys(factories), func(a, b string) int {
		return -cmp.Compare(levels[a], levels[b])
	})
}

func (f *factory) parseDependencies(scanner *bufio.Scanner) error {
	units := scanner.Text()
	for {
		if !scanner.Scan() {
			return fmt.Errorf("unexpected EOF")
		}
		item, err := parseItem(units, scanner.Text())
		if err != nil {
			return fmt.Errorf("failed to parse item: %w", err)
		}
		f.dependencies = append(f.dependencies, *item)
		if !scanner.Scan() {
			return fmt.Errorf("unexpected EOF")
		}
		units = scanner.Text()
		if units == "=>" {
			return nil
		}
	}
}

func parseInput(filename string) (factories, error) {
	f, err := os.Open(filename)
	defer f.Close()
	if err != nil {
		return nil, fmt.Errorf("failed to open file: %w", err)
	}
	r := bufio.NewReader(f)
	scanner := bufio.NewScanner(r)
	scanner.Split(bufio.ScanWords)
	factories := make([]factory, 0)
	for scanner.Scan() {
		var f factory
		if err := f.parseDependencies(scanner); err != nil {
			return nil, fmt.Errorf("failed to parse dependencies: %w", err)
		}
		if !scanner.Scan() {
			return nil, fmt.Errorf("unexpected EOF")
		}
		units := scanner.Text()
		if !scanner.Scan() {
			return nil, fmt.Errorf("unexpected EOF")
		}
		item, err := parseItem(units, scanner.Text())
		if err != nil {
			return nil, fmt.Errorf("failed to parse item: %w", err)
		}
		f.symbol = item.symbol
		f.units = item.units
		factories = append(factories, f)
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("failed to scan: %w", err)
	}
	l := len(factories)
	fs := make(map[string]*factory, l)
	for i := 0; i < l; i++ {
		fs[factories[i].symbol] = &factories[i]
	}
	return fs, nil
}

func parseItem(units, symbol string) (*item, error) {
	if symbol[len(symbol)-1] == ',' {
		symbol = symbol[:len(symbol)-1]
	}
	u, err := strconv.Atoi(units)
	if err != nil {
		return nil, fmt.Errorf("failed to parse int: %w", err)
	}
	return &item{symbol, u}, nil
}

func main() {
	out, err := compute1("input")
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		os.Exit(1)
	}
	fmt.Printf("%d\n", out)
	i, err := compute2("input", out)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		os.Exit(1)
	}
	fmt.Printf("%d\n", i)
}
