package main

import (
	"fmt"
)

func compute1(filename string) error {
	r, err := parseInputFile(filename)
	if err != nil {
		return fmt.Errorf("failed to parse input file: %w", err)
	}
	return nil
}
