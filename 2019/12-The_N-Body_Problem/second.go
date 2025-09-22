package main

import (
	"fmt"
	"math/big"

	"git.adyxax.org/adyxax/advent-of-code/2019/12-The_N-Body_Problem/moon"
)

func compute2(filename string) (*big.Int, error) {
	moons, err := parseMoonsFromFile(filename)
	if err != nil {
		return nil, fmt.Errorf("failed to parse moons: %w", err)
	}
	l := 4
	history := moon.NewHistory()
	for step := int64(0); true; step++ {
		if history.Append(step, moons) {
			break
		}
		for m := 0; m < l; m++ {
			for n := m + 1; n < l; n++ {
				moons[m].Gravity(&moons[n])
			}
		}
		for m := 0; m < l; m++ {
			moons[m].Velocity()
		}
	}
	return history.Periodicity(), nil
}
