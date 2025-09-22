package main

import "fmt"

func compute1(filename string, steps int) (int, error) {
	moons, err := parseMoonsFromFile(filename)
	if err != nil {
		return 0, fmt.Errorf("failed to parse moons: %w", err)
	}
	l := len(moons)
	for i := 0; i < steps; i++ {
		for m := 0; m < l; m++ {
			for n := m + 1; n < l; n++ {
				moons[m].Gravity(&moons[n])
			}
		}
		for m := 0; m < l; m++ {
			moons[m].Velocity()
		}
	}
	e := 0
	for m := 0; m < l; m++ {
		e += moons[m].Energy()
	}
	return e, nil
}
