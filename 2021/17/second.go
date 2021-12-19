package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type vector struct {
	x, y int
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Scan()
	line := scanner.Text()
	elts := strings.Split(line, "..")
	eltsx1 := strings.Split(elts[0], "=")
	x1, _ := strconv.Atoi(eltsx1[1])
	eltsx2 := strings.Split(elts[1], ",")
	x2, _ := strconv.Atoi(eltsx2[0])
	eltsy1 := strings.Split(elts[1], "=")
	y1, _ := strconv.Atoi(eltsy1[1])
	y2, _ := strconv.Atoi(elts[2])

	possible := 0
	for nextVx := 0; nextVx <= x2; nextVx++ {
	out:
		for nextVy := y1; nextVy < 1000; nextVy++ {
			vx := nextVx
			vy := nextVy
			x := 0
			y := 0
			for {
				if x >= x1 && x <= x2 && y >= y1 && y <= y2 {
					possible++
					continue out
				}
				if x > x2 || y < y1 {
					break
				}
				if vx >= 0 {
					x += vx
					vx--
				}
				y += vy
				vy--
			}
		}
	}
	fmt.Println(possible)
}
