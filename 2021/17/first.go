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

	// We need to calculate which steps and horizontal speeds can hit the target box
	x := 0
	vx := 0 // its a vx max more than a vx
	i := 1
	for {
		if x+vx+1 <= x2 {
			vx += 1
			x += vx
			i++
		} else {
			break
		}
	}

	hmax := 0
	for nextVy := 0; nextVy < 10000; nextVy++ {
		y := 0
		vy := nextVy
		tmpHmax := 0
		j := 0
		for {
			if y+vy >= y1 {
				y += vy
				vy--
				j++
			} else {
				break
			}
			if tmpHmax < y {
				tmpHmax = y
			}
		}
		if y >= y1 && y <= y2 && hmax < tmpHmax {
			hmax = tmpHmax
			fmt.Printf("h=%d, y=%d, vy=%d, j=%d\n", hmax, y, nextVy, j)
		}
	}

	fmt.Printf("x1=%d, x2=%d, y1=%d, y2=%d\n", x1, x2, y1, y2)
	fmt.Printf("i=%d, x=%d, vx=%d\n", i, x, vx)
	fmt.Println(hmax)
}
