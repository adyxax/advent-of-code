package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type grille struct {
	lines   [][]int
	columns [][]int
}

var (
	best  = -1
	score = 0
)

// winning func
func win(g *grille, n int, iter int) {
	sum := 0
	for i := 0; i < 5; i++ {
		for j := 0; j < len(g.lines[i]); j++ {
			sum += g.lines[i][j]
		}
	}
	if iter > best {
		best = iter
		score = sum * n
		//fmt.Println(iter, n, score, g)
	}
}

func main() {
	f, err := os.Open("input")
	if err != nil {
		log.Fatalf("%+v", err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	scanner.Split(bufio.ScanLines)

	scanner.Scan()
	tirage := make([]int, 0)
	tirageStr := strings.Split(scanner.Text(), ",")
	// lets process the tirage
	for _, v := range tirageStr {
		n, err := strconv.Atoi(v)
		if err != nil {
			log.Fatalf("%+v", err)
		}
		tirage = append(tirage, n)
	}

	for scanner.Scan() {
		// we just scanned the new line
		// lets init the grille
		g := new(grille)
		g.lines = make([][]int, 5)
		g.columns = make([][]int, 5)
		for i := 0; i < 5; i++ {
			g.lines[i] = make([]int, 5)
			g.columns[i] = make([]int, 5)
		}
		// lets populate the grille
		for i := 0; i < 5; i++ {
			scanner.Scan()
			numbers := strings.Fields(scanner.Text())
			for j := 0; j < 5; j++ {
				n, err := strconv.Atoi(numbers[j])
				if err != nil {
					log.Fatalf("%+v", err)
				}
				g.lines[i][j] = n
				g.columns[j][i] = n
			}
		}
		// lets process the tirage
	out:
		for iter, n := range tirage {
			// remove the number
			for i := 0; i < 5; i++ {
				for j := 0; j < len(g.lines[i]); j++ {
					if g.lines[i][j] == n {
						if len(g.lines[i]) == 1 {
							g.lines[i] = []int{}
							win(g, n, iter)
							break out
						}
						if j < len(g.lines[i])-1 {
							g.lines[i][j] = g.lines[i][len(g.lines[i])-1]
						}
						g.lines[i] = g.lines[i][:len(g.lines[i])-1]
					}
				}
			}
			for i := 0; i < 5; i++ {
				for j := 0; j < len(g.columns[i]); j++ {
					if g.columns[i][j] == n {
						if len(g.columns[i]) == 1 {
							g.columns[i] = []int{}
							win(g, n, iter)
							break out
						}
						if j < len(g.columns[i])-1 {
							g.columns[i][j] = g.columns[i][len(g.columns[i])-1]
						}
						g.columns[i] = g.columns[i][:len(g.columns[i])-1]
					}
				}
			}
		}
	}
	fmt.Println(score)
}
