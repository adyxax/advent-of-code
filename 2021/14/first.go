package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
)

type transform struct {
	first  byte
	second byte
	res    byte
	count  int
}

type letter struct {
	value byte
	count int
}

var (
	pairs   []transform
	letters []letter
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)

	scanner.Scan()
	polymer := scanner.Text()
out:
	for i := 0; i < len(polymer); i++ {
		for j := range letters {
			if letters[j].value == polymer[i] {
				letters[j].count++
				continue out
			}
		}
		letters = append(letters, letter{polymer[i], 1})
	}
	scanner.Scan()

	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " -> ")
		pairs = append(pairs, transform{line[0][0], line[0][1], line[1][0], 0})
	}
	for i := 0; i < len(polymer)-1; i++ {
		for j, v := range pairs {
			if v.first == polymer[i] && v.second == polymer[i+1] {
				pairs[j].count++
			}
		}
	}
	newPairs := make([]transform, len(pairs))
	copy(newPairs, pairs)
	for s := 1; s < 10; s++ {
		for j := range newPairs {
			newPairs[j].count = 0
		}
		for _, a := range pairs {
			if a.count > 0 {
				for j, b := range pairs {
					if b.first == a.first && b.second == a.res {
						newPairs[j].count += a.count
						break
					}
				}
				for j, b := range pairs {
					if b.first == a.res && b.second == a.second {
						newPairs[j].count += a.count
						break
					}
				}
			}
		}
		copy(pairs, newPairs)
	out2:
		for _, v := range pairs {
			for j := range letters {
				if letters[j].value == v.res {
					letters[j].count += v.count
					continue out2
				}
			}
			letters = append(letters, letter{v.res, v.count})
		}
	}
	sort.Slice(letters, func(i, j int) bool {
		return letters[i].count < letters[j].count
	})
	fmt.Println(letters[len(letters)-1].count - letters[0].count)
}
