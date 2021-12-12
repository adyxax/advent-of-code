package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type cave struct {
	name  string
	big   bool
	leafs []int // index in caves array
}

var (
	caves = []cave{cave{name: "start"}, cave{name: "end"}}
	score = 0
)

const (
	START = 0
	END   = 1
)

func getIdOrAdd(name string) int {
	for i, v := range caves {
		if v.name == name {
			return i
		}
	}
	caves = append(caves, cave{
		name: name,
		big:  name[0] >= 'A' && name[0] <= 'Z',
	})
	return len(caves) - 1
}

func contains(id int, passed []int) bool {
	for _, v := range passed {
		if v == id {
			return true
		}
	}
	return false
}

func parcours(id int, passed []int) {
	if !caves[id].big {
		if contains(id, passed) {
			return
		}
		passed = append(passed, id)
	}
	for _, v := range caves[id].leafs {
		if v == END {
			score++
			continue
		}
		parcours(v, passed)
	}
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), "-")
		a := getIdOrAdd(line[0])
		b := getIdOrAdd(line[1])
		caves[a].leafs = append(caves[a].leafs, b)
		caves[b].leafs = append(caves[b].leafs, a)
	}
	parcours(START, nil)
	fmt.Println(score)
}
