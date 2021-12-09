package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	score := 0

	s := bufio.NewScanner(os.Stdin)
	for s.Scan() {
		inputs := strings.Split(s.Text(), " | ")
		output := strings.Split(inputs[1], " ")
		for _, v := range output {
			l := len(v)
			if l < 5 || l == 7 {
				score++
			}
		}
	}
	fmt.Println(score)
}
