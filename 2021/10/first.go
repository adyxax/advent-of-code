package main

import (
	"bufio"
	"fmt"
	"os"
)

type stack []byte

func (s *stack) push(b byte) {
	*s = append(*s, b)
}

func (s *stack) pop() *byte {
	l := len(*s)
	if l == 0 {
		return nil
	} else {
		elt := (*s)[l-1]
		*s = (*s)[:l-1]
		return &elt
	}
}

func main() {
	score := 0
	s := make(stack, 0)

	scanner := bufio.NewScanner(os.Stdin)
out:
	for scanner.Scan() {
		line := scanner.Text()
		for i := 0; i < len(line); i++ {
			c := line[i]
			if c == '(' || c == '[' || c == '{' || c == '<' {
				s.push(c)
				continue
			}
			b := s.pop()
			switch c {
			case ')':
				if *b != '(' {
					score += 3
					continue out
				}
			case ']':
				if *b != '[' {
					score += 57
					continue out
				}
			case '}':
				if *b != '{' {
					score += 1197
					continue out
				}
			case '>':
				if *b != '<' {
					score += 25137
					continue out
				}
			}
		}
	}
	fmt.Println(score)
}
