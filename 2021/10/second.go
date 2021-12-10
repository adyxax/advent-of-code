package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

type stack []byte

func (s *stack) clear() {
	*s = make(stack, 0)
}

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
	scores := make([]int, 0)
	s := make(stack, 0)

	scanner := bufio.NewScanner(os.Stdin)
out:
	for scanner.Scan() {
		line := scanner.Text()
		s.clear()
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
					continue out
				}
			case ']':
				if *b != '[' {
					continue out
				}
			case '}':
				if *b != '{' {
					continue out
				}
			case '>':
				if *b != '<' {
					continue out
				}
			}
		}
		score := 0
		for c := s.pop(); c != nil; c = s.pop() {
			score *= 5
			switch *c {
			case '(':
				score += 1
			case '[':
				score += 2
			case '{':
				score += 3
			case '<':
				score += 4
			}
		}
		if score != 0 {
			scores = append(scores, score)
		}
	}
	sort.Ints(scores)
	fmt.Println(scores[len(scores)/2])
}
