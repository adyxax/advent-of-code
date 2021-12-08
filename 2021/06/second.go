package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	g := make([]int, 9)

	s := bufio.NewReader(os.Stdin)
	for {
		buf := make([]byte, 2)
		if n, _ := s.Read(buf); n == 0 {
			break
		}
		g[buf[0]-'0']++
	}
	for d := 0; d <= 256; d++ {
		fmt.Printf("%+v\n", g)
		n := g[0]
		for i := 0; i < 8; i++ {
			g[i] = g[i+1]
		}
		g[6] += n
		g[8] = n
	}
	total := 0
	for i := 0; i < 8; i++ {
		total += g[i]
	}
	fmt.Printf("%d\n", total)
}
