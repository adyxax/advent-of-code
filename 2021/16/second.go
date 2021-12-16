package main

import (
	"bufio"
	"fmt"
	"os"

	"git.adyxax.org/aoc/2021/16/bits"
)

func main() {
	scanner := bits.NewScanner(bufio.NewReader(os.Stdin))
	b := scanner.Scan()
	fmt.Println(b)
	fmt.Println(b.Value)
}
