package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	f, err := os.Open("input")
	if err != nil {
		log.Fatalf("%+v", err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	scanner.Split(bufio.ScanLines)

	aim := 0
	pos := 0
	depth := 0
	for scanner.Scan() {
		elts := strings.Split(scanner.Text(), " ")
		i, err := strconv.Atoi(elts[1])
		if err != nil {
			log.Fatalf("%+v", err)
		}
		switch elts[0] {
		case "forward":
			pos += i
			depth += i * aim
		case "down":
			aim += i
		case "up":
			aim -= i
		default:
			panic(elts[0])
		}
	}
	fmt.Println(pos * depth)
}
