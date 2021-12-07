package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	f, err := os.Open("input")
	if err != nil {
		log.Fatalf("%+v", err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	scanner.Split(bufio.ScanLines)

	scanner.Scan()
	prev, err := strconv.Atoi(scanner.Text())
	if err != nil {
		log.Fatalf("%+v", err)
	}
	result := 0

	for scanner.Scan() {
		i, err := strconv.Atoi(scanner.Text())
		if err != nil {
			log.Fatalf("%+v", err)
		}
		if i > prev {
			result++
		}
		prev = i
	}
	fmt.Println(result)
}
