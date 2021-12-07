package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	f, err := os.Open("input")
	if err != nil {
		log.Fatalf("%+v", err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	scanner.Split(bufio.ScanLines)

	bits := make([]int, 12)
	for scanner.Scan() {
		num := scanner.Text()
		for n := 0; n < 12; n++ {
			if num[n] == '1' {
				bits[n]++
			}
		}
	}

	gamma := 0
	epsilon := 0
	for n := 0; n < 12; n++ {
		gamma *= 2
		epsilon *= 2
		if bits[n] > 500 {
			gamma++
		} else {
			epsilon++
		}
	}
	fmt.Println(gamma * epsilon)
}
