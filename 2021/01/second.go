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
	A, err := strconv.Atoi(scanner.Text())
	if err != nil {
		log.Fatalf("%+v", err)
	}
	scanner.Scan()
	B, err := strconv.Atoi(scanner.Text())
	if err != nil {
		log.Fatalf("%+v", err)
	}
	A += B
	scanner.Scan()
	C, err := strconv.Atoi(scanner.Text())
	if err != nil {
		log.Fatalf("%+v", err)
	}
	B += C
	A += C

	result := 0

	for scanner.Scan() {
		D, err := strconv.Atoi(scanner.Text())
		if err != nil {
			log.Fatalf("%+v", err)
		}
		C += D
		B += D
		if B > A {
			result++
		}
		A = B
		B = C
		C = D
	}
	fmt.Println(result)
}
