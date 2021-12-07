package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func findBitCount(data []string) []int {
	bits := make([]int, 12)
	for _, line := range data {
		for n := 0; n < 12; n++ {
			if line[n] == '1' {
				bits[n]++
			}
		}
	}
	return bits
}

func main() {
	f, err := os.Open("input")
	if err != nil {
		log.Fatalf("%+v", err)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	scanner.Split(bufio.ScanLines)

	data := make([]string, 0)
	for scanner.Scan() {
		data = append(data, scanner.Text())
	}

	datacsr := make([]string, len(data))
	copy(datacsr, data)

	var ogr int
	for n := 0; n < 12; n++ {
		if len(data) == 1 {
			break
		}
		bits := findBitCount(data)
		var i byte
		if 2*bits[n] >= len(data) {
			i = '1'
		} else {
			i = '0'
		}
		newdata := make([]string, 0)
		for j := 0; j < len(data); j++ {
			if data[j][n] == i {
				newdata = append(newdata, data[j])
			}
		}
		data = newdata
	}
	for n := 0; n < 12; n++ {
		ogr *= 2
		if data[0][n] == '1' {
			ogr++
		}
	}

	data = datacsr
	var csr int
	for n := 0; n < 12; n++ {
		if len(data) == 1 {
			break
		}
		bits := findBitCount(data)
		var i byte
		if 2*bits[n] < len(data) {
			i = '1'
		} else {
			i = '0'
		}
		newdata := make([]string, 0)
		for j := 0; j < len(data); j++ {
			if data[j][n] == i {
				newdata = append(newdata, data[j])
			}
		}
		data = newdata
	}
	for n := 0; n < 12; n++ {
		csr *= 2
		if data[0][n] == '1' {
			csr++
		}
	}

	fmt.Println(ogr * csr)
}
