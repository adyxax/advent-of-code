package main

import "testing"

func TestCompute1(t *testing.T) {
	tests := []struct {
		expected int
		filename string
		steps    int
	}{
		{179, "example1", 10},
		{1940, "example2", 100},
	}
	for _, tt := range tests {
		e, err := compute1(tt.filename, tt.steps)
		if err != nil {
			t.Fatalf("failed %s: %+v", tt.filename, err)
		}
		if e != tt.expected {
			t.Fatalf("failed %s: got %d but expected %d", tt.filename, e, tt.expected)
		}
	}
}
