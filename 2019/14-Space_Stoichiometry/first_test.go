package main

import "testing"

func TestCompute1(t *testing.T) {
	tests := []struct {
		expected int
		filename string
	}{
		{31, "example1"},
		{165, "example2"},
		{13312, "example3"},
		{180697, "example4"},
		{2210736, "example5"},
	}
	for _, tt := range tests {
		e, err := compute1(tt.filename)
		if err != nil {
			t.Fatalf("failed %s: %+v", tt.filename, err)
		}
		if e != tt.expected {
			t.Fatalf("failed %s: got %d but expected %d", tt.filename, e, tt.expected)
		}
	}
}
