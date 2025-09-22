package main

import (
	"math/big"
	"testing"
)

func TestCompute2(t *testing.T) {
	tests := []struct {
		expected *big.Int
		filename string
	}{
		{big.NewInt(2772), "example1"},
		{big.NewInt(4686774924), "example2"},
	}
	for _, tt := range tests {
		e, err := compute2(tt.filename)
		if err != nil {
			t.Fatalf("failed %s: %+v", tt.filename, err)
		}
		if tt.expected.Cmp(e) != 0 {
			t.Fatalf("failed %s: got %s but expected %s", tt.filename, e.String(), tt.expected.String())
		}
	}
}
