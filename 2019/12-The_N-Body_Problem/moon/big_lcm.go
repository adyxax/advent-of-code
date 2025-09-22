package moon

import "math/big"

// Least Common Multiple
func lcm(a, b *big.Int) *big.Int {
	var d, g, o big.Int
	g.GCD(nil, nil, a, b)
	d.Div(a, &g)
	return o.Mul(&d, b)
}

func lcm3(a, b, c *big.Int) *big.Int {
	d := lcm(a, b)
	return lcm(d, c)
}
