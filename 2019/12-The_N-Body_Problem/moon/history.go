package moon

import (
	"math/big"
)

type dataPoint [8]int

type dataHistory = map[dataPoint]int64

type History struct {
	px int64
	py int64
	pz int64
	x  dataHistory
	y  dataHistory
	z  dataHistory
}

func NewHistory() *History {
	var h History
	h.x = make(dataHistory)
	h.y = make(dataHistory)
	h.z = make(dataHistory)
	return &h
}

func (h *History) Append(n int64, moons []Moon) bool {
	if h.px == 0 {
		var dp [8]int
		for i := 0; i < 4; i++ {
			dp[i] = moons[i].x
			dp[i+4] = moons[i].vx
		}
		if p, ok := h.x[dp]; ok {
			h.px = n - p
		} else {
			h.x[dp] = n
		}
	}
	if h.py == 0 {
		var dp [8]int
		for i := 0; i < 4; i++ {
			dp[i] = moons[i].y
			dp[i+4] = moons[i].vy
		}
		if p, ok := h.y[dp]; ok {
			h.py = n - p
		} else {
			h.y[dp] = n
		}
	}
	if h.pz == 0 {
		var dp [8]int
		for i := 0; i < 4; i++ {
			dp[i] = moons[i].z
			dp[i+4] = moons[i].vz
		}
		if p, ok := h.z[dp]; ok {
			h.pz = n - p
		} else {
			h.z[dp] = n
		}
	}
	return h.px != 0 && h.py != 0 && h.pz != 0
}

func (h *History) Periodicity() *big.Int {
	return lcm3(
		big.NewInt(h.px),
		big.NewInt(h.py),
		big.NewInt(h.pz),
	)
}
