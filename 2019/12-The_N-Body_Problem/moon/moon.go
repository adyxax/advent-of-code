package moon

import "fmt"

type Moon struct {
	x  int
	y  int
	z  int
	vx int
	vy int
	vz int
}

func (m *Moon) Energy() int {
	return (abs(m.x) + abs(m.y) + abs(m.z)) * (abs(m.vx) + abs(m.vy) + abs(m.vz))
}

func gravityAdjust(ax int, avx *int, bx int, bvx *int) {
	if ax < bx {
		*avx++
		*bvx--
	} else if ax > bx {
		*avx--
		*bvx++
	}
}

func (m *Moon) Gravity(b *Moon) {
	gravityAdjust(m.x, &m.vx, b.x, &b.vx)
	gravityAdjust(m.y, &m.vy, b.y, &b.vy)
	gravityAdjust(m.z, &m.vz, b.z, &b.vz)
}

func (m *Moon) Velocity() {
	m.x += m.vx
	m.y += m.vy
	m.z += m.vz
}

func NewMoon(line string) (*Moon, error) {
	var m Moon
	if _, err := fmt.Sscanf(line, "<x=%d, y=%d, z=%d>", &m.x, &m.y, &m.z); err != nil {
		return nil, fmt.Errorf("failed to parse: %w", err)
	}
	return &m, nil
}
