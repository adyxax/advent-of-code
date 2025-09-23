package computer

import "fmt"

type op int

const (
	ADD    op = 1
	EQ        = 8
	HALT   op = 99
	INPUT  op = 3
	JNZ    op = 5
	JZ     op = 6
	LT     op = 7
	MUL    op = 2
	OUTPUT op = 4
	REBASE op = 9
)

type mode int

const (
	IMMEDIATE mode = 1
	POSITION  mode = 0
	RELATIVE  mode = 2
)

type Computer struct {
	Halted       bool
	input        []int
	ip           int
	Memory       map[int]int
	relativeBase int
}

func (c *Computer) get(i int) int {
	if n, ok := c.Memory[i]; ok {
		return n
	} else {
		return 0
	}
}

func (c *Computer) getWithMode(pos int, m mode) int {
	i := c.get(pos)
	switch m {
	case IMMEDIATE:
		return i
	case POSITION:
		return c.get(i)
	case RELATIVE:
		return c.get(i + c.relativeBase)
	}
	panic(fmt.Errorf("invalid mode: %d", m))
}

func (c *Computer) getInstruction() (mode, mode, mode, op) {
	d := []int{0, 0, 0, 0, 0}
	i := 4
	n := c.get(c.ip)
	for ; n > 0; i-- {
		d[i] = n % 10
		n /= 10
	}
	return mode(d[0]), mode(d[1]), mode(d[2]), op(d[3]*10 + d[4])
}

func (c *Computer) set(i, v int) {
	c.Memory[i] = v
}

func (c *Computer) setWithMode(i int, v int, m mode) {
	switch m {
	case IMMEDIATE:
		c.set(i, v)
	case POSITION:
		c.set(c.get(i), v)
	case RELATIVE:
		c.set(c.get(i)+c.relativeBase, v)
	default:
		panic(fmt.Errorf("invalid mode: %d", m))
	}
}

func (c *Computer) Run(input []int) []int {
	c.input = append(c.input, input...)
	var output []int
	for {
		A, B, C, op := c.getInstruction()
		// 0 parameter instructions
		switch op {
		case HALT:
			c.Halted = true
			return output
		// 1 parameter instructions
		case INPUT:
			if len(c.input) > 0 {
				c.setWithMode(c.ip+1, c.input[0], C)
				c.input = c.input[1:]
			} else {
				return output
			}
			c.ip += 2
		case OUTPUT:
			output = append(output, c.getWithMode(c.ip+1, C))
			c.ip += 2
		case REBASE:
			c.relativeBase += c.getWithMode(c.ip+1, C)
			c.ip += 2
		// 2 parameter instructions
		case JNZ:
			if c.getWithMode(c.ip+1, C) != 0 {
				c.ip = c.getWithMode(c.ip+2, B)
			} else {
				c.ip += 3
			}
		case JZ:
			if c.getWithMode(c.ip+1, C) == 0 {
				c.ip = c.getWithMode(c.ip+2, B)
			} else {
				c.ip += 3
			}
			// 3 parameter instructions
		case ADD:
			c.setWithMode(c.ip+3, c.getWithMode(c.ip+1, C)+c.getWithMode(c.ip+2, B), A)
			c.ip += 4
		case MUL:
			c.setWithMode(c.ip+3, c.getWithMode(c.ip+1, C)*c.getWithMode(c.ip+2, B), A)
			c.ip += 4
		case LT:
			if c.getWithMode(c.ip+1, C) < c.getWithMode(c.ip+2, B) {
				c.setWithMode(c.ip+3, 1, A)
			} else {
				c.setWithMode(c.ip+3, 0, A)
			}
			c.ip += 4
		case EQ:
			if c.getWithMode(c.ip+1, C) == c.getWithMode(c.ip+2, B) {
				c.setWithMode(c.ip+3, 1, A)
			} else {
				c.setWithMode(c.ip+3, 0, A)
			}
			c.ip += 4
		default:
			panic(fmt.Errorf("invalid op: %d", op))
		}
	}
}

func (c *Computer) Set(pos int, v int) {
	c.Memory[pos] = v
}
