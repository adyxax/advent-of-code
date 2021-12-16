package bits

import (
	"bufio"
	"fmt"
	"io"
)

type Bits struct {
	Version   byte
	TypeID    byte
	Value     int
	Operators []*Bits
	Len       int
}

// Scanner represents a lexical scanner.
type Scanner struct {
	r            *bufio.Reader
	buff         byte
	readBits     byte
	parsingDepth int
}

// NewScanner returns a new instance of Scanner.
func NewScanner(r io.Reader) *Scanner {
	return &Scanner{r: bufio.NewReader(r)}
}

// read reads the next bits from the bufferred reader.
// Returns the rune(0) if an error occurs (or io.EOF is returned).
func (s *Scanner) readBit() (bool, error) {
	var ret bool
	s.readBits++
	switch s.readBits {
	case 2:
		ret = s.buff&0b0100 != 0
	case 3:
		ret = s.buff&0b0010 != 0
	case 4:
		ret = s.buff&0b0001 != 0
	default:
		var err error
		s.buff, err = s.r.ReadByte()
		if err != nil || s.buff == '\n' {
			return false, err
		}
		if s.buff <= '9' {
			s.buff -= '0'
		} else {
			s.buff = s.buff - 'A' + 10
		}
		if err != nil {
			return false, err
		}
		s.readBits = 1
		ret = s.buff&0b1000 != 0
	}
	if ret {
		fmt.Print("1")
	} else {
		fmt.Print("0")
	}
	return ret, nil
}

// Scan returns the next Bits packet
func (s *Scanner) Scan() *Bits {
	var bits Bits
	for i := 0; i < 3; i++ {
		bits.Version <<= 1
		b, err := s.readBit()
		if err != nil {
			return nil
		}
		if b {
			bits.Version++
		}
	}
	fmt.Println("Version:", bits.Version)
	for i := 0; i < 3; i++ {
		bits.TypeID <<= 1
		b, err := s.readBit()
		if err != nil {
			return nil
		}
		if b {
			bits.TypeID++
		}
	}
	bits.Len += 6
	fmt.Println("TypeID:", bits.TypeID)
	if bits.TypeID == 4 {
		for {
			keepGoing, err := s.readBit()
			if err != nil {
				return nil
			}
			var buff byte
			for i := 0; i < 4; i++ {
				buff <<= 1
				b, err := s.readBit()
				if err != nil {
					return nil
				}
				if b {
					buff++
				}
			}
			bits.Len += 5
			bits.Value = bits.Value<<4 + int(buff)
			if !keepGoing {
				break
			}
		}
		fmt.Println("Value:", bits.Value)
	} else { // operator
		b, err := s.readBit()
		if err != nil {
			return nil
		}
		bits.Len++
		if b {
			var buff int
			for i := 0; i < 11; i++ {
				buff <<= 1
				b, err := s.readBit()
				if err != nil {
					return nil
				}
				if b {
					buff++
				}
			}
			bits.Len += 11
			fmt.Println("SubPackets type 1, nb of subpackets:", buff)
			s.parsingDepth++
			for i := 0; i < buff; i++ {
				subBits := s.Scan()
				if subBits == nil {
					return nil
				}
				bits.Operators = append(bits.Operators, subBits)
				bits.Len += subBits.Len
			}
			s.parsingDepth--
		} else {
			var buff int
			for i := 0; i < 15; i++ {
				buff <<= 1
				b, err := s.readBit()
				if err != nil {
					return nil
				}
				if b {
					buff++
				}
			}
			bits.Len += 15
			fmt.Println("SubPackets type 0 of len:", buff)
			s.parsingDepth++
			for buff > 0 {
				subBits := s.Scan()
				if subBits == nil {
					return nil
				}
				bits.Operators = append(bits.Operators, subBits)
				bits.Len += subBits.Len
				buff -= subBits.Len
				fmt.Println("remaining bits:", buff)
			}
			s.parsingDepth--
		}
	}
	if s.parsingDepth == 0 {
		bits.Len += 4 - int(s.readBits)
		s.readBits = 0
	}
	switch bits.TypeID {
	case 0:
		for _, sub := range bits.Operators {
			bits.Value += sub.Value
		}
	case 1:
		bits.Value = 1
		for _, sub := range bits.Operators {
			bits.Value *= sub.Value
		}
	case 2:
		bits.Value = bits.Operators[0].Value
		for _, sub := range bits.Operators {
			if bits.Value > sub.Value {
				bits.Value = sub.Value
			}
		}
	case 3:
		bits.Value = bits.Operators[0].Value
		for _, sub := range bits.Operators {
			if bits.Value < sub.Value {
				bits.Value = sub.Value
			}
		}
	case 5:
		if bits.Operators[0].Value > bits.Operators[1].Value {
			bits.Value = 1
		}
	case 6:
		if bits.Operators[0].Value < bits.Operators[1].Value {
			bits.Value = 1
		}
	case 7:
		if bits.Operators[0].Value == bits.Operators[1].Value {
			bits.Value = 1
		}
	}
	return &bits
}
