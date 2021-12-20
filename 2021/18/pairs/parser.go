package pairs

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"strconv"
)

// Token represents a lexical token.
type Token int

const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	WHITESPACE

	// Literals
	INT // integers

	// Misc characters
	COMMA // ,
	OPEN_BRACKET
	CLOSE_BRACKET
)

type Pair struct {
	HasSubPairs bool
	Value       int
	Left, Right *Pair
}

func (p Pair) String() string {
	if p.HasSubPairs {
		return fmt.Sprintf("[%s, %s]", p.Left.String(), p.Right.String())
	}
	return fmt.Sprintf("%d", p.Value)
}

func (p Pair) Magnitude() int {
	if p.HasSubPairs {
		return 3*p.Left.Magnitude() + 2*p.Right.Magnitude()
	}
	return p.Value
}

func (p1 *Pair) Add(p2 *Pair) *Pair {
	p := &Pair{
		HasSubPairs: true,
		Left:        p1,
		Right:       p2,
	}
	p.Reduce()
	return p
}

func (p Pair) DeepCopy() *Pair {
	if p.HasSubPairs {
		return &Pair{
			HasSubPairs: true,
			Left:        p.Left.DeepCopy(),
			Right:       p.Right.DeepCopy(),
		}
	}
	return &Pair{Value: p.Value}
}

func (p *Pair) Reduce() {
	for {
		if exploded, _, _ := p.TryExplode(0); exploded {
			continue
		}
		if p.TrySplit() {
			continue
		}
		break
	}
}

func (p *Pair) TrySplit() bool {
	if p.HasSubPairs {
		if p.Left.TrySplit() {
			return true
		}
		return p.Right.TrySplit()
	}
	if p.Value >= 10 {
		p.HasSubPairs = true
		p.Left = &Pair{Value: p.Value / 2}
		p.Right = &Pair{Value: p.Value/2 + p.Value%2}
		p.Value = 0
		return true
	}
	return false
}

func (p *Pair) TryExplode(depth int) (bool, *int, *int) {
	if !p.HasSubPairs {
		return false, nil, nil
	}
	if depth >= 4 {
		p.HasSubPairs = false
		p.Value = 0
		l := p.Left.Value
		r := p.Right.Value
		p.Left = nil
		p.Right = nil
		return true, &l, &r
	}
	depth++
	if exploded, l, r := p.Left.TryExplode(depth); exploded {
		if r != nil {
			r = p.Right.TryAddToLeftMostNumber(r)
		}
		return exploded, l, r
	}
	if exploded, l, r := p.Right.TryExplode(depth); exploded {
		if l != nil {
			l = p.Left.TryAddToRightMostNumber(l)
		}
		return exploded, l, r
	}
	return false, nil, nil
}

func (p *Pair) TryAddToLeftMostNumber(n *int) *int {
	if p.HasSubPairs {
		return p.Left.TryAddToLeftMostNumber(n)
	}
	p.Value += *n
	return nil
}

func (p *Pair) TryAddToRightMostNumber(n *int) *int {
	if p.HasSubPairs {
		return p.Right.TryAddToRightMostNumber(n)
	}
	p.Value += *n
	return nil
}

func isWhitespace(ch rune) bool {
	return ch == ' ' || ch == '\t' || ch == '\n'
}

func isDigit(ch rune) bool {
	return (ch >= '0' && ch <= '9')
}

var eof = rune(0)

// Scanner represents a lexical scanner.
type Scanner struct {
	r *bufio.Reader
}

// NewScanner returns a new instance of Scanner.
func NewScanner(r io.Reader) *Scanner {
	return &Scanner{r: bufio.NewReader(r)}
}

// read reads the next rune from the bufferred reader.
// Returns the rune(0) if an error occurs (or io.EOF is returned).
func (s *Scanner) read() rune {
	ch, _, err := s.r.ReadRune()
	if err != nil {
		return eof
	}
	return ch
}

// unread places the previously read rune back on the reader.
func (s *Scanner) unread() { _ = s.r.UnreadRune() }

// Scan returns the next token and literal value.
func (s *Scanner) Scan() (tok Token, lit string) {
	// Read the next rune.
	ch := s.read()

	// If we see whitespace then consume all contiguous whitespace.
	for isWhitespace(ch) {
		ch = s.read()
	}

	// If we see a digit then consume as an int.
	if isDigit(ch) {
		return s.scanInt(ch)
	}

	// Otherwise read the individual character.
	switch ch {
	case eof:
		return EOF, ""
	case ',':
		return COMMA, string(ch)
	case '[':
		return OPEN_BRACKET, string(ch)
	case ']':
		return CLOSE_BRACKET, string(ch)
	}

	return ILLEGAL, string(ch)
}

// scanInt consumes the current rune and all contiguous digit runes.
func (s *Scanner) scanInt(read rune) (tok Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(read)

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if !isDigit(ch) {
			s.unread()
			break
		} else {
			_, _ = buf.WriteRune(ch)
		}
	}

	// Otherwise return as a regular identifier.
	return INT, buf.String()
}

// Parser represents a parser.
type Parser struct {
	s   *Scanner
	buf struct {
		tok Token  // last read token
		lit string // last read literal
		n   int    // buffer size (max=1)
	}
}

// NewParser returns a new instance of Parser.
func NewParser(r io.Reader) *Parser {
	return &Parser{s: NewScanner(r)}
}

// scan returns the next token from the underlying scanner.
// If a token has been unscanned then read that instead.
func (p *Parser) scan() (tok Token, lit string) {
	// If we have a token on the buffer, then return it.
	if p.buf.n != 0 {
		p.buf.n = 0
		return p.buf.tok, p.buf.lit
	}

	// Otherwise read the next token from the scanner.
	tok, lit = p.s.Scan()

	// Save it to the buffer in case we unscan later.
	p.buf.tok, p.buf.lit = tok, lit

	return
}

// unscan pushes the previously read token back onto the buffer.
func (p *Parser) unscan() { p.buf.n = 1 }

func (p *Parser) Parse() (pair *Pair, err error) {
	pair = &Pair{}
	tok, lit := p.scan()
	switch tok {
	case OPEN_BRACKET:
		pair.HasSubPairs = true
		pair.Left, err = p.Parse()
		if err != nil {
			return nil, err
		}
		if tok, lit := p.scan(); tok != COMMA {
			return nil, fmt.Errorf("found %q, expected COMMA", lit)
		}
		pair.Right, err = p.Parse()
		if err != nil {
			return nil, err
		}
		if tok, lit := p.scan(); tok != CLOSE_BRACKET {
			return nil, fmt.Errorf("found %q, expected CLOSE_BRACKET", lit)
		}
	case INT:
		if v, err := strconv.Atoi(lit); err != nil {
			return nil, fmt.Errorf("failed to atoi an INT %s : %w", lit, err)
		} else {
			pair.Value = v
		}
	default:
		return nil, fmt.Errorf("unexpected %q, expected OPEN_BRACKER or INT", lit)
	}
	return pair, nil
}
