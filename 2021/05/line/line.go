package line

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"strconv"
)

type Line struct {
	X1, Y1, X2, Y2 int
}

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
	ARROW // ->
)

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
	// If we see a digit then consume as an int.
	if isWhitespace(ch) {
		return s.scanWhitespace(ch)
	} else if isDigit(ch) {
		return s.scanInt(ch)
	}

	// Otherwise read the individual character.
	switch ch {
	case eof:
		return EOF, ""
	case ',':
		return COMMA, string(ch)
	case '-':
		return s.scanArrow(ch)
	}

	return ILLEGAL, string(ch)
}

// scanWhitespace consumes the current rune and all contiguous whitespace.
func (s *Scanner) scanWhitespace(read rune) (tok Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(read)

	// Read every subsequent whitespace character into the buffer.
	// Non-whitespace characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if !isWhitespace(ch) {
			s.unread()
			break
		} else {
			buf.WriteRune(ch)
		}
	}

	return WHITESPACE, buf.String()
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

func (s *Scanner) scanArrow(read rune) (tok Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(read)

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if ch == '>' {
			_, _ = buf.WriteRune(ch)
			return ARROW, buf.String()
		} else {
			_, _ = buf.WriteRune(ch)
			break
		}
	}

	// Otherwise return as a regular identifier.
	return ILLEGAL, buf.String()
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

// scanIgnoreWhitespace scans the next non-whitespace token.
func (p *Parser) scanIgnoreWhitespace() (tok Token, lit string) {
	tok, lit = p.scan()
	if tok == WHITESPACE {
		tok, lit = p.scan()
	}
	return
}

func (p *Parser) Parse() (*Line, error) {
	l := &Line{}
	if tok, lit := p.scanIgnoreWhitespace(); tok != INT {
		return nil, fmt.Errorf("found %q, expected INT", lit)
	} else {
		l.X1, _ = strconv.Atoi(lit)
	}
	if tok, lit := p.scanIgnoreWhitespace(); tok != COMMA {
		return nil, fmt.Errorf("found %q, expected COMMA", lit)
	}
	if tok, lit := p.scanIgnoreWhitespace(); tok != INT {
		return nil, fmt.Errorf("found %q, expected INT", lit)
	} else {
		l.Y1, _ = strconv.Atoi(lit)
	}
	if tok, lit := p.scanIgnoreWhitespace(); tok != ARROW {
		return nil, fmt.Errorf("found %q, expected ARROW", lit)
	}
	if tok, lit := p.scanIgnoreWhitespace(); tok != INT {
		return nil, fmt.Errorf("found %q, expected INT", lit)
	} else {
		l.X2, _ = strconv.Atoi(lit)
	}
	if tok, lit := p.scanIgnoreWhitespace(); tok != COMMA {
		return nil, fmt.Errorf("found %q, expected COMMA", lit)
	}
	if tok, lit := p.scanIgnoreWhitespace(); tok != INT {
		return nil, fmt.Errorf("found %q, expected INT", lit)
	} else {
		l.Y2, _ = strconv.Atoi(lit)
	}
	if tok, lit := p.scan(); tok != WHITESPACE {
		return nil, fmt.Errorf("found %q, expected WHITESPACE", lit)
	}
	return l, nil
}
