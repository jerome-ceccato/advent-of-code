package main

import (
	"fmt"
	"os"
	"strings"
	"time"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
	"github.com/muesli/termenv"
)

// Utils

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

var colors = termenv.ColorProfile()
var colorMapping = map[string]string{
	"a": "4",
	"b": "5",
	"c": "2",
	"d": "1",
	"e": "3",
}

// Types

type renderer struct {
	view lipgloss.Style
	help lipgloss.Style
	meta lipgloss.Style

	start time.Time
}

type board struct {
	tick  uint64
	limit uint64

	jetidx      int
	jet         string
	lines       []byte
	extraHeight uint64
}

type model struct {
	board    board
	renderer renderer
}

type tickMsg time.Time

// Initial data

const viewportHeight = 20

var raw_shapes = []string{
	"  #### ",
	"   #   \n  ###  \n   #   ",
	"    #  \n    #  \n  ###  ",
	"  #    \n  #    \n  #    \n  #    ",
	"  ##   \n  ##   ",
}

func encode_shapes() [][]byte {
	res := [][]byte{}
	for _, shape := range raw_shapes {
		shapebits := []byte{}
		for _, line := range strings.Split(shape, "\n") {
			var single byte = 0
			for _, c := range line {
				single <<= 1
				if c == '#' {
					single |= 1
				}
			}
			shapebits = append(shapebits, single)
		}
		res = append(res, shapebits)
	}
	return res
}

var shapes = encode_shapes()

func readInput() string {
	content, err := os.ReadFile("input")
	if err != nil {
		fmt.Println("could not load file:", err)
		os.Exit(1)
	}
	return string(content)
}

func newModel(limit uint64) model {
	return model{
		board: board{
			jet:         readInput(),
			lines:       []byte{},
			tick:        0,
			jetidx:      0,
			limit:       limit,
			extraHeight: 0,
		},
		renderer: renderer{
			view: lipgloss.NewStyle().
				BorderStyle(lipgloss.RoundedBorder()).
				BorderForeground(lipgloss.Color("62")).
				Padding(0, 2).
				Height(viewportHeight),
			help: lipgloss.NewStyle().
				Foreground(lipgloss.Color("241")),
			meta: lipgloss.NewStyle().
				Foreground(lipgloss.Color("62")),
			start: time.Now(),
		},
	}
}

// Logic

func ShiftShape(shape []byte, direction byte) ([]byte, bool) {
	updated := make([]byte, len(shape))

	for i, current := range shape {
		if direction == '<' {
			if (current & 0x40) != 0 {
				return shape, false
			}
			updated[i] = current << 1
		} else {
			if (current & 1) != 0 {
				return shape, false
			}
			updated[i] = current >> 1
		}
	}
	return updated, true
}

func ShiftInto(shape []byte, into []byte, direction byte) ([]byte, bool) {
	updated := make([]byte, len(shape))
	for i := 0; i < len(shape); i++ {
		shapeidx := len(shape) - 1 - i
		current := shape[shapeidx]
		var target byte = 0
		if len(into) > i {
			target = into[len(into)-1-i]
		}
		if direction == 'v' {
			if (current & target) != 0 {
				return shape, false
			}
			updated[shapeidx] = current
		} else if direction == '<' {
			if (current&0x40) != 0 || ((current<<1)&target) != 0 {
				return shape, false
			}
			updated[shapeidx] = current << 1
		} else {
			if (current&1) != 0 || ((current>>1)&target) != 0 {
				return shape, false
			}
			updated[shapeidx] = current >> 1

		}
	}
	return updated, true
}

func Merge(shape []byte, into []byte) []byte {
	//fmt.Printf("merging:\n%s\ninto:\n%s\n", strings.Join(stringify(shape), "\n"), strings.Join(stringify(into), "\n"))
	for i := 0; i < len(shape); i++ {
		if i < len(into) {
			shape[len(shape)-1-i] |= into[len(into)-1-i]
		}
	}
	return shape
}

func (board board) Process(shape []byte) board {
	//fmt.Printf("Process shape:\n%s\n", strings.Join(stringify(shape), "\n"))

	// Initial fall in empty space
	for i := 0; i < 3; i++ {
		shape, _ = ShiftShape(shape, board.jet[board.jetidx])
		//fmt.Printf("shifted %c:\n%s\n", board.jet[board.jetidx], strings.Join(stringify(shape), "\n"))
		board.jetidx = (board.jetidx + 1) % len(board.jet)
	}

	// Try to keep falling
	pos := 0
	falling := true

	//fmt.Printf("now:\n%s\n", strings.Join(stringify(shape), "\n"))
	//fmt.Printf("board:\n%s\n", strings.Join(stringify(board.lines), "\n"))

	for falling {
		low := max(0, pos-len(shape))
		high := min(len(board.lines), pos+1)

		//fmt.Printf("[%d:%d] (%d)\n", low, high, len(board.lines))
		//fmt.Printf("will shift into\n%s\n", strings.Join(stringify(board.lines[low:min(len(board.lines), pos)]), "\n"))

		shape, _ = ShiftInto(shape, board.lines[low:min(len(board.lines), pos)], board.jet[board.jetidx])

		//fmt.Printf("shifted %c:\n%s\n", board.jet[board.jetidx], strings.Join(stringify(shape), "\n"))
		board.jetidx = (board.jetidx + 1) % len(board.jet)
		if pos < len(board.lines) {
			shape, falling = ShiftInto(shape, board.lines[low:high], 'v')
			//fmt.Printf("shifted down: %v\n", falling)
		} else {
			break
		}

		//fmt.Printf("shifted v:\n%s\n", strings.Join(stringify(shape), "\n"))
		if falling {
			pos += 1
		}
	}

	//fmt.Printf("done falling:\n%s\n", strings.Join(stringify(shape), "\n"))
	//fmt.Printf("pos: %d\n", pos)

	// Merge the shape into the existing
	posup := pos - len(shape)
	low := max(0, posup)
	high := min(len(board.lines), pos)

	//fmt.Printf("[%d:%d] (%d)\n", low, high, len(board.lines))
	//fmt.Printf("into:\n%s\n", strings.Join(stringify(board.lines), "\n"))

	//fmt.Printf("[%d:%d] (%d)\n", low, high, len(board.lines))
	updated := Merge(shape, board.lines[low:high])

	//fmt.Printf("merged shape:\n%s\n", strings.Join(stringify(updated), "\n"))

	if high-low == len(updated) {
		for i := low; i < high; i++ {
			// Cut unreachable lines
			if updated[i-low] == 0x7F {
				board.extraHeight += uint64(len(board.lines) - i)
				/*
					fmt.Printf("full row:%d\nfrom:\n%s\nto\n%s\n%s\n",
						board.tick,
						strings.Join(stringify(cp), "\n"),
						strings.Join(stringify(pm), "\n"),
						strings.Join(stringify(updated), "\n"))
					fmt.Printf("[%d:%d] (%d)\n", low, high, len(board.lines))
					fmt.Printf("%s\n", strings.Join(stringify(board.lines[0:5]), "\n"))
				*/
				board.lines = board.lines[:i]
				//fmt.Printf("after:\n%s\n", strings.Join(stringify(board.lines), "\n"))
				return board
			}
			board.lines[i] = updated[i-low]
		}
	} else {
		board.lines = append(updated, board.lines[high:]...)
	}

	//fmt.Printf("full board:\n%s\n", strings.Join(stringify(board.lines), "\n"))

	return board
}

func (board board) Update() board {
	shapeid := int(board.tick % uint64(len(shapes)))
	shape := shapes[shapeid]
	board = board.Process(shape)
	board.tick += 1
	return board
}

// Rendering loop

func (e model) Init() tea.Cmd {
	return tickCmd()
}

func (e model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "q", "ctrl+c", "esc":
			return e, tea.Quit
		case "a":
			e.board = e.board.Update()
			return e, nil
		case "s":
			return e, tickCmd()
		default:
			return e, nil
		}
	case tickMsg:
		for i := 0; i < 1000000 && e.board.tick < e.board.limit; i++ {
			e.board = e.board.Update()
		}
		if e.board.tick >= e.board.limit {
			return e, nil
		}
		return e, tickCmd()
	default:
		return e, nil
	}
}

func (e model) View() string {
	return e.boardView() + e.helpView() + e.metaView()
}

func (r renderer) enlarge(str string) string {
	lines := strings.Split(str, "\n")
	large := []string{}

	for _, line := range lines {
		bigline := ""
		for _, c := range line {
			bigline = bigline + string(c) + string(c) + string(c)
		}
		large = append(large, bigline, bigline)
	}

	return strings.Join(large, "\n")
}

func (r renderer) prettify(str string) string {
	for key, color := range colorMapping {
		str = strings.ReplaceAll(str, key, fmt.Sprint(termenv.String(" ").Background(colors.Color(color))))
	}
	return str
}

func stringify(encoded []byte) []string {
	strs := make([]string, 0, len(encoded))

	for _, b := range encoded {
		next := ""
		for expected := byte(0x40); expected > 0; expected >>= 1 {
			if (b & expected) != 0 {
				next += "a"
			} else {
				next += " "
			}
		}
		strs = append(strs, next)
	}
	return strs
}

func (e model) boardView() string {
	raw_slice := e.board.lines[:min(len(e.board.lines), viewportHeight)]
	slice := strings.Join(stringify(raw_slice), "\n")
	if len(e.board.lines) < viewportHeight {
		if len(e.board.lines) > 0 {
			slice = strings.Repeat("       \n", viewportHeight-len(e.board.lines)) + slice
		} else {
			slice = strings.Repeat("\n       ", viewportHeight)[1:]
		}
	}

	return e.renderer.view.Render(e.renderer.prettify(e.renderer.enlarge(slice)))
}

func (e model) helpView() string {
	return e.renderer.help.Render("\n s: Start • q: Quit")
}

func (e model) metaView() string {
	return e.renderer.meta.Render(fmt.Sprintf(
		"\n\n ticks: %d • height: %d (%d)\n\n progress: %.8f%%\n time: %.1f sec",
		e.board.tick,
		uint64(len(e.board.lines))+e.board.extraHeight,
		len(e.board.lines),
		(float64(e.board.tick) / float64(e.board.limit) * 100),
		time.Since(e.renderer.start).Seconds()))
}

func tickCmd() tea.Cmd {
	return tea.Tick(time.Microsecond, func(t time.Time) tea.Msg {
		return tickMsg(t)
	})
}

func main() {
	if true {
		if _, err := tea.NewProgram(newModel(1000000000000), tea.WithAltScreen()).Run(); err != nil {
			fmt.Println(err)
		}
	}

	/*
		0.01 -> 45sec
		45*1/0.0001
			a := newModel(1000000000000)
			for i := 0; i < 1000000; i++ {
				a.board = a.board.Update()
			}
			fmt.Printf("-> %d\n", uint64(len(a.board.lines))+a.board.extraHeight)
	*/
}
