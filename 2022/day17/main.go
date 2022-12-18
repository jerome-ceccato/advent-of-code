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

type pattern struct {
	tick   uint64
	height uint64
}

type board struct {
	tick  uint64
	limit uint64

	jetidx      int
	jet         string
	lines       []byte
	extraHeight uint64

	memo map[string]pattern
}

type model struct {
	board    board
	renderer renderer
}

type tickMsg time.Time

// Initial data

const viewportHeight = 20
const updatesPerTick = 20

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
			memo:        make(map[string]pattern),
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

// Shift a shape left or right when falling in empty space
func shiftShape(shape []byte, direction byte) ([]byte, bool) {
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

// Shift a shape left/right/down into exiting space if possible
func shiftInto(shape []byte, into []byte, direction byte) ([]byte, bool) {
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

// Merge two shapes
func merge(shape []byte, into []byte) []byte {
	for i := 0; i < len(shape); i++ {
		if i < len(into) {
			shape[len(shape)-1-i] |= into[len(into)-1-i]
		}
	}
	return shape
}

// Add a new shape into the board
func (board board) Process(shape []byte) board {
	// Initial fall in empty space
	for i := 0; i < 3; i++ {
		shape, _ = shiftShape(shape, board.jet[board.jetidx])
		board.jetidx = (board.jetidx + 1) % len(board.jet)
	}

	// Try to keep falling
	pos := 0
	falling := true
	for falling {
		low := max(0, pos-len(shape))
		high := min(len(board.lines), pos+1)

		shape, _ = shiftInto(shape, board.lines[low:min(len(board.lines), pos)], board.jet[board.jetidx])
		board.jetidx = (board.jetidx + 1) % len(board.jet)
		if pos < len(board.lines) {
			shape, falling = shiftInto(shape, board.lines[low:high], 'v')
		} else {
			break
		}

		if falling {
			pos += 1
		}
	}

	// Merge the shape into the existing
	posup := pos - len(shape)
	low := max(0, posup)
	high := min(len(board.lines), pos)
	updated := merge(shape, board.lines[low:high])

	// Merge back the updated lines into the board
	if high-low == len(updated) {
		for i := low; i < high; i++ {
			// Cut unreachable lines
			if updated[i-low] == 0x7F {
				board.extraHeight += uint64(len(board.lines) - i)
				board.lines = board.lines[:i]
				return board
			}
			board.lines[i] = updated[i-low]
		}
	} else {
		board.lines = append(updated, board.lines[high:]...)
	}

	return board
}

// Generate a string identifier for a given board/jet state
func (board board) EncodeState() string {
	return fmt.Sprintf("%d-%v", board.jetidx, string(board.lines))
}

func (board board) Update() board {
	// First check if we encountered this pattern before
	state := board.EncodeState()
	cached, has := board.memo[state]
	if has {
		// Jump ahead
		loopTicks := board.tick - cached.tick
		loopHeight := (board.extraHeight + uint64(len(board.lines))) - cached.height

		remainingTicks := board.limit - board.tick
		jumpAhead := remainingTicks / loopTicks
		board.extraHeight += jumpAhead * loopHeight
		board.tick += jumpAhead * loopTicks

		// Reset the memo so we finish manually
		board.memo = make(map[string]pattern)
	} else {
		board.memo[state] = pattern{
			tick:   board.tick,
			height: board.extraHeight + uint64(len(board.lines)),
		}
	}

	// Process the new shape
	shapeid := int(board.tick % uint64(len(shapes)))
	shape := shapes[shapeid]
	board = board.Process(shape)
	board.tick += 1
	return board
}

// Rendering loop

func (e model) Init() tea.Cmd {
	return nil
}

func (e model) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.KeyMsg:
		switch msg.String() {
		case "q", "ctrl+c", "esc":
			return e, tea.Quit
		case "s":
			e.renderer.start = time.Now()
			return e, tickCmd()
		default:
			return e, nil
		}
	case tickMsg:
		for i := 0; i < updatesPerTick && e.board.tick < e.board.limit; i++ {
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

func enlarge(str string) string {
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

func prettify(str string) string {
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

	return e.renderer.view.Render(prettify(enlarge(slice)))
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
	return tea.Tick(time.Second/20, func(t time.Time) tea.Msg {
		return tickMsg(t)
	})
}

func main() {
	if _, err := tea.NewProgram(newModel(1000000000000), tea.WithAltScreen()).Run(); err != nil {
		fmt.Println(err)
	}
}
