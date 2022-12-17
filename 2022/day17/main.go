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
	tick   uint64
	jetidx int
	jet    string
	lines  []string
	limit  uint64
}

type model struct {
	board    board
	renderer renderer
}

type tickMsg time.Time

// Initial data

const viewportHeight = 20

var shapes = []string{
	"  #### ",
	"   #   \n  ###  \n   #   ",
	"    #  \n    #  \n  ###  ",
	"  #    \n  #    \n  #    \n  #    ",
	"  ##   \n  ##   ",
}

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
			jet:    readInput(),
			lines:  []string{},
			tick:   0,
			jetidx: 0,
			limit:  limit,
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

func MoveCurrent(lines []string, pos int, direction byte) ([]string, bool) {
	updated := make([]string, len(lines))
	copy(updated, lines)

	for y := pos; y > pos-4 && y >= 0; y-- {
		if direction == '>' {
			for x := len(updated[pos]) - 1; x >= 0; x-- {
				if updated[y][x] == '#' {
					if (x+1) < len(updated[y]) && updated[y][x+1] == ' ' {
						// Go...
						updated[y] = updated[y][:x] + " #" + updated[y][x+2:]
					} else {
						return lines, false
					}
				}
			}
		} else {
			for x, c := range updated[y] {
				if c == '#' {
					if direction == 'v' {
						if len(updated) > (y+1) && updated[y+1][x] == ' ' {
							updated[y+1] = updated[y+1][:x] + "#" + updated[y+1][x+1:]
							updated[y] = updated[y][:x] + " " + updated[y][x+1:]
						} else {
							return lines, false
						}
					} else if direction == '<' {
						if x > 0 && updated[y][x-1] == ' ' {
							updated[y] = updated[y][:(x-1)] + "# " + updated[y][x+1:]
						} else {
							return lines, false
						}
					}
				}
			}
		}
	}
	return updated, true
}

func (board board) Process(lines []string, pos int, shapec string) board {
	//fmt.Printf("lines size: %d\n%v\n", len(lines), strings.Join(lines, "\n"))
	falling := true
	for falling {
		lines, _ = MoveCurrent(lines, pos, board.jet[board.jetidx])
		//fmt.Printf("%c(%d)\n%s\n", board.jet[board.jetidx], pos, strings.Join(lines, "\n"))
		lines, falling = MoveCurrent(lines, pos, 'v')
		//fmt.Printf("v\n%s\n", strings.Join(lines, "\n"))

		board.jetidx = (board.jetidx + 1) % len(board.jet)
		pos += 1
	}

	//fmt.Print("done now\n")
	for i := 0; i < 5; i++ {
		if (pos-i) >= 0 && (pos-i) < len(lines) {
			lines[pos-i] = strings.ReplaceAll(lines[pos-i], "#", shapec)
		}
	}

	for lines[0] == "       " {
		lines = lines[1:]
	}
	//fmt.Printf("->\n%v\n", strings.ReplaceAll(strings.Join(lines, "\n"), " ", "."))
	board.lines = lines
	return board
}

func (board board) Update() board {
	shapeid := int(board.tick % uint64(len(shapes)))
	shape := strings.Split(shapes[shapeid], "\n")
	//fmt.Printf("shape size: %d\n", len(shape))
	newlines := append(shape, strings.Split(strings.Repeat("\n       ", 3)[1:], "\n")...)
	newlines = append(newlines, board.lines...)
	board = board.Process(newlines, len(shape)-1, string("abcde"[shapeid]))
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
		case "s":
			return e, tickCmd()
		default:
			return e, nil
		}
	case tickMsg:
		e.board = e.board.Update()
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

func (e model) boardView() string {
	slice := strings.Join(e.board.lines[:min(len(e.board.lines), viewportHeight)], "\n")
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
		"\n\n ticks: %d • height: %d\n\n progress: %.8f%%\n time: %.1f sec",
		e.board.tick,
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
	if _, err := tea.NewProgram(newModel(2022), tea.WithAltScreen()).Run(); err != nil {
		fmt.Println(err)
	}
}
