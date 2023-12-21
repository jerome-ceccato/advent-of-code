import std/strutils
import std/sets
import std/math

type Garden = seq[string]
type Point = tuple[x: int, y: int]

let UP: Point = (x: 0, y: -1)
let RIGHT: Point = (x: 1, y: 0)
let DOWN: Point = (x: 0, y: 1)
let LEFT: Point = (x: -1, y: 0)

proc `+`(a, b: Point): Point = (x: a.x + b.x, y: a.y + b.y)

proc parseInput(): (Garden, Point) =
    var lines: Garden = readFile("input").splitLines()
    for y in low(lines)..high(lines):
        for x in low(lines[y])..high(lines[y]):
            if lines[y][x] == 'S':
                lines[y][x] = '.'
                return (lines, (x: x, y: y))

proc inBounds(point: Point, garden: Garden): bool =
    point.y >= low(garden) and point.y <= high(garden) and point.x >= low(garden[point.y]) and point.x <= high(garden[point.y])

proc walk(garden: Garden, starting: Point, steps: int): int =
    var positions = [starting].toOrderedSet()
    var next: OrderedSet[Point]
    for step in 1..steps:
        next = initOrderedSet[Point]()
        for point in positions:
            for dir in [UP, RIGHT, DOWN, LEFT]:
                var next_point = point + dir
                if inBounds(next_point, garden) and garden[next_point.y][next_point.x] == '.':
                    next.incl(next_point)
        positions = next
    positions.len()

proc walk_multiple(garden: Garden, points: seq[Point], steps: int): int =
    var total = 0
    for pt in points:
        total += walk(garden, pt, steps)
    total


# From the starting pos, there are straight lines going up/down/left/right without walls (and the starting pos is at the middle)
# This means that new dimensions will always be entered first from one of these 4 points
# So we can calculate the number of dimensions and easily calculate the number of positions
# for filled dimensions depending on the parity. The hard part will be to figure out the edges of the diamond
# Also the number of (steps - starting pos) / garden len is a whole number
proc calculateWalk(garden: Garden, starting_point: Point, target: int): int =
    let midUp = (x: starting_point.x, y: 0)
    let midDown = (x: starting_point.x, y: high(garden))
    let midLeft = (x: 0, y: starting_point.y)
    let midRight = (x: high(garden), y: starting_point.y)

    let topLeft = (x: 0, y: 0)
    let topRight = (x: high(garden), y: 0)
    let bottomLeft = (x: 0, y: high(garden))
    let bottomRight = (x: high(garden), y: high(garden))

    let loops = int((target - starting_point.x) / garden.len())
  
    let full = walk(garden, starting_point, garden.len())
    let full_off = walk(garden, starting_point, garden.len() + 1)
    let full_total = full * (loops - 1)^2 + full_off * loops^2

    let corners = walk_multiple(garden, @[midUp, midRight, midDown, midLeft], garden.len() - 1)
    let tiny = walk_multiple(garden, @[topLeft, topRight, bottomLeft, bottomRight], starting_point.x - 1)
    let thicc = walk_multiple(garden, @[topLeft, topRight, bottomLeft, bottomRight], starting_point.x + garden.len() - 1)

    full_total + corners + tiny * loops + thicc * (loops - 1)


var (garden, starting_point) = parseInput()
echo walk(garden, starting_point, 64)
echo calculateWalk(garden, starting_point, 26501365)
