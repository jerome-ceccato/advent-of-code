import std/strutils
import std/sequtils
import std/tables
import std/sets

type Garden = seq[string]
type Point = tuple[x: int, y: int]

let ZERO: Point = (x: 0, y: 0)
let UP: Point = (x: 0, y: -1)
let RIGHT: Point = (x: 1, y: 0)
let DOWN: Point = (x: 0, y: 1)
let LEFT: Point = (x: -1, y: 0)

proc `+`(a, b: Point): Point = (x: a.x + b.x, y: a.y + b.y)
proc `-`(a, b: Point): Point = (x: a.x - b.x, y: a.y - b.y)
proc `-`(a: Point): Point = (x: -a.x, y: -a.y)
proc `*`(a: Point, b: int): Point = (x: a.x * b, y: a.y * b)

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

var (garden, starting_point) = parseInput()
echo walk(garden, starting_point, 64)
