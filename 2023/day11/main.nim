import std/sets
import std/strutils
import std/sequtils
import std/sugar

type Point = tuple[x: int, y: int]

proc expandUniverse(lines: var seq[string]): seq[string] =
    for y in countdown(high(lines), low(lines)):
        if not lines[y].contains('#'):
            lines.insert(lines[y], y)
    for x in countdown(high(lines[0]), low(lines[0])):
        var hasGalaxy = false
        for y in low(lines)..high(lines):
            if lines[y][x] == '#':
                hasGalaxy = true
        if not hasGalaxy:
            for y in low(lines)..high(lines):
                lines[y].insert(".", x)
    lines

proc collectGalaxies(lines: seq[string]): OrderedSet[Point] =
    var points = initOrderedSet[Point]()
    for y in low(lines)..high(lines):
        for x in low(lines[y])..high(lines[y]):
            if lines[y][x] == '#':
                points.incl((x, y))
    points

proc parseInput(): OrderedSet[Point] =
    var lines = readFile("input").splitLines()
    lines = expandUniverse(lines)
    collectGalaxies(lines)

proc distance(lhs: Point, rhs: Point): int = 
    abs(lhs.x - rhs.x) + abs(lhs.y - rhs.y)

proc countDistances(galaxies: OrderedSet[Point]): int =
    var total = 0
    let garray = galaxies.toSeq()
    for i in low(garray)..high(garray):
        for j in i..high(garray):
            total += distance(garray[i], garray[j])
    total

echo countDistances(parseInput())
