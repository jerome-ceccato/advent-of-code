import std/strutils
import std/sequtils
import std/algorithm

type Point = tuple[x: int, y: int]

proc expandUniverse(rows: seq[int], cols: seq[int], galaxies: var seq[Point], amount: int): seq[Point] =
    for y in rows.reversed():
        galaxies = galaxies.mapIt(if it.y > y: (it.x, it.y + amount) else: it)
    for x in cols.reversed():
        galaxies = galaxies.mapIt(if it.x > x: (it.x + amount, it.y) else: it)
    galaxies

proc findExpansionAreas(lines: seq[string]): tuple[rows: seq[int], cols: seq[int]] =
    var rows = newSeq[int]()
    var cols = newSeq[int]()

    for y in low(lines)..high(lines):
        if not lines[y].contains('#'):
            rows.add(y)

    for x in low(lines[0])..high(lines[0]):
        var hasGalaxy = false
        for y in low(lines)..high(lines):
            if lines[y][x] == '#':
                hasGalaxy = true
        if not hasGalaxy:
            cols.add(x)
    (rows, cols)

proc collectGalaxies(lines: seq[string]): seq[Point] =
    var points = newSeq[Point]()
    for y in low(lines)..high(lines):
        for x in low(lines[y])..high(lines[y]):
            if lines[y][x] == '#':
                points.add((x, y))
    points

proc prepareInput(universeExpansionFactor: int): seq[Point] =
    var lines = readFile("input").splitLines()
    let expansionAreas = findExpansionAreas(lines)
    var galaxies = collectGalaxies(lines)
    expandUniverse(expansionAreas.rows, expansionAreas.cols, galaxies, universeExpansionFactor - 1)

proc distance(lhs: Point, rhs: Point): int = 
    abs(lhs.x - rhs.x) + abs(lhs.y - rhs.y)

proc countDistances(galaxies: seq[Point]): int =
    var total = 0
    for i in low(galaxies)..high(galaxies):
        for j in i..high(galaxies):
            total += distance(galaxies[i], galaxies[j])
    total

echo countDistances(prepareInput(2))
echo countDistances(prepareInput(1000000))
