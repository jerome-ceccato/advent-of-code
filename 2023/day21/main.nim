import std/strutils
import std/sequtils
import std/tables
import std/sets
import std/math

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

proc show(garden: Garden, possible_pos: OrderedSet[Point]) =
    for y in low(garden)..high(garden):
        for x in low(garden[y])..high(garden[y]):
            if possible_pos.contains((x: x, y: y)):
                stdout.write('O')
            else:
                stdout.write(garden[y][x])
        stdout.write('\n')
    stdout.write(possible_pos.len())
    stdout.write("\n\n")
    stdout.flushFile()

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
    show(garden, next)
        #echo next.len()
    positions.len()

proc countGrid(garden: Garden, target: int): int =
    var count = 0
    for y in low(garden)..high(garden):
        for x in low(garden[y])..high(garden[y]):
            if garden[y][x] == '.' and (x + y) mod 2 == target:
                count += 1
    count

proc modlen(lhs: int, rhs: int): int =
    ((lhs mod rhs) + rhs) mod rhs

proc get(garden: Garden, target: Point): char =
    garden[modlen(target.y, garden.len())][modlen(target.x, garden[0].len())]

proc walk2(garden: Garden, starting: Point, steps: int): int =
    var positions = [starting].toOrderedSet()
    var next: OrderedSet[Point]
    var tmp: OrderedSet[Point]
    var step = 0
    while step < steps:
        next = initOrderedSet[Point]()
        tmp = initOrderedSet[Point]()
        for point in positions:
            for dir in [UP, RIGHT, DOWN, LEFT]:
                var next_point = point + dir
                if garden.get(next_point) == '.':
                    tmp.incl(next_point)
        
        for point in tmp:
            for dir in [UP, RIGHT, DOWN, LEFT]:
                var next_point = point + dir
                if garden.get(next_point) == '.' and not positions.contains(next_point):
                    next.incl(next_point)
        step += 2
        for point in next:
            positions.incl(point)
        echo next.len()
        #show(garden, positions)
        #echo next.len()
    positions.len()

proc getNextSteps(garden: Garden, initial: HashSet[Point]): seq[HashSet[Point]] =
    var next = newTable[Point, HashSet[Point]]()
    for point in initial:
        for dir in [UP, RIGHT, DOWN, LEFT]:
            var next_point = point + dir
            var id = ZERO
            if not inBounds(next_point, garden):
                id = dir
                next_point.y = (next_point.y + garden.len()) mod garden.len()
                next_point.x = (next_point.x + garden[next_point.y].len()) mod garden[next_point.y].len()
            if garden[next_point.y][next_point.x] == '.':
                next.mgetOrPut(id, initHashSet[Point]()).incl(next_point)
    next.values.toSeq()

proc walk3(garden: Garden, starting: Point, steps: int): int =
    var memo = newTable[HashSet[Point], seq[HashSet[Point]]]()
    var state = newTable[HashSet[Point], int]()

    var startSet = initHashSet[Point]()
    startSet.incl(starting)
    state[startSet] = 1

    for step in 1..steps:
        var nextState = newTable[HashSet[Point], int]()
        for table in state.keys:
            if not memo.hasKey(table):
                memo[table] = getNextSteps(garden, table)
            for item in memo[table]:
                nextState.mgetOrPut(item, 0) += state[table]
        state = nextState
    
    var res = 0
    for points in state.keys:
        res += points.len() * state[points]
    res

var (garden, starting_point) = parseInput()
# echo walk(garden, starting_point, 9)
# echo "---"
# echo walk2(garden, starting_point, 100)
# echo "---"
# echo countGrid(garden, 0)
# echo countGrid(garden, 1)

# echo walk3(garden, starting_point, 10)

echo garden.len()
echo garden[0].len()
echo starting_point

#echo (26501365 - starting_point.x) / garden.len()

proc walk4(garden: Garden, starting: seq[Point], steps: int): int =
    var positions = starting.toOrderedSet()
    var next: OrderedSet[Point]
    for step in 1..steps:
        next = initOrderedSet[Point]()
        for point in positions:
            for dir in [UP, RIGHT, DOWN, LEFT]:
                var next_point = point + dir
                if inBounds(next_point, garden) and garden[next_point.y][next_point.x] == '.':
                    next.incl(next_point)
        positions = next
    show(garden, next)
    positions.len()

let midUp = (x: starting_point.x, y: 0)
let midDown = (x: starting_point.x, y: high(garden))
let midLeft = (x: 0, y: starting_point.y)
let midRight = (x: high(garden), y: starting_point.y)

let topLeft = (x: 0, y: 0)
let topRight = (x: high(garden), y: 0)
let bottomLeft = (x: 0, y: high(garden))
let bottomRight = (x: high(garden), y: high(garden))

## test

proc showInf(garden: Garden, possible_pos: OrderedSet[Point], start: Point, inf: int) =
    let sz = garden.len()
    let lb = -(sz * inf)
    let hb = sz * (inf + 1)
    for y in lb..<hb:
        if (y + hb) mod sz == 0:
            stdout.write('\n')
        for x in lb..<hb:
            if (x + hb) mod sz == 0:
                stdout.write(' ')
            if start.x == x and start.y == y:
                stdout.write('@')
            elif possible_pos.contains((x: x, y: y)):
                stdout.write('O')
            else:
                stdout.write(get(garden, (x: x, y: y)))
        stdout.write('\n')
        
    stdout.write(possible_pos.len())
    stdout.write("\n\n")
    stdout.flushFile()

proc walkinf(garden: Garden, starting: Point, steps: int, inf: int): int =
    var positions = [starting].toOrderedSet()
    var next: OrderedSet[Point]
    for step in 1..steps:
        next = initOrderedSet[Point]()
        for point in positions:
            for dir in [UP, RIGHT, DOWN, LEFT]:
                var next_point = point + dir
                if garden.get(next_point) == '.':
                    next.incl(next_point)
        positions = next
    showInf(garden, positions, starting, inf)
    positions.len()

#let target = 4 * garden.len() + starting_point.x
let target = 26501365
let loops = int((target - starting_point.x) / garden.len())
echo "target: ", target
echo "loops: ", loops

# let manual_count = walkinf(garden, starting_point, target, loops)
# echo "Count number: ", manual_count

#9
#16
let full = walk4(garden, @[starting_point], garden.len())
let full_off = walk4(garden, @[starting_point], garden.len() + 1)

let full_total = full * (loops - 1)^2 + full_off * loops^2

proc walk4_multiple(garden: Garden, points: seq[seq[Point]], steps: int): int =
    var total = 0
    for pts in points:
        total += walk4(garden, pts, steps)
    total

let corners = walk4_multiple(garden, @[@[midUp], @[midRight], @[midDown], @[midLeft]], garden.len()-1)
let tiny = walk4_multiple(garden, @[@[topLeft], @[topRight], @[bottomLeft], @[bottomRight]], starting_point.x - 1)
let thicc = walk4_multiple(garden, @[@[topLeft], @[topRight], @[bottomLeft], @[bottomRight]], starting_point.x + garden.len()-1)

echo full_total + corners + tiny * loops + thicc * (loops - 1)
#echo manual_count
#let thicc = walk4_multiple(garden, @[@[midUp, midLeft], @[midUp, midRight], @[midDown, midLeft], @[midDown, midRight]], starting_point.x)
# let corners = walk4(garden, @[midUp, midLeft], starting_point.x) +
#     walk4(garden, @[midUp, midRight], starting_point.x) +
#     walk4(garden, @[midDown, midLeft], starting_point.x) +
#     walk4(garden, @[midDown, midRight], starting_point.x)

# let totalFull = (2 * (loops * loops) + 2 * loops + 1) * full
# let totalCorners = (loops - 1) * corners
# echo totalFull + totalCorners

## end test


# echo walk4(garden, @[midDown, midRight], garden.len())

# echo walk4(garden, @[midUp, midLeft], starting_point.x)
# echo walk4(garden, @[midUp, midRight], starting_point.x)
# echo walk4(garden, @[midDown, midLeft], starting_point.x)
# echo walk4(garden, @[midDown, midRight], starting_point.x)
# echo starting_point.x + garden.len()

# let full = walk4(garden, @[starting_point], garden.len())
# let corners = walk4(garden, @[midUp, midLeft], starting_point.x) +
#     walk4(garden, @[midUp, midRight], starting_point.x) +
#     walk4(garden, @[midDown, midLeft], starting_point.x) +
#     walk4(garden, @[midDown, midRight], starting_point.x)

# let corners = walk4(garden, @[(x: 0, y: 0)], garden.len()) +
#     walk4(garden, @[(x: 0, y: high(garden))], garden.len()) +
#     walk4(garden, @[(x: high(garden), y: 0)], garden.len()) +
#     walk4(garden, @[(x: high(garden), y: high(garden))], garden.len())

# let target = 26501365
# let loops = int((target - starting_point.x) / garden.len())
# echo loops

# let totalFull = (2 * (loops * loops) + 2 * loops + 1) * full
# let totalCorners = (loops - 1) * full * 4
# echo totalFull + totalCorners

# echo walk4(garden, @[starting_point], 140)
# echo walk4(garden, @[starting_point], 141)

#633857226738716 too low
#633860291163968 ?
#633863493230320 ?

#echo walk4(garden, @[starting_point], starting_point.x + garden.len()*2)
# for y in low(garden)..high(garden):
#     for x in low(garden[y])..high(garden[y]):
#         if x == starting_point.x or y == starting_point.y:
#             if garden[y][x] == '.':
#                 stdout.write('+')
#             else:
#                 stdout.write('@')
#         else:
#             stdout.write(garden[y][x])
#     stdout.write('\n')
# stdout.flushFile()


# Count the number of positions for each "dimension" and multiply that at the end.
# -> Not possible because not repeating positions is different for each dimension

# Check even/odd rule for whether a spot is on
# Run the example step by step and see if any interesting properties appear

# From the starting pos, there are straight lines going up/down/left/right without walls (and the starting pos is at the middle)
# This means that new dimensions will always be entered first from one of these 4 points
# So we can calculate the number of dimensions and easily calculate the number of positions
# for filled dimensions depending on the parity. The hard part will be to figure out the edges of the diamond
# Also the number of (steps - starting pos) / garden len is a whole number

# steps = start_pos + 3*garden_len

# full rectangles: 2(n^2) + 2n + 1
# half rectangles: (n-1) of each kind

# 1,3,5,7,5,3,1
# .........
# .../#\...
# ../###\..
# ./#####\.
# .#######.
# .\#####/.
# ..\###/..
# ...\#/...
# .........


# 4^2 * 2 + 4*2+1
# .../#\...
# ../###\..
# ./#####\.
# /#######\
# #########
# \#######/
# .\#####/.
# ..\###/..
# ...\#/...

# ..../#\....
# .../###\...
# ../#####\..
# ./#######\.
# /#########\
# ###########
# \#########/
# .\#######/.
# ..\#####/..
# ...\###/...
# ....\#/....

# .........
# ....^....
# .../#\...
# ../###\..
# .<#####>.
# ..\###/..
# ...\#/...
# ....v....
# .........


# 11
# 11
# (x: 5, y: 5)
# 3

#  ........... ........... ........... .....O..... ........... ........... ...........
#  ........... ........... ........... ....O.O.... ........... ........... ...........
#  ........... ........... ........... ...O.O.O... ........... ........... ...........
#  ........... ........... ........... ..O.O.O.O.. ........... ........... ...........
#  ........... ........... ........... .O.O.O.O.O. ........... ........... ...........
#  ........... ........... ........... O.O.O.O.O.O ........... ........... ...........
#  ........... ........... ..........O .O.O.O.O.O. O.......... ........... ...........
#  ........... ........... .........O. O.O.O.O.O.O .O......... ........... ...........
#  ........... ........... ........O.O .O.O.O.O.O. O.O........ ........... ...........
#  ........... ........... .......O.O. O.O.O.O.O.O .O.O....... ........... ...........
#  ........... ........... ......O.O.O .O.O.O.O.O. O.O.O...... ........... ...........

#  ........... ........... .....O.O.O. O.O.O.O.O.O .O.O.O..... ........... ...........
#  ........... ........... ....O.O.O.O .O.O.O.O.O. O.O.O.O.... ........... ...........
#  ........... ........... ...O.O.O.O. O.O.O.O.O.O .O.O.O.O... ........... ...........
#  ........... ........... ..O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.. ........... ...........
#  ........... ........... .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. ........... ...........
#  ........... ........... O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O ........... ...........
#  ........... ..........O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.......... ...........
#  ........... .........O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O......... ...........
#  ........... ........O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O........ ...........
#  ........... .......O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O....... ...........
#  ........... ......O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O...... ...........

#  ........... .....O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O..... ...........
#  ........... ....O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.... ...........
#  ........... ...O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O... ...........
#  ........... ..O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.. ...........
#  ........... .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. ...........
#  ........... O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O ...........
#  ..........O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O..........
#  .........O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.........
#  ........O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O........
#  .......O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.......
#  ......O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O......

#  .....O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.....
#  ....O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O....
#  ...O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O...
#  ..O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O..
#  .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O.
#  O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.@.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O
#  .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O.
#  ..O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O..
#  ...O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O...
#  ....O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O....
#  .....O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.....

#  ......O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O......
#  .......O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.......
#  ........O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O........
#  .........O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.........
#  ..........O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O..........
#  ........... O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O ...........
#  ........... .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. ...........
#  ........... ..O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.. ...........
#  ........... ...O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O... ...........
#  ........... ....O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.... ...........
#  ........... .....O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O..... ...........

#  ........... ......O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O...... ...........
#  ........... .......O.O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O.O....... ...........
#  ........... ........O.O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.O........ ...........
#  ........... .........O. O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O .O......... ...........
#  ........... ..........O .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. O.......... ...........
#  ........... ........... O.O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.O ........... ...........
#  ........... ........... .O.O.O.O.O. O.O.O.O.O.O .O.O.O.O.O. ........... ...........
#  ........... ........... ..O.O.O.O.O .O.O.O.O.O. O.O.O.O.O.. ........... ...........
#  ........... ........... ...O.O.O.O. O.O.O.O.O.O .O.O.O.O... ........... ...........
#  ........... ........... ....O.O.O.O .O.O.O.O.O. O.O.O.O.... ........... ...........
#  ........... ........... .....O.O.O. O.O.O.O.O.O .O.O.O..... ........... ...........

#  ........... ........... ......O.O.O .O.O.O.O.O. O.O.O...... ........... ...........
#  ........... ........... .......O.O. O.O.O.O.O.O .O.O....... ........... ...........
#  ........... ........... ........O.O .O.O.O.O.O. O.O........ ........... ...........
#  ........... ........... .........O. O.O.O.O.O.O .O......... ........... ...........
#  ........... ........... ..........O .O.O.O.O.O. O.......... ........... ...........
#  ........... ........... ........... O.O.O.O.O.O ........... ........... ...........
#  ........... ........... ........... .O.O.O.O.O. ........... ........... ...........
#  ........... ........... ........... ..O.O.O.O.. ........... ........... ...........
#  ........... ........... ........... ...O.O.O... ........... ........... ...........
#  ........... ........... ........... ....O.O.... ........... ........... ...........
#  ........... ........... ........... .....O..... ........... ........... ...........
# 1521