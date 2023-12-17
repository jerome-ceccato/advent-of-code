import std/strutils
import std/sequtils
import std/heapqueue
import std/sets

type Point = tuple[x: int, y: int]
type Path = object
    points: seq[Point]
    direction: Point
    loss: int

let ZERO: Point = (x: 0, y: 0)
let UP: Point = (x: 0, y: -1)
let RIGHT: Point = (x: 1, y: 0)
let DOWN: Point = (x: 0, y: 1)
let LEFT: Point = (x: -1, y: 0)

proc `<`(a, b: Path): bool = a.loss < b.loss
proc `+`(a, b: Point): Point = (x: a.x + b.x, y: a.y + b.y)
proc `-`(a, b: Point): Point = (x: a.x - b.x, y: a.y - b.y)
proc `-`(a: Point): Point = (x: -a.x, y: -a.y)

proc parseInput(): seq[seq[int]] =
    let lines = readFile("input").splitLines()
    var res = newSeq[seq[int]]()
    for line in lines:
        var row = newSeq[int]()
        for c in line:
            row.add(int(c) - int('0'))
        res.add(row)
    res

proc inBounds(point: Point, board: seq[seq[int]]): bool =
    point.y >= low(board) and point.y <= high(board) and point.x >= low(board[point.y]) and point.x <= high(board[point.y])

proc getLast3Dir(path: Path): array[3, Point] =
    if path.points.len() < 4:
        [ZERO, ZERO, ZERO]
    else:
        [path.points[^1] - path.points[^2],
        path.points[^2] - path.points[^3],
        path.points[^3] - path.points[^4]]

proc explore(board: seq[seq[int]]): int =
    let target: Point = (x: high(board[high(board)]), y: high(board))
    var queue = initHeapQueue[Path]()
    queue.push(Path(points: @[ZERO], direction: RIGHT, loss: 0))
    queue.push(Path(points: @[ZERO], direction: DOWN, loss: 0))
    var visited = initHashSet[Point]()

    while queue.len() > 0:
        let current = queue.pop()
        let lastPoint = current.points[^1]
        
        if lastPoint == target:
            return current.loss
        visited.incl(lastPoint)

        let last3Directions = getLast3Dir(current)
        for dir in [RIGHT, DOWN, LEFT, UP]:
            let next = lastPoint + dir
            if inBounds(next, board) and 
                not visited.contains(next) and
                dir != -current.direction and
                not last3Directions.allIt(it == dir):
                var nextPath = current
                nextPath.points.add(next)
                nextPath.direction = dir
                nextPath.loss += board[next.y][next.x]
                queue.push(nextPath)
    -1

echo explore(parseInput())
