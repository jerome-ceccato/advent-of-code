import std/strutils
import std/sequtils
import std/heapqueue
import std/sets

type Point = tuple[x: int, y: int]
type Path = object
    point: Point
    last3Dir: seq[Point]
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

proc explore(board: seq[seq[int]]): int =
    let target: Point = (x: high(board[high(board)]), y: high(board))
    var queue = initHeapQueue[Path]()
    var visited = initHashSet[(Point, seq[Point])]()

    queue.push(Path(point: ZERO, last3Dir: @[], loss: 0))
    
    var steps = 0
    while queue.len() > 0:
        let current: Path = queue.pop()
        
        steps += 1
        if current.point == target:
            echo "steps: ", steps
            return current.loss

        if not visited.contains((current.point, current.last3Dir)):
            visited.incl((current.point, current.last3Dir))

            for dir in [RIGHT, DOWN, LEFT, UP]:
                let next = current.point + dir
                if inBounds(next, board) and 
                    (current.last3Dir.len() == 0 or dir != -current.last3Dir[^1]) and
                    not (current.last3Dir.len() == 3 and current.last3Dir.allIt(it == dir)):
                    
                    var nextDirs = current.last3Dir
                    nextDirs.add(dir)
                    if nextDirs.len() > 3:
                        nextDirs.delete(0)

                    queue.push(Path(
                        point: next, 
                        last3Dir: nextDirs, 
                        loss: current.loss + board[next.y][next.x]))
    -1

echo explore(parseInput())
