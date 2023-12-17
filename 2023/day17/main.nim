import std/strutils
import std/sequtils
import std/heapqueue
import std/sets

type Point = tuple[x: int, y: int]
type Path = object
    point: Point
    lastDir: seq[Point]
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
proc `*`(a: Point, b: int): Point = (x: a.x * b, y: a.y * b)

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

    queue.push(Path(point: ZERO, lastDir: @[], loss: 0))
    
    while queue.len() > 0:
        let current: Path = queue.pop()
        
        if current.point == target:
            return current.loss

        if not visited.contains((current.point, current.lastDir)):
            visited.incl((current.point, current.lastDir))

            for dir in [RIGHT, DOWN, LEFT, UP]:
                let next = current.point + dir
                if inBounds(next, board) and 
                    (current.lastDir.len() == 0 or dir != -current.lastDir[^1]) and
                    not (current.lastDir.len() == 3 and current.lastDir.allIt(it == dir)):
                    
                    var nextDirs = current.lastDir
                    nextDirs.add(dir)
                    if nextDirs.len() > 3:
                        nextDirs.delete(0)

                    queue.push(Path(
                        point: next, 
                        lastDir: nextDirs, 
                        loss: current.loss + board[next.y][next.x]))
    -1

proc exploreUltra(board: seq[seq[int]]): int =
    let target: Point = (x: high(board[high(board)]), y: high(board))
    var queue = initHeapQueue[Path]()
    var visited = initHashSet[(Point, seq[Point])]()

    queue.push(Path(point: ZERO, lastDir: @[], loss: 0))
    
    while queue.len() > 0:
        let current: Path = queue.pop()
        
        if current.point == target:
            return current.loss

        if not visited.contains((current.point, current.lastDir)):
            visited.incl((current.point, current.lastDir))

            for dir in [RIGHT, DOWN, LEFT, UP]:
                let next = current.point + (dir * 4)
                if inBounds(next, board) and 
                    (current.lastDir.len() == 0 or dir != -current.lastDir[0]) and
                    not (current.lastDir.len() == 1 and current.lastDir[0] == dir):
                    
                    var pos = current.point
                    var loss = current.loss
                    for _ in 1..3:
                        pos = pos + dir
                        loss += board[pos.y][pos.x]
                    for _ in 4..10:
                        pos = pos + dir
                        if inBounds(pos, board):
                            loss += board[pos.y][pos.x]
                            queue.push(Path(
                                point: pos, 
                                lastDir: @[dir], 
                                loss: loss))
    -1

let input = parseInput()
echo explore(input)
echo exploreUltra(input)