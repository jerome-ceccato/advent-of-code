import std/strutils
import std/sequtils
import std/tables
import std/random
import std/sets
import std/deques
import std/algorithm

proc parseInput(): TableRef[string, seq[string]] =
    var lines = readFile("input").splitLines()
    var res = newTable[string, seq[string]]()
    for line in lines:
        let parts = line.split(": ")
        let lhs = parts[0]
        for rhs in parts[1].split(" "):
            res.mgetOrPut(lhs, newSeq[string]()).add(rhs)
            res.mgetOrPut(rhs, newSeq[string]()).add(lhs)
    res

proc path(data: TableRef[string, seq[string]], lhs: string, rhs: string): seq[string] =
    var queue = initDeque[seq[string]]()
    var visited = initHashSet[string]()

    queue.addLast(@[lhs])
    while queue.len() > 0:
        let current = queue.popFirst()
        let last = current[high(current)]
        if last == rhs:
            return current
        if not visited.contains(last):
            visited.incl(last)
            for other in data[last]:
                if not visited.contains(other):
                    var next = current
                    next.add(other)
                    queue.addLast(next)
    @[]

proc findMostCommonPaths(data: TableRef[string, seq[string]], n: int, steps: int): seq[(string, string)] =
    var freqTable = newTable[(string, string), int]()

    let nodes = data.keys.toSeq()
    for _ in 0..steps:
        let lhs = nodes.sample()
        let rhs = nodes.sample()
        let foundPath = path(data, lhs, rhs)
        for i in (low(foundPath) + 1)..high(foundPath):
            let pair = (min(foundPath[i], foundPath[i - 1]), max(foundPath[i], foundPath[i - 1]))
            freqTable.mgetOrPut(pair, 0) += 1

    var flattened = newSeq[(string, string, int)]()
    for key in freqTable.keys:
        flattened.add((key[0], key[1], freqTable[key]))
    flattened.sort(proc (x, y: (string, string, int)): int = y[2] - x[2])
    flattened.setLen(n)
    flattened.mapIt((it[0], it[1]))
 
proc cutVertices(data: TableRef[string, seq[string]], vertices: seq[(string, string)]) =
    for pair in vertices:
        data[pair[0]].delete(data[pair[0]].find(pair[1]))
        data[pair[1]].delete(data[pair[1]].find(pair[0]))

proc countSubgraphsLen(data: TableRef[string, seq[string]]): seq[int] =
    var remaining = data.keys.toSeq().toHashSet()
    var res = newSeq[int]()
    while remaining.len() > 0:
        var total = 1
        var queue = @[remaining.pop()]
        while queue.len() > 0:
            let current = queue.pop()
            for other in data[current]:
                if remaining.contains(other):
                    remaining.excl(other)
                    queue.add(other)
                    total += 1
        res.add(total)
    res

var data = parseInput()

cutVertices(data, findMostCommonPaths(data, 3, 1000))
echo countSubgraphsLen(data).foldl(a * b)
