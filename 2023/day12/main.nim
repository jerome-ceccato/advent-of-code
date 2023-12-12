import std/strutils
import std/sequtils

type Row = tuple
    springs: string
    groups: seq[int]

proc parseLine(line: string): Row =
    let parts = line.split(" ")
    (parts[0], parts[1].split(",").map(parseInt))

proc parseInput(): seq[Row] =
    readFile("input").splitLines().map(parseLine)

proc isRowValid(row: Row): bool =
    var currentGroup = newSeq[int]()
    var current = 0
    for c in row.springs:
        if c == '.' and current > 0:
            currentGroup.add(current)
            current = 0
        elif c == '#':
            current += 1
    if current > 0:
        currentGroup.add(current)
    currentGroup == row.groups

proc countValidArrangements(springs: var string, groups: seq[int], p: int): int =
    if p >= springs.len():
        return if isRowValid((springs, groups)): 1 else: 0
    if springs[p] == '?':
        # Avoid making copies since we're doing dfs
        springs[p] = '.'
        let lhs = countValidArrangements(springs, groups, p + 1)
        springs[p] = '#'
        let rhs = countValidArrangements(springs, groups, p + 1)
        springs[p] = '?'
        return lhs + rhs
    else:
        return countValidArrangements(springs, groups, p + 1)

proc unfold(rows: seq[Row]): seq[Row] =
    let unfoldSprings = proc(springs: string): string =
        var res = springs
        for _ in 0..<4:
            res.add("?")
            res.add(springs)
        res
    
    let unfoldGroups = proc(groups: seq[int]): seq[int] =
        var res = newSeq[int]()
        for _ in 0..<5:
            res.add(groups)
        res
    
    rows.map(proc(row: Row): Row = (unfoldSprings(row.springs), unfoldGroups(row.groups)))

proc solve(rows: seq[Row]): int =
    var count = 0
    for row in rows:
        var springs = row.springs
        let res = countValidArrangements(springs, row.groups, 0)
        echo row.springs, " (", row.groups , "): ", res
        count += res
    count

echo (unfold(parseInput()))[0]