import std/strutils
import std/sequtils
import std/tables

type Row = tuple
    springs: string
    groups: seq[int]

proc parseLine(line: string): Row =
    let parts = line.split(" ")
    (parts[0], parts[1].split(",").map(parseInt))

proc parseInput(): seq[Row] =
    readFile("input").splitLines().map(parseLine)

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

proc canFitGroup(springs: string, group: int): bool =
    if springs.len() < group:
        return false
    for n in 0..<group:
        if springs[n] == '.':
            return false
    
    return group == springs.len() or springs[group] != '#'

proc countValidArrangements(springs: var string, groups: seq[int], memo: TableRef[(string, seq[int]), int]): int =
    springs.removePrefix('.')

    if memo.contains((springs, groups)):
        return memo[(springs, groups)]
    
    if groups.len() == 0:
        let res = if springs.contains('#'): 0 else: 1
        memo[(springs, groups)] = res
        return res
    elif springs.len() == 0:
        return 0
    
    var total = 0
    # Try skipping the first ?
    if springs[0] == '?':
        var next = springs.substr(1)
        total += countValidArrangements(next, groups, memo)
    if canFitGroup(springs, groups[0]):
        # +1 to consume the . or ? as a separator
        var next = if springs.len() == groups[0]: "" else: springs.substr(groups[0] + 1)
        total += countValidArrangements(next, groups[1..high(groups)], memo)

    memo[(springs, groups)] = total
    return total

proc solve(rows: seq[Row]): int =
    var count = 0
    # Aggressively memoize things across all arrangements
    var memo = newTable[(string, seq[int]), int]()
    for row in rows:
        var springs = row.springs
        let res = countValidArrangements(springs, row.groups, memo)
        # echo row.springs, " (", row.groups , "): ", res
        count += res
    count

let input = parseInput()
echo solve(input)
echo solve(unfold(input))
