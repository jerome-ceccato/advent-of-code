import std/tables
import std/strutils
import std/sequtils
import std/re

type
    Pair = object
        left, right: string

proc parseLine(into: TableRef[string, Pair], line: string): TableRef[string, Pair] =
    var matches: array[3, string]
    discard line.find(re"(\w+) = \((\w+), (\w+)\)", matches)
    into[matches[0]] = Pair(left: matches[1], right: matches[2])
    into

proc readInput(): (string, TableRef[string, Pair]) =
    let contents = readFile("input").splitLines()
    let instructions = contents[0]
    let network = contents[2..contents.high].foldl(parseLine(a, b), newTable[string, Pair]())
    (instructions, network)

proc run(instructions: string, network: TableRef[string, Pair]): int =
    var ip = 0
    var steps = 0
    var currentNode = "AAA"
    while currentNode != "ZZZ":
        if instructions[ip] == 'L':
            currentNode = network[currentNode].left
        else:
            currentNode = network[currentNode].right
        ip = (ip + 1) mod instructions.len()
        steps += 1
    steps

let (instructions, network) = readInput()
echo run(instructions, network)
