import std/tables
import std/strutils
import std/sequtils
import std/re
import std/sugar
import std/math

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

proc next(node: string, i: char, network: TableRef[string, Pair]): string =
    if i == 'L':
        network[node].left
    else:
        network[node].right

proc run(instructions: string, network: TableRef[string, Pair]): int =
    var ip = 0
    var steps = 0
    var currentNode = "AAA"
    while currentNode != "ZZZ":
        currentNode = next(currentNode, instructions[ip], network)
        ip = (ip + 1) mod instructions.len()
        steps += 1
    steps

# We can observe that ghosts only reach a single Z node and are perfectly periodic
# We use this obsersation in runGhosts
proc observeGhosts(instructions: string, network: TableRef[string, Pair]): int =
    var startingNodes = collect(newSeq):
        for n in network.keys:
            if n.endsWith("A"): n
    
    echo "ghosts: ", startingNodes.len()
    for node in startingNodes:
        var ip = 0
        var currentNode = node
        var steps = 0
        var tmp = 0
        var last_steps = 0
        echo "-> ", currentNode
        while (steps == 0 or not (ip == 0 and currentNode == node)) and tmp < 5:
            if currentNode.endsWith("Z"):
                echo steps, ", ", ip, ", ", currentNode, "(", steps - last_steps, ")"
                tmp += 1
                last_steps = steps
            currentNode = next(currentNode, instructions[ip], network)
            ip = (ip + 1) mod instructions.len()
            steps += 1
        echo steps
    

proc runGhosts(instructions: string, network: TableRef[string, Pair]): int =
    var startingNodes = collect(newSeq):
        for n in network.keys:
            if n.endsWith("A"): n
    
    var period = newSeq[int](startingNodes.len())
    for i in low(startingNodes)..high(startingNodes):
        var ip = 0
        var currentNode = startingNodes[i]
        var steps = 0
        while not currentNode.endsWith("Z"):
            currentNode = next(currentNode, instructions[ip], network)
            ip = (ip + 1) mod instructions.len()
            steps += 1
        period[i] = steps
    
    period.foldl(lcm(a, b))
  

let (instructions, network) = readInput()
echo "part1: ", run(instructions, network)
echo "part2: ", runGhosts(instructions, network)