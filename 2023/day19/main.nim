import std/strutils
import std/sequtils
import std/tables
import std/options
import std/re

type Operation = enum
    gt, lt

type Rule = object
    category: char
    op: Operation
    target: int

type Instruction = object
    condition: Option[Rule]
    label: string

type Workflow = object
    instructions: seq[Instruction]

type Part = object
    x: int
    m: int
    a: int
    s: int

type Input = object
    workflows: TableRef[string, Workflow]
    parts: seq[Part]

type PartRange = object
    x: Slice[int]
    m: Slice[int]
    a: Slice[int]
    s: Slice[int]

proc parseRule(input: string): Rule =
    let op = if input.contains('>'): Operation.gt else: Operation.lt
    Rule(category: input[0], op: op, target: input[2..high(input)].parseInt())

proc parseInstructions(input: string): seq[Instruction] =
    var instructions = newSeq[Instruction]()
    for raw_rule in input.split(","):
        if raw_rule.contains(':'):
            let parts = raw_rule.split(':')
            instructions.add(Instruction(condition: some(parseRule(parts[0])), label: parts[1]))
        else:
            instructions.add(Instruction(condition: none(Rule), label: raw_rule))
    instructions

proc parseWorkflows(input: string): TableRef[string, Workflow] =
    var workflows = newTable[string, Workflow]()
    for line in input.splitLines():
        var matches: array[2, string]
        discard line.find(re"(\w+){([^}]+)}", matches)
        workflows[matches[0]] = Workflow(instructions: parseInstructions(matches[1]))
    workflows

proc parseParts(input: string): seq[Part] =
    var parts = newSeq[Part]()
    for line in input.splitLines():
        var matches: array[4, string]
        discard line.find(re"{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}", matches)
        parts.add(Part(x: matches[0].parseInt(), m: matches[1].parseInt(), a: matches[2].parseInt(), s: matches[3].parseInt()))
    parts

proc parseInput(): Input =
    let parts = readFile("input").split("\n\n")
    Input(workflows: parseWorkflows(parts[0]), parts: parseParts(parts[1]))

proc xmas(part: Part): int =
    part.x + part.m + part.a + part.s

proc xmas(part: PartRange): int =
    part.x.len() * part.m.len() * part.a.len() * part.s.len()

proc getValue(category: char, part: Part): int =
    case category
    of 'x': part.x
    of 'm': part.m
    of 'a': part.a
    of 's': part.s
    else: 0

proc applyOp(lhs: int, op: Operation, rhs: int): bool =
    case op
    of gt: lhs > rhs
    of lt: lhs < rhs

proc check(condition: Rule, part: Part): bool =
    applyOp(getValue(condition.category, part), condition.op, condition.target)

proc runOne(workflows: TableRef[string, Workflow], part: Part): bool =
    var current = "in"
    while not (current in ["A", "R"]):
        for instruction in workflows[current].instructions:
            if instruction.condition.isSome:
                if check(instruction.condition.get(), part):
                    current = instruction.label
                    break
            else:
                current = instruction.label
                break
    current == "A"

proc runAll(input: Input): int =
    var total = 0
    for part in input.parts:
        if runOne(input.workflows, part):
            total += part.xmas()
    total

proc splitSingle(r: Slice[int], op: Operation, target: int): (Slice[int], Slice[int]) =
    var left = r
    var right = r

    case op
    of gt: 
        left.a = max(r.a, target + 1)
        right.b = min(r.b, target)
    of lt: 
        left.b = min(r.b, target - 1)
        right.a = max(r.a, target)

    (left, right)

proc split(part: PartRange, rule: Rule): (PartRange, PartRange) =
    var left = part
    var right = part

    case rule.category
    of 'x':
        (left.x, right.x) = splitSingle(part.x, rule.op, rule.target)
    of 'm':
        (left.m, right.m) = splitSingle(part.m, rule.op, rule.target)
    of 'a':
        (left.a, right.a) = splitSingle(part.a, rule.op, rule.target)
    of 's':
        (left.s, right.s) = splitSingle(part.s, rule.op, rule.target)
    else:
        discard
    
    (left, right)

proc runCombRec(workflows: TableRef[string, Workflow], current: string, part: PartRange): int =
    if current == "R":
        0
    elif current == "A":
        part.xmas()
    elif part.xmas() == 0:
        0
    else:
        var total = 0
        var currentPart = part
        for instruction in workflows[current].instructions:
            if instruction.condition.isSome:
                let both = currentPart.split(instruction.condition.get())
                total += runCombRec(workflows, instruction.label, both[0])
                if both[1].xmas() == 0:
                    return total
                currentPart = both[1]
            else:
                return total + runCombRec(workflows, instruction.label, currentPart)
        total

proc runCombinations(input: Input): int =
    let possibilities = 1..4000
    let part = PartRange(x: possibilities, m: possibilities, a: possibilities, s: possibilities)
    runCombRec(input.workflows, "in", part)

let input = parseInput()
echo runAll(input)
echo runCombinations(input)
