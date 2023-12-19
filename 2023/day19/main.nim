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

let input = parseInput()
echo runAll(input)
