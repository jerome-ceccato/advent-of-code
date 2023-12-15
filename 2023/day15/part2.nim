import std/strutils
import std/sequtils
import std/tables

proc parseInput(): seq[string] =
    readFile("input").strip().split(",")

proc hash(s: string): int =
    var h = 0
    for c in s:
        h = ((h + ord(c)) * 17) mod 256
    h

proc updateBox(box: var seq[(string, int)], item: (string, int)) =
    for i in low(box)..high(box):
        if box[i][0] == item[0]:
            box[i] = item
            return
    box.add(item)

proc buildHashmap(input: seq[string]): TableRef[int, seq[(string, int)]] =
    var hashmap = newTable[int, seq[(string, int)]]()
    for op in input:
        if op.contains('='):
            let parts = op.split('=')
            let item = (parts[0], parts[1].parseInt())
            let id = hash(item[0])
            updateBox(hashmap.mgetOrPut(id, newSeq[(string, int)]()), item)
        elif op.endsWith('-'):
            let label = op[low(op)..<high(op)]
            let id = hash(label)
            if hashmap.hasKey(id):
                hashmap[id] = hashmap[id].filterIt(it[0] != label)
    hashmap

proc focusingPower(hashmap: TableRef[int, seq[(string, int)]]): int =
    var total = 0
    for id, boxes in hashmap:
        for i in low(boxes)..high(boxes):
            total += (id + 1) * (i + 1) * boxes[i][1]
    total

let input = parseInput()
echo focusingPower(buildHashmap(input))

