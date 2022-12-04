Object subclass: Aoc [
    | pairs |

    readFrom: filename [
        | x input |
        "Reads from the given file and stores each line internally"

        pairs := OrderedCollection new.
        input := FileStream open: 'input' mode: FileStream read.
        [(x := input nextLine) notNil] whileTrue: [pairs add: x].
    ]

    pairToRange: pair [
        | split left right |
        "Splits a pair string into 2 arrays of number pairs"

        split := pair substrings: ','.
        left := ((split at: 1) substrings: '-') collect: [:a | a asNumber].
        right := ((split at: 2) substrings: '-') collect: [:a | a asNumber].

        ^Array with: left with: right
    ]

    doesRange: left contain: right [
        | lower upper |
        "Returns whether or not the left range contains the right range"

        lower := (left at: 1) <= (right at: 1).
        upper := (left at: 2) >= (right at: 2).
        ^lower & upper
    ]

    checkRangeOverlap: ranges [
        | leftContainsRight rightContainsLeft |
        "Returns whether or not a range contains the other"

        leftContainsRight := self doesRange: (ranges at: 1) contain: (ranges at: 2).
        rightContainsLeft := self doesRange: (ranges at: 2) contain: (ranges at: 1).
        ^leftContainsRight | rightContainsLeft
    ]

    compute [
        | acc |
        "Solves part 1"

        acc := 0.
        pairs do: [ :pair |
            | ranges isContained |
            
            ranges := self pairToRange: pair.
            (self checkRangeOverlap: ranges) ifTrue: [acc := acc + 1].
        ].
        ^acc
    ]
]

result := (Aoc new) readFrom: 'input'; compute .
Transcript show: result printString; cr.