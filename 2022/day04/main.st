Object subclass: Aoc [
    | pairs checksFullOverlap |

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

    doesRange: left fullyContain: right [
        | lower upper |
        "Returns whether or not the left range contains the right range"

        lower := (left at: 1) <= (right at: 1).
        upper := (left at: 2) >= (right at: 2).
        ^lower & upper
    ]

    doesRange: left overlap: right [
        | a1 a2 b1 b2 lower upper |
        "Returns whether or the two ranges overlap"

        a1 := (left at: 1).
        a2 := (left at: 2).
        b1 := (right at: 1).
        b2 := (right at: 2).

        lower := (a1 <= b1) & (a2 >= b1).
        upper := (a1 <= b2) & (a2 >= b2).
        ^lower | upper
    ]

    checkRangeOverlap: ranges [
        | leftContainsRight rightContainsLeft |
        "Returns whether or not a range contains the other"

        checksFullOverlap
            ifTrue: [
                leftContainsRight := self doesRange: (ranges at: 1) fullyContain: (ranges at: 2).
                rightContainsLeft := self doesRange: (ranges at: 2) fullyContain: (ranges at: 1).
                ^leftContainsRight | rightContainsLeft
            ]
            ifFalse: [
                leftContainsRight := self doesRange: (ranges at: 1) overlap: (ranges at: 2).
                rightContainsLeft := self doesRange: (ranges at: 2) overlap: (ranges at: 1).
                ^leftContainsRight | rightContainsLeft
            ]
    ]

    computeFullOverlap: fullOverlap [
        | acc |
        "Solves the puzzle"

        checksFullOverlap := fullOverlap.
        acc := 0.
        pairs do: [ :pair |
            | ranges isContained |
            
            ranges := self pairToRange: pair.
            (self checkRangeOverlap: ranges) ifTrue: [acc := acc + 1].
        ].
        ^acc
    ]
]

solver := (Aoc new) readFrom: 'input'.
Transcript show: (solver computeFullOverlap: true) printString; cr.
Transcript show: (solver computeFullOverlap: false) printString; cr.