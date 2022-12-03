USING: kernel math io prettyprint 
io.encodings.utf8 io.files sequences ; 

IN: aoc

: solve ( x -- )
0 "input" utf8 file-lines [ [ dup ] 2dip swap [ swap index ] dip + ] each . drop ;

! part 1
{ "" "B X" "C Y" "A Z" "A X" "B Y" "C Z" "C X" "A Y" "B Z" } solve
! part 2
{ "" "B X" "C X" "A X" "A Y" "B Y" "C Y" "C Z" "A Z" "B Z" } solve

