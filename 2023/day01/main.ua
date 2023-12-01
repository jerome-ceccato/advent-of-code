N ← {"one" "two" "three" "four" "five" "six" "seven" "eight" "nine"}
# Gets the first digit in the string
GetN ← parse⊔⊢↘1regex"^[^\\d]*(\\d)"
# |[needle n haystack] If the haystack starts with needle, replace the first character by n
# Originally built to replace the whole word so the implementation might be clunky
ReplIf ← (⋅⊙⋅∘|⊂+@0:↘)⊃(⊢⬚@_⌕)(⊓(1;)∘)⊙:
# |haystack If the haystack starts with a spelled out number, replace it by its digit
ReplLeading ← ∧(|2 ReplIf :/⊔)⍉⊟N+1⇡9
# |haystack Replaces all occurences of spelled out numbers, from left to right
ReplAll ← ;⍢(⊃(;:+1)⍜↻ReplLeading)(<⧻:) 0⊂:@_

Solve! ← /+⊜(+×10:⊃(GetN ⇌) GetN ^1) ≠@\n.&fras"input"
# Part 1
Solve!∘
# Part 2
Solve!ReplAll
