N ← {"one" "two" "three" "four" "five" "six" "seven" "eight" "nine"}
# Gets the first digit in the string
GetN ← parse⊔⊢↘1regex"^[^\\d]*(\\d)"
# |[needle n haystack] If the haystack starts with needle, replace it by n
ReplIf ← (⋅⊙⋅∘|⊂+@0:↘)⊃(⊢⬚@_⌕)(⊓⧻∘)⊙:
# |haystack If the haystack starts with a spelled out number, replace it by its digit
ReplLeading ← ∧(|2 ReplIf :/⊔)⍉⊟N+1⇡9
# |haystack Replaces all occurences of spelled out numbers, from left to right
ReplAll ← ;⍢(⊃(;:+1)⍜↻ReplLeading)(<⧻:) 0⊂:@_

Solve! ← /+⊜(+×10:⊃(GetN ⇌) GetN ^1) ≠@\n.&fras"input"
# Solve!∘
Solve!ReplAll
# Po ← +×10:⊃(GetN ⇌) GetN
Pt ← +×10:⊃(GetN ⇌) GetN ReplAll
# # ⊃Pt Po ≠@\n.&fras"input"
# # Solve!Pt
# # Pt "eightwothree"
# # ⍢∘(⊃) 0 "eightwothree"
# ReplAll "eightwothree" 

# not 53587
ReplAll "neo"
