N ← {"one" "two" "three" "four" "five" "six" "seven" "eight" "nine"}
GetN ← parse⊔⊢↘1regex"^[^\\d]*(\\d)"
StrRepl ← ⋅⊙⋅∘∧(:⍜↻(⊂:⊙:↘:)⊙⊙∩.⊙:)⇌⊚⊃⌕(⊓⧻∘)⊙:
ReplAll ← ∧(|2 StrRepl:+@0/⊔)⍉⊟N+1⇡9
Solve! ← /+⊜^1 ≠@\n.&fras"input"
Po ← +×10:⊃(GetN ⇌) GetN
Pt ← +×10:⊃(GetN ⇌) GetN ReplAll
# ⊃Pt Po ≠@\n.&fras"input"
# Solve!Po
Solve!Pt
Pt "eightwothree"
