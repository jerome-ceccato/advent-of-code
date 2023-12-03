Input ← ⊜∘≠@\n.&fras"input"
# (times, array) -> array
Pad ← ⍥(⬚@.⊂:@.⬚@.⊂@.≡(⊂:@.⊂@.))
# (3x3 array) -> bool
IsNumberWithSymbol ← (0|¬/×+=@.:×≤@9,≥@0.♭)×≤@9:≥@0.⊡1_1.
# (input) -> number mask
# FindNumbers ← ♭×≥@0:≤@9.
# (input) -> digits next to symbols mask
FindSymbolAdjacent ← ≡IsNumberWithSymbol☇2◫3_3 Pad 1
# (input) -> mask with digits
ExtractNumbers ← ∵(⍣parse (0;;))♭
# ⊃FindSymbolAdjacent FindNumbers .Input
# ⊙(∵(⍣parse (0;;))♭:)
# ⊜□:⍉⊟,
# ⍉⊟,
# ⊜parse ∩♭×≥@0,≤@9. Input
# NumberWithSymbol ["..." ".56" "3*."]

⊃ExtractNumbers FindSymbolAdjacent Input
# Split into boxes of numbers and symbol mask
⊜□±⊙(⍉⊟).
?
# Extract numbers near a symbol
∵(|1 ×±/∘ ≡(/(+×10))⍉ ⊔)
?
/+
# sign/∘[467 1]
# 420911 too low 
