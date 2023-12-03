Input ← ⊜∘≠@\n.&fras"input"
# (times, array) -> array
Pad ← ⍥(⬚@.⊂:@.⬚@.⊂@.≡(⊂:@.⊂@.))
# (3x3 array) -> bool
IsNumberWithSymbol ← (0|¬/×+=@.:×≤@9,≥@0.♭)×≤@9:≥@0.⊡1_1.
# (input) -> digits next to symbols mask
FindSymbolAdjacent ← ≡IsNumberWithSymbol☇2◫3_3 Pad 1
# (input) -> mask with digits
ExtractNumbers ← ∵(⍣parse (∞;;))♭

⊃ExtractNumbers FindSymbolAdjacent Input
# Split into boxes of numbers and symbol mask
⊜□≠∞⊙(⍉⊟).
?
# Extract numbers near a symbol
∵(|1 ×±/∘ ≡(/(+×10))⍉ ⊔)
?
/+

# 420911 too low 
