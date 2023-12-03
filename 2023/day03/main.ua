Input ← ⊜∘≠@\n.&fras"input"
# (times, array) -> array
Pad ← ⍥(⬚@.⊂:@.⬚@.⊂@.≡(⊂:@.⊂@.))
# (str) -> [bool]
IsSymbol ← ¬+=@.:×≤@9,≥@0.
# (3x3 array) -> bool
IsNumberWithSymbol ← (0|±/+IsSymbol♭)×≤@9:≥@0.⊡1_1.
# (input) -> digits next to symbols mask
FindSymbolAdjacent ← ≡IsNumberWithSymbol☇2◫3_3 Pad 1
# (input) -> mask with digits
ExtractNumbers ← ∵(⍣parse (∞;;))♭
# (input) -> [parts]
GetPartNumbers ← (
  ⊃ExtractNumbers FindSymbolAdjacent
  # Split into boxes of numbers and symbol mask
  ⊜□≠∞⊙(⍉⊟).
  # Extract numbers near a symbol
  ∵(|1 ×±/∘ ≡(/(+×10))⍉ ⊔)
)
# p1 
# /+ GetPartNumbers Input

# (input) -> [windows with * centered]
PossibleGearWindows ← ≡(↙3↘2) ▽≡(=@*⊡3_3).☇2◫7_7 Pad 3

# (gear windows) -> (gear windows without additional symbols)
CleanNonGearSymbols ← ≡(
  # Flatten and remove all symbols
  ∧(⍜⊡(@.;)) ⊚ IsSymbol.♭
  # Add the gear back and revert shape
  ↯3_7⍜⊡(@*;)10
)

PossibleGearWindows Input
?↙10↘10

CleanNonGearSymbols
≡(
  ?
  ▽±.GetPartNumbers
  ×/×:=2⧻.
  ?
)
?
/+
# 339311890 too high
