# (color) -> colorid
ColorToId ← ⊗□:{"red" "green" "blue"}
# (color, amount, bag) -> bag
AddCubes ← ⍜⊡+⊙:ColorToId
# (roll str) -> bag
SplitRoll ← ⊜(⍜⊡(□parse)0↘1regex"\\s*(\\d+) (red|blue|green)")≠@,.
# (roll) -> bag
GetRoll ← ∧(|2 AddCubes/⊔):0_0_0 SplitRoll
# (game) -> [bag]
GetGame ← ⊜GetRoll≠@;.↘+2⊗@:.
# (input) -> [[bag]]
GetInput ← ⬚0⊜GetGame≠@\n.
Input ← GetInput &fras"input"
# (pattern, [[bag]]) -> [valid]
GetValidIds ← ×+1⇡⧻.¬≡(±/+>)¤⊙(≡/↥)

/+ GetValidIds 12_13_14 Input
/+≡/×≡/↥ Input
