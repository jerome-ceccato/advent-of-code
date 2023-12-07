ParseInput ← ⊜(⊜□≠@ .)≠@\n.
Occurences ← ≡(□⊏⍏.⊕⧻⊛.⊏⍏.°□)⍉⊢⍉
GetHandType ← ⊢⊚∊{[5] 1_4 2_3 1_1_3 1_2_2 1_1_1_2 1_1_1_1_1}
GroupByType ← ⊃(⊕□⊛)(⊏⍖.⊝)⊏⍖.⊙(⊏⍖).
ExtractOrderedBids ← ▽≠∞.♭⬚∞∵(∵⊐⋕↘1⍉°□)
OrderSubGroups ← ⊏⍖⊗:"AKQJT98765432"∵°□⊢⍉.

ParseInput &fras"input"
Occurences.
≡GetHandType
⊙;GroupByType

≡(□OrderSubGroups °□)
ExtractOrderedBids
/+×+1⇡⧻.
