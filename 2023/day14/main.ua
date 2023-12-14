Input ← ⊜∘≠@\n.&fras"input"
Slide ← ≡(⍜⊜□∵⊐(□⊏⍖.)≠@#.)
Score ← /+×⊏⍖.+1⇡⧻.≡/+=@O
Cycle ← (
  # North
  ⍜⍉Slide
  # West
  Slide
  # South
  ⍜⇌(⍜⍉Slide)
  # East
  ⍜(≡⇌)Slide
)
# 105 is the first occurence of the loop and 39 is the period
# It was easier to calculate it manually on the first 200 results
# rather than coding a generic solution
Target ← ⍜(-105)(◿39) 1e9

Score ⍜⍉Slide Input
Score ⍥Cycle Target Input
