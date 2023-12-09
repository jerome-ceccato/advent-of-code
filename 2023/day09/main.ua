Input ← ⊜(⊜⋕≠@ .)≠@\n.&fras"input"

AddDiffRow ← (
  .
  ⊢⇌
  ↻¯1.
  ↘1-
  ⊂:⊂∞
)
EndOfDiff ← (
  ⊢⇌
  ▽≠∞.
  ▽≠¯∞.
  ≠⧻:/+=0.
)
CollapseHistory ← /+⊢⇌⍉
CollapseHistoryBackwards ← /-⇌⊢⍉
RotateHistory ← ≡(↻/++=¯∞,=∞.)

# p1
Input
≡(□⍢AddDiffRow EndOfDiff¤)
/+≡(CollapseHistory°□)

# p2
Input
≡(□⍢AddDiffRow EndOfDiff¤)
/+≡(CollapseHistoryBackwards RotateHistory °□)
