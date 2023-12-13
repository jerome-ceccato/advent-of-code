Input ← ⊜(□⊜∘≠@\n.)¬⌕"\n\n".&fras"input"
TryReflect ← /×/×=⊃↙(↙⊙⇌)↧⊙-.⊙(⧻.↻).
GetReflectionIdx ← ([0]|∘)⊃(>0/+)(+1⊚)
FindRowReflection ← GetReflectionIdx≡TryReflect⊙¤+1⇡-1⧻.
PatternScore ← ⊂⊃(FindRowReflection⍉)(×100 FindRowReflection)

FindFixedScore ← (
  ⊃∘(/+PatternScore °□)
  # generate all possibilities
  ⊙(¤ ♭:)⇡/×.△.=@#°□
  ↯⊂⊙:⧻.≡(⍜⊡(¬))

  # get the different score
  ♭⬚0≡°□≡(□PatternScore)
  ▽≠0.
  ⊢▽:⊙≠.
)

/+ ≡(/+PatternScore°□) Input
/+≡FindFixedScore Input
