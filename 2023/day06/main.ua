ParseInputA ← ⍉⊜(⊜parse≠@ .⊔⊡1⊜□≠@:.)≠@\n.
ParseInputB ← ⊜(parse▽≠@ .⊔⊡1⊜□≠@:.)≠@\n.
GetWinners ← |1 ⧻▽<,:;∵(×⊃-⊙∘)⍜¤↘1⇡.:/∘

/×≡GetWinners ParseInputA &fras"input"
GetWinners ParseInputB &fras"input"
