Input ← &fras"input"
ParseGame ← (
  # Extract hands
  ⬚@ ⊜∘≠@|.↘+2⊚=@:.
  # Parse into arrays
  ⬚∞≡(⊜parse≠@ .)
)
WinningNumbers ← |1 ▽∊,/∘
CardScore ← (0|ⁿ:2-1)≠0.⧻

# p1
/+⊜(|1 CardScore WinningNumbers ParseGame)≠@\n.Input

DupCards ← (|2
  # Extract target line
  /∘⊡⊙:.⊙.
  # Generate increase targets
  ⊙(+1+⇡)
  # Discard oob indexes
  ⊙(▽<⊙.:⊙(⧻.))
  # Rearange stack
  ⊙::
  # Updates target rows
  ⍜⊏(+↯:⊙(△.)⊂0:)
)

# p2
# Get amount of winning number per game
⊜(⧻ WinningNumbers ParseGame)≠@\n.Input
# Associate winning numbers with amount of cards (1)
⍉⊟:↯:1⧻.
# Dup all cards
∧DupCards⇡⧻.
# Count cards
/+♭↘1⍉
