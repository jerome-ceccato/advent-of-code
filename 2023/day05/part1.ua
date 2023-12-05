# (input string) -> seeds, convertion maps
ParseInput ← (
  # Box all sections
  ⊜(□⊜(⊜parse≠@ .)≠@\n.⊔⊢↘1⊜□≠@:.)¬⌕"\n\n".
  # Split the seed list and convertion maps
  ⊙(↘1) ⊔♭⊢.
)

# (in, convertion) -> out
Convert ← (
  ≡(
    # Extract the ranges
    .⊙(|1.2 -1 /∘):\+↘1,:
    # Check if the seed number is in bounds
    :⊙(×⊙≤:⊙≥.)
    # Return the converted value or inf
    (∞;;||2 +⊙-:/∘↙2:)
  ):⊙:.⊙⊔
  # Take the converted value or the original value if no convertion succeeded
  (;|⊢⊙;)⧻.▽≠∞.
)

# ([seeds], convertion maps) -> [locations], convertion maps
GetLocationFromSeeds ← (
  ⊙;≡∧(Convert:),⊙(¤.)
)

/↧⊙; GetLocationFromSeeds ParseInput &fras"input"
