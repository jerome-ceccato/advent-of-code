ParseInput ← ⊜(⊜□≠@ .)≠@\n.
Occurences ← ≡(□⊏⍏.⊕⧻⊛.⊏⍏.°□)⍉⊢⍉
GetHandType ← ⊢⊚∊{[5] 1_4 2_3 1_1_3 1_2_2 1_1_1_2 1_1_1_1_1}
GroupByType ← ⊃(⊕□⊛)(⊏⍖.⊝)⊏⍖.⊙(⊏⍖).
ExtractOrderedBids ← ▽≠∞.♭⬚∞∵(∵⊐⋕↘1⍉°□)
OrderSubGroups ← ⊏⍖⊗⊙:∵°□⊢⍉.

RunGame! ← (
  ¤
  ParseInput &fras"input"
  ^1.
  Occurences
  ≡GetHandType
  ⊙;GroupByType
  ≡(□OrderSubGroups °□)
  ExtractOrderedBids
  /+×+1⇡⧻.
)

MostCommonCharacter ← (
  # Ignore J because we're trying to replace it
  ▽≠@J.
  # but break early if the array was only J's
  >0⧻.
  (
    @J
  | ⊕□⊛.⊏⍏.
    ⊢°□⊢⊏⍖∵⧻.
  )
)

ReplaceJoker ← ⍜⊏(↯△)⊚=@J.:MostCommonCharacter.
ReplaceJokers ← ≡(⍜⊡(□ReplaceJoker°□) 0)

# p1
RunGame!∘ "AKQJT98765432"
# p2
RunGame!ReplaceJokers "AKQT98765432J"
