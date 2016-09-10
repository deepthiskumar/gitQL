module VTextExamples where

{-

Example for VText.  Illustrates the use of the representation.

-}

import VText
import Edits

-- Example
--
-- root is the original document
-- edit A changes "two" to "1 + 1"
-- edit B changes the first "1" in "1 + 1" to "One" and add "four" at the end
-- edit C changes "One" to "ONE" and "1" to "I"
--
root :: Text
root = "one two three"

a :: VText
a = VText [Plain "one ",
           Chc 1 (plain "two") (plain "1 + 1"),
           Plain " three"]

b :: VText
b = VText [Plain "one ",
           Chc 1 (plain "two")
                 (VText [Chc 2 (plain "1") (plain "One"),
                         Plain " + 1"]),
           Plain " three",
           Chc 2 (plain "") (plain " four")]

c :: VText
c = VText [Plain "one ",
           Chc 1 (plain "two")
                 (VText [Chc 2 (plain "1") (VText [Chc 3 (plain "One") (plain "ONE")]),
                         Plain " + ",
                         Chc 3 (plain "1") (plain "I")]),
           Plain " three",
           Chc 2 (plain "") (plain " four")]


-- Edits
--
edits_c :: Edits
edits_c = edits c

revertible_c :: [Dim]
revertible_c = revertible c
