module VPMTest where

import VPM
import Prelude hiding (seq)


--VTexts

v = [Str "xabyabcz"]
vc = [Chc 1 [Str "ab"] [Str "cd"],Chc 2 [Str "ef"] [Str "gh"]]
vc1 = [Chc 1 [Str "mabn"] [Str "xca"], Str "c"]
multipleAs =  [ Str "x",  Chc 1 ( [Str "aca"] ) ([Str "c"] )]
share =  [ Str "c",  Chc 1 ( [Str "aba"] ) ([Str  "c"] ), Str "a"]

-- |Tests - Only plain string
-- >>> match ab v
-- [(1,[MStr "ab"]),(4,[MStr "ab"])]
-- >>> match abc v
-- [(4,[MStr "abc"])]
-- >>> match _a v
-- [(0,[MStr "xa"]),(3,[MStr "ya"])]
-- >>> match a'b v
-- [(1,[MStr "a"]),(2,[MStr "b"]),(4,[MStr "a"]),(5,[MStr "b"])]
-- >>> match a v
-- [(1,[MStr "a"]),(4,[MStr "a"])]
-- >>> match abc [Str "aab", Str "cxabc"]
-- [(1,[MStr "abc"]),(5,[MStr "abc"])]
-- >>> match (seq [a,a]) [Str "abab", Str "cxaaaabc"]
-- [(6,[MStr "aa"]),(8,[MStr "aa"])]
-- >>> match abc [Str "abcab", Str "cxab"]
-- [(0,[MStr "abc"]),(3,[MStr "abc"])]
-- >>> match ab [Str "abca", Str "bxab"]
-- [(0,[MStr "ab"]),(3,[MStr "ab"]),(6,[MStr "ab"])]
-- 
-- | Tests - with choices
-- |>>> match a vc
-- [(0,[MChc 1 [(0,[MChr 'a'])] []])]
-- |>>> match (ch 'b') vc
-- [(0,[MChc 1 [(1,[MChr 'b'])] []])]
-- |>>> match (ch 'c') vc
-- [(0,[MChc 1 [] [(0,[MChr 'c'])]])]
-- |>>> match (ch 'd') vc
-- [(0,[MChc 1 [] [(1,[MChr 'd'])]])]
-- |>>> match (ch 'e') vc
-- [(1,[MChc 2 [(0,[MChr 'e'])] []])]
-- |>>> match (ch 'x') vc
-- []
-- 
-- we need 2 separate matches and encode both the matches in one
-- |>>> match a multipleAs
-- [(1,[MChc 1 [(0,[MChr 'a'])] []]),(2,[MChc 1 [(1,[MChr 'a'])] []])]
-- |>>> match (VPM.seq [c,a]) share
-- [(0,[MChr 'c',MChc 1 [(0,[MChr 'a'])] []])]
-- |>>> match (VPM.seq [c,a]) [ Chr 'x',  Chc 1 ( [Chr 'x',Chr  'b', Chr 'a'] ) ([Chr  'c'] ), Chr 'a']
-- [(1,[MChc 1 [] [(0,[MChr 'c'])],MChr 'a'])]
-- |>>> match (VPM.seq [c,a]) [ Chr 'c',  Chc 1 ( [Chr 'a',Chr  'b', Chr 'a'] ) ([Chr  'c'] ), Chr 'a']
-- 

