module VPMTest where

import VPM
import Prelude hiding (seq)


--VTexts

v = [Str "xabyabcz"]
vc = [Chc 1 [Str "ab"] [Str "cd"],Chc 2 [Str "ef"] [Str "gh"]]
vc1 = [Chc 1 [Str "mabn"] [Str "xca"], Str "c"]
multipleAs =  [ Str "x",  Chc 1 ( [Str "aca"] ) ([Str "c"] )]
share =  [ Str "c",  Chc 1 ( [Str "aba"] ) ([Str  "c"] ), Str "a"]

-- |Case 1 - Only plain string
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
-- | Case 2 - only choices
-- >>> match a vc
-- [(0,[MChc 1 [(0,[MStr "a"])] []])]
-- >>> match (ch 'b') vc
-- [(0,[MChc 1 [(1,[MStr "b"])] []])]
-- >>> match (ch 'c') vc
-- [(0,[MChc 1 [] [(0,[MStr "c"])]])]
-- >>> match (ch 'd') vc
-- [(0,[MChc 1 [] [(1,[MStr "d"])]])]
-- >>> match (ch 'e') vc
-- [(1,[MChc 2 [(0,[MStr "e"])] []])]
-- >>> match (ch 'x') vc
-- []
-- >>> match a multipleAs
-- [(1,[MChc 1 [(0,[MStr "a"]),(2,[MStr "a"])] []])]
-- >>> match a [Chc 1 [Str "lm"] [Str "aaxa"]]
-- [(0,[MChc 1 [] [(0,[MStr "a"]),(1,[MStr "a"]),(3,[MStr "a"])]])]
-- >>> match ab [Chc 1 [Str "ab"] [Str "cd"]]
-- [(0,[MChc 1 [(0,[MStr "ab"])] []])]
-- >>> match ab [Chc 1 [Str "xab"] [Str "cd"]]
-- [(0,[MChc 1 [(1,[MStr "ab"])] []])]
-- >>> match ab [Chc 1 [Str "abab"] [Str "cd"]]
-- [(0,[MChc 1 [(0,[MStr "ab"]),(2,[MStr "ab"])] []])]
-- >>> match ab [Chc 1 [Str "abcab"] [Str "cd"]]
-- [(0,[MChc 1 [(0,[MStr "ab"]),(3,[MStr "ab"])] []])]
--
-- | Parallel matches
-- >>> match a [Chc 1 [Str "ab"] [Str "ca"]]
-- [(0,[MChc 1 [(0,[MStr "a"])] [(1,[MStr "a"])]])]
-- >>> match a [Chc 1 [Str "aba"] [Str "ca"]]
-- [(0,[MChc 1 [(0,[MStr "a"]),(2,[MStr "a"])] [(1,[MStr "a"])]])]
-- >>> match a [Chc 1 [Str "aaa"] [Str "ca"]]
-- [(0,[MChc 1 [(0,[MStr "a"]),(1,[MStr "a"]),(2,[MStr "a"])] [(1,[MStr "a"])]])]
-- >>> match ab [Chc 1 [Str "abcdyab"] [Str "xabab"]]
-- [(0,[MChc 1 [(0,[MStr "ab"]),(5,[MStr "ab"])] [(3,[MStr "ab"])]])]

-- |Case 3L
-- >>> match ab [Str "a", Chc 1 [Str "b"] [Str "c"]]
-- [(0,[MStr "a",MChc 1 [(0,[MStr "b"])] []])]
-- >>> match ab [Str "a", Chc 1 [Str "cb"] [Str "bc"]]
-- [(0,[MStr "a",MChc 1 [] [(0,[MStr "b"])]])]
-- >>> match ab [Str "a", Chc 1 [Str "cb"] [Str "c"]]
-- []
-- >>> match abc [Str "ab", Chc 1 [Str "c"] [Str "d"]]
-- [(0,[MStr "ab",MChc 1 [(0,[MStr "c"])] []])]
-- >>> match abc [Str "a", Chc 1 [Str "bcd"] [Str "ef"]]
-- [(0,[MStr "a",MChc 1 [(0,[MStr "bc"])] []])]
-- >>> match ab [Str "a", Chc 1 [Str "bcab"] [Str "c"]]
-- [(0,[MStr "a",MChc 1 [(0,[MStr "b"]),(2,[MStr "ab"])] []])]
--
-- |parallel matches
-- >>> match (VPM.seq [c,a]) share
-- [(0,[MStr "c",MChc 1 [(0,[MStr "a"])] []])]
-- >>> match ab [Str "a", Chc 1 [Str "bd"] [Str "be"]]
-- [(0,[MStr "a",MChc 1 [(0,[MStr "b"])] [(0,[MStr "b"])]])]

-- |Case 3R
-- >>> match ab [Chc 1 [Str "a"] [Str "c"],Str "b"]
-- [(0,[MChc 1 [(0,[MStr "a"])] [],MStr "b"])]
-- >>> match ab [Chc 1 [Str "x"] [Str "a"],Str "b"]
-- [(0,[MChc 1 [] [(0,[MStr "a"])],MStr "b"])]
-- >>> match ab [Chc 1 [Str "xa"] [Str "c"],Str "b"]
-- [(0,[MChc 1 [(1,[MStr "a"])] [],MStr "b"])]
-- >>> match ab [Chc 1 [Str "x"] [Str "ya"],Str "b"]
-- [(0,[MChc 1 [] [(1,[MStr "a"])],MStr "b"])]
-- >>> match abc [Chc 1 [Str "ab"] [Str "x"],Str "abc"]
-- [(1,[MStr "abc"])]
-- >>> match ab [Str "a", Chc 1 [Str "bca"] [Str "c"], Str "bcx"]
-- [(0,[MStr "a",MChc 1 [(0,[MStr "b"])] []]),(1,[MChc 1 [(2,[MStr "a"])] [],MStr "b"])]
-- >>> match ab [Str "a", Chc 1 [Str "bca"] [Str "c"], Str "x"]
-- [(0,[MStr "a",MChc 1 [(0,[MStr "b"])] []])]
--
-- | these are not parallel matches
-- >>> match ab [Chc 1 [Str "ax"] [Str "ya"],Str "b"]
-- [(0,[MChc 1 [] [(1,[MStr "a"])],MStr "b"])]
-- >>> match ab [Chc 1 [Str "xa"] [Str "ay"],Str "b"]
-- [(0,[MChc 1 [(1,[MStr "a"])] [],MStr "b"])]
--
-- |parallel matches
-- >>> match (VPM.seq [c,a]) [ Str "x",  Chc 1 ( [Str "xba"] ) ([Str  "c"] ), Str "a"]
-- [(1,[MChc 1 [] [(0,[MStr "c"])],MStr "a"])]
-- >>> match ab [Chc 1 [Str "xa"] [Str "ya"], Str "b"]
-- [(0,[MChc 1 [(1,[MStr "a"])] [(1,[MStr "a"])],MStr "b"])]
-- >>> match ab [Chc 1 [Str "xa"] [Str "yab"], Str "b"]
-- [(0,[MChc 1 [] [(1,[MStr "ab"])]]),(0,[MChc 1 [(1,[MStr "a"])] [],MStr "b"])]
-- >>> match ab [Chc 1 [Str "a"] [Str "ab"], Str "b"]
-- [(0,[MChc 1 [] [(0,[MStr "ab"])]]),(0,[MChc 1 [(0,[MStr "a"])] [],MStr "b"])]
-- >>> match abc [Chc 1 [Str "abcyab"] [Str "lmnab"], Str "c"]
--
-- |Following needs to be resolved
-- |Error: Position withing the choice is lost and therefore
-- >>> match (seq [a,a,b]) [Str "a", Chc 1 [Str "bcaa"] [Str "c"], Str "abx"]
-- [(0,[MChc 1 [(0,[MStr "a"])] [],MStr "ab"])]
--
-- |Error:  "ca" from the right alternative needs to be matches
-- >>> match (VPM.seq [c,a]) [ Str "c", Chc 1 ( [Str "aba"] ) ([Str  "c"] ), Str "a"]
-- [(0,[MStr "c",MChc 1 [(0,[MStr "a"])] []])]
--
-- |Error : resolve overlap between the matched value of StrSplit and PatSplit
-- >>> match ab [Chc 1 [Str "xab"] [Str "a"], Str "b"]
-- [(0,[MChc 1 [(1,[MStr "ab"])] []]),(0,[MChc 1 [] [(0,[MStr "a"])],MStr "b"])]
--
-- |Error: start 2 threads for matching when PM occurs
-- >>> match abc [Chc 1 [Str "abcya"] [Str "lmnab"], Str "c"]







