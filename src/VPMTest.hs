module VPMTest where

import VPM
import Prelude hiding (seq)


--VTexts

v = [Str "xabyabcz"]
vc = [Chc 1 [Str "ab"] [Str "cd"],Chc 2 [Str "ef"] [Str "gh"]]
vc1 = [Chc 1 [Str "mabn"] [Str "xca"], Str "c"]
multipleAs =  [ Str "x",  Chc 1 ( [Str "aca"] ) ([Str "c"] )]
share =  [ Str "c",  Chc 1 ( [Str "aba"] ) ([Str  "c"] ), Str "a"]

--Patterns
cp = PChc (D 1) (ch 'a') (ch 'b')
cp1 = PChc (D 1) ab (seq $ map ch "ac")
cp2 = PChc (D 1) a a
d = PChc (DVar "d") (ch 'a') (ch 'b')
d1 = PChc (DVar "d") ab (seq $ map ch "ac")

-- | Case 1 - Only plain string
-- >>> match ab v
-- [VM [] 1 [MStr "ab"],VM [] 4 [MStr "ab"]]
-- >>> match abc v
-- [VM [] 4 [MStr "abc"]]
-- >>> match _a v
-- [VM [] 0 [MStr "xa"],VM [] 3 [MStr "ya"]]
-- >>> match a'b v
-- [VM [] 1 [MStr "a"],VM [] 2 [MStr "b"],VM [] 4 [MStr "a"],VM [] 5 [MStr "b"]]
-- >>> match a v
-- [VM [] 1 [MStr "a"],VM [] 4 [MStr "a"]]
-- >>> match abc [Str "aab", Str "cxabc"]
-- [VM [] 1 [MStr "abc"],VM [] 5 [MStr "abc"]]
-- >>> match (seq [a,a]) [Str "abab", Str "cxaaaabc"]
-- [VM [] 6 [MStr "aa"],VM [] 8 [MStr "aa"]]
-- >>> match abc [Str "abcab", Str "cxab"]
-- [VM [] 0 [MStr "abc"],VM [] 3 [MStr "abc"]]
-- >>> match ab [Str "abca", Str "bxab"]
-- [VM [] 0 [MStr "ab"],VM [] 3 [MStr "ab"],VM [] 6 [MStr "ab"]]
--
-- | Case 2 - only choices
-- >>> match a vc
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "a"]] []]]
-- >>> match (ch 'b') vc
-- [VM [] 0 [MChc 1 [VM [] 1 [MStr "b"]] []]]
-- >>> match (ch 'c') vc
-- [VM [] 0 [MChc 1 [] [VM [] 0 [MStr "c"]]]]
-- >>> match (ch 'd') vc
-- [VM [] 0 [MChc 1 [] [VM [] 1 [MStr "d"]]]]
-- >>> match (ch 'e') vc
-- [VM [] 1 [MChc 2 [VM [] 0 [MStr "e"]] []]]
-- >>> match (ch 'x') vc
-- []
-- >>> match a multipleAs
-- [VM [] 1 [MChc 1 [VM [] 0 [MStr "a"],VM [] 2 [MStr "a"]] []]]
-- >>> match a [Chc 1 [Str "lm"] [Str "aaxa"]]
-- [VM [] 0 [MChc 1 [] [VM [] 0 [MStr "a"],VM [] 1 [MStr "a"],VM [] 3 [MStr "a"]]]]
-- >>> match ab [Chc 1 [Str "ab"] [Str "cd"]]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "ab"]] []]]
-- >>> match ab [Chc 1 [Str "xab"] [Str "cd"]]
-- [VM [] 0 [MChc 1 [VM [] 1 [MStr "ab"]] []]]
-- >>> match ab [Chc 1 [Str "abab"] [Str "cd"]]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "ab"],VM [] 2 [MStr "ab"]] []]]
-- >>> match ab [Chc 1 [Str "abcab"] [Str "cd"]]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "ab"],VM [] 3 [MStr "ab"]] []]]
--
-- | Parallel matches
-- >>> match a [Chc 1 [Str "ab"] [Str "ca"]]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 1 [MStr "a"]]]]
-- >>> match a [Chc 1 [Str "aba"] [Str "ca"]]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "a"],VM [] 2 [MStr "a"]] [VM [] 1 [MStr "a"]]]]
-- >>> match a [Chc 1 [Str "aaa"] [Str "ca"]]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "a"],VM [] 1 [MStr "a"],VM [] 2 [MStr "a"]] [VM [] 1 [MStr "a"]]]]
-- >>> match ab [Chc 1 [Str "abcdyab"] [Str "xabab"]]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "ab"],VM [] 5 [MStr "ab"]] [VM [] 3 [MStr "ab"]]]]

-- |Case 3L
-- >>> match ab [Str "a", Chc 1 [Str "b"] [Str "c"]]
-- [VM [] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "b"]] []]]
-- >>> match ab [Str "a", Chc 1 [Str "cb"] [Str "bc"]]
-- [VM [] 0 [MStr "a",MChc 1 [] [VM [] 0 [MStr "b"]]]]
-- >>> match ab [Str "a", Chc 1 [Str "cb"] [Str "c"]]
-- []
-- >>> match abc [Str "ab", Chc 1 [Str "c"] [Str "d"]]
-- [VM [] 0 [MStr "ab",MChc 1 [VM [] 0 [MStr "c"]] []]]
-- >>> match abc [Str "a", Chc 1 [Str "bcd"] [Str "ef"]]
-- [VM [] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "bc"]] []]]
-- >>> match ab [Str "a", Chc 1 [Str "bcab"] [Str "c"]]
-- [VM [] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "b"],VM [] 2 [MStr "ab"]] []]]
--
-- |parallel matches
-- >>> match (VPM.seq [c,a]) share
-- [VM [] 0 [MStr "c",MChc 1 [VM [] 0 [MStr "a"]] []]]
-- >>> match ab [Str "a", Chc 1 [Str "bd"] [Str "be"]]
-- [VM [] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "b"]] [VM [] 0 [MStr "b"]]]]

-- |Case 3R
-- >>> match ab [Chc 1 [Str "a"] [Str "c"],Str "b"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "a"]] [],MStr "b"]]
-- >>> match ab [Chc 1 [Str "x"] [Str "a"],Str "b"]
-- [VM [] 0 [MChc 1 [] [VM [] 0 [MStr "a"]],MStr "b"]]
-- >>> match ab [Chc 1 [Str "xa"] [Str "c"],Str "b"]
-- [VM [] 0 [MChc 1 [VM [] 1 [MStr "a"]] [],MStr "b"]]
-- >>> match ab [Chc 1 [Str "x"] [Str "ya"],Str "b"]
-- [VM [] 0 [MChc 1 [] [VM [] 1 [MStr "a"]],MStr "b"]]
-- >>> match abc [Chc 1 [Str "ab"] [Str "x"],Str "abc"]
-- [VM [] 1 [MStr "abc"]]
-- >>> match ab [Str "a", Chc 1 [Str "bca"] [Str "c"], Str "bcx"]
-- [VM [] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "b"]] []],VM [] 1 [MChc 1 [VM [] 2 [MStr "a"]] [],MStr "b"]]
-- >>> match ab [Str "a", Chc 1 [Str "bca"] [Str "c"], Str "x"]
-- [VM [] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "b"]] []]]
--
-- | these are not parallel matches
-- >>> match ab [Chc 1 [Str "ax"] [Str "ya"],Str "b"]
-- [VM [] 0 [MChc 1 [] [VM [] 1 [MStr "a"]],MStr "b"]]
-- >>> match ab [Chc 1 [Str "xa"] [Str "ay"],Str "b"]
-- [VM [] 0 [MChc 1 [VM [] 1 [MStr "a"]] [],MStr "b"]]
--
-- |parallel matches
-- >>> match (VPM.seq [c,a]) [ Str "x",  Chc 1 ( [Str "xba"] ) ([Str  "c"] ), Str "a"]
-- [VM [] 1 [MChc 1 [] [VM [] 0 [MStr "c"]],MStr "a"]]
-- >>> match ab [Chc 1 [Str "xa"] [Str "ya"], Str "b"]
-- [VM [] 0 [MChc 1 [VM [] 1 [MStr "a"]] [VM [] 1 [MStr "a"]],MStr "b"]]
-- >>> match ab [Chc 1 [Str "xa"] [Str "yab"], Str "b"]
-- [VM [] 0 [MChc 1 [] [VM [] 1 [MStr "ab"]]],VM [] 0 [MChc 1 [VM [] 1 [MStr "a"]] [],MStr "b"]]
-- >>> match ab [Chc 1 [Str "a"] [Str "ab"], Str "b"]
-- [VM [] 0 [MChc 1 [] [VM [] 0 [MStr "ab"]]],VM [] 0 [MChc 1 [VM [] 0 [MStr "a"]] [],MStr "b"]]
-- >>> match abc [Chc 1 [Str "abcyab"] [Str "lmnab"], Str "c"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "abc"]] []],VM [] 0 [MChc 1 [] [VM [] 3 [MStr "ab"]],MStr "c"]]
-- >>> match abc [Chc 1 [Str "abcya"] [Str "lmnab"], Str "c"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "abc"]] []],VM [] 0 [MChc 1 [] [VM [] 3 [MStr "ab"]],MStr "c"]]
-- >>> match ((ch 'b') `Seq` _a) [Chc 1 [Str "abcab"] [Str "lmnaba"], Str "aac"]
-- [VM [] 0 [MChc 1 [VM [] 1 [MStr "bca"]] []],VM [] 0 [MChc 1 [] [VM [] 4 [MStr "ba"]],MStr "a"],VM [] 0 [MChc 1 [VM [] 4 [MStr "b"]] [],MStr "aa"]]
-- >>> match ((ch 'a') `Seq` _a) [Chc 1 [Str "acaa"] [Str "lmnaba"], Str "aac"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "aca"]] [VM [] 3 [MStr "aba"]]],VM [] 0 [MChc 1 [VM [] 3 [MStr "a"]] [],MStr "aa"]]
-- >>> match ((ch 'a') `Seq` _a) [Chc 1 [Str "acaa"] [Str "lmnaba"], Str "aaca"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "aca"]] [VM [] 3 [MStr "aba"]]],VM [] 0 [MChc 1 [VM [] 3 [MStr "a"]] [],MStr "aa"]]
-- >>> match ((ch 'a') `Seq` _a) [Chc 1 [Str "acaa"] [Str "lmnaba"], Str "aaa"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "aca"]] [VM [] 3 [MStr "aba"]]],VM [] 0 [MChc 1 [VM [] 3 [MStr "a"]] [],MStr "aa"]]
-- >>> match ((ch 'a') `Seq` _a) [Chc 1 [Str "acaab"] [Str "lmnabaa"], Str "aaa"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "aca"]] [VM [] 3 [MStr "aba"]]],VM [] 0 [MChc 1 [] [VM [] 6 [MStr "a"]],MStr "aa"],VM [] 0 [MChc 1 [VM [] 3 [MStr "ab"]] [],MStr "a"]]
-- >>> match ((ch 'a') `Seq` _a) [Chc 1 [Str "ab"] [Str "a"], Str "aaaa"]
-- [VM [] 0 [MChc 1 [] [VM [] 0 [MStr "a"]],MStr "aa"],VM [] 0 [MChc 1 [VM [] 0 [MStr "ab"]] [],MStr "a"]]
-- >>> match ((ch 'a') `Seq` _a) [Chc 1 [Str "acaab"] [Str "lmnabaa"], Str "aaaa"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "aca"]] [VM [] 3 [MStr "aba"]]],VM [] 0 [MChc 1 [] [VM [] 6 [MStr "a"]],MStr "aa"],VM [] 0 [MChc 1 [VM [] 3 [MStr "ab"]] [],MStr "a"]]
-- >>> match ((ch 'a') `Seq` _a) [Chc 1 [Str "acaab"] [Str "lmabaa"], Str "aaaa"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "aca"]] []],VM [] 0 [MChc 1 [] [VM [] 5 [MStr "a"]],MStr "aa"],VM [] 0 [MChc 1 [VM [] 3 [MStr "ab"]] [],MStr "a"]]
--
-- |Fixed: overlap between the matched value of StrSplit and PatSplit
-- >>> match ab [Chc 1 [Str "xab"] [Str "a"], Str "b"]
-- [VM [] 0 [MChc 1 [] [VM [] 0 [MStr "a"]],MStr "b"]]
--
-- |Fixed: start 2 threads for matching when PM occurs
-- >>> match abc [Chc 1 [Str "abcya"] [Str "lmnab"], Str "c"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "abc"]] []],VM [] 0 [MChc 1 [] [VM [] 3 [MStr "ab"]],MStr "c"]]
--
-- |Question: Should "ca" from the right alternative be matched?
-- |Answer: No. It shoudn't be matched because ca and cca are overlapping matches and
--          ca in right starts before ca in left ends
-- >>> match (VPM.seq [c,a]) [ Str "c", Chc 1 ( [Str "aba"] ) ([Str  "c"] ), Str "a"]
-- [VM [] 0 [MStr "c",MChc 1 [VM [] 0 [MStr "a"]] []]]

-- |Following needs to be resolved
-- |Error: Position withing the choice is lost and therefore
-- >>> match (seq [a,a,b]) [Str "a", Chc 1 [Str "bcaa"] [Str "c"], Str "abx"]
-- [VM [] 1 [MChc 1 [VM [] 0 [MStr "a"]] [],MStr "ab"]]


-- | Choice patterns
-- | Exact Dimension
-- >>> match cp [Str "abcd"]
-- []
-- >>> match cp [Chc 2 [Str "a"] [Str "b"]]
-- []
-- >>> match cp [Chc 1 [Str "ax"] [Str "by"]]
-- []
-- >>> match cp [Chc 1 [Str "a"] [Str "b"]]
-- [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match cp [Str "xy", Chc 2 [Str "a"] [Str "b"]]
-- []
-- >>> match cp [Str "xy", Chc 1 [Str "a"] [Str "b"]]
-- [VM [1] 2 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match cp [Chc 1 [Str "a"] [Str "b"], Str "xy"]
-- [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match cp [Chc 2 [Chc 1 [Str "a"] [Str "b"]] [Str "x"]]
-- [VM [] 0 [MChc 2 [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]] []]]
-- >>> match cp [Chc 1 [Str "a"] [Str "b"], Str "xy", Chc 1 [Str "a"] [Str "b"]]
-- [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]],VM [1] 3 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match cp1 [Chc 1 [Str "ab"] [Str "ac"]]
-- [VM [1] 0 [MChc 1 [VM [] 0 [MStr "ab"]] [VM [] 0 [MStr "ac"]]]]
-- >>> match cp1 [Str "a", Chc 1 [Str "b"] [Str "c"]]
-- [VM [1] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "b"]] [VM [] 0 [MStr "c"]]]]
-- >>> match cp1 [Str "ab", Chc 1 [Str "b"] [Str "c"]]
-- []
-- >>> match cp1 [Str "a", Chc 1 [Str "bx"] [Str "c"]]
-- []
-- >>> match cp1 [Str "a", Chc 1 [Str "b"] [Str "cy"]]
-- []
-- >>> match cp1 [Str "a", Chc 1 [Str "bx"] [Str "cy"]]
-- []
-- >>> match cp1 [Str "a", Chc 1 [Str "b"] [Str "c"], Chc 1 [Str "ab"] [Str "ac"]]
-- [VM [1] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "b"]] [VM [] 0 [MStr "c"]]],VM [1] 2 [MChc 1 [VM [] 0 [MStr "ab"]] [VM [] 0 [MStr "ac"]]]]

-- |Dimension Variable
-- >>> match d [Chc 2 [Str "a"] [Str "b"]]
-- [VM [2] 0 [MChc 2 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match d [Chc 1 [Str "a"] [Str "b"]]
-- [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match d [Chc 1 [Str "ax"] [Str "by"]]
-- []
-- >>> match d [Chc 1 [Str "a"] [Str "b"]]
-- [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match d [Str "xy", Chc 2 [Str "a"] [Str "b"]]
-- [VM [2] 2 [MChc 2 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match d [Str "xy", Chc 1 [Str "a"] [Str "b"]]
-- [VM [1] 2 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match d [Chc 1 [Str "a"] [Str "b"], Str "xy"]
-- [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]] 
-- >>> match d [Chc 1 [Str "a"] [Str "b"], Str "xy", Chc 1 [Str "a"] [Str "b"]]
-- [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]],VM [1] 3 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]
-- >>> match d1 [Chc 1 [Str "ab"] [Str "ac"]]
-- [VM [1] 0 [MChc 1 [VM [] 0 [MStr "ab"]] [VM [] 0 [MStr "ac"]]]]
-- >>> match d1 [Str "a", Chc 1 [Str "b"] [Str "c"]]
-- [VM [1] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "b"]] [VM [] 0 [MStr "c"]]]]
-- >>> match d1 [Str "ab", Chc 1 [Str "b"] [Str "c"]]
-- []
-- >>> match d1 [Str "a", Chc 1 [Str "bx"] [Str "c"]]
-- []
-- >>> match d1 [Str "a", Chc 1 [Str "b"] [Str "cy"]]
-- []
-- >>> match d1 [Str "a", Chc 1 [Str "bx"] [Str "cy"]]
-- []
-- >>> match d1 [Str "a", Chc 1 [Str "b"] [Str "c"], Chc 1 [Str "ab"] [Str "ac"]]
-- [VM [1] 0 [MStr "a",MChc 1 [VM [] 0 [MStr "b"]] [VM [] 0 [MStr "c"]]],VM [1] 2 [MChc 1 [VM [] 0 [MStr "ab"]] [VM [] 0 [MStr "ac"]]]]
-- >>> match d [Chc 2 [Chc 1 [Str "a"] [Str "b"]] [Str "b"]]
-- [VM [2] 0 [MChc 2 [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]] [VM [] 0 [MStr "b"]]]]
-- >>> match d [Chc 2 [Chc 1 [Str "a"] [Str "b"]] [Str "x"]]
-- [VM [] 0 [MChc 2 [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]] []]]
-- >>> match d [Chc 2 [Str "a"] [Chc 1 [Str "a"] [Str "b"]]]
-- [VM [2] 0 [MChc 2 [VM [] 0 [MStr "a"]] [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]]]
-- >>> match d [Chc 2 [Str "x"] [Chc 1 [Str "a"] [Str "b"]]]
-- [VM [] 0 [MChc 2 [] [VM [1] 0 [MChc 1 [VM [] 0 [MStr "a"]] [VM [] 0 [MStr "b"]]]]]]
-- 


