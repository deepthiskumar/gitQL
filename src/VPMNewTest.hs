module VPMNewTest where

import Prelude hiding (seq)
import VPMNew

-- patterns
--
ch :: Char -> Pattern
ch = Plain . C

seq :: [Pattern] -> Pattern
seq [p]    = p
seq (a:ps) = Seq a (seq ps)

wild :: Pattern
wild = Plain Wild

star :: Pattern -> Pattern
star x = Repeat x 0 Nothing

plus :: Pattern -> Pattern
plus x = Repeat x 1 Nothing

possibly :: Pattern -> Pattern
possibly x = Repeat x 0 (Just 1)

[a,b,c,d] = map ch "abcd"

ab = seq [a,b]
abc = seq [a,b,c]
_a = Seq (Plain Wild) a

a'b = Alt a b


-- Strings
--
v = [Str "xabyabcz"]
vc = [Chc 1 [Str "ab"] [Str "cd"],Chc 2 [Str "ef"] [Str "gh"]]
vc1 = [Chc 1 [Str "mabn"] [Str "xca"], Str "c"]
multipleAs =  [ Str "x",  Chc 1 ( [Str "aca"] ) ([Str "c"] )]
share =  [ Str "c",  Chc 1 ( [Str "aba"] ) ([Str  "c"] ), Str "a"]

--Patterns
cp = PChc (D 1) (ch 'a') (ch 'b')
cp1 = PChc (D 1) ab (seq $ map ch "ac")
cp2 = PChc (D 1) a a
d0 = PChc (DVar "d") (ch 'a') (ch 'b')
d1 = PChc (DVar "d") ab (seq $ map ch "ac")
np = PChc (DVar "d") a (PChc (DVar "d1") (ch 'b') (ch 'b') )

-- |doctests
-- | String without any variations
-- >>>vgrep ab v
-- [(((P 0 (Left 1),[]),[Str "ab"]),[]),(((P 0 (Left 4),[]),[Str "ab"]),[])]
-- >>> vgrep abc v
-- [(((P 0 (Left 4),[]),[Str "abc"]),[])]
--
-- | below one needs patsplit implementation in continue
-- >>> vgrep _a v
-- [(((P 0 (Left 0),[]),[Str "xa"]),[]),(((P 0 (Left 3),[]),[Str "ya"]),[])]
-- >>> vgrep abc [Str "aab", Str "cxabc"]
-- [(((P 0 (Left 1),[]),[Str "abc"]),[]),(((P 1 (Left 2),[]),[Str "abc"]),[])]
-- >>> vgrep abc [Str "abcab", Str "cxab"]
-- [(((P 0 (Left 0),[]),[Str "abc"]),[]),(((P 0 (Left 3),[]),[Str "abc"]),[])]
-- >>> vgrep ab [Str "abca", Str "bxab"]
-- [(((P 0 (Left 0),[]),[Str "ab"]),[]),(((P 0 (Left 3),[]),[Str "ab"]),[]),(((P 1 (Left 2),[]),[Str "ab"]),[])]
-- >>> vgrep a'b v
-- [(((P 0 (Left 1),[]),[Str "a"]),[]),(((P 0 (Left 2),[]),[Str "b"]),[]),(((P 0 (Left 4),[]),[Str "a"]),[]),(((P 0 (Left 5),[]),[Str "b"]),[])]
-- >>> vgrep a v
-- [(((P 0 (Left 1),[]),[Str "a"]),[]),(((P 0 (Left 4),[]),[Str "a"]),[])]
-- >>> vgrep (seq [a,a]) [Str "abab", Str "cxaaaabc"]
-- [(((P 1 (Left 2),[]),[Str "aa"]),[]),(((P 1 (Left 4),[]),[Str "aa"]),[])]

-- |With Choices
-- >>>vgrep ab ([Chc 1 [Str "ab"] [Str "cd"]])
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
-- >>>vgrep b ([Chc 1 [Str "ab"] [Str "cd"]])
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "b"] []]),[])]
-- >>>vgrep d ([Chc 1 [Str "ab"] [Str "cd"]])
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "d"]]),[])]
-- >>>vgrep c vc1
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "c"]]),[]),(((P 1 (Left 0),[]),[Str "c"]),[])]
-- >>>vgrep a multipleAs
-- [(((P 1 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 1 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "a"] []]),[])]
--
-- | Case 2 - only choices
-- >>> vgrep a vc
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[])]
-- >>> vgrep (ch 'b') vc
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "b"] []]),[])]
-- >>> vgrep (ch 'c') vc
-- [(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "c"]]),[])]
-- >>> vgrep (ch 'd') vc
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "d"]]),[])]
-- >>> vgrep (ch 'e') vc
-- [(((P 1 (Right (P 0 (Left 0),NoPos)),[]),[Chc 2 [Str "e"] []]),[])]
-- >>> vgrep (ch 'x') vc
-- []
-- >>> vgrep a multipleAs
-- [(((P 1 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 1 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "a"] []]),[])]
-- >>> vgrep a [Chc 1 [Str "lm"] [Str "aaxa"]]
-- [(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "a"]]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"]]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "a"]]),[])]
-- >>> vgrep ab [Chc 1 [Str "ab"] [Str "cd"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
-- >>> vgrep ab [Chc 1 [Str "xab"] [Str "cd"]]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
-- >>> vgrep ab [Chc 1 [Str "abab"] [Str "cd"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
-- >>> vgrep ab [Chc 1 [Str "abcab"] [Str "cd"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
--
-- | Parallel matches
-- >>> vgrep a [Chc 1 [Str "ab"] [Str "ca"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"]]),[])]
-- >>> vgrep a [Chc 1 [Str "aba"] [Str "ca"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"]]),[])]
-- >>> vgrep a [Chc 1 [Str "aaa"] [Str "ca"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"]]),[])]
-- >>> vgrep ab [Chc 1 [Str "abcdyab"] [Str "xabab"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (P 0 (Left 5),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "ab"]]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "ab"]]),[])]

-- |Case 3L
-- >>> vgrep ab [Str "a", Chc 1 [Str "b"] [Str "c"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[])]
-- >>> vgrep ab [Str "a", Chc 1 [Str "cb"] [Str "bc"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [] [Str "b"]]),[])]
-- >>> vgrep ab [Str "a", Chc 1 [Str "cb"] [Str "c"]]
-- []
-- >>> vgrep abc [Str "ab", Chc 1 [Str "c"] [Str "d"]]
-- [(((P 0 (Left 0),[]),[Str "ab",Chc 1 [Str "c"] []]),[])]
-- >>> vgrep abc [Str "a", Chc 1 [Str "bcd"] [Str "ef"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "bc"] []]),[])]
-- >>> vgrep ab [Str "a", Chc 1 [Str "bcab"] [Str "c"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[]),(((P 1 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
--
-- |parallel matches
-- >>> vgrep (seq [c,a]) share
-- [(((P 0 (Left 0),[]),[Str "c",Chc 1 [Str "a"] []]),[]),(((P 1 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "c"],Str "a"]),[])]
-- >>> vgrep ab [Str "a", Chc 1 [Str "bd"] [Str "be"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[]),(((P 0 (Left 0),[]),[Str "a",Chc 1 [] [Str "b"]]),[])]

-- |Case 3R
-- >>> vgrep ab [Chc 1 [Str "a"] [Str "c"],Str "b"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [Str "x"] [Str "a"],Str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "a"],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [Str "xa"] [Str "c"],Str "b"]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [Str "x"] [Str "ya"],Str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"],Str "b"]),[])]
-- >>> vgrep abc [Chc 1 [Str "ab"] [Str "x"],Str "abc"]
-- [(((P 1 (Left 0),[]),[Str "abc"]),[])]
-- >>> vgrep ab [Str "a", Chc 1 [Str "bca"] [Str "c"], Str "bcx"]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[]),(((P 1 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ab [Str "a", Chc 1 [Str "bca"] [Str "c"], Str "x"]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[])]
--
-- | these are not parallel matches
-- >>> vgrep ab [Chc 1 [Str "ax"] [Str "ya"],Str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [Str "xa"] [Str "ay"],Str "b"]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
--
-- |parallel matches
-- >>> vgrep (seq [c,a]) [ Str "x",  Chc 1 ( [Str "xba"] ) ([Str  "c"] ), Str "a"]
-- [(((P 1 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "c"],Str "a"]),[])]

-- TODO PM
-- >>> vgrep ab [Chc 1 [Str "xa"] [Str "ya"], Str "b"]
-- [VM [] 0 [MChc 1 [VM [] 1 [MStr "a"]] [VM [] 1 [MStr "a"]],MStr "b"]]
-- >>> vgrep abc [Chc 1 [Str "abcyab"] [Str "lmnab"], Str "c"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "abc"]] []],VM [] 0 [MChc 1 [] [VM [] 3 [MStr "ab"]],MStr "c"]]
-- >>> vgrep abc [Chc 1 [Str "abcya"] [Str "lmnab"], Str "c"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "abc"]] []],VM [] 0 [MChc 1 [] [VM [] 3 [MStr "ab"]],MStr "c"]]
-- >>> vgrep ((ch 'b') `Seq` _a) [Chc 1 [Str "abcab"] [Str "lmnaba"], Str "aac"]
-- [VM [] 0 [MChc 1 [VM [] 1 [MStr "bca"]] []],VM [] 0 [MChc 1 [] [VM [] 4 [MStr "ba"]],MStr "a"],VM [] 0 [MChc 1 [VM [] 4 [MStr "b"]] [],MStr "aa"]]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaab"] [Str "lmnabaa"], Str "aaa"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "aca"]] [VM [] 3 [MStr "aba"]]],VM [] 0 [MChc 1 [] [VM [] 6 [MStr "a"]],MStr "aa"],VM [] 0 [MChc 1 [VM [] 3 [MStr "ab"]] [],MStr "a"]]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "ab"] [Str "a"], Str "aaaa"]
-- [VM [] 0 [MChc 1 [] [VM [] 0 [MStr "a"]],MStr "aa"],VM [] 0 [MChc 1 [VM [] 0 [MStr "ab"]] [],MStr "a"]]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaab"] [Str "lmnabaa"], Str "aaaa"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "aca"]] [VM [] 3 [MStr "aba"]]],VM [] 0 [MChc 1 [] [VM [] 6 [MStr "a"]],MStr "aa"],VM [] 0 [MChc 1 [VM [] 3 [MStr "ab"]] [],MStr "a"]]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaab"] [Str "lmabaa"], Str "aaaa"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "aca"]] []],VM [] 0 [MChc 1 [] [VM [] 5 [MStr "a"]],MStr "aa"],VM [] 0 [MChc 1 [VM [] 3 [MStr "ab"]] [],MStr "a"]]
-- Fixed: start 2 threads for matching when PM occurs
-- >>> vgrep abc [Chc 1 [Str "abcya"] [Str "lmnab"], Str "c"]
-- [VM [] 0 [MChc 1 [VM [] 0 [MStr "abc"]] []],VM [] 0 [MChc 1 [] [VM [] 3 [MStr "ab"]],MStr "c"]]



-- |
-- >>> vgrep ab [Chc 1 [Str "xa"] [Str "yab"], Str "b"]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "ab"]]),[])]
-- >>> vgrep ab [Chc 1 [Str "a"] [Str "ab"], Str "b"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[]),(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "ab"]]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaa"] [Str "lmnaba"], Str "aac"]
-- [(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "aa"]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaa"] [Str "lmnaba"], Str "aaca"]
-- [(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "aa"]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaa"] [Str "lmnaba"], Str "aaa"]
-- [(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "aa"]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[])]
--
-- |Fixed: overlap between the matched value of StrSplit and PatSplit
-- >>> vgrep ab [Chc 1 [Str "xab"] [Str "a"], Str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "a"],Str "b"]),[]),(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
--
--
-- |Question: Should "ca" from the right alternative be matched?
-- |Answer: No. It shoudn't be matched because ca and cca are overlapping matches and
--          ca in right starts before ca in left ends
-- >>> vgrep (seq [c,a]) [ Str "c", Chc 1 ( [Str "aba"] ) ([Str  "c"] ), Str "a"]
-- [(((P 0 (Left 0),[]),[Str "c",Chc 1 [Str "a"] []]),[]),(((P 1 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "c"],Str "a"]),[])]

-- |Rewind scenario
-- >>> vgrep (seq [a,a,b]) [Str "a", Chc 1 [Str "bcaa"] [Str "c"], Str "abx"]
-- [(((P 1 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "ab"]),[])]


