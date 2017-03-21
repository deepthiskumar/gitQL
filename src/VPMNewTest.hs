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
cp3 = PChc (D 1) (seq $ map ch "ba") (seq $ map ch "ca")
d0 = PChc (DVar "d") (ch 'a') (ch 'b')
d1 = PChc (DVar "d") ab (seq $ map ch "ac")
d2 = PChc (DVar "d") (seq $ map ch "ba") (seq $ map ch "ca")
np = PChc (DVar "d") a (PChc (DVar "d1") (ch 'b') (ch 'b') )

--Query variables
qv = PChc (D 1) (ch 'a') (QVar "x")
qv1 = PChc (DVar "d") (ch 'a') (QVar "x")
rqv = PChc (DVar "d") (QVar "x") (ch 'b')

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
-- >>>vgrep (seq [a,b,c,d]) [Str "a",Chc 1 [Str "b"][Str"x"],Chc 2 [Str "c"] [Str "y"],Chc 3 [Str "z"] [Str "d"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] [],Chc 2 [Str "c"] [],Chc 3 [] [Str "d"]]),[])]
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

-- |PM
-- >>> vgrep ab [Chc 1 [Str "xa"] [Str "ya"], Str "b"]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"],Str "b"]),[])]
-- >>> vgrep abc [Chc 1 [Str "abcyab"] [Str "lmnab"], Str "c"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "abc"] []]),[]),(((P 0 (Right (P 0 (Left 4),NoPos)),[]),[Chc 1 [Str "ab"] [],Str "c"]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "ab"],Str "c"]),[])]
-- >>> vgrep abc [Chc 1 [Str "abcya"] [Str "lmnab"], Str "c"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "abc"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "ab"],Str "c"]),[])]
-- >>> vgrep ((ch 'b') `Seq` _a) [Chc 1 [Str "abcab"] [Str "lmnaba"], Str "aac"]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "bca"] []]),[]),(((P 0 (Right (P 0 (Left 4),NoPos)),[]),[Chc 1 [Str "b"] [],Str "aa"]),[]),(((P 0 (Right (NoPos,P 0 (Left 4))),[]),[Chc 1 [] [Str "ba"],Str "a"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaab"] [Str "lmnabaa"], Str "aaa"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "ab"] [],Str "a"]),[]),(((P 0 (Right (NoPos,P 0 (Left 6))),[]),[Chc 1 [] [Str "a"],Str "aa"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "ab"] [Str "a"], Str "aaaa"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] [],Str "a"]),[]),(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "a"],Str "aa"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaab"] [Str "lmnabaa"], Str "aaaa"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "ab"] [],Str "a"]),[]),(((P 0 (Right (NoPos,P 0 (Left 6))),[]),[Chc 1 [] [Str "a"],Str "aa"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaab"] [Str "lmabaa"], Str "aaaa"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 2))),[]),[Chc 1 [] [Str "aba"]]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "ab"] [],Str "a"]),[]),(((P 0 (Right (NoPos,P 0 (Left 5))),[]),[Chc 1 [] [Str "a"],Str "aa"]),[])]
--
-- |Fixed: start 2 threads for matching when PM occurs
-- >>> vgrep abc [Chc 1 [Str "abcya"] [Str "lmnab"], Str "c"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "abc"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "ab"],Str "c"]),[])]



-- |
-- >>> vgrep ab [Chc 1 [Str "xa"] [Str "yab"], Str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "ab"]]),[]),(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [Str "a"] [Str "ab"], Str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "ab"]]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaa"] [Str "lmnaba"], Str "aac"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "aa"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaa"] [Str "lmnaba"], Str "aaca"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "aa"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [Str "acaa"] [Str "lmnaba"], Str "aaa"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "aa"]),[])]
--
-- |Fixed: overlap between the matched value of StrSplit and PatSplit
-- >>> vgrep ab [Chc 1 [Str "xab"] [Str "a"], Str "b"]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "a"],Str "b"]),[])]
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

-- | Choice patterns
-- | Exact Dimension
-- >>> vgrep cp [Str "abcd"]
-- []
-- >>> vgrep cp [Chc 2 [Str "a"] [Str "b"]]
-- []
-- >>> vgrep cp [Chc 1 [Str "ax"] [Str "by"]]
-- []
-- >>> vgrep cp [Chc 1 [Str "a"] [Str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp [Str "xy", Chc 2 [Str "a"] [Str "b"]]
-- []
-- >>> vgrep cp [Str "xy", Chc 1 [Str "a"] [Str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp [Chc 1 [Str "a"] [Str "b"], Str "xy"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp [Chc 2 [Chc 1 [Str "a"] [Str "b"]] [Str "x"]]
-- [(((P 0 (Right (P 0 (Right (P 0 (Left 0),P 0 (Left 0))),NoPos)),[]),[Chc 2 [Chc 1 [Str "a"] [Str "b"]] []]),[])]
-- >>> vgrep cp [Chc 1 [Str "a"] [Str "b"], Str "xy", Chc 1 [Str "a"] [Str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp1 [Chc 1 [Str "ab"] [Str "ac"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]
-- >>> vgrep cp1 [Str "a", Chc 1 [Str "b"] [Str "c"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[])]
-- >>> vgrep cp3 [Chc 1 [Str "b"] [Str "c"],Str "a"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "b"] [Str "c"],Str "a"]),[])]
-- >>> vgrep cp1 [Str "ab", Chc 1 [Str "b"] [Str "c"]]
-- []
-- >>> vgrep cp1 [Str "a", Chc 1 [Str "bx"] [Str "c"]]
-- []
-- >>> vgrep cp1 [Str "a", Chc 1 [Str "b"] [Str "cy"]]
-- []
-- >>> vgrep cp1 [Str "a", Chc 1 [Str "bx"] [Str "cy"]]
-- []
-- >>> vgrep cp1 [Str "a", Chc 1 [Str "b"] [Str "c"], Chc 1 [Str "ab"] [Str "ac"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]

-- |Dimension Variable
-- >>> vgrep d0 [Chc 2 [Str "a"] [Str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Str "xy", Chc 2 [Str "a"] [Str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Chc 1 [Str "a"] [Str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Chc 1 [Str "ax"] [Str "by"]]
-- []
-- >>> vgrep d0 [Str "xy", Chc 2 [Str "a"] [Str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Str "xy", Chc 1 [Str "a"] [Str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Chc 1 [Str "a"] [Str "b"], Str "xy"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])] 
-- >>> vgrep d0 [Chc 1 [Str "a"] [Str "b"], Str "xy", Chc 1 [Str "a"] [Str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Chc 1 [Str "a"] [Str "b"], Str "xy", Chc 2 [Str "a"] [Str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d1 [Chc 1 [Str "ab"] [Str "ac"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]
-- >>> vgrep d1 [Str "a", Chc 1 [Str "b"] [Str "c"]]
-- [(((P 0 (Left 0),[("d",1)]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[])]
-- >>> vgrep d1 [Str "ab", Chc 1 [Str "b"] [Str "c"]]
-- []
-- >>> vgrep d1 [Str "a", Chc 1 [Str "bx"] [Str "c"]]
-- []
-- >>> vgrep d1 [Str "a", Chc 1 [Str "b"] [Str "cy"]]
-- []
-- >>> vgrep d1 [Str "a", Chc 1 [Str "bx"] [Str "cy"]]
-- []
-- >>> vgrep d1 [Str "a", Chc 1 [Str "b"] [Str "c"], Chc 1 [Str "ab"] [Str "ac"]]
-- [(((P 0 (Left 0),[("d",1)]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]
-- >>> vgrep d [Chc 2 [Chc 1 [Str "a"] [Str "b"]] [Str "b"]]
-- []
-- >>> vgrep d [Chc 2 [Chc 1 [Str "a"] [Str "b"]] [Str "x"]]
-- []
-- >>> vgrep d [Chc 2 [Str "a"] [Chc 1 [Str "a"] [Str "b"]]]
-- []
-- >>> vgrep d [Chc 2 [Str "x"] [Chc 1 [Str "a"] [Str "b"]]]
-- []


-- | nested matches in both the alternatives TODO CHECK IF THE BEHAVIOR IS CORRECT 
-- >>> vgrep d0 [Chc 2 [Chc 1 [Str "a"] [Str "b"]] [Str "b"]]
-- [(((P 0 (Right (P 0 (Right (P 0 (Left 0),P 0 (Left 0))),NoPos)),[("d",1)]),[Chc 2 [Chc 1 [Str "a"] [Str "b"]] []]),[])]
-- >>> vgrep d0 [Chc 2 [Chc 1 [Str "a"] [Str "b"]] [Str "x"]]
-- [(((P 0 (Right (P 0 (Right (P 0 (Left 0),P 0 (Left 0))),NoPos)),[("d",1)]),[Chc 2 [Chc 1 [Str "a"] [Str "b"]] []]),[])]
-- >>> vgrep d0 [Chc 2 [Str "a"] [Chc 1 [Str "a"] [Str "b"]]]
-- [(((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1)]),[Chc 2 [] [Chc 1 [Str "a"] [Str "b"]]]),[])]
-- >>> vgrep d0 [Chc 2 [Str "x"] [Chc 1 [Str "a"] [Str "b"]]]
-- [(((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1)]),[Chc 2 [] [Chc 1 [Str "a"] [Str "b"]]]),[])]
-- >>>vgrep d2 [Chc 1 [Str "b"] [Str "c"],Str "a"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "b"] [Str "c"],Str "a"]),[])]

-- 
-- |nested Matches
-- >>> vgrep (PChc (DVar "d") a (PChc (DVar "d1") (ch 'b') (ch 'c') )) ([Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1),("d1",2)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]]),[])]

-- | query variables
-- >>> vgrep qv [Chc 1 [Str "a"] [Str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "b"]]))])]
-- >>> vgrep qv1 [Chc 1 [Str "a"] [Str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "b"]]))])]
-- >>> vgrep qv1 ([Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Chc 2 [Str "b"] [Str "c"]]]))])]
-- >>> vgrep rqv ([Str "efg", Chc 1 [Str "a"] [Chc 2 [Str "c"] [Str "b"]]])
-- [(((P 1 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",2)]),[Chc 1 [] [Chc 2 [Str "c"] [Str "b"]]]),[("x",((P 1 (Right (NoPos,P 0 (Right (P 0 (Left 0),NoPos)))),[]),[Chc 1 [] [Chc 2 [Str "c"] []]]))])]

-- | Query variables on split choices and empty strings
-- >>> vgrep (PChc (DVar "d") (ab) (QVar "x")) [Chc 1 [Str "a"] [Str "lm"], Chc 1 [Str "b"] [Str "op"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "lm"],Chc 1 [Str "b"] [Str "op"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "lm"],Chc 1 [] [Str "op"]]))])]
-- >>> vgrep (PChc (DVar "d") (ab) (QVar "x")) [Chc 2 [Str ""] [Str "xy"],Chc 1 [Str "a"] [Str "lm"], Chc 1 [Str "b"] [Str "op"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 2 [Str ""] [Str "xy"],Chc 1 [Str "a"] [Str "lm"],Chc 1 [Str "b"] [Str "op"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 2 [] [Str "xy"],Chc 1 [] [Str "lm"],Chc 1 [] [Str "op"]]))])]




