module VPMNewTest where

import Prelude hiding (seq)
import VPMEngine -- }VPMNew
import Types
import Data.Text as T (Text, pack)

-- patterns
--

str s = Str (T.pack s)

p :: String -> Text
p = T.pack

ch :: Char -> Pattern
ch = Plain . C

seq :: [Pattern] -> Pattern
seq [p]    = p
seq (a:ps) = Seq a (seq ps)

seqM :: String -> Pattern
seqM = seq.(map ch)

wild :: Pattern
wild = Plain Wild

star :: Pattern -> Pattern
star x = Repeat x 0 Nothing

plus :: Pattern -> Pattern
plus x = Repeat x 1 Nothing

possibly :: Pattern -> Pattern
possibly x = Repeat x 0 (Just 1)

[a,b,c,d,z] = map ch "abcdz"

ab = seq [a,b]
czN = seq [c,z,None]
abN = seq [a,b,None]
abc = seq [a,b,c]
_a = Seq (Plain Wild) a

a'b = Alt a b


-- Strings
--
v = [Str $ p "xabyabcz"]
vc = [Chc 1 [Str $ p "ab"] [Str $ p "cd"],Chc 2 [Str $ p "ef"] [Str $ p "gh"]]
vc1 = [Chc 1 [Str $ p "mabn"] [Str $ p "xca"], Str $ p "c"]
multipleAs =  [ Str $ p "x",  Chc 1 ( [Str $ p "aca"] ) ([Str $ p "c"] )]
share =  [ Str $ p "c",  Chc 1 ( [Str $ p "aba"] ) ([Str  $ p "c"] ), Str $ p "a"]

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

--Patterns Star
cp' = PChc (D 1) (seq [Any,(ch 'a'),Any]) (seq [Any,(ch 'b'),Any])
cp1' = PChc (D 1) (seq [Any,a,b,Any]) (seq $ [Any] ++ (map ch "ac") ++ [Any])
cp2' = PChc (D 1) (seq [Any,a,Any]) (seq [Any,a,Any])
cp3' = PChc (D 1) (seq [Any,b,a,Any]) (seq $ [Any] ++ (map ch "ca") ++ [Any])
d0' = PChc (DVar "d") (seq [Any,(ch 'a'),Any]) (seq [Any,(ch 'b'),Any])
d1' = PChc (DVar "d") (seq [Any,a,b,Any]) (seq $ [Any] ++ (map ch "ac") ++ [Any])
d2' = PChc (DVar "d") (seq [Any,b,a,Any]) (seq $ [Any] ++ (map ch "ca") ++ [Any])
np' = PChc (DVar "d") (seq [Any,a,Any]) (PChc (DVar "d1") (seq [Any,b,Any]) (seq [Any,b,Any]) )

--Query variables Star
qv' = PChc (D 1) (seq[Any,a,Any]) (QVar "x")
qv1' = PChc (DVar "d") (seq[Any,a,Any]) (QVar "x")
rqv' = PChc (DVar "d") (QVar "x") (seq[Any,b,Any])

-- |doctests
-- | String without any variations
-- >>>vgrep ab v
-- [(((P 0 (Left 1),[]),[Str "ab"]),[]),(((P 0 (Left 4),[]),[Str "ab"]),[])]
-- >>> vgrep abc v
-- [(((P 0 (Left 4),[]),[Str "abc"]),[])]
-- >>> vgrep (QVar "x") v
-- [(((P 0 (Left 0),[]),[Str "xabyabcz"]),[("x",((P 0 (Left 0),[]),[Str "xabyabcz"]))])]
-- >>> vgrep None v
-- []
-- >>> vgrep (Seq Any ab) v
-- [(((P 0 (Left 0),[]),[Str "xab"]),[]),(((P 0 (Left 3),[]),[Str "yab"]),[])]
-- >>> vgrep (Seq ab Any) v
-- [(((P 0 (Left 1),[]),[Str "abyabcz"]),[])]
-- >>> vgrep (Seq ab (Seq Any ab)) v
-- [(((P 0 (Left 1),[]),[Str "abyab"]),[])]

-- | Some of the below ones may require patsplit implementation in continue
-- >>> vgrep _a v
-- [(((P 0 (Left 0),[]),[Str "xa"]),[]),(((P 0 (Left 3),[]),[Str "ya"]),[])]
-- >>> vgrep a'b v
-- [(((P 0 (Left 1),[]),[Str "a"]),[]),(((P 0 (Left 2),[]),[Str "b"]),[]),(((P 0 (Left 4),[]),[Str "a"]),[]),(((P 0 (Left 5),[]),[Str "b"]),[])]
-- >>> vgrep a v
-- [(((P 0 (Left 1),[]),[Str "a"]),[]),(((P 0 (Left 4),[]),[Str "a"]),[])]
-- >>> vgrep abc [str "aab", str "cxabc"]
-- [(((P 0 (Left 1),[]),[Str "abc"]),[]),(((P 1 (Left 2),[]),[Str "abc"]),[])]
-- >>> vgrep abc [str "abcab", str "cxab"]
-- [(((P 0 (Left 0),[]),[Str "abc"]),[]),(((P 0 (Left 3),[]),[Str "abc"]),[])]
-- >>> vgrep ab [str "abca", str "bxab"]
-- [(((P 0 (Left 0),[]),[Str "ab"]),[]),(((P 0 (Left 3),[]),[Str "ab"]),[]),(((P 1 (Left 2),[]),[Str "ab"]),[])]
-- >>> vgrep (seq [a,a]) [str "abab", str "cxaaaabc"]
-- [(((P 1 (Left 2),[]),[Str "aa"]),[]),(((P 1 (Left 4),[]),[Str "aa"]),[])]
-- >>> vgrep abc [str "ab", str "xyabcd"]
-- [(((P 1 (Left 2),[]),[Str "abc"]),[])]
-- >>> vgrep abc [str "a", str "xyabcd"]
-- [(((P 1 (Left 2),[]),[Str "abc"]),[])]
-- >>> vgrep cp v
-- []
-- >>> vgrep abN v
-- []
-- >>> vgrep czN v
-- [(((P 0 (Left 6),[]),[Str "cz"]),[])]

-- |With Choices
-- >>>vgrep ab ([Chc 1 [str "ab"] [str "cd"]])
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
-- >>>vgrep b ([Chc 1 [str "ab"] [str "cd"]])
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "b"] []]),[])]
-- >>>vgrep d ([Chc 1 [str "ab"] [str "cd"]])
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "d"]]),[])]
-- >>>vgrep c vc1
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "c"]]),[]),(((P 1 (Left 0),[]),[Str "c"]),[])]
-- >>>vgrep a multipleAs
-- [(((P 1 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 1 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "a"] []]),[])]
-- >>>vgrep (seq [a,b,c,d]) [str "a",Chc 1 [str "b"][str"x"],Chc 2 [str "c"] [str "y"],Chc 3 [str "z"] [str "d"]]
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
-- >>> vgrep a [Chc 1 [str "lm"] [str "aaxa"]]
-- [(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "a"]]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"]]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "a"]]),[])]
-- >>> vgrep ab [Chc 1 [str "ab"] [str "cd"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
-- >>> vgrep ab [Chc 1 [str "xab"] [str "cd"]]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
-- >>> vgrep ab [Chc 1 [str "abab"] [str "cd"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
-- >>> vgrep ab [Chc 1 [str "abcab"] [str "cd"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
--

-- | Parallel matches
-- >>> vgrep a [Chc 1 [str "ab"] [str "ca"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"]]),[])]
-- >>> vgrep a [Chc 1 [str "aba"] [str "ca"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"]]),[])]
-- >>> vgrep a [Chc 1 [str "aaa"] [str "ca"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "a"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"]]),[])]
-- >>> vgrep ab [Chc 1 [str "abcdyab"] [str "xabab"]]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (P 0 (Left 5),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "ab"]]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "ab"]]),[])]

-- |Case 3L
-- >>> vgrep ab [str "a", Chc 1 [str "b"] [str "c"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[])]
-- >>> vgrep ab [str "a", Chc 1 [str "cb"] [str "bc"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [] [Str "b"]]),[])]
-- >>> vgrep ab [str "a", Chc 1 [str "cb"] [str "c"]]
-- []
-- >>> vgrep abc [str "ab", Chc 1 [str "c"] [str "d"]]
-- [(((P 0 (Left 0),[]),[Str "ab",Chc 1 [Str "c"] []]),[])]
-- >>> vgrep abc [str "a", Chc 1 [str "bcd"] [str "ef"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "bc"] []]),[])]
-- >>> vgrep ab [str "a", Chc 1 [str "bcab"] [str "c"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[]),(((P 1 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "ab"] []]),[])]
--
-- |parallel matches (--** 1) THis scenario would not occur )
-- >>> vgrep (seq [c,a]) share 
-- [(((P 0 (Left 0),[]),[Str "c",Chc 1 [Str "a"] []]),[])]
-- >>> vgrep (seq [Any,a]) [Chc 1 [Str $ p "mobn"] [Str $ p "xyz"], Str $ p "a"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "mobn"] [Str "xyz"],Str "a"]),[])]
-- >>> vgrep ab [str "a", Chc 1 [str "bd"] [str "be"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [] [Str "b"]]),[]),(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[])]

-- |Case 3R
-- >>> vgrep ab [Chc 1 [str "a"] [str "c"],str "b"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [str "x"] [str "a"],str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "a"],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [str "xa"] [str "c"],str "b"]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [str "x"] [str "ya"],str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"],Str "b"]),[])]
-- >>> vgrep abc [Chc 1 [str "ab"] [str "x"],str "abc"]
-- [(((P 1 (Left 0),[]),[Str "abc"]),[])]
-- >>> vgrep ab [str "a", Chc 1 [str "bca"] [str "c"], str "bcx"]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[]),(((P 1 (Right (P 0 (Left 2),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ab [str "a", Chc 1 [str "bca"] [str "c"], str "x"]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] []]),[])]
--
-- | these are not parallel matches
-- >>> vgrep ab [Chc 1 [str "ax"] [str "ya"],str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "a"],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [str "xa"] [str "ay"],str "b"]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
--
-- |parallel matches
-- >>> vgrep (seq [c,a]) [ str "x",  Chc 1 ( [str "xba"] ) ([str  "c"] ), str "a"]
-- [(((P 1 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "c"],Str "a"]),[])]

-- |PM
-- >>> vgrep ab [Chc 1 [str "xa"] [str "ya"], str "b"]
-- [(((P 0 (Right (P 0 (Left 1),P 0 (Left 1))),[]),[Chc 1 [Str "a"] [Str "a"],Str "b"]),[])]
-- >>> vgrep abc [Chc 1 [str "abcyab"] [str "lmnab"], str "c"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "abc"] []]),[]),(((P 0 (Right (P 0 (Left 4),P 0 (Left 3))),[]),[Chc 1 [Str "ab"] [Str "ab"],Str "c"]),[])]
-- >>> vgrep abc [Chc 1 [str "abcya"] [str "lmnab"], str "c"]
-- [(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "ab"],Str "c"]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "abc"] []]),[])]
-- >>> vgrep ((ch 'b') `Seq` _a) [Chc 1 [str "abcab"] [str "lmnaba"], str "aac"]
-- [(((P 0 (Right (NoPos,P 0 (Left 4))),[]),[Chc 1 [] [Str "ba"],Str "a"]),[]),(((P 0 (Right (P 0 (Left 4),NoPos)),[]),[Chc 1 [Str "b"] [],Str "aa"]),[]),(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "bca"] []]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [str "acaab"] [str "lmnabaa"], str "aaa"]
-- [(((P 0 (Right (NoPos,P 0 (Left 6))),[]),[Chc 1 [] [Str "a"],Str "aa"]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "ab"] [],Str "a"]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [str "ab"] [str "a"], str "aaaa"]
-- [(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "a"],Str "aa"]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "ab"] [],Str "a"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [str "acaab"] [str "lmnabaa"], str "aaaa"]
-- [(((P 0 (Right (NoPos,P 0 (Left 6))),[]),[Chc 1 [] [Str "a"],Str "aa"]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "ab"] [],Str "a"]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [str "acaab"] [str "lmabaa"], str "aaaa"]
-- [(((P 0 (Right (NoPos,P 0 (Left 5))),[]),[Chc 1 [] [Str "a"],Str "aa"]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "ab"] [],Str "a"]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 2))),[]),[Chc 1 [] [Str "aba"]]),[])]
--
-- |Fixed: start 2 threads for matching when PM occurs
-- >>> vgrep abc [Chc 1 [str "abcya"] [str "lmnab"], str "c"]
-- [(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "ab"],Str "c"]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "abc"] []]),[])]

-- |
-- >>> vgrep ab [Chc 1 [str "xa"] [str "yab"], str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 1))),[]),[Chc 1 [] [Str "ab"]]),[]),(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ab [Chc 1 [str "a"] [str "ab"], str "b"]
-- [(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "ab"]]),[]),(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "a"] [],Str "b"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [str "acaa"] [str "lmnaba"], str "aac"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "aa"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [str "acaa"] [str "lmnaba"], str "aaca"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "aa"]),[])]
-- >>> vgrep ((ch 'a') `Seq` _a) [Chc 1 [str "acaa"] [str "lmnaba"], str "aaa"]
-- [(((P 0 (Right (P 0 (Left 0),NoPos)),[]),[Chc 1 [Str "aca"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 3))),[]),[Chc 1 [] [Str "aba"]]),[]),(((P 0 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "aa"]),[])]
--
-- |Fixed: overlap between the matched value of StrSplit and PatSplit
-- >>> vgrep ab [Chc 1 [str "xab"] [str "a"], str "b"]
-- [(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "ab"] []]),[]),(((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Chc 1 [] [Str "a"],Str "b"]),[])]
--
--
-- |Question: Should "ca" from the right alternative be matched?
-- |Answer: No. It shoudn't be matched because ca and cca are overlapping matches and
--          ca in right starts before ca in left ends
-- >>> vgrep (seq [c,a]) [ str "c", Chc 1 ( [str "aba"] ) ([str  "c"] ), str "a"]
-- [(((P 0 (Left 0),[]),[Str "c",Chc 1 [Str "a"] []]),[])]

-- |Rewind scenario
-- >>> vgrep (seq [a,a,b]) [str "a", Chc 1 [str "bcaa"] [str "c"], str "abx"]
-- [(((P 1 (Right (P 0 (Left 3),NoPos)),[]),[Chc 1 [Str "a"] [],Str "ab"]),[])]

-- | Choice patterns
-- | Exact Dimension
-- >>> vgrep cp [str "abcd"]
-- []
-- >>> vgrep cp [Chc 2 [str "a"] [str "b"]]
-- []
-- >>> vgrep cp [Chc 1 [str "ax"] [str "by"]]
-- []
-- >>> vgrep cp [Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp [str "xy", Chc 2 [str "a"] [str "b"]]
-- []
-- >>> vgrep cp [str "xy", Chc 1 [str "a"] [str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp [Chc 1 [str "a"] [str "b"], str "xy"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp [Chc 2 [Chc 1 [str "a"] [str "b"]] [str "x"]]
-- [(((P 0 (Right (P 0 (Right (P 0 (Left 0),P 0 (Left 0))),NoPos)),[]),[Chc 2 [Chc 1 [Str "a"] [Str "b"]] []]),[])]
-- >>> vgrep cp [Chc 1 [str "a"] [str "b"], str "xy", Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp1 [Chc 1 [str "ab"] [str "ac"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]
-- >>> vgrep cp1 [str "a", Chc 1 [str "b"] [str "c"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[])]
-- >>> vgrep cp3 [Chc 1 [str "b"] [str "c"],str "a"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "b"] [Str "c"],Str "a"]),[])]
-- >>> vgrep cp1 [str "ab", Chc 1 [str "b"] [str "c"]]
-- []
-- >>> vgrep cp1 [str "a", Chc 1 [str "bx"] [str "c"]]
-- []
-- >>> vgrep cp1 [str "a", Chc 1 [str "b"] [str "cy"]]
-- []
-- >>> vgrep cp1 [str "a", Chc 1 [str "bx"] [str "cy"]]
-- []
-- >>> vgrep cp1 [str "a", Chc 1 [str "b"] [str "c"], Chc 1 [str "ab"] [str "ac"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]

-- |Dimension Variable
-- >>> vgrep d0 [Chc 2 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [str "xy", Chc 2 [str "a"] [str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Chc 1 [str "ax"] [str "by"]]
-- []
-- >>> vgrep d0 [str "xy", Chc 2 [str "a"] [str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [str "xy", Chc 1 [str "a"] [str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Chc 1 [str "a"] [str "b"], str "xy"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])] 
-- >>> vgrep d0 [Chc 1 [str "a"] [str "b"], str "xy", Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0 [Chc 1 [str "a"] [str "b"], str "xy", Chc 2 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d1 [Chc 1 [str "ab"] [str "ac"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]
-- >>> vgrep d1 [str "a", Chc 1 [str "b"] [str "c"]]
-- [(((P 0 (Left 0),[("d",1)]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[])]
-- >>> vgrep d1 [str "ab", Chc 1 [str "b"] [str "c"]]
-- []
-- >>> vgrep d1 [str "a", Chc 1 [str "bx"] [str "c"]]
-- []
-- >>> vgrep d1 [str "a", Chc 1 [str "b"] [str "cy"]]
-- []
-- >>> vgrep d1 [str "a", Chc 1 [str "bx"] [str "cy"]]
-- []
-- >>> vgrep d1 [str "a", Chc 1 [str "b"] [str "c"], Chc 1 [str "ab"] [str "ac"]]
-- [(((P 0 (Left 0),[("d",1)]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]
-- >>> vgrep d [Chc 2 [Chc 1 [str "a"] [str "b"]] [str "b"]]
-- []
-- >>> vgrep d [Chc 2 [Chc 1 [str "a"] [str "b"]] [str "x"]]
-- []
-- >>> vgrep d [Chc 2 [str "a"] [Chc 1 [str "a"] [str "b"]]]
-- []
-- >>> vgrep d [Chc 2 [str "x"] [Chc 1 [str "a"] [str "b"]]]
-- []

-- | nested matches in both the alternatives (** return the results from both the alternatives if present) 
-- >>> vgrep d0 [Chc 2 [Chc 1 [str "a"] [str "b"]] [str "b"]]
-- [(((P 0 (Right (P 0 (Right (P 0 (Left 0),NoPos)),P 0 (Left 0))),[("d",2)]),[Chc 2 [Chc 1 [Str "a"] []] [Str "b"]]),[]),(((P 0 (Right (P 0 (Right (P 0 (Left 0),P 0 (Left 0))),NoPos)),[("d",1)]),[Chc 2 [Chc 1 [Str "a"] [Str "b"]] []]),[])]
-- >>> vgrep d0 [Chc 2 [Chc 1 [str "a"] [str "b"]] [str "x"]]
-- [(((P 0 (Right (P 0 (Right (P 0 (Left 0),P 0 (Left 0))),NoPos)),[("d",1)]),[Chc 2 [Chc 1 [Str "a"] [Str "b"]] []]),[])]
-- >>> vgrep d0 [Chc 2 [str "a"] [Chc 1 [str "a"] [str "b"]]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (NoPos,P 0 (Left 0))))),[("d",2)]),[Chc 2 [Str "a"] [Chc 1 [] [Str "b"]]]),[]),(((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1)]),[Chc 2 [] [Chc 1 [Str "a"] [Str "b"]]]),[])]
-- >>> vgrep d0 [Chc 2 [str "x"] [Chc 1 [str "a"] [str "b"]]]
-- [(((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1)]),[Chc 2 [] [Chc 1 [Str "a"] [Str "b"]]]),[])]
-- >>>vgrep d2 [Chc 1 [str "b"] [str "c"],str "a"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "b"] [Str "c"],Str "a"]),[])]


-- 
-- |nested Matches
-- >>> vgrep (PChc (DVar "d") a (PChc (DVar "d1") (ch 'b') (ch 'c') )) ([Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]]])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1),("d1",2)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]]),[])]

-- | query variables ** query varaiables will not have the entire choice structure, instead just the part which matches the variable
-- >>> vgrep qv [Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Str "b"]))])]
-- >>> vgrep qv1 [Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Str "b"]))])]
-- >>> vgrep qv1 ([Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]]])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]]),[("x",((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[]),[Chc 2 [Str "b"] [Str "c"]]))])]
-- >>> vgrep rqv ([str "efg", Chc 1 [str "a"] [Chc 2 [str "c"] [str "b"]]])
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Right (NoPos,P 0 (Left 0))))),[("d",1)]),[Chc 1 [Str "a"] [Chc 2 [] [Str "b"]]]),[("x",((P 1 (Right (P 0 (Left 0),NoPos)),[]),[Str "a"]))]),(((P 1 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",2)]),[Chc 1 [] [Chc 2 [Str "c"] [Str "b"]]]),[("x",((P 1 (Right (NoPos,P 0 (Right (P 0 (Left 0),NoPos)))),[]),[Str "c"]))])]

-- | Query variables on split choices and empty strings
-- >>> vgrep (PChc (DVar "d") (ab) (QVar "x")) [Chc 1 [str "a"] [str "lm"], Chc 1 [str "b"] [str "op"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "lm"],Chc 1 [Str "b"] [Str "op"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Str "lm",Str "op"]))])]
-- >>> vgrep (PChc (DVar "d") (ab) (QVar "x")) [Chc 2 [str ""] [str "xy"],Chc 1 [str "a"] [str "lm"], Chc 1 [str "b"] [str "op"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "lm"],Chc 1 [Str "b"] [Str "op"]]),[("x",((P 1 (Right (NoPos,P 0 (Left 0))),[]),[Str "lm",Str "op"]))])]
-- >>> vgrep ((ch 'b') `Seq` _a) [Chc 1 [str "abcab"] [str "lmnaba"], str "aacabxa"]
-- [(((P 0 (Right (NoPos,P 0 (Left 4))),[]),[Chc 1 [] [Str "ba"],Str "a"]),[]),(((P 0 (Right (P 0 (Left 4),NoPos)),[]),[Chc 1 [Str "b"] [],Str "aa"]),[]),(((P 0 (Right (P 0 (Left 1),NoPos)),[]),[Chc 1 [Str "bca"] []]),[]),(((P 1 (Left 4),[]),[Str "bxa"]),[])]

-- | Choice patterns Star
-- | Exact Dimension **1) Side effect of allowing "Any"
-- >>> vgrep cp' [str "abcd"]
-- [(((P 0 (Left 0),[]),[Str "abcd"]),[])]
-- >>> vgrep cp' [Chc 2 [str "a"] [str "b"]]
-- []
-- >>> vgrep cp' [Chc 1 [str "ax"] [str "by"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "ax"] [Str "by"]]),[])]
-- >>> vgrep cp' [Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp' [str "xy", Chc 2 [str "a"] [str "b"]]
-- []
-- >>> vgrep cp' [str "xy", Chc 1 [str "a"] [str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp' [Chc 1 [str "a"] [str "b"], str "xy"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp' [Chc 2 [Chc 1 [str "a"] [str "b"]] [str "x"]]
-- [(((P 0 (Right (P 0 (Right (P 0 (Left 0),P 0 (Left 0))),NoPos)),[]),[Chc 2 [Chc 1 [Str "a"] [Str "b"]] []]),[])]
-- >>> vgrep cp' [Chc 1 [str "a"] [str "b"], str "xy", Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep cp1' [Chc 1 [str "ab"] [str "ac"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]
-- >>> vgrep cp1' [str "a", Chc 1 [str "b"] [str "c"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[])]
-- >>> vgrep cp3' [Chc 1 [str "b"] [str "c"],str "a"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "b"] [Str "c"],Str "a"]),[])]
-- >>> vgrep cp1' [str "ab", Chc 1 [str "b"] [str "c"]]
-- []
-- >>> vgrep cp1' [str "a", Chc 1 [str "bx"] [str "c"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "bx"] [Str "c"]]),[])]
-- >>> vgrep cp1' [str "a", Chc 1 [str "b"] [str "cy"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] [Str "cy"]]),[])]
-- >>> vgrep cp1' [str "a", Chc 1 [str "bx"] [str "cy"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "bx"] [Str "cy"]]),[])]
-- >>> vgrep cp1' [str "a", Chc 1 [str "lbx"] [str "cy"]]
-- []
-- >>> vgrep cp1' [str "a", Chc 1 [str "b"] [str "c"], Chc 1 [str "ab"] [str "ac"]]
-- [(((P 0 (Left 0),[]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]

-- |Dimension Variable
-- >>> vgrep d0' [Chc 2 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0' [str "xy", Chc 2 [str "a"] [str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0' [Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0' [Chc 1 [str "ax"] [str "by"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "ax"] [Str "by"]]),[])]
-- >>> vgrep d0' [str "xy", Chc 2 [str "a"] [str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0' [str "xy", Chc 1 [str "a"] [str "b"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0' [Chc 1 [str "a"] [str "b"], str "xy"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])] 
-- >>> vgrep d0' [Chc 1 [str "a"] [str "b"], str "xy", Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d0' [Chc 1 [str "a"] [str "b"], str "xy", Chc 2 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",2)]),[Chc 2 [Str "a"] [Str "b"]]),[])]
-- >>> vgrep d1' [Chc 1 [str "ab"] [str "ac"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]
-- >>> vgrep d1' [str "a", Chc 1 [str "b"] [str "c"]]
-- [(((P 0 (Left 0),[("d",1)]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[])]
-- >>> vgrep d1' [str "ab", Chc 1 [str "b"] [str "c"]]
-- []
-- >>> vgrep d1' [str "a", Chc 1 [str "bx"] [str "c"]]
-- [(((P 0 (Left 0),[("d",1)]),[Str "a",Chc 1 [Str "bx"] [Str "c"]]),[])]
-- >>> vgrep d1' [str "a", Chc 1 [str "b"] [str "cy"]]
-- [(((P 0 (Left 0),[("d",1)]),[Str "a",Chc 1 [Str "b"] [Str "cy"]]),[])]
-- >>> vgrep d1' [str "a", Chc 1 [str "bx"] [str "cy"]]
-- [(((P 0 (Left 0),[("d",1)]),[Str "a",Chc 1 [Str "bx"] [Str "cy"]]),[])]
-- >>> vgrep d1' [str "a", Chc 1 [str "b"] [str "c"], Chc 1 [str "ab"] [str "ac"]]
-- [(((P 0 (Left 0),[("d",1)]),[Str "a",Chc 1 [Str "b"] [Str "c"]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "ab"] [Str "ac"]]),[])]
-- >>> vgrep (PChc (DVar "d") a b) ([Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]] ])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),NoPos)))),[("d",1)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] []]]),[])]
-- >>> vgrep (PChc (DVar "d") (seq [a,Any]) (seq [b,Any])) ([Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]] ])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),NoPos)))),[("d",1)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] []]]),[])]
-- 
-- >> vgrep (PChc (DVar "d") (seq [Any,a,Any]) (seq [Any,b,Any])) ([Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]] ])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),NoPos)))),[("d",1)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] []]]),[])]
-- >>> vgrep (PChc (DVar "d") a b) ([Chc 1 [str "a"] [Chc 2 [str "b"] [Chc 3 [str "a"] [str "a"]]] ])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),NoPos)))),[("d",1)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] []]]),[])]
-- >>> vgrep (PChc (DVar "d") b a) ([Chc 1 [str "a"] [Chc 2 [str "b"] [Chc 3 [str "a"] [str "a"]]] ])
-- [(((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Right (NoPos,P 0 (Left 0))))))),[("d",2)]),[Chc 1 [] [Chc 2 [Str "b"] [Chc 3 [] [Str "a"]]]]),[]),(((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),NoPos)))))),[("d",2)]),[Chc 1 [] [Chc 2 [Str "b"] [Chc 3 [Str "a"] []]]]),[])]

-- | nested matches in both the alternatives TODO CHECK IF THE BEHAVIOR IS CORRECT 
-- >>> vgrep d0' [Chc 2 [Chc 1 [str "a"] [str "b"]] [str "b"]]
-- [(((P 0 (Right (P 0 (Right (P 0 (Left 0),P 0 (Left 0))),NoPos)),[("d",1)]),[Chc 2 [Chc 1 [Str "a"] [Str "b"]] []]),[])]
-- >>> vgrep d0' [Chc 2 [Chc 1 [str "a"] [str "b"]] [str "x"]]
-- [(((P 0 (Right (P 0 (Right (P 0 (Left 0),P 0 (Left 0))),NoPos)),[("d",1)]),[Chc 2 [Chc 1 [Str "a"] [Str "b"]] []]),[])]
-- >>> vgrep d0' [Chc 2 [str "a"] [Chc 1 [str "a"] [str "b"]]]
-- [(((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1)]),[Chc 2 [] [Chc 1 [Str "a"] [Str "b"]]]),[])]
-- >>> vgrep d0' [Chc 2 [str "x"] [Chc 1 [str "a"] [str "b"]]]
-- [(((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1)]),[Chc 2 [] [Chc 1 [Str "a"] [Str "b"]]]),[])]
-- >>>vgrep d2' [Chc 1 [str "b"] [str "c"],str "a"]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "b"] [Str "c"],Str "a"]),[])]
-- 
-- |nested Matches
-- >>> vgrep (PChc (DVar "d") (seq [a,Any]) (PChc (DVar "d1") (seq [Any,b,Any]) (seq [Any,c]) )) ([Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]]])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1),("d1",2)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]]),[])]
-- >>> vgrep (PChc (DVar "d") (seq [Any,a]) (PChc (DVar "d1") (seq [Any,b,Any]) (seq [c,Any]) )) ([Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]]])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1),("d1",2)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]]),[])]
-- >>> vgrep (PChc (DVar "d") (seq [Any,a]) (PChc (DVar "d1") (seq [Any,b,Any]) (seq [c,Any]) )) ([Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]], str "abc",Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]] ])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1),("d1",2)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]]),[]),(((P 2 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1),("d1",2)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]]),[])]


-- | query variables
-- >>> vgrep qv' [Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[]),[Chc 1 [Str "a"] [Str "b"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Str "b"]))])]
-- >>> vgrep qv1' [Chc 1 [str "a"] [str "b"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "b"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Str "b"]))])]
-- >>> vgrep qv1' ([Chc 1 [str "a"] [Chc 2 [str "b"] [str "c"]]])
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",1)]),[Chc 1 [Str "a"] [Chc 2 [Str "b"] [Str "c"]]]),[("x",((P 0 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[]),[Chc 2 [Str "b"] [Str "c"]]))])]
-- >>> vgrep rqv' ([str "efg", Chc 1 [str "a"] [Chc 2 [str "c"] [str "b"]]])
-- [(((P 1 (Right (NoPos,P 0 (Right (P 0 (Left 0),P 0 (Left 0))))),[("d",2)]),[Chc 1 [] [Chc 2 [Str "c"] [Str "b"]]]),[("x",((P 1 (Right (NoPos,P 0 (Right (P 0 (Left 0),NoPos)))),[]),[Str "c"]))])]

-- | Query variables on split choices and empty strings
-- >>> vgrep (PChc (DVar "d") (seq [Any,a,b,Any]) (QVar "x")) [Chc 1 [str "a"] [str "lm"], Chc 1 [str "b"] [str "op"]]
-- [(((P 0 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "lm"],Chc 1 [Str "b"] [Str "op"]]),[("x",((P 0 (Right (NoPos,P 0 (Left 0))),[]),[Str "lm",Str "op"]))])]
-- >>> vgrep (PChc (DVar "d") (seq [a,b,Any]) (QVar "x")) [Chc 2 [str ""] [str "xy"],Chc 1 [str "a"] [str "lm"], Chc 1 [str "b"] [str "op"]]
-- [(((P 1 (Right (P 0 (Left 0),P 0 (Left 0))),[("d",1)]),[Chc 1 [Str "a"] [Str "lm"],Chc 1 [Str "b"] [Str "op"]]),[("x",((P 1 (Right (NoPos,P 0 (Left 0))),[]),[Str "lm",Str "op"]))])]

