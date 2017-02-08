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
-- |>>> vgrep _a v
-- [VM [] 0 [MStr "xa"],VM [] 3 [MStr "ya"]]
-- |>>> vgrep abc [Str "aab", Str "cxabc"]
-- [VM [] 1 [MStr "abc"],VM [] 5 [MStr "abc"]]
-- |>>> vgrep abc [Str "abcab", Str "cxab"]
-- [VM [] 0 [MStr "abc"],VM [] 3 [MStr "abc"]]
-- |>>> vgrep ab [Str "abca", Str "bxab"]
-- [VM [] 0 [MStr "ab"],VM [] 3 [MStr "ab"],VM [] 6 [MStr "ab"]]
--
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


