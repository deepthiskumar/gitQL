module VMatcher (vmatch, VMatches, VMatch, Selection, Decision) where

import VPM hiding (M, VMatches, VMatch, match)
import Data.Maybe (fromJust, isJust)
import Data.List (sortBy)

type VMatches = [VMatch]
type VMatch = ([Pos], Selection, String)

type Selection = [Decision]
type Decision = (Dim, Bool)

{-|
>>> vmatch (ch '3') [Str "31"]
[([0,0],[],"3")]
>>> vmatch (ch '1') [Str "31"]
[([0,1],[],"1")]
>>> vmatch (Seq (ch '3') (ch '3')) [Str "31"]
[]
>>> vmatch (Seq (ch '3') (ch '1')) [Str "31"]
[([0,0],[],"31")]
>>> vmatch (Seq (Alt (ch '3') (ch '1')) None) [Str "31"]
[([0,0],[],"3"),([0,1],[],"1")]
>>> vmatch (Seq (ch 'a') (ch 'b')) [Chc 1 [Str "ab"] [Str "cd"]]
[([0,0,0,0],[(1,False)],"ab")]
>>> vmatch (Seq (ch 'a') (ch 'b')) [Chc 1 [Str "xab"] [Str "cd"]]
[([0,0,0,1],[(1,False)],"ab")]
>>> vmatch (Seq (ch 'a') (ch 'b')) [Str "xyabc"]
[([0,2],[],"ab")]
>>> vmatch (Seq (ch 'a') (ch 'b')) [Str "a", Str "b"]
[([0,0],[],"ab")]
>>> vmatch (Seq (ch 'a') (ch 'b')) [Chc 1 [Str "a"] [], Chc 1 [Str "b"] []]
[([0,0,0,0],[(1,False)],"ab")]
>>> vmatch (Seq (ch 'a') (ch 'a')) [Chc 1 [Str "a"] [], Str "aa"]
[([0,0,0,0],[(1,False)],"aa"),([1,0],[],"aa")]
>>> vmatch (Repeat (ch 'a') 2 (Just 2)) [Chc 1 [Str "a"] [], Str "aa"]
[([0,0,0,0],[(1,False)],"aa"),([1,0],[],"aa")]
>>> vmatch (Repeat (ch 'a') 2 Nothing) [Chc 1 [Str "a"] [], Str "aa"]
[([0,0,0,0],[(1,False)],"aaa"),([1,0],[],"aa")]
>>> vmatch (Seq (ch 'a') (ch 'b')) [Str "ababcab"]
[([0,0],[],"ab"),([0,2],[],"ab"),([0,5],[],"ab")]
>>> vmatch (Repeat (Seq (ch 'a') (ch 'b')) 0 (Just 1)) [Str "ababcab"]
[([0,0],[],"ab"),([0,2],[],"ab"),([0,5],[],"ab")]
-}
{-|
>>> vmatch (Repeat (Seq (ch 'a') (ch 'b')) 0 Nothing) [Str "ababcab"]
[([0,0],[],"abab"),([0,5],[],"ab")]
-}
vmatch :: Pattern -> VString -> VMatches
vmatch pat vstring =
  sortBy
  (\ (a, _, _) (b, _, _) -> compare a b)
  (map
   (\ (a, b, c) -> (reverse a, b, reverse c))
   (map fst . filter snd $ vmatch' pat vstring [] 0 [0] []))

{-|
>>> vmatch' (Seq (ch 'a') (ch 'b')) [Chc 1 [Str "a"] [], Chc 1 [Str "b"] []] [] 0 [0] []
[(([0,0,0,0],[(1,False)],"ba"),True),(([0,0,0,1],[(1,False)],""),False)]
-}
vmatch' :: Pattern -> VString -> VString -> Pos -> [Pos] -> Selection -> [(VMatch, Bool)]
vmatch' _ _ _ _ [] _ = undefined
vmatch' _ [] _ _ _ _ = []
vmatch' pat (Str str:xs) rest index (p:ps) sel =
  rep str 0 ++
  vmatch' pat xs rest (index + 1) (p+1 : ps) sel
  where rep (s:ss) i =
          (++)                    -- matchMerge
          (m [pat] (Str (s:ss):xs) rest 0 (i:index:ps) True sel "" False)
          (rep ss (1+i))
        rep [] _ = []
vmatch' pat (chc:xs) rest index pos@(p:ps) sel =
  concatMap (\(x, newpos, newsel) -> vmatch' pat x (xs ++ rest) 0 (index:newpos) newsel)
            (chcHandler chc pos sel True) ++
  vmatch' pat xs rest (index + 1) (p + 1 : ps) sel

chcHandler :: Segment -> [Pos] -> Selection -> Bool -> [(VString, [Pos], Selection)]
chcHandler (Chc dim a b) pos sel updatePos =
  case lookup dim sel of
    Just False -> [left sel]
    Just True -> [right sel]
    Nothing -> [left ((dim, False):sel), right ((dim, True):sel)]
  where left newsel = (a, maybePush updatePos 0 pos, newsel)
        right newsel = (b, maybePush updatePos 1 pos, newsel)

maybePush :: Bool -> a -> [a] -> [a]
maybePush True x xs = x:xs
maybePush False _ xs = xs

{-|
>>> m [Seq (ch 'a') (ch 'b')] [Chc 1 [Str "a"] [], Chc 1 [Str "b"] []] [] 0 [] True [] "" False
[(([0,0],[(1,False)],"ba"),True),(([1,0,1,0],[(1,True)],""),False)]
>>> m [Repeat (ch 'a') 2 Nothing] [Chc 1 [Str "a"] []] [Str "a"] 0 [] True [] "" False
[(([0,0],[(1,False)],"aa"),True),(([1,0],[(1,True)],""),False)]
>>> m [Repeat (ch 'a') 2 Nothing] [Chc 1 [Str "a"] []] [Str "aa"] 0 [] True [] "" False
[(([0,0],[(1,False)],"aaa"),True),(([1,0],[(1,True)],""),False)]
-}
m :: [Pattern] -> VString -> VString -> Pos -> [Pos] -> Bool -> Selection -> String -> Bool -> [(VMatch, Bool)]
-- m pat vstring rest vstringIndex pos updatePos sel match started
m _ [] _ _ pos _ sel match False = [((pos, sel, match), False)]
m [] _ _ _ pos _ sel match started = [((pos, sel, match), started)]
m pat (chc@(Chc _ _ _):xs) rest vstringIndex pos updatePos sel match started =
  concatMap (\(x, newpos, newsel) -> m pat (x++xs) rest 0 newpos updatePos
                                       newsel match started)
            (chcHandler chc (maybePush updatePos vstringIndex pos) sel updatePos)
m pat [] (chc@(Chc _ _ _):rest) vstringIndex pos updatePos sel match started =
  concatMap (\(x, newpos, newsel) -> m pat [] (x++rest) 0 newpos updatePos
                                       newsel match started)
            (chcHandler chc (maybePush updatePos vstringIndex pos) sel updatePos)
m pat (Str []:xs) rest _ pos _ sel match started =
  m pat xs rest undefined pos False sel match started
m pat [] (Str []:rest) _ pos _ sel match started =
  m pat [] rest undefined pos False sel match started
m (Plain p:ps) (Str (c:cs):xs) rest _ pos _ sel match _ =
  if charMatch p c
  then m ps (Str cs:xs) rest undefined pos False sel (c:match) True
  else [((pos, sel, match), False)]
m (Plain p:ps) [] (Str (c:cs):rest) _ pos _ sel match _ =
  if charMatch p c
  then m ps [] (Str cs:rest) undefined pos False sel (c:match) True
  else [((pos, sel, match), False)]
m (Seq a b:ps) vstring rest vstringIndex pos updatePos sel match started =
  m (a:b:ps) vstring rest vstringIndex pos updatePos sel match started
m (Alt a b:ps) vstring rest vstringIndex pos updatePos sel match started =
  matchMerge (m (a:ps) vstring rest vstringIndex pos updatePos sel match started)
             (m (b:ps) vstring rest vstringIndex pos updatePos sel match started)
m (Repeat _ _ (Just 0):ps) vstring rest vstringIndex pos updatePos sel match started =
  m ps vstring rest vstringIndex pos updatePos sel match started
m (Repeat _ 0 _:xs) [] [] vstringIndex pos updatePos sel match started =
  m xs [] [] vstringIndex pos updatePos sel match started
m (Repeat pat 0 max:xs) vstring rest vstringIndex pos updatePos sel match started =
  concatMap (\ x@((newpos, newsel, _), matched) ->
              if matched then [x]
              else m xs vstring rest vstringIndex newpos updatePos newsel match started)
            (m (pat:Repeat pat 0 (fmap unsignedDec max):xs) vstring rest
               vstringIndex pos updatePos sel match started)
m (Repeat pat min max:ps) vstring rest vstringIndex pos updatePos sel match started =
  m (pat:Repeat pat (unsignedDec min) (fmap unsignedDec max):ps)
    vstring rest vstringIndex pos updatePos sel match started
m (None:xs) vstring rest vstringIndex pos updatePos sel match started =
  m xs vstring rest vstringIndex pos updatePos sel match started
m pat [] [] _ pos _ sel match _ = [((pos, sel, match), null pat)]

unsignedDec :: Int -> Int
unsignedDec 0 = 0
unsignedDec n = n-1

-- | Discard the second argument if the first argument is all matches.
matchMerge :: [(VMatch, Bool)] -> [(VMatch, Bool)] -> [(VMatch, Bool)]
matchMerge a b = if all snd a then a else a ++ b
