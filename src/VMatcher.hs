module VMatcher (vmatch, VMatches, VMatch, Selection) where

import VPM hiding (M, VMatches, VMatch, match)
import Data.Maybe (fromJust, isJust)

type VMatches = [VMatch]
type VMatch = ([Pos], Selection, String)

type Selection = [Decision]
type Decision = (Dim, Bool)

{-|
>>> vmatch (ch '3') [Str "31"]
[([0],[],"3")]
>>> vmatch (Seq (ch '3') (ch '3')) [Str "31"]
[]
>>> vmatch (Seq (ch '3') (ch '1')) [Str "31"]
[([0],[],"31")]
-}
vmatch :: Pattern -> VString -> VMatches
vmatch pat vstring = fst (separate (vmatch' pat vstring [] [] []) [] [])

vmatch' :: Pattern -> VString -> VString -> [Pos] -> Selection -> [(VMatch, Bool)]
vmatch' _ [] _ _ _ = []
vmatch' pat vstr@(Str s:xs) rest pos sel =
  m [pat] vstr 0 (0:pos) sel pat "" True ++
  vmatch' pat xs rest (head pos + 1 : tail pos) sel
vmatch' pat (chc:xs) rest pos sel =
  concatMap (\ (x, newpos, newsel) -> vmatch' pat x (xs ++ rest) newpos newsel)
            (chcHandler chc pos sel) ++
  vmatch' pat xs rest (head pos + 1 : tail pos) sel

{-|
>>> separate [(1,True),(3,True),(5,False),(2,True)] [5] [0]
([2,3,1,5],[5,0])
-}
separate :: [(a, Bool)] -> [a] -> [a] -> ([a], [a])
separate [] match unmatch = (match, unmatch)
separate ((x, True):xs) match unmatch = separate xs (x:match) unmatch
separate ((x, False):xs) match unmatch = separate xs match (x:unmatch)

chcHandler :: Segment -> [Pos] -> Selection -> [(VString, [Pos], Selection)]
chcHandler (Chc dim a b) pos sel =
  case lookup dim sel of
    Just False -> [left sel]
    Just True -> [right sel]
    Nothing -> [left ((dim, False):sel), right ((dim, True):sel)]
  where left = c a 0
        right = c b 1
        c x p sel = (x, (p:tail pos), sel)

m :: [Pattern] -> VString -> Pos -> [Pos] -> Selection -> Pattern -> String -> Bool -> [(VMatch, Bool)]
-- m pat vstring i pos sel repeat match continue
m pat (Chc dim a b:xs) _ pos sel repeat match continue =
  case lookup dim sel of
    Just False -> left sel
    Just True -> right sel
    Nothing -> left ((dim, False):sel) ++ right ((dim, True):sel)
  where left sel = c sel a 0
        right sel = c sel b 1
        c sel x p = m pat (x++xs) 0 (0:p:tail pos) sel repeat match continue
m pat (Str []:xs) _ pos sel repeat match _ =
  m pat xs 0 pos sel repeat match False
m [] vstring i pos sel repeat match continue =
  let merged = (if continue
                then matchMerge (m [repeat] vstring i pos
                                   sel repeat match continue)
                else id) [((pos, sel, reverse match), True)] in
  if continue
  then merged ++ m [repeat] vstring i pos sel repeat match continue
  else merged
m (Plain p:ps) (Str (c:cs):xs) i pos sel repeat match continue =
  if charMatch p c
  then m ps (Str cs:xs) (1+i) pos sel repeat (c:match) continue
  else [((pos, sel, reverse match), False)]
m (Seq a b:ps) vstring i pos sel repeat match continue =
  m (a:b:ps) vstring i pos sel repeat match continue
m (Alt a b:ps) vstring i pos sel repeat match continue =
  matchMerge (m (a:ps) vstring i pos sel repeat match continue)
             (m (b:ps) vstring i pos sel repeat match continue)
m (Repeat _ _ (Just 0):ps) vstring i pos sel repeat match continue =
  m ps vstring i pos sel repeat match continue
m [Repeat _ 0 _] [] _ pos sel _ match _ = [((pos, sel, reverse match), True)]
m (Repeat pat min max:ps) vstring i pos sel repeat match continue =
  undefined
m (None:xs) vstring i pos sel repeat match continue =
  m xs vstring i pos sel repeat match continue
m pat [] _ pos sel _ match continue = [((pos, sel, reverse match), null pat)]

-- | Discard the third argument if the second argument is a match with
-- the same selection as the first argument.
matchMerge :: [(VMatch, Bool)] -> [(VMatch, Bool)] -> [(VMatch, Bool)]
matchMerge a _ | all snd a = a
matchMerge a b = if all snd a then a else a ++ b