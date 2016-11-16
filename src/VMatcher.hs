module VMatcher (vmatch, VMatches, VMatch, Selection) where

import VPM hiding (M, VMatches, VMatch, match)
import Data.Maybe (fromJust, isJust)

type VMatches = [VMatch]
type VMatch = ([Pos], Selection, String)

type Selection = [Decision]
type Decision = (Dim, Bool)

vmatch :: Pattern -> VString -> VMatches
vmatch pat vstring = fst (separate (vmatch' [pat] vstring [] [] [] (Just pat) "") [] [])

vmatch' :: [Pattern] -> VString -> VString -> [Pos] -> Selection -> Maybe Pattern -> String -> [(VMatch, Bool)]
vmatch' _ [] _ _ _ _ _ = []

separate [] match unmatch = (match, unmatch)
separate ((x, True):xs) match unmatch = separate xs (x:match) unmatch
separate ((x, False):xs) match unmatch = separate xs match (x:unmatch)

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
m pat (Str []:xs) _ pos sel repeat match _continue =
  m pat xs 0 pos sel repeat match False
m [] vstring i pos sel repeat match continue =
  (if continue then matchMerge (m [repeat] vstring i (i:tail pos)
                                  sel repeat match continue)
   else id) [((pos, sel, match), True)]
m (Plain p:ps) (Str (c:cs):xs) i pos sel repeat match continue =
  if charMatch p c then m ps (Str cs:xs) (1+i) pos sel repeat match continue
  else [((pos, sel, match), False)]
m (Seq a b:ps) vstring i pos sel repeat match continue =
  m (a:b:ps) vstring i pos sel repeat match continue
m (Alt a b:ps) vstring i pos sel repeat match continue =
  matchMerge (m (a:ps) vstring i pos sel repeat match continue)
             (m (b:ps) vstring i pos sel repeat match continue)
m (Repeat _ _ (Just 0):ps) vstring i pos sel repeat match continue =
  m ps vstring i pos sel repeat match continue
m [Repeat _ 0 _] [] _ pos sel _ match _ = [((pos, sel, match), True)]
m (Repeat pat min max:ps) vstring i pos sel repeat match continue =
  []
m (None:xs) vstring i pos sel repeat match continue =
  m xs vstring i pos sel repeat match continue
m pat [] _ pos sel _ match continue = [((pos, sel, match), null pat)]

-- | Discard the third argument if the second argument is a match with
-- the same selection as the first argument.
matchMerge :: [(VMatch, Bool)] -> [(VMatch, Bool)] -> [(VMatch, Bool)]
matchMerge a _ | all snd a = a
matchMerge a b = a ++ b
