{-# LANGUAGE ViewPatterns #-}

module VPM where

import Prelude hiding (seq)
import Data.Maybe


data Atomic = C Char | Wild deriving(Show,Eq)

data Pattern = Plain Atomic
             | Seq Atomic Pattern
             | Alt Pattern Pattern
             deriving(Show,Eq)


type VString = [Segment]

type Dim = Int

data Segment = Str String | Chc Dim VString VString
             deriving(Show)

type VMatch = (Pos,[Match])

data Match = MStr String | MChc Dim [VMatch] [VMatch]
            deriving(Show,Eq)

type Input = (Pos,VString)

type Pos = Int

data SplitTy = StrSplit VMatch Input
             | PatSplit VMatch Pattern
             deriving(Show)

data Split = NoMatch
           | SM SplitTy  --Single Match
           | PM SplitTy SplitTy --Parallel Match (SplitStop, SplitContinue)
           deriving(Show)

type Matches = [VMatch]

data Alt = L|R


mStr c = (0, [MStr c])

vChr cs = Str cs

vStr [] = []
vStr s  = [Str s]

mlistToV :: [VMatch] -> Input
mlistToV []  = (0,[])
mlistToV ms  = mToV (last ms)

mToV :: VMatch -> Input
mToV (i,[])                = (i,[])
mToV (i,MStr s : ss)       = (i,Str s : (snd $ mToV (i,ss)))
mToV (i,MChc d v1 v2 : ss) = (i,(Chc d (snd $ mlistToV v1 ) (snd $ mlistToV v2 ))
                                : (snd $ mToV (i,ss)))

proceed1 :: VString -> VString
proceed1 []                 = []
proceed1 (Str [] : ss)      = ss
proceed1 (Str (c:s) : ss)   = (vStr s) ++ ss
proceed1 (Chc d v1 v2 : ss) = Chc d (proceed1 v1) (proceed1 v2) : ss

appendVM :: VMatch -> VMatch -> VMatch
appendVM (p, ms) (p',ms') = (p, (appendM ms ms'))

appendM :: [Match] -> [Match] -> [Match]
appendM [] m                          = m
appendM m []                          = m
appendM (MStr s : ms) (MStr s' : ms') = (MStr (s++s')) : appendM ms ms'
appendM (m:ms) (m':ms')               = m:m':appendM ms ms' --TODO choices

orElse :: Split -> Split -> Split
orElse NoMatch s   = s
orElse s _         = s --TODO return both the matches like the unix grep?

andThen :: Split -> Split-> Split
andThen NoMatch _                                 = NoMatch
andThen _       NoMatch                           = NoMatch
andThen (SM (StrSplit m s)) (SM (PatSplit m' p))  = SM (PatSplit (appendVM m m') p)
andThen (SM (StrSplit m s)) (SM (StrSplit m' s')) = SM (StrSplit (appendVM m m') s')
andThen _                         _ = undefined


matchStr :: Pattern -> Pos -> String -> Split
matchStr (Plain (C d)) i (c:s)
                     | c==d    = SM $ StrSplit (i,[MStr[c]]) (i+1,vStr s)
matchStr (Plain Wild) i (c:s)  = SM $ StrSplit (i,[MStr[c]]) (i+1,vStr s)
matchStr (Seq a p)    i (c:[]) = matchStr (Plain a) i [c] `andThen` (SM (PatSplit (0,[]) p))
matchStr (Seq a p)    i (c:s)  = matchStr (Plain a) i [c] `andThen` matchStr p (i+1) s
matchStr (Alt p q)    i s      = matchStr p i s `orElse` matchStr q i s
matchStr _          _   _      = NoMatch

--To Match the first character of the next segment and not scan
rigidMatch :: Pattern -> VMatch -> Pattern -> (Pos,Segment) -> (Maybe VMatch,Split)
rigidMatch origP (j,m) p (i,s) = case matchSegment origP p (i,s) of
  (Nothing,NoMatch)                  -> (Nothing,SM (StrSplit (0,[]) (j,
                               (proceed1 $ snd $ mToV (j,m)) ++ [s]))) --new match
  (ma,SM (StrSplit m' (j',s'))) -> (Just $ appendVM (appendVM (j,m) (fromMaybe (0,[]) ma)) m',SM (StrSplit (0,[]) (j',s')))
  (ma,SM (PatSplit m' p'))      -> (Just $ appendVM (j,m) (fromMaybe (0,[]) ma),SM (PatSplit m' p'))
  (ma,PM s1 s2)                 -> undefined

matchSegment :: Pattern -> Pattern -> (Pos,Segment) -> (Maybe VMatch,Split)
matchSegment origP p (i, Str s') = (Nothing,matchStr p i s')
matchSegment origP p (i,vs@(Chc d v1 v2)) = matchChoice d i (matchAlt origP d i L p (0,v1))
                                         (matchAlt origP d i R p (0,v2))

matchChoice :: Dim -> Pos -> (Maybe VMatch,Split) -> (Maybe VMatch,Split) -> (Maybe VMatch,Split)
matchChoice d i s (Nothing,NoMatch) = s
matchChoice d i (Nothing,NoMatch) s = s
matchChoice d i s1 s2     = checkOverlap d i s1 s2

checkOverlap :: Dim -> Pos -> (Maybe VMatch,Split) -> (Maybe VMatch,Split) -> (Maybe VMatch,Split)
checkOverlap d i (Nothing,SM (StrSplit (k,[MChc dim v1 []]) (j,[]))) (Nothing,SM (StrSplit (k',[MChc dim' [] v2]) (j',[])))
   | j == j'   = (Nothing,SM (StrSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 v2))) (j+1,[])))
   | otherwise = undefined
checkOverlap d i (Nothing,SM (StrSplit (k,[MChc dim v1 []]) (j,[]))) (ma',SM (PatSplit (k',m) p)) =
   (Nothing, PM (StrSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 (getAltMatches (fromMaybe (0,[]) ma'))))) (j,[])) (PatSplit (k',m) p))
checkOverlap d i (ma,SM (PatSplit (k,m) p)) (Nothing,SM (StrSplit (k',[MChc dim' [] v2]) (j',[]))) =
   (Nothing, PM (StrSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 (getAltMatches (fromMaybe (0,[]) ma)) v2))) (j',[])) (PatSplit (k,m) p))
checkOverlap d i (ma,SM (PatSplit m@(k,[MChc dim v1 []]) p)) (ma',SM (PatSplit m'@(k',[MChc dim' [] v2]) p'))
  | p == p'          = (genVMatch d i (resolveOverlap 0 (getAltMatches (fromMaybe (0,[]) ma)) (getAltMatches (fromMaybe (0,[]) ma'))), SM (PatSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 v2))) p))
  | otherwise = undefined --TODO
checkOverlap d i _ _ = undefined --if PM


getAltMatches :: VMatch -> Matches
getAltMatches (_,[]) = []
getAltMatches (_,[MChc d m []]) = m
getAltMatches (_,[MChc d [] m]) = m
getAltMatches (_,_) = undefined



genVMatch :: Dim -> Pos -> (Matches,Matches) -> Maybe VMatch
genVMatch d i ([],[])  = Nothing
genVMatch d i (ms,ms') = Just (i,[MChc d ms ms'])

resolveOverlap :: Pos -> Matches -> Matches -> (Matches,Matches)
resolveOverlap _ [] [] = ([],[])
resolveOverlap _ ms [] = (ms,[])
resolveOverlap _ [] ms' = ([],ms')
resolveOverlap k ((j,m):ms) ((j',m'):ms')
   | j < j' && j>= k = ([(j,m)],[]) `and` resolveOverlap (j+longest m []) ms ((j',m'):ms')
   | j < j' && j< k  = ([],[]) `and` resolveOverlap k ms ((j',m'):ms')
   | j'< j && j'>= k = ([], [(j',m')]) `and` resolveOverlap (j'+longest m' []) ((j,m):ms) ms'
   | j'< j && j'< k  = ([], []) `and` resolveOverlap k ((j,m):ms) ms'
   | j == j' && j >= k = ([(j,m)],[(j',m')]) `and` resolveOverlap (longest m m') ms ms'
   where and (m,m') (ms,ms') = (m++ms, m'++ms')

longest :: [Match] -> [Match] -> Int
longest v1 v2 = let l = foldl (+) 0 (map (lenIncrM) v1)
                    r = foldl (+) 0 (map (lenIncrM) v2)
                in max l r

lenIncrM :: Match -> Int
lenIncrM (MStr s)     = length s
lenIncrM (MChc _ _ _) = 1

--TODO[1] the position of the remaining segments (j) in the alternative is lost.
-- to track this, we either have to introduce Pos in VString or match the
-- remaining alternatives with the original patten again (not the Split pattern)
inLeft :: Dim -> Pos -> Split ->  Split
inLeft d i (SM (StrSplit m (j,s))) = SM (StrSplit m (i,[Chc d s []]))
inLeft d i s                   = s

inRight :: Dim -> Pos -> Split ->  Split
inRight d i (SM (StrSplit m (j,s))) = SM (StrSplit m (i,[Chc d [] s]))
inRight d i s                   = s

matchAlt :: Pattern -> Dim -> Pos -> Alt -> Pattern -> Input -> (Maybe VMatch,Split)
matchAlt origP d i a p (j,[]) = (Nothing,SM (PatSplit (0,[]) p))
matchAlt origP d i a p (j,s:ss) = case matchSegment origP p (j,s) of
  (ma,NoMatch)                  -> (Nothing,NoMatch){-let (ms,sp) = scan origP (j,s:ss)
                                       (ma',sp') = altSplit a i d (ms) sp
                                   in (join ma ma',sp')-}
  (ma,SM (StrSplit m' (j',s'))) -> let (ms,sp) = scan origP (j',s'++ss)
                                       (ma',sp') = altSplit a i d (m':ms) sp
                                   in (join ma ma',sp')
  (m,SM (PatSplit m' p'))       -> (m,SM (PatSplit m' p'))
  (m,PM s1 s2)                 -> undefined

join :: Maybe VMatch -> Maybe VMatch -> Maybe VMatch
join Nothing Nothing = Nothing
join _ Nothing       = Nothing
join Nothing m       = m
join (Just (i,m)) (Just (j,m')) = Just (i,m++m')

altSplit :: Alt -> Pos -> Dim -> Matches -> Split -> (Maybe VMatch, Split)
altSplit a i d ms NoMatch                  = (Nothing, SM $ StrSplit (i,createMChc a d ms) (i+1,[]))
altSplit a i d ms (SM (StrSplit m (j,[]))) = (Nothing, SM $ StrSplit (i,createMChc a d (ms++[m])) (i+1,[]))
altSplit a i d ms (SM (PatSplit m p))      = (Just (i,createMChc a d ms) ,SM (PatSplit (i,createMChc a d [m]) p))
altSplit _ _ _ _ (PM s1 s2)                = undefined

createMChc :: Alt -> Dim -> [VMatch] -> [Match]
createMChc a d [] = []
createMChc L d vs = [MChc d vs []]
createMChc R d vs = [MChc d [] vs]

appendMatches :: Matches -> (Matches,Split) -> (Matches,Split)
appendMatches ms (ms',s) = (ms++ms',s)

addMatch :: VMatch -> (Matches,Split) -> (Matches,Split)
addMatch (_,[]) m = m
addMatch m (ms,s) = (m:ms,s)

continue :: Pattern -> Pos -> VString -> Split -> (Matches,Split)
continue p i v (SM (StrSplit m (j,s')))      = addMatch m (scan p (j,s'++v)) --next occurrence
continue p i (s:ss) (SM (PatSplit (j,m) p')) = let (ma,sp) = rigidMatch p (j,m) p' (i,s)
 in addMatch (fromMaybe (j,[]) ma) (continue p (i+(lenIncr s)) ss sp)
continue p i v (PM s1 s2)                    = undefined
continue _ _ [] s@(SM (PatSplit (j,m) p'))   = ([],s)


leftMatch :: Dim -> Pos -> Split -> Split --TODO use Alt
leftMatch _ _ NoMatch             = NoMatch
leftMatch d i (SM (StrSplit m s)) = SM $ StrSplit (i,[MChc d [m] []]) s
leftMatch d i (SM (PatSplit m p)) = SM $ PatSplit (i,[MChc d [m] []]) p
leftMatch d i (PM s1 s2)          = undefined

rightMatch :: Dim -> Pos -> Split -> Split
rightMatch _ _ NoMatch             = NoMatch
rightMatch d i (SM (StrSplit m s)) = SM $ StrSplit (i,[MChc d [] [m]]) s
rightMatch d i (SM (PatSplit m p)) = SM $ PatSplit (i,[MChc d [] [m]]) p
rightMatch d i (PM s1 s2)          = undefined

scanChoice :: Dim -> Pos -> (Matches,Split) -> (Matches,Split) -> (Matches,Split)
scanChoice d i ([],NoMatch) ([],NoMatch) = ([],NoMatch) --TODO non empty matches and NoMatch
scanChoice d i (ml,sl) ([],NoMatch)
  | null ml    = ([], leftMatch d i sl)
  | otherwise  = ([(i,[MChc d ml []])], leftMatch d i sl)
scanChoice d i ([],NoMatch) (mr,sr)
  | null mr    = ([], rightMatch d i sr)
  | otherwise  = ([(i,[MChc d [] mr])], rightMatch d i sr)
scanChoice d i (ml,sl) (mr,sr)  =
  let (ma,sp) = matchChoice d i (Nothing,leftMatch d i sl) (Nothing,rightMatch d i sr)
  in (checkNoMatch (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 ml mr)))
    ++  (checkNoMatch $ fromMaybe (0,[]) ma), sp)

checkNoMatch :: VMatch -> Matches
checkNoMatch (0,[]) = []
checkNoMatch v      = [v]

scanStr :: Pattern -> (Pos,String) -> Split
scanStr p (i,matchStr p i-> SM s)     = SM s
scanStr p (i,matchStr p i-> PM s1 s2) = undefined --TODO
scanStr p (i,_:s)                     = scanStr p (i+1,s)
scanStr _ (_,[])                      = NoMatch

scanSegment :: Pattern -> (Pos,Segment) -> (Matches,Split)
scanSegment p (i, Str s')            = ([],scanStr p (i,s'))
scanSegment p (i,vs@(Chc dim v1 v2)) = scanChoice dim i (scan p (0,v1)) (scan p (0,v2))

lenIncr :: Segment -> Int
lenIncr (Str s)     = length s
lenIncr (Chc _ _ _) = 1

scan :: Pattern -> Input -> (Matches,Split)
scan p (i,[])  = ([],NoMatch)
scan p (i,s:ss) = case scanSegment p (i,s) of
  (ms,NoMatch)  -> appendMatches ms (scan p (i+(lenIncr s),ss))
  (ms,split)    -> appendMatches ms (continue p (i+(lenIncr s)) ss split)

match :: Pattern -> VString -> Matches
match p s = fst $ scan p (0,s)


-- patterns
--
ch :: Char -> Pattern
ch = Plain . C

seq :: [Pattern] -> Pattern
seq [p]    = p
seq (Plain a:ps) = Seq a (seq ps)

[a,b,c] = map ch "abc"

ab = seq [a,b]
abc = seq [a,b,c]
_a = Wild `Seq` a

a'b = Alt a b


-- Strings
--
s = "xabyabcz"
