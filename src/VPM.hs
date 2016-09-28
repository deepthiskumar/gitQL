{-# LANGUAGE ViewPatterns #-}

module VPM where

import Prelude hiding (seq)
import Data.Maybe
import Data.List

data Atomic = C Char | Wild deriving(Show,Eq)

data Pattern = Plain Atomic
             | Seq Pattern Pattern
             | Alt Pattern Pattern
             | Repeat Pattern Int {-min-} (Maybe Int) {-max-}
             --Ex. `a*` => `Repeat (ch 'a') 0 None`
             | None --Ex. `(a|)b` => `Seq (Alt (ch 'a') None) (ch 'b')
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
andThen _                   _                     = undefined


matchStr :: Pattern -> Pos -> String -> Split
matchStr (Plain (C d)) i (c:s)
                     | c==d    = SM $ StrSplit (i,[MStr[c]]) (i+1,vStr s)
matchStr (Plain Wild) i (c:s)  = SM $ StrSplit (i,[MStr[c]]) (i+1,vStr s)
matchStr (Seq a p)    i (c:[]) = matchStr a i [c] `andThen` (SM (PatSplit (0,[]) p))
matchStr (Seq a p)    i (c:s)  = matchStr a i [c] `andThen` matchStr p (i+1) s
matchStr (Alt p q)    i s      = matchStr p i s `orElse` matchStr q i s
matchStr _          _   _      = NoMatch

--To Match the first character of the next segment and not scan
rigidMatch :: Pattern -> VMatch -> Pattern -> (Pos,Segment) -> (Maybe VMatch,Split)
rigidMatch origP (j,m) p (i,s) = case matchSegment origP p (i,s) of
  (Nothing,NoMatch)             -> (Nothing,SM (StrSplit (0,[]) (j,
                               (proceed1 $ snd $ mToV (j,m)) ++ [s]))) --new match
  (ma,SM (StrSplit m' (j',s'))) -> (Just $ appendVM (appendVM (j,m) (fromMaybe (0,[]) ma)) m',SM (StrSplit (0,[]) (j',s')))
  (ma,SM (PatSplit m' p'))      -> (Just $ appendVM (j,m) (fromMaybe (0,[]) ma),SM (PatSplit m' p'))
  (ma,PM s1 s2)                 -> undefined

matchSegment :: Pattern -> Pattern -> (Pos,Segment) -> (Maybe VMatch,Split)
matchSegment origP p (i, Str s')          = (Nothing,matchStr p i s')
matchSegment origP p (i,vs@(Chc d v1 v2)) = matchChoice d i (matchAlt origP d i L p (0,v1))
                                         (matchAlt origP d i R p (0,v2))

matchChoice :: Dim -> Pos -> (Maybe VMatch,Split) -> (Maybe VMatch,Split) -> (Maybe VMatch,Split)
matchChoice d i s (Nothing,NoMatch) = s
matchChoice d i (Nothing,NoMatch) s = s
matchChoice d i s1 s2               = checkOverlap d i s1 s2

checkOverlap :: Dim -> Pos -> (Maybe VMatch,Split) -> (Maybe VMatch,Split) -> (Maybe VMatch,Split)
checkOverlap d i (Nothing,SM (StrSplit (k,[MChc dim v1 []]) (j,[]))) (Nothing,SM (StrSplit (k',[MChc dim' [] v2]) (j',[])))
   | j == j'   = (Nothing,SM (StrSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 v2))) (j+1,[])))
   | otherwise = undefined
checkOverlap d i (Nothing,s1@(SM (StrSplit (k,[MChc dim v1 []]) (j,[])))) (ma',s2@(SM (PatSplit (k',m) p))) =
   let (ms,v) = separateLastMatch s1 s2 v1
   in (genVMatch d i (resolveOverlap 0 ms (getAltMatches (fromMaybe (0,[]) ma'))), PM (StrSplit v (j,[])) (PatSplit (k',m) p))
checkOverlap d i (ma,s1@(SM (PatSplit (k,m) p))) (Nothing,s2@(SM (StrSplit (k',[MChc dim' [] v2]) (j',[])))) =
   let (ms,v) = separateLastMatch s2 s1 v2
   in (genVMatch d i (resolveOverlap 0 (getAltMatches (fromMaybe (0,[]) ma)) ms), PM (StrSplit v (j',[])) (PatSplit (k,m) p))
checkOverlap d i (ma,SM (PatSplit m@(k,[MChc dim v1 []]) p)) (ma',SM (PatSplit m'@(k',[MChc dim' [] v2]) p')) 
   | p == p'          = (genVMatch d i (resolveOverlap 0 (getAltMatches (fromMaybe (0,[]) ma)) (getAltMatches (fromMaybe (0,[]) ma'))), SM (PatSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 v2))) p))
   | otherwise = (genVMatch d i (resolveOverlap 0 (getAltMatches (fromMaybe (0,[]) ma)) (getAltMatches (fromMaybe (0,[]) ma'))),PM (PatSplit m p) (PatSplit m' p'))
checkOverlap d i _ _ = undefined --if PM


getAltMatches :: VMatch -> Matches
getAltMatches (_,[])            = []
getAltMatches (_,[MChc d m []]) = m
getAltMatches (_,[MChc d [] m]) = m
getAltMatches (_,_)             = undefined



genVMatch :: Dim -> Pos -> (Matches,Matches) -> Maybe VMatch
genVMatch d i ([],[])  = Nothing
genVMatch d i (ms,ms') = Just (i,[MChc d ms ms'])

resolveOverlap :: Pos -> Matches -> Matches -> (Matches,Matches)
resolveOverlap _ [] [] = ([],[])
resolveOverlap k ((j,m):ms) []   
   | j >= k    = ((j,m):ms,[])
   | otherwise = resolveOverlap k ms []
resolveOverlap k [] ((j',m'):ms') 
   | j' >= k   = ([],(j',m'):ms')
   | otherwise = resolveOverlap k [] ms'
resolveOverlap k ((j,m):ms) ((j',m'):ms')
   | j < j' && j>= k   = ([(j,m)],[]) `and` resolveOverlap (j+longest m []) ms ((j',m'):ms')
   | j < j' && j< k    = ([],[]) `and` resolveOverlap k ms ((j',m'):ms')
   | j'< j && j'>= k   = ([], [(j',m')]) `and` resolveOverlap (j'+longest m' []) ((j,m):ms) ms'
   | j'< j && j'< k    = ([], []) `and` resolveOverlap k ((j,m):ms) ms'
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
join Nothing      Nothing       = Nothing
join _            Nothing       = Nothing
join Nothing      m             = m
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
appendMatches ms (ms',s) = (removeNoMatch ms ++ removeNoMatch ms',s)

removeNoMatch :: Matches -> Matches
removeNoMatch []          = []
removeNoMatch ((_,[]):ms) = removeNoMatch ms
removeNoMatch (m:ms)      = m : removeNoMatch ms

addMatch :: VMatch -> (Matches,Split) -> (Matches,Split)
addMatch (_,[]) m = m
addMatch m (ms,s) = (m:ms,s)

continue :: Pattern -> Pos -> VString -> Split -> (Matches,Split)
continue p i v (SM (StrSplit m (j,s')))      = addMatch m (scan p (j,s'++v)) --next occurrence
continue p i (s:ss) (SM (PatSplit (j,m) p')) = let (ma,sp) = rigidMatch p (j,m) p' (i,s)
 in addMatch (fromMaybe (j,[]) ma) (continue p (i+(lenIncr s)) ss sp)
continue p i v (PM s1 s2)                    = matchPM p i v (SM s1) (SM s2)
continue _ _ [] s@(SM (PatSplit (j,m) p'))   = ([],s)

matchPM :: Pattern -> Pos -> VString -> Split -> Split -> (Matches, Split)
matchPM origP i ss (SM (StrSplit m (j,s))) (SM (StrSplit m' (j',s')))   = longestMatch origP ss (m,(j,s)) (m',(j',s'))
matchPM origP i ss s1@(SM (StrSplit m (j,[]))) s2@(SM (PatSplit m' p))  = let (ma,sp) = continueInNext origP i ss s2
  in appendMatches ma (getCompleteMatch origP i ss s1 sp)--addMatch m (continue origP i ss (SM $ PatSplit m' p)) --TODO Here check for overlapping or the first match between m and complete m'
matchPM origP i ss s1@(SM (PatSplit m' p )) s2@(SM (StrSplit m (j,[]))) = let (ma,sp) = continueInNext origP i ss s1
  in appendMatches ma (getCompleteMatch origP i ss sp s2)--addMatch m (continue origP i ss (SM $ PatSplit m' p)) --TODO same as above
matchPM origP i (s:ss) (SM(PatSplit (j,m) p )) (SM(PatSplit (j',m') p'))= 
  let (ma,sp)   = rigidMatch origP (j,m) p (i,s)
      (ma',sp') = rigidMatch origP (j',m') p' (i,s)
  in appendMatches [fromMaybe (j,[]) ma,fromMaybe (j',[]) ma'] (getCompleteMatch origP (i+(lenIncr s)) ss sp sp')

longestMatch :: Pattern -> VString -> (VMatch,Input) -> (VMatch,Input) -> (Matches, Split)
longestMatch p ss l@((i,m),(j,s)) r@((i',m'),(j',s')) 
  | i > i'  = addMatch (i',m') (scan p (j',s'++ss))
  | i < i'  = addMatch (i,m) (scan p (j,s++ss))
  | i == i' = firstMatch p ss l r 
  
firstMatch :: Pattern -> VString -> (VMatch,Input) -> (VMatch,Input) -> (Matches, Split)
firstMatch p ss ((_,[]),y) ((_,[]),y') = let (l,vs) = shortestSegment y y'
                                         in  scan p (l,vs++ss)
firstMatch p ss ((_,[]),y) (m,(l,vs))  = addMatch m (scan p (l,vs++ss))
firstMatch p ss (m,(l,vs)) ((_,[]),y)  = addMatch m (scan p (l,vs++ss))
firstMatch p ss l@(x@(i,MChc _ [(k,m)] [] :ms),y@(j,s)) r@(x'@(i',MChc _ [] [(k',m')] :ms'),y'@(j',s'))
  | k > k'  = addMatch x' (scan p (j',s'++ss))
  | k < k'  = addMatch x (scan p (j,s++ss))
  | k == k' = let (l,vs) = shortestSegment y y'
              in appendMatches [x,x'] (scan p (l,vs++ss))
firstMatch _ _ _ _ = undefined

shortestSegment :: Input -> Input -> Input
shortestSegment (j,s) (j',s') 
  | j < j'     = (j',s')
  | otherwise  = (j,s) --both the cases same result
  
getCompleteMatch :: Pattern -> Pos -> VString -> Split -> Split -> (Matches,Split)
getCompleteMatch p i v (PM s1 s2) (PM s1' s2') = undefined
getCompleteMatch p i v NoMatch NoMatch         = ([],SM $ StrSplit (i,[]) (i,[])) --TODO continue, rewind the match from previous segment
getCompleteMatch p i v NoMatch s               = continueInNext p i v s
getCompleteMatch p i v s NoMatch               = continueInNext p i v s
getCompleteMatch p i v s1 s2                   = let (ma,sp) = next s1
                                                     (ma',sp') = next s2
                                                 in appendMatches (ma++ma') (matchPM p i v sp sp')
                                                 where next s = continueInNext p i v s

--Need this because `continue` starts scanning again after the match
continueInNext :: Pattern -> Pos -> VString -> Split -> (Matches,Split)
continueInNext p i v (NoMatch)                     = ([],NoMatch)
continueInNext p i v (SM (StrSplit m (j,s')))      = ([],SM $ StrSplit m (j,s'))
continueInNext p i (s:ss) (SM (PatSplit (j,m) p')) = let (ma,sp) = rigidMatch p (j,m) p' (i,s)
 in addMatch (fromMaybe (j,[]) ma) (continueInNext p (i+(lenIncr s)) ss sp)
continueInNext p i v (PM s1 s2)                    = matchPM p i v (SM s1) (SM s2)
continueInNext _ _ [] s                            = ([],s)


leftMatch :: Dim -> Pos -> Matches -> Split -> Split --TODO use Alt
leftMatch _ _ [] NoMatch             = NoMatch
leftMatch d i ms NoMatch             = SM $ StrSplit (i,[MChc d (ms) []]) (i+1,[])
leftMatch d i ms (SM (StrSplit m s)) = SM $ StrSplit (i,[MChc d (ms++[m]) []]) s
leftMatch d i ms (SM (PatSplit m p)) = SM $ PatSplit (i,[MChc d (ms++[m]) []]) p
leftMatch d i ms (PM s1 s2)          = undefined

rightMatch :: Dim -> Pos -> Matches -> Split -> Split
rightMatch _ _ [] NoMatch             = NoMatch
rightMatch d i ms NoMatch             = SM $ StrSplit (i,[MChc d [] (ms)]) (i+1,[])
rightMatch d i ms (SM (StrSplit m s)) = SM $ StrSplit (i,[MChc d [] (ms++[m])]) s
rightMatch d i ms (SM (PatSplit m p)) = SM $ PatSplit (i,[MChc d [] (ms++[m])]) p
rightMatch d i ms (PM s1 s2)          = undefined

scanChoice :: Dim -> Pos -> (Matches,Split) -> (Matches,Split) -> (Matches,Split)
scanChoice d i ([],NoMatch) ([],NoMatch) = ([],NoMatch) --TODO non empty matches and NoMatch
scanChoice d i (ml,sl) ([],NoMatch)
  | null ml    = ([], leftMatch d i [] sl)
  | otherwise  = let (ms,m) = separateLastMatch sl NoMatch ml
  in ([(i,[MChc d ms []])], leftMatch d i (checkNoMatch m) sl)
scanChoice d i ([],NoMatch) (mr,sr)
  | null mr    = ([], rightMatch d i [] sr)
  | otherwise  = let (ms,m) = separateLastMatch sr NoMatch mr
  in ([(i,[MChc d [] ms])], rightMatch d i (checkNoMatch m) sr)
scanChoice d i (ml,sl) (mr,sr)  =
  let (ms,m)   = separateLastMatch sl sr ml
      (ms',m') = separateLastMatch sl sr mr
      (ma,sp)  = matchChoice d i (Nothing,leftMatch d i (checkNoMatch m) sl) (Nothing,rightMatch d i (checkNoMatch m') sr)
  in (checkNoMatch (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 ms ms')))
    ++  (checkNoMatch $ fromMaybe (0,[]) ma), sp)

checkNoMatch :: VMatch -> Matches
checkNoMatch (0,[]) = []
checkNoMatch v      = [v]

separateLastMatch :: Split -> Split -> Matches -> (Matches,VMatch)
separateLastMatch (SM (PatSplit _ _)) (SM (PatSplit _ _)) ms = (ms,(0,[]))
separateLastMatch _                   (SM (PatSplit _ _)) ms = getLast ms
separateLastMatch _                   _                   ms = (ms,(0,[]))

getLast []  = ([],(0,[]))
getLast [m] = ([],m)
getLast ms  = (ms\\[last ms],last ms)


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
scan p (i,[])   = ([],NoMatch)
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
seq (a:ps) = Seq a (seq ps)

wild :: Pattern
wild = Plain Wild

star :: Pattern -> Pattern
star x = Repeat x 0 Nothing

plus :: Pattern -> Pattern
plus x = Repeat x 1 Nothing

possibly :: Pattern -> Pattern
possibly x = Repeat x 0 (Just 1)

-- examples
--
[a,b,c] = map ch "abc"

ab = seq [a,b]
abc = seq [a,b,c]
_a = Seq (Plain Wild) a

a'b = Alt a b


-- Strings
--
s = "xabyabcz"
