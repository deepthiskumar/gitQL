{-# LANGUAGE ViewPatterns #-}

module VPM where

import Prelude hiding (seq)
import Data.Maybe
import Data.List
import Debug.Trace

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

{- SM - when matching against a plain string, only a pattern split is returned
   PM - When matching against a choice, all the matches can be combined into one
        String split and group all the pattern splits into one. -}
data Split = NoMatch
           | SM SplitTy  --Single Match
           | PM SplitTy [SplitTy] --Parallel Match (SplitStop, SplitContinue)
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
appendVM (p, ms) (p',ms') = (p, combineMStr (ms++ms'))

{-appendM :: [Match] -> [Match] -> [Match]
appendM [] m                          = m
appendM m []                          = m
appendM (MStr s : ms) (MStr s' : ms') = (MStr (s++s')) : appendM ms ms'
appendM (m:ms) (m':ms')               = m:m':appendM ms ms' --TODO choices-}

combineMStr :: [Match] -> [Match]
combineMStr []                              = []
combineMStr [m]                             = [m]
combineMStr ((MStr s) : (MStr s') : ms)     = combineMStr ((MStr (s++s')):ms)
combineMStr (MStr s : m1@(MChc _ _ _ ): ms) = (MStr s) : m1 : combineMStr ms
combineMStr (m1@(MChc _ _ _ ) :ms )         = m1 : combineMStr ms

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
rigidMatch :: Pattern -> VMatch -> Pattern -> (Pos,Segment) -> Split
rigidMatch origP (j,m) p (i,s) = case matchSegment origP p (i,s) of
  NoMatch                    -> let (pos,vstring) = incrPosition (j,(proceed1 $ snd $ mToV (j,m))) 
                                in (SM (StrSplit (0,[]) (pos,vstring++[s]))) --new match
  SM (StrSplit m' (j',s'))   -> (SM $ StrSplit (appendVM (j,m) m') (j',s'))
  SM (PatSplit m' p')        -> (SM (PatSplit (appendVM (j,m) m') p'))
  PM (StrSplit m' (_,[])) ps -> PM (StrSplit (appendVM (j,m) m') (0,[])) ps

--TODO 
incrPosition :: Input -> Input
incrPosition (i,[])              = (i+1,[])
incrPosition (i,v@(Str s :ss))   = (i+1,v)
incrPosition (i,Chc _ [] [] :ss) = (i+1,ss)
incrPosition v@(i,Chc _ _ _:ss)  = v --TODO increment the alternative position

incrStr :: Segment -> Int
incrStr (Str _ )      = 1
incrStr (Chc _ [] []) = 1
incrStr (Chc _ _ _)   = 0

matchSegment :: Pattern -> Pattern -> (Pos,Segment) -> Split
matchSegment origP p (i, Str s')          = matchStr p i s'
matchSegment origP p (i,vs@(Chc d v1 v2)) = matchChoice d i (matchAlt origP d i L p (0,v1))
                                         (matchAlt origP d i R p (0,v2))

matchChoice :: Dim -> Pos -> Split -> Split -> Split
matchChoice d i s NoMatch = s
matchChoice d i NoMatch s = s
matchChoice d i s1 s2     = checkOverlap' d i s1 s2

checkOverlap' :: Dim -> Pos -> Split -> Split -> Split
checkOverlap' d i (SM s1) (SM s2)        = checkOverlap'' (checkOverlap d i s1 s2) []
checkOverlap' d i (SM s1) (PM s2 ps)     = checkOverlap'' (checkOverlap d i s1 s2) ps
checkOverlap' d i (PM s1 ps) (SM s2)     = checkOverlap'' (checkOverlap d i s1 s2) ps
checkOverlap' d i (PM s1 ps) (PM s2 ps') = checkOverlap'' (checkOverlap d i s1 s2) (ps++ps')

checkOverlap'' :: Split -> [SplitTy] -> Split
checkOverlap'' s [] = s
checkOverlap'' (SM s1@(StrSplit _ _)) ps = PM s1 ps
checkOverlap'' (SM s1@(PatSplit _ _)) ps = PM (StrSplit (0,[]) (0,[])) (s1:ps)
checkOverlap'' (PM s ps') ps             = PM s (ps'++ps)

checkOverlap :: Dim -> Pos -> SplitTy -> SplitTy -> Split
checkOverlap d i (StrSplit (k,[MChc dim v1 []]) (j,[])) (StrSplit (k',[MChc dim' [] v2]) (j',[]))
   | j == j'   = (SM (StrSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 v2))) (j+1,[])))
   | otherwise = undefined
checkOverlap d i (s1@(StrSplit (k,[MChc dim v1 []]) (j,[]))) (p1@(PatSplit (k',[MChc dim' [] v2]) p)) =
   PM s1 [p1]
checkOverlap d i (p1@(PatSplit (k,[MChc dim v1 []]) p)) (s1@(StrSplit (k',[MChc dim' [] v2]) (j',[]))) =
   PM s1 [p1]
checkOverlap d i p1@(PatSplit m@(k,[MChc dim v1 []]) p) p2@(PatSplit m'@(k',[MChc dim' [] v2]) p')
     | p == p'   = SM (PatSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 v2))) p)
     | otherwise = PM (StrSplit (0,[]) (0,[])) [p1,p2]
checkOverlap d i p1@(PatSplit m@(0,[]) p) p2@(PatSplit m'@(0,[]) p')
     | p == p'   = SM (PatSplit (0,[]) p)
     | otherwise = PM (StrSplit (0,[]) (0,[])) [p1,p2] 
checkOverlap d i s1 (StrSplit (k',[]) (j',[])) = SM s1
checkOverlap d i (StrSplit (k',[]) (j',[])) s1 = SM s1
checkOverlap d i p1@(PatSplit m@(k,[]) p) p2@(PatSplit m'@(k',[MChc dim' [] v2]) p') = SM p2
checkOverlap d i p1@(PatSplit m@(k,[MChc dim v1 []]) p) p2@(PatSplit m'@(k',[]) p')  = SM p1
checkOverlap d i s@(StrSplit _ _) p@(PatSplit (_,[]) _) = SM s
checkOverlap d i p@(PatSplit (_,[]) _) s@(StrSplit _ _) = SM s
checkOverlap d i s1 s2 = trace (show "Split1 "++show s1 ++ ",Split2 " ++ show s2 ) undefined


{-checkOverlap :: Dim -> Pos -> SplitTy -> SplitTy -> Split
checkOverlap d i (StrSplit (k,[MChc dim v1 []]) (j,[])) (StrSplit (k',[MChc dim' [] v2]) (j',[]))
   | j == j'   = (SM (StrSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 v2))) (j+1,[])))
   | otherwise = undefined
checkOverlap d i (s1@(StrSplit (k,[MChc dim v1 []]) (j,[]))) (p1@(PatSplit (k',[MChc dim' [] v2]) p)) =
   PM s1 [p1]
   --let (ms,v) = separateLastMatch s1 s2 v2
   --in (genVMatch d i (resolveOverlap 0 ms (getAltMatches (fromMaybe (0,[]) ma'))), PM (StrSplit v (j,[])) (PatSplit (k',m) p))
checkOverlap d i (p1@(PatSplit (k,m) p)) (s1@(StrSplit (k',[MChc dim' [] v2]) (j',[]))) =
   PM s1 [p1]
   --let (ms,v) = separateLastMatch s2 s1 v2
   --in (genVMatch d i (resolveOverlap 0 (getAltMatches (fromMaybe (0,[]) ma)) ms), PM (StrSplit v (j',[])) (PatSplit (k,m) p))
checkOverlap d i p1@(PatSplit m@(k,[MChc dim v1 []]) p) p2@(PatSplit m'@(k',[MChc dim' [] v2]) p')
     | p == p'   = SM (PatSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 v2))) p)
     | otherwise = PM (StrSplit (0,[]) (0,[])) [p1,p2] 
checkOverlap d i s1 (StrSplit (k',[]) (j',[])) = SM s1
checkOverlap d i (StrSplit (k',[]) (j',[])) s1 = SM s1
checkOverlap d i p1@(PatSplit m@(k,[]) p) p2@(PatSplit m'@(k',[MChc dim' [] v2]) p') = SM p2
checkOverlap d i p1@(PatSplit m@(k,[MChc dim v1 []]) p) p2@(PatSplit m'@(k',[]) p')  = SM p1
checkOverlap d i s1 s2 = trace (show "Split1 "++show s1 ++ ",Split2 " ++ show s2 ) undefined
-}
{-| p == p'          = (genVMatch d i (resolveOverlap 0 (getAltMatches (fromMaybe (0,[]) ma)) (getAltMatches (fromMaybe (0,[]) ma'))), SM (PatSplit (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 v1 v2))) p))
   | otherwise = (genVMatch d i (resolveOverlap 0 (getAltMatches (fromMaybe (0,[]) ma)) (getAltMatches (fromMaybe (0,[]) ma'))),PM (PatSplit m p) (PatSplit m' p'))
checkOverlap d i _ _ = undefined --if PM-}


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

matchAlt :: Pattern -> Dim -> Pos -> Alt -> Pattern -> Input -> Split
matchAlt origP d i a p (j,[]) = SM (PatSplit (0,[]) p)
matchAlt origP d i a p (j,s:ss) = case matchSegment origP p (j,s) of
  (NoMatch) -> (NoMatch)
  sp        -> let (ma,sp') = continue origP (i+(lenIncr s)) ss sp
               in altSplit a i d ma sp'
   {-@(SM (StrSplit m' (j',s')))                             --let (ms,sp) = scan origP (j',s'++ss)
                                --in altSplit a i d (m':ms) sp --(ma',sp') = altSplit a i d (m':ms) sp
                                   --in (join ma ma',sp')
  (SM (PatSplit m' p'))      -> let (ma,sp) = rigidMatch origP m' p' (i,s)
 in addMatch (fromMaybe (j,[]) ma) (continue p (i+(lenIncr s)) ss sp) 
  (PM s1 s2)                 -> undefined-}

join :: Maybe VMatch -> Maybe VMatch -> Maybe VMatch
join Nothing      Nothing       = Nothing
join _            Nothing       = Nothing
join Nothing      m             = m
join (Just (i,m)) (Just (j,m')) = Just (i,m++m')

altSplit :: Alt -> Pos -> Dim -> Matches -> Split -> Split
altSplit a i d [] NoMatch                     = NoMatch
altSplit a i d ms NoMatch                     = SM $ StrSplit (i,createMChc a d ms) (i+1,[])
altSplit a i d ms (SM (StrSplit m (j,[])))    = SM $ StrSplit (i,createMChc a d (ms++[m])) (i+1,[])
altSplit a i d [] (SM (PatSplit m p))         = SM (PatSplit (i,createMChc a d [m]) p)
altSplit a i d ms (SM (PatSplit m p))         = PM ( StrSplit (i,createMChc a d ms) (0,[])) [(PatSplit (i,createMChc a d [m]) p)]
altSplit a i d ms (PM (StrSplit m (j,[])) s2) = PM ( StrSplit (i,createMChc a d (ms++[m])) (0,[])) s2

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

--TODO 
continue :: Pattern -> Pos -> VString -> Split -> (Matches,Split)
continue p i v (SM (StrSplit m (j,s')))      = addMatch m (scan p (j,s'++v)) --next occurrence
continue p i (s:ss) (SM (PatSplit (j,m) p')) = let sp = rigidMatch p (j,m) p' (i,s)
 in (continue p (i+(lenIncr s)) ss sp) --addMatch (fromMaybe (j,[]) ma) (continue p (i+(lenIncr s)) ss sp)
continue p i v (PM s@(StrSplit (i',[]) (j,s')) s2)              =
  let ms = (map (continueInNext p i v) s2)
      sps = matchPM p (map snd ms)
      ms' = concatMap (\(ma,_) -> (map (\m'->StrSplit m' (0,[])) ma)) ms--TODO check the position of the matches
      (k,s) = getShortestSegment (ms'++ sps)
  in appendMatches (getMatches sps) (scan p (k,s))
continue p i v (PM s@(StrSplit (i',m) (j,s')) s2)               = 
  let ms = (map (continueInNext p i v) s2)
      sps = matchPM p (map snd ms)
      ms' = concatMap (\(ma,_) -> (map (\m'->StrSplit m' (0,[])) ma)) ms--TODO check the position of the matches
      (k,s) = getShortestSegment ((StrSplit (i',m) (j,s'++v)): (ms'++ sps))
      (n,sps') = {-trace (show "Match: "++show (i'+(length m)-1,[last m])++ show "sps: "++ show sps)-} (elimOverlap (Just $StrSplit (i'+(length m)-1,[last m]) (j,s')) sps)
  in appendMatches (getMatches $ getJust (mergeMatch (Just $ StrSplit (i',m\\[(last m)]) (j,s')) n) ++ sps') (scan p (k,s)) --appendMatches (getMatches ((StrSplit m (j,s'++ v)):sps)) (scan p (k,s))
continue _ _ [] s@(SM (PatSplit (j,m) p'))   = ([],s)

--get matches in left or right alternative and compare it with 
elimOverlap :: Maybe SplitTy -> [SplitTy] -> (Maybe SplitTy, [SplitTy])
elimOverlap s [] = (s,[])
elimOverlap Nothing ss = (Nothing,ss)
elimOverlap (Just (StrSplit (i,[MChc d [] []]) (j,v))) x = (Nothing,x)
elimOverlap (Just (StrSplit (i,[MChc d (m1:ms1) x]) (j,v))) (s@(StrSplit (i',(MChc d' [] m'):ms') (j',v')) : ss)=
  let (n,ns) = again (Just $ StrSplit (i,[MChc d (m1:ms1) []]) (j,v)) (Just s)
      (n',ns') = {-trace (show "After: "++ show (n,ns) )-}elimOverlap n ss
  in ({-mergeMatch n -}n',getJust ns ++ ns')
elimOverlap (Just (StrSplit (i,[MChc d x (m1:ms1)]) (j,v))) (s@(StrSplit (i',(MChc d' m' []):ms') (j',v')) : ss)=
  let (n,ns) = again (Just $ StrSplit (i,[MChc d [] (m1:ms1)]) (j,v)) (Just s)
      (n',ns') = elimOverlap n ss
  in ({-mergeMatch n -}n',getJust ns ++ ns')
elimOverlap s (s':ss) = 
  let (n',ns') = {-trace (show s)-} (elimOverlap s ss)
  in (n',s': ns')

--eliminateEmptyMatches :: 

again :: Maybe SplitTy -> Maybe SplitTy -> (Maybe SplitTy, Maybe SplitTy)
again Nothing ss       = (Nothing,ss)
again s Nothing        = (s,Nothing)
again (Just (StrSplit (i,[MChc d (m1:ms1) x]) (j,v))) (Just (StrSplit (i',m@((MChc d' [] [m']):ms')) (j',v'))) = 
  let (a,a') = compare' 0 (m1:ms1) (longest m [],Just m')
  in case (a,a') of
       ([],[]) -> (Nothing,Nothing)
       ([],ma') -> (Nothing,Just $ StrSplit (i',(MChc d' [] ma'):ms') (j',v'))
       (ma,[])  -> (Just (StrSplit (i,[MChc d ma x]) (j,v)),Nothing)
       (ma,ma') -> (Just (StrSplit (i,[MChc d ma x]) (j,v)),Just $ StrSplit (i',(MChc d' [] ma'):ms') (j',v'))
again (Just (StrSplit (i,[MChc d x m1]) (j,v))) (Just (StrSplit (i',m@((MChc d' [m'] []):ms')) (j',v'))) = 
  let (a,a') = compare' 0 (m1) (longest m [],Just m')
  in case (a,a') of
       ([],[]) -> (Nothing,Nothing)
       ([],ma') -> (Nothing,Just $ StrSplit (i',(MChc d' ma' []):ms') (j',v'))
       (ma,[])  -> (Just (StrSplit (i,[MChc d x ma]) (j,v)),Nothing)
       (ma,ma') -> (Just (StrSplit (i,[MChc d x ma]) (j,v)),Just $ StrSplit (i',(MChc d' ma' []):ms') (j',v'))
  {-let (n,n') = getLongestMatch' s s'  
      (ns,ns') = again n ss
  in trace (show "again: "++ show (n,n')) (mergeMatch n ns, getJust n'++ ns')-}
  
--compare which matches are ahead of others
compare' :: Pos -> Matches -> (Int,Maybe VMatch) -> (Matches,Matches)
compare' _ [] (l,Nothing) = ([],[])
compare' k ((j,m):ms) (l,Nothing)   
   | j >= k    = ((j,m):ms,[])
   | otherwise = compare' k ms (l,Nothing)
compare' k [] m@(l,Just (j',m')) 
   | j' >= k   = ([],[(j',m')])
   | otherwise = compare' k [] m
compare' k ((j,m):ms) ma@(l,Just (j',m'))
   | j < j' && j>= k   = ([(j,m)],[]) `and` compare' (j+longest m []) ms ma
   | j < j' && j< k    = ([],[]) `and` compare' k ms ma
   | j'< j && j'>= k   = ([], [(j',m')]) `and` compare' (j'+l) ((j,m):ms) (l,Nothing)
   | j'< j && j'< k    = ([], []) `and` compare' k ((j,m):ms) (l,Nothing)
   | j == j' && j >= k = ([(j,m)],[(j',m')]) `and` compare' (longest m m') ms (l,Nothing)
   where and (m,m') (ms,ms') = (m++ms, m'++ms')

mergeMatch :: Maybe SplitTy -> Maybe SplitTy -> Maybe SplitTy
mergeMatch Nothing s = s
mergeMatch s Nothing = s
mergeMatch (Just (StrSplit (i,m) _)) (Just (StrSplit (i',m') v)) = (Just (StrSplit (i,m++m') v))

matchPM :: Pattern -> [Split] -> [SplitTy]--([SplitTy],[SplitTy])
matchPM origP ps = --getNOM $ segLeftRight ps
  let (ls,rs) = getNOM $ segLeftRight ps
  in ls++rs 

getMatches :: [SplitTy] -> Matches
getMatches [] = []
getMatches ((StrSplit m _):ss) = m : getMatches ss
getMatches (_ : ss ) = getMatches ss

segLeftRight :: [Split] -> ([SplitTy],[SplitTy])
segLeftRight [] = ([],[])
segLeftRight (SM s1@(StrSplit (i,MChc d (v:vs) [] : ms) (j,s)) : ss) =
  let (ls,rs) = segLeftRight ss in (ls, s1:rs)
segLeftRight (SM s1@(StrSplit (i,MChc d [] (v:vs) : ms) (j,s)) : ss) =
  let (ls,rs) = segLeftRight ss in (s1:ls, rs)
segLeftRight (_ : ss) = segLeftRight ss --need complete matches, incomplete matches can be discarded

--get Non overlapping matches [Only StrSplit will reach here]
getNOM :: ([SplitTy],[SplitTy]) -> ([SplitTy],[SplitTy])
getNOM ([],[]) = ([],[])
getNOM (ss,[]) = (ss,[])
getNOM ([],ss) = ([],ss)
getNOM (s:ss, s':ss') = 
  let (ns,ns') = getNOM (ss,ss')
      (n,n') = getLongestMatch' s s'
  in (getJust n ++ ns, getJust n' ++ ns') --Does it require proper sorting technique??

getJust :: Maybe SplitTy -> [SplitTy]
getJust Nothing = []
getJust (Just x) = [x] 

getLongestMatch' :: SplitTy -> SplitTy -> (Maybe SplitTy, Maybe SplitTy)
getLongestMatch' s1@(StrSplit (i,m) (j,v)) s2@(StrSplit (i',m') (j',v')) 
  | i > i' = (Nothing, Just s2)
  | i < i' = (Just s1,Nothing)
  | i == i' = firstMatch' ((i,m),(j,v)) ((i',m'),(j',v'))
  
firstMatch' :: (VMatch,Input) -> (VMatch,Input) -> (Maybe SplitTy, Maybe SplitTy)
firstMatch' ((_,[]),y) ((_,[]),y') = (Nothing,Nothing)--let (l,vs) = shortestSegment y y'
                                     --    in  ([],StrSplit (0,[]) (l,vs))
firstMatch' ((_,[]),y) (m,(l,vs))  = (Nothing, Just $ StrSplit m (l,vs))
firstMatch' (m,(l,vs)) ((_,[]),y)  = (Just $ StrSplit m (l,vs),Nothing)

firstMatch' l@(x@(i,m :ms),y@(j,s)) r@(x'@(i',m':ms'),y'@(j',s')) =
  earliestAlt (getAltPos m) (getAltPos m') l r

getAltPos :: Match -> (Alt,Int)
getAltPos (MChc _ [(k,m)] []) = (L,k)
getAltPos (MChc _ [] [(k,m)]) = (R,k)
getAltPos _                   = undefined

earliestAlt :: (Alt,Int) -> (Alt,Int) ->(VMatch,Input) -> (VMatch,Input) -> (Maybe SplitTy, Maybe SplitTy)
earliestAlt (L,k)  (L,k') l@(x@(i,m),y@(j,s)) r@(x'@(i',m'),y'@(j',s')) = undefined
earliestAlt (R,k)  (R,k') l@(x@(i,m),y@(j,s)) r@(x'@(i',m'),y'@(j',s')) = undefined
earliestAlt (_,k)  (_,k') l@(x@(i,m),y@(j,s)) r@(x'@(i',m'),y'@(j',s')) = 
   case resolveOverlap 0 [x] [x'] of
    ([],[])     -> (Nothing, Nothing)
    ([ma],[])   -> (Just (StrSplit ma (j,s)), Nothing)
    ([],[ma])   -> (Nothing, Just (StrSplit ma (j',s')))
    ([ma],[ma'])-> (Just (StrSplit ma (j,s)), Just (StrSplit ma' (j',s')))

{-
  | k > k'  = (Nothing,Just (StrSplit x' (j',s')))
  | k < k'  = (Just (StrSplit x (j,s)),Nothing)
  | k == k' = (Just (StrSplit x (j,s)),Just (StrSplit x' (j',s')))
  --let (l,vs) = shortestSegment y y'
              --in ([x,x'],StrSplit (0,[]) (l,vs)) 
-}

getShortestSegment :: [SplitTy] -> Input
getShortestSegment [] = (0,[])
getShortestSegment (StrSplit m (i,v) : ss) = shortestSegment (i,v) (getShortestSegment ss)
  

{-matchPM :: Pattern -> Pos -> VString -> Split -> Split -> (Matches, Split)
matchPM origP i ss (SM (StrSplit m (j,s))) (SM (StrSplit m' (j',s')))   = longestMatch origP ss (m,(j,s)) (m',(j',s'))
matchPM origP i ss s1@(SM (StrSplit m (j,[]))) s2@(SM (PatSplit m' p))  = let (ma,sp) = continueInNext origP i ss s2
  in appendMatches ma (getCompleteMatch origP i ss s1 sp)--addMatch m (continue origP i ss (SM $ PatSplit m' p)) --TODO Here check for overlapping or the first match between m and complete m'
matchPM origP i ss s1@(SM (PatSplit m' p )) s2@(SM (StrSplit m (j,[]))) = let (ma,sp) = continueInNext origP i ss s1
  in appendMatches ma (getCompleteMatch origP i ss sp s2)--addMatch m (continue origP i ss (SM $ PatSplit m' p)) --TODO same as above
matchPM origP i (s:ss) (SM(PatSplit (j,m) p )) (SM(PatSplit (j',m') p'))= 
  let (ma,sp)   = rigidMatch origP (j,m) p (i,s)
      (ma',sp') = rigidMatch origP (j',m') p' (i,s)
  in appendMatches [fromMaybe (j,[]) ma,fromMaybe (j',[]) ma'] (getCompleteMatch origP (i+(lenIncr s)) ss sp sp')-}

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
  
{-getCompleteMatch :: Pattern -> Pos -> VString -> Split -> Split -> (Matches,Split)
getCompleteMatch p i v (PM s1 s2) (PM s1' s2') = undefined
getCompleteMatch p i v NoMatch NoMatch         = ([],SM $ StrSplit (i,[]) (i,[])) --TODO continue, rewind the match from previous segment
getCompleteMatch p i v NoMatch s               = continueInNext p i v s
getCompleteMatch p i v s NoMatch               = continueInNext p i v s
getCompleteMatch p i v s1 s2                   = let (ma,sp) = next s1
                                                     (ma',sp') = next s2
                                                 in appendMatches (ma++ma') (matchPM p i v sp sp')
                                                 where next s = continueInNext p i v s-}

--Need this because `continue` starts scanning again after the match
continueInNext :: Pattern -> Pos -> VString -> SplitTy -> (Matches,Split)
continueInNext p i [] s = ([],SM s)
continueInNext p i v (StrSplit m (j,s'))      = ([],SM $ StrSplit m (j,s'))
continueInNext p i (s:ss) (PatSplit (j,m) p') = let sp = rigidMatch p (j,m) p' (i,s)
 in case sp of 
      SM s1 -> (continueInNext p (i+(lenIncr s)) ss s1)
      pm    -> continue p i ss pm


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
      sp  = matchChoice d i (leftMatch d i (checkNoMatch m) sl) (rightMatch d i (checkNoMatch m') sr)
  in (checkNoMatch (fromMaybe (i,[]) (genVMatch d i (resolveOverlap 0 ms ms'))), sp)

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

-- predicates
--
isWild :: Atomic -> Bool
isWild Wild = True
isWild _ = False

isChar :: Atomic -> Bool
isChar (C _) = True
isChar _ = False


isPlain :: Pattern -> Bool
isPlain (Plain _) = True
isPlain _ = False

isSeq :: Pattern -> Bool
isSeq (Seq _ _) = True
isSeq _ = False

isAlt :: Pattern -> Bool
isAlt (Alt _ _) = True
isAlt _ = False

isRepeat :: Pattern -> Bool
isRepeat (Repeat _ _ _) = True
isRepeat _ = False

isNone :: Pattern -> Bool
isNone None = True
isNone _ = False


charMatch :: Atomic -> Char -> Bool
charMatch Wild _ = True
charMatch (C x) y = x == y

repeatMatch :: Int -> Maybe Int -> Int -> Bool
repeatMatch min (Just max) x = x >= min && x <= max
repeatMatch min Nothing x = x >= min

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
