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
             | PChc DimTy Pattern Pattern
             | Repeat Pattern Int {-min-} (Maybe Int) {-max-}
             --Ex. `a*` => `Repeat (ch 'a') 0 None`
             | None --Ex. `(a|)b` => `Seq (Alt (ch 'a') None) (ch 'b')
             deriving(Show,Eq)

type DimVarName = String
data DimTy = D Dim | DVar DimVarName deriving(Show,Eq)
type VString = [Segment]

type Dim = Int

data Segment = Str String | Chc Dim VString VString
             deriving(Show, Eq)

--Each match could be have multiple dimensions in it.
-- Eg: d<abc,xby> # A<a,x>bB<c,y> --> A<a,x>bB<c,y> | d = {A,B}
--Therefore we have a list of dimensions instead of just

type DimVars = Maybe (DimVarName,[Dim])

data VMatch = VM [Dim] Pos [Match] deriving(Show,Eq)
--data VMatch = VM DimVars Pos [Match] deriving(Show,Eq)

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
mToV (VM ds i [] )                 = (i,[])
mToV (VM ds i (MStr s : ss))       = (i,Str s : (snd $ mToV (VM ds i ss)))
mToV (VM ds i (MChc d v1 v2 : ss)) = (i,(Chc d (snd $ mlistToV v1 ) (snd $ mlistToV v2 ))
                                    : (snd $ mToV (VM ds i ss)))

proceed1 :: VString -> VString
proceed1 []                 = []
proceed1 (Str [] : ss)      = ss
proceed1 (Str (c:s) : ss)   = (vStr s) ++ ss
proceed1 (Chc d v1 v2 : ss) = Chc d (proceed1 v1) (proceed1 v2) : ss
--
appendVM :: VMatch -> VMatch -> VMatch
appendVM (VM ds p ms) (VM ds' p' ms') = (VM (ds++ds') p (combineMStr (ms++ms')))

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
--
orElse :: Split -> Split -> Split
orElse NoMatch s   = s
orElse s _         = s --TODO return both the matches like the unix grep?
--
andThen :: Split -> Split-> Split
andThen NoMatch _                                 = NoMatch
andThen _       NoMatch                           = NoMatch
andThen (SM (StrSplit m s)) (SM (PatSplit m' p))  = SM (PatSplit (appendVM m m') p)
andThen (SM (StrSplit m s)) (SM (StrSplit m' s')) = SM (StrSplit (appendVM m m') s')
andThen _                   _                     = undefined

and' :: Split -> Split -> Split
and' NoMatch _ = NoMatch
and' _ NoMatch = NoMatch
and' (SM (StrSplit m s)) (SM (StrSplit m' s')) 
  | s == s' && m==m'    = SM (StrSplit m' s')
  | otherwise           = NoMatch
and' (SM (PatSplit m p)) (SM (PatSplit m' p'))
  | m == m'             = SM (PatSplit m' (PChc (D 0) p p'))
  | otherwise           = NoMatch
and' _ _                = NoMatch

--
matchStr :: Pattern -> Pos -> String -> Split
matchStr (Plain (C d)) i (c:s)
                     | c==d         = SM $ StrSplit (VM [] i [MStr[c]]) (i+1,vStr s)
matchStr (Plain Wild) i (c:s)       = SM $ StrSplit (VM [] i [MStr[c]]) (i+1,vStr s)
matchStr (Seq a p)    i (c:[])      = matchStr a i [c] `andThen` (SM (PatSplit (VM [] 0 []) p))
matchStr (Seq a p)    i (c:s)       = matchStr a i [c] `andThen` matchStr p (i+1) s
matchStr (Alt p q)    i s           = matchStr p i s `orElse` matchStr q i s
matchStr (PChc (_) p q) i s         = matchStr p i s `and'` matchStr q i s
matchStr _          _   _           = NoMatch
--
--To Match the first character of the next segment and not scan
rigidMatch :: Pattern -> VMatch -> Pattern -> (Pos,Segment) -> Split
rigidMatch origP (VM ds j m) p (i,s) = case matchSegment origP p (i,s) of
  NoMatch                    -> let (pos,vstring) = incrPosition (j,(proceed1 $ snd $ mToV (VM ds j m))) 
                                in (SM (StrSplit (VM ds 0 []) (pos,vstring++[s]))) --new match
  SM (StrSplit m' (j',s'))   -> (SM $ StrSplit (appendVM (VM ds j m) m') (j',s'))
  SM (PatSplit m' p')        -> (SM (PatSplit (appendVM (VM ds j m) m') p'))
  PM (StrSplit m' (_,[])) ps -> PM (StrSplit (appendVM (VM ds j m) m') (0,[])) ps
--
--TODO 
incrPosition :: Input -> Input
incrPosition (i,[])              = (i+1,[])
incrPosition (i,v@(Str s :ss))   = (i+1,v)
incrPosition (i,Chc _ [] [] :ss) = (i+1,ss)
incrPosition v@(i,Chc _ _ _:ss)  = v --TODO increment the alternative position

{-incrStr :: Segment -> Int
incrStr (Str _ )      = 1
incrStr (Chc _ [] []) = 1
incrStr (Chc _ _ _)   = 0-}
--
matchSegment :: Pattern -> Pattern -> (Pos,Segment) -> Split
matchSegment oriiP pat@(PChc d p q) inp   = snd $ scanChoicePattern pat inp --undefined --TODO use scanChoicePattern and see if that is enough
matchSegment origP p (i, Str s')          = matchStr p i s'
matchSegment origP p (i,vs@(Chc d v1 v2)) = matchChoice d i (matchAlt origP d i L p (0,v1))
                                         (matchAlt origP d i R p (0,v2))
--
matchChoice :: Dim -> Pos -> Split -> Split -> Split
matchChoice d i s NoMatch = s
matchChoice d i NoMatch s = s
matchChoice d i s1 s2     = checkOverlap' d i s1 s2
--
checkOverlap' :: Dim -> Pos -> Split -> Split -> Split
checkOverlap' d i (SM s1) (SM s2)        = checkOverlap'' (checkOverlap d i s1 s2) []
checkOverlap' d i (SM s1) (PM s2 ps)     = checkOverlap'' (checkOverlap d i s1 s2) ps
checkOverlap' d i (PM s1 ps) (SM s2)     = checkOverlap'' (checkOverlap d i s1 s2) ps
checkOverlap' d i (PM s1 ps) (PM s2 ps') = checkOverlap'' (checkOverlap d i s1 s2) (ps++ps')
--
checkOverlap'' :: Split -> [SplitTy] -> Split
checkOverlap'' s [] = s
checkOverlap'' (SM s1@(StrSplit _ _)) ps = PM s1 ps
checkOverlap'' (SM s1@(PatSplit _ _)) ps = PM (StrSplit (VM [] 0 []) (0,[])) (s1:ps)
checkOverlap'' (PM s ps') ps             = PM s (ps'++ps)

checkOverlap :: Dim -> Pos -> SplitTy -> SplitTy -> Split
checkOverlap d i (StrSplit (VM ds k [MChc dim v1 []]) (j,[])) (StrSplit (VM ds' k' [MChc dim' [] v2]) (j',[]))
   | j == j'   = (SM (StrSplit (fromMaybe (VM ds i []) (genVMatch d [] i (resolveOverlap 0 v1 v2))) (j+1,[])))
   | otherwise = undefined
checkOverlap d i (s1@(StrSplit (VM ds k [MChc dim v1 []]) (j,[]))) (p1@(PatSplit (VM ds' k' [MChc dim' [] v2]) p)) =
   PM s1 [p1]
checkOverlap d i (p1@(PatSplit (VM ds k [MChc dim v1 []]) p)) (s1@(StrSplit (VM ds' k' [MChc dim' [] v2]) (j',[]))) =
   PM s1 [p1]
checkOverlap d i p1@(PatSplit m@(VM ds k [MChc dim v1 []]) p) p2@(PatSplit m'@(VM ds' k' [MChc dim' [] v2]) p')
     | p == p'   = SM (PatSplit (fromMaybe (VM ds i []) (genVMatch d [] i (resolveOverlap 0 v1 v2))) p)
     | otherwise = PM (StrSplit (VM ds 0 []) (0,[])) [p1,p2]
checkOverlap d i p1@(PatSplit m@(VM ds 0 []) p) p2@(PatSplit m'@(VM ds' 0 []) p')
     | p == p'   = SM (PatSplit (VM ds 0 []) p)
     | otherwise = PM (StrSplit (VM ds 0 []) (0,[])) [p1,p2] 
checkOverlap d i s1 (StrSplit (VM ds' k' []) (j',[])) = SM s1
checkOverlap d i (StrSplit (VM ds' k' []) (j',[])) s1 = SM s1
checkOverlap d i p1@(PatSplit m@(VM ds k []) p) p2@(PatSplit m'@(VM ds' k' [MChc dim' [] v2]) p') = SM p2
checkOverlap d i p1@(PatSplit m@(VM ds k [MChc dim v1 []]) p) p2@(PatSplit m'@(VM ds' k' []) p')  = SM p1
checkOverlap d i s@(StrSplit _ _) p@(PatSplit (VM ds _ []) _) = SM s
checkOverlap d i p@(PatSplit (VM ds _ []) _) s@(StrSplit _ _) = SM s
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


{-getAltMatches :: VMatch -> Matches
getAltMatches (_,[])            = []
getAltMatches (_,[MChc d m []]) = m
getAltMatches (_,[MChc d [] m]) = m
getAltMatches (_,_)             = undefined-}


--
genVMatch :: Dim -> [Dim] -> Pos -> (Matches,Matches) -> Maybe VMatch
genVMatch d _ i ([],[])  = Nothing
genVMatch d ds i (ms,ms') = Just (VM ds i [MChc d ms ms'])
--
resolveOverlap :: Pos -> Matches -> Matches -> (Matches,Matches)
resolveOverlap _ [] [] = ([],[])
resolveOverlap k ((VM ds j m):ms) []   
   | j >= k    = ((VM ds j m):ms,[])
   | otherwise = resolveOverlap k ms []
resolveOverlap k [] ((VM ds' j' m'):ms') 
   | j' >= k   = ([],(VM ds' j' m'):ms')
   | otherwise = resolveOverlap k [] ms'
resolveOverlap k ((VM ds j m):ms) ((VM ds' j' m'):ms')
   | j < j' && j>= k   = ([(VM ds j m)],[]) `and` resolveOverlap (j+longest m []) ms ((VM ds' j' m'):ms')
   | j < j' && j< k    = ([],[]) `and` resolveOverlap k ms ((VM ds' j' m'):ms')
   | j'< j && j'>= k   = ([], [(VM ds' j' m')]) `and` resolveOverlap (j'+longest m' []) ((VM ds j m):ms) ms'
   | j'< j && j'< k    = ([], []) `and` resolveOverlap k ((VM ds j m):ms) ms'
   | j == j' && j >= k = ([(VM ds j m)],[(VM ds' j' m')]) `and` resolveOverlap (longest m m') ms ms'
   where and (m,m') (ms,ms') = (m++ms, m'++ms')
--
longest :: [Match] -> [Match] -> Int
longest v1 v2 = let l = sum (map (lenIncrM) v1)
                    r = sum (map (lenIncrM) v2)
                in max l r
--
lenIncrM :: Match -> Int
lenIncrM (MStr s)     = length s
lenIncrM (MChc _ _ _) = 1

--TODO[1] the position of the remaining segments (j) in the alternative is lost.
-- to track this, we either have to introduce Pos in VString or match the
-- remaining alternatives with the original patten again (not the Split pattern)
{-inLeft :: Dim -> Pos -> Split ->  Split
inLeft d i (SM (StrSplit m (j,s))) = SM (StrSplit m (i,[Chc d s []]))
inLeft d i s                   = s

inRight :: Dim -> Pos -> Split ->  Split
inRight d i (SM (StrSplit m (j,s))) = SM (StrSplit m (i,[Chc d [] s]))
inRight d i s                   = s-}
--
matchAlt :: Pattern -> Dim -> Pos -> Alt -> Pattern -> Input -> Split
matchAlt origP d i a p (j,[]) = SM (PatSplit (VM [] 0 []) p)
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

{-join :: Maybe VMatch -> Maybe VMatch -> Maybe VMatch
join Nothing      Nothing       = Nothing
join _            Nothing       = Nothing
join Nothing      m             = m
join (Just (i,m)) (Just (j,m')) = Just (i,m++m')-}
--
altSplit :: Alt -> Pos -> Dim -> Matches -> Split -> Split
altSplit a i d [] NoMatch                     = NoMatch
altSplit a i d ms NoMatch                     = SM $ StrSplit (VM [] i (createMChc a d ms)) (i+1,[]) --TODO check ds
altSplit a i d ms (SM (StrSplit m (j,[])))    = SM $ StrSplit (VM [] i (createMChc a d (ms++[m]))) (i+1,[])
altSplit a i d [] (SM (PatSplit m p))         = SM (PatSplit (VM [] i (createMChc a d [m])) p)
altSplit a i d ms (SM (PatSplit m p))         = PM ( StrSplit (VM [] i (createMChc a d ms)) (0,[])) [(PatSplit (VM [] i (createMChc a d [m])) p)]
altSplit a i d ms (PM (StrSplit m (j,[])) s2) = PM ( StrSplit (VM [] i (createMChc a d (ms++[m]))) (0,[])) s2
--
createMChc :: Alt -> Dim -> [VMatch] -> [Match]
createMChc a d [] = []
createMChc L d vs = [MChc d vs []]
createMChc R d vs = [MChc d [] vs]
--
appendMatches :: Matches -> (Matches,Split) -> (Matches,Split)
appendMatches ms (ms',s) = (removeNoMatch ms ++ removeNoMatch ms',s)
--
removeNoMatch :: Matches -> Matches
removeNoMatch []               = []
removeNoMatch ((VM _ _ []):ms) = removeNoMatch ms
removeNoMatch (m:ms)           = m : removeNoMatch ms
--
addMatch :: VMatch -> (Matches,Split) -> (Matches,Split)
addMatch (VM _ _ []) m = m
addMatch m (ms,s) = (m:ms,s)
--
--TODO 
continue :: Pattern -> Pos -> VString -> Split -> (Matches,Split)
continue p i v (SM (StrSplit m (j,s')))      = addMatch m (scan p (j,s'++v)) --next occurrence
continue p i (s:ss) (SM (PatSplit (VM ds j m) p')) = let sp = rigidMatch p (VM ds j m) p' (i,s)
 in (continue p (i+(lenIncr s)) ss sp) --addMatch (fromMaybe (j,[]) ma) (continue p (i+(lenIncr s)) ss sp)
continue p i v (PM s@(StrSplit (VM ds i' []) (j,s')) s2)              =
  let ms = (map (continueInNext p i v) s2)
      sps = matchPM p (map snd ms)
      ms' = concatMap (\(ma,_) -> (map (\m'->StrSplit m' (0,[])) ma)) ms--TODO check the position of the matches
      (k,s) = getShortestSegment (ms'++ sps)
  in appendMatches (getMatches sps) (scan p (k,s))
continue p i v (PM s@(StrSplit (VM ds i' m) (j,s')) s2)               = 
  let ms = (map (continueInNext p i v) s2)
      sps = matchPM p (map snd ms)
      ms' = concatMap (\(ma,_) -> (map (\m'->StrSplit m' (0,[])) ma)) ms--TODO check the position of the matches
      (k,s) = getShortestSegment ((StrSplit (VM ds i' m) (j,s'++v)): (ms'++ sps))
      (n,sps') = {-trace (show "Match: "++show (i'+(length m)-1,[last m])++ show "sps: "++ show sps)-} (elimOverlap (Just $StrSplit (VM ds (i'+(length m)-1) [last m]) (j,s')) sps)
  in appendMatches (getMatches $ getJust (mergeMatch (Just $ StrSplit (VM ds i' (m\\[(last m)])) (j,s')) n) ++ sps') (scan p (k,s)) --appendMatches (getMatches ((StrSplit m (j,s'++ v)):sps)) (scan p (k,s))
continue _ _ [] s@(SM (PatSplit (VM ds j m) p'))   = ([],s)
--
--get matches in left or right alternative and compare it with 
elimOverlap :: Maybe SplitTy -> [SplitTy] -> (Maybe SplitTy, [SplitTy])
elimOverlap s [] = (s,[])
elimOverlap Nothing ss = (Nothing,ss)
elimOverlap (Just (StrSplit (VM ds i [MChc d [] []]) (j,v))) x = (Nothing,x)
elimOverlap (Just (StrSplit (VM ds i [MChc d (m1:ms1) x]) (j,v))) (s@(StrSplit (VM ds' i' ((MChc d' [] m'):ms')) (j',v')) : ss)=
  let (n,ns) = again (Just $ StrSplit (VM ds i [MChc d (m1:ms1) []]) (j,v)) (Just s)
      (n',ns') = {-trace (show "After: "++ show (n,ns) )-}elimOverlap n ss
  in ({-mergeMatch n -}n',getJust ns ++ ns')
elimOverlap (Just (StrSplit (VM ds i [MChc d x (m1:ms1)]) (j,v))) (s@(StrSplit (VM ds' i' ((MChc d' m' []):ms')) (j',v')) : ss)=
  let (n,ns) = again (Just $ StrSplit (VM ds i [MChc d [] (m1:ms1)]) (j,v)) (Just s)
      (n',ns') = elimOverlap n ss
  in ({-mergeMatch n -}n',getJust ns ++ ns')
elimOverlap s (s':ss) = 
  let (n',ns') = {-trace (show s)-} (elimOverlap s ss)
  in (n',s': ns')

--eliminateEmptyMatches :: 
--
again :: Maybe SplitTy -> Maybe SplitTy -> (Maybe SplitTy, Maybe SplitTy)
again Nothing ss       = (Nothing,ss)
again s Nothing        = (s,Nothing)
again (Just (StrSplit (VM ds i ([MChc d (m1:ms1) x])) (j,v))) (Just (StrSplit (VM ds' i' m@((MChc d' [] [m']):ms')) (j',v'))) = 
  let (a,a') = compare' 0 (m1:ms1) (longest m [],Just m')
  in case (a,a') of
       ([],[]) -> (Nothing,Nothing)
       ([],ma') -> (Nothing,Just $ StrSplit (VM ds' i' ((MChc d' [] ma'):ms')) (j',v'))
       (ma,[])  -> (Just (StrSplit (VM ds i ([MChc d ma x])) (j,v)),Nothing)
       (ma,ma') -> (Just (StrSplit (VM ds i ([MChc d ma x])) (j,v)),Just $ StrSplit (VM ds' i' ((MChc d' [] ma'):ms')) (j',v'))
again (Just (StrSplit (VM ds i [MChc d x m1]) (j,v))) (Just (StrSplit (VM ds' i' m@((MChc d' [m'] []):ms')) (j',v'))) = 
  let (a,a') = compare' 0 (m1) (longest m [],Just m')
  in case (a,a') of
       ([],[]) -> (Nothing,Nothing)
       ([],ma') -> (Nothing,Just $ StrSplit (VM ds' i' ((MChc d' ma' []):ms')) (j',v'))
       (ma,[])  -> (Just (StrSplit (VM ds i [MChc d x ma]) (j,v)),Nothing)
       (ma,ma') -> (Just (StrSplit (VM ds i [MChc d x ma]) (j,v)),Just $ StrSplit (VM ds' i' ((MChc d' ma' []):ms')) (j',v'))
  {-let (n,n') = getLongestMatch' s s'  
      (ns,ns') = again n ss
  in trace (show "again: "++ show (n,n')) (mergeMatch n ns, getJust n'++ ns')-}
  
--compare which matches are ahead of others
compare' :: Pos -> Matches -> (Int,Maybe VMatch) -> (Matches,Matches)
compare' _ [] (l,Nothing) = ([],[])
compare' k ((VM ds j m):ms) (l,Nothing)   
   | j >= k    = ((VM ds j m):ms,[])
   | otherwise = compare' k ms (l,Nothing)
compare' k [] m@(l,Just (VM ds j' m')) 
   | j' >= k   = ([],[(VM ds j' m')])
   | otherwise = compare' k [] m
compare' k ((VM ds j m):ms) ma@(l,Just (VM ds' j' m'))
   | j < j' && j>= k   = ([(VM ds j m)],[]) `and` compare' (j+longest m []) ms ma
   | j < j' && j< k    = ([],[]) `and` compare' k ms ma
   | j'< j && j'>= k   = ([], [(VM ds' j' m')]) `and` compare' (j'+l) ((VM ds j m):ms) (l,Nothing)
   | j'< j && j'< k    = ([], []) `and` compare' k ((VM ds j m):ms) (l,Nothing)
   | j == j' && j >= k = ([(VM ds j m)],[(VM ds' j' m')]) `and` compare' (longest m m') ms (l,Nothing)
   where and (m,m') (ms,ms') = (m++ms, m'++ms')
--
mergeMatch :: Maybe SplitTy -> Maybe SplitTy -> Maybe SplitTy
mergeMatch Nothing s = s
mergeMatch s Nothing = s
mergeMatch (Just (StrSplit (VM ds i m) _)) (Just (StrSplit (VM ds' i' m') v)) = 
  (Just (StrSplit (VM (ds++ds') i (m++m')) v))
--
matchPM :: Pattern -> [Split] -> [SplitTy]--([SplitTy],[SplitTy])
matchPM origP ps = --getNOM $ segLeftRight ps
  let (ls,rs) = getNOM $ segLeftRight ps
  in ls++rs 
--
getMatches :: [SplitTy] -> Matches
getMatches [] = []
getMatches ((StrSplit m _):ss) = m : getMatches ss
getMatches (_ : ss ) = getMatches ss
--
segLeftRight :: [Split] -> ([SplitTy],[SplitTy])
segLeftRight [] = ([],[])
segLeftRight (SM s1@(StrSplit (VM ds i (MChc d (v:vs) [] : ms)) (j,s)) : ss) =
  let (ls,rs) = segLeftRight ss in (ls, s1:rs)
segLeftRight (SM s1@(StrSplit (VM ds i (MChc d [] (v:vs) : ms)) (j,s)) : ss) =
  let (ls,rs) = segLeftRight ss in (s1:ls, rs)
segLeftRight (_ : ss) = segLeftRight ss --need complete matches, incomplete matches can be discarded
--
--get Non overlapping matches [Only StrSplit will reach here]
getNOM :: ([SplitTy],[SplitTy]) -> ([SplitTy],[SplitTy])
getNOM ([],[]) = ([],[])
getNOM (ss,[]) = (ss,[])
getNOM ([],ss) = ([],ss)
getNOM (s:ss, s':ss') = 
  let (ns,ns') = getNOM (ss,ss')
      (n,n') = getLongestMatch' s s'
  in (getJust n ++ ns, getJust n' ++ ns') --Does it require proper sorting technique??
--
getJust :: Maybe SplitTy -> [SplitTy]
getJust Nothing = []
getJust (Just x) = [x] 
--
getLongestMatch' :: SplitTy -> SplitTy -> (Maybe SplitTy, Maybe SplitTy)
getLongestMatch' s1@(StrSplit (VM ds i m) (j,v)) s2@(StrSplit (VM ds' i' m') (j',v')) 
  | i > i' = (Nothing, Just s2)
  | i < i' = (Just s1,Nothing)
  | i == i' = firstMatch' ((VM ds i m),(j,v)) ((VM ds' i' m'),(j',v'))
--  
firstMatch' :: (VMatch,Input) -> (VMatch,Input) -> (Maybe SplitTy, Maybe SplitTy)
firstMatch' ((VM ds _ []),y) ((VM ds' _ []),y') = (Nothing,Nothing)--let (l,vs) = shortestSegment y y'
                                     --    in  ([],StrSplit (0,[]) (l,vs))
firstMatch' ((VM ds _ []),y) (m,(l,vs))  = (Nothing, Just $ StrSplit m (l,vs))
firstMatch' (m,(l,vs)) ((VM ds _ []),y)  = (Just $ StrSplit m (l,vs),Nothing)

firstMatch' l@(x@(VM ds i (m :ms)),y@(j,s)) r@(x'@(VM ds' i' (m':ms')),y'@(j',s')) =
  earliestAlt (getAltPos m) (getAltPos m') l r
--
getAltPos :: Match -> (Alt,Int)
getAltPos (MChc _ [(VM ds k m)] []) = (L,k)
getAltPos (MChc _ [] [(VM ds k m)]) = (R,k)
getAltPos _                   = undefined
--
earliestAlt :: (Alt,Int) -> (Alt,Int) ->(VMatch,Input) -> (VMatch,Input) -> (Maybe SplitTy, Maybe SplitTy)
earliestAlt (L,k)  (L,k') l@(x@(VM ds i m),y@(j,s)) r@(x'@(VM ds' i' m'),y'@(j',s')) = undefined
earliestAlt (R,k)  (R,k') l@(x@(VM ds i m),y@(j,s)) r@(x'@(VM ds' i' m'),y'@(j',s')) = undefined
earliestAlt (_,k)  (_,k') l@(x@(VM ds i m),y@(j,s)) r@(x'@(VM ds' i' m'),y'@(j',s')) = 
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
--
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

{-longestMatch :: Pattern -> VString -> (VMatch,Input) -> (VMatch,Input) -> (Matches, Split)
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
-}
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
--
--Need this because `continue` starts scanning again after the match
continueInNext :: Pattern -> Pos -> VString -> SplitTy -> (Matches,Split)
continueInNext p i [] s = ([],SM s)
continueInNext p i v (StrSplit m (j,s'))      = ([],SM $ StrSplit m (j,s'))
continueInNext p i (s:ss) (PatSplit (VM ds j m) p') = let sp = rigidMatch p (VM ds j m) p' (i,s)
 in case sp of
      SM s1   -> (continueInNext p (i+(lenIncr s)) ss s1)
      pm      -> continue p i ss pm

--
leftMatch :: Dim -> Pos -> Matches -> Split -> Split --TODO use Alt
leftMatch _ _ [] NoMatch             = NoMatch
leftMatch d i ms NoMatch             = SM $ StrSplit (VM [] i [MChc d (ms) []]) (i+1,[])
leftMatch d i ms (SM (StrSplit m s)) = SM $ StrSplit (VM [] i [MChc d (ms++[m]) []]) s
leftMatch d i ms (SM (PatSplit m p)) = SM $ PatSplit (VM [] i [MChc d (ms++[m]) []]) p
leftMatch d i ms (PM s1 s2)          = undefined
--
rightMatch :: Dim -> Pos -> Matches -> Split -> Split
rightMatch _ _ [] NoMatch             = NoMatch
rightMatch d i ms NoMatch             = SM $ StrSplit (VM [] i [MChc d [] (ms)]) (i+1,[])
rightMatch d i ms (SM (StrSplit m s)) = SM $ StrSplit (VM [] i [MChc d [] (ms++[m])]) s
rightMatch d i ms (SM (PatSplit m p)) = SM $ PatSplit (VM [] i [MChc d [] (ms++[m])]) p
rightMatch d i ms (PM s1 s2)          = undefined

--
scanChoice :: Dim -> Pos -> (Matches,Split) -> (Matches,Split) -> (Matches,Split)
scanChoice d i ([],NoMatch) ([],NoMatch) = ([],NoMatch) --TODO non empty matches and NoMatch
scanChoice d i (ml,sl) ([],NoMatch)
  | null ml    = ([], leftMatch d i [] sl)
  | otherwise  = let (ms,m) = separateLastMatch sl NoMatch ml
  in ([(VM [] i [MChc d ms []])], leftMatch d i (checkNoMatch m) sl)
scanChoice d i ([],NoMatch) (mr,sr)
  | null mr    = ([], rightMatch d i [] sr)
  | otherwise  = let (ms,m) = separateLastMatch sr NoMatch mr
  in ([(VM [] i [MChc d [] ms])], rightMatch d i (checkNoMatch m) sr)
scanChoice d i (ml,sl) (mr,sr)  =
  let (ms,m)   = separateLastMatch sl sr ml
      (ms',m') = separateLastMatch sl sr mr
      sp  = matchChoice d i (leftMatch d i (checkNoMatch m) sl) (rightMatch d i (checkNoMatch m') sr)
  in (checkNoMatch (fromMaybe (VM [] i []) (genVMatch d [] i (resolveOverlap 0 ms ms'))), sp)
--
checkNoMatch :: VMatch -> Matches
checkNoMatch (VM _ 0 []) = []
checkNoMatch v      = [v]
--
separateLastMatch :: Split -> Split -> Matches -> (Matches,VMatch)
separateLastMatch (SM (PatSplit _ _)) (SM (PatSplit _ _)) ms = (ms,(VM [] 0 []))
separateLastMatch _                   (SM (PatSplit _ _)) ms = getLast ms
separateLastMatch _                   _                   ms = (ms,(VM [] 0 []))
--
getLast []  = ([],(VM [] 0 []))
getLast [m] = ([],m)
getLast ms  = (ms\\[last ms],last ms)

--3
scanStr :: Pattern -> (Pos,String) -> Split
scanStr p (i,matchStr p i-> SM s)     = SM s
scanStr p (i,matchStr p i-> PM s1 s2) = undefined --TODO
scanStr p (i,_:s)                     = scanStr p (i+1,s)
scanStr _ (_,[])                      = NoMatch

--2
scanSegment :: Pattern -> (Pos,Segment) -> (Matches,Split)
scanSegment p@(PChc _ _ _) inp       = scanChoicePattern p inp
scanSegment p (i, Str s')            = ([],scanStr p (i,s'))
scanSegment p (i,Chc dim v1 v2) = scanChoice dim i (scan p (0,v1)) (scan p (0,v2))

scanChoicePattern :: Pattern -> (Pos,Segment) -> (Matches,Split)
scanChoicePattern pat@(PChc (D d) p q) (i,Chc dim v1 v2) 
   | d == dim  = scanChoicePattern' dim i False pat--True pat
      (continueInNext p 0 v1 (PatSplit (VM [] 0 []) p)) 
      (continueInNext q 0 v2 (PatSplit (VM [] 0 []) q))-- should be match and not scan -- (scan p (0,v1)) (scan q (0,v2))
   | otherwise   = scanChoicePattern' dim i False pat (scan pat (0,v1)) (scan pat (0,v2)) --TODO??
scanChoicePattern pat@(PChc (DVar d) p q) (i,Chc dim v1 v2) = 
   let ls = (scan pat (0,v1))
       rs = (scan pat (0,v2))
   in {--trace (show dim ++ ": " ++ show ls ++ show rs) (--}case (varCPEmpty ls, varCPEmpty rs) of
      (True, True)   -> let l = (continueInNext p 0 v1 (PatSplit (VM [] 0 []) p))
                            r = (continueInNext q 0 v2 (PatSplit (VM [] 0 []) q))
                        in {--trace (show dim ++ "in : " ++ show l ++ show r) (--}scanChoicePattern' dim i True pat l r --) --(scan p (0,v1)) (scan q (0,v2))
      (True, False)  -> scanChoicePattern' dim i True pat (continueInNext p 0 v1 (PatSplit (VM [] 0 []) p)) {-(scan p (0,v1))-} rs
      (False, True)  -> let m = (continueInNext q 0 v2 (PatSplit (VM [] 0 []) q))
                        in {--trace ("Right Match : "++show m ) (--}scanChoicePattern' dim i True pat ls m --)--(scan q (0,v2))
      (False, False) -> scanChoicePattern' dim i False pat ls rs --)
scanChoicePattern pat (i,Str s) = 
    case matchStr pat i s of
      (SM (PatSplit m (PChc _ p p'))) -> ([], SM (PatSplit m (PChc (dimVar pat) p p')))
      sp                              -> ([],sp)

scanChoicePattern' ::  Dim -> Pos -> Bool -> Pattern -> (Matches,Split) -> (Matches,Split) -> (Matches,Split)  -- scanChoicePattern' = undefined
scanChoicePattern' d i inD _ (ms,NoMatch) (ms',NoMatch) = {-trace ("patern' : " ++show ms ++ show ms') (-} (checkNoMatch (fromMaybe (VM [] i []) (genVMatch d (addDim ((not $ null ms) && (not $ null ms')) d) i ({-resolveOverlap 0 ms ms'-}ms,ms'))),NoMatch)--)
scanChoicePattern' d i inD p (ms,NoMatch) (ms',sp) 
  | inD       = {-trace (show d) (-}case isNoMatch sp of
                                      True -> scanChoicePattern' d i inD p (ms,NoMatch) (ms',NoMatch)
                                      False -> ([],createSP False True i d sp ms ms') --)
  | otherwise = undefined 
--TODO Add NoMatch, Also rigid match would return strsplit if there was a no match so check accordingly
scanChoicePattern' d i inD p (ms,sp) (ms',NoMatch)
  | inD       = case isNoMatch sp of
                                      True -> scanChoicePattern' d i inD p (ms,NoMatch) (ms',NoMatch)
                                      False -> ([],createSP True True i d sp ms ms') 
  | otherwise = undefined
scanChoicePattern' d i inD _ (ms,SM (StrSplit m (_,[]))) (ms',SM (StrSplit m' (_,[]))) 
  | inD       = ([] {-ms++ms'-},SM $ StrSplit ( VM [d] i [MChc d [m] [m']]) (i+1,[]))
  | otherwise = undefined --extract the dimensions from m and m' coz there have been matches in them
scanChoicePattern' d i inD pat (ms,SM (PatSplit m p)) (ms',SM (PatSplit m' p')) 
   =   ([],SM (PatSplit (VM (addDim inD d) i [MChc d [m] [m']] ) (formPatSplit p p' pat)))
scanChoicePattern' d i inD _ (ms,SM (StrSplit m (_,[]))) (ms',SM (PatSplit m' p)) = undefined {-Not sure of the scenario-}
scanChoicePattern' d i inD _ (ms,SM (PatSplit m p)) (m's,SM (StrSplit m' (_,[]))) = undefined {-Not sure of the scenario-}
scanChoicePattern' d i inD pat ms ms' = {-trace (show ms ++ "  " ++ show ms')-} ([],NoMatch)

-- rigid match would return strsplit if there was a no match so check accordingly
isNoMatch :: Split -> Bool
isNoMatch (SM (StrSplit (VM [] 0 []) _ )) = True
isNoMatch _                               = False

dimVar :: Pattern -> DimTy
dimVar (PChc d _ _) = d

formPatSplit :: Pattern -> Pattern -> Pattern -> Pattern
formPatSplit p p' (PChc (D d) _ _)    = (PChc (D d) p p')
formPatSplit p p' (PChc (DVar d) _ _) = (PChc (DVar d) p p')
formPatSplit _ _  _                   = undefined

addDim :: Bool -> Dim -> [Dim]
addDim True d   = [d]
addDim False d  = [] 

createSP :: Bool -> Bool -> Pos -> Dim -> Split -> Matches -> Matches -> Split
createSP left inD i d (SM (StrSplit m (_,[]))) ms ms'
  | left      = (SM (StrSplit (VM (addDim inD d) i [MChc d (ms++[m]) ms']) (i+1,[])))
  | otherwise = (SM (StrSplit (VM (addDim inD d) i [MChc d ms (ms'++[m])]) (i+1,[])))
createSP _ _ _ _ _ _ _= undefined
--createSP left d (SM (PatSplit m' p))

varCPEmpty :: (Matches,Split) -> Bool
varCPEmpty ([],NoMatch) = True
varCPEmpty _            = False
 
--
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

showMatches :: Matches -> String
showMatches [] = ""
showMatches ((VM ds i m):ms) = "("++show i++","++ show (map showMatch m) ++")" ++ showMatches ms

showMatch :: Match -> String
showMatch (MChc d v1 v2) = "MChc "++ show d ++ showMatches v1 ++ showMatches v2
showMatch m              = show m
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
