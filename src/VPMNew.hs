{-# LANGUAGE ViewPatterns #-}
module VPMNew where

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
             | QVar String
             deriving(Show,Eq)
             
type DimVarName = String
data DimTy = D Dim | DVar DimVarName deriving(Show,Eq)

type VString = [Segment]

type Dim = Int

data Segment = Str String | Chc Dim VString VString
             deriving(Show, Eq)
             
type VMatch = (MetaInfo,VString)

--The output type of vgrep is a list of pair of VMatch and query variable bindings
type FinalMatch = (VMatch, QVarEnv)

type Matches = [FinalMatch]

type MetaInfo = (Pos, DimEnv)

type Block  = Int
type Offset = Int

data Pos = P Block (Either Offset (Pos,Pos))
         | NoPos
         deriving(Show,Eq)
         
type DimEnv = [(String,[Dim])]

type QVarEnv = [(String, VMatch)]

data SplitTy = StrSplit FinalMatch Input
             | PatSplit FinalMatch Pattern
             deriving(Show)

{- SM - when matching against a plain string, only a pattern split is returned
   PM - When matching against a choice, all the matches can be combined into one
        String split and group all the pattern splits into one. -}
data Split = NoMatch
           | SM SplitTy  --Single Match
           | PM SplitTy [SplitTy] --Parallel Match (SplitStop, SplitContinue)
           deriving(Show)

type Input = (Pos,VString)

data Alternative = L | R | Both

startPos = (P 0 (Left 0))

emptyMatch :: FinalMatch
emptyMatch = (((NoPos,[]),[]),[])

vgrep :: Pattern -> VString -> Matches
vgrep p vs = fst (scan p (nextBlock NoPos vs,vs))

scan :: Pattern -> Input -> (Matches,Split)
scan _ (_, [])     = ([],NoMatch)
scan p (i,[v])   = case scanSegment p i v of
   (ms,(SM (StrSplit m s))) -> appendMatches (m:ms) (scan p s)
   ms                       -> ms
scan p (i, v:vs) = case scanSegment p i v of
  (ms,NoMatch)  -> appendMatches ms (scan p (nextBlock i vs, vs))
  (ms,(SM (StrSplit m (j,s')))) -> appendMatches (m:ms) (scan p (j,s'++vs))
  (ms,(SM (PatSplit m p')))  -> case rigidMatch p' ((getblock i) + 1) vs of--appendMatches ms (continue p (nextBlock i) vs split)
     ([],NoMatch)           -> let (pos,vs') = rewind ((fst.fst.fst) m, ((snd.fst) m) ++ vs)
                               in  appendMatches ms (scan p (pos, vs')) --rewind
     (ms',SM(StrSplit (((NoPos,[]),[]),[]) s)) -> appendMatches ((map (appendVM m) ms') ++ ms) (scan p s)
     (ms',SM(StrSplit m' s))-> {-trace ("Scan "++ show (m,m',ms))-} appendMatches ((map (appendVM m) (m':ms')) ++ ms) (scan p s)
     (ms',sp@(SM(PatSplit m' p'')))-> (ms',sp)--(((map (appendVM m) ms') ++ ms),sp)
  (ms,PM (StrSplit m (j,s')) ps) ->
    let ms = matchPatSplits i vs ps
  
scanSegment :: Pattern -> Pos -> Segment -> (Matches,Split)
scanSegment p i (Str s)                               = 
  ([],scanStr p (i, s))
scanSegment p i@(P b (Right (lpos,rpos))) (Chc d l r) = 
  let c = combineAlternatives d i (scan p (lpos,l)) (scan p (rpos,r))
  in {-trace ("Scan Segment" ++ show p ++ show i ++ ","++ show (l,r) ++ " , "++show (scan p (lpos,l))++" , "++ show (scan p (rpos,r))++" , "++ show c)-} c 
--scanSegment p i (Chc d l r)                           = 
--  combineAlternatives d i (scan p (startPos,l)) (scan p (startPos,r))
  
{-continue :: Pattern -> Pos -> VString -> Split -> (Matches,Split)
continue p i v (SM (StrSplit m (j,s')))      = addMatch m (scan p (j,s'++v))
continue p i [] s                            = ([],s)
continue p i (v:vs) (SM (PatSplit m p))      = rigidMatch p 
continue p i v s = trace (show s) undefined --TODO-}

rigidMatch :: Pattern -> Block -> VString -> (Matches,Split)
rigidMatch p b []     = ([],NoMatch)
rigidMatch p b (v:vs) = case (matchSegment p b v) of
  (ms,NoMatch)                 -> {-trace ("NoMatch" ++ show p ++ show v)-}(ms,NoMatch)
  (ms,SM (StrSplit m (pos,s))) -> {-trace ("StrSplit "++ show v ++ "Next " ++show (m, pos, s++vs))-} (ms,SM (StrSplit m (pos,s++vs)))
  (ms,SM (PatSplit m p'))      -> {-trace ("PatSplit "++show p' ++ show m ++ show vs)-} appendMatches ms (rigidMatch p' (b+1) vs)

matchSegment :: Pattern -> Block-> Segment -> (Matches,Split)
matchSegment p b (Str s)       = let ma = ([],matchStr p (P b (Left 0) ) s) in {-trace ("match segment STR " ++ show p ++ show s ++ " , " ++(show (snd ma)))-} ma
matchSegment p b (Chc d v1 v2) = 
  let l = rigidMatch p 0 v1
      r = rigidMatch p 0 v2
  in {-trace ("MatchSegment " ++ show l ++ " , " ++ show r)-} combineAlternatives d (P b (Right (startPos,startPos))) (fst l,makeStrSplit (snd l) (nextBlock NoPos v1,v1))
   (fst r,makeStrSplit (snd r) (nextBlock NoPos v2,v2)) 

{-trace (show ((rigidMatch p 0 v1)) ++ " AND " ++ show (rigidMatch p 0 v2) ) snd $ 
  combineAlternatives d NoPos ([],(rigidMatch p 0 v1)) ([],(rigidMatch p 0 v2)) -}

{-combineAlternative' :: Dim -> Pos -> Split -> Split -> Split
combineAlternative' d p (m,(pos,s) (m',(pos',s')) = 
combineAlternative' -}

combineAlternatives :: Dim -> Pos -> (Matches,Split) -> (Matches,Split) -> (Matches,Split)
combineAlternatives d p ([], NoMatch) ([],NoMatch)  = ([],NoMatch) 
combineAlternatives d p ([], SM(StrSplit (((NoPos,[]),[]),[])  _)) 
 ([],SM(StrSplit (((NoPos,[]),[]),[]) _))           = ([],NoMatch)
  --What about non empty matches and NoMatch
combineAlternatives d p (ms,split) ([],NoMatch)     = 
  let (ms',sp) = mergeAlt d p split NoMatch 
  in ((map (addAlternative L d p) ms) ++ ms', sp )
combineAlternatives d p ([],NoMatch) (ms,split)     = 
  let (ms',sp) =  mergeAlt d p NoMatch split
  in {-trace ("Combine "++show sp)-} ((map (addAlternative R d p) ms) ++ ms', sp )
combineAlternatives d p (ms,split) (ms',split')     = 
  let (ms'',sp) = mergeAlt d p split split'
  in ((map (addAlternative L d p) ms)++(map (addAlternative R d p) ms')++ms'',
   sp) --Check for overlapping






{-resolveOverlap :: Pos -> Matches -> Matches -> (Matches,Matches)
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

earlier :: Pos -> Pos -> Maybe Bool
earlier (NoPos) (NoPos) = Nothing
earlier (NoPos) p       = Just False
earlier p       (NoPos) = Just True
earlier (P b (Left o)) (P b' (Left o')) 
  | b < b'          = Just True
  | b > b'          = Just False
  | b == b' && o<o' = Just True
  | b == b' && o'<o = Just False
  | b == b' && o==o'= Nothing  
earlier (P b (Left o)) (P b' (Right (l,r)))-}


scanStr :: Pattern -> (Pos,String) -> Split
scanStr p (i,matchStr p i-> SM s)     = {-trace (show s)-} (SM s)
scanStr p (i,matchStr p i-> PM s1 s2) = undefined
scanStr p (i,_:s)                     = scanStr p (incOffset i,s)
scanStr _ (_,[])                      = NoMatch

matchStr :: Pattern -> Pos -> String -> Split
matchStr (Plain (C d)) pos (c:s)
                     | c==d           = {-trace (show (pos) ++ (show c) ++ show (incOffset pos) ++ s++ "\n")-} SM $ StrSplit (((pos,[]),[Str[c]]),[]) 
                                                       (incOffset pos,vStr s)
matchStr (Plain Wild) pos (c:s)       = SM $ StrSplit (((pos,[]),[Str[c]]),[]) 
                                                       (incOffset pos,vStr s)
matchStr (Seq a p)    pos (c:[])      = let m = matchStr a pos [c] `andThen` (SM (PatSplit (((startPos,[]),[]),[]) p))
 in {-trace (show m)-} m
matchStr (Seq a p)    pos (c:s)       = matchStr a pos [c] `andThen` 
  matchStr p (incOffset pos) s
matchStr (Alt p q)    pos s           = matchStr p pos s `orElse` 
  matchStr q pos s
matchStr (PChc (_) p q) pos s         = matchStr p pos s `and'` 
  matchStr q pos s
matchStr _          _   _             = NoMatch


   --next occurrence
{-continue p i (s:ss) (SM (PatSplit (VM ds j m) p')) = let sp = rigidMatch p (VM ds j m) p' (i,s)
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
-}


-------Helpers------------------------------------
vStr [] = []
vStr s  = [Str s]

getblock :: Pos -> Int
getblock NoPos   = 0
getblock (P b _) = b

nextBlock :: Pos -> VString -> Pos
nextBlock p []    = p
nextBlock NoPos (Str _ : vs) = startPos
nextBlock (P b o) (Str _ : vs) = P (b+1) (Left 0)
nextBlock NoPos (Chc _ _ _ : vs) = P 0 (Right (startPos,startPos))
nextBlock (P b o) (Chc _ _ _ : vs) = P (b+1) (Right (startPos,startPos))

incOffset :: Pos -> Pos
incOffset NoPos          = startPos
incOffset (P b (Left o)) = P b (Left (o+1)) 

incBlock :: Pos -> Pos
incBlock NoPos   = NoPos
incBlock (P b o) = P (b+1) o 

orElse :: Split -> Split -> Split
orElse NoMatch s   = s
orElse s _         = s --return both the matches like the unix grep?
--
andThen :: Split -> Split-> Split
andThen NoMatch _                                 = NoMatch
andThen _       NoMatch                           = NoMatch
andThen (SM (StrSplit m s)) (SM (PatSplit m' p))  = 
  SM (PatSplit (appendVM m m') p)
andThen (SM (StrSplit m s)) (SM (StrSplit m' s')) = 
  SM (StrSplit (appendVM m m') s')
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

appendVM :: FinalMatch -> FinalMatch -> FinalMatch
appendVM (((pos,dimEnv),vs),qVars) (((pos',dimEnv'),vs'),qVars') = 
  (((pos,dimEnv++dimEnv'),combineStr (vs++vs')),qVars++qVars')        

combineStr :: VString -> VString
combineStr []                              = []
combineStr [m]                             = [m]
combineStr ((Str s) : (Str s') : ms)     = combineStr ((Str (s++s')):ms)
combineStr (Str s : m1@(Chc _ _ _ ): ms) = (Str s) : m1 : combineStr ms
combineStr (m1@(Chc _ _ _ ) :ms )         = m1 : combineStr ms

appendMatches :: Matches -> (Matches,Split) -> (Matches,Split)
appendMatches ms (ms',s) = (ms++ms',s)--(removeNoMatch ms ++ removeNoMatch ms',s)

addMatch :: FinalMatch -> (Matches,Split) -> (Matches,Split)
addMatch (((NoPos,_),[]),_) m = m
addMatch m (ms,s) = (m:ms,s)

nonEmptyChc :: Pos -> Dim -> Input -> Input -> Split
nonEmptyChc p d (_,[]) (_,[])     = NoMatch
nonEmptyChc p d (lpos,l) (_,[])   =
 SM $ StrSplit (emptyMatch) (insertPos L p lpos NoPos, [choice L d l []])
nonEmptyChc p d (_,[]) (rpos,r)   =
 SM $ StrSplit (emptyMatch) (insertPos R p rpos NoPos, [choice R d r []])
nonEmptyChc p d (lpos,l) (rpos,r) =
 SM $ StrSplit (emptyMatch) (insertPos Both p lpos rpos, [choice Both d l r])

choice :: Alternative -> Dim -> VString -> VString -> Segment
choice L d l _    = Chc d l []
choice R d r _    = Chc d [] r
choice Both d l r = Chc d l r 

insertPos :: Alternative -> Pos -> Pos -> Pos -> Pos
insertPos a (NoPos) l r = P 0 (Right (choicePos a l r))
insertPos a (P b _) l r = P b (Right (choicePos a l r)) 


choicePos :: Alternative -> Pos -> Pos -> (Pos,Pos)
choicePos L p _     = (p,NoPos)
choicePos R p _     = (NoPos,p)
choicePos Both p p' = (p,p')

--Wrap the matches with the choice and create different matches    
addAlternative :: Alternative -> Dim -> Pos -> FinalMatch -> FinalMatch
addAlternative a d p m 
 | m == emptyMatch = emptyMatch
addAlternative a d p ((((pos,dimEnv),vs),varEnv)) = 
  (((insertPos a p pos NoPos,dimEnv),[choice a d vs []]), varEnv)

--wrap the matches within split with the choice and create a single split

mergeAlt :: Dim -> Pos -> Split -> Split ->  (Matches,Split)
mergeAlt d i NoMatch NoMatch        = ([],NoMatch)
mergeAlt d i (SM(StrSplit m (p,s)) ) NoMatch= 
  ([], SM (StrSplit (addAlternative L d i m) 
    (insertPos L i p NoPos,[choice L d s []])))
mergeAlt d i NoMatch (SM(StrSplit m (p,s)) )= 
  ([], SM (StrSplit (addAlternative R d i m) 
    (insertPos R i p NoPos,[choice R d s []])))
mergeAlt d i (SM(PatSplit m' p)) NoMatch= 
  ([],SM(PatSplit (addAlternative L d i m') p))
mergeAlt d i NoMatch (SM(PatSplit m' p))=
  ([],SM(PatSplit (addAlternative R d i m') p))
mergeAlt d i (SM s1) (SM s2)        = mergeAlt' (wrapChoice d i s1 s2) []
mergeAlt d i (SM s1) (PM s2 ps)     = mergeAlt' (wrapChoice d i s1 s2) ps
mergeAlt d i (PM s1 ps) (SM s2)     = mergeAlt' (wrapChoice d i s1 s2) ps
mergeAlt d i (PM s1 ps) (PM s2 ps') = mergeAlt' (wrapChoice d i s1 s2) (ps++ps')
mergeAlt d i sp sp'                 = 
  trace (show "Split1 "++show sp ++ ",Split2 " ++ show sp' ) undefined

mergeAlt' :: (Matches,Split) -> [SplitTy] -> (Matches,Split)
mergeAlt' s []                          = s
mergeAlt' (ms, SM s1@(StrSplit _ _)) ps = (ms, PM s1 ps)
mergeAlt' (ms, SM s1@(PatSplit _ _)) ps = 
  (ms, PM (StrSplit emptyMatch (NoPos,[]) ) (s1:ps))
mergeAlt' (ms, PM s ps') ps             = (ms, PM s (ps'++ps))


wrapChoice :: Dim -> Pos -> SplitTy -> SplitTy -> (Matches, Split)
wrapChoice d pos (StrSplit m l ) (StrSplit m' r) = {-trace ("Wrap Choice : "++ (show $ pos))-}
  ((checkEmpty $ addAlternative L d pos m) ++ (checkEmpty $ addAlternative R d pos m'),
    nonEmptyChc pos d l r)
wrapChoice d pos (StrSplit m _ ) (PatSplit m' p) = 
  (checkEmpty $ addAlternative L d pos m, SM $ PatSplit (addAlternative R d pos m') p )
wrapChoice d pos (PatSplit m p) (StrSplit m' _ ) = 
  (checkEmpty $ addAlternative R d pos m',SM $ PatSplit (addAlternative L d pos m) p )
wrapChoice d pos (PatSplit m p) (PatSplit m' p') =
  let p1 = PatSplit (addAlternative L d pos m) p
      p2 = PatSplit (addAlternative R d pos m') p'
  in ([], PM (StrSplit emptyMatch (NoPos,[]) ) [p1,p2])
  
rewind :: Input -> Input
rewind (p,[])                                    = (p,[])
rewind (p@(P b (Left o)), (Str [c]):vs)          = (nextBlock p vs {-P (b+1) (Left 0)-}, vs)
rewind (P b (Left o), (Str (s:ss)):vs)           = (P b (Left (o+1)), (Str ss) : vs)
rewind (p@(P b (Right (l,r))), (Chc d v1 v2):vs) = 
  case (rewind (l,v1), rewind (r,v2)) of
    ((l',[]),(r',[]))   -> (nextBlock p vs, vs)
    ((l',[]),(r',v2'))  -> (P b (Right (NoPos,r')), (Chc d [] v2') : vs)
    ((l',v1'),(r',[]))  -> (P b (Right (l',NoPos)), (Chc d v1' []) : vs)
rewind i = trace (show i) undefined
    
makeStrSplit :: Split -> Input -> Split
makeStrSplit NoMatch inp = SM (StrSplit emptyMatch inp)
makeStrSplit sp _        = sp

checkEmpty :: FinalMatch -> [FinalMatch]
checkEmpty m 
 | m ==emptyMatch = []
 |otherwise       = [m]







