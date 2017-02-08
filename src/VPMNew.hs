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
vgrep p vs = fst (scan p (startPos,vs))

scan :: Pattern -> Input -> (Matches,Split)
scan _ (_, [])     = ([],NoMatch)
scan p (i, v:vs) = case scanSegment p i v of
  (ms,NoMatch)  -> appendMatches ms (scan p (nextBlock i, vs))
  --(ms,(SM (StrSplit m (j,s')))) -> appendMatches (m:ms) (scan p (j,s'++vs))
  (ms,split)    -> appendMatches ms (continue p (nextBlock i) vs split) 
  
scanSegment :: Pattern -> Pos -> Segment -> (Matches,Split)
scanSegment p i (Str s)     = ([],scanStr p (i, s))
scanSegment p i (Chc d l r) = combineAlternatives d i (scan p (startPos,l)) 
  (scan p (startPos,r))

combineAlternatives :: Dim -> Pos -> (Matches,Split) -> (Matches,Split) -> (Matches,Split)
combineAlternatives d p ([], NoMatch) ([],NoMatch)  = ([],NoMatch) 
  --What about non empty matches and NoMatch
combineAlternatives d p (ms,split) ([],NoMatch)     = 
  let (ms',sp) = mergeAlt d p split NoMatch 
  in ((map (addAlternative L d p) ms) ++ ms', sp )
combineAlternatives d p ([],NoMatch) (ms,split)     = 
  let (ms',sp) =  mergeAlt d p NoMatch split
  in ((map (addAlternative R d p) ms) ++ ms', sp )
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
                     | c==d           = SM $ StrSplit (((pos,[]),[Str[c]]),[]) 
                                                       (incOffset pos,vStr s)
matchStr (Plain Wild) pos (c:s)       = SM $ StrSplit (((pos,[]),[Str[c]]),[]) 
                                                       (incOffset pos,vStr s)
matchStr (Seq a p)    pos (c:[])      = matchStr a pos [c] `andThen` 
 (SM (PatSplit (((startPos,[]),[]),[]) p))
matchStr (Seq a p)    pos (c:s)       = matchStr a pos [c] `andThen` 
  matchStr p (incOffset pos) s
matchStr (Alt p q)    pos s           = matchStr p pos s `orElse` 
  matchStr q pos s
matchStr (PChc (_) p q) pos s         = matchStr p pos s `and'` 
  matchStr q pos s
matchStr _          _   _             = NoMatch

continue :: Pattern -> Pos -> VString -> Split -> (Matches,Split)
continue p i v (SM (StrSplit m (j,s')))      = addMatch m (scan p (j,s'++v))
continue p i v s = trace (show s) undefined --TODO
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

nextBlock :: Pos -> Pos
nextBlock NoPos   = startPos
nextBlock (P b o) = P (b+1) (Left 0)

incOffset :: Pos -> Pos
incOffset NoPos          = startPos
incOffset (P b (Left o)) = P b (Left (o+1)) 

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
addAlternative a d p ((((pos,dimEnv),vs),varEnv)) = 
  (((insertPos a p pos NoPos,dimEnv),[choice a d vs []]), varEnv)

--wrap the matches within split with the choice and create a single split

mergeAlt :: Dim -> Pos -> Split -> Split ->  (Matches,Split)
mergeAlt d i NoMatch NoMatch        = ([],NoMatch)
mergeAlt d i (SM(StrSplit m _) ) NoMatch= ([addAlternative L d i m], NoMatch)
mergeAlt d i NoMatch (SM(StrSplit m _) )= ([addAlternative R d i m], NoMatch)
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
wrapChoice d pos (StrSplit m _ ) (StrSplit m' _) = 
  ([addAlternative L d pos m,addAlternative R d pos m'],NoMatch)
wrapChoice d pos (StrSplit m _ ) (PatSplit m' p) = 
  ([addAlternative L d pos m], SM $ PatSplit (addAlternative R d pos m') p )
wrapChoice d pos (PatSplit m p) (StrSplit m' _ ) = 
  ([addAlternative R d pos m'],SM $ PatSplit (addAlternative L d pos m) p )
wrapChoice d pos (PatSplit m p) (PatSplit m' p') =
  let p1 = PatSplit (addAlternative L d pos m) p
      p2 = PatSplit (addAlternative R d pos m') p'
  in ([], PM (StrSplit emptyMatch (NoPos,[]) ) [p1,p2])







