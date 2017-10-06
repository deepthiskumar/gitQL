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
             | PChcStar DimTy Pattern Pattern
             | Repeat Pattern Int {-min-} (Maybe Int) {-max-}
             --Ex. `a*` => `Repeat (ch 'a') 0 None`
             | None --Ex. `(a|)b` => `Seq (Alt (ch 'a') None) (ch 'b')
             | QVar QVarName
             | Any
             deriving(Show,Eq)
             
type DimVarName = String
data DimTy = D Dim | DVar DimVarName deriving(Show,Eq)

type QVarName = String
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
         
instance Ord Pos where
  NoPos <= NoPos = True
  P b o <= NoPos = False
  NoPos <= P b o = True
  P b o <= P b' o' 
    | b < b' = True
    | b > b' = False
    | b == b' = case (o,o') of
                 (Left i, Left i') -> i <= i
                 (Right (p,p'), Right (q,q')) -> p <= q && p' <= q' --TODO For p<=q and p' > q' we need to merge the vstring to equalize the offsets.   
         
type DimEnv = [(String,Dim)]

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
scan _ (_,[])        = ([],NoMatch)
scan (QVar x) (i,vs) = ([], SM $ PatSplit (((nextBlock (incBlock (-1) i) vs,[]),vs),[(x,((i,[]),vs))]) (QVar x))
scan p inp           = case scanOnce p inp of
  (ms,SM (StrSplit _ inp')) -> {-trace ("Scan "++ show ms ++ show inp')-} (appendMatches (concatMap checkEmpty ms) (scan p inp'))
  sp                        -> sp
{-scan p (i,[v])       = case scanSegment p i v of
   (ms,(SM (StrSplit m s))) -> (appendMatches ((checkEmpty m)++ms) (scan p s))
   ms                       -> ms
scan p (i, v:vs)     = case scanSegment p i v of
  (ms,NoMatch)  -> appendMatches ms (scan p (nextBlock i vs, vs))
  (ms,(SM (StrSplit m (j,s')))) -> (appendMatches ((checkEmpty m)++ms) (scan p (j,s'++vs)))
  (ms,(SM (PatSplit m p')))  -> case match m p' ((getblock i)+1) vs of
    (ms', SM(StrSplit _ inp)) -> appendMatches (ms++ms') (scan p inp)
    (ms', _) -> (ms++ms',NoMatch)
  (ms,PM sp@(StrSplit m (j,s')) ps) -> case matchPatSplits i vs ps of
     []  -> appendMatches (ms) (addMatch m (scan p (j,s'++vs)))
     (sps) -> appendMatches (ms ++ (concatMap fst sps)) (addMatch m (scan p (getShortestSegment (((SM sp): (map snd sps))))))-}

scanOnce :: Pattern -> Input -> (Matches,Split)
--scan p (i, [])       = trace (show p ++ " AND " ++ show i) ([],SM $ PatSplit (((i,[]),[]),[]) p)
scanOnce p (i,[v])   = {-trace ("Scan Once "++ show p ++ show i ++ show v)-} (case scanSegment p i v of
   (ms,(SM (StrSplit m s))) -> (((checkEmpty m)++ms),SM $ StrSplit emptyMatch s)
   (ms,sp)                  -> (ms,sp))
scanOnce p (i, v:vs)     = case scanSegment p i v of
  (ms,NoMatch)  -> {-trace ("NoMatch "++ show v ++ show p ++ show (nextBlock i vs) ++ show vs)-} (ms, SM $ StrSplit emptyMatch (nextBlock i vs, vs))
  (ms,sp@(SM (StrSplit m (j,s')))) -> {-trace ("Str" ++ show sp)-} ((checkEmpty m)++ms, SM $ StrSplit emptyMatch (correctPos s' i j vs,s'++vs))
  (ms,(SM (PatSplit m p')))  -> {-trace ("Pat " ++ show m ++ show p' ++ show vs)-} (case p' == p of
   False -> (case match m p' ((getblock i)+1) vs of 
    (ms', SM(StrSplit _ (j,s'))) -> {-trace ("ScanOnce "++show ms' ++ show s')-}(ms++ms', SM $ StrSplit emptyMatch (correctPos s' i j vs, s'))
    (ms', sp) -> (ms++ms',sp))
   True  -> (ms, SM (StrSplit emptyMatch (nextBlock i vs,vs))))-- same pattern. Would have matched empty string. So ignore that
  (ms,PM sp@(StrSplit m (j,s')) ps) -> (case matchPatSplits i vs (eliminate p ps) of
     []  -> (ms++[m], SM $ StrSplit emptyMatch (correctPos s' i j vs ,s'++vs))
     (sps) -> {-trace ("Show PMs " ++ show ps ++ show sps) -}(ms ++ (concatMap fst sps) ++ [m], SM $ StrSplit emptyMatch (getShortestSegment (((SM (StrSplit m (correctPos s' i j vs,s'++vs))): (map snd sps))))))

{-match :: FinalMatch -> Pattern -> Pattern -> Pos -> VString -> (Matches,Split)
match m p p' i vs = case rigidMatch p' ((getblock i) + 1) vs of
  ([],NoMatch)           -> let (pos,vs') = rewind ((fst.fst.fst) m, ((snd.fst) m) ++ vs)
                            in  (scan p (pos, vs')) --rewind
  (ms',SM(StrSplit (((NoPos,[]),[]),[]) s)) -> appendMatches (map (appendVM m) ms') (scan p s)
  (ms',SM(StrSplit m' s))-> {-trace ("Scan "++ show (m,m',ms))-} appendMatches (map (appendVM m) (m':ms')) (scan p s)
  (ms',sp@(SM(PatSplit m' p'')))-> (ms',sp)--(((map (appendVM m) ms') ++ ms),sp) --Input exhausted-}
  
match :: FinalMatch -> Pattern -> Block -> VString -> (Matches,Split)
match m p i [] = ([], SM (PatSplit m p))
match m p i vs = case rigidMatch p i vs of
  ([],NoMatch)           -> {-trace ("Match: "++show m++ "VS: "++show vs++ "Block:"++ show i ++ "Pattern:" ++ show p)-} (let (pos,vs') = rewind (fromMaybe (nextBlock (P (i-1) (Left 0)) vs) (getPos m), (getMatchVS m) ++ vs)
                            in  ([], SM(StrSplit emptyMatch (pos, vs')))) --rewind
  (ms',NoMatch)           -> (map (appendVM m) ms',SM(StrSplit emptyMatch (NoPos,[])))
  (ms',SM(StrSplit (((NoPos,[]),[]),[]) s)) -> (map (appendVM m) ms',SM(StrSplit emptyMatch s))
  (ms',SM(StrSplit m' s))-> {-trace ("Scan "++ show (m,m',ms))-} (map (appendVM m) (m':ms'),SM(StrSplit emptyMatch s)) 
  (ms',sp@(SM(PatSplit m' p')))-> (ms',sp)--(((map (appendVM m) ms') ++ ms),sp) --Input exhausted
  (ms',sp) -> trace (show ms' ++ "||"++show sp) undefined
    
matchPatSplits :: Pos -> VString -> [SplitTy] -> [(Matches,Split)]
matchPatSplits _ vs []                   = []
matchPatSplits i vs ((PatSplit m p):ps) = (case rigidMatch p ((getblock i) + 1) vs of--appendMatches ms (continue p (nextBlock i) vs split)
     ([],NoMatch)           -> let s = rewind ((fst.fst.fst) m, ((snd.fst) m) ++ vs) --Just return the remaining vs to be scanned
                               in  {-trace ("Here")-} ([],SM $ StrSplit emptyMatch s) : matchPatSplits i vs ps--rewind
     (ms',sp@(SM(StrSplit (((NoPos,[]),[]),[]) s))) -> {-trace ("Empty Str : " ++ show m++ show ms' ++ show sp)-} ((map (appendVM m) ms',sp ): matchPatSplits i vs ps)
     (ms',SM(StrSplit m' s))-> {-trace ("Str "++ show (m,m',ms'))-} (map (appendVM m) (m':ms'),SM $ StrSplit emptyMatch s) :matchPatSplits i vs ps
     (ms',sp@(SM(PatSplit m' p'')))-> {-trace ("or here" ++ show ms' ++ show sp)-} ((map (appendVM m)ms',sp) : matchPatSplits i vs ps))--(((map (appendVM m) ms') ++ ms),sp)

  
scanSegment :: Pattern -> Pos -> Segment -> (Matches,Split)
scanSegment p@(PChc _ _ _) i s                        = scanChoicePattern True p i s
scanSegment p@(PChcStar _ _ _) i s                    = let r = scanChoiceStarPattern p i s in {-trace (show p ++ show s ++ show r)-} r
scanSegment p i (Str "")                              = {-trace ("IsNOne: "++show p)-} ([],SM (PatSplit (((i,[]),[Str ""]),[]) p ))
scanSegment p i (Str s)                               = 
  ([],scanStr p (i, s))
scanSegment p i@(P b (Right (lpos,rpos))) c1@(Chc d l r) = {-trace ("Scan Seg "++show i ++ show c1  )-}
  (let c = combineAlternatives d i (scan p (lpos,l)) (scan p (rpos,r))
  in {-trace ("Scan Segment" ++ show p ++ show i ++ ","++ show (l,r) ++ " , "++show (scan p (lpos,l))++" , "++ show (scan p (rpos,r))++" , "++ show c)-} c )
scanSegment p i s = trace ("Pattern: "++show p ++ " | Pos: " ++ show i ++ " | VS: " ++ show s  ) undefined
--  combineAlternatives d i (scan p (startPos,l)) (scan p (startPos,r))

scanChoicePattern :: Bool -> Pattern -> Pos-> Segment -> (Matches,Split)
scanChoicePattern _ pat@(PChc (D d) p q) pos (Chc dim v1 v2) 
   | d == dim   = (combine False pos dim (D d) (match ((((getLeftPos pos),[]),[]),[]) p (getblock $ getLeftPos pos) v1) (match ((((getRightPos pos),[]),[]),[]) q (getblock $ getRightPos pos) v2))
   -- (match emptyMatch p (getLeftPos pos) v1)  (match emptyMatch q (getRightPos pos) v2)
   | otherwise  = (combineAlternatives dim pos (scan pat (getLeftPos pos, v1)) (scan pat (getRightPos pos,v2))) -- Will not produces any extra matches. matches in each alternative is combined into one single match
scanChoicePattern _ pat@(PChc (DVar d) p q) pos (Chc dim v1 v2) = {-trace ("Scan Choice Pattern: " ++show p ++ show q ++ show v1 ++ show v2 ++ show (match ((((getLeftPos pos),[]),[]),[]) p (getblock $ getLeftPos pos) v1) ++ show (match ((((getRightPos pos),[]),[]),[]) q (getblock $ getRightPos pos) v2) ++ show (scan pat (getLeftPos pos, v1)) ++ show (scan pat (getRightPos pos,v2)))-}
  (combineAlternatives dim pos (scan pat (getLeftPos pos, v1)) (scan pat (getRightPos pos,v2))) `or'` 
   (combine False pos dim (DVar d)(match ((((getLeftPos pos),[]),[]),[]) p (getblock $ getLeftPos pos) v1) (match ((((getRightPos pos),[]),[]),[]) q (getblock $ getRightPos pos) v2))
--scanChoicePattern pat@(PChc dy _ _) i (Str "") = ([],SM (Pa))
scanChoicePattern scan pat@(PChc dy _ _) i (Str s) 
 | scan      = case scanStr pat (i, s) of --In case of sharing TODO needs to be a scan as well because the match could start within a plain string
      (SM (PatSplit m (PChc _ p q))) -> ([],SM (PatSplit m (PChc dy p q)))
      sp@(SM(StrSplit m s))          -> ([],sp)
      otherwise                      -> ([],NoMatch)
 | otherwise = case matchStr pat i s of --In case of sharing 
      (SM (PatSplit m (PChc _ p q))) -> {-trace ("PatSplit: "++show p ++ show q)-} ([],SM (PatSplit m (PChc dy p q)))
      sp@(SM(StrSplit m s))          -> {-trace ("here" ++ show sp)-}([],sp)
      otherwise                      -> {-trace ("or here")-} ([],NoMatch)
      
scanChoiceStarPattern :: Pattern -> Pos-> Segment -> (Matches,Split)
scanChoiceStarPattern pat@(PChcStar (D d) p q) pos (Chc dim v1 v2) 
   | d == dim   = (combine True pos dim (D d) (scan p (getLeftPos pos,v1)) (scan q (getRightPos pos,v2)))
   -- (match emptyMatch p (getLeftPos pos) v1)  (match emptyMatch q (getRightPos pos) v2)
   | otherwise  = (combineAlternatives dim pos (scan pat (nextBlock (incBlock (-1) $ getLeftPos pos) v1, v1)) (scan pat (nextBlock (incBlock (-1) $ getRightPos pos) v2,v2))) -- Will not produces any extra matches. matches in each alternative is combined into one single match
scanChoiceStarPattern pat@(PChcStar (DVar d) p q) pos (Chc dim v1 v2) = 
  (combineAlternatives dim pos (scan pat (getLeftPos pos, v1)) (scan pat (getRightPos pos,v2))) `or'` 
   (combine True pos dim (DVar d)(scan p (nextBlock (incBlock (-1) $ getLeftPos pos) v1,v1)) (scan q (nextBlock (incBlock (-1) $ getRightPos pos) v2,v2)))
--scanChoicePattern pat@(PChc dy _ _) i (Str "") = ([],SM (Pa))
scanChoiceStarPattern pat@(PChcStar dy _ _) i (Str s) = case scanStr pat (i, s)of --In case of sharing
      (SM (PatSplit m (PChcStar _ p q))) -> ([],SM (PatSplit m (PChcStar dy p q)))
      sp@(SM(StrSplit m s))          -> ([],sp)
      otherwise                      -> ([],NoMatch)
      
matchChoiceStarPattern :: Pattern -> Pos-> Segment -> (Matches,Split)
matchChoiceStarPattern pat@(PChcStar (D d) p q) pos (Chc dim v1 v2) 
   | d == dim   = (combine True pos dim (D d) (match ((((getLeftPos pos),[]),[]),[]) p (getblock $ getLeftPos pos) v1) (match ((((getRightPos pos),[]),[]),[]) q (getblock $ getRightPos pos) v2))
   -- (match emptyMatch p (getLeftPos pos) v1)  (match emptyMatch q (getRightPos pos) v2)
   | otherwise  = ([],NoMatch)
matchChoiceStarPattern pat@(PChcStar (DVar d) p q) pos (Chc dim v1 v2) = 
  {-(combineAlternatives dim pos (match ((((getLeftPos pos),[]),[]),[]) pat (getblock $ getLeftPos pos) v1) (match ((((getRightPos pos),[]),[]),[]) pat (getblock $ getRightPos pos) v2)) `or'` -}
   (combine True pos dim (DVar d)(match ((((getLeftPos pos),[]),[]),[]) p (getblock $ getLeftPos pos) v1) (match ((((getRightPos pos),[]),[]),[]) q (getblock $ getRightPos pos) v2))
--scanChoicePattern pat@(PChc dy _ _) i (Str "") = ([],SM (Pa))
matchChoiceStarPattern pat@(PChcStar dy _ _) i (Str s) = case matchStr pat i s of --In case of sharing
      (SM (PatSplit m (PChcStar _ p q))) -> ([],SM (PatSplit m (PChcStar dy p q)))
      sp@(SM(StrSplit m s))          -> ([],sp)
      otherwise                      -> ([],NoMatch)


combineStar :: Bool -> Pos -> Dim -> DimTy -> (Matches,Split) -> (Matches,Split) -> (Matches,Split)
combineStar isStar pos d dy (ms,NoMatch) (ms',NoMatch)                                      =
  ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
combineStar isCStar pos d dy a@(ms, SM(StrSplit ma (_,[]))) b@(ms', SM(StrSplit ma' (_,[]))) =
  ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
combineStar isCStar pos d dy a@(ms, SM(StrSplit ma (l,v1))) b@(ms', SM(StrSplit ma' (r,v2))) =
  ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (insertPos Both pos l r,[Chc d v1 v2])))
combineStar isCStar pos d dy (ms, SM(PatSplit m p)) (ms', SM(PatSplit m' q)) {-trace (show p ++ " AND " ++ show q)-}
  | isCStar   = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM(PatSplit (combineAlternative d dy pos m m') (PChcStar dy p q)))
  | otherwise = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM(PatSplit (combineAlternative d dy pos m m') (PChc dy p q)))
  -- TODO should match only the same dimension if the next segment is a choice
combineStar isCStar pos d dy (ms, SM(PatSplit ma (QVar x)) ) (ms', SM(StrSplit ma' (_,[]))) = 
  ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
combineStar isCStar pos d dy (ms, SM(StrSplit ma (_,[]))) (ms', SM(PatSplit ma' (QVar x)) ) = 
  ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
combineStar isCStar pos d dy (ms, SM(PatSplit ma (QVar x)) ) (ms', NoMatch)
  | not $ null ms' = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
  | otherwise      = ([],NoMatch)
combineStar isCStar pos d dy (ms, NoMatch) (ms', SM(PatSplit ma' (QVar x)) ) 
  | not $ null ms  = ([combine'' pos d dy m m' | m <- ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
  | otherwise      = ([],NoMatch)
combineStar isCStar pos d dy a@(ms, NoMatch) b@(ms', SM(PatSplit ma' p) ) 
  | not $ null ms && isCStar = {-trace ("Combine"++show a ++ " And "++show b )-} ([combine'' pos d dy m m' | m <- tail ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d dy pos (head ms) ma') (PChcStar dy None p)))
  | not $ null ms = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d dy pos (head ms) ma') (PChc dy None p)))
  | otherwise      = ([],NoMatch)
combineStar isCStar pos d dy (ms', SM(PatSplit ma' p) ) (ms, NoMatch)
  | not $ null ms && isCStar = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d dy pos ma' (head ms) ) (PChcStar dy p None)))
  | not $ null ms   = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d dy pos ma' (head ms)) (PChc dy p None)))
  | otherwise      = ([],NoMatch)
combineStar _ _ _ _ ms ms' = {-trace ("Combine: "++ show ms ++ " And " ++ show ms')-} ([],NoMatch) 

  
combine :: Bool -> Pos -> Dim -> DimTy -> (Matches,Split) -> (Matches,Split) -> (Matches,Split)
combine isStar pos d dy (ms,NoMatch) (ms',NoMatch)                                      =
  ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
combine isCStar pos d dy a@(ms, SM(StrSplit ma (_,[]))) b@(ms', SM(StrSplit ma' (_,[]))) = {-trace ("Combine" ++ show ms ++ show ma ++ show ms' ++ show ma')-}
  ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
combine isCStar pos d dy (ms, SM(PatSplit ma None)) (ms', SM(PatSplit ma' None)) = 
  ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
combine isCStar pos d dy (ms, SM(PatSplit ma (QVar x)) ) (ms', SM(PatSplit ma' (QVar y)) ) =
  ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit (combineAlternative d dy pos ma ma' ) (incBlock 1 pos,[])))
combine isCStar pos d dy (ms, SM(PatSplit m p)) (ms', SM(PatSplit m' q)) = {-trace (show p ++ " AND " ++ show q)-}
  ([combine'' pos d dy ma ma' | ma <- ms, ma' <- ms', ma/= emptyMatch, ma'/= emptyMatch], SM(PatSplit (combineAlternative d dy pos m m') (makeCP isCStar dy p q)))
  -- TODO should match only the same dimension if the next segment is a choice
combine isCStar pos d dy (ms, SM(PatSplit ma (QVar x)) ) (ms', SM(StrSplit ma' (_,[]))) = 
  ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
combine isCStar pos d dy (ms, SM(StrSplit ma (_,[]))) (ms', SM(PatSplit ma' (QVar x)) ) = 
  ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
combine isCStar pos d dy (ms, SM(PatSplit ma (None)) ) (ms', SM(StrSplit ma' (i,s)))  
  | not isCStar && null s = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
  | isCStar          = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (i,s)))
combine isCStar pos d dy (ms, SM(StrSplit ma (i,s))) (ms', SM(PatSplit ma' (None)) ) 
  | not isCStar && null s = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
  | isCStar            = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (i,s)))
combine isCStar pos d dy (ms, SM(PatSplit ma (QVar x)) ) (ms', NoMatch)
  | not $ null ms' = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
  | otherwise      = ([],NoMatch)
combine isCStar pos d dy (ms, NoMatch) (ms', SM(PatSplit ma' (QVar x)) ) 
  | not $ null ms  = ([combine'' pos d dy m m' | m <- ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (incBlock 1 pos,[])))
  | otherwise      = ([],NoMatch)
combine isCStar pos d dy a@(ms, SM(StrSplit ma (l,v1))) b@(ms', SM(StrSplit ma' (r,v2)))
  | isCStar = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (insertPos Both pos l r,[Chc d v1 v2])))
  | otherwise = ([],NoMatch)
combine isCStar pos d dy a@(ms, NoMatch) b@(ms', SM(PatSplit ma' p) ) 
  | isNone p  && (not $ null ms) && isCStar  =  ([combine'' pos d dy m m' | m <- ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], NoMatch) --Pchc none none case
  | not $ null ms && isCStar = let r = ([combine'' pos d dy m m' | m <- tail ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d dy pos (head ms) ma') (PChcStar dy None p))) in {-trace ("Combine"++show a ++ " And "++show b ++ "AND " ++ show r) -} r
  | otherwise      = ([],NoMatch)
combine isCStar pos d dy (ms', SM(PatSplit ma' p) ) (ms, NoMatch)
  | isNone p && (not $ null ms) && isCStar  =  ([combine'' pos d dy m' m | m <- ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
  | not $ null ms && isCStar = ([combine'' pos d dy m' m | m <- tail ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d dy pos ma' (head ms) ) (PChcStar dy p None)))
  | otherwise      = ([],NoMatch)
--combine isCStar 
{-combine isCStar pos d dy a@(ms, NoMatch) b@(ms', SM(PatSplit ma' p) ) 
  | not $ null ms && isCStar = {-trace ("Combine"++show a ++ " And "++show b )-} ([combine'' pos d dy m m' | m <- tail ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d pos (head ms) ma') (PChcStar dy None p)))
  | not $ null ms = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d pos (head ms) ma') (PChc dy None p)))
  | otherwise      = ([],NoMatch)
combine isCStar pos d dy (ms', SM(PatSplit ma' p) ) (ms, NoMatch)
  | not $ null ms && isCStar = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d pos ma' (head ms) ) (PChcStar dy p None)))
  | not $ null ms   = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combineAlternative d pos ma' (head ms)) (PChc dy p None)))
  | otherwise      = ([],NoMatch)-}
combine _ _ _ _ ms ms' = {-trace ("Combine: "++ show ms ++ " And " ++ show ms')-} ([],NoMatch)

or' :: (Matches,Split) -> (Matches,Split) -> (Matches,Split)
or' ([],NoMatch) ms = ms
or' ms ([],NoMatch) = ms
or' (ms,sp1@(SM (StrSplit m s))) (ms',sp2@(SM (StrSplit m' s'))) =
  (concatMap checkEmpty [m,m'] ++ (ms++ms'),SM $ StrSplit emptyMatch (getShortestSegment ([sp1,sp2]) ))
or' (ms,(SM sp1@(PatSplit m p))) (ms',(SM sp2@(PatSplit m' p'))) =
  (ms++ms', PM (StrSplit emptyMatch (NoPos,[])) [sp1,sp2])
or' (ms,sp1@(SM (StrSplit m s))) (ms',(SM sp2@(PatSplit m' p'))) =
  (checkEmpty m ++ ms++ms', PM (StrSplit emptyMatch s) [sp2])
or' (ms,(SM sp1@(PatSplit m p))) (ms',sp2@(SM (StrSplit m' s'))) =
  (checkEmpty m' ++ ms++ms', PM (StrSplit emptyMatch s') [sp1])
or' (ms,sp1@(PM (StrSplit m s) ps)) (ms',(SM sp2@(PatSplit m' p'))) =
  (checkEmpty m ++ ms++ms', PM (StrSplit emptyMatch s) (sp2:ps))
or' (ms,sp1@(PM (StrSplit m s) ps)) (ms',sp2@(SM (StrSplit m' s'))) =
  (concatMap checkEmpty [m,m'] ++ ms++ms', PM (StrSplit emptyMatch (getShortestSegment [sp1,sp2])) ps)
or' (ms,(SM sp1@(PatSplit m p))) (ms',(PM (StrSplit m' s') ps')) =
  (checkEmpty m'++ ms++ms', PM (StrSplit emptyMatch s') (sp1:ps'))
or' (ms,sp1@(SM (StrSplit m s))) (ms',(PM sp2@(StrSplit m' s') ps'))  =
  (concatMap checkEmpty [m,m'] ++ms++ms', PM (StrSplit emptyMatch (getShortestSegment [sp1,SM sp2])) ps')
or' (ms,(PM sp1@(StrSplit m s) ps)) (ms',(PM sp2@(StrSplit m' s') ps')) = 
  (concatMap checkEmpty [m,m'] ++ms++ms', PM (StrSplit emptyMatch (getShortestSegment [SM sp1, SM sp2])) (ps++ps'))
or' ms _  = ms--trace ("or':"++show ms ++ show ms') undefined


rigidMatch :: Pattern -> Block -> VString -> (Matches,Split)
rigidMatch (QVar x) b vs= ([],SM (PatSplit (((nextBlock (P (b-1) (Left 0)) vs,[]),vs),[(x,((((nextBlock (P (b-1) (Left 0)) vs,[]),vs))))]) (QVar x)))--SM (StrSplit (((P b (Left 0),[]),vs),[(x,vs)]) (incrBlock (length vs) (P b (Left 0)), []) ))
rigidMatch p b []       = ([],SM (PatSplit (((P b (Left 0),[]),[]),[]) p ))
rigidMatch p b (v:vs)   = case (matchSegment p b v) of
  (ms,NoMatch)                       -> {-trace ("NoMatch" ++ show p ++ show v)-}(ms,NoMatch)
  (ms,SM (StrSplit m (pos,s)))       -> {-trace ("StrSplit "++ show v ++ "Next " ++show (m, pos, s++vs))-} (ms,SM (StrSplit m (pos,s++vs)))
  (ms,SM (PatSplit m p'))            -> {-trace ("PatSplit "++show p' ++ show m ++ show vs)-} appendMatches ms (match m p' (b+1) vs)
  (ms,PM sp@(StrSplit m (j,s')) ps) -> {-trace ("PM " ++ show ms ++ show sp ++ show ps ++ show vs)-} (case matchPatSplits (nextBlock (P (b-1) (Left 0)) vs) vs (eliminate p ps) of
     []    -> (ms,SM (StrSplit m (j,s'++vs)))--appendMatches (ms) (addMatch m (scan p (j,s'++vs)))
     (sps) -> (ms ++ (concatMap fst sps), SM (StrSplit m (getShortestSegment (((SM sp): (map snd sps)))))))
  (ms, sp)                     -> trace ("RigidMatch: "++show ms ++ " and "++show sp) undefined

matchSegment :: Pattern -> Block-> Segment -> (Matches,Split)
matchSegment p b (Str "")       = ([],SM (PatSplit (((P b (Left 0),[]),[Str ""]),[]) p ))
matchSegment p@(PChc _ _ _) b s =  (scanChoicePattern False p (nextBlock (P (b-1) (Left 0) ) [s]) s )
matchSegment p@(PChcStar _ _ _) b s =  (matchChoiceStarPattern p (nextBlock (P (b-1) (Left 0) ) [s]) s )
matchSegment p b (Str s)        = let ma = ([],matchStr p (P b (Left 0) ) s) in{- trace ("match segment STR " ++ show p ++ show s ++ " , " ++(show (snd ma)))-} ma
matchSegment p b (Chc d v1 v2)  = 
  let l = rigidMatch p 0 v1
      r = rigidMatch p 0 v2
  in {-trace ("MatchSegment " ++ show l ++ " , " ++ show r) -} combineAlternatives d (P b (Right (startPos,startPos))) (fst l,makeStrSplit (snd l) (nextBlock NoPos v1,v1))
   (fst r,makeStrSplit (snd r) (nextBlock NoPos v2,v2)) 

{-trace (show ((rigidMatch p 0 v1)) ++ " AND " ++ show (rigidMatch p 0 v2) ) snd $ 
  combineAlternatives d NoPos ([],(rigidMatch p 0 v1)) ([],(rigidMatch p 0 v2)) -}


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
  in ((concatMap (\m -> if m == emptyMatch then [] else [ addAlternative L d p m]) ms)++(map (addAlternative R d p) ms')++ms'',
   sp) --Check for overlapping

scanStr :: Pattern -> (Pos,String) -> Split
scanStr None (i,s)
  | s == ""                           = (SM (StrSplit (((i,[]),[Str ""]),[]) ( incOffset i,vStr "")))
  | otherwise                         = NoMatch 
scanStr p (i,matchStr p i-> SM s)     = {-trace (show s)-} (SM s)
scanStr p (i,matchStr p i-> PM s1 s2) = undefined
scanStr p (i,_:s)                     = (scanStr p (incOffset i,s))
scanStr _ (_,[])                      = NoMatch

matchStr :: Pattern -> Pos -> String -> Split
matchStr (QVar x) pos vs              = SM $ PatSplit (((pos,[]),[Str vs]),[(x,((pos,[]),[Str vs]))]) (QVar x)
matchStr (Plain (C d)) pos (c:s)
                     | c==d           = {-trace (show (pos) ++ (show c) ++ show (incOffset pos) ++ s++ "\n")-} SM $ StrSplit (((pos,[]),[Str[c]]),[]) 
                                                       (incOffset pos,vStr s)
matchStr (Plain Wild) pos (c:s)       = (SM $ StrSplit (((pos,[]),[Str[c]]),[]) 
                                                       (incOffset pos,vStr s))
matchStr (Seq p p') pos s             = case matchStr p pos s of
  NoMatch                              -> NoMatch
  sp@(SM (StrSplit m (pos',[Str s']))) -> sp `andThen` matchStr p' pos' s'
  sp@(SM (StrSplit m (pos',[]))) -> SM (PatSplit m p')
  (SM (PatSplit m p''))          -> SM (PatSplit m (Seq p'' p'))
  sp                             -> trace (show sp) undefined       
matchStr (Alt p q)    pos s           = matchStr p pos s `orElse` 
  matchStr q pos s
matchStr (PChc (_) p q) pos s         =
  let m1 = matchStr p pos s
      m2 = matchStr q pos s
  in {-trace("MatchStr: "++show m1++ show m2 ++ show p ++ show q ++ show s)-} m1 `and'` m2
matchStr (PChcStar (_) p q) pos s     = {-trace ("Match Str: "++show pos ++ show s)-} (matchStr p pos s `and''`
  matchStr q pos s)
matchStr (None) pos s                 
  | s == ""                           = (SM (StrSplit (((pos,[]),[Str ""]),[]) ( incOffset pos,vStr "")))
  | otherwise                         = NoMatch
matchStr p pos s                      = NoMatch
{-matchStr (Seq a p)    pos (c:[])      = matchStr a pos [c] `andThen` 
  (SM (PatSplit (((startPos,[]),[]),[]) p))
matchStr (Seq a p)    pos (c:s)       = matchStr a pos [c] `andThen` 
  matchStr p (incOffset pos) s-}



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
nextBlock NoPos (Chc _ l r : vs) = P 0 (Right (nextBlock NoPos l ,nextBlock NoPos r))
nextBlock (P b o) (Chc _ l r : vs) = P (b+1) (Right (nextBlock NoPos l ,nextBlock NoPos r))

incOffset :: Pos -> Pos
incOffset NoPos          = startPos
incOffset (P b (Left o)) = P b (Left (o+1)) 
incOffset p              = trace (show p) undefined

incBlock :: Int -> Pos -> Pos
incBlock _ NoPos   = NoPos
incBlock i (P b o) = P (b+i) o 

getLeftPos :: Pos -> Pos
getLeftPos NoPos               = startPos
getLeftPos (P _ (Left _))      = startPos
getLeftPos (P _ (Right (l,_))) = l

getRightPos :: Pos -> Pos
getRightPos NoPos               = startPos
getRightPos (P _ (Left _))      = startPos
getRightPos (P _ (Right (_,r))) = r

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
and' (SM (PatSplit m p)) (SM (StrSplit m' s))
  | m == m'             = SM (PatSplit m (PChc (D 0) p None))
  | otherwise           = NoMatch
and' (SM (StrSplit m' s)) (SM (PatSplit m p)) 
  | m == m'             = SM (PatSplit m (PChc (D 0) None p))
  | otherwise           = NoMatch
and' _ _                = NoMatch

and'' :: Split -> Split -> Split
and'' NoMatch _ = NoMatch
and'' _ NoMatch = NoMatch
and'' (SM (StrSplit m s)) (SM (StrSplit m' s')) 
  | s == s' && m==m'    = SM (StrSplit m' s')
  | otherwise           = NoMatch
and'' (SM (PatSplit m p)) (SM (PatSplit m' p'))
  | m == m'             = SM (PatSplit m' (PChcStar (D 0) p p'))
  | otherwise           = NoMatch
and'' (SM (PatSplit m p)) (SM (StrSplit m' s))
  | m == m'             = SM (PatSplit m (PChcStar (D 0) p None))
  | otherwise           = NoMatch
and'' (SM (StrSplit m' s)) (SM (PatSplit m p)) 
  | m == m'             = SM (PatSplit m (PChcStar (D 0) None p))
  | otherwise           = NoMatch
and'' _ _= NoMatch

appendVM :: FinalMatch -> FinalMatch -> FinalMatch
appendVM (((pos,dimEnv),vs),qVars) (((pos',dimEnv'),vs'),qVars') = 
  (((pos,noDupDim dimEnv dimEnv'),combineStr (vs++vs')),noDupVar qVars qVars')
  
noDupVar :: QVarEnv -> QVarEnv -> QVarEnv
noDupVar [] [] = []
noDupVar [] xs = xs
noDupVar xs [] = xs
noDupVar ((x,vs):xs) xs' = --[if x==y then mergeQVar else | (x,vs) <- xs, (y,vs') <- xs',]
  case find (\(a,_)-> x == a) xs' of
    Just (y,vs') -> (x,appendQVar vs vs') : noDupVar xs (xs'\\[(y,vs')])
    Nothing      -> (x,vs): noDupVar xs (xs') 
    
noDupDim :: DimEnv -> DimEnv -> DimEnv
noDupDim [] [] = []
noDupDim [] xs = xs
noDupDim xs [] = xs
noDupDim ((x,vs):xs) xs' = --[if x==y then mergeQVar else | (x,vs) <- xs, (y,vs') <- xs',]
  case find (\(a,_)-> x == a) xs' of
    Just (y,vs') -> (x,vs) : noDupDim xs (xs'\\[(y,vs')])
    Nothing      -> (x,vs): noDupDim xs (xs')


appendQVar :: VMatch -> VMatch -> VMatch
appendQVar ((pos,ds),vs) ((pos',ds'),vs') 
 | pos <= pos' = ((pos,ds++ds'),vs++vs')
 | otherwise   = ((pos',ds++ds'),vs'++vs)

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
  (((insertPos a p pos NoPos,dimEnv),[choice a d vs []]), updatePos a p d varEnv)

combineAlternative :: Dim -> DimTy -> Pos -> FinalMatch -> FinalMatch -> FinalMatch
combineAlternative d dy p m m'
 | isEmptyMatch m  || isEmptyMatch m' = emptyMatch
combineAlternative d dy pos ((((l,dimEnv),vs),varEnv)) ((((r,dimEnv'),vs'),varEnv')) = 
 (((insertPos Both pos l r,updateDimEnv dy d (dimEnv++dimEnv')),[Chc d vs vs']),(updatePos L pos d varEnv) ++ (updatePos R pos d varEnv'))

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
mergeAlt d i (PM (StrSplit m l) ps) NoMatch  = mergeAlt' (checkEmpty $ addAlternative L d i m, nonEmptyChc i d l (NoPos,[])) ps 
mergeAlt d i (PM (PatSplit m' p) ps) NoMatch = mergeAlt' ([],SM $ PatSplit (addAlternative L d i m') p) ps
mergeAlt d i NoMatch (PM (StrSplit m r) ps)  = mergeAlt' (checkEmpty $ addAlternative R d i m, nonEmptyChc i d (NoPos,[]) r) ps 
mergeAlt d i NoMatch (PM (PatSplit m' p) ps) = mergeAlt' ([],SM $ PatSplit (addAlternative R d i m') p) ps
--mergeAlt d i sp sp'                 = 
--  trace (show "Split1 "++show sp ++ ",Split2 " ++ show sp' ) undefined

mergeAlt' :: (Matches,Split) -> [SplitTy] -> (Matches,Split)
mergeAlt' s []                          = s
mergeAlt' (ms, NoMatch) ps              = 
  (ms, PM (StrSplit emptyMatch (NoPos,[]) ) (ps))
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
rewind (p,Str "" : vs)                           = (nextBlock p vs,vs)
rewind (p@(P b (Left o)), (Str [c]):vs)          = (nextBlock p vs {-P (b+1) (Left 0)-}, vs)
rewind (P b (Left o), (Str (s:ss)):vs)           = (P b (Left (o+1)), (Str ss) : vs)
rewind (p@(P b (Right (l,r))), (Chc d v1 v2):vs) = 
  case (rewind (l,v1), rewind (r,v2)) of
    ((l',[]),(r',[]))   -> (nextBlock p vs, vs)
    ((l',[]),(r',v2'))  -> (P b (Right (NoPos,r')), (Chc d [] v2') : vs)
    ((l',v1'),(r',[]))  -> (P b (Right (l',NoPos)), (Chc d v1' []) : vs)
    ((l',v1'),(r',v2')) -> (P b (Right (l',r')), (Chc d v1' v2') : vs)
rewind i = trace (show i) undefined
    
makeStrSplit :: Split -> Input -> Split
makeStrSplit NoMatch inp = (SM (StrSplit emptyMatch inp))
makeStrSplit sp _        = sp

checkEmpty :: FinalMatch -> [FinalMatch]
checkEmpty m 
 | m ==emptyMatch = []
 |otherwise       = [m]
 
getShortestSegment :: [Split] -> Input
getShortestSegment [] = (NoPos,[])
getShortestSegment (SM (StrSplit m (i,v)) : ss) = shortestSegment (i,v) (getShortestSegment ss)
getShortestSegment (_ : ss)                = getShortestSegment ss
     
shortestSegment :: Input -> Input -> Input
shortestSegment (j,s) (j',s') 
  | j < j'     = (j',s')
  | otherwise  = (j,s)
 

isEmptyMatch :: FinalMatch -> Bool
isEmptyMatch (((_,_),[]),_) = True
isEmptyMatch _              = False 
  
combine'' :: Pos -> Dim -> DimTy -> FinalMatch -> FinalMatch -> FinalMatch
combine'' p d dy (((l,dimVars),vs),qvars) (((r,dimVars'),vs'),qvars') = {-trace (show p ++ " | Left: "++ show l ++ "| Right: "++show r)-}
  (((insertPos Both p l r, updateDimEnv dy d (dimVars ++ dimVars')),[Chc d vs vs'] ), (updatePos L p d qvars) ++ (updatePos R p d qvars'))
  
updateDimEnv :: DimTy -> Dim -> DimEnv -> DimEnv
updateDimEnv (D _) _ dimenv = dimenv
updateDimEnv (DVar d) dim dimenv 
 | not $ dimPresent d dimenv = (d,dim) : dimenv
 | otherwise = dimenv

dimPresent :: String -> DimEnv -> Bool
dimPresent _ [] = False
dimPresent x ((d,val):ds) 
  | x == d = True
  | otherwise = dimPresent x ds

updatePos :: Alternative -> Pos -> Dim -> QVarEnv -> QVarEnv
updatePos _ _ _ []                         = []
updatePos a p d ((x,((pos,dimenv),vs)):qs) = 
  (x,((insertPos a p pos NoPos, dimenv),[choice a d vs []])) : updatePos a p d qs
  
getQVarEnv :: FinalMatch -> QVarEnv
getQVarEnv = snd

getPos :: FinalMatch -> Maybe Pos
getPos v = case getMatchVS v of
   [] -> Nothing
   otherwise -> Just $ (fst.fst.fst) v

getDimEnv :: FinalMatch -> DimEnv
getDimEnv = snd.fst.fst

getMatchVS :: FinalMatch -> VString
getMatchVS = snd.fst

getDim :: DimVarName -> DimEnv -> Maybe Int
getDim d ds = lookup d ds

makeCP :: Bool -> DimTy -> Pattern -> Pattern -> Pattern
makeCP False dy l r = PChc dy l r
makeCP True dy l r  = PChcStar dy l r 

eliminate :: Pattern -> [SplitTy] -> [SplitTy]
eliminate _ []= []
eliminate p (sp@(PatSplit _ p') : ps) 
  | p == p'   = eliminate p ps
  | otherwise = sp : eliminate p ps 

correctPos :: VString -> Pos -> Pos -> VString -> Pos  
correctPos [] i j vs = nextBlock i vs
correctPos _  _ j _  = j

isNone :: Pattern -> Bool
isNone None = True
isNone _    = False









