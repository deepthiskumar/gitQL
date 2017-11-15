{-# LANGUAGE ViewPatterns #-}
module VPMEngine where

import Prelude hiding (seq)
import Data.Maybe
import Data.List as L
import Data.Text as T
import Debug.Trace
import Types as Ty
import Control.Monad.Trans.State


{- SM - when matching against a plain string, only a pattern split is returned
   PM - When matching against a choice, all the matches can be combined into one
        String split and group all the pattern splits into one. -}
data Split = NoMatch
           | SM SplitTy  --Single Match
           | PM SplitTy [SplitTy] -- StrSplit and PatSplits
           deriving(Show)
           
data SplitTy = StrSplit VMatch Input
             | PatSplit VMatch Pattern
             deriving(Show)


data VPMEnv = VPMEnv{pattern :: Pattern,
                      input :: Input,
                      matches :: Matches} 
             deriving(Show)
             
type VPMState a = State VPMEnv a

vgrep :: Pattern -> VString -> Matches
vgrep p vs = L.reverse $ matches $ execState (scan) (VPMEnv p (nextBlock NoPos vs,vs) [])
             

scan ::  VPMState Split
scan = do
  env <- get
  let pat = pattern env
  let (i,inp) = input env
  case inp of
    []   -> return NoMatch
    v:vs -> do
            put (VPMEnv pat (nextBlock i vs,vs) (matches env))
            sp <- scanSegment pat (i,v)
            sp' <- extract sp
            continue sp'
            

continue :: Split -> VPMState Split
continue NoMatch               = scan
continue (SM (StrSplit _ _))   = undefined --This should be resolved in extract
continue s@(SM (PatSplit m p)) = do
  env <- get
  let pat = pattern env
  let (i,vs) = input env
  case vs of
    []     -> {-trace ("Continue")-} (case p of --return match in case of QVar, None and Any
               QVar x -> do 
                          put (VPMEnv pat (i,[]) (m:matches env))
                          return NoMatch
               Any    -> do
                          put (VPMEnv pat (i,[]) (m:matches env))
                          return NoMatch
               None   -> do
                          put (VPMEnv pat (i,[]) (m:matches env))
                          return NoMatch
               _      -> return s)
    (v:vs) -> do
               put (VPMEnv pat (nextBlock i vs,vs) (matches env))
               sp <- matchSegment m p (i,v)
               sp' <- extract sp
               continue sp'
continue s@(PM sp@(StrSplit m (j,s')) ps) = do
  env <- get
  let pat = pattern env
  let (i,vs) = input env
  case vs of
    []     -> return s
    (vs) -> do
              case matchPatSplits i vs ps of
                []  -> put (VPMEnv pat (j,s'++vs) (matches env))--(ms, extract $ SM (StrSplit m (j,s'++vs)))
                sps -> put (VPMEnv pat (getShortestSegment (((SM sp): (L.map snd sps)))) (matches env ++ (L.concatMap fst sps)))
              scan
                

scanSegment :: Pattern -> (Pos, Segment) -> VPMState Split
scanSegment p (i,Str s)        = return $  scanStr p (i,s)
scanSegment p c@(i,Chc _ _ _ ) = scanChoice p c

matchSegment :: VMatch -> Pattern -> (Pos, Segment) -> VPMState Split
matchSegment m p (i,Str s)          = {-trace ("MatchSegment "++show s ++ show p ++ show m++ show i)-} (return $ matchStrConditions m p i (Str s) )          
matchSegment m p (i, c@(Chc d l r)) = (matchChoice m p (i,c)) 

---------------------Plain string-----------------------------------------------
scanStr :: Pattern -> (Pos,Text) -> Split
scanStr None (i,s) 
  | T.null s                          = (SM (StrSplit (VMatch (i,[]) [Str empty] []) ( incOffset i 1,[])))
  | otherwise                         = NoMatch
scanStr p (i,matchStr p i-> SM s)     = (SM s)
scanStr p (i,matchStr p i-> PM s1 s2) = undefined
scanStr p (i,s)                     
  | T.null s                          = NoMatch
  | otherwise                         = (scanStr p (incOffset i 1,T.tail s))
   
matchStr :: Pattern -> Pos -> Text -> Split
matchStr (QVar x) pos s              = matchQVar x pos (Str s) 
matchStr Any pos s                   = matchAny pos (Str s)
matchStr (Plain (C d)) pos s
  | (not $ T.null s) && (T.head s)==d = SM $ StrSplit (VMatch (pos,[]) [Str (T.singleton$ T.head s)] []) 
                                             (incOffset pos 1,vStr (T.tail s))
matchStr (Plain Wild) pos s  
  | not $ T.null s   = (SM $ StrSplit (VMatch (pos,[]) [Str (T.singleton $ T.head s)] []) (incOffset pos 1,vStr (T.tail s)))
matchStr p@(Seq Any p') pos s         = matchSeqAny p pos s
matchStr p@(Seq (QVar x) p') pos s    = matchSeqQVar p pos s                                       
matchStr (Seq p p') pos s             = case matchStr p pos s of
  NoMatch                              -> NoMatch
  sp@(SM (StrSplit m (pos',[Str s']))) -> sp `andThen` matchStr p' pos' s'
  sp@(SM (StrSplit m (pos',[]))) -> SM (PatSplit m p')
  (SM (PatSplit m p''))          -> SM (PatSplit m (Seq p'' p'))
  sp                             -> trace (show sp) undefined       
matchStr (Alt p q)    pos s           = matchStr p pos s `orElse` matchStr q pos s
matchStr (PChc (_) p q) pos s         =
                                        let m1 = matchStr p pos s
                                            m2 = matchStr q pos s
                                        in m1 `and'` m2
matchStr (None) pos s                 --When used as end of input to be matched
  | T.null s                          = (SM (StrSplit (VMatch (pos,[]) [Str T.empty] []) ( incOffset pos 1,vStr empty)))
  | otherwise                         = NoMatch
matchStr p pos s                      = NoMatch


matchStrConditions :: VMatch -> Pattern -> Pos -> Segment -> Split
matchStrConditions m p i (Str s) = case matchStr p i s of
                                     NoMatch          -> case m == emptyMatch of
                                                          True  -> NoMatch
                                                          False -> SM $ StrSplit emptyMatch (rewind (fst $ metaInfo m, vstring m ++ [Str s]))
                                     SM(StrSplit m' s') -> SM $ StrSplit (appendVM m m') s'
                                     SM(PatSplit m' p') -> SM $ PatSplit (appendVM m m') p'
                                     PM _ _             -> undefined --only for choices )           

----------------------------------Choices---------------------------------------------

scanChoice :: Pattern -> (Pos,Segment) -> VPMState Split
scanChoice (QVar x) (i,c) = return $ matchQVar x i c
scanChoice Any (i,c)      = return $ matchAny i c
scanChoice p (i,Chc d l r)= do
  let (spl,sl) = runState scan (VPMEnv p (getLeftPos i l,l) [])
  let (spr,sr) = runState scan (VPMEnv p (getRightPos i r,r) [])
  env <- get
  let (ms, sp)  = mergeAlt d i spl spr
  let mat = ms ++ (inRight i d (matches sr)) ++ (inLeft i d (matches sl))++ matches env
  put (VPMEnv (pattern env) (input env) mat) --TODO update the input as well
  return sp

matchChoice :: VMatch -> Pattern -> (Pos,Segment) -> VPMState Split
matchChoice m (QVar x) (i,c) = (return $ matchQVar x i c)
matchChoice m Any (i,c)      = (return $ matchAny i c)
matchChoice m p (i, Chc d l r) = (do
  let (spl,sl) = runState match (VPMEnv p (getLeftPos i l,l) [])
  let (spr,sr) = runState match (VPMEnv p (getRightPos i r,r) [])
  env <- get
  let (ms, sp)  = mergeAlt d i spl spr
  let mat = L.map (appendVM m) ((inRight i d (matches sr)) ++ (inLeft i d (matches sl))++ms)
  put (VPMEnv (pattern env) (input env) (mat ++ matches env)) --TODO update the input as well
  return $ prefixMatchSp m sp)

match :: VPMState Split
match = do
  env <- get
  let pat = pattern env
  let (i,inp) = input env
  case inp of
    []   -> (return NoMatch)
    v:vs -> do
            put (VPMEnv pat (nextBlock i vs,vs) (matches env))
            sp <- matchSegment emptyMatch pat (i,v)
            return sp 
--------------------------------------------------------------------------------------
--Helpers

nextBlock :: Pos -> VString -> Pos
nextBlock p []    = p
nextBlock NoPos (Str _ : vs) = startPos
nextBlock (P b o) (Str _ : vs) = P (b+1) (Left 0)
nextBlock NoPos (Chc _ l r : vs) = P 0 (Right (nextBlock NoPos l ,nextBlock NoPos r))
nextBlock (P b o) (Chc _ l r : vs) = P (b+1) (Right (nextBlock NoPos l ,nextBlock NoPos r))

incOffset :: Pos -> Int -> Pos
incOffset NoPos _          = startPos
incOffset (P b (Left o)) i = P b (Left (o+i)) 
incOffset p _              = trace (show p) undefined

andThen :: Split -> Split-> Split
andThen NoMatch _                                 = NoMatch
andThen _       NoMatch                           = NoMatch
andThen (SM (StrSplit m s)) (SM (PatSplit m' p))  = 
  SM (PatSplit (appendVM m m') p)
andThen (SM (StrSplit m s)) (SM (StrSplit m' s')) = 
  SM (StrSplit (appendVM m m') s')
andThen _                   _                     = undefined

orElse :: Split -> Split -> Split
orElse NoMatch s   = s
orElse s _         = s --return both the matches like the unix grep?

appendVM :: VMatch -> VMatch -> VMatch
appendVM m v 
   | isEmptyMatch m  = v
appendVM (VMatch (pos,dimEnv) vs qVars) (VMatch (pos',dimEnv') vs' qVars') = 
  (VMatch (pos,noDupDim dimEnv dimEnv') (combineStr (vs++vs')) (noDupVar qVars qVars'))


noDupVar :: QVarEnv -> QVarEnv -> QVarEnv
noDupVar [] [] = []
noDupVar [] xs = xs
noDupVar xs [] = xs
noDupVar ((x,vs):xs) xs' = --[if x==y then mergeQVar else | (x,vs) <- xs, (y,vs') <- xs',]
  case L.find (\(a,_)-> x == a) xs' of
    Just (y,vs') -> (x,appendQVar vs vs') : noDupVar xs (xs'\\[(y,vs')])
    Nothing      -> (x,vs): noDupVar xs (xs') 
    
noDupDim :: DimEnv -> DimEnv -> DimEnv
noDupDim [] [] = []
noDupDim [] xs = xs
noDupDim xs [] = xs
noDupDim ((x,vs):xs) xs' = --[if x==y then mergeQVar else | (x,vs) <- xs, (y,vs') <- xs',]
  case L.find (\(a,_)-> x == a) xs' of
    Just (y,vs') -> (x,vs) : noDupDim xs (xs'\\[(y,vs')])
    Nothing      -> (x,vs): noDupDim xs (xs')


appendQVar :: (MetaInfo, VString) -> (MetaInfo, VString) -> (MetaInfo, VString)
appendQVar ((pos,ds),vs) ((pos',ds'),vs') 
 | pos <= pos' = ((pos,ds++ds'),vs++vs')
 | otherwise   = ((pos',ds++ds'),vs'++vs)

combineStr :: VString -> VString
combineStr []                              = []
combineStr [m]                             = [m]
combineStr ((Str s) : (Str s') : ms)     = combineStr ((Str (T.append s s')):ms)
combineStr (Str s : m1@(Chc _ _ _ ): ms) = (Str s) : m1 : combineStr ms
combineStr (m1@(Chc _ _ _ ) :ms )         = m1 : combineStr ms

appendMatches :: Matches -> (Matches,Split) -> (Matches,Split)
appendMatches ms (ms',s) = (ms++ms',s)--(removeNoMatch ms ++ removeNoMatch ms',s)

vStr :: Text -> VString
vStr s  
  | T.null s    = []
  | otherwise   = [Str s]
  
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

extract :: Split -> VPMState Split
extract NoMatch        = (return NoMatch)
extract sp@(SM (StrSplit m s)) 
  | isEmptyMatch m     = do 
                          env <- get; 
                          put $ VPMEnv (pattern env) (prependInput s (input env)) (matches env)
                          return NoMatch
  | otherwise          = do
                          env <- get
                          put $ VPMEnv (pattern env) (prependInput s (input env)) (m : matches env)
                          return NoMatch
extract sp             = return sp

isEmptyMatch :: VMatch -> Bool
isEmptyMatch (VMatch (_,_) [] _) = True
isEmptyMatch _                   = False 

prependInput :: Input -> Input -> Input
prependInput (i,[]) n                         = n
prependInput (i,v@((Chc _ [] []):vs)) (i',v') = (nextBlock i (vs++v'), vs++v')
prependInput (i,v) (i',v')                    = (i, (v++v'))

rewind :: Input -> Input
rewind (p,[])                                    = (p,[]) 
rewind (p@(P b (Left o)), (Str ss):vs) 
  | T.null ss          = (nextBlock p vs,vs)
  | T.length ss == 1   = (nextBlock p vs {-P (b+1) (Left 0)-}, vs)
  | otherwise          = (P b (Left (o+1)), (Str (T.tail ss)) : vs)
rewind (p@(P b (Right (l,r))), (Chc d v1 v2):vs) = 
  case (rewind (l,v1), rewind (r,v2)) of
    ((l',[]),(r',[]))   -> (nextBlock p vs, vs)
    ((l',[]),(r',v2'))  -> (P b (Right (NoPos,r')), (Chc d [] v2') : vs)
    ((l',v1'),(r',[]))  -> (P b (Right (l',NoPos)), (Chc d v1' []) : vs)
    ((l',v1'),(r',v2')) -> (P b (Right (l',r')), (Chc d v1' v2') : vs)
rewind i = trace (show i) undefined

matchAny :: Pos -> Segment -> Split
matchAny pos s =  SM $ PatSplit (VMatch (pos,[]) [s] []) (Any)

matchQVar :: String -> Pos -> Segment -> Split
matchQVar x pos s =  SM $ PatSplit (VMatch (pos,[]) [s] [(x,((pos,[]),[s]))]) (QVar x)

matchSeqAny :: Pattern -> Pos -> Text -> Split
matchSeqAny p pos vs
  | T.null vs                     = SM $ PatSplit (VMatch (pos,[]) [Str vs] []) p 
matchSeqAny p@(Seq Any p') pos vs =  case matchStr p' pos vs of --Special case of Any
  NoMatch -> case (matchStr Any pos (T.singleton $ T.head vs)) of
               sp@(SM (PatSplit m Any)) -> SM (StrSplit m (pos,[])) `andThen` matchStr p (incOffset pos 1) (T.tail vs)
               otherwise                -> undefined --it will never match this because of case 2 in matchStr
  sp      -> sp
  
matchSeqQVar :: Pattern -> Pos -> Text -> Split
matchSeqQVar p@(Seq (QVar x) p') pos vs
  | T.null vs                     = SM $ PatSplit (VMatch (pos,[]) [Str vs] [(x,((pos,[]),[Str vs]))]) p 
matchSeqQVar p@(Seq (QVar x) p') pos vs =  case matchStr p' pos vs of --Special case of Any
  NoMatch -> case (matchStr (QVar x) pos (T.singleton $ T.head vs)) of
               sp@(SM (PatSplit m (QVar x'))) -> SM (StrSplit m (pos,[])) `andThen` matchStr p (incOffset pos 1) (T.tail vs)
               otherwise                      -> undefined --it will never match this because of case 2 in matchStr
  sp      -> sp

getLeftPos :: Pos -> VString -> Pos
getLeftPos NoPos vs               = nextBlock NoPos vs
getLeftPos (P _ (Left _)) vs      = nextBlock NoPos vs
getLeftPos (P _ (Right (l,_))) vs = l

getRightPos :: Pos -> VString -> Pos
getRightPos NoPos vs               = nextBlock NoPos vs
getRightPos (P _ (Left _)) vs      = nextBlock NoPos vs
getRightPos (P _ (Right (_,r))) vs = r

inLeft :: Pos -> Dim -> Matches -> Matches
inLeft _ _ []      = []
inLeft p d (m:ms) 
 | m == emptyMatch = inLeft p d ms
inLeft p d ((VMatch (pos,dimEnv) vs varEnv):ms) = (VMatch (insertPos p pos NoPos,dimEnv) 
                                                           [Chc d vs []]
                                                           (inLVar p d varEnv)) : inLeft p d ms
                                                               
inRight :: Pos -> Dim -> Matches -> Matches
inRight _ _ []      = []
inRight p d (m:ms) 
 | m == emptyMatch = inRight p d ms
inRight p d ((VMatch (pos,dimEnv) vs varEnv):ms) = (VMatch (insertPos p NoPos pos,dimEnv) 
                                                           [Chc d [] vs]
                                                           (inRVar p d varEnv)) : inRight p d ms
inLVar :: Pos -> Dim -> QVarEnv -> QVarEnv
inLVar p d varEnv = L.map (\(x,((p',d'),v))->(x,((insertPos p p' NoPos,d'),[Chc d v []]))) varEnv

inRVar :: Pos -> Dim -> QVarEnv -> QVarEnv
inRVar p d varEnv = L.map (\(x,((p',d'),v))->(x,((insertPos p NoPos p',d'),[Chc d [] v]))) varEnv

inLandR :: Pos -> Dim -> VMatch -> VMatch -> VMatch
inLandR p d (VMatch (p',dim) vl ql) (VMatch (p'',dim') vr qr) = 
  VMatch (insertPos p p' p'', dim++dim') [Chc d vl vr] ((inLVar p d ql) ++ (inRVar p d qr))  
  
insertPos :: Pos -> Pos -> Pos -> Pos
insertPos (NoPos) l r = P 0 (Right (l,r))
insertPos (P b _) l r = P b (Right (l,r))

{-combineSplit :: Pos -> Dim -> Split -> Split -> (Matches, Split)
combineSplit p d NoMatch NoMatch = ([],NoMatch)
combineSplit p d NoMatch sp      = ([],inRightSp p d sp)
combineSplit p d sp NoMatch      = ([],inLeftSp p d sp)
combineSplit p d (SM(StrSplit m (i,s))) (SM(StrSplit m' (i',s'))) = 
   (((inLeft p d [m]) ++ (inRight p d [m])),SM $ StrSplit emptyMatch (insertPos p i i', createChoice d s s'))
combineSplit p d (SM x@(PatSplit m p')) (SM y@(PatSplit m' p''))
   | m == emptyMatch && m' ==emptyMatch = ([],NoMatch) 
   | p' == p''  = SM $ PatSplit (inLandR p d m m') p'
   | otherwise  = PM $ dummySplitTy ((inLSplitTys p d [x])++(inRSplitTys p d [y]))
combineSplit p d sp@(SM x@(PatSplit m p')) (SM (StrSplit m'' (i,s))) 
   | L.null s   = ((inRight p d [m'']),inLeftSp p d sp)      
   | otherwise  = ((inRight p d [m'']),PM (StrSplit emptyMatch (insertPos p NoPos i),[Chc d [] s]) (inLSplitTys p d [x]))
combineSplit p d (SM (StrSplit m'' (i,s))) sp@(SM x@(PatSplit m p'))
   | L.null s   = ((inLeft p d [m'']),inRightSp p d sp)      
   | otherwise  = ((inLeft p d [m'']),PM (StrSplit emptyMatch (insertPos p i NoPos),[Chc d s []]) (inRSplitTys p d [x]))
combineSplit p d (PM s ss)-}

--wrap the matches within split with the choice and create a single split

mergeAlt :: Dim -> Pos -> Split -> Split ->  (Matches,Split)
mergeAlt d i NoMatch NoMatch        = {-trace ("merge 1")-}([],NoMatch)
mergeAlt d i sp@(SM _ ) NoMatch     = {-trace ("merge 2" )-}([], inLeftSp i d sp)
mergeAlt d i NoMatch sp@(SM _ )     = {-trace ("merge 3")-}([], inRightSp i d sp)
mergeAlt d i (SM s1) (SM s2)        = {-trace ("merge 4")-} (mergeAlt' (wrapChoice d i s1 s2) [])
mergeAlt d i (SM s1) (PM s2 ps)     = {-trace ("merge 5")-} (mergeAlt' (wrapChoice d i s1 s2) ps)
mergeAlt d i (PM s1 ps) (SM s2)     = {-trace ("merge 6")-} (mergeAlt' (wrapChoice d i s1 s2) ps)
mergeAlt d i (PM s1 ps) (PM s2 ps') = {-trace ("merge 7")-} (mergeAlt' (wrapChoice d i s1 s2) (ps++ps'))
mergeAlt d i (PM (StrSplit m (l,s)) ps) NoMatch  = {-trace ("merge 8") -}(mergeAlt' (inLeft i d [m], SM $ StrSplit emptyMatch (insertPos i l NoPos,[Chc d s []])) ps )
mergeAlt d i (PM (PatSplit m' p) ps) NoMatch     = {-trace ("merge 9") -}(mergeAlt' ([],SM $ PatSplit (L.head $ inLeft i d [m']) p) ps)
mergeAlt d i NoMatch (PM (StrSplit m (r,s)) ps)  = {-trace ("merge 10") -}(mergeAlt' ((inRight i d [m]), SM $ StrSplit emptyMatch (insertPos i NoPos r,[Chc d [] s])) ps )
mergeAlt d i NoMatch (PM (PatSplit m' p) ps)     = {-trace ("merge 11") -}(mergeAlt' ([],SM $ PatSplit (L.head $ inRight i d [m']) p) ps)
--mergeAlt d i sp sp'                 = 
--  trace (show "Split1 "++show sp ++ ",Split2 " ++ show sp' ) undefined

mergeAlt' :: (Matches,Split) -> [SplitTy] -> (Matches,Split)
mergeAlt' s []                          = s
mergeAlt' (ms, NoMatch) ps              = (ms, PM dummySplitTy (ps))
mergeAlt' (ms, SM s1@(StrSplit _ _)) ps = (ms, PM s1 ps)
mergeAlt' (ms, SM s1@(PatSplit _ _)) ps = (ms, PM dummySplitTy (s1:ps))
mergeAlt' (ms, PM s ps') ps             = (ms, PM s (ps'++ps)) 


wrapChoice :: Dim -> Pos -> SplitTy -> SplitTy -> (Matches, Split)
wrapChoice d pos (StrSplit m (i,s) ) (StrSplit m' (i',s'))      = {-trace ("Wrap Choice : "++ (show $ pos))-}
  (((inLeft pos d [m]) ++ (inRight pos d [m])),SM $ StrSplit emptyMatch (insertPos pos i i', createChoice d s s'))
wrapChoice d pos (StrSplit m (i,s) ) y@(PatSplit m' p) = ((inLeft pos d [m]),inRightSp pos d (SM y))
wrapChoice d pos y@(PatSplit m p) (StrSplit m' _ )     = ((inRight pos d [m']),inLeftSp pos d (SM y))
wrapChoice d pos (PatSplit m p) (PatSplit m' p')       
  | p == p' = ([],SM $ PatSplit (inLandR pos d m m') p')
  | otherwise = 
   let p1 = PatSplit (L.head $ inLeft pos d [m]) p
       p2 = PatSplit (L.head $ inRight pos d [m']) p'
   in ([], PM dummySplitTy [p1,p2])


{-inLSplitTys :: Pos -> Dim -> [SplitTy] -> [SplitTy]
inLSplitTys p d [] = []
inLSplitTys p d (s:ss) = case inLeftSp p d (SM s) of
  NoMatch -> inLSplitTys p d ss
  SM sp   -> sp : inLSplitTys p d ss
  
inRSplitTys :: Pos -> Dim -> [SplitTy] -> [SplitTy]
inRSplitTys p d [] = []
inRSplitTys p d (s:ss) = case inRightSp p d (SM s) of
  NoMatch -> inRSplitTys p d ss
  SM sp   -> sp : inRSplitTys p d ss -} 


inLeftSp :: Pos -> Dim -> Split -> Split
inLeftSp p d (SM (StrSplit m (pos,s))) 
  | m == emptyMatch && L.null s = NoMatch
  | m == emptyMatch = SM $ StrSplit m (insertPos p pos NoPos,[Chc d s []] )
  | L.null s        = SM $ StrSplit (L.head $ inLeft p d [m]) (insertPos p pos NoPos, [])
  | otherwise       = SM $ StrSplit (L.head $ inLeft p d [m]) (insertPos p pos NoPos,[Chc d s []] )
inLeftSp p d sp@(SM (PatSplit m p'))
  | m == emptyMatch   = sp 
  | otherwise         = SM $ PatSplit (L.head $ inLeft p d [m]) p'
inLeftSP p d (PM _ _) = undefined

inRightSp :: Pos -> Dim -> Split -> Split
inRightSp p d (SM (StrSplit m (pos,s))) 
  | m == emptyMatch && L.null s = NoMatch
  | m == emptyMatch = SM $ StrSplit m (insertPos p NoPos pos,[Chc d [] s] )
  | L.null s        = SM $ StrSplit (L.head $ inRight p d [m]) (insertPos p NoPos pos, [])
  | otherwise       = SM $ StrSplit (L.head $ inRight p d [m]) (insertPos p NoPos pos,[Chc d [] s] )
inRightSp p d sp@(SM (PatSplit m p'))
  | m == emptyMatch   = sp 
  | otherwise         = SM $ PatSplit (L.head $ inRight p d [m]) p'
inRightSP p d (PM _ _) = undefined

createChoice :: Dim -> VString -> VString -> VString
createChoice d [] [] = []
createChoice d l r   = [Chc d l r]

prefixMatchSp :: VMatch -> Split -> Split
prefixMatchSp m s 
  | m == emptyMatch                  = s
prefixMatchSp m NoMatch              = SM $ StrSplit emptyMatch (rewind (fst $ metaInfo m, vstring m))
prefixMatchSp m (SM (StrSplit m' s)) = SM $ StrSplit (appendVM m m') s
prefixMatchSp m (SM (PatSplit m' p)) = SM $ PatSplit (appendVM m m') p
prefixMatchSp m (PM sp ps)           = PM (splitTy $ prefixMatchSp m (SM sp)) (L.map (\s -> splitTy (prefixMatchSp m (SM s))) ps)


getShortestSegment :: [Split] -> Input
getShortestSegment [] = (NoPos,[])
getShortestSegment (SM (StrSplit m (i,v)) : ss) = shortestSegment (i,v) (getShortestSegment ss)
getShortestSegment (_ : ss)                = getShortestSegment ss
     
shortestSegment :: Input -> Input -> Input
shortestSegment (j,s) (j',s') 
  | j < j'     = (j',s')
  | otherwise  = (j,s)
  
matchPatSplits :: Pos -> VString -> [SplitTy] -> [(Matches,Split)]
matchPatSplits _ _ [] = []
matchPatSplits i vs ((PatSplit m p):ps) = 
   let (sp,st) = runState match (VPMEnv p (i,vs) [])
   in case (matches st ,sp) of
        ([],NoMatch)           -> let s = rewind (fst $ metaInfo m, (vstring m) ++ vs) --Just return the remaining vs to be scanned
                   in ([],SM $ StrSplit emptyMatch s) : matchPatSplits i vs ps
        (ms,sp@(SM (StrSplit (VMatch (NoPos,[]) [] []) s))) -> (L.map (appendVM m) ms,sp ): matchPatSplits i vs ps
        (ms,SM(StrSplit m' s)) -> (L.map (appendVM m) (m':ms),SM $ StrSplit emptyMatch s) :matchPatSplits i vs ps
        (ms,sp@(SM(PatSplit m' p'')))-> (L.map (appendVM m) ms ,sp) : matchPatSplits i vs ps


splitTy :: Split -> SplitTy
splitTy (SM s) = s

dummySplitTy = StrSplit emptyMatch (NoPos,[])
-----------------------------------------------------------------------------------
--Doctests

-- |Match Plain Strings

