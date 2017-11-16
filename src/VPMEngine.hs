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
    []     -> {-trace ("Continue")-} (case foreverPatterns p of --return match in case of QVar, None and Any
               True -> do 
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
    []     -> do 
                let (ms,ps') = checkForeverPatterns ps
                put (VPMEnv pat (i,[]) (matches env ++ms))
                case ps' of
                  [] -> return $ SM sp
                  [x] -> return $ SM x
                  oitherwise -> return (PM sp ps')
    (vs) -> do
              case matchPatSplits i vs ps of
                []  -> put (VPMEnv pat (j,s'++vs) (matches env))--(ms, extract $ SM (StrSplit m (j,s'++vs)))
                sps -> put (VPMEnv pat (getShortestSegment (((SM sp): (L.map snd sps)))) (matches env ++ (L.concatMap fst sps)))
              scan


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

scanSegment :: Pattern -> (Pos, Segment) -> VPMState Split
--scanSegment (QVar x) (i,s)     = return $ matchQVar x i s --these two cases donot have to go through the entire matching
--scanSegment (Any) (i,s)        = return $ matchAny i s
scanSegment p (i,Str s)          = return $  scanStr p (i,s)
--  | T.null s                   = return (SM $ PatSplit (VMatch (i,[]) [Str s] []) p) **Not matching unless it is "None"
--  | otherwise                  = return $  scanStr p (i,s)
scanSegment p c@(i,Chc _ _ _ ) = scanChoice p c

matchSegment :: VMatch -> Pattern -> (Pos, Segment) -> VPMState Split
--matchSegment m (QVar x)  (i,s)      = return $ prefixMatchSp m (matchQVar x i s) --these two cases donot have to go through the entire matching
--matchSegment m (Any) (i,s)          = return $ prefixMatchSp m (matchAny i s)
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
matchStr pat@(PChc d p q) pos s       = matchCPStr pat pos s
matchStr (None) pos s                 --When used as end of input to be matched
  | T.null s                          = (SM (StrSplit (VMatch (pos,[]) [Str T.empty] []) ( incOffset pos 1,vStr empty)))
  | otherwise                         = NoMatch
matchStr p pos s                      = NoMatch


matchStrConditions :: VMatch -> Pattern -> Pos -> Segment -> Split
matchStrConditions m p i (Str s) 
--    | T.null s                   = prefixMatchSp m (SM $ PatSplit (VMatch (i,[]) [Str s] []) p)
--    | otherwise                  
    = {-trace (show p ++ show s) -}(case matchStr p i s of
                                     NoMatch          -> case m == emptyMatch of
                                                          True  -> NoMatch
                                                          False -> SM $ StrSplit emptyMatch (rewind (fst $ metaInfo m, vstring m ++ [Str s]))
                                     SM(StrSplit m' s') -> SM $ StrSplit (appendVM m m') s'
                                     SM(PatSplit m' p') -> SM $ PatSplit (appendVM m m') p'
                                     PM _ _             -> undefined) --only for choices )           

----------------------------------Choices---------------------------------------------

scanChoice :: Pattern -> (Pos,Segment) -> VPMState Split
scanChoice p@(PChc _ _ _) c = matchCP emptyMatch p c
scanChoice p (i,Chc d l r)= do
  let (spl,sl) = runState scan (VPMEnv p (getLeftPos i l,l) [])
  let (spr,sr) = runState scan (VPMEnv p (getRightPos i r,r) [])
  env <- get
  let (ms, sp)  = mergeAlt d i spl spr
  let mat = ms ++ (inRight i d (matches sr)) ++ (inLeft i d (matches sl))++ matches env
  put (VPMEnv (pattern env) (input env) mat) --TODO update the input as well ** Done in Continue
  return sp

matchChoice :: VMatch -> Pattern -> (Pos,Segment) -> VPMState Split
matchChoice m (QVar x) (i,c) = (return $ matchQVar x i c)
matchChoice m Any (i,c)      = (return $ matchAny i c)
matchChoice m p@(PChc _ _ _) c = matchCP m p c
matchChoice m p (i, c@(Chc d l r)) = (do
  let (spl,sl) = runState match (VPMEnv p (getLeftPos i l,l) [])
  let (spr,sr) = runState match (VPMEnv p (getRightPos i r,r) [])
  env <- get
  let (ms, sp)  = mergeAlt d i spl spr
  let mat = (L.map (appendVM m) ((inRight i d (matches sr)) ++ (inLeft i d (matches sl))++ms)) ++ matches env
  case (sp, isEmptyMatch m) of
    (NoMatch, False) -> let s' =  (rewind (fst $ metaInfo m, (vstring m) ++ [c] ))
                        in put (VPMEnv (pattern env) (prependInput s' (input env)) mat)
    otherwise        -> put (VPMEnv (pattern env) (input env) mat) --TODO update the input as well ** Done in Continue
  return $ prefixMatchSp m sp)            

matchCP :: VMatch -> Pattern -> (Pos,Segment) -> VPMState Split
matchCP m p@(PChc (D dim) lp rp) (i,c@(Chc d l r)) 
  | dim == d = do
    let (spl,sl) = runState match (VPMEnv lp (getLeftPos i l,l) [])
    let (spr,sr) = runState match (VPMEnv rp (getRightPos i r,r) [])
    env <- get
    let (ms, sp) = mergeAltForCP i d (D dim) (matches sl,spl) (matches sr,spr)
    let mat = {-trace (show ms ++ show sp)-}(L.map (appendVM m) ms) 
    case (sp, isEmptyMatch m,mat) of
      (NoMatch, False, []) -> let s' =  (rewind (fst $ metaInfo m, (vstring m) ++ [c] ))
                              in put (VPMEnv (pattern env) (prependInput s' (input env)) (mat++ matches env))
      otherwise        -> put (VPMEnv (pattern env) (input env) (mat ++ matches env)) --TODO update the input as well ** Done in Continue
    return $ prefixMatchSp m sp
  | otherwise = do
    --See if it is nested
    let (spl,sl) = runState scan (VPMEnv p (getLeftPos i l,l) [])
    let (spr,sr) = runState scan (VPMEnv p (getRightPos i r,r) [])
    env <- get
    let (ms, sp)  = mergeAlt d i spl spr
    let mat = (L.map (appendVM m) ((inRight i d (matches sr)) ++ (inLeft i d (matches sl))++ms))
    case (sp, isEmptyMatch m, mat) of
      (NoMatch, False, []) -> let s' =  (rewind (fst $ metaInfo m, (vstring m) ++ [c] ))
                              in put (VPMEnv (pattern env) (prependInput s' (input env)) (mat++ matches env))
      otherwise        -> put (VPMEnv (pattern env) (input env) (mat++ matches env)) --TODO update the input as well ** Done in Continue
    return $ prefixMatchSp m sp
matchCP m p@(PChc (DVar dvar) lp rp) (i,c@(Chc d l r)) = do
  env <- get
  let (spl,sl) = runState (match) (VPMEnv lp (getLeftPos i l,l) [])
  let (spr,sr) = runState (match) (VPMEnv rp (getRightPos i r,r) [])
  let (ms, sp) = mergeAltForCP i d (DVar dvar) (matches sl,spl) (matches sr,spr)
  --search for nested choices as well
  let (spl',sl') = runState scan (VPMEnv p (getLeftPos i l,l) [])
  let (spr',sr') = runState scan (VPMEnv p (getRightPos i r,r) [])
  let (ms', sp') = mergeAlt d i spl' spr'
  let (fms, fsp) = ((ms', sp') `or'` (ms, sp))
  let mat = {-trace (show spl ++ show sl ++ "|"++ show spr ++ show sr ++ "|" ++ show ms ++ show sp ++ "|scan: " ++show ms' ++ show sp' ++ "|final:" ++ show fms ++ show fsp)-}(L.map (appendVM m) ((inLeft i d (matches sl')) ++ (inRight i d (matches sr')) ++ fms))
  case (fsp, isEmptyMatch m, []) of
      (NoMatch, False, []) -> let s' =  (rewind (fst $ metaInfo m, (vstring m) ++ [c] ))
                              in put (VPMEnv (pattern env) (prependInput s' (input env)) (mat++ matches env))
      otherwise        -> put (VPMEnv (pattern env) (input env) (mat++ matches env)) --TODO update the input as well ** Done in Continue
  return $ prefixMatchSp m fsp
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

incBlock :: Int -> Pos -> Pos
incBlock _ NoPos   = NoPos
incBlock i (P b o) = P (b+i) o

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
 
combine'' :: Pos -> Dim -> DimTy -> VMatch -> VMatch -> VMatch
combine'' p d dy m m'
 | isEmptyMatch m  || isEmptyMatch m' = emptyMatch
combine'' p d dy (VMatch (l,dimVars) vs qvars) (VMatch (r,dimVars') vs' qvars') = 
  (VMatch (insertPos p l r, updateDimEnv dy d (dimVars ++ dimVars')) [Chc d vs vs']  ((inLVar p d qvars) ++ (inRVar p d qvars')))
  
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

combineStr :: VString -> VString
combineStr []                              = []
combineStr [m]                             = [m]
combineStr ((Str s) : (Str s') : ms)       = combineStr ((Str (T.append s s')):ms)
combineStr (Str s : m1@(Chc _ _ _ ): ms)   = (Str s) : m1 : combineStr ms
combineStr (m1@(Chc _ _ _ ) :ms )          = m1 : combineStr ms

appendMatches :: Matches -> (Matches,Split) -> (Matches,Split)
appendMatches ms (ms',s) = (ms++ms',s)--(removeNoMatch ms ++ removeNoMatch ms',s)

vStr :: Text -> VString
vStr s  
  | T.null s    = []
  | otherwise   = [Str s]
  
and' :: DimTy -> Split -> Split -> Split
and' _ NoMatch _ = NoMatch
and' _ _ NoMatch = NoMatch
and' _ (SM (StrSplit m s)) (SM (StrSplit m' s')) 
  | s == s' && m==m'    = SM (StrSplit m' s')
  | otherwise           = NoMatch
and' d (SM (PatSplit m p)) (SM (PatSplit m' p'))
  | m == m'             = SM (PatSplit m' (PChc d p p'))
  | otherwise           = NoMatch
and' d (SM (PatSplit m p)) (SM (StrSplit m' s))
  | m == m'             = SM (PatSplit m (PChc d p None))
  | otherwise           = NoMatch
and' d (SM (StrSplit m' s)) (SM (PatSplit m p)) 
  | m == m'             = SM (PatSplit m (PChc d None p))
  | otherwise           = NoMatch
and' _ _ _              = NoMatch

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
inLVar p d varEnv = L.map (\(x,((p',d'),v))->(x,((insertPos p p' NoPos,d'),v))) varEnv

inRVar :: Pos -> Dim -> QVarEnv -> QVarEnv
inRVar p d varEnv = L.map (\(x,((p',d'),v))->(x,((insertPos p NoPos p',d'),v))) varEnv

inLandR :: Pos -> Dim -> VMatch -> VMatch -> VMatch
inLandR p d (VMatch (p',dim) vl ql) (VMatch (p'',dim') vr qr) = 
  VMatch (insertPos p p' p'', dim++dim') [Chc d vl vr] ((inLVar p d ql) ++ (inRVar p d qr))  
  
insertPos :: Pos -> Pos -> Pos -> Pos
insertPos (NoPos) l r = P 0 (Right (l,r))
insertPos (P b _) l r = P b (Right (l,r))

createCC :: Dim -> VString -> VString -> VString
createCC d [] [] = []
createCC d l r   = [Chc d l r]

mergeAltForCP ::  Pos -> Dim -> DimTy -> (Matches,Split) -> (Matches,Split) -> (Matches,Split)
mergeAltForCP pos d dy (ms,NoMatch) (ms',NoMatch) = 
  ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], NoMatch) 
mergeAltForCP pos d dy (ms,SM(StrSplit _ _)) (ms',NoMatch) =
  ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
mergeAltForCP pos d dy (ms,SM(PatSplit ma p)) (ms',NoMatch) 
   | foreverPatterns p = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
   | otherwise         = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch],NoMatch) --SM (PatSplit (combine'' pos d dy ma emptyMatch ) (PChc dy p None)))
mergeAltForCP pos d dy (ms,NoMatch) (ms',SM(StrSplit _ _))=
  ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
mergeAltForCP pos d dy (ms,NoMatch)  (ms',SM(PatSplit ma' p))
   | foreverPatterns p = ([combine'' pos d dy m m' | m <- ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
   | otherwise         = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)--SM (PatSplit (combine'' pos d dy emptyMatch ma' ) (PChc dy None p)))
mergeAltForCP pos d dy (ms,SM(StrSplit ma (l,s))) (ms',SM(StrSplit ma' (r,s')))
   | L.null s && L.null s' = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM $ StrSplit emptyMatch (insertPos pos l r,createCC d s s'))
   | otherwise             = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (insertPos pos l r,[])))
mergeAltForCP pos d dy (ms,SM(StrSplit ma (l,s))) (ms',SM(PatSplit ma' p))
   | foreverPatterns p && L.null s = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
   | foreverPatterns p = ([combine'' pos d dy m m' | m <- ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
   | otherwise         = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
mergeAltForCP pos d dy (ms,SM(PatSplit ma p)) (ms',SM(StrSplit ma' (r,s')))
   | foreverPatterns p && L.null s' = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (insertPos pos NoPos NoPos,[])))
   | foreverPatterns p = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
   | otherwise         = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], NoMatch)
mergeAltForCP pos d dy x@(ms,SM(PatSplit ma p)) y@(ms',SM(PatSplit ma' p'))
   | (foreverPatterns p) && (foreverPatterns p') = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (StrSplit emptyMatch (insertPos pos NoPos NoPos,[])))
--   | (foreverPatterns p) = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combine'' pos d dy emptyMatch ma' ) (PChc dy None p')))
--   | (foreverPatterns p') = ([combine'' pos d dy m m' | m <- ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combine'' pos d dy ma emptyMatch ) (PChc dy p None)))
   | otherwise            = ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch], SM (PatSplit (combine'' pos d dy ma ma' ) (PChc dy p p')))
mergeAltForCP pos d dy x@(ms,PM s ps) y@(ms',NoMatch) = trace ("1."++show x ++ show y)
  ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch],NoMatch)
mergeAltForCP pos d dy x@(ms,NoMatch) y@(ms',PM s ps) = trace ("2."++show x ++ show y)
  ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch],NoMatch)
mergeAltForCP pos d dy x@(ms,PM s ps) y@(ms',SM(StrSplit ma' (r,s'))) =  trace ("3."++show x ++ show y)
   ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch],NoMatch)
mergeAltForCP pos d dy x@(ms,SM(StrSplit ma (l,s))) y@(ms',PM s' ps)  =  trace ("4."++show x ++ show y) 
   ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch],NoMatch)
mergeAltForCP pos d dy x@(ms,PM s ps) y@(ms',SM(PatSplit ma' p'))-- =  trace ("5."++show x ++ show y) 
   | foreverPatterns p' = ([combine'' pos d dy m m' | m <- ms, m' <- ma':ms', m/= emptyMatch, m'/= emptyMatch],NoMatch)
mergeAltForCP pos d dy x@(ms,SM(PatSplit ma p)) y@(ms',PM s' ps) -- =  trace ("6."++show x ++ show y) 
   | foreverPatterns p = ([combine'' pos d dy m m' | m <- ma:ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch],NoMatch)
mergeAltForCP pos d dy x@(ms,PM s ps) y@(ms',PM s' ps')  =  trace ("7."++show x ++ show y)
   ([combine'' pos d dy m m' | m <- ms, m' <- ms', m/= emptyMatch, m'/= emptyMatch],NoMatch)
mergeAltForCP _ _ _ _ _ = ([],NoMatch)


combineMultiple :: Pos -> Dim -> DimTy -> [SplitTy] -> [SplitTy] -> (Matches, [SplitTy])
combineMultiple pos d dy ls rs = eliminateStr [mergeAltForCP pos d dy ([],SM l) ([],SM r) | l <- ls, r <- rs ]

eliminateStr :: [(Matches,Split)] -> (Matches, [SplitTy])
eliminateStr [] = ([],[])
eliminateStr ((ms,(SM (StrSplit m s))):ss) 
   | not $ isEmptyMatch m = let (ms',sp) = eliminateStr ss
                            in (m:(ms++ms'),sp)
   | otherwise            = let (ms',sp) = eliminateStr ss
                            in ((ms++ms'),sp)
eliminateStr ((ms,(SM pat@(PatSplit m p))):ss) = let (ms',sp) = eliminateStr ss
                                                 in ((ms++ms'),pat:sp)


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
prefixMatchSp m sp@(SM (StrSplit m' s)) 
  | not $ isEmptyMatch m'            = SM $ StrSplit (appendVM m m') s
  | otherwise                        = sp
prefixMatchSp m sp@(SM (PatSplit m' p)) 
  | not $ isEmptyMatch m'            = SM $ PatSplit (appendVM m m') p
  | otherwise                        = sp
prefixMatchSp m (PM sp ps)           = PM (splitTy $ prefixMatchSp m (SM sp)) (L.map (\s -> splitTy (prefixMatchSp m (SM s))) ps)

--shortest segment or longest match
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
matchPatSplits i vs (x@(PatSplit m p):ps) = 
   let (sp,st) = runState (firstMatch (SM x)) (VPMEnv p (i,vs) [])
   in {-trace (show (i,vs) ++ show x ++ "And" ++ show ps ++"\n")-} (case (matches st ,sp) of
        ([],NoMatch)           -> let s = rewind (fst $ metaInfo m, (vstring m) ++ vs) --Just return the remaining vs to be scanned
                                  in ([],SM $ StrSplit emptyMatch s) : matchPatSplits i vs ps
        (ms,NoMatch)           -> {-trace ("2. "++show ms)-} ((ms,SM $ StrSplit emptyMatch (input st) ): matchPatSplits i vs ps)
        (ms,sp@(SM (StrSplit (VMatch (_,[]) _ _) s))) -> {-trace ("3. "++show ms)-} ((ms,SM $ StrSplit emptyMatch (prependInput s (input st)) ): matchPatSplits i vs ps)
        (ms,SM(StrSplit m' s))       -> {-trace ("4. "++show ms ++ ","++ show m')-} ( ((m':ms),SM $ StrSplit emptyMatch (prependInput s (input st))) :matchPatSplits i vs ps)
        (ms,sp@(SM(PatSplit m' p'')))-> {-trace ("5. "++show ms)-} case foreverPatterns p'' of
                        True -> ((m':ms ,sp) : matchPatSplits i vs ps)
                        False -> ((ms ,sp) : matchPatSplits i vs ps)
--        (ms,sp@(PM (StrSplit (VMatch (_,[]) _ _) s) ps')) -> (L.map (appendVM m) ms,sp ): matchPatSplits i vs ps
--        (ms,sp@(PM (StrSplit m' s) ps')) -> (L.map (appendVM m) (m':ms), PM (StrSplit emptyMatch s) ps') : (matchPatSplits i vs ps)
        y -> trace (show y) undefined)

--same as continue but stop after first match
firstMatch :: Split -> VPMState Split
firstMatch NoMatch                 = return NoMatch
firstMatch s@(SM (StrSplit _ _))   = extract s --This should be resolved in extract
firstMatch s@(SM (PatSplit m p)) = do
  env <- get
  let pat = pattern env
  let (i,vs) = input env
  case vs of
    []     -> {-trace ("Continue")-} (case foreverPatterns p of --return match in case of QVar, None and Any
               True -> do 
                          put (VPMEnv pat (i,[]) (m:matches env))
                          return NoMatch
               _      -> return s)
    (v:vs) -> do
               put (VPMEnv pat (nextBlock i vs,vs) (matches env))
               sp <- matchSegment m p (i,v)
               sp' <- extract sp
               firstMatch sp'
firstMatch s@(PM sp@(StrSplit m (j,s')) ps) = do
  env <- get
  let pat = pattern env
  let (i,vs) = input env
  case vs of
    []     -> do 
                let (ms,ps') = checkForeverPatterns ps
                put (VPMEnv pat (i,[]) (matches env ++ms))
                return (PM sp ps')
    (vs) -> do
              case matchPatSplits i vs ps of
                []  -> put (VPMEnv pat (prependInput (j,s') (i,vs)) (matches env))--(ms, extract $ SM (StrSplit m (j,s'++vs)))
                sps -> put (VPMEnv pat (getShortestSegment (((SM (StrSplit m (prependInput (j,s') (i,vs)))): (L.map snd sps)))) (matches env ++ (L.concatMap fst sps)))
              return NoMatch


or' :: (Matches,Split) -> (Matches,Split) -> (Matches,Split)
or' ([],NoMatch) ms = ms
or' ms ([],NoMatch) = ms
or' (ms,sp1@(SM (StrSplit m s))) (ms',sp2@(SM (StrSplit m' s'))) =
  (L.concatMap checkEmpty [m,m'] ++ (ms++ms'),SM $ StrSplit emptyMatch (getShortestSegment ([sp1,sp2]) ))
or' (ms,(SM sp1@(PatSplit m p))) (ms',(SM sp2@(PatSplit m' p'))) =
  (ms++ms', PM (StrSplit emptyMatch (NoPos,[])) [sp1,sp2])
or' (ms,sp1@(SM (StrSplit m s))) (ms',(SM sp2@(PatSplit m' p'))) =
  (checkEmpty m ++ ms++ms', PM (StrSplit emptyMatch s) [sp2])
or' (ms,(SM sp1@(PatSplit m p))) (ms',sp2@(SM (StrSplit m' s'))) =
  (checkEmpty m' ++ ms++ms', PM (StrSplit emptyMatch s') [sp1])
or' (ms,sp1@(PM (StrSplit m s) ps)) (ms',(SM sp2@(PatSplit m' p'))) =
  (checkEmpty m ++ ms++ms', PM (StrSplit emptyMatch s) (sp2:ps))
or' (ms,sp1@(PM (StrSplit m s) ps)) (ms',sp2@(SM (StrSplit m' s'))) =
  (L.concatMap checkEmpty [m,m'] ++ ms++ms', PM (StrSplit emptyMatch (getShortestSegment [sp1,sp2])) ps)
or' (ms,(SM sp1@(PatSplit m p))) (ms',(PM (StrSplit m' s') ps')) =
  (checkEmpty m'++ ms++ms', PM (StrSplit emptyMatch s') (sp1:ps'))
or' (ms,sp1@(SM (StrSplit m s))) (ms',(PM sp2@(StrSplit m' s') ps'))  =
  (L.concatMap checkEmpty [m,m'] ++ms++ms', PM (StrSplit emptyMatch (getShortestSegment [sp1,SM sp2])) ps')
or' (ms,(PM sp1@(StrSplit m s) ps)) (ms',(PM sp2@(StrSplit m' s') ps')) = 
  (L.concatMap checkEmpty [m,m'] ++ms++ms', PM (StrSplit emptyMatch (getShortestSegment [SM sp1, SM sp2])) (ps++ps'))
or' ms _  = ms--trace ("or':"++show ms ++ show ms') undefined


matchCPStr :: Pattern -> Pos -> Text -> Split
matchCPStr p@(PChc d lp rp) i t
  | foreverPatterns p || foreverPatterns lp || foreverPatterns rp = NoMatch
matchCPStr p@(PChc d lp rp) i t =         
  let m1 = matchStr lp i t
      m2 = matchStr rp i t
  in case progressed lp rp m1 m2 of
       True -> and' d m1 m2
       False -> NoMatch
  
progressed :: Pattern -> Pattern -> Split -> Split -> Bool
progressed lp rp (SM (PatSplit m p)) (SM (PatSplit m' p'))
  | p == lp || p' == rp  = False
progressed _ _ _ _ = True


foreverPatterns :: Pattern -> Bool
foreverPatterns (QVar _)     = True
foreverPatterns Any          = True
foreverPatterns None         = True
foreverPatterns (PChc _ p q) = foreverPatterns p && foreverPatterns q
foreverPatterns _            = False


checkForeverPatterns :: [SplitTy] -> (Matches, [SplitTy])
checkForeverPatterns [] = ([],[])
checkForeverPatterns (s@(PatSplit m p):ps)
   | foreverPatterns p  = let (ms,sp) = checkForeverPatterns ps
                          in (m:ms,sp)
   | otherwise          = let (ms,sp) = checkForeverPatterns ps
                          in (ms, s:sp)



checkEmpty :: VMatch -> [VMatch]
checkEmpty m 
 | m == emptyMatch = []
 | otherwise       = [m]

splitTy :: Split -> SplitTy
splitTy (SM s) = s

dummySplitTy = StrSplit emptyMatch (NoPos,[])
-----------------------------------------------------------------------------------
--Doctests

-- |Match Plain Strings

