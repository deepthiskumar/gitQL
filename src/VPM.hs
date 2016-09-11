{-# LANGUAGE ViewPatterns #-}

module VPM where

import Prelude hiding (seq)


data Atomic = C Char | Wild deriving(Show)

data Pattern = Plain Atomic
             | Seq Atomic Pattern
             | Alt Pattern Pattern
             deriving(Show)


type VString = [Segment]

type Dim = Int

data Segment = Str String | Chc Dim VString VString
             deriving(Show)

type VMatch = (Pos,[Match])

data Match = MStr String | MChc Dim [VMatch] [VMatch]
            deriving(Show)

type Input = (Pos,VString)

type Pos = Int

data SplitTy = StrSplit VMatch Input
             | PatSplit VMatch Pattern
             deriving(Show)

data Split = NoMatch
           | SM SplitTy  --Single Match
           | PM SplitTy SplitTy --Parallel Match
           deriving(Show)

type Matches = [VMatch]


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
rigidMatch :: VMatch -> Pattern -> (Pos,Segment) -> Split
rigidMatch (j,m) p (i,s) = case matchSegment p (i,s) of
                   NoMatch                  -> SM (StrSplit (0,[]) (j+1,
                                                (proceed1 $ snd $ mToV (j,m)) ++ [s])) --new match
                   SM (StrSplit m' (j',s')) -> SM (StrSplit (appendVM (j,m) m') (j',s'))
                   SM (PatSplit m' p')      -> SM (PatSplit (appendVM (j,m) m') p')
                   PM s1 s2                 -> undefined

matchSegment :: Pattern -> (Pos,Segment) -> Split
matchSegment p (i, Str s') = matchStr p i s'
matchSegment p (i,vs@(Chc dim v1 v2)) = undefined


continue :: Pattern -> Pos -> VString -> Split -> Matches
continue p i v (SM (StrSplit m (j,[])))      = m: scan p (j,v) --next occurrence
continue p i v (SM (StrSplit m (j,s')))      = m: scan p (j,s'++v) --next occurrence
continue p i (s:ss) (SM (PatSplit (j,m) p')) = continue p (i+(lenIncr s)) ss
                                                (rigidMatch (j,m) p' (i,s))
continue p i v (PM s1 s2)                    = undefined
continue _ _ [] _                            = []

scanStr :: Pattern -> (Pos,String) -> Split
scanStr p (i,matchStr p i-> SM s)     = SM s
scanStr p (i,matchStr p i-> PM s1 s2) = undefined --TODO
scanStr p (i,_:s)                     = scanStr p (i+1,s)
scanStr _ (_,[])                      = NoMatch

scanSegment :: Pattern -> (Pos,Segment) -> Split
scanSegment p (i, Str s')            = scanStr p (i,s')
scanSegment p (i,vs@(Chc dim v1 v2)) = undefined --TODO

lenIncr :: Segment -> Int
lenIncr (Str s)     = length s
lenIncr (Chc _ _ _) = 1

scan :: Pattern -> Input -> Matches
scan p (i,[])  = []
scan p (i,s:ss) = case scanSegment p (i,s) of
                   NoMatch  -> scan p (i+(lenIncr s),ss)
                   split    -> continue p (i+(lenIncr s)) ss split

match :: Pattern -> VString -> Matches
match p s = scan p (0,s)


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
