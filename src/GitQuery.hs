module GitQuery where

--import VPMNewTest
import Prelude hiding (filter)
import VPMEngine
import VPMNewTest as T
import Types

--Example 1: Find all the commits that affected a method foo

-- Pattern: d<*foo*,$x>
-- $x will refer to the variational string in the right alternative.
-- Using this value, one could simply display, count or perform another
-- query
-- ms = vgrep d<*foo*,$x> vs

-- To count : count $x from vgrep d<*foo*,$x> vs  :: Int
-- count (qvar "x" ms)
-- To Show  : pretty $x from vgrep d<*foo*,$x> vs    :: String
-- pretty (qvar "x" ms)
-- Query    : vgrep pat $x from vgrep d<*foo*,$x> vs :: VMatch
-- vgrepAgain bar (qvar "x" ms)

-- All of the above functions map their respective functionality to each match
-- in the list of matches returned by vgrep


-- Exmaple 2: Were all the occurrences of the method "foo" renamed
-- to "bar"

-- filter ("bar" `notIn` $x) from vgrep d<foo,$x> vs 
-- (notIn :: Pattern -> VString -> Bool)
-- (in :: Pattern -> VString -> Bool)
-- ms = vgrep d<foo,$x> vs
-- vgrepagain bar (qVar "x" ms)

-- Example 3: Find names that have been renamed differently in different 
-- branches

-- filter ($x /= $y) from vgrep d<d'<foo,$x>,$y>
-- ms = vgrep d<d'<foo,$x>,$y> vs
-- filter (FilterMatch (/=) (qVar "x" ms) (qVar "y" ms) )


data Filter = FilterDim DimVarName DimFilters --d (>= 100)
            -- | FilterQVar QVarName QVarName (FinalMatch -> FinalMatch -> Bool)
            | FilterMatch (VMatch -> VMatch -> Bool)
            
data DimFilters = Comp (Int->Bool)
                | Top Int --sort based on dimension


countAny :: [a] -> Int
countAny = length

pretty :: (Show a) => [a] -> String
pretty = concatMap show


--Combining two vgrep result

oR :: Matches -> Matches -> Matches
oR [] ms = ms
oR ms _ = ms


aND :: Matches -> Matches -> Matches
aND [] []  = []
aND ms []  = []
aND [] ms  = []
aND ms ms' = ms ++ ms'


--TODO what to use?? $x or the whole match? or both? Check the chat bot command language
qVar :: String -> Matches -> Matches
qVar x []          = []
qVar x ((VMatch _ _ []):ms) = qVar x ms
qVar x ((VMatch p v ((q,m):qs)):ms) 
  | x == q         = (VMatch (fst m) (snd m) []) : qVar x ((VMatch p v qs):ms)
  | otherwise      = qVar x ((VMatch p v qs):ms)

vString :: (MetaInfo,VString) -> Input
vString ((pos,_),vs) = (pos,vs)

getMatch :: VMatch -> Input
getMatch (VMatch (pos,_) vs _) = (pos,vs)

listDim :: VString -> [Dim]
listDim [] = []
listDim (Str _ : vs) = listDim vs
listDim (Chc d _ _ : vs) = d: listDim vs

getDims :: String -> Matches -> [Dim]
getDims x [] = []
getDims x ((VMatch (p,d) s q):ms) = 
 (concatMap (\(d',dim)-> if d' == x then [dim] else [] ) d) ++
  getDims x ms

-- Sub query
vgrepAgain :: Pattern -> [Input] -> Matches
vgrepAgain _ [] = []
vgrepAgain p (i:is) = (vgrepWithPos p i) ++ vgrepAgain p is

--Predicates

--filters
filter :: Filter -> Matches -> Matches -> Matches
filter _ [] [] = []
filter ft@(FilterDim d (Comp f)) (m:ms) _ = 
  case getDim d (getDimEnv m) of
    Just dim -> case f dim of
      True  -> m : filter ft ms []
      False -> filter ft ms []
    Nothing -> filter ft ms []
filter ft@(FilterDim d (Top i)) ms _ = 
  undefined --TODO implement sort-k O(n) algorithm
filter ft@(FilterMatch f) ms ms'  = 
  concat [ [m,m'] | m<-ms, m'<-ms', f m m']
{-filter ft@(FilterQVar q r f) ms ms' = 
  let nm = qVar q ms
      nm'= qVar r ms
  in concat [ [m,m'] | m<-nm, m'<-nm',  f m m']-}
--Helpers

getDimEnv :: VMatch -> DimEnv
getDimEnv (VMatch (_,dimenv) _ _) = dimenv

getDim :: DimVarName -> DimEnv -> Maybe Int
getDim d ds = lookup d ds



