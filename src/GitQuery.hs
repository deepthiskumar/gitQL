module GitQuery where

--import VPMNewTest
import VPMNew

--Example 1: Find all the commits that affected a method foo

-- Pattern: d<*foo*,$x>
-- $x will refer to the variational string in the right alternative.
-- Using this value, one could simply display, count or perform another
-- query

-- To count : count $x from vgrep d<*foo*,$x> vs  :: Int
-- To Show  : pretty $x from vgrep d<*foo*,$x> vs    :: String
-- Query    : vgrep pat $x from vgrep d<*foo*,$x> vs :: VMatch

--All of the above functions map their respective functionality to each match
-- in the list of matches returned by vgrep


-- Exmaple 2: Were all the occurrences of the method "foo" renamed
-- to "bar"

-- filter ("bar" `notIn` $x) from vgrep d<foo,$x> vs 
-- (notIn :: Pattern -> VString -> Bool)
-- (in :: Pattern -> VString -> Bool)

-- Example 3: Find names that have been renamed differently in different 
-- branches

-- filter ($x /= $y) from vgrep d<d'<foo,$x>,$y>

countAny :: [a] -> Int
countAny = length

pretty :: (Show a) => [a] -> String
pretty = concatMap show


--Combining two vgrep result

oR :: Matches -> Matches -> Matches
oR ms [] = ms
oR [] ms = ms

aND :: Matches -> Matches -> Matches
aND [] []  = []
aND ms []  = []
aND [] ms  = []
aND ms ms' = ms ++ ms'


--TODO what to use?? $x or the whole match? or both? Check the chat bot command language
qVar :: String -> Matches -> [VMatch]
qVar x []          = []
qVar x ((_,[]):ms) = qVar x ms
qVar x ((s,(q,m):qs):ms) 
  | x == q         = m : qVar x ((s,qs):ms)
  | otherwise      = qVar x ((s,qs):ms)

vString :: VMatch -> Input
vString ((pos,_),vs) = (pos,vs)

getMatch :: FinalMatch -> Input
getMatch (((pos,_),vs),_) = (pos,vs)

-- Sub query
multiVgrep :: Pattern -> [Input] -> Matches
multiVgrep _ [] = []
multiVgrep p (i:is) = (fst (scan p i)) ++ multiVgrep p is

--Predicates

--filters

filter :: (a-> Bool) -> [a] -> [a]
filter = undefined


