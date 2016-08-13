module Main where

import Parsers
import System.Environment
import System.IO.Strict as F(readFile)
import System.Directory (doesFileExist)
import VText
import CCMerge
import CCLib

infixr 8 +.+ , +.. , ..+
infixl 7 <<< , <<*
infixr 6 |||
 
(+.+) = thn
(..+) = xthn
(+..) = thnx
(|||) = alt
(<<<) = using
(<<*) = using2
lit   :: Eq a => a -> Parser a a
lit   = literal
star  = rpt
anyC  = satisfy (const True)
butC cs = satisfy (not.(`elem` cs))
noC   "" = [("","")]
noC   _  = []

----------------------------------------------------------------------

main = do
  args <- getArgs
  parse_args' args
--main = getArgs exit parse_args

vSearch :: String -> FilePath -> IO ()
vSearch regex file = do
 vexists <- doesFileExist file
 if(not vexists)
  then do
   print $ "Source " ++ file ++ " doesnt exist"
   return ()
  else do
   vsource <- F.readFile file
   let e_vtext = ccParser (stripNewline vsource)
   let v_parsed = case e_vtext of { Left _ -> False; Right _ -> True }
   errorIf (not v_parsed) $ "Failed to parse " ++ file
   let Right vtext = e_vtext
   print ("VText :\n" ++ (show vtext))
   let matched = searchInVText regex vtext
   let unified = map (unifyVText) matched
   print unified

searchInVText :: String -> VText -> [Match]
searchInVText regex vtext = let vtext' = vSplit vtext
                                acc = acceptor' (fst(head(nnRegexp regex)))
                            in map (unifyVText) (acc vtext')--unlines . filter acc . lines
 --figure out how to get the VText by filtering h

parse_args :: [String] -> IO()
parse_args (regexp: file : []) = do
	let acc = acceptor (fst(head(nnRegexp regexp)))
	let acc' = unlines . filter acc . lines 
	text <- F.readFile $ file
	print (acc' text )
parse_args _ = putStr ("Incorrect Usage: regexp\n") --hPutStr stderr ("Usage: regexp\n")

parse_args' :: [String] -> IO()
parse_args' (regexp: file : []) = do
	let acc = acceptor (fst(head(nnRegexp regexp)))
	let acc' = unlines . filter acc 
	text <- F.readFile $ file
	print (acc' [text] )
parse_args' _ = putStr ("Incorrect Usage: regexp\n") --hPutStr stderr ("Usage: regexp\n")


{-
  Atom		= character | "\\" character | "." | "\\(" Regexp "\\) .
  ExtAtom	= Atom ["*" | "+" | "?"] .
  Factor	= ExtAtom + .
  Regexp	= Factor / "\\|" ["$"].
-}

data NFANode 
      	= NFAChar Char NFANode
 	| NFAAny  NFANode
	| NFAEps  [NFANode]
 	| NFAEnd  NFANode
 	| NFAFinal
	| NFATable [(Char, NFANode)] [NFANode] [NFANode] Bool
        deriving (Show,Eq)
{-NFAChar c next	- a state with arc on character c to next state
NFAAny next	- a state with arc on any character
NFAEps nexts	- a state with a set of epsilon transitions
NFAEnd		- a state with an arc if end of string is reached
NFAFinal	- a final state
NFATable charTrans anyTrans endTrans final
		- a state with character arcs according to charTrans,
		  any character arcs according to anyTrans, end arcs
		  according to endTrans, and a boolean flag indicating
		  a final state
-}


nfaChar = NFAChar
nfaAny  = NFAAny
 -- nfaEps  = NFAEps
nfaEps  = mkTable [] [] [] False . epsClosure
nfaEnd  = NFAEnd
nfaFinal= NFAFinal

--just wrappers for the NFANode constructors, 
--modified such that epsilon transitions are compressed into tables

mkTable pairs anys ends final []      = NFATable pairs anys ends final 
mkTable pairs anys ends final (NFAChar c n:ns) = mkTable ((c,n):pairs) anys ends final ns
mkTable pairs anys ends final (NFAAny n:ns) = mkTable pairs (n:anys) ends final ns
mkTable pairs anys ends final (NFATable pairs' anys' ends' final':ns) = mkTable (pairs'++pairs) (anys'++anys) (ends'++ends) (final' || final) ns
mkTable pairs anys ends final (NFAEnd n:ns) = mkTable pairs anys (n:ends) final ns
mkTable pairs anys ends final (NFAFinal:ns) = mkTable pairs anys ends True ns
mkTable _ _ _ _ _ = error "illegal argument to mkTable"
 
type NFAproducer = NFANode -> NFANode

--An NFAproducer takes a final state and produces the initial state of a
--non-deterministic automaton.

nnAtom :: Parser Char NFAproducer
nnAtom =
      lit '\\' ..+ lit '(' ..+ nnRegexp +.. lit '\\' +.. lit ')'
  ||| lit '\\' ..+ butC "|()"	 <<< nfaChar
  ||| lit '.'			 <<< const NFAAny
  ||| butC "\\.$"		 <<< nfaChar
  ||| lit '$' `followedBy` anyC <<< nfaChar

nnExtAtom :: Parser Char NFAproducer
nnExtAtom =
      nnAtom +.+ opt (lit '*' <<< const (\ at final ->
 					 let at_init = at (nfaEps [final, at_init])
 					 in  nfaEps [at_init, final])
 		|||  lit '+' <<< const (\ at final ->
 					 let at_init = at (nfaEps [final, at_init])
 					 in  nfaEps [at_init])
 		|||  lit '?' <<< const (\ at final ->
 					 let at_init = at (nfaEps [final])
 					 in  nfaEps [final, at_init]))
 	<<< helper
      where
        helper (ea, []) = ea
        helper (ea, [f]) = f ea
 
nnFactor :: Parser Char NFAproducer
nnFactor =
      plus nnExtAtom	<<< foldr (.) id

nnRegexp :: Parser Char NFAproducer
nnRegexp =
      nnFactor +.+ star (lit '\\' ..+ lit '|' ..+ nnFactor) +.+ opt (lit '$')
 	<<< helper
      where
        helper (ef, (efs, [])) = foldl combine ef efs
        helper (ef, (efs, _ )) = foldl combine ef efs . nfaEnd
	combine f1 f2 final = nfaEps [f1 final, f2 final]

--Step function for the NFA interpreter.
--Note if epsilon compression is removed above, all {- epsClosure -} must 
--be uncommented!

nfaStep states c = {- epsClosure -} (concat (map step states))
   where
     step (NFAChar c' n') | c == c' = [n']
     step (NFAAny n') = [n']
     step (NFATable pairs anys ends finals) = [ n' | (c',n') <- pairs, c == c' ] ++ anys
     step _ = []

--precondition: there are no epsilon cycles!

epsClosure [] = []
epsClosure (NFAEps ns:ns') = epsClosure (ns++ns')
epsClosure (n:ns) = n:epsClosure ns

acceptor :: NFAproducer -> String -> Bool
acceptor nfa str = nfaRun ( {- epsClosure -} [nfa nfaFinal]) str

--The NFA interpreter

nfaRun :: [NFANode] -> String -> Bool
nfaRun ns (c:cs) = nfaRun (nfaStep ns c) cs
nfaRun ns [] = not (null ( {- epsClosure -} (concat (map step ns))))
   where
     step (NFAEnd n') = [n']
     step (NFAFinal)  = [NFAFinal]
     step (NFATable pairs anys ends True) = [NFAFinal]
     step (NFATable pairs anys ends finals) = ends
     step _           = []

nfaRunNodes :: [NFANode] -> String -> [NFANode]
nfaRunNodes ns (c:cs) = nfaRunNodes (nfaStep ns c) cs
nfaRunNodes ns [] = (concat (map step ns))
   where
     step (NFAEnd n') = [n']
     step (NFAFinal)  = [NFAFinal]
     step (NFATable pairs anys ends True) = [NFAFinal]
     step (NFATable pairs anys ends finals) = ends
     step _           = []
     
--step function for the NFA interpreter to accept VText
--Note if epsilon compression is removed above, all {- epsClosure -} must 
--be uncommented!
type Match = VText

nfaStep' :: [(NFANode,Match)] -> VText -> [(NFANode,Match)]
nfaStep' s      (VText [])     = s
nfaStep' states (VText (v:vs)) = nfaStep' ( nfaStep1' states v) (VText vs)

nfaStep1' :: [(NFANode,Match)] -> Segment -> [(NFANode,Match)]
nfaStep1' states s = concatMap (step1 s) states

{-nfaStepSegment :: [(NFANode,Match)] -> Segment -> [(NFANode,Match)]
nfaStepSegment states s@(Plain [c])                   = {- epsClosure -} concatMap (step c s) states
--nfaStepSegment states s@(Chc d (VText [Plain [c]]) v) = (concatMap (step c s) states) ++ (nfaStep' states v) 
--nfaStepSegment states s@(Chc d v (VText [Plain [c]])) = concatMap (step c s) states ++ (nfaStep' states v)
nfaStepSegment states (Chc d v1 v2)                   = appendVTexts m (VText [ Chc d (VText (nfaStep' states v1)) (VText (nfaStep' states v2))])
 -}  

step1 :: Segment ->  (NFANode,Match) -> [(NFANode,Match)]
step1 (Plain "") _           = []
step1 (Plain [c]) (node,m)   = let ms = step c node in updateMatch ms m
step1 (Chc d v1 v2) s@(node,m) = let ms = nfaStep' [(node,VText[])] v1 
                                     ms'= nfaStep' [(node,VText[])] v2
                                 in updateMatchChc d ms ms' m 

step :: Char -> NFANode -> [(NFANode,Match)]
step c (NFAChar c' n') | c == c'          = [(n',VText [Plain [c]])]
step c (NFAAny n')                        = [(n',VText [Plain [c]])]
step c (NFATable pairs anys ends finals)  = [(n',VText [Plain [c]]) | (c',n') <- pairs, c == c' ] ++ (map (\n -> (n,VText [Plain [c]])) anys)
step _ _                                  = []

updateMatch :: [(NFANode,Match)] -> Match -> [(NFANode,Match)]
updateMatch [] m           = []
updateMatch ((n1,m1):ms) m = (n1,appendVTexts m m1) : updateMatch ms m

updateMatchChc :: Dim -> [(NFANode,Match)] -> [(NFANode,Match)] -> Match -> [(NFANode,Match)]
updateMatchChc _ [] [] _   = []
updateMatchChc d ms [] m   = map (\(n',m') -> (n', appendVTexts m (VText [Chc d m' (VText [])]))) ms
updateMatchChc d [] ms m   = map (\(n',m') -> (n', appendVTexts m (VText [Chc d (VText []) m']))) ms
updateMatchChc d ((nl,ml):msL) ((nr,mr):msR) m  -- (nl == nr)  = (nl,appendVTexts m (VText [Chc d ml mr])) : updateMatchChc d msL msR m
  | (length $ finalStateExt (nl,ml)) > 0 
      && (length $ finalStateExt (nr,mr)) == 0 =  (nl,appendVTexts m (VText [Chc d ml mr])) 
                                                    : updateMatchChc d msL msR m--Identify such a case : searchInVText ".*c" (toVtext "@3<c@,l@>").
  | (length $ finalStateExt (nr,mr)) > 0 
      && (length $ finalStateExt (nl,ml)) == 0 = (nr,appendVTexts m (VText [Chc d ml mr])) 
                                                    : updateMatchChc d msL msR m
  | (length $ finalStateExt (nl,ml)) > 0 
      && (length $ finalStateExt (nr,mr)) > 0  = (nl,appendVTexts m (VText [Chc d ml mr])) 
                                                    : updateMatchChc d msL msR m--since both are successfull, doesnt matter which final node we pick
  | otherwise = [(nl,appendVTexts m (VText [Chc d ml mr]))] ++ updateMatchChc d msL msR m -- here it doesnt matter which node you take since both are created out of same and if both havent been temrminated, they ought to be the same. Cannot use == here because * or + will have to be fully evaluated (nl == nr)

acceptor' :: NFAproducer -> VText -> [Match]
acceptor' nfa vtext = nfaRunSkipBegin (nfa nfaFinal) vtext --nfaRun' ( {- epsClosure -} [(nfa nfaFinal,"")]) vtext

--The NFA interpreter

nfaRun' :: NFANode -> [(NFANode,Match)] -> VText -> [Match]
nfaRun' orig [] vt                = nfaRerun orig [] [] vt --failed match
nfaRun' orig ns (VText (v:vs)) 
  | (length $ concat (map finalState ns) ) == 0 = nfaRun' orig (nfaStep' ns (VText [v])) (VText vs)
  | otherwise                     = nfaRerun orig [] (getFinalMatches ns) (VText (v:vs))  --todo : need to propagate the matches 
nfaRun' _ ns (VText [])           = map (snd) (concat (map finalStateExt ns))

getFinalMatches :: [(NFANode,Match)] -> [Match]
getFinalMatches ns = map (snd) (concat (map finalState ns) )
 
finalState :: (NFANode,Match) -> [(NFANode,Match)]
finalState (NFAEnd n',m)  = [(n',m)]
finalState (NFAFinal,m)   = [(NFAFinal,m)]
finalState _              = [] --NFATable can be checked only if all the input is consumed.

--When all the input is consumed. if there are extended regex at the end
finalStateExt :: (NFANode,Match) -> [(NFANode,Match)]
finalStateExt (NFAEnd n',m)                       = [(n',m)]
finalStateExt (NFAFinal,m)                        = [(NFAFinal,m)]
finalStateExt (NFATable pairs anys ends True,m)   = [(NFAFinal,m)]
finalStateExt (NFATable pairs anys ends finals,m) = map (\end -> (end,m)) ends
finalStateExt _                                   = []

  
--skip the beginning characters until a match is found   
nfaRunSkipBegin :: NFANode -> VText -> [Match]
nfaRunSkipBegin n (VText [])                      = [] --no matches in this iteration
nfaRunSkipBegin n vt@(VText (v:vs)) 
  | (length $ nfaStep' [(n,VText[])] (VText [v])) == 0 = nfaRunSkipBegin n (VText vs)
  | otherwise                                     = nfaRun' n [(n,VText[])] vt

--Check if the end transition has reached. 
nfaRerun :: NFANode -> [(NFANode,Match)] -> [Match] -> VText -> [Match]
nfaRerun orig [] ms (VText [])      = ms
nfaRerun orig [] ms v               = ms ++ nfaRunSkipBegin orig v
nfaRerun orig ns ms v               = ms ++ nfaRun' orig ns v 

-- for testing
nfa regex = (fst(head(nnRegexp regex))) nfaFinal -- [ ((fst(head(nnRegexp regex))) nfaFinal, "")]

toVtext st = let (Right vtext) = ccParser st in vtext 

--show VText as is
showV :: VText -> String
showV (VText [])     = ""
showV (VText (v:vs)) = "VText " ++ concatMap (showS) vs 

showS :: Segment -> String
showS (Plain x)      = "(Plain " ++ x ++ ")"
showS (Chc d v1 v2)  = "(Chc " ++ (show d) ++ "(" ++ showV v1 ++ ") (" ++ showV v2++ ")"

-- | Doctests - searchInVText returns a pair of boolean and the matched string
-- >>> searchInVText "a" (toVtext "a")
-- [a]
--
-- | Fixed. Now matches substring
-- >>> searchInVText "a" (toVtext "ab")
-- [a]
--
-- >>> searchInVText "a" (toVtext "@1<a@,b@>")
-- [1<a,>]
--
-- >>> searchInVText "a" (toVtext "@1<a@,a@>")
-- [1<a,a>]
--
-- >>> searchInVText "xy" (toVtext "@1<ab@,xy@>")
-- [1<,xy>]
--
-- >>> searchInVText "ay" (toVtext "@1<ab@,xy@>")
-- [1<a,y>] 
--
-- | Following doesnot match because 'a' and 'x' are characters in the same place 
--   and therefore cannot occur beside each other in 1<ab,xy>
-- >>> searchInVText "ax" (toVtext "@1<ab@,xy@>")
-- []
--
-- >>> searchInVText "az" (toVtext "@1<ab@,@2<x@,z@>y@>")
-- []
--
-- >>> searchInVText "zb" (toVtext "@1<ab@,@2<x@,z@>y@>")
-- [1<b,2<,z>>]
--
-- >>> searchInVText "a.c" (toVtext "@1<ab@,@2<x@,z@>y@>c")
-- [1<ab,y>c]
--
-- >>> searchInVText ".*c" (toVtext "@1<ab@,@2<x@,z@>y@>@3<c@,l@>")
-- [1<ab,2<x,z>y>3<c,>]
--
-- >>> searchInVText ".*c.*" (toVtext "@1<ab@,@2<x@,z@>c@>@3<c@,l@>")
-- ["abc","xbc","zbc","acc","acc","xcc","xcc","zcc","zcc","acl","xcl","zcl"]
--
-- | NFANode : (NFAChar 'a' (NFATable [] [NFATable [] [] [] True] [] True)
-- >>> searchInVText "a.?" (toVtext "@1<ab@,@2<x@,z@>c@>")
-- ["ab","ac"]
--
-- >>> searchInVText ".?m" (toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- ["cm","lm"]
--
-- >>> searchInVText ".?b" (toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- ["ab","xb","zb"]
--
-- >>> searchInVText "a.*l" (toVtext "@1<ab@,@2<x@,z@>y@>\n@1<cd@,lm@>\n@3<e@,n@>")
-- ["ab\nl","ay\nl"]
--
-- | the langauge for Alt and subexpression needs "\\" in order to not match to the literal.
--   This is opposite to all the regex behaviours
-- >>> searchInVText "\\(a\\|z\\).*l" (toVtext "@1<ab@,@2<x@,z@>y@>\n@1<cd@,lm@>\n@3<e@,n@>")
-- ["ab\nl","zb\nl","ay\nl","zy\nl"]

