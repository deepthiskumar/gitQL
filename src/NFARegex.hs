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
   print matched

searchInVText :: String -> VText -> [Bool]
searchInVText regex vtext = do
 let vtext' = vSplit vtext
 let acc = acceptor' (fst(head(nnRegexp regex)))
 let acc' = acc vtext'--unlines . filter acc . lines
 [acc'] --figure out how to get the VText by filtering h

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
        deriving (Show)
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
nfaStep' :: [NFANode] -> VText -> [NFANode]
nfaStep' states (VText vs) = concatMap ( nfaStepSegment states) vs

nfaStepSegment :: [NFANode] -> Segment -> [NFANode]
nfaStepSegment states (Plain [c])                   = {- epsClosure -} concatMap (step c) states
nfaStepSegment states (Chc d (VText [Plain [c]]) v) = (concatMap (step c) states) ++ (nfaStep' states v) 
nfaStepSegment states (Chc d v (VText [Plain [c]])) = concatMap (step c) states ++ (nfaStep' states v)
nfaStepSegment states (Chc d v1 v2)                 = (nfaStep' states v1) ++ (nfaStep' states v2)
 
step :: Char -> NFANode -> [NFANode]
step c (NFAChar c' n') | c == c'          = [n']
step _ (NFAAny n')                        = [n']
step c (NFATable pairs anys ends finals)  = [ n' | (c',n') <- pairs, c == c' ] ++ anys
step _ _                                  = []


acceptor' :: NFAproducer -> VText -> Bool
acceptor' nfa vtext = nfaRun' ( {- epsClosure -} [nfa nfaFinal]) vtext

--The NFA interpreter

nfaRun' :: [NFANode] -> VText -> Bool
nfaRun' ns (VText (v:vs)) = nfaRun' (nfaStep' ns (VText [v])) (VText vs)
nfaRun' ns (VText [])     = not (null ( {- epsClosure -} (concat (map step ns))))
   where
     step (NFAEnd n') = [n']
     step (NFAFinal)  = [NFAFinal]
     step (NFATable pairs anys ends True) = [NFAFinal]
     step (NFATable pairs anys ends finals) = ends
     step _           = []

