module Main where

import Parsers
import System.Environment
import System.IO.Strict as F(readFile)
import System.Directory (doesFileExist)
import Data.List (nub)
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
   --print ("VText :\n" ++ (show vtext))
   let matched = vgrep1 regex vtext
   let unified = map (\(m,e) -> (unifyVText m,e)) matched
   print (showResult unified)

vgrep1 :: String -> VText -> [Result]
vgrep1 regex vtext = let vtext' = vSplit vtext
                         acc = acceptor' (fst(head(nnRegexp regex)))
                     in map (\(m,e) -> (unifyVText m,e)) (acc vtext')--unlines . filter acc . lines
 --figure out how to get the VText by filtering h

showResult :: [Result] -> String
showResult []         = ""
showResult ((m,d):rs) = "Match: " ++ (show m) ++ " Dimension Variable : "++ (show d) ++"\n" ++ (showResult rs)

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
	| NFATable [NFANode] [(Char, NFANode)] [NFANode] [NFANode] Bool
	| NFAChc DimEx (NFANode,NFANode) [Match] NFANode
        deriving (Show)

--DimEx is used to specify a dimension and to define variables to refer them
type Variable = String
data DimEx = Dim Int | VD Variable deriving (Show)

--To specify which alternative to query
data Branch = BL | BR | BB

type Match = VText
--Dimensions for each of the dimension variable specified in the query.
type DimEnv = [(String,[Int])]
type Result = (Match,DimEnv)


{-NFAChar c next	- a state with arc on character c to next state
NFAAny next	- a state with arc on any character
NFAEps nexts	- a state with a set of epsilon transitions
NFAEnd		- a state with an arc if end of string is reached
NFAFinal	- a final state
NFATable chcTrans charTrans anyTrans endTrans final
                - a state with Chc transitions according to chcTrans
		- a state with character arcs according to charTrans,
		  any character arcs according to anyTrans, end arcs
		  according to endTrans, and a boolean flag indicating
		  a final state
NFAChc dimension regexL regexR matches next
               - a state with two arcs on a dimension to the 2 alternatives
                 contains the choice expression that it matches to.
                 ([(String,[Int])],[Match])
-}


nfaChar = NFAChar
nfaAny  = NFAAny
 -- nfaEps  = NFAEps
nfaEps  = mkTable [] [] [] [] False . epsClosure
nfaEnd  = NFAEnd
nfaFinal= NFAFinal

--just wrappers for the NFANode constructors,
--modified such that epsilon transitions are compressed into tables

mkTable choices pairs anys ends final []      = NFATable choices pairs anys ends final
mkTable choices pairs anys ends final (NFAChar c n:ns) = mkTable choices ((c,n):pairs) anys ends final ns
mkTable choices pairs anys ends final (NFAAny n:ns) = mkTable choices pairs (n:anys) ends final ns
mkTable choices pairs anys ends final (NFATable choices' pairs' anys' ends' final':ns) = mkTable (choices'++choices) (pairs'++pairs) (anys'++anys) (ends'++ends) (final' || final) ns
mkTable choices pairs anys ends final (NFAEnd n:ns) = mkTable choices pairs anys (n:ends) final ns
mkTable choices pairs anys ends final (NFAFinal:ns) = mkTable choices pairs anys ends True ns
mkTable choices pairs anys ends final ((NFAChc d (n1,n2) ms next) : ns) = mkTable (NFAChc d (n1,n2) ms next : choices) pairs anys ends final ns
mkTable _ _ _ _ _ _ = error "illegal argument to mkTable"

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
     step (NFATable choices pairs anys ends finals) = [ n' | (c',n') <- pairs, c == c' ] ++ anys
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
     step (NFATable choices pairs anys ends True) = [NFAFinal]
     step (NFATable choices pairs anys ends finals) = ends
     step _           = []

nfaRunNodes :: [NFANode] -> String -> [NFANode]
nfaRunNodes ns (c:cs) = nfaRunNodes (nfaStep ns c) cs
nfaRunNodes ns [] = (concat (map step ns))
   where
     step (NFAEnd n') = [n']
     step (NFAFinal)  = [NFAFinal]
     step (NFATable choices pairs anys ends True) = [NFAFinal]
     step (NFATable choices pairs anys ends finals) = ends
     step _           = []

--step function for the NFA interpreter to accept VText
--Note if epsilon compression is removed above, all {- epsClosure -} must
--be uncommented!

nfaStep' :: Branch -> String -> [(NFANode,Match,DimEnv)] -> VText -> [(NFANode,Match,DimEnv)]
nfaStep' b dimVar s      (VText [])     = s
nfaStep' b dimVar states (VText (v:vs)) = nfaStep' b dimVar ( nfaStep1' b dimVar states v) (VText vs)

nfaStep1' :: Branch -> String ->  [(NFANode,Match,DimEnv)] -> Segment -> [(NFANode,Match,DimEnv)]
nfaStep1' b dimVar states s = concatMap (step1 b dimVar s) states

step1 :: Branch -> String -> Segment -> (NFANode,Match,DimEnv) -> [(NFANode,Match,DimEnv)] --step1 _ (Plain "") n@((NFATable choices pairs anys ends finals),m,e) = map (\a -> (a,m,e)) anys
step1 _ _ (Plain "") n@(node,m,e)        = let ms = checkZeroOrMore (n) in updateMatch ms m --TODO allow in case of ? , * or +
step1 _ _ (Plain [c]) n@(node,m,e)       = step c n
step1 b dimVar c@(Chc d v1 v2) n         = matchVRegex b dimVar c n
step1 _ _ _ _                            = undefined

step :: Char -> (NFANode,Match,DimEnv) -> [(NFANode,Match,DimEnv)]
step c (NFAChar c' n',m,e) | c == c'          = [(n',appendVTexts m (VText [Plain [c]]),e)]
step c (NFAAny n',m,e)                        = [(n',appendVTexts m (VText [Plain [c]]),e)]
step c (NFATable choices pairs anys ends finals,m,e)  = [(n',appendVTexts m (VText [Plain [c]]),e) | (c',n') <- pairs, c == c' ] ++ (map (\n -> (n,appendVTexts m (VText [Plain [c]]),e)) anys) -- TODO how the choices here needs to handled
step c n@(NFAChc d (n1, n2) ms next,m,e)      = concatMap (matchRight n (Plain [c])) (nfaStep1' BB "" [(n1,VText[],e)] (Plain [c]))
step _ _                                      = []

--epsilon transitions
checkZeroOrMore :: (NFANode,Match,DimEnv) -> [(NFANode,Match,DimEnv)]
checkZeroOrMore (n@(NFATable choices pairs anys ends True), m ,e)  = [(n,VText[Plain ""],e)]
checkZeroOrMore (n@(NFAAny next), m ,e)                            = [(n,VText[Plain ""],e)]
checkZeroOrMore (n@(NFAFinal),m,e)                                 = [(n,VText[Plain ""],e)]
checkZeroOrMore (n@(NFAEnd n'),m,e)                                = [(n,VText[Plain ""],e)]
checkZeroOrMore _                                                  = []


matchVRegex :: Branch -> String -> Segment -> (NFANode,Match,DimEnv) -> [(NFANode,Match,DimEnv)]
matchVRegex BB dimVar s n@((NFAChc d (n1,n2) ms next),m,e) -- first appearance of the choice regex
               | matchDim d s       =  concatMap (matchRight n s) (applyVRegex BL s (getDimVD d ) (n1,VText [],e))
               | otherwise          = []
matchVRegex BL dimVar s (NFAChc d (n1,n2) ms next,m,e)
               | matchDim d s       = let ms' = applyVRegex BL s (getDimVD d ) (n1,m,e) in map (\(n',m',e')-> (n',m',{-updateDimEnv d s -}e')) ms'
               | otherwise          = [(n1,m,e)]
matchVRegex BR dimVar s (NFAChc d (n1,n2) ms next,m,e)
               | matchDim d s       = let ms' = applyVRegex BR s (getDimVD d ) (n2,m,e) in map (\(n',m',e')-> (n',m',{-updateDimEnv d s -}e')) ms'
               | otherwise          = [(n2,m,e)]
matchVRegex b dimVar s (NFATable choices p a e' final,m,e) = (nfaStep' b dimVar (map (\ch -> (ch,m,e)) choices) (VText [s])) ++ (applyVRegex b s dimVar (NFATable [] p a e' final,m,e))
matchVRegex b dimVar s n                   = applyVRegex b s dimVar n

getDimVD :: DimEx -> String
getDimVD (Dim d)  = ""
getDimVD (VD d)   = d

--MatchRight -> current choice regex -> next node from left regex.
matchRight :: (NFANode,Match,DimEnv) -> Segment -> (NFANode,Match,DimEnv) -> [(NFANode,Match,DimEnv)]
matchRight n@(NFAChc d (_,n2) ms next,m,eOld) s curr@(NFAEnd n', m',e)  = let ms' = concatMap (nfaStep' BR (getDimVD d ) [(n2,VText[],e)]) (appendMatch ms m') -- Right side might not match for example in the case of <0,NULL>
                                                                          in returnMatch n s curr ms'
matchRight n@(NFAChc d (_,n2) ms next,m,eOld) s curr@(NFAFinal,m',e)  = let ms' = concatMap (nfaStep' BR (getDimVD d ) [(n2,VText[],e)]) (appendMatch ms m')
                                                                        in returnMatch n s curr ms'
matchRight n@(NFAChc d (_,n2) ms next,m,eOld) s n'@((NFATable c p a e' True),m',e) =
  let ms' = concatMap (nfaStep' BR (getDimVD d ) [(n2,VText[],e)]) (appendMatch ms m')
  in case ms' of
    []        -> [(NFAChc d ((NFATable c p a e' True),n2) ms next , m,e) ] --No Match so return
    otherwise -> concatMap (checkFinalAndReturn n s n') ms' -- left biased
matchRight (NFAChc d (_,n2) ms next,m,eOld) s (n',m',e) = [(NFAChc d (n',n2) (appendMatch ms m') next , m,e)] --No Match

--When repetition is encountered in the left regex, then after for every final = true, match right. If it
-- returns [] then continue in the left it self (because right might match after the next input is consumed)

checkFinalAndReturn :: (NFANode,Match,DimEnv) -> Segment -> (NFANode,Match,DimEnv) ->  (NFANode,Match,DimEnv) -> [(NFANode,Match,DimEnv)]
checkFinalAndReturn (NFAChc d (_,n2) ms next,m,eOld) s (curr,m',e) (n'',m'',e'')
  | not $ null $ finalStateExt (n'',m'',e'') = [(next,appendVTexts m m'',{-updateDimEnv d s -}e'')]
  | otherwise                                = [(NFAChc d (curr,n2) (appendMatch ms m') next , m,e) ]

returnMatch ::  (NFANode,Match,DimEnv) -> Segment -> (NFANode,Match,DimEnv) ->  [(NFANode,Match,DimEnv)] -> [(NFANode,Match,DimEnv)]
returnMatch _ _ _ []                         = [] --failed match
returnMatch n@(NFAChc d (_,n2) ms next,m,eOld) s n'@(curr,m',e) ((n'',m'',e''):ms'')
  | not $ null $ finalStateExt (n'',m'',e'') = (next,appendVTexts m m'',{-updateDimEnv d s-} e'') : returnMatch n s n' ms''
  | otherwise                                = (NFAChc d (curr,n2) (appendMatch ms m') next , m,e) : returnMatch n s n' ms''--let regex will be final, but will accept empty inputs in order to match the right regex

appendMatch :: [Match] -> Match -> [Match]
appendMatch [] m' = [m']
appendMatch [m] m'    = [appendVTexts m m']
appendMatch (m:ms) m' = appendVTexts m m' : appendMatch ms m'

applyVRegex :: Branch -> Segment -> String -> (NFANode,Match,DimEnv) -> [(NFANode,Match,DimEnv)] -- returns matched dimensions
applyVRegex BB (Chc d v1 v2) dimVar s@(node,m,e)  = let ms = nfaStep' BB dimVar [(node,VText[],e)] v1
                                                        ms'= nfaStep' BB dimVar [(node,VText[],e)] v2
                                                    in updateMatchChc d ms ms' m --return []. Doesnot come under NFAChc
applyVRegex BL (Chc d v1 v2) dimVar s@(node,m,e)  = let ms = nfaStep' BL dimVar [(node,VText[],e)] v1 --return d
                                                    in case ms of
                                                       []        -> map (\(n',m',e') -> (n', appendVTexts m (VText [Chc d (VText []) m']),e')) (nfaStep' BL dimVar [s] v2) --match d in v2.
                                                       otherwise -> map (\(n',m',e') -> (n', appendVTexts m (VText [Chc d m' v2]),searchAndupdate dimVar d e')) (ms)
applyVRegex BR (Chc d v1 v2) dimVar s@(node,m,e)  = let ms = nfaStep' BR dimVar [(node,VText[],e)] v2 --return d
                                                    in case ms of
                                                      []        -> map (\(n',m',e') -> (n', appendVTexts m (VText [Chc d m' (VText [])]),e')) (nfaStep' BR dimVar  [s] v1) -- matches left alternative of plain expression thereby causing an unintended match. Eg vgrep1 (d<b,.*>) (1<b,z>). use the dim in v1 and no the outer c. Delete the dimension if right doesnt match
                                                      otherwise -> map (\(n',m',e') -> (n', appendVTexts m (VText [Chc d v1 m']),{-searchAndupdate dimVar d-} e')) ms
applyVRegex _ _ _ _                        = []



matchDim :: DimEx -> Segment -> Bool
matchDim (Dim d) (Chc d' v1 v2) = d==d'
matchDim (VD d) s               = True --TODO how to track the dimension variable??

updateDimEnv :: DimEx -> Segment -> DimEnv -> DimEnv
updateDimEnv _ (Plain _) es        = es
updateDimEnv (Dim _) _ es          = es
updateDimEnv (VD s) (Chc d _ _) es = searchAndupdate s d es

searchAndupdate :: String -> Int -> DimEnv -> DimEnv
searchAndupdate "" _ e     = e
searchAndupdate s d []     = [(s,[d])]
searchAndupdate s d (e@(s',vs):es)
 | s == s'     = (s,nub(d:vs)):es
 | otherwise   = e:searchAndupdate s d es

appendDimEnvs :: DimEnv -> DimEnv -> DimEnv
appendDimEnvs [] []   = []
appendDimEnvs [] es   = es
appendDimEnvs es []   = es
appendDimEnvs (e1:es1) es2 = appendDimEnvs es1 (appendDimEnv e1 es2)

appendDimEnv :: (String,[Int]) -> DimEnv -> DimEnv
appendDimEnv (s,vs) []                 = [(s,vs)]
appendDimEnv e@(s,vs) (e'@(s',vs'):es)
  | s ==s'    = (s,nub(vs++vs')):es
  | otherwise = e' : appendDimEnv e es

getDim :: Segment -> Maybe Int
getDim (Plain _)   = Nothing
getDim (Chc d _ _) = Just d

updateMatch :: [(NFANode,Match,DimEnv)] -> Match -> [(NFANode,Match,DimEnv)]
updateMatch [] m           = []
updateMatch ((n1,m1,e):ms) m = (n1,appendVTexts m m1,e) : updateMatch ms m

updateMatchChc :: Dim -> [(NFANode,Match,DimEnv)] -> [(NFANode,Match,DimEnv)] -> Match -> [(NFANode,Match,DimEnv)]
updateMatchChc _ [] [] _   = []
updateMatchChc d ms [] m   = map (\(n',m',e') -> (n', appendVTexts m (VText [Chc d m' (VText [])]),e')) ms
updateMatchChc d [] ms m   = map (\(n',m',e') -> (n', appendVTexts m (VText [Chc d (VText []) m']),e')) ms
updateMatchChc d ((nl,ml,el):msL) ((nr,mr,er):msR) m  -- (nl == nr)  = (nl,appendVTexts m (VText [Chc d ml mr])) : updateMatchChc d msL msR m
  | (length $ finalStateExt (nl,ml,el)) > 0
      && (length $ finalStateExt (nr,mr,er)) == 0 =  [(nl,appendVTexts m (VText [Chc d ml (VText [])]),el),(nr,appendVTexts m (VText [Chc d ml mr]),appendDimEnvs el er)]
                                                    ++ updateMatchChc d msL msR m--Identify such a case : vgrep1 ".*c" (toVtext "@3<c@,l@>").
  | (length $ finalStateExt (nl,ml,el)) == 0
      && (length $ finalStateExt (nr,mr,er)) > 0  = [(nl,appendVTexts m (VText [Chc d ml mr]),appendDimEnvs el er),(nr,appendVTexts m (VText [Chc d (VText []) mr]),er)]
                                                    ++ updateMatchChc d msL msR m
  | (length $ finalStateExt (nl,ml,el)) > 0
      && (length $ finalStateExt (nr,mr,er)) > 0  = (nl,appendVTexts m (VText [Chc d ml mr]),appendDimEnvs el er)
                                                    : updateMatchChc d msL msR m--since both are successfull, doesnt matter which final node we pick
  | otherwise = [(nl,appendVTexts m (VText [Chc d ml mr]),appendDimEnvs el er)] ++ updateMatchChc d msL msR m -- here it doesnt matter which node you take since both are created out of same and if both havent been temrminated, they ought to be the same. Cannot use == here because * or + will have to be fully evaluated (nl == nr)

acceptor' :: NFAproducer -> VText -> [Result]
acceptor' nfa vtext = nfaRunSkipBegin BB (nfa nfaFinal) vtext --nfaRun' ( {- epsClosure -} [(nfa nfaFinal,"")]) vtext
{-case nfa nfaFinal of
 NFAChc d p       -> concatMap (nfaRunSkipBegin BR (NFAChc d p)) (nfaRunSkipBegin BL (NFAChc d p) vtext)
 otherwise        -> nfaRunSkipBegin BB (nfa nfaFinal) vtext --nfaRun' ( {- epsClosure -} [(nfa nfaFinal,"")]) vtext
-}
--The NFA interpreter

--skip the beginning characters until a match is found
nfaRunSkipBegin :: Branch -> NFANode -> VText -> [Result]
nfaRunSkipBegin b n (VText [])                      = [] --no matches in this iteration
nfaRunSkipBegin b n vt@(VText (v:vs))
  | (length $ nfaStep' b "" [(n,VText[],[])] (VText [v])) == 0 = nfaRunSkipBegin b n (VText vs)
  | otherwise                                            = nfaRun' b n [(n,VText[],[])] vt

nfaRun' :: Branch -> NFANode -> [(NFANode,Match,DimEnv)] -> VText -> [Result]
nfaRun' b orig [] vt                = nfaRerun b orig [] [] vt --failed match
nfaRun' b orig ns (VText (v:vs))
  | (length $ concat (map finalState ns) ) == 0 = nfaRun' b orig (nfaStep' b "" ns (VText [v])) (VText vs)
  | otherwise                     = nfaRerun b orig [] (getFinalMatches ns) (VText (v:vs))  --todo : need to propagate the matches
nfaRun' _ _ ns (VText [])           = map (getResult) (concat (map finalStateExt ns))

--Check if the end transition has reached.
nfaRerun :: Branch -> NFANode -> [(NFANode,Match,DimEnv)] -> [Result] -> VText -> [Result]
nfaRerun b orig [] ms (VText [])      = ms
nfaRerun b orig [] ms v               = ms ++ nfaRunSkipBegin b orig v
nfaRerun b orig ns ms v               = ms ++ nfaRun' b orig ns v

getFinalMatches :: [(NFANode,Match,DimEnv)] -> [Result]
getFinalMatches ns = map (getResult) (concat (map finalState ns) )

getResult :: (NFANode,Match,DimEnv) -> Result
getResult =  \(n,m,e) -> (m,e)

finalState :: (NFANode,Match,DimEnv) -> [(NFANode,Match,DimEnv)]
finalState (NFAEnd n',m,e)  = [(n',m,e)]
finalState (NFAFinal,m,e)   = [(NFAFinal,m,e)]
finalState _                = [] --NFATable can be checked only if all the input is consumed.

--When all the input is consumed. if there are extended regex at the end
finalStateExt :: (NFANode,Match,DimEnv) -> [(NFANode,Match,DimEnv)]
finalStateExt (NFAEnd n',m,e)                               = [(n',m,e)]
finalStateExt (NFAFinal,m,e)                                = [(NFAFinal,m,e)]
finalStateExt (NFATable choices pairs anys ends True,m,e)   = [(NFAFinal,m,e)]
finalStateExt (NFATable choices pairs anys ends finals,m,e) = map (\end -> (end,m,e)) ends
finalStateExt _                                             = []

--NFAChc function
--matchNFAChc :: DimExt -> (NFANode, NFANode) -> VText -> [Match]
--matchNFAChc d p v  = nfaRunSkipBegin BR (NFAChc d p) (nfaRunSkipBegin BL (NFAChc d p)

-- for testing
nfa regex = (fst(head(nnRegexp regex))) nfaFinal-- [ ((fst(head(nnRegexp regex))) nfaFinal, "")]

toVtext st = let (Right vtext) = ccParser st in vtext

testVregex :: NFANode -> VText -> [Result]
testVregex nfa vtext = nfaRunSkipBegin BB (nfa) (vSplit vtext)

--show VText as is
showV :: VText -> String
showV (VText [])     = ""
showV (VText (v:vs)) = "VText " ++ concatMap (showS) vs

showS :: Segment -> String
showS (Plain x)      = "(Plain " ++ x ++ ")"
showS (Chc d v1 v2)  = "(Chc " ++ (show d) ++ "(" ++ showV v1 ++ ") (" ++ showV v2++ ")"


testSearch :: NFANode -> FilePath -> IO ()
testSearch nfa file = do
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
   --print ("VText :\n" ++ (show vtext))
   let val = (testVregex nfa vtext)
   let matched = map (\(m,e) -> (unifyVText m,e)) val
   putStrLn (showResult matched)

-- | Doctests - vgrep1 returns a pair of boolean and the matched string
-- >>> vgrep1 "a" (toVtext "a")
-- [(a,[])]
--
-- | Fixed. Now matches substring
-- >>> vgrep1 "a" (toVtext "ab")
-- [(a,[])]
--
-- >>> vgrep1 "a" (toVtext "@1<a@,b@>")
-- [(1<a,>,[])]
--
-- >>> vgrep1 "a" (toVtext "@1<a@,a@>")
-- [(1<a,a>,[])]
--
-- >>> vgrep1 "xy" (toVtext "@1<ab@,xy@>")
-- [(1<,xy>,[])]
--
-- >>> vgrep1 "ay" (toVtext "@1<ab@,xy@>")
-- [(1<a,y>,[])]
--
-- | Following doesnot match because 'a' and 'x' are characters in the same place
--   and therefore cannot occur beside each other in 1<ab,xy>
-- >>> vgrep1 "ax" (toVtext "@1<ab@,xy@>")
-- []
--
-- >>> vgrep1 "az" (toVtext "@1<ab@,@2<x@,z@>y@>")
-- []
--
-- >>> vgrep1 "zb" (toVtext "@1<ab@,@2<x@,z@>y@>")
-- [(1<b,2<,z>>,[])]
--
-- >>> vgrep1 "a.c" (toVtext "@1<ab@,@2<x@,z@>y@>c")
-- [(1<ab,y>c,[])]
--
-- >>> vgrep1 ".*c" (toVtext "@1<ab@,@2<x@,z@>y@>@3<c@,l@>")
-- [(1<ab,2<x,z>y>3<c,>,[])]
--
-- This is a tricky example. earlier ["abc","xbc","zbc","acc","acc","xcc","xcc","zcc","zcc","acl","xcl","zcl"]
-- >>> vgrep1 ".*c.*" (toVtext "@1<ab@,@2<x@,z@>c@>@3<c@,l@>")
-- [(1<ab,2<x,z>c>3<c,>,[]),(1<a,2<x,z>c>3<c,l>,[]),(1<a,2<x,z>c>3<c,>,[])]
--
-- | NFANode : (NFAChar 'a' (NFATable [] [NFATable [] [] [] True] [] True). earlier ["ab","ac"]
-- >>> vgrep1 "a.?" (toVtext "@1<ab@,@2<x@,z@>c@>")
-- [(1<ab,c>,[])]
--
-- Earlier ["cm","lm"]
-- >>> vgrep1 ".?m" (toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- [(1<c,lm>,[])]
--
-- Earlier ["ab","xb","zb"]
-- >>> vgrep1 ".?b" (toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- [(1<ab,2<x,z>>,[])]
--
-- ****earlier ["ab\nl","ay\nl"]. Should there be space instead of empty when there is no match? the result looks weird.
-- >>> vgrep1 "a.*l" (toVtext "@1<ab@,@2<x@,z@>y@>\n@1<cd@,lm@>\n@3<e@,n@>")
-- [(1<ab,y>
-- 1<,l>,[])]
--
-- | the langauge for Alt and subexpression needs "\\" in order to not match to the literal.
--   This is opposite to all the regex behaviours. Earlier ["ab\nl","zb\nl","ay\nl","zy\nl"]
-- >>> vgrep1 "\\(a\\|z\\).*l" (toVtext "@1<ab@,@2<x@,z@>y@>\n@1<cd@,lm@>\n@3<e@,n@>")
-- [(1<ab,2<,z>y>
-- 1<,l>,[])]
--
-- | VRegex
-- >>> testVregex (NFAChc (VD "d1") (nfa "b",nfa "y") ([VText []]) (NFAFinal) ) (vSplit $ toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- [(1<b,y>,[("d1",[1])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "b",nfa "y") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<bx@,yz@>")
-- [(1<b,y>,[("d1",[1])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "b",nfa "y") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<bx@,ez@>")
-- []
--
-- >>> testVregex (NFAChc (Dim 1) (nfa "b",nfa "y") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<bx@,ez@>")
-- []
--
-- >>> testVregex (NFAChc (Dim 1) (nfa "b",nfa "y") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<bx@,yz@>")
-- [(1<b,y>,[])]
--
-- testVregex (NFAChc (VD "d1") (nfa "x",nfa ".*") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
--
--
-- >>> testVregex (NFAChc (Dim 1) (nfa "b",nfa "y") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<bx@,yz@>@2<b@,y@>")
-- [(1<b,y>,[])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "b",nfa "y") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<bx@,yz@>@2<b@,y@>")
-- [(1<b,y>,[("d1",[1])]),(2<b,y>,[("d1",[2])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "b",nfa ".*") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<bx@,yz@>@2<b@,x@>")
-- [(1<b,y>,[("d1",[1])]),(2<b,x>,[("d1",[2])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa ".?",nfa ".*") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<b@,y@>@2<b@,x@>")
-- [(1<b,y>,[("d1",[1])]),(2<b,x>,[("d1",[2])])]
--
-- [1<b,y>2<b,x>] or the current answer (left biased)
-- >>> testVregex (NFAChc (VD "d1") (nfa ".*",nfa ".*") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<b@,y@>@2<b@,x@>")
-- [(1<b,y>,[("d1",[1])]),(2<b,x>,[("d1",[2])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "x",nfa ".*") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- [(1<,2<x,z>>,[("d1",[2])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "a",nfa "z") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- [(1<a,2<x,z>>,[("d1",[1])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "a",nfa "x") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- [(1<a,2<x,>>,[("d1",[1])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "a",nfa "x") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- [(1<a,2<x,>>,[("d1",[1])])]
--
-- Should this return 1<a,2<,z>>???
-- >>> testVregex (NFAChc (VD "d1") (nfa "a",nfa "z") ([VText []]) (NFAFinal)) (vSplit $ toVtext "@1<abcde@,@2<x@,z@>ylmn@>")
-- [(1<a,2<x,z>>,[("d1",[1])])]
--
-- >>> testVregex (NFAChar 'c' (NFAChc (Dim 1) (nfa "b",nfa "y") ([VText []]) (NFAChar 'l' NFAFinal))) (vSplit $ toVtext "abc@1<b@,y@>lmn")
-- [(c1<b,y>l,[])]
--
-- >>> testVregex (NFAChar 'c' (NFAChc (Dim 1) (nfa "b",nfa "y") ([VText []]) (NFAChar 'l' (NFAChc (VD "d1") (nfa "m",nfa "n") ([VText []]) NFAFinal)))) (vSplit $ toVtext "abc@1<b@,y@>l@2<m@,n@>o")
-- [(c1<b,y>l2<m,n>,[("d1",[2])])]
--
-- >>> testVregex (NFATable [NFAChc (Dim 1) (nfa "b",nfa "y") ([VText []]) NFAFinal,NFAChc (VD "d1") (nfa "m",nfa "n") ([VText []]) NFAFinal] [] [] [] False) (vSplit $ toVtext "abc@1<b@,y@>l@2<m@,n@>o")
-- [(1<b,y>,[]),(2<m,n>,[("d1",[2])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "ab",nfa ".*") ([VText []]) (NFAFinal) ) (vSplit $ toVtext "@1<ab@,@>")
-- [(1<a,>1<b,>,[("d1",[1])])]
--
-- >>> testVregex (NFAChc (VD "d1") (nfa "ab",nfa ".*") ([VText []]) (NFAFinal) ) (vSplit $ toVtext "@1<abc@,@>")
-- [(1<a,>1<b,>,[("d1",[1])])]
-- >>> testVregex (NFAChc (VD "d1") (nfa "ab",nfa ".*") ([VText []]) (NFAFinal) ) (vSplit $ toVtext "@1<abc@,l@>")
-- [(1<a,l>1<b,>,[("d1",[1])])]
--
-- testVregex (NFAChar 'x' (NFAChc (VD "d1") (nfa "ayz",nfa "lyz") ([VText []]) (NFAFinal)) ) (vSplit $ toVtext "xⱺ1<aⱺ,lⱺ>yz")
-- [(xⱺ1<aⱺ,lⱺ>yz,[("d1",[1])])]
-- testVregex (NFAChar 'x' (NFAChc (VD "d1") (nfa "ay",nfa "ly") ([VText []]) (NFAChar 'z' NFAFinal)) ) (vSplit $ toVtext "xⱺ1<aⱺ,lⱺ>yz")
-- [(xⱺ1<aⱺ,lⱺ>yz,[("d1",[1])])]
-- testVregex (NFAChar 'x' (NFAChc (VD "d1") (nfa "ay",nfa "ly") ([VText []]) (NFAFinal)) ) (vSplit $ toVtext "xⱺ1<aⱺ,lⱺ>yz")
-- [(xⱺ1<aⱺ,lⱺ>y,[("d1",[1])])]
-- testVregex (NFAChar 'x' (NFAChc (VD "d1") (nfa "amny",nfa "lmpy") ([VText []]) (NFAFinal)) ) (vSplit $ toVtext "xⱺ1<aⱺ,lⱺ>mⱺ1<nⱺ,pⱺ>yz")
-- [(xⱺ1<aⱺ,lⱺ>mⱺ1<nⱺ,pⱺ>y,[("d1",[1])])]
-- testVregex (NFAChc (VD "d1") (nfa "xamny",nfa "xlmpy") ([VText []]) (NFAFinal) ) (vSplit $ toVtext "xⱺ1<aⱺ,lⱺ>mⱺ1<nⱺ,pⱺ>y")
-- [(xⱺ1<aⱺ,lⱺ>mⱺ1<nⱺ,pⱺ>y,[("d1",[1])])]
-- testVregex (NFAChc (VD "d1") (nfa "xamny",nfa "xlmpy") ([VText []]) (NFAFinal) ) (vSplit $ toVtext "xxⱺ1<aⱺ,lⱺ>mⱺ1<nⱺ,pⱺ>yz")
-- [] * should not be empty
-- Choice with NFATable

