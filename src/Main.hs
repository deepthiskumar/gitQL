import System.TimeIt
import CCLibPat
import VPMNewTest
import VPMEngine
import Types
import Prelude as P hiding (readFile,seq)
import System.IO.Strict (readFile)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Control.Exception as Exc
import Data.List (nub)
import GitParser
import GitQuery
import Pretty
import Data.Text as T hiding (map, concatMap, head, length)
import MetaFileReader
import Data.Map as M hiding(map)
import Data.Maybe
import Data.DateTime
import Debug.Trace
import Data.List as L

{- TODO
1. implement _
2.map dimension to commit ID (Should the dimension be changed to 7char hash instead of int?) 
-}


type Env = [(String, Result)]

data Result = VRes Matches
            | DRes [Dim]
            | ERes String
            deriving(Show)
            
type MetaFileMap = Map FilePath MetaFile

resultFile :: FilePath
resultFile = "/home/eecs/Documents/Papers/gitql/GQLPaper/src/res.txt"

-- TODO Refine Pretty Match
prettyMatch :: [Var] -> Env -> String
prettyMatch _ []           = ""
prettyMatch (v:vs) env = case lookupVar v env of
   Just (VRes inp)  -> show v ++" : "++ show inp
   Just (DRes dims) -> show v ++" : "++ (show dims)
   Just (ERes e)    -> "Error: "++e 
   Nothing          -> "Error: "++ show v ++ "not found"
   
prettyVString :: MetaFileMap -> VString -> String
prettyVString m = concatMap (prettySegment m)

prettySegment :: MetaFileMap -> Segment -> String
prettySegment _ (Str t)   = T.unpack t
prettySegment m (Chc d v v') = showChcNoColor (show (d+1)){-(commitHash (M.elems m) d)-} (map (prettyVString m) [v,v'])


commitHash :: [MetaFile] -> Dim -> String
commitHash [] d       = show d
commitHash ((m):ms) d = case M.lookup d m of
   Just m  -> T.unpack $ (T.take 7 (commitId m))
   Nothing -> commitHash ms d
{-showInp :: [Input] -> String
showInp [] = ""
showInp ((pos,v):res) = (prettyVString v ++ " \n") ++ showInp res-}

run :: Pattern -> FilePath -> IO Matches
run pattern vFile = do 
     v <- parseText (vFile++".v")
     return $ vgrep pattern v
     
runShow :: Pattern -> FilePath -> IO ()
runShow pattern vFile = do 
     v <- parseText vFile
     {-timeIt $ -}
     print (showMatch $ vgrep pattern v)
     
showMatch :: Matches -> String
showMatch [] = ""
showMatch (m:ms) = show m ++ "\n \n" ++ showMatch ms

showMatches :: MetaFileMap -> [Var] -> Env -> String
showMatches _ [] env = ""
showMatches m (v:vs) env = (getVarMatches m v env ) ++ "\n" ++ showMatches m vs env

getVarMatches :: MetaFileMap -> Var -> Env -> String
getVarMatches m (VStr s) = getVStringOrDim m s
getVarMatches _ (VPos s) = showPos s
getVarMatches _ (VCount s) = countS s

getVStringOrDim :: MetaFileMap -> String -> Env -> String
getVStringOrDim m' s env = case P.lookup s env of
   Just (VRes ms) -> s ++ ":\n" ++ showInps m' ms
   Just (ERes e)  -> s ++ ":\n[]"
   Nothing        -> case lookupDims s env of
     [] -> s ++ ": []\n"
     ds  -> s ++ ":" ++ show (map (+1) ds)--concatMap (commitHash (M.elems m')) ds
     

showPos :: String -> Env -> String
showPos s env =  case P.lookup s env of
   Just (VRes ms) -> "pos " ++ s ++ ":" ++ show (positions ms) ++ "\n"
   Just (ERes e)  -> e ++ "\n"
   Nothing        -> s ++ " not found\n"
   
countS :: String -> Env -> String
countS s env = case P.lookup s env of
   Just (VRes ms) -> "count " ++ s ++ ": " ++ show (length ms) ++ "\n"
   Just (ERes e)  -> e ++ "\n"
   Nothing        -> s ++ " not found\n"
   
positions :: Matches -> [Pos]
positions ms = map (fst.metaInfo) ms
   
lookupDims :: String -> Env -> [Dim]
lookupDims s [] = []
lookupDims s ((_,VRes ms):env) = getDims s ms ++ lookupDims s env
 

   
showInps :: MetaFileMap -> Matches -> String
showInps _ [] = ""
showInps m' (m:ms) = prettyVString m' (vstring m) ++ "\n" ++ showInps m' ms  

parseText :: FilePath -> IO VString
parseText vFile= do
  exists <- doesFileExist vFile
  if(not exists)
    then do
     print $ "Source " ++ vFile ++ " doesnt exist"
     return []
    else do
     vsource <- readFile vFile
     let e_vstring = ccParser (stripNewline vsource)
     let v_parsed = case e_vstring of { Left _ -> False; Right _ -> True }
     errorIf (not v_parsed) $ "Failed to parse " ++ vFile
     let Right vstring = e_vstring
     --Exc.catch ( writeFile (vFile++"_V") (show vstring)) writeHandler
     return vstring

--res = runQuery (Query ["x"] [MatchGen "m" (PChcStar (DVar "d") (Seq (Plain (C 'e')) (Seq (Plain (C 'l')) (Seq (Plain (C 'e')) (Seq (Plain (C 'm')) (Seq (Plain (C 'e')) (Seq (Plain (C 'n')) (Seq (Plain (C 't')) (Seq (Plain (C 'O')) (Plain (C 'f')))))))))) (QVar "x")) (FName "/home/eecs/Documents/gitRepos/testEncoding/repo1/test1.java") Nothing])

main = do
  args <- getArgs
  let ast = parseString (head args)
  let (Query vs ms) = ast
  let fs = nub $ sourceFiles ast
  mf <- mapM (\f-> do {m <- readMetafile f; return (f,m)} ) fs
  let mfileMap = M.fromList mf
  env <- runQuery mfileMap ast
  putStrLn $ showMatches mfileMap vs env

main2 :: String -> IO ()
main2 query = do
  let ast = parseString query
  let (Query vs ms) = ast
  let fs = nub $ sourceFiles ast
  mf <- mapM (\f-> do {m <- readMetafile f; return (f,m)} ) fs
  let mfileMap = M.fromList mf
  env <- runQuery mfileMap ast
  print ast
  print env
  putStr $ showMatches mfileMap vs env
  
  
metaFileMap :: [(FilePath, MetaFile)] -> MetaFileMap
metaFileMap fs = M.fromList fs
  
runQuery :: MetaFileMap -> Query -> IO Env
runQuery m (Query vs ms) = mapVgrep m ms []
  
mapVgrep :: MetaFileMap -> Search -> Env -> IO Env
mapVgrep _ [] env      = return env
mapVgrep ma (m:ms) env = do
  b <- runVgrep ma m env
  mapVgrep ma ms b
  
runVgrep :: MetaFileMap -> MatchGen -> Env -> IO Env
runVgrep ma (MatchGen v p (FName filename) c) env = do
  m' <- run p filename
  let m = filterMatches ma env c m'
  return $ ((v,(VRes m)):env) ++ allQVar m []
runVgrep ma (MatchGen v p (VBinding var) c) env = do
  let vs = lookup' var env -- v::[input]
  let m' = vgrepAgain p vs
  let m  = filterMatches ma env c m'
  return $ ((v,VRes m):env) ++ allQVar m []
runVgrep ma (MatchGen v p (Q q@(Query vs ms ) ) c) env      = do
  env' <- runQuery ma q
  let m' = (vgrepAgain p (getVStringFromEnv $ lookupMany vs env'))
  let m  = filterMatches ma env c m'
  return $ ((v,VRes m):env) ++ allQVar m []

filterMatches :: MetaFileMap -> Env -> Maybe Conditions -> Matches -> Matches
filterMatches _ _ Nothing m    = m
filterMatches ma env (Just cs) m = fc cs
   where fc (Cond c') = cond ma env c' m
         fc (Bin And cs' cs'') = filterMatches ma env (Just cs'') (filterMatches ma env (Just cs') m)
         fc (Bin Or cs' cs'')  = L.union (filterMatches ma env (Just cs') m) (filterMatches ma env (Just cs'') m)
         fc (Not cs')          = m L.\\ (filterMatches ma env (Just cs') m)
   
cond :: MetaFileMap -> Env -> Condition -> Matches -> Matches
cond _ _ _ []                            = []
cond ma _ c@(CommitInfo op i i') ms      = concatMap (filterByCI ma op i i') ms
cond _ env c@(ResultComp op var var') ms = concatMap (filterByRC env op var var') ms

filterByCI :: MetaFileMap -> RelOp -> Info -> Info -> VMatch -> Matches
filterByCI ma op i i' m 
 | execOp op (getInfoValue ma m i) (getInfoValue ma m i')  = [m]
 | otherwise                                               = []

filterByRC :: Env -> RelOp -> String -> String -> VMatch -> Matches
filterByRC env op var var' m
 | execOp' op (getResultVal env m var) (getResultVal env m var') = [m]
 | otherwise                                                     = []

execOp :: (Ord a, Ord b, Show a, Show b) => RelOp -> Maybe (Either a b) -> Maybe (Either a b) -> Bool
execOp _ Nothing Nothing = False
execOp _ Nothing _       = False
execOp _ _       Nothing = False
execOp op x y            = resolve op (fromJust x) (fromJust y)

resolve :: (Ord a, Ord b) => RelOp -> (Either a b) -> (Either a b) -> Bool
resolve op (Left x) (Left y)   = (getOp op) x y
resolve op (Right x) (Right y) = (getOp op) x y
resolve _ _ _                  = error "Invalid condition"

getOp :: Ord a => RelOp -> (a -> a -> Bool)
getOp Gr = (>)
getOp Ls = (<)
getOp Ge = (>=)
getOp Le = (<=)
getOp Equ = (==)
getOp NotEqu = (/=)

condError = "Matches can only be checked for equality"

execOp' :: RelOp -> Matches -> Matches -> Bool
execOp' Equ ms ms'    = or [ (VMatch d v []) == (VMatch d' v' [])| (VMatch d v q ) <- ms, (VMatch d' v' q') <- ms']
execOp' NotEqu ms ms' = or [ (VMatch d v []) /= (VMatch d' v' [])| (VMatch d v q ) <- ms, (VMatch d' v' q') <- ms']
execOp' _ _ _         = error condError

getInfoValue :: MetaFileMap -> VMatch -> Info -> Maybe (Either DateTime Text)
getInfoValue ma m (CDate (DVar d)) =
  case getDims d [m] of
     []     -> Nothing
     dim:ds -> maybe Nothing (\x -> Just $ Left $ commitDate x) (getDimFromMeta (M.elems ma) dim)
getInfoValue ma m (DateVal d) = Just $ Left d
getInfoValue ma m (CAuthor (DVar d)) =
  case getDims d [m] of
     []     -> Nothing
     dim:ds -> maybe Nothing (\x -> Just $ Right $ author x) (getDimFromMeta (M.elems ma) dim)
getInfoValue ma m (AuthorVal s) = Just $ Right (T.pack s)


getDimFromMeta :: [MetaFile] -> Dim -> Maybe MetaData
getDimFromMeta [] _       = Nothing
getDimFromMeta (m:ms) dim = case M.lookup dim m of
   Nothing -> getDimFromMeta ms dim
   x       -> x

getResultVal :: Env -> VMatch -> String -> Matches
getResultVal env m var 
  | not $ P.null $ qVar var [m] = [(P.head $ qVar var [m])]
  | otherwise  =  case P.lookup var env of
   Just (VRes ms) -> ms
   _              -> []













sourceFiles :: Query -> [FilePath]
sourceFiles (Query _ []) = []
sourceFiles (Query v (m:ms)) = sourceFile m ++ sourceFiles (Query v ms)

--MatchGen Var Pattern Source (Maybe Conditions)
sourceFile :: MatchGen -> [FilePath]
sourceFile (MatchGen _ _ (FName f) _)  = [f]
sourceFile (MatchGen _ _ (Q q) _)      = sourceFiles q 
sourceFile _                           = [] 

 
getDimCond :: Maybe Conditions -> [Dim]
getDimCond Nothing   = []
getDimCond (Just c)  = undefined 
  
lookupMany :: [Var] -> Env -> Env
lookupMany [] env = []
lookupMany (v:vs) env = case lookupVar v env of
  Just res -> (varName v,res) : lookupMany vs env
  Nothing  -> (varName v, ERes $ (varName v) ++" not found") : lookupMany vs env

varName :: Var -> String
varName (VStr s) = s
varName (VPos s) = s
varName (VCount s ) = s

lookup' :: String -> Env -> [Input]
lookup' v env = case P.lookup v env of
  Just res -> getInputFromRes res
  Nothing  -> error $ v++" not found"
  
lookupVar :: Var -> Env -> Maybe Result
lookupVar (VStr s) = P.lookup s
lookupVar (VPos s) = P.lookup s
lookupVar (VCount s ) = P.lookup s

  
getInputFromRes :: Result -> [Input]
getInputFromRes (ERes s)     = error s
getInputFromRes (VRes ms)     = map (\m -> (fst$ metaInfo m, vstring m)) ms 
getInputFromRes (DRes dims)  = error $ "Cannot use commit meta-info as source" 
 

allQVar ::  Matches -> Env -> Env
allQVar [] env                = env
allQVar ((VMatch _ _ []):ms) env       = allQVar ms env
allQVar ((VMatch p v ((q,m):qs)):ms) env = let env' = merge (q,m) env
 in allQVar ((VMatch p v qs):ms) env'

 
merge :: (String, (MetaInfo,VString)) -> Env -> Env
merge (q,(m,v')) [] = [(q, VRes [VMatch m v' []])]
merge (q,(m,v')) (e@(v,VRes vs):env) 
  | q == v    = (q,VRes ((VMatch m v' []) : vs) ) :env
  | otherwise = e : merge (q,(m,v')) env
merge (q,(m,v')) (e@(v,vs):env) 
  | q == v    = (q, ERes $ q ++" is already used to store variational string") : env
  | otherwise = e : merge (q,(m,v')) env
  

getVStringFromEnv :: Env -> [Input]
getVStringFromEnv []           = []
getVStringFromEnv ((v,res):es) = getInputFromRes res ++ getVStringFromEnv es 
  
--todo
--check what all is pending
--env should will have all the values, how to identify selected values in nested queries?
  
--lookup
   
    
stripNewline :: [Char] -> [Char]
stripNewline []                 = []
stripNewline ('\n' :[])         = []
stripNewline (x:xs)             = x : stripNewline xs

errorIf :: Bool -> String -> IO ()
errorIf b m = if b then print m else return ()

writeHandler :: IOError -> IO ()
writeHandler e = do
  putStrLn "Something went wrong during write operation"
  return ()
  
noOfCommits :: FilePath -> IO()
noOfCommits f = do
  vstring <- parseText f
  print (length $ nub $ dimensions $ vstring)

dimensions' :: VString -> Int
dimensions' [] = 0
dimensions' ((Str _):vs) = dimensions' (vs)
dimensions' ((Chc d l r):vs) = 1 + (dimensions' l) + (dimensions' r) + (dimensions' vs)

---Patterns
getPattern :: Int -> Pattern
getPattern 1 = p1
getPattern 2 = p2
getPattern 3 = p3
getPattern 4 = p4
getPattern 5 = p5
getPattern 6 = p6
getPattern 7 = p7
getPattern 8 = p8
getPattern 9 = p9
getPattern 10 = p10
getPattern 11 = p11
getPattern 12 = p12
getPattern 13 = p13
getPattern 14 = p14
getPattern 15 = p15
getPattern 16 = p16
getPattern 17 = p17
getPattern 18 = p18
getPattern 19 = p19
getPattern 20 = p20
getPattern 21 = p21
getPattern 22 = p22
getPattern 23 = p23
getPattern 24 = p24
getPattern 25 = p25
getPattern 26 = p26
getPattern 27 = p27
getPattern 28 = p28
getPattern 29 = p29
getPattern 30 = p30
getPattern 31 = p31
getPattern 32 = p32
getPattern 33 = p33
getPattern 34 = p34
getPattern 35 = p35
getPattern 36 = p36
getPattern 37 = p37
getPattern 38 = p38
getPattern 39 = p39
getPattern 40 = p40
getPattern 41 = p41
getPattern 42 = p42
getPattern _ = undefined

--repo1
p1 = (PChcStar (DVar "d") (seq $ map ch "Nucleus ") ((seq $ map ch "Google Play Music Desktop"))) 
p2 = (PChc (DVar "d") (seq $ map ch "\"Nucleus ") ((seq $ map ch "\"Google Play Music Desktop ")))
p3 = (PChc (DVar "d") (seq $ map ch "spec") (seq $ map ch "spec-xunit-file"))
p4 = (seq $ map ch "spec-xunit-file")
p5 = (seq $ map ch "spec")
p6 = (PChc (DVar "d") (seq $ map ch "\"node ") (seq $ map ch "\"electron-rebuild"))

--repo2

p7 = PChc (DVar "d") (seq $ map ch "#ifdef EBUG_LEXER\n") None
p8 = PChcStar (DVar "d") (seq $ map ch "#ifdef EBUG_LEXER") None
p9 = (seq $ map ch "accept(OP_SLOT_ASSIGN)")
p10 = PChc (DVar "d") (seq $ map ch "id)?") (seq $ map ch "type_expr)?") 

--repo3
p11 = PChcStar (DVar "d") (seq $ map ch "\"https://github.com/BerkeleyTrue/react-router#freecodecamp\",") (seq $ map ch "\"^1.0.0-rc1\",")
p12 = PChcStar (DVar "d") None (seq $ map ch "gulpfile.js")
p13 = seq $ map ch "stampit"
p14 = PChcStar (DVar "d") (seq $ map ch "\"crypto\":") (QVar "x")

--repo4
p21 = seqM "VGG16" --oldest
p22 = PChcStar (DVar "d") (seqM "Defaults to 1.0") (seqM "Defaults to 0.02")
p23 = PChc (DVar "d") (seqM "Defaults to 1.0") (seqM "Defaults to 0.02")
p24 = PChcStar (DVar "d") (QVar "x") (seqM "Image of season transfer") --latest

--repo5
p25 = PChcStar (DVar "d") (None) (seqM "gulp-rename") --oldest
p26 = PChcStar (DVar "d") (seqM "gulp-eslint") (None) --mid
p27 = PChcStar (DVar "d") (None) (seqM "homepage")

--repo6

p28 = PChc (DVar "d") (seqM "\"webpack\",") (seqM "\"tests\",") -- oldest
p29 = PChcStar (DVar "d") (None) (seqM "polyfill") -- mid
p30 = seqM "Cloud Code"

--repo7
p15 = PChc (DVar "d") (seqM "comment") (seqM "text") --latest
p16 = PChcStar (DVar "d") (None) (seqM "tauRho")--oldest
p17 = PChcStar (DVar "d") (seqM "allAsciiBinders,") (seqM "allBinders,")

--repo8
p31 = seqM "codecov.io" -- oldest
p32 = PChcStar (DVar "d") (seqM "xcode7.2") (seqM "xcode7.3") --mid
p33 = PChc (DVar "d") (seqM "xctool") (seqM "xcodebuild") 

--repo10
p18 = PChc (DVar "d") (seqM "5") (seqM "2-5")
p19 = seqM "collaborate"
p20 = PChcStar (DVar "d") (None) (seqM "github.com")

--repo11
p34 = PChcStar (DVar "d") None (seqM "libgit2") --old
p35 = seqM "coding style" --mid
p36 = PChcStar (DVar "d") (QVar "x") (seqM "wiki")

--test QueryExamples
p37 = seqM "getCurrentDesignation"
p38 = PChcStar (DVar "d") (seqM "getdesignation()") (seqM "getCurrentDesignation()")
p39 = PChcStar (D 2) (None) (PChc (DVar "d") (seqM "curr") (seqM "currDesig") )
p40 = PChcStar (DVar "d") None (seqM "setSalary")
p41 = PChcStar (D 2) (QVar "x") (QVar "y")
p42 = PChcStar (D 2) (QVar "x") (PChc (DVar "d") (QVar "y") (QVar "z")) -- get all the changes made to setSalary

-- for nested queries, combine p40 and p41, P40 and p42


