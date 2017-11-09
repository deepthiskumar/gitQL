import System.TimeIt
import CCLib
import VPMNewTest
import VPMNew
import Prelude hiding (readFile,seq)
import System.IO.Strict (readFile)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Control.Exception as Exc
import Data.List (nub)
import GitParser
import GitQuery
import Pretty

{- TODO
1. implement _
2.map dimension to commit ID (Should the dimension be changed to 7char hash instead of int?) 
-}
type Env = [(Var, Result)]

data Result = VRes [(Input)]
            | DRes [Dim]
            | ERes String
            deriving(Show)

resultFile :: FilePath
resultFile = "/home/eecs/Documents/Papers/gitql/GQLPaper/src/res.txt"

-- TODO Refine Pretty Match
prettyMatch :: Env -> String
prettyMatch []           = ""
prettyMatch ((v,res):xs) = case res of
   VRes inp  -> v++" : "++ showInp inp
   DRes dims -> v++" : "++ (show dims)
   ERes e    -> "Error: "++e 
   
prettyVString :: VString -> String
prettyVString = concatMap prettySegment

prettySegment :: Segment -> String
prettySegment (Str t)     = t
prettySegment (Chc d v v') = showChc d (map prettyVString [v,v'])

showInp :: [Input] -> String
showInp [] = ""
showInp ((pos,v):res) = (prettyVString v ++ " \n") ++ showInp res

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

res = runQuery (Query ["x"] [MatchGen "m" (PChcStar (DVar "d") (Seq (Plain (C 'e')) (Seq (Plain (C 'l')) (Seq (Plain (C 'e')) (Seq (Plain (C 'm')) (Seq (Plain (C 'e')) (Seq (Plain (C 'n')) (Seq (Plain (C 't')) (Seq (Plain (C 'O')) (Plain (C 'f')))))))))) (QVar "x")) (FName "/home/eecs/Documents/gitRepos/testEncoding/repo1/test1.java") Nothing])

main = do
  args <- getArgs
  let ast = parseString (head args)
  env <- runQuery ast
  --print ast
  newEnv <- res
  putStr $ prettyMatch newEnv

main2 :: String -> IO ()
main2 query = do
  let ast = parseString query
  env <- runQuery ast
  print ast
  print env
  
  {-let i = read (head args) ::Int
  let file = (head.tail) args
  runShow (getPattern i) file-}
  
runQuery :: Query -> IO Env
runQuery (Query vs ms) = do
  env <- mapVgrep ms []
  return $ lookupMany vs env -- lookupMany :: vs, env -> Result
  
mapVgrep :: Search -> Env -> IO Env
mapVgrep [] env = return env
mapVgrep (m:ms) env = do
  b <- runVgrep m env
  mapVgrep ms (b++env)
  
runVgrep :: MatchGen -> Env -> IO Env
runVgrep (MatchGen v p (FName filename) c) e = do
  m <- run p filename
  return $ ((v,(VRes $ map getMatch m)):e) ++ allQVar m []  --TODO add all the query variable bindings from m
runVgrep (MatchGen v p (VBinding var) c) env = do
  let vs = lookupVar var env -- v::[input]
  return ((v,VRes $ map getMatch (vgrepAgain p vs)):env) 
runVgrep (MatchGen v p (Q q@(Query vs ms ) ) c) env      = do
  env' <- runQuery q
  return ((v,VRes $ map getMatch (vgrepAgain p (getVStringFromEnv $ lookupMany vs env'))):env)
  
lookupMany :: [Var] -> Env -> Env
lookupMany [] env = []
lookupMany (v:vs) env = case lookup v env of
  Just res -> (v,res) : lookupMany vs env
  Nothing  -> (v, ERes $ v++" not found") : lookupMany vs env


lookupVar :: Var -> Env -> [Input]
lookupVar v env = case lookup v env of
  Just res -> getInputFromRes res
  Nothing  -> error $ v++" not found"
  
getInputFromRes :: Result -> [Input]
getInputFromRes (ERes s)     = error s
getInputFromRes (VRes m)     = m
getInputFromRes (DRes dims)  = error $ "Cannot use commit meta-info as source" 
 

allQVar ::  Matches -> Env -> Env
allQVar [] env                = env
allQVar ((_,[]):ms) env       = allQVar ms env
allQVar ((s,(q,m):qs):ms) env = let env' = merge (q,m) env
 in allQVar ((s,qs):ms) env'

 
merge :: (String, VMatch) -> Env -> Env
merge (q,m) [] = [(q, VRes [vString m])]
merge (q,m) (e@(v,VRes vs):env) 
  | q == v    = (q,VRes (vString m : vs) ) :env
  | otherwise = e : merge (q,m) env
merge (q,m) (e@(v,vs):env) 
  | q == v    = (q, ERes $ q ++" is already used to store variational string") : env
  | otherwise = e : merge (q,m) env

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


