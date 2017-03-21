import CCLib
import VPMNewTest
import VPMNew
import Prelude hiding (readFile,seq)
import System.IO.Strict (readFile)
import System.Environment (getArgs)
import System.Directory (doesFileExist)
import Control.Exception as Exc
import Data.List (nub)

resultFile :: FilePath
resultFile = "/home/eecs/Documents/CS583/gitRepo/repo2/res.txt"

run :: Pattern -> FilePath -> IO ()
run pattern vFile = do 
     v <- parseText vFile
     writeFile resultFile (showMatch $ vgrep pattern v)
     
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

main = print "Not defined"
    
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

