--Get the total number of commits in a file (# dimensions)

module Commits where

import CCLib

noOfCommits :: FilePath -> IO()
noOfCommits f = do
  vstring <- parseText f
  print (dimensions vstring)




dimensions :: VString -> Int
dimensions [] = 0
dimensions ((Plain _):vs) = dimensions (vs)
dimensions ((Chc d l r):vs) = 1 + (dimensions l) + (dimensions r) + (dimensions vs)

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
