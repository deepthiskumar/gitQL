module Search where

import VText
import Text.Regex
import CCLib

--for main function 
import Prelude hiding (readFile)
import System.IO.Strict (readFile)
import System.Environment (getArgs)
import Data.List (sort)

type Variant = Text

vgrep1 :: String -> VText -> [String]
vgrep1 pat vtext = do
  let xs = applyVD (viewDecisions (sort $ dimensions vtext)) vtext
  match (mkRegex pat) xs


match :: Regex -> [(Variant, Selection)] -> [String]
match pat []         = []
match pat ((v,s):vs) = case matchRegexAll pat v of
                    Just (bef,matchStr,after,subexp) -> matchStr : match pat vs
                    Nothing                          -> match pat vs

--get all the view decisions in th eorder of edits. As of now undo or redo 
--scenarios are not included
--get all the dimensions using CCLib.dimensions and order them in ascending
viewDecisions :: [Int] -> [Selection]
viewDecisions ds = (map LSel ds) : getSelections ds ds

getSelections :: [Int] -> [Int] -> [Selection]
getSelections [] _       = []
getSelections (d:ds) ds' = getSelection d ds' : getSelections ds ds'

getSelection :: Int -> [Int] -> Selection
getSelection x ds = (map RSel (fst $ break (> x) ds)) ++ 
        (map LSel (snd $ break (> x) ds))
        

applyVD :: [Selection] -> VText -> [(Variant, Selection)]
applyVD ss v = map (getVariant v) ss

getVariant :: VText -> Selection -> (Variant,Selection)
getVariant v s = (applySelection s v,s)

--return the match and the variant
--matchRegex :: Regex -> [(Variant, Selection)] -> [String]
--matchRegex r ((v,s):vs) = matchRegexAll r v

func :: String -> IO()
func file = do 
   --args <- getArgs
   --let file = head args
   vsource <- readFile file
   let e_vtext = ccParser $! (stripNewline vsource)
   let v_parsed = case e_vtext of { Left _ -> False; Right _ -> True }
   errorIf (not v_parsed) $ "Failed to parse " ++ file
   let Right vtext = e_vtext
   print vtext
   let res = vgrep1 "x" vtext
   print res
   
   
stripNewline :: [Char] -> [Char]
stripNewline []                 = []
stripNewline ('\n' :[])         = []
stripNewline (x:xs)             = x : stripNewline xs


errorIf :: Bool -> String -> IO ()
errorIf b m = if b then print m else return ()
