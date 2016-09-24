module RegexParser (parseRegex) where

import Prelude hiding (sequence)
import VPM

{- |
>>> parseRegex "."
Plain Wild
>>> parseRegex ".a"
Seq (Plain Wild) (Plain (C 'a'))
>>> parseRegex "a.b"
Seq (Seq (Plain (C 'a')) (Plain Wild)) (Plain (C 'b'))

>>> parseRegex "\\.."
Seq (Plain (C '.')) (Plain Wild)
>>> parseRegex "\\|a"
Seq (Plain (C '|')) (Plain (C 'a'))

>>> parseRegex "a|b"
Alt (Plain (C 'a')) (Plain (C 'b'))
>>> parseRegex "a|b|c"
Alt (Plain (C 'a')) (Alt (Plain (C 'b')) (Plain (C 'c')))
>>> parseRegex "|b|c"
Alt None (Alt (Plain (C 'b')) (Plain (C 'c')))
-}

-- | Parse a Regular Expression String into a Pattern.
parseRegex :: String -> Pattern
parseRegex str = pr str None

pr :: String -> Pattern -> Pattern
pr ('\\':x:xs) p = pr xs (sequence p (ch (escaped x)))
pr ('.':xs) p = pr xs (sequence p wild)
pr ('|':xs) p = Alt p (pr xs None)
pr (x:xs) p = pr xs (sequence p (ch x))
pr [] p = p

escaped :: Char -> Char
escaped '.' = '.'
escaped '|' = '|'
escaped _ = undefined

sequence :: Pattern -> Pattern -> Pattern
sequence a None = a
sequence None b = b
sequence a b    = Seq a b
