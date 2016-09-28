module RegexParser (parseRegex) where

import Prelude hiding (sequence)
import VPM

-- | Parse a Regular Expression String into a Pattern.
--
-- Examples:
--
-- >>> parseRegex "."
-- Plain Wild
-- >>> parseRegex ".a"
-- Seq (Plain Wild) (Plain (C 'a'))
-- >>> parseRegex "a.b"
-- Seq (Seq (Plain (C 'a')) (Plain Wild)) (Plain (C 'b'))
--
-- >>> parseRegex "\\.."
-- Seq (Plain (C '.')) (Plain Wild)
-- >>> parseRegex "\\|a"
-- Seq (Plain (C '|')) (Plain (C 'a'))
--
-- >>> parseRegex "a|b"
-- Alt (Plain (C 'a')) (Plain (C 'b'))
-- >>> parseRegex "a|b|c"
-- Alt (Plain (C 'a')) (Alt (Plain (C 'b')) (Plain (C 'c')))
-- >>> parseRegex "|b|c"
-- Alt None (Alt (Plain (C 'b')) (Plain (C 'c')))
--
-- >>> parseRegex "a(bc|d)"
-- Seq (Plain (C 'a')) (Alt (Seq (Plain (C 'b')) (Plain (C 'c'))) (Plain (C 'd')))
-- >>> parseRegex "a(bc|d)e"
-- Seq (Seq (Plain (C 'a')) (Alt (Seq (Plain (C 'b')) (Plain (C 'c'))) (Plain (C 'd')))) (Plain (C 'e'))
parseRegex :: String -> Pattern
parseRegex str = snd (pr str None None False)
pr :: String -> Pattern -> Pattern -> Bool -> (String, Pattern)
pr ('\\':x:xs) p l b = pr xs (sequence p l) (ch (escaped x)) b
pr ('.':xs)    p l b = pr xs (sequence p l) wild b
pr ('|':xs)    p l b = let (str, pat) = pr xs None None b
                       in (str, Alt (sequence p l) pat)
pr ('(':xs)    p l b = let (str, pat) = pr xs None None True
                       in pr str (sequence p l) pat b
pr (')':xs)    p l b = if b then (xs, sequence p l)
                       else error "Ended a group that didn't start."
pr (x:xs)      p l b = pr xs (sequence p l) (ch x) b
pr []          p l b = if b then error "Ended the regex while in a group."
                       else ("", sequence p l)

-- | Escape a character, erroring if it doesn't have an escape.
escaped :: Char -> Char
escaped x = if elem x ".|()" then x else undefined

-- | Sequence two patterns, eliminating `None`s where possible.
sequence :: Pattern -> Pattern -> Pattern
sequence a None = a
sequence None b = b
sequence a b    = Seq a b
