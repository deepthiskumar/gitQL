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

-- | Implementation of parseRegex.
--
-- First argument is the regex string.
--
-- The second is the previous results.  This is usually Sequenced with
-- the immediate previous result if this invocation doesn't manipulate
-- the structure of the Pattern.
--
-- The third is the immediate previous result.  This needs to be
-- Sequenced with the second argument to form the second argument for
-- the next call.  This allows for repitition operators to not have to
-- guess.
--
-- The extra Pattern arguments allows for Alternatives and repition to
-- work correctly.
--
-- The boolean controls what should happen when encountering ')', in
-- other words, whether we are walking through a group.
--
-- We return a String and a Pattern so that groups can continue
-- afterwards correctly.
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
escaped x | elem x ".|()" = x
escaped x = error ("Undefined character to escape: " ++ [x])

-- | Sequence two patterns, eliminating `None`s where possible.
sequence :: Pattern -> Pattern -> Pattern
sequence a None = a
sequence None b = b
sequence a b    = Seq a b
