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
--
-- >>> parseRegex "ab?c"
-- Seq (Seq (Plain (C 'a')) (Repeat (Plain (C 'b')) 0 (Just 1))) (Plain (C 'c'))
-- >>> parseRegex "(cd)*e"
-- Seq (Repeat (Seq (Plain (C 'c')) (Plain (C 'd'))) 0 Nothing) (Plain (C 'e'))
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
pr ('*':xs)    p l b = if isNone l then error "Must repeat something" else
                         pr xs p (star l) b
pr ('+':xs)    p l b = if isNone l then error "Must repeat something" else
                         pr xs p (plus l) b
pr ('?':xs)    p l b = if isNone l then error "Must repeat something" else
                         pr xs p (possibly l) b
pr ('{':xs)    p l b = let (min, max, continue) = parseBracket xs
                       in pr continue p (Repeat l min max) b
pr ('\\':x:xs) p l b = pr xs (sequence p l) (ch (escaped x)) b
pr ('.':xs)    p l b = pr xs (sequence p l) wild b
pr ('|':xs)    p l b = let (str, pat) = pr xs None None b
                       in (str, Alt (sequence p l) pat)
pr ('(':xs)    p l b = let (str, pat) = pr xs None None True
                       in pr str (sequence p l) pat b
pr (')':xs)    p l b = if b then (xs, sequence p l)
                       else error "Ended a group that didn't start."
pr ('}':xs)    _ _ _ = error "Ended a curly bracket that didn't start."
pr (x:xs)      p l b = pr xs (sequence p l) (ch x) b
pr []          p l b = if b then error "Ended the regex while in a group."
                       else ("", sequence p l)

-- | Escape a character, erroring if it doesn't have an escape.
escaped :: Char -> Char
escaped x | elem x ".|(){}*+?\\" = x
escaped x = error ("Undefined character to escape: " ++ [x])

-- | Sequence two patterns, eliminating `None`s where possible.
sequence :: Pattern -> Pattern -> Pattern
sequence a None = a
sequence None b = b
sequence a b    = Seq a b

-- | Parse the internals of a braced repetition operator.
--
-- First argument looks like "1,3}...".  Note that the opening '{' has
-- been removed.
--
-- Return value: (min, max (or infinity), remaining regex to parse)
--
-- Examples:
--
-- >>> parseBracket "1,3}xxx"
-- (1,Just 3,"xxx")
-- >>> parseBracket "2,}xxx"
-- (2,Nothing,"xxx")
-- >>> parseBracket ",2}xxx"
-- (0,Just 2,"xxx")
parseBracket :: String -> (Int, Maybe Int, String)
parseBracket [] = error "End of string reached while in curly bracket."
parseBracket str0 = let (min, str1) = parse 0 str0 False
                        (max, str2) = parse 0 (fix ',' str1) False
                    in (fromJust min 0, max, fix '}' str2)
  where fromJust (Just x) _ = x
        fromJust Nothing y = y

        fix c (x:xs) | c == x = xs
        fix _ [] = error "End of string reached while in curly bracket."
        fix c xs = error ("Expected " ++ c : ", found: " ++ xs)

        parse _ [] _ = error "End of string reached while in curly bracket."
        parse prev (x:xs) b | elem x "1234567890" =
                              parse (prev * 10 +
                                     case x of '1' -> 1; '2' -> 2;
                                               '3' -> 3; '4' -> 4;
                                               '5' -> 5; '6' -> 6;
                                               '7' -> 7; '8' -> 8;
                                               '9' -> 9; '0' -> 0)
                              xs True
        parse prev xs b = (if b then Just prev else Nothing, xs)
