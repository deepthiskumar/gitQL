{-# LANGUAGE ViewPatterns #-}

module VPM where

import Prelude hiding (seq)


data Atomic = C Char | Wild

data Pattern = Plain Atomic
             | Seq Atomic Pattern
             | Alt Pattern Pattern

type Input = (Pos,String)
type Pos = Int

type Split = Maybe (String,String)

type Match   = (Pos,String)
type Matches = [Match]



when :: a -> Bool -> Maybe a
when x p = if p then Just x else Nothing

andThen :: Split -> Split -> Split
andThen (Just (m,r)) (Just (m',r')) = Just (m++m',r++r')
andThen _ _ = Nothing

orElse :: Split -> Split -> Split
orElse s@(Just _) _ = s
orElse _          s = s

matches :: Pattern -> String -> Split
matches (Plain (C d)) (c:s) = ([c],s) `when` (c==d)
matches (Plain Wild)  (c:s) = Just ([c],s)
matches (Seq a p)     (c:s) = matches (Plain a) [c] `andThen` matches p s
matches (Alt p q)     s     = matches p s `orElse` matches q s
matches _             _     = Nothing


scan :: Pattern -> Input -> Matches
scan p (i,matches p -> Just (m,r)) = (i,m):scan p (i+length m,r)
scan p (i,_:s) = scan p (i+1,s)
scan _ (_,[]) = []


match :: Pattern -> String -> Matches
match p s = scan p (0,s)


-- patterns
--
ch :: Char -> Pattern
ch = Plain . C

seq :: [Pattern] -> Pattern
seq [p]    = p
seq (Plain a:ps) = Seq a (seq ps)

[a,b,c] = map ch "abc"

ab = seq [a,b]
abc = seq [a,b,c]
_a = Wild `Seq` a

a'b = Alt a b


-- Strings
--
s = "xabyabcz"
