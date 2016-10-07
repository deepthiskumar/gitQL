module Matcher (Matcher.match, Matcher.Matches, Matcher.Match) where

import VPM hiding (Matches, Match, match, isNone)
import Data.Maybe (isJust)

type Matches = [Match]
type Match = (Pos, String)

match :: Pattern -> String -> Matches
match p | isNone p = error ""
        | otherwise = m 0 p
  where m _ _ [] = []
        m pos pat (x:xs) = let ys = m (pos + 1) pat xs
                           in map (\(_, y) -> (pos, y)) (match' pat (x:xs)) ++ ys

{-|
>>> match' (ch '3') "31"
Just ("1","3")
>>> match' (Seq (ch '3') (ch '1')) "31"
Just ("","31")
>>> match' (ch '3') "13"
Nothing
>>> match' (Repeat (ch '3') 0 Nothing) "33311"
Just ("11","333")
>>> match' (Repeat (ch '3') 3 Nothing) "33311"
Just ("11","333")
>>> match' (Repeat (ch '3') 4 Nothing) "33311"
Nothing
>>> match' (Repeat (ch '3') 1 (Just 1)) "33311"
Just ("3311","3")
>>> match' (Repeat (ch '3') 1 (Just 4)) "33311"
Just ("11","333")
>>> match' (Repeat (ch '3') 1 (Just 2)) "33311"
Just ("311","33")
-}
match' :: Pattern -> String -> [(String, String)]
match' (Plain x) (y:ys) | charMatch x y = [(ys, [y])]
                        | otherwise = []
match' (Plain _) [] = []
match' (Seq a b) yy = case match' a yy
                      of [] -> []
                         xx -> concatMap (\(rest, result) -> match'' b result rest) xx
match' (Alt a b) yy = let ma = match' a yy
                      in if (not . null) ma then ma else match' b yy
match' (Repeat _ _ (Just 0)) yy = [(yy, [])]
match' (Repeat pat min max) yy =
  case match' pat yy of [] -> if min == 0 then [(yy, [])] else []
                        xx -> concatMap (\(rest, result) ->
                                          match'' (Repeat pat (if min == 0 then 0
                                                               else min - 1)
                                                          (fmap (subtract 1) max))
                                          result rest) xx
match' None rest = [(rest, "")]

match'' pat result rest = fmap (fmap (result ++)) (match' pat rest)

isNone None = True
isNone (Seq a b) = isNone a && isNone b
isNone (Alt a b) = isNone a
isNone (Repeat a min max) = isNone a ||
                            (min == 0 && case max of Just x -> x == 0
                                                     Nothing -> False)
