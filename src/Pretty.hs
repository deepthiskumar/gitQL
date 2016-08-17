
module Pretty (showChc,showChcNoColor) where

import Data.List (intersperse)

--Escape sequence for choices
escape = "ⱺ" --""
---------------------
-- Pretty Printing --
---------------------

commas style = concat . intersperse (style ",")
brackets style s = style "<" ++ s ++ style ">"

showChc d es = color (show d) ++ brackets color (commas color es)
               where color = style (colors !! (mod (d-1) 6))

commasNoColor = concat . intersperse (escape ++ ",")

bracketsNoColor s = "<" ++ s ++ escape ++ ">"

showChcNoColor d es = escape ++ show d ++ bracketsNoColor (commasNoColor es)


--
-- Color & style definitions
--

reset = "\27[0m"
bold  = "\27[1m"

attrFG c = "\27[3;" ++ show c ++ "m"

black  = attrFG 0
red    = attrFG 1
green  = attrFG 2
yellow = attrFG 3
blue   = attrFG 4
purple = attrFG 5
cyan   = attrFG 6
white  = attrFG 7

colors = map (++bold) [green,red,blue,yellow,purple,cyan]

defaultColor = black ++ reset

style c s = c ++ s ++ defaultColor
