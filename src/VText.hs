
module VText where

{-

This module VText presents a representation for variational text. This does
not directly employ the V type constructor, because it required one
more indirection that would have complicated function unnecessarily.

  newtype VText = VText [Segment]
  data Segment = Plain Text | Chc Dim VText VText

This file contains an example that illustrates the use of the representation.
Ultimately, this should be moved into a testing module. This file also contains
a representation for the dependencies of (linear, non-branching) edits.

  type Edits = [EditTree]
  data EditTree = ET Dim Edits

The edit dependencies can be computed from a variational text using the
following function.

  edits :: VText -> Edits

Finally, the revertible commits can be obtained by finding all leaves of the
edit tree that are not also internal nodes.

  revertible :: VText -> [Dim]

What is missing are functions to produce VText from a github repository. At
the core should be a function:

  addEdit :: VText -> Text -> Text

-}

import Pretty (showChc,showChcNoColor)

import Data.List ((\\),nub)


type Dim = Int


-- Plain documents
--
type Text = String


-- Variational text
--
newtype VText = VText [Segment]

data Segment = Plain Text
             | Chc Dim VText VText

instance Show VText where
  show (VText ss) = concatMap show ss

instance Show Segment where
  show (Plain t)     = t
  show (Chc d v v') = showChcNoColor d (map show [v,v'])

{-instance Show VText where
  show = showV

instance Show Segment where
  show = showS


--show VText as is
showV :: VText -> String
showV (VText [])     = ""
showV (VText (v:vs)) = "VText [" ++ showS v ++ "," ++ showV (VText vs) ++ "]"

showS :: Segment -> String
showS (Plain x)      = "(Plain " ++ x ++ ")"
showS (Chc d v1 v2)  = "(Chc " ++ (show d) ++ "(" ++ showV v1 ++ ") (" ++ showV v2++ ")"
-}
instance Eq VText where
  (==) (VText []) (VText [])         = True
  (==) (VText (x:xs)) (VText (y:ys)) = x==y && xs==ys
  (==) _ _                           = False

instance Eq Segment where
  (==) (Plain t) (Plain t')       = t == t'
  (==) (Chc d l r) (Chc d' l' r') = d == d' &&  l == l' && r == r'
  (==) _ _                        = False

-- Smart constructors
--
plain :: Text -> VText
plain t = VText [Plain t]

change :: Dim -> Text -> Text -> Segment
change d old new = Chc d (plain old) (plain new)


-- Example
--
-- root is the original document
-- edit A changes "two" to "1 + 1"
-- edit B changes the first "1" in "1 + 1" to "One" and add "four" at the end
-- edit C changes "One" to "ONE" and "1" to "I"
--
root :: Text
root = "one two three"

a :: VText
a = VText [Plain "one ",
           Chc 1 (plain "two") (plain "1 + 1"),
           Plain " three"]

b :: VText
b = VText [Plain "one ",
           Chc 1 (plain "two")
                 (VText [Chc 2 (plain "1") (plain "One"),
                         Plain " + 1"]),
           Plain " three",
           Chc 2 (plain "") (plain " four")]

c :: VText
c = VText [Plain "one ",
           Chc 1 (plain "two")
                 (VText [Chc 2 (plain "1") (VText [Chc 3 (plain "One") (plain "ONE")]),
                         Plain " + ",
                         Chc 3 (plain "1") (plain "I")]),
           Plain " three",
           Chc 2 (plain "") (plain " four")]


-- Linear (i.e., non-branching) edit dependencies
--
-- Representation of dependencies as a rose tree called "edit tree":
-- If edit l directly applies to a variant produced by edit k,
-- then we have the node "ET k [...,l, ...]" in the tree.
-- Indirect dependencies are represented by paths in the tree.
--
type Edits = [EditTree]
data EditTree = ET Dim Edits

instance Show EditTree where
  show (ET d [])  = show d
  show (ET d [e]) = show d++"<"++show e
  show (ET d es)  = show d++"<"++show es


-- partitioning of nodes into internal nodes and leaves
--
part :: EditTree -> ([Dim],[Dim])
part (ET d []) = ([],[d])
part (ET d es) = (d:is,ls) where (is,ls) = parts es

parts :: Edits -> ([Dim],[Dim])
parts = pair concat . unzip . map part
        where pair f (x,y) = (f x,f y)

nodes :: EditTree -> [Dim]
nodes = uncurry (++) . part

leaves :: EditTree -> [Dim]
leaves = snd . part


-- Computing dependencies and collecting them in an edit tree
--
isChoice :: Segment -> Bool
isChoice (Chc _ _ _) = True
isChoice _           = False

edits :: VText -> Edits
edits (VText ss) = [editTree s | s <- ss, isChoice s]

editTree :: Segment -> EditTree
editTree (Chc d _ v) = ET d (edits v)

-- get all dimensions
allDimensions :: VText -> [Dim]
allDimensions (VText [])     = []
allDimensions (VText (x:xs)) = case x of
  Plain _     -> allDimensions (VText xs)
  Chc d v1 v2 -> d : (allDimensions v1) ++ (allDimensions v2) ++ (allDimensions (VText xs))

-- revertible commits
-- (= all leaves of the edit tree that are not also internal nodes)
--
revertible :: VText -> [Dim]
revertible v = (nub ls) \\ is
               where (is,ls) = parts (edits v)


-- Continuing the example ...
--
edits_c :: Edits
edits_c = edits c

revertible_c :: [Dim]
revertible_c = revertible c
