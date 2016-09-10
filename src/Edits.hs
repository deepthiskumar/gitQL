module Edits where

{-

Linear (i.e., non-branching) edit dependencies

Representation of dependencies as a rose tree called "edit tree":
If edit l directly applies to a variant produced by edit k,
then we have the node "ET k [..., l, ...]" in the tree.
Indirect dependencies are represented by paths in the tree.

This file contains a representation for the dependencies of (linear,
non-branching) Edits.

  type Edits = [EditTree]
  data EditTree = ET Dim Edits

The edit dependencies can be computed from a variational text using
the `edits` function.

  edits :: VText -> Edits

Finally, the `revertible` function finds all commits that can be
individually undone without dependencies. This is done by finding all
leaves of the edit tree that are not also internal nodes.

  revertible :: VText -> [Dim]

-}

import VText

type Edits = [EditTree]
data EditTree = ET Dim Edits

instance Show EditTree where
  show (ET d [])  = show d
  show (ET d [e]) = show d++"<"++show e
  show (ET d es)  = show d++"<"++show es


-- Partitioning of nodes into internal nodes and leaves
--
part :: EditTree -> ([Dim],[Dim])
part (ET d []) = ([],[d])
part (ET d es) = (d:is, ls) where (is, ls) = parts es

parts :: Edits -> ([Dim],[Dim])
parts = pair concat . unzip . map part
        where pair f (x, y) = (f x, f y)

nodes :: EditTree -> [Dim]
nodes = uncurry (++) . part

leaves :: EditTree -> [Dim]
leaves = snd . part

-- Smart constructors
--
edits :: VText -> Edits
edits (VText ss) = [editTree s | s <- ss, isChoice s]

editTree :: Segment -> EditTree
editTree (Chc d _ v) = ET d (edits v)

-- revertible commits
-- (= all leaves of the edit tree that are not also internal nodes)
--
revertible :: VText -> [Dim]
revertible v = (nub ls) \\ is
               where (is, ls) = parts (edits v)
