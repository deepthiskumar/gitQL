module VText where

{-

VText is a representation for variational text. This does not directly
employ the V type constructor, because it required one more
indirection that would have complicated function unnecessarily.

  newtype VText = VText [Segment]
  data Segment = Plain Text | Chc Dim VText VText

What is missing are functions to produce VText from a github repository. At
the core should be a function:

  addEdit :: VText -> Text -> Text

-}

import Pretty (showChc, showChcNoColor)

import Data.List ((\\), nub)


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

-- Variant predicates
--
-- This is synonymous with `not (isPlain x)`
isChoice :: Segment -> Bool
isChoice (Chc _ _ _) = True
isChoice _           = False

-- This is synonymous with `not (isChoice x)`
isPlain :: Segment -> Bool
isPlain (Plain _) = True
isPlain _ = False

-- Get all dimensions
allDimensions :: VText -> [Dim]
allDimensions (VText [])     = []
allDimensions (VText (x:xs)) = case x of
  Plain _     -> allDimensions (VText xs)
  Chc d v1 v2 -> d : (allDimensions v1) ++ (allDimensions v2) ++ (allDimensions (VText xs))
