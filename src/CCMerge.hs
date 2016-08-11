module CCMerge where

{-
 For merging 2 Choice calculus expressions, both the expressions need to be of similar structure.
 That is to say, changed/unchanged characters in the mergee file should correspond to the same changed/unchanged characters in the base file.
 (Mergee is being merged into the base)
 For example, abcde --> 1<ab,xy>cde (mergee)
                    --> 1<a,m>bcde (base)
  Here the choice expressions in mergee and base represent changes on different Strings, one for "ab" and the other for "a" .
  For these to be compared, Choice in mergee needs to be split into 2 choices => 1<a,x>1<b,y>cde.
  Now update on character "a" can be easily merged.
  But edit on character b cannot be easily compared because base has the whole of bcde unchanged.
  Hence we split Plain "bcde" to Plain "b" and Plain "cde".
  Now changed b in mergee can be compared with unchanged b in base. Both expressions have "cde" unchanged and therefore is simply propogated.

  The dimensions in mergee are renamed by so that when they are merged, the dimensions don't conflict.
  Example 1 (new base is the merged CC expresion)
  base     : 1<ab,ij>
  mergee   : 1<ab,x2<y,l>> => 2<ab,x3<y,l>>
  new base : 1<2<ab,x3<y,l>>,ij>

  Example 2 (common dimension: 1)
  base     : 1<ab,2<x,r>y>
  mergee   : 1<ab,x2<y,l>> => 1<ab,x3<y,l>>
  new base : 1<ab,2<x,r>3<y,l>>

 The Algorithm for merging is as follows:
 Given -  1. Common dimension from which the branch was created

 1. Split all the choices and plain expressions at the character level in both mergee and base
    Example : Plain "ab" => Plain "a",Plain "b"
              Chc 1 (plain "ab") (plain "xy") => Chc 1 (plain "a") (plain "x"), Chc 1 (plain "b") (plain "y")
 2. rename the dimensions in mergee
 3. Merge both the CC Expressions
 4. Unify the expressions where possible (Opposite of split)

-}


import VText
import Data.List
import CCLib
import Pretty

type DimVText = (VText,Dim)
type DimSegment = (Segment,Dim)


--Apply partial decision (i.e., not all the decisions in a view decision are applied).
--The output will be a VText where applied decisions will be a Plain a and the remaining decisions will remain as Choices
--in order to get a choice expression up till a particular commit by selecting left choice for all the dimensions that come after it
partialDecision :: Selection -> VText -> VText
partialDecision [] (VText [])                   = VText []
partialDecision [] v                            = v
partialDecision ss (VText [])                   = VText []
partialDecision (ss) (VText (Plain a :vs))      = (VText [Plain a]) `appendVTexts` partialDecision ss (VText vs)
partialDecision (ss) (VText (c@(Chc d l r) : vs)) = case getSelection d ss of 
 Just L  -> applyDecision ss l `appendVTexts` partialDecision ss (VText vs)
 Just R  -> applyDecision ss r `appendVTexts` partialDecision ss (VText vs)
 Nothing -> VText [Chc d (partialDecision ss l) (partialDecision ss r)]   `appendVTexts` partialDecision ss (VText vs)

applyDecision :: Selection -> VText -> VText
applyDecision ss p@(VText [Plain a]) = p
applyDecision ss v                   = partialDecision ss v

appendVTexts :: VText -> VText -> VText
appendVTexts (VText []) v           = v
appendVTexts v          (VText [])  = v
appendVTexts (VText ss) (VText ss') = VText (ss ++ ss' )

getSelection :: Int -> [Sel] -> Maybe Alt
getSelection d []             = Nothing
getSelection d ((LSel d'):ss) = if d == d' then Just L else d `getSelection` ss
getSelection d ((RSel d'):ss) = if d == d' then Just R else d `getSelection` ss


-- | tests for partial view decision
--
-- >>>partialDecision  [RSel 1,RSel 2] (VText [Chc 1 (plain "a") (plain "x"),Chc 2 (plain "b") (plain "y") ])
-- xy
-- >>>partialDecision  [RSel 1] (VText [Chc 1 (plain "a") (plain "x"),Chc 2 (plain "b") (plain "y") ])
-- x2<b,y>
-- >>>partialDecision  [RSel 1] (VText [Chc 1 (plain "a") (VText[Chc 3 (plain "x") (plain "z")]),Chc 2 (plain "b") (plain "y") ])
-- 3<x,z>2<b,y>
-- >>>partialDecision  [LSel 1] (VText [Chc 1 (plain "a") (VText[Chc 3 (plain "x") (plain "z")]),Chc 2 (plain "b") (plain "y") ])
-- a2<b,y>
-- >>>partialDecision  [LSel 1] (VText [Plain "m",Chc 1 (plain "a") (VText[Chc 3 (plain "x") (plain "z")]),Plain "n",Chc 2 (plain "b") (plain "y") ])
-- man2<b,y>
-- >>>partialDecision  [RSel 1,RSel 2] (VText [Plain "m",Chc 1 (plain "a") (VText[Chc 3 (plain "x") (plain "z")]),Plain "n",Chc 2 (plain "b") (plain "y")]) 
--  m3<x,z>ny
-- >>>partialDecision  [RSel 3,RSel 2] (VText [Plain "m",Chc 1 (plain "a") (VText[Chc 3 (plain "x") (plain "z")]),Plain "n",Chc 2 (plain "b") (plain "y")])
-- m1<a,z>ny

---------------------------------------------------------------------------------------
{-To identify if the actual changes made by the user in the merge commit corresponds 
  to the changes present in either of the branches.
  The function takes 1) merged VText: ccmerge between both the branches before taking the merged commit
                     2) next VText: VText after applying the merged changes to its CC exp (the one without the merge )
                       (The dimensions added in 'next VText' will be inline with the merged dimensions and therefore
                       no overlapping of dimensions. Also next VText will have only one level of nesting atmost since it has only 1 change)
  selections need to be merged seperately
-}
patchCC :: VText -> VText -> (VText,Selection)
patchCC m n = let m' = agressiveSplit m
                  n' = agressiveSplit n
              in compareChanges m' n'

compareChanges :: VText -> VText -> (VText,Selection)
compareChanges (VText (m:ms)) (VText (n:ns)) 
   | m == n    = case compareChanges (VText ms) (VText ns) of (v,ss) -> (VText [m] `appendVTexts` v, ss)
   | otherwise = case (m,n) of
     (Plain a, Plain b)                    -> if a == b then case compareChanges (VText ms) (VText ns) of (v,ss) -> (plain a `appendVTexts` v, ss) else undefined --if plain then both should be same
     (c1@(Chc d1 (VText[Plain ""]) r1),c2@(Chc d2 (VText[Plain ""]) r2)) -> case compareChanges (VText (m:ms)) (VText ns) of (v,ss) -> (VText[Chc d1 (plain "") (VText[Chc d2 r1 r2])] `appendVTexts` v, (RSel d1):ss)
     (_, c@(Chc d (VText[Plain ""]) r))    -> case compareChanges (VText (m:ms)) (VText ns) of (v,ss) -> (VText [c] `appendVTexts` v, ss) --check if the a in plain is equal to the one in l to make sure its the corresponding
     (c@(Chc d (VText[Plain ""]) r), _)    -> case compareChanges (VText ms) (VText (n:ns)) of (v,ss) -> (VText [c] `appendVTexts` v, (LSel d) : ss)
     (Plain a, c@(Chc d l r))              -> case compareChanges (VText ms) (VText ns) of (v,ss) -> (VText [c] `appendVTexts` v, ss) --l should have a, get selection from nextvText
     (c@(Chc d l r), Plain a)              -> case compareChanges (VText ms) (VText ns) of (v,ss) -> (VText [c] `appendVTexts` v, ss) --l should have a, get selection from merge
     (c1@(Chc d1 l1 r1),c2@(Chc d2 l2 r2)) -> case d1 == d2 of
       True   -> case (compareChanges l1 l2, compareChanges r1 r2) of
                  ((v1,ss1),(v2,ss2)) -> case compareChanges (VText ms) (VText ns) of (v,ss) -> (VText [Chc d1 v1 v2] `appendVTexts` v, ss) 
       False  -> case l1 == l2 of --something different added by the user since if L is same but R is diff otherwise same as one of the branches (which is handled by above patterns)
        True -> case (r1,r2) of
         (VText[Plain a],VText[Plain b]) -> case compareChanges (VText ms) (VText ns) of (v,ss) -> (VText[Chc d1 l1 (VText[Chc d2 (plain a) (plain b)])] `appendVTexts` v, (RSel d1) : ss)
         (v1,v2)                         -> let (r',ss') = compareChanges v1 v2 in case compareChanges (VText ms) (VText ns) of (v,ss) -> (VText[Chc d1 l1 (r')] `appendVTexts` v, ss'++ss)
        False -> undefined --something is wrong if both d's and l's are different


--split the plain string into characters
granulatePlain :: [Segment] -> [Segment]
granulatePlain [] = []
granulatePlain ((Plain a) : xs)   = (getPlainChar a) ++ granulatePlain xs
granulatePlain ((Chc d (VText l) (VText r) : xs)) = (Chc d (VText (granulatePlain l)) (VText (granulatePlain r))) : granulatePlain xs

getPlainChar :: [Char] -> [Segment]
getPlainChar []     = [Plain ""]
getPlainChar [x]    = [Plain [x]]
getPlainChar (x:xs) = Plain [x] : getPlainChar xs

-- |
-- >>> vSplit (VText [Plain "abc"])
-- abc
--
-- >>> vSplit (VText [Plain "ab", Chc 1 (VText[Plain "c"]) (VText[Plain "x"])])
-- ab1<c,x>
--
-- >>> vSplit (VText [Plain "a", Chc 1 (VText[Plain "b"]) (VText[Plain "y"])])
-- a1<b,y>
--
-- >>> vSplit (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "xy") (plain "zl"),Plain "ymn"]),Plain "crty"])
-- 1<a,2<x,z>>1<b,2<y,l>>1<,y>1<,m>1<,n>crty
--
-- >>> vSplit (VText [Chc 1 (VText[Chc 2 (plain "xy") (plain "zl"),Plain "ymn"]) (plain "ab"),Plain "crty"])
-- 1<2<x,z>,a>1<2<y,l>,b>1<y,>1<m,>1<n,>crty
--

vSplit::VText -> VText
vSplit (VText x) = agressiveSplit (VText (granulatePlain x))

--Splits the choice expression at character level
agressiveSplit :: VText -> VText
agressiveSplit (VText [])     = VText []
agressiveSplit (VText (x:xs)) = let ys = splitSegment x
                                    VText zs = agressiveSplit (VText xs)
                                in VText (ys ++ zs)

splitSegment :: Segment -> [Segment]
splitSegment (Plain x) = [Plain x]
splitSegment (Chc d (VText l) (VText r)) = case (l,r) of
                            ([],[])                     -> []
                            ([],ys)                     -> splitSegment (Chc d (plain "") (VText ys) )
                            (xs,[])                     -> splitSegment (Chc d (VText xs) (plain ""))
                            (Plain x : xs,Plain y : ys) -> (Chc d (plain x) (plain y)) : (splitSegment (Chc d (VText xs) (VText ys)))
                            (x : xs, y : ys)            -> let (x':xs') =  splitSegment x
                                                               (y':ys') =  splitSegment y
                                                           in (Chc d (VText [x']) (VText [y'])) : splitSegment (Chc d (VText (xs'++xs)) (VText (ys'++ys)))

unifyVText :: VText -> VText
unifyVText (VText [])       = VText []
unifyVText (VText [s])      = case s of
                               Plain _   -> VText [s]
                               Chc d l r -> VText [Chc d (unifyVText l) (unifyVText r)]
unifyVText (VText (s:t:ss)) = case unifySegment s t of
                                  Just u  -> unifyVText (VText (u:ss))
                                  Nothing -> let (VText us) = unifyVText (VText (t:ss))
                                             in (VText (s:us))

--unify the two segment, if same dimension return unified else return both of them
unifySegment :: Segment -> Segment -> Maybe Segment
unifySegment (Chc d (VText l) (VText r)) (Chc d' (VText l') (VText r'))
                   |  d == d'     = Just $ Chc d (unifyVText (VText (l++l'))) (unifyVText (VText (r ++ r')))
                   | otherwise    = Nothing
unifySegment (Plain x) (Plain y)  = Just (Plain (x++y))
unifySegment _ _                  = Nothing

--increment all the dimensions by a value so that they dont collide with the dimensions in the expression on to which it is being merged
-- it takes the latest dimension and also the latest common dimension between the 2 expressions
renameDimensions :: Int -> Int -> VText -> VText
renameDimensions ld cd (VText [])     = (VText [])
renameDimensions ld cd (VText (x:xs)) = let s = renameDimension ld cd x
                                            (VText vs) = renameDimensions ld cd (VText xs)
                                        in (VText (s : vs))

renameDimension :: Int -> Int -> Segment -> Segment
renameDimension _ _ (Plain a)      = (Plain a)
renameDimension ld cd (Chc d l r)
       | d > cd                    = (Chc (d + (ld-cd)) (renameDimensions ld cd l) (renameDimensions ld cd r))
       | otherwise                 = (Chc d (renameDimensions ld cd l) (renameDimensions ld cd r))


--mergee, base and merged base
mergeVText :: VText -> VText -> VText
mergeVText (VText []) (ds)               = (ds)
mergeVText (ss) (VText [])               = (ss)
mergeVText (VText (s:ss)) (VText (d:ds)) = case (s,d) of
                                                (Chc x (VText [Plain ""]) r,Chc y (VText [Plain ""]) r') -> let d' = mergeSegment s d
                                                                                                                (VText ds') = mergeVText (VText ss) (VText ds)
                                                                                                            in (VText (d' : ds'))
                                                (c@(Chc x (VText [Plain ""]) r), _)                      -> let (VText ds') = mergeVText (VText ss) (VText (d:ds))
                                                                                                            in (VText (c : ds'))
                                                (_, c@(Chc y (VText [Plain ""]) r))                      -> let (VText ds') = mergeVText (VText (s:ss)) (VText ds)
                                                                                                            in (VText (c : ds'))
                                                otherwise                                                -> let d' = mergeSegment s d
                                                                                                                (VText ds') = mergeVText (VText ss) (VText ds)
                                                                                                            in (VText (d' : ds'))

mergeSegment :: Segment -> Segment -> Segment
mergeSegment (Plain a) (Plain b)        = (Plain b) --Invariant 1 If a character in the same position is different in both the files, then atleast either one has to be a choice
mergeSegment (Chc d l r) (Plain b)      = (Chc d l r)
mergeSegment (Plain a) (Chc d l r)      = (Chc d l r)
mergeSegment c1@(Chc d l r) c2@(Chc d' l' r')
   | c1 == c2 = c2
   | otherwise = mergeChoice c1 c2

mergeChoice :: Segment -> Segment -> Segment
mergeChoice c1@(Chc d l r) c2@(Chc d' l' r')
  | d == d'   = case l == l' of --left or right alternative has changes
                 True  -> case r == r' of
                           True  -> c2
                           False -> Chc d l' (mergeVText r r')
                 False -> case r == r' of
                           True  -> Chc d (mergeVText l l') r'
                           False -> undefined --Chc d (mergeVText l l') (mergeVText r r') --Invariant 2. This is not allowed (an invariant we impose). A choice can have edits either in left or right alternatives. Not both
  | otherwise = (Chc d' (VText [(Chc d l r)]) r')-- these are new choices introduced and put c1 in the left branch of c2

maxDimensionS :: VText -> Int
maxDimensionS vt = case dimensions vt of
  [] -> 1
  ds -> (maximum ds)

--Common dimension (Needs to be stored while branching out)
mergeCC :: Int -> DimVText -> DimVText -> DimVText
mergeCC cd (vs,d) (vs',d') = let as'= renameDimensions d' cd (vSplit vs)
                                 rs'= unifyVText $ mergeVText (as') (vSplit vs')
                             in  (rs', max (maxDimensionS as') d')


-- | Unifying VText
-- >>> unifyVText (VText [Chc 1 (plain "a") (plain "x"), Chc 1 (plain "b") (plain "y")])
-- 1<ab,xy>
-- >>> unifyVText (VText [Plain "l", Plain "m",Chc 1 (plain "a") (plain "x"), Chc 1 (plain "b") (plain "y")])
-- lm1<ab,xy>
-- >>> unifyVText (VText [Plain "l", Plain "m",Chc 1 (plain "a") (plain "x"), Chc 1 (VText[Chc 2 (plain "b") (plain "i")]) (plain "y")])
-- lm1<a2<b,i>,xy>
-- >>> unifyVText (VText [Plain "l", Plain "m",Chc 1 (plain "a") (plain "x"), Chc 2 (VText[Chc 3 (plain "b") (plain "i")]) (plain "y")])
-- lm1<a,x>2<3<b,i>,y>
-- >>> unifyVText (VText [Plain "l",Chc 1 (plain "j") (plain "k"), Plain "m",Chc 1 (plain "a") (plain "x"), Chc 1 (VText[Chc 2 (plain "b") (plain "i")]) (plain "y")])
-- l1<j,k>m1<a2<b,i>,xy>
-- >>> unifyVText (VText [Chc 1 (VText[Plain "a",Plain "b", Chc 2 (plain "c") (plain "d"), Chc 2 (plain "e") (plain "f")]) (plain "wxyz")])
-- 1<ab2<ce,df>,wxyz>

-- | Renaming dimensions
-- >>> renameDimensions 2 0 (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]),Plain "c"])
-- 3<ab,4<x,z>y>c
-- >>> renameDimensions 2 1 (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]),Plain "c"])
-- 1<ab,3<x,z>y>c
-- >>> renameDimensions 2 2 (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]),Plain "c"])
-- 1<ab,2<x,z>y>c


-- | merge 2 different vTexts.
--
-- Changes only in the left branch
-- >>> mergeCC 0 (VText [Plain "ab", Chc 1 (VText[Plain "c"]) (VText[Plain "x"])],1) (VText [Plain "abc"],0)
-- (ab1<c,x>,1)
-- >>> mergeCC 0 (VText [Plain "a", Chc 1 (VText[Plain "b"]) (VText[Plain "x"]), Plain "c" ],1) (VText [Plain "abc"],0)
-- (a1<b,x>c,1)
-- >>> mergeCC 0 (VText [Chc 1 (VText[Plain "a"]) (VText[Plain "x"]), Plain "bc"],1) (VText [Plain "abc"],0)
-- (1<a,x>bc,1)
--
-- | Changes only in the right branch
--
-- >>> mergeCC 0 (VText [Plain "abc"],0) (VText [Plain "ab", Chc 1 (VText[Plain "c"]) (VText[Plain "x"])],1)
-- (ab1<c,x>,1)
-- >>> mergeCC 0 (VText [Plain "abc"],0) (VText [Plain "a", Chc 1 (VText[Plain "b"]) (VText[Plain "x"]), Plain "c" ],1)
-- (a1<b,x>c,1)
-- >>> mergeCC 0 (VText [Plain "abc"],0) (VText [Chc 1 (VText[Plain "a"]) (VText[Plain "x"]), Plain "bc"],1)
-- (1<a,x>bc,1)
--
-- | Changes in both the branches
--
-- >>> mergeCC 0 (VText [Plain "a", Chc 1 (VText[Plain "b"]) (VText[Plain "y"])],1) (VText [Plain "a", Chc 1 (VText[Plain "b"]) (VText[Plain "x"])],1)
-- (a1<2<b,y>,x>,2)
-- >>> mergeCC 0 (VText [Chc 1 (VText[Plain "a"]) (VText[Plain "x"]), Plain "b" ],1) (VText [Plain "a", Chc 1 (VText[Plain "b"]) (VText[Plain "x"])],1)
-- (2<a,x>1<b,x>,2)
-- >>> mergeCC 0 (VText [Plain "a", Chc 1 (VText[Plain ""]) (VText[Plain "y"]),Plain "b"],1) (VText [Plain "ab"],0)
-- (a1<,y>b,1)
--
-- >>> mergeCC 0 (VText [Plain "a", Chc 1 (VText[Plain ""]) (VText[Plain "y"]),Plain "b"],1) (VText [Plain "a",Chc 1 (VText[Plain "b"]) (VText[Plain "x"])],1)
-- (a2<,y>1<b,x>,2)
-- >>> mergeCC 0 (VText [Plain "a",Chc 1 (VText[Plain "b"]) (VText[Plain "x"])],1) (VText [Plain "a", Chc 1 (VText[Plain ""]) (VText[Plain "y"]),Plain "b"],1)
-- (a1<,y>2<b,x>,2)
-- >>> mergeCC 0 (VText [Plain "a", Chc 1 (VText [Plain ""]) (VText [Plain "x"]),Plain "b"],1) (VText [Plain "a", Chc 1 (VText [Plain ""]) (VText [Plain "y"]),Plain "b"],1)
-- (a1<2<,x>,y>b,2)
--
-- | overlapping changes
--
-- >>> mergeCC 0 (VText [Chc 1 (VText[Plain "ab"]) (VText[Plain "xy"]), Plain "cde"],1 ) (VText [Chc 1 (VText[Plain "a"]) (VText[Plain "m"]), Plain "bcde"],1)
-- (1<2<a,x>,m>2<b,y>cde,2)
-- >>> mergeCC 0 (VText [Chc 1 (VText[Plain "a"]) (VText[Plain "m"]), Plain "bcde"],1 ) (VText [Chc 1 (VText[Plain "ab"]) (VText[Plain "xy"]), Plain "cde"],1)
-- (1<2<a,m>b,xy>cde,2)
--
-- | Chain edits
-- >>> mergeCC 0 (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]),Plain "c"],2) (plain "abc",0)
-- (1<ab,2<x,z>y>c,2)
-- >>> mergeCC 0 (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]),Plain "c"],2) (VText [Plain "a",Chc 1 (plain "bc") (plain "lm")],1)
-- (2<a,3<x,z>>1<2<b,y>c,lm>,3)
--
-- | branch edits
-- >>> mergeCC 0 (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (plain "ab"),Plain "c"],2) (plain "xyc",0)
-- (1<2<x,z>y,ab>c,2)
-- >>> mergeCC 0 (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (plain "ab") ,Plain "c"],2) (VText [Plain "x",Chc 1 (plain "yc") (plain "lm")],1)
-- (2<3<x,z>,a>1<2<y,b>c,lm>,3)
--

-- | branch and chain edits
-- >>> mergeCC 0 (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (VText[Chc 3 (plain "a") (plain "l"),Plain "b"]),Plain "c"],3) (plain "xyc",0)
-- (1<2<x,z>y,3<a,l>b>c,3)
--
-- >>> mergeCC 0 (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (VText[Chc 3 (plain "ab") (plain "l")]),Plain "c"],3) (plain "xyc",0)
-- (1<2<x,z>y,3<ab,l>>c,3)
--
-- >>> mergeCC 0 (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (VText[Chc 3 (plain "ab") (plain "l"),Plain "d"]),Plain "c"],3) (plain "xyc",0)
-- (1<2<x,z>y,3<ab,l>d>c,3)
--
-- >>> mergeCC 0 (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (VText[Chc 3 (plain "a") (VText [Chc 4 (plain "l") (plain "s")]),Plain "b"]),Plain "c"],4) (plain "xyc",0)
-- (1<2<x,z>y,3<a,4<l,s>>b>c,4)
--
-- >>> mergeCC 0 (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (plain "ab") ,Plain "c"],2) (VText [Plain "x",Chc 1 (plain "yc") (VText [Chc 2 (plain "l") (plain "s")]),Plain "m"],2)
-- (3<4<x,z>,a>1<3<y,b>c,2<l,s>>m,4)
--
-- >>> mergeCC 0 (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (VText[Chc 3 (plain "a") (plain "l"),Plain "b"]),Plain "c"],3) (VText [Plain "x",Chc 1 (plain "yc") (plain "l")],1)
-- (2<3<x,z>,4<a,l>>1<2<y,b>c,l>,4)
--
-- | Changes in both the branches. There can only be changes in one branch
-- >>> mergeCC 2 (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (plain "ab") ,Plain "c"],2) (VText [Chc 1 (VText[Chc 2 (plain "x") (VText [Chc 3 (plain "z") (plain "i")]),Plain "y"]) (VText[Chc 3 (plain "a") (plain "j"),Plain "b"]),Chc 1 (plain "c") (plain "m")],3)
-- (*** Exception: Prelude.undefined
--
-- >>> mergeCC 0 (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]),Plain "c"],2) (VText [Chc 1 (plain "abc") (VText [Chc 2 (plain "lm") (VText [Chc 3 (plain "k") (plain "p")]),Plain "o"])],3)
-- (1<4<ab,5<x,z>y>c,2<lm,3<k,p>>o>,5)
--
--
-- | Branch after some modifications - i,e., there are some common dimesnions in both the VTexts
--
-- >>> mergeCC 1 (VText [Chc 1 (VText[Plain "ab"]) (VText[Plain "xy"]), Plain "cde"],1 ) (VText [Chc 1 (VText[Plain "ab"]) (VText[Chc 2 (plain "x") (plain "i"),Plain "y"]), Plain "cde"],2)
-- (1<ab,2<x,i>y>cde,2)
-- >>> mergeCC 1 (VText [Chc 1 (VText[Plain "ab"]) (VText[Chc 2 (plain "x") (plain "i"),Plain "y"]), Plain "cde"],2 ) (VText [Chc 1 (VText[Plain "ab"]) (VText[Plain "xy"]), Plain "cde"],1)
-- (1<ab,2<x,i>y>cde,2)
--
-- | Chain edits
-- >>> mergeCC 2 (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]),Plain "c"],2) (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Chc 3 (plain "y") (plain "i")]),Plain "c"],3)
-- (1<ab,2<x,z>3<y,i>>c,3)
-- >>> mergeCC 2 (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Chc 3 (plain "y") (plain "i")]),Plain "c"],3) (VText [Chc 1 (plain "ab") (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]),Plain "c"],2)
-- (1<ab,2<x,z>3<y,i>>c,3)
--
-- | branch edits
-- >>> mergeCC 2 (VText [Chc 1 (VText[Chc 2 (VText[Chc 3 (plain "x") (plain "i") ]) (plain "z"),Plain "y"]) (plain "ab"),Plain "c"],3) (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (plain "ab"),Plain "c"],2)
-- (1<2<3<x,i>,z>y,ab>c,3)
-- >>> mergeCC 2 (VText [Chc 1 (VText[Chc 2 (VText[Chc 3 (plain "x") (plain "i") ]) (plain "z"),Plain "y"]) (VText[Chc 4 (plain "a") (VText[Chc 5 (plain "j") (plain "k")]),Plain "b"]),Plain "c"],3) (VText [Chc 1 (VText[Chc 2 (plain "x") (plain "z"),Plain "y"]) (VText[Chc 4 (plain "a") (plain "j"),Plain "b"]),Plain "c"],2)
-- (*** Exception: Prelude.undefined
--
