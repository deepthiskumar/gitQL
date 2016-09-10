{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module CCLib where

  import Data.List

  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Char
  import qualified Data.Algorithm.Diff as D
  --import qualified Diff as D
  import VText
  import Pretty
  --import Data.Vector.Unboxed as Uvector  hiding ((++),concat,length,tail,map,take,drop,concatMap,maximum,reverse)

  escape = "ⱺ" --"@"--

  data Sel = LSel !Int | RSel !Int
  type Selection = [Sel]

  data Alt = L | R deriving Eq

  instance Show (Sel) where
    show (LSel d) = (show d) ++ ".l"
    show (RSel d) = (show d) ++ ".r"

  -- Use a sequence of child indices as a path type:
  type Path = [Int]
  type VMap = [(Int, Path)]

  --lift :: [a] -> VText
  --lift = (VText(:[])) . Plain

--  Customized diff
--------------------------------------------------------------------------------

  data Diff a = Same [a] | Different [a] [a] deriving Show

  collectDiff :: [D.Diff [a]] -> [Diff a]
  collectDiff [] = []
  collectDiff ((D.Both x _):ds) = Same x : collectDiff ds
  collectDiff ((D.First x):(D.Second y):ds) = Different x y : collectDiff ds --update
  collectDiff ((D.First x):ds) = Different x [] : collectDiff ds --delete
  collectDiff ((D.Second x):ds) = Different [] x : collectDiff ds --insert

--let v = f k x in v `seq` Tip k v
-- gives the diff output. custom diff so that the 'Both' data contructor can be changed to take only single argument
--and identify different typed of changes ie., change, delete, insert
  (-?-) :: [Char] -> [Char] -> [Diff Char]
  old -?- new = collectDiff $ D.getGroupedDiff old new


  _diffAsCC :: [Diff Char] -> String
  _diffAsCC [] = ""
  _diffAsCC ((Same x):ds) = x ++ _diffAsCC ds
  _diffAsCC ((Different x y):ds) = concat [
      escape,
      "<",
      x,
      escape,
      ",",
      y,
      escape,
      ">",
      _diffAsCC ds
    ]

  partitionDiff :: [Diff a] -> [Int] -> [Diff a]
  partitionDiff d bs = p d bs
    where
      p :: [Diff a] -> [Int] -> [Diff a]
      p [] _ = []
      p ds [] = ds
      p ds (0:bs) = p ds bs
      p (d:ds) (b:bs) = case d of
        Same x        -> t x x $! flip $! const Same
        Different x y -> t x y Different
        where
          t x y f = case length x `compare` b of
            EQ -> f x y   : (p ds $! tail bs')
            LT -> f x y   : (p ds bs'')
            GT -> f x' y' : (p (f x'' y'' : ds) $! tail bs')
            where
              bs' = map (subtract b) $! b:bs
              bs'' = map (subtract $! length x) $! b:bs
              x'  = take b x
              x'' = drop b x
              y'  = take b y
              y'' = drop b y

  diffBoundaries :: [Diff a] -> [Int]
  diffBoundaries [] = []
  diffBoundaries ((Same x):ds) = 0 : (map (+ length x) $! diffBoundaries ds)
  diffBoundaries ((Different x _):ds) = 0 : (map (+ length x) $! diffBoundaries ds)

  diffL :: [Diff a] -> [a]
  diffL [] = []
  diffL ((Same x):ds) = x ++ diffL ds
  diffL ((Different x _):ds) = x ++ diffL ds

  diffR :: [Diff a] -> [a]
  diffR [] = []
  diffR ((Same x):ds) = x ++ diffR ds
  diffR ((Different _ x):ds) = x ++ diffR ds

  showVText :: VText -> String
  showVText (VText ss) = concatMap showSegment ss

  showSegment :: Segment -> String
  showSegment (Plain t)     = t
  showSegment (Chc d v v') = showChcNoColor d (map showVText [v,v'])

--  Parsers
--------------------------------------------------------------------------------

  ccFile :: GenParser Char st VText
  ccFile = do
    doc <- many ccConstruct
    eof
    return $! (VText doc)

  ccConstruct :: GenParser Char st (Segment)
  ccConstruct = do
    try ccChoice <|> ccPlain

  ccPlain :: GenParser Char st (Segment)
  ccPlain = fmap Plain $! many1 $! noneOf escape

  ccChoice :: GenParser Char st (Segment)
  ccChoice = try $! do
    string escape
    dim <- many1 alphaNum
    char '<'
    left <- many ccConstruct
    string escape
    char ','
    right <- many ccConstruct
    string escape
    char '>'
    return $! Chc (read dim :: Int) (VText left) (VText right)

  ccParser :: String -> Either ParseError VText
  ccParser = parse ccFile ""

  dimensions :: VText -> [Int]
  dimensions (VText []) = []
  dimensions (VText ((Plain _):vs)) = dimensions (VText vs)
  dimensions (VText ((Chc d l r):vs)) =
    [d] `union`
      (dimensions l) `union`
      (dimensions r) `union`
      (dimensions (VText vs))

  nextDimension :: VText -> Int
  nextDimension = succ . maximum . dimensions

  latest :: VText -> [Sel]
  latest = map RSel . dimensions
---------------------------------------------------------------------------------
  ppVText :: VText -> String
  ppVText (VText xs) = concatMap show xs

  applySelectionWithMap :: Selection-> VText -> (Text, VMap)
  applySelectionWithMap s = a [0] 0
    where
      a :: Path -> Int -> VText -> (Text, VMap)
      a _ _ (VText []) = ([], [])
      a p o (VText((Plain b):vs)) =
        (b, (o, reverse p)) ^: a (psucc p) (o + length b) (VText vs)
      a p o (VText ((Chc d l r):vs)) =
        a' ^++ a (psucc p) (o + length v')(VText vs)
        where
          c = case d `asSelectedIn` s of { L -> l; R -> r }
          a'@(v', _) = a (0:p) o c

      (^:) :: (Text, (Int, Path)) -> (Text, VMap) -> (Text, VMap)
      (v, p) ^: (vs, ps) = (v ++ vs, p:ps)

      (^++) :: (Text, VMap) -> (Text, VMap) -> (Text, VMap)
      (v, p) ^++ (vs, ps) = (v ++ vs, p ++ ps)

      psucc (p:ps) = succ p : ps

  applySelection :: [Sel] -> VText -> Text
  applySelection s v = fst $! applySelectionWithMap s v

  asSelectedIn :: Int -> [Sel] -> Alt
  asSelectedIn d ((LSel d'):ss) = if d == d' then L else d `asSelectedIn` ss
  asSelectedIn d ((RSel d'):ss) = if d == d' then R else d `asSelectedIn` ss


  denormalizeV ::Selection -> VText -> [Int] -> VText
  denormalizeV s v b = fst $! d s v b
    where
      (^:) :: Segment -> (VText, [Int]) -> (VText, [Int])
      x ^: (VText y, z) = (VText(x:y), z)

      d :: Selection -> VText -> [Int] -> (VText, [Int])
      d _ (VText []) bs = (VText [], bs)
      d _ vs [] = (vs, [])
      d s vs (0:bs) = d s vs bs
      d s (VText((Chc dim l r):vs)) bs = case dim `asSelectedIn` s of
        L -> e id   l r
        R -> e flip r l
        where
          e f x y = ((f $! Chc dim) x' y) ^: (d s (VText vs) bs')
            where
              (x', bs') = d s x bs
      d s (VText((Plain x):vs)) (b:bs) = case length x `compare` b of
        EQ -> (Plain x) ^: (d s (VText vs) $! tail bs')
        LT -> (Plain x) ^: (d s (VText vs) bs'')
        GT -> (Plain x') ^: (d s (VText((Plain x''):vs)) $! tail bs')
        where
          bs'   = map (subtract b) $! b:bs
          bs''  = map (subtract $! length x) (b:bs)
          x'    = take b x
          x''   = drop b x

  normalize :: VText -> VText
  normalize (VText []) = VText []
  normalize (VText ((Plain x):(Plain y):z)) = normalize $! VText (Plain (x ++ y) : z)
  normalize (VText (x@(Plain _):y)) = case (normalize (VText y)) of VText ys -> VText (x:ys)
  normalize (VText ((Chc d (VText l) (VText r)):(Chc d' (VText l') (VText r')):z)) = if d == d'
    then  normalize $! VText (Chc d (normalize $! (VText (l ++ l'))) (normalize $ (VText (r ++ r'))) : z)
    else  VText ((Chc d (normalize (VText l)) (normalize (VText r))) :
            (Chc d' (normalize (VText l')) (normalize (VText r'))) : (case (normalize (VText z)) of VText zs -> zs) )
  normalize (VText ((Chc d l r):z)) =
    VText (Chc d (normalize l) (normalize r) : (case (normalize (VText z)) of VText zs -> zs) )

--Unnesting
-----------------------------------------------------------------------------------------
{-unnest :: VText -> VText
unnest VText [] = VText []
unnest VText xs = unnestRecur xs

unnestRecur :: [Segment] -> [Segment]
unnestRecur [] = []
unnestRecur (x:xs) = case x of
                      Plain a -> [Plain a] ++ (unnestRecur xs)
                      c -> (unnestChoices c) ++ (unnestRecur xs)

unnestChoices :: Segment -> [Segment]
unnextChoices [] = []
unnestChoices Chc d v1 (VText vs) = case (last vs) of
			             Chc d1 (VText [Plain ""]) (VText [Plain a]) -> (unnestChoices $ Chc d v1 (VText (init vs))) ++ [Chc d1 (VText [Plain ""]) (VText [Plain a])]
                                     _ -> [Chc d v1 (VText vs]
unnestChoices (Plain a) = [Plain a]

sepSegments :: [Segment] -> [Segment]
sepSegments [] = []
sepSegments xs = case last xs of --last takes O(n). Use sequence which has constant time access to the fisrt and last element
                  Plain x -> xs
                  Chc d
 -}


--  Distillation
--------------------------------------------------------------------------------

  distill :: Int -> VText -> Selection -> Text -> VText
  distill dim v s n = normalize $! fst $! l pv d
    where
      (o, m) = s `applySelectionWithMap` v
      d = partitionDiff (o -?- n) $! map fst m
      pv = denormalizeV s v $! diffBoundaries d

      (^:) :: Segment-> (VText, [Diff Char]) -> (VText, [Diff Char])
      x ^: (VText y, z) = (VText (x:y), z)

      l :: VText -> [Diff Char] -> (VText, [Diff Char])

      l (VText []) ((Different [] x):ds) = (VText [Chc dim (VText []) (VText [Plain x])], ds)

      l (VText []) ds = (VText [], ds)

      -- Unchanged plain text:
      l (VText ((Plain x):vs)) ((Same _):ds) = Plain x ^: (l (VText vs) ds)

      -- Addition:
      l vs ((Different [] x):ds) = Chc dim (VText []) (VText [Plain x]) ^: (l vs ds)

      -- Removal:
      l (VText ((Plain x):vs)) ((Different x' []):ds) =
        Chc dim (VText [Plain x]) (VText []) ^: (l (VText vs) ds)

      -- Changed plain:
      l (VText ((Plain x):vs)) ((Different x' y):ds) = if x == x'
        then Chc dim (VText [Plain x]) (VText [Plain y]) ^: (l (VText vs) ds)
        else error $! concat [ "Mismatch ", show x, " /= ", show x' ]

      -- Recurse downward along choice:
      l (VText ((Chc dim' left right):vs)) ds = case dim' `asSelectedIn` s of
        L -> l' id    left right
        R -> l' flip  right left
        where
          l' f x y = ((f $! Chc dim') x' y) ^: (l (VText vs) ds')
            where
              (x', ds') = l x ds

--General helper functions
------------------------------------------------------------------------------
  stripNewline :: [Char] -> [Char]
  stripNewline []                 = []
  stripNewline ('\n' :[])         = []
  stripNewline (x:xs)             = x : stripNewline xs


  errorIf :: Bool -> String -> IO ()
  errorIf b m = if b then print m else return ()
