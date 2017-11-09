{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module CCLib where

  import Data.List

  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Char
  import qualified Data.Algorithm.Diff as D
  import Data.List.Split
  --import qualified Diff as D
  import VPMNew
  --import Pretty
  --import Data.Vector.Unboxed as Uvector  hiding ((++),concat,length,tail,map,take,drop,concatMap,maximum,reverse)

  escape = "ⱺ"
  type Text = String
  data Sel = LSel !Int | RSel !Int
  type Selection = [Sel]


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
  (-?-) :: [String] -> [String] -> [Diff String]
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

  {-showVString :: VString -> String
  showVString ss = concatMap showSegment ss

  showSegment :: Segment -> String
  showSegment (Str t)     = t
  showSegment (Chc d v v') = showChcNoColor d (map showVString [v,v'])-}
  
  --tokenizes into words and also maintains the spaces between them as tokens.
  tokenizer :: String -> [String]
  tokenizer "" = []
  tokenizer s = 
    let (spaces, s1) = break (/=' ') s
        (lines, s2)  = break (/='\n') s1
        (line, s3)   = break (=='\n') s2
        words        = getwords line
        (nlines,s4)  = break (/= '\n') s3
    in noEmpty spaces ++ noEmpty lines ++ words ++ noEmpty nlines ++ tokenizer s4
    
  getwords :: String -> [String]
  getwords ""    = []
  getwords line  = 
   let (word, s1)   = break (==' ') line
       (spaces, s2) = break (/=' ') s1
   in noEmpty word ++ noEmpty spaces ++ getwords s2
   
  noEmpty :: String -> [String]
  noEmpty "" = []
  noEmpty s  = [s]

--  Parsers
--------------------------------------------------------------------------------

  ccFile :: GenParser Char st VString
  ccFile = do
    doc <- many ccConstruct
    eof
    return $ doc

  ccConstruct :: GenParser Char st (Segment)
  ccConstruct = do
    try ccChoice <|> ccPlain

  ccPlain :: GenParser Char st (Segment)
  ccPlain = fmap Str $! many1 $! noneOf escape

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
    return $! Chc (read dim :: Int) (epsilon left) (epsilon right)
  
  epsilon :: VString -> VString
  epsilon [] = [Str ""]
  epsilon vs = vs

  ccParser :: String -> Either ParseError VString
  ccParser = parse ccFile ""

  dimensions :: VString -> [Int]
  dimensions [] = []
  dimensions ((Str _):vs) = dimensions vs
  dimensions ((Chc d l r):vs) =
    [d] `union`
      (dimensions l) `union`
      (dimensions r) `union`
      (dimensions vs)

  nextDimension :: VString -> Int
  nextDimension = succ . maximum . dimensions

  latest :: VString -> [Sel]
  latest = map RSel . dimensions
---------------------------------------------------------------------------------
  ppVText :: VString -> String
  ppVText xs = concatMap show xs

  applySelectionWithMap :: Selection-> VString -> (Text, VMap)
  applySelectionWithMap s = a [0] 0
    where
      a :: Path -> Int -> VString -> (Text, VMap)
      a _ _ [] = ([], [])
      a p o ((Str b):vs) =
        (b, (o, reverse p)) ^: a (psucc p) (o + length (tokenizer b)) (vs)
      a p o ((Chc d l r):vs) =
        a' ^++ a (psucc p) (o + length (tokenizer v'))(vs)
        where
          c = case d `asSelectedIn` s of { L -> l; R -> r }
          a'@(v', _) = a (0:p) o c

      (^:) :: (Text, (Int, Path)) -> (Text, VMap) -> (Text, VMap)
      (v, p) ^: (vs, ps) = (v ++ vs, p:ps)

      (^++) :: (Text, VMap) -> (Text, VMap) -> (Text, VMap)
      (v, p) ^++ (vs, ps) = (v ++ vs, p ++ ps)

      psucc (p:ps) = succ p : ps

  applySelection :: [Sel] -> VString -> Text
  applySelection s v = fst $! applySelectionWithMap s v

  asSelectedIn :: Int -> [Sel] -> Alternative
  asSelectedIn d ((LSel d'):ss) = if d == d' then L else d `asSelectedIn` ss
  asSelectedIn d ((RSel d'):ss) = if d == d' then R else d `asSelectedIn` ss


  denormalizeV ::Selection -> VString -> [Int] -> VString
  denormalizeV s v b = fst $! d s v b
    where
      (^:) :: Segment -> (VString, [Int]) -> (VString, [Int])
      x ^: (y, z) = ((x:y), z)

      d :: Selection -> VString -> [Int] -> (VString, [Int])
      d _ [] bs = ([], bs)
      d _ vs [] = (vs, [])
      d s vs (0:bs) = d s vs bs
      d s ((Chc dim l r):vs) bs = case dim `asSelectedIn` s of
        L -> e id   l r
        R -> e flip r l
        where
          e f x y = ((f $! Chc dim) x' y) ^: (d s vs bs')
            where
              (x', bs') = d s x bs
      d s ((Str x):vs) (b:bs) = case length (tokenizer x) `compare` b of
        EQ -> (Str x) ^: (d s vs $! tail bs')
        LT -> (Str x) ^: (d s vs bs'')
        GT -> (Str (concat x')) ^: (d s ((Str (concat x'')):vs) $! tail bs')
        where
          bs'   = map (subtract b) $! b:bs
          bs''  = map (subtract $! length (tokenizer x)) (b:bs)
          x'    = take b (tokenizer x)
          x''   = drop b (tokenizer x)

  normalize :: VString -> VString
  normalize [] = []
  normalize ((Str x):(Str y):z) = normalize $! (Str (x ++ y) : z)
  normalize (x@(Str _):y) = case (normalize ( y)) of  ys ->  (x:ys)
  normalize ((Chc d (l) (r)):(Chc d' (l') (r')):z) = if d == d'
    then  normalize $! (Chc d (normalize $! ( (l ++ l'))) (normalize $ ( (r ++ r'))) : z)
    else   ((Chc d (normalize ( l)) (normalize ( r))) :
            (Chc d' (normalize ( l')) (normalize ( r'))) : (case (normalize ( z)) of  zs -> zs) )
  normalize ( ((Chc d l r):z)) =
     (Chc d (normalize l) (normalize r) : (case (normalize ( z)) of  zs -> zs) )

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

  distill :: Int -> VString -> Selection -> Text -> VString
  distill dim v s n = normalize $! fst $! l pv d
    where
      (o, m) = s `applySelectionWithMap` v
      d = partitionDiff ((tokenizer o) -?- (tokenizer n)) $! map fst m
      pv = denormalizeV s v $! diffBoundaries d

      (^:) :: Segment-> (VString, [Diff String]) -> (VString, [Diff String])
      x ^: (y, z) = ((x:y), z)

      l :: VString -> [Diff String] -> (VString, [Diff String])

      l ( []) ((Different [] x):ds) = ( [Chc dim ( []) ( [Str (concat x)])], ds)

      l ( []) ds = ( [], ds)

      -- Unchanged plain text:
      l ( ((Str x):vs)) ((Same _):ds) = Str x ^: (l ( vs) ds)

      -- Addition:
      l vs ((Different [] x):ds) = Chc dim ( []) ( [Str (concat x)]) ^: (l vs ds)

      -- Removal:
      l ( ((Str x):vs)) ((Different x' []):ds) =
        Chc dim ( [Str x]) ( []) ^: (l ( vs) ds)

      -- Changed plain:
      l ( ((Str x):vs)) ((Different x' y):ds) = if x == (concat x')
        then Chc dim ( [Str x]) ( [Str (concat y)]) ^: (l ( vs) ds)
        else error $! concat [ "Mismatch ", show x, " /= ", show (concat x') ]

      -- Recurse downward along choice:
      l ( ((Chc dim' left right):vs)) ds = case dim' `asSelectedIn` s of
        L -> l' id    left right
        R -> l' flip  right left
        where
          l' f x y = ((f $! Chc dim') x' y) ^: (l ( vs) ds')
            where
              (x', ds') = l x ds
