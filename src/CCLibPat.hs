{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, DeriveGeneric, DeriveAnyClass #-}

module CCLibPat where

  import Data.List as L

  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Char
  import qualified Data.Algorithm.Patience as D
  import Data.List.Split
  import Pretty
  import Debug.Trace ( trace )
  import qualified Data.Set as S
  import Control.DeepSeq
  import GHC.Generics (Generic)
  import Data.Text as T
  import Types
  
  {-type VString = [Segment]

  type Dim = Int

  data Segment = Str Text | Chc Dim VString VString
             deriving(Show, Eq, Generic, NFData, Ord)-}

  --intermediate vstring that has tokens.
  --This is to maintain the tokens instead of the tokenize-concat-tokenize-concat
  --that happens in the denormalize function and l       
  type VStringIntr = [SegmentIntr]
  data SegmentIntr = StrIntr [Text] |ChcIntr Dim VStringIntr VStringIntr

  escape = "ⱺ"

  -- Use a sequence of child indices as a path type:
  type Path = [Int]
  type VMap = [(Int, Path)]

  --lift :: [a] -> VText
  --lift = (VText(:[])) . Plain

--  Customized diff
--------------------------------------------------------------------------------

  data Diff a = Same ![a] | Different ![a] ![a] deriving (Show, Generic, NFData)
  
  data Edit a = Add !a | Delete !a | Update !a !a | NoChange !a

  collectDiff :: [D.Item a] -> [Diff a]
  collectDiff [] = []
  collectDiff ((D.Both x _):ds) = appendD (NoChange x) (collectDiff ds)
  collectDiff ((D.Old x):(D.New y):ds) = appendD (Update x y) (collectDiff ds) --update
  collectDiff ((D.Old x):ds) = appendD (Delete x) (collectDiff ds) --delete
  collectDiff ((D.New x):ds) = appendD (Add x) (collectDiff ds) --insert

  appendD :: Edit a -> [Diff a] -> [Diff a]
  appendD (Add x) (Different [] y : ds)       = (Different [] (x:y)) : ds
  appendD (Delete x) (Different y [] : ds)    = (Different (x:y) []) : ds
  --appendD (Add x) (Different y [] : ds)       = (Different [] (x:y)) : ds
  --appendD (Delete x) (Different [] y : ds)    = (Different (x:y) []) : ds
  appendD (Update x y) (Different x' y' : ds) = (Different (x:x') (y:y')) : ds
  appendD (NoChange x) (Same y : ds)          = (Same (x:y)) : ds
  appendD (Add x) ds                          = (Different [] [x]) : ds
  appendD (Delete x) ds                       = (Different [x] []) : ds
  appendD (Update x y) ds                     = (Different [x] [y]) : ds
  appendD (NoChange x) ds                     = (Same [x]) : ds
  

--let v = f k x in v `seq` Tip k v
-- gives the diff output. custom diff so that the 'Both' data contructor can be changed to take only single argument 
--and identify different typed of changes ie., change, delete, insert
  (-?-) :: [Text] -> [Text] -> [Diff Text]
  old -?- new = collectDiff $ D.diff old new


  _diffAsCC :: [Diff Char] -> String
  _diffAsCC [] = ""
  _diffAsCC ((Same x):ds) = x ++ _diffAsCC ds
  _diffAsCC ((Different x y):ds) = L.concat [
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

  partitionDiff :: Show a => [Diff a] -> [Int] -> [Diff a]
  partitionDiff d bs = p d bs {-trace ("PartDiff: "++ show res)-}
    where
      p :: Show a => [Diff a] -> [Int] -> [Diff a]
      p [] _ = []
      p ds [] = ds
      p ds (0:bs) = p ds bs
      p (d:ds) (b:bs) = case d of
        Same x        -> t x x $ flip $ const Same
        Different x y -> t x y Different
        where
          t x y f = {-trace ("Partition Diff "++show x ++ " "++ show y ++ show b)-} (case L.length x `compare` b of
            EQ -> f x y   : (p ds $ L.tail bs')
            LT -> f x y   : (p ds bs'')
            GT -> f x' y' : (p (f x'' y'' : ds) $ L.tail bs'))
            where
              bs'  = L.map (subtract b) $ b:bs
              bs'' = L.map (subtract $ L.length x) $ b:bs
              x'   = L.take b x
              x''  = L.drop b x
              y'   = L.take b y
              y''  = L.drop b y

  diffBoundaries :: [Diff a] -> [Int]
  diffBoundaries [] = []
  diffBoundaries ((Same x):ds) = 0:L.length x:diffBoundaries ds --0 : (L.map (+ L.length x) $ diffBoundaries ds)
  diffBoundaries ((Different x _):ds) = 0:L.length x:diffBoundaries ds -- 0 : (L.map (+ L.length x) $ diffBoundaries ds)

  diffL :: [Diff a] -> [a]
  diffL [] = []
  diffL ((Same x):ds) = x ++ diffL ds
  diffL ((Different x _):ds) = x ++ diffL ds

  diffR :: [Diff a] -> [a]
  diffR [] = []
  diffR ((Same x):ds) = x ++ diffR ds
  diffR ((Different _ x):ds) = x ++ diffR ds

  showVText :: VString -> String
  showVText [] = ""
  showVText [s] = showSegment s
  showVText (s:ss) = (showSegment s) {-++ (if endsWithNewLine' s then "" else " " )-} ++ (showVText ss)
  --showVText (VText ss) = --concatMap showSegment ss

  showSegment :: Segment -> String
  showSegment (Str t)     = unpack t--showText t--intercalate " " t
  showSegment (Chc d v v') = showChcNoColor (show d) (L.map showVText [v,v'])
  
  
  --tokenizes into words and also maintains the spaces between them as tokens.
  tokenizer :: Text -> [Text]
  tokenizer s
    | T.null s = []
    | otherwise = tokenizer' s
  
  tokenizer' :: Text -> [Text]
  tokenizer' s 
    | T.null s  = []
    | otherwise = 
      let (spaces, s1) = T.break (/=' ') s
          (lines, s2)  = T.break (/='\n') s1
          (line, s3)   = T.break (=='\n') s2
          words        = getwords line
          (nlines,s4)  = T.break (/= '\n') s3
      in noEmpty spaces ++ noEmpty lines ++ words ++ noEmpty nlines ++ tokenizer s4
    
  getwords :: Text -> [Text]
  getwords line  
    | T.null line = []
    | otherwise   = w $! T.break (==' ') line
      where
       w :: (Text,Text) -> [Text]
       w (word, s1) = u (T.break (/=' ') s1) word
     
       u :: (Text,Text) -> Text -> [Text]
       u (spaces, s2) word1 = noEmpty word1 ++ noEmpty spaces ++ getwords s2
   
   
  noEmpty :: Text -> [Text]
  noEmpty s  
    | T.null s  = []
    | otherwise = [s]
  
--  Parsers
--------------------------------------------------------------------------------

  ccFile :: GenParser Char st VString
  ccFile = do
    doc <- many ccConstruct
    eof
    return (emptyStr doc)

  ccConstruct :: GenParser Char st (Segment)
  ccConstruct = do
    try ccChoice <|> ccPlain

  ccPlain :: GenParser Char st (Segment)
  ccPlain = fmap (Str.(T.pack)) $ many1 $ noneOf escape{-try $ do 
    p <- many1 $ noneOf escape
    trace (show p ++ "AND" ++ show (tokenizer p)) (return $ Plain (tokenizer p))-}
  
  emptyString :: GenParser Char st (Segment)
  emptyString = do
    string ""
    return $ Str T.empty

  ccChoice :: GenParser Char st (Segment)
  ccChoice = try $ do
    string escape
    dim <- many1 alphaNum
    char '<'
    left <- many ccConstruct
    string escape
    char ','
    right <- many ccConstruct
    string escape
    char '>'
    return $ Chc (read dim :: Int) (emptyStr left) (emptyStr right)
    
  emptyStr :: VString -> VString
  emptyStr [] = [Str T.empty]
  emptyStr xs = xs

  ccParser :: String -> Either ParseError VString
  ccParser = parse ccFile ""

  dimensions :: VString -> [Int]
  dimensions [] = []
  dimensions ((Str _):vs) = dimensions vs
  dimensions ((Chc d l r):vs) = --let dims = dimensions (VText vs) in dims `seq`
      (d :
      (dimensions l) ++
      (dimensions r) ++
      dimensions vs )--dims )
 -- a choice will not have nested choice with dimension same as the outer choice. 
 -- This invariant is applicable in the change commit history scenario only
 -- Therefore `union` can be replaced with simple concatenation
 
  betterNub :: [Int] -> [Int]
  betterNub = n S.empty
    where
      n :: S.Set Int -> [Int] -> [Int]
      n _ []     = []
      n s (x:xs) 
        | S.member x s = n s xs
        | otherwise    = x : n (S.insert x s) xs

  nextDimension :: VString -> Int
  nextDimension = succ . L.maximum . dimensions

  latest :: VString -> [Sel]
  latest = L.map RSel . (betterNub.dimensions)
---------------------------------------------------------------------------------
  ppVText :: VString -> String
  ppVText xs = L.concatMap show xs

  applySelectionWithMap :: Selection-> VString -> (Text, VMap)
  applySelectionWithMap s = a [0] 0
    where
      a :: Path -> Int -> VString -> (Text, VMap)
      a _ _ [] = (T.empty, [])
      a p o ((Str b):vs) =
        (b, (o, L.reverse p)) ^: a (psucc p) (o + L.length (tokenizer b)) vs
      a p o ((Chc d l r):vs) =
        a' ^++ a (psucc p) (o + L.length (tokenizer v')) vs
        where
          c = case d `asSelectedIn` s of { L -> l; R -> r }
          a'@(v', _) = a (0:p) o c

      (^:) :: (Text, (Int, Path)) -> (Text, VMap) -> (Text, VMap)
      (v, p) ^: (vs, ps) = (T.append v vs, p:ps)

      (^++) :: (Text, VMap) -> (Text, VMap) -> (Text, VMap)
      (v, p) ^++ (vs, ps) = (T.append v vs, p ++ ps)

      psucc (p:ps) = succ p : ps

  applySelection :: [Sel] -> VString -> Text
  applySelection s v = fst $ applySelectionWithMap s v

  asSelectedIn :: Int -> [Sel] -> Alt
  asSelectedIn d ((LSel d'):ss) = if d == d' then L else d `asSelectedIn` ss
  asSelectedIn d ((RSel d'):ss) = if d == d' then R else d `asSelectedIn` ss


  denormalizeV ::Selection -> VString -> [Int] -> VString
  denormalizeV s v b = fst $ (d s v b)
  
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
       e f x y = ((f $ Chc dim) x' y) ^: z
         where
           (x', bs') = ((d $ s) $ x) $ bs
           z =( ((d $ s) $ vs) $ bs')
  d s ((Str x):vs) (b:bs) = plainHelper (L.length (tokenizer x) `compare` b) s x vs (b:bs) 
          
  plainHelper :: Ordering -> Selection -> Text -> VString -> [Int] -> (VString, [Int])
  plainHelper EQ s x vs (b:bs) = (^:) (Str x) (z s vs bs)--(b:bs))
  plainHelper LT s x vs (b:bs) 
     | T.null x            = (Str x) ^: (d s vs (b:bs))
     | otherwise           = trace("LT :"++ show x ++ " b= " ++ show b ++ ":"++ show bs ++ " VS: " ++ show vs) undefined
     --((Str x) ^: (d s vs (bs'' x (b:bs)))) --unknown case
  plainHelper GT s x vs (b:bs) = (Str (T.concat (x' b x))) ^: (z' s x (T.length (T.concat (x' b x))) vs (b:bs))
  
  bs1' (b:bs) = (L.map (subtract b) $ b:bs)
  
  bs'' x (b:bs) = L.map (subtract $ L.length (tokenizer x)) (b:bs)
  
  x' :: Int -> Text -> [Text]
  x' b x = (L.take $ b) $ (tokenizer $ x)
  
  x'' :: Int -> Text -> [Text]
  x'' b x  = (L.drop $ b) $ (tokenizer $ x)
  
  z :: Selection -> VString -> [Int] -> (VString, [Int])
  z s vs bs = d s vs bs--(L.tail $ bs1' bs) --bs
  
  z' :: Selection -> Text -> Int -> VString -> [Int] -> (VString, [Int])
  z' s x rem vs (b:bs) = d s ((Str (T.drop rem x )) :vs) bs
  
  
  {-z' :: Selection -> Text -> Int -> VString -> [Int] -> (VString, [Int])
  z' s x rem vs (b:bs) = d s ((Str $ T.concat $ x'' b x) : vs)(L.tail $ bs1' (b:bs)) --d s ((Str (T.drop rem x )) :vs) (L.tail $ bs1' (b:bs))--bs--(((d $ s) $!! ((Str $ (T.concat $!! (x'' b x))):vs)) $!! bs)-}
  
  
        
      {-trace ("Denormalize: " ++ show x ++ " | " ++ show b )-} {-(case length (tokenizer x) `compare` b of
        EQ -> ((^:) $!! (Plain $!! x)) $!! z
        LT -> trace("LT :"++ show x ++ " b= " ++ show b ++ " VS: " ++ show vs) undefined--((Plain x) ^: (d s (VText vs) bs'')) -- is this a valid case??
        GT -> (((^:) $!! (Plain $!! (concat $!! x'))) $!! z'))
         where
          bs1'  = (map (subtract b) $ b:bs)
          bs''  = map (subtract $ length (tokenizer x)) (b:bs)
          x'    = (take $!! b) $!! (tokenizer $!! x)
          x''   = (drop $!! b) $!! (tokenizer $!! x)
          z     = (((d $!! s) $!! (VText $!! vs)) $!! bs)--tail bs1')
          z'    = (((d $!! s) $!! (VText $!! ((Plain $!! (concat $!! x'')):vs))) $!! bs)--tail bs1')-}
          
          --How to implment such that we do not have to subtract. create like a list of lists and just remove the head element
  

  normalize :: VString -> VString
  normalize []                  = []
  normalize ((Str x):(Str y):z) = normalize $ (Str (T.append x y) : z)
  normalize (x@(Str _):y)       = x: normalize y
  normalize ((Chc d l r):(Chc d' l' r'):z) 
      | d == d'    = normalize $ (Chc d (normalize $ (l ++ l')) (normalize $ (r ++ r')) : z)
      | otherwise  = (Chc d (normalize l) (normalize r)) : normalize ((Chc d' l' r'):z)
  normalize ((Chc d l r):z) = (Chc d (normalize l) (normalize r)) : (normalize z)

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
  distill dim v s n = {-trace (show pv ++ "\n" ++ show d)-} (normalize $ fst $ ((l $ pv) $ d)) --deepseq pv
    where
      (o, m) = s `applySelectionWithMap` v
      d      = ((partitionDiff $ ((tokenizer $ o) -?- (tokenizer $ n))) $ L.map fst m)
      pv     = {-trace ("Partitioned Diffs: "++ show d ++ " Boundaries: "++ show (diffBoundaries $ d))-} (((denormalizeV $ s) $ v) $ diffBoundaries $ d)

      (^:) :: Segment-> (VString, [Diff Text]) -> (VString, [Diff Text])
      x ^: (y, z) = ((x:y), z)

      l :: VString -> [Diff Text] -> (VString, [Diff Text])

      l [] di@((Different [] x):ds) = {-trace ("1. [] and "++ show di)-} ([Chc dim [] [Str (T.concat x)]], ds)

      l [] ds = ([], ds)
      
      l [Str x] [] 
         | T.null x = ([Str x], [])
         |otherwise = {-trace ("3."++ show x)-} undefined

      -- Unchanged plain text:
      l vt@((Str x):vs) dif@((Same y):ds)
        | T.null x         = (Str x) ^: l vs dif --empty String
        |  x == T.concat y = {-trace ("2."++ show vt ++ " and "++ show dif)-} (Str x ^: (l vs ds))
        | otherwise = {-trace ("2."++ show x ++ " and "++ show y)-} undefined

      -- Addition:
      l vs dif@((Different [] x):ds) = {-trace ("3."++ show vs ++ " and "++ show dif)-} (Chc dim [] ([Str (T.concat x)]) ^: (l vs ds))

      -- Removal:
      l vt@((Str x):vs) dif@((Different x' []):ds) = {-trace ("4."++ show vt ++ " and "++ show dif)-} 
        (Chc dim [Str x] [] ^: (l vs ds))

      -- Changed plain:
      l vt@((Str x):vs) dif@((Different x' y):ds) = {-trace ("5."++ show vt ++ " and "++ show dif)-} (if x == (T.concat x')
        then Chc dim [Str x] [Str (T.concat y)] ^: (l vs ds)
        else error $ L.concat [ "Mismatch ", show vt, " /= ", show x', " and ", show y ])

      -- Recurse downward along choice:
      l vt@((Chc dim' left right):vs) ds = {-trace ("6."++ show vt ++ " and "++ show ds)-} (case dim' `asSelectedIn` s of
        L -> l' id    left right
        R -> l' flip  right left)
        where
          l' f x y = ((f $ Chc dim') x' y) ^: (l vs ds')
            where
              (x', ds') = l x ds
      
      l vs ds = {-trace (show vs ++ " and " ++ show ds)-} undefined
