module Types where
import Data.Text as T

--Variational String

type TextType = Text
type VString = [Segment]

type Dim = Int

data Segment = Str Text | Chc Dim VString VString
             deriving(Show, Eq)

type Input = (Pos,VString)
--Patterns
data Atomic = C Char | Wild deriving(Show,Eq)

data Pattern = Plain Atomic
             | Seq Pattern Pattern
             | Alt Pattern Pattern
             | PChc DimTy Pattern Pattern
             | PChcStar DimTy Pattern Pattern
             | Repeat Pattern Int {-min-} (Maybe Int) {-max-}
             --Ex. `a*` => `Repeat (ch 'a') 0 None`
             | None --Ex. `(a|)b` => `Seq (Alt (ch 'a') None) (ch 'b')
             | QVar QVarName
             | Any
             deriving(Show,Eq)
             
type DimVarName = String
data DimTy = D Dim | DVar DimVarName deriving(Show,Eq)

type QVarName = String

--Variational Match
             
type VMatch = (MetaInfo,VString)

--The output type of vgrep is a list of pair of VMatch and query variable bindings
type FinalMatch = (VMatch, QVarEnv)

type Matches = [FinalMatch]

type MetaInfo = (Pos, DimEnv)

type Block  = Int
type Offset = Int

data Pos = P Block (Either Offset (Pos,Pos))
         | NoPos
         deriving(Show,Eq)
         
instance Ord Pos where
  NoPos <= NoPos = True
  P b o <= NoPos = False
  NoPos <= P b o = True
  P b o <= P b' o' 
    | b < b' = True
    | b > b' = False
    | b == b' = case (o,o') of
                 (Left i, Left i') -> i <= i
                 (Right (p,p'), Right (q,q')) -> p <= q && p' <= q' --TODO For p<=q and p' > q' we need to merge the vstring to equalize the offsets.   
         
type DimEnv = [(String,Dim)]

type QVarEnv = [(String, VMatch)]



startPos = (P 0 (Left 0))

emptyMatch :: FinalMatch
emptyMatch = (((NoPos,[]),[]),[])
