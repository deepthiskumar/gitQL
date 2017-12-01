module GitParser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Error
import VPMNewTest
import VPMEngine
import Types
import Data.DateTime
import Data.Time
import Data.Text as T hiding (map)
import Data.Maybe

data Query = Query [Var] Search
 deriving(Show)

data Var = VStr String | VPos String | VCount String
  deriving(Show)

type Search = [MatchGen]

data MatchGen = MatchGen String Pattern Source (Maybe Conditions)
 deriving(Show)

data Source = FName String 
            | VBinding String
            | Q Query
            deriving(Show)
            

data Conditions = Cond Condition
                | Bin BoolOp Conditions Conditions
                | Not Conditions
                deriving(Show)

data BoolOp = And | Or deriving (Show)

data RelOp = Gr | Ls | Ge | Le | Equ | NotEqu 
  deriving(Show)
                
data Condition = CommitInfo RelOp Info Info
               | ResultComp RelOp String String
               deriving(Show)

data Info = CDate DimTy
          | CAuthor DimTy
          | DateVal DateTime
          | AuthorVal String
          deriving(Show)
          
{-instance Eq Info where
   (CDate d date) == (CDate d' date') = date == date'
   (CAuthor d auth) == (CAuthor d' auth') = auth == auth'
   DateVal date == DateVal date' = date == date'
   AuthorVal auth == AuthorVal auth' = auth == auth'
   CDate d date == DateVal date' = date == date'
   DateVal date == CDate d date' = date == date'
   CAuthor d auth == AuthorVal auth' = auth == auth'
   AuthorVal auth == CAuthor d' auth' = auth == auth'-}

{-instance Show Conditions where
  show (Cond cond) = show cond
  show (Bin f c c') = show c ++ " BoolOp " ++ show c'
  show (Not cond)   = "Not " ++ show c
  
instance Show Condition where
  show (CommitInfo i f i') = show i ++ "RelOp" ++ show i'
  show (ResultComp v f v') = show v ++ "RelOp" ++ show v'-}
  
--instance Show 
          
            

{-
languageDef =
>   emptyDef { Token.commentStart    = "/*"
>            , Token.commentEnd      = "*/"
>            , Token.commentLine     = "//"
>            , Token.identStart      = letter
>            , Token.identLetter     = alphaNum
>            , Token.reservedNames   = [ "if"
>                                      , "then"
>                                      , "else"
>                                      , "while"
>                                      , "do"
>                                      , "skip"
>                                      , "true"
>                                      , "false"
>                                      , "not"
>                                      , "and"
>                                      , "or"
>                                      ]
>            , Token.reservedOpNames = ["+", "-", "*", "/", ":="
>                                      , "<", ">", "and", "or", "not"
>                                      ]
>            }
-}


languageDef = emptyDef {
   Token.identStart       =   letter <|> char '_' ,
   Token.identLetter      =   alphaNum <|> char '_' <|> char '\'' ,
   Token.reservedNames    =   [ "from", "vgrep", "where", "in", "date", "author", "pos", "count" ], 
   Token.reservedOpNames  =   [">", "<", ">=", "<=", "==", "/=", "and", "or", "not"]
   }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens   lexer -- parses surrounding parenthesis:
                                     --   parens p
                                     -- takes care of the parenthesis and
                                     -- uses p to parse what's inside them
brackets   = Token.brackets   lexer
integer    = Token.integer    lexer -- parses an integer
commaSep1  = Token.commaSep1  lexer -- parse one or more ocurrences of p separated by comma 
whiteSpace = Token.whiteSpace lexer -- parses whitespace
stringLiteral = Token.stringLiteral lexer -- parses a literal string
symbol = Token.symbol lexer
braces     = Token.braces lexer

--operators = [[Infix (]]

whileParser :: Parser Query
whileParser = whiteSpace >> query

query :: Parser Query
query = query'--brackets query'

--parse the inside of [] in a query
query' :: Parser Query
query' = do
  vs <- listOfVals
  reserved "from"
  s <- matchGens
  return $ Query vs s
  
listOfVals :: Parser [Var]
listOfVals = commaSep1 value

value :: Parser Var
value = try (pos <|> countP <|> identifier' )

identifier' :: Parser Var
identifier' = do
  s <- identifier
  return $ VStr s
  
pos :: Parser Var
pos = do
  reserved "pos"
  s <- identifier
  return $ VPos s
  
countP :: Parser Var
countP = do
   reserved "count"
   s <- identifier
   return $ VCount s


matchGens :: Parser Search
matchGens = commaSep1 matchGens'

matchGens' :: Parser MatchGen
matchGens' = do
  m <- identifier
  symbol "<-"
  reserved "match"
  p <- many1Pattern
  reserved "in"
  s <- source
  c <- optionMaybe whereClause
  return $ MatchGen m p s c


{-
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
             deriving(Show,Eq)
             
type DimVarName = String
data DimTy = D Dim | DVar DimVarName deriving(Show,Eq)


"character"
|  _
|pattern pattern
|pattern|pattern
|commit〈pattern,pattern〉
|query-var
|(pattern)
-}

many1Pattern :: Parser Pattern
many1Pattern = do
  ps <- many1 cpattern
  return $ VPMNewTest.seq  ps

cpattern :: Parser Pattern
cpattern = try ( --seqPattern  <|> 
          altPattern <|>
          pPattern <|>
          basePattern-- <|>
      )

pPattern :: Parser Pattern
pPattern = parens many1Pattern
      
basePattern :: Parser Pattern
basePattern = try (  strP <|>
      choicePattern <|>
      underscore <|>
      queryVar
      )
      
strP :: Parser Pattern
strP = try( do
  s <- stringLiteral
  return $ VPMNewTest.seq $ map ch s )
  
underscore :: Parser Pattern
underscore = try( do
 symbol "_"
 return $ Any )
 
emptyString :: Parser Pattern
emptyString = try( do
 string ""
 return $ None)
 
choicePattern :: Parser Pattern
choicePattern =  try( do
  c <- commit
  symbol "<"
  l <- try ( many1Pattern <|> emptyString)
  symbol ","
  r <- try ( many1Pattern <|> emptyString)
  symbol ">"
  return $ PChc c l r)

commit :: Parser DimTy
commit = try (commitStr <|> commitVar)

commitStr :: Parser DimTy
commitStr = try( do
  symbol "#"
  dim <- integer--stringLiteral
  return $ D $ fromIntegral dim)
  
commitVar :: Parser DimTy
commitVar = try( do
  var <- identifier
  return $ DVar var)
  
queryVar :: Parser Pattern
queryVar = try( do   
 v <- identifier
 return $ QVar v)
 
altPattern :: Parser Pattern
altPattern = altOp--chainl1 many1Pattern altOp

altOp   =   --do{ symbol "|"; return (Alt)   }
 try( do
  p1 <- try ( basePattern <|> pPattern)
  symbol "|"
  p2 <- try ( basePattern <|> pPattern)
  return $ Alt p1 p2 )
  
  {- do
  ps <- sepBy (basePattern <|> pattern) (char '|')
  return $ alt ps -}
  
alt :: [Pattern] -> Pattern
alt [x] = x
alt (x:xs) = Alt x (alt xs) 
  
source :: Parser Source  
source = try (filename <|> vBinding <|> sQuery)

filename :: Parser Source
filename = try( do
  symbol "-f"
  i <- try (stringLiteral <|> many1 filenameChars)
  return $ FName (T.unpack $ T.strip $ T.pack i))
  
filenameChars :: Parser Char
filenameChars = try (alphaNum <|> (char '/') <|> (char '-') <|> (char '_') <|> (char '.') <|> (char '\\'))
  
vBinding :: Parser Source       
vBinding = try( do
 i <- identifier
 return $ VBinding i)
 
sQuery :: Parser Source
sQuery = try( do
  q <- braces query
  return $ Q q)
  
  
whereClause :: Parser Conditions
whereClause = do
 whiteSpace
 reserved "where"
 --whiteSpace
 c <- conditions
 --whiteSpace
 return c
 
conditions :: Parser Conditions
conditions  = try ((buildExpressionParser bOperators bterm) <|> getCond)

--condition :: Parser Condition
--condition = buildExpressionParser rOperators rterm

rterm = try(commitInfo <|> resultComp)

bOperators = [ [Prefix (reservedOp "not" >> return (Not))          ]
             , [Infix  (reservedOp "and" >> return (Bin And )) AssocLeft,
                Infix  (reservedOp "or"  >> return (Bin Or) ) AssocLeft]
             ]

bterm = parens conditions
    <|> getCond

getCond :: Parser Conditions
getCond = do
  c <- rterm
  return $ Cond c
    
commitInfo =
   do a1 <- info
      op <- rOperators
      a2 <- info
      return $ CommitInfo op a1 a2 
      
resultComp =
   do a1 <- identifier
      op <- rOperators
      a2 <- identifier
      return $ ResultComp op a1 a2
      
info :: Parser Info
info = try( commVal <|> dateAuthVal)

commVal :: Parser Info
commVal = do
  c <- commit
  symbol "."
  try ( (do {reserved "date" ; return (CDate c)}) <|> (do {reserved "author" ; return (CAuthor c)}))
  
dateAuthVal :: Parser Info
dateAuthVal = do
  s <- stringLiteral
  let d = readDate s
  case d of
    Nothing -> return $ AuthorVal s
    Just d  -> return $ DateVal d

readDate :: String -> Maybe DateTime
readDate t = parseDateTime dateFormat t

dateFormat = "%Y/%m/%d" --same as %Y-%m-%d

rOperators =  try( (reservedOp ">" >> return Gr)
               <|> (reservedOp "<" >> return Ls) 
               <|> (reservedOp ">=" >> return Ge) 
               <|> (reservedOp "<=" >> return Le) 
               <|> (reservedOp "==" >> return Equ)
               <|> (reservedOp "/=" >> return NotEqu))
               
  
parseString :: String -> Query
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ ("Syntax Error: " ++ show e)
    Right r -> r

parseWhere :: String -> Conditions
parseWhere str = 
  case parse whereClause "" str of
    Left e  -> error $ ("Syntax Error: " ++ show e)
    Right r -> r
---Helpers---------------------------------------------------

rightChoice :: Parser Pattern
rightChoice = try (do 
   p <- many1Pattern--try(many1Pattern <|> emptyString) 
   symbol ">"
   return p )
  
noneLeft :: Parser Pattern
noneLeft = do 
 symbol ","
 return None
 
noneRight :: Parser Pattern
noneRight = try(do
  emptyString
  symbol ">"
  return None)
   
seqPattern :: Parser Pattern
seqPattern =  try( do
 p1 <- try ( basePattern <|> cpattern)
 p2 <- try ( basePattern <|> cpattern)
 return $ Seq p1 p2 )--use lists for sequence of patterns?  

getDateTime :: String -> LocalTime
getDateTime s = parseTimeOrError True defaultTimeLocale "%Y/%m/%d" s

---TODO get commit id from the env 
getDateTimeDim :: DimTy -> LocalTime
getDateTimeDim s = parseTimeOrError True defaultTimeLocale "%Y/%m/%d" ""
