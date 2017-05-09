module NewTypes where
-- Experimenting with the types for vgrep inorder to support git queries
-- that include aggregation, query placeholders($x, $y), dimension variable
-- bindings

-- Search expession
type VString = [Segment]

type Dim = Int

data Segment = Str String | Chc Dim VString VString
             deriving(Show, Eq)

-- Previously the position (Int) was embedded within the VString
-- i.e., type VMatch = (Pos, Match)
--       data Match  = MStr String | MChc Dim [VMatch] [VMatch]
-- 
-- In order to compose different git queries, we need to retain the
-- structure of the source expression and therefore the following is the type
-- for the result of a vgrep

-- A variational match consists of position, dimension varaible bindings (in case of
-- choice patterns with dimension variables)  and the matched vstring

type VMatch = (MetaInfo,VString)

--The output type of vgrep is a list of pair of VMatch and query variable bindings

type Matches = [(VMatch,VarEnv)]

type MetaInfo = (Pos, DimEnv)

-- The positon of the string within in the choice structure has to also
-- follow the same structure. 

-- Consider the following examples that
-- illustrate this requirement

{-
Search Expression : ab1<2<xy3<z,o>cd,pqr>,lmn>  
VS = [Str "abc", 
      Chc 1 [Chc 2 [Str "xy",
                    Chc 3 [Str "z"] 
                          [Str "o"], 
                    Str "cd"] 
                   [Str "pqr"] ] 
            [Str ""lmn]
     ]

Exampe 1: Pattern = "b"
Pos = Plain 0 1

Example 2: Pattern = "d"
Pos = ChcP 1 (Plain 2 1) (NoPos)

Example 1: Pattern = "z"
Pos = ChcP 1 (Chc 0 (Chc 1 (Plain 0 0) (NoPos)) (NoPos) ) (NoPos)

-}

-- A block represents the position of a Str
-- or a Chc in VString. The offset value represents the position of the match
-- in that block.
-- Each block is basically a node in the choice tree and therefore
-- makes it faster to search/project/highlight the match in the
-- original search expression.

type Block  = Int
type Offset = Int

data Pos = P Block (Either Offset (Pos,Pos))
         | NoPos

{-data Pos = Plain Block Offset
         | ChcP Block Pos Pos
         | NoPos
-}         

-- A query can contain many dimension variables and therefore each match
-- can have a list of variable bindings.
-- A dimension varible can bind to a single dimension or a set of dimension.
-- In case of d<P1,P2>, "d" binds to a single dimesion whereas in case of
-- d+<P1,P2> where we match even if the change occured in multiple dimensions,
-- "d" will be bound to a list of dimensions all of which contributed to the 
-- change.

type DimEnv = [(String,[Dim])]

-- Query variables are used to extract parts of matches.
-- Example
-- Consider the pattern = d<a,$x> 
-- Here $x refers to the value in the right alternative.
-- These variables also have the type as VMatch and therefore can be reused 
-- like any other query result.
-- d<a,$x> # A<a,b>> --> (pos, d = A, $x = (pos',empty, empty, b), A<a,b>)
 

data Pattern =  QVar String
          -- | other constructors

-- When matching, these variable binding are encoded in the output. 
-- These variables will again be bound to values of type VMatch itself.
-- Refer to the example above

type VarEnv = (String, VMatch)

-- TODO write down queries for the example queries

--Example 1: Find all the commits that affected a method foo

-- Pattern: d<*foo*,$x>
-- $x will refer to the variational string in the right alternative.
-- Using this value, one could simply display, count or perform another
-- query

-- To count : count $x from vgrep d<*foo*,$x> vs  :: Int
-- To Show  : pretty $x from vgrep d<*foo*,$x> vs    :: String
-- Query    : vgrep pat $x from vgrep d<*foo*,$x> vs :: VMatch

--All of the above functions map their respective functionality to each match
-- in the list of matches returned by vgrep


-- Exmaple 2: Were all the occurrences of the method "foo" renamed
-- to "bar"

-- filter ("bar" `notIn` $x) from vgrep d<foo,$x> vs 
-- (notIn :: Pattern -> VString -> Bool)
-- (in :: Pattern -> VString -> Bool)

-- Example 3: Find names that have been renamed differently in different 
-- branches

-- filter ($x /= $y) from vgrep d<d'<foo,$x>,$y>





