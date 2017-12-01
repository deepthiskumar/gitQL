module MetaFileReader where


import Data.Text.IO as I hiding (putStrLn)
import Data.Text as T
import Data.Map as M
import Types
import Data.DateTime
import Control.Exception as E
import Data.List as L
import Data.Maybe
import Debug.Trace

type MetaFile = Map Dim MetaData

data MetaData = MetaData{
                 commitId :: Text,
                 commitDate :: DateTime,
                 author :: Text,
                 viewDecision :: Selection}
     deriving(Show)


readMetafile :: FilePath -> IO MetaFile
readMetafile f = do
   content <- readSFile (f++".m")
   let m = parseContents (L.tail $ T.lines content) M.empty
   return m

--"COMMIT ID:DIM|DATE|AUTHOR|VIEW DECISION\n"
   
parseContents :: [Text] -> MetaFile -> MetaFile
parseContents [] m = m
parseContents (l:ls) m 
  | T.null l       = parseContents ls m
  | otherwise      = let (d,meta) = metaData l
                     in M.insert d meta (parseContents ls m)
   
metaData :: Text -> (Dim, MetaData)
metaData line = case T.splitOn pipe line of
       [id,dim,d,author,vd] -> {-trace (show [id,dim,d,author,vd])-} (readDim dim, MetaData id (readDate d ) author (readVD vd)) 
       x                 -> trace (show x) undefined   
 
readDim :: Text -> Dim
readDim t = {-trace (show t)-} (read (T.unpack t) ::Dim)

readDate :: Text -> DateTime
readDate t = fromJust $ parseDateTime dateFormat (T.unpack (L.head (T.splitOn space t)))

dateFormat = "%F" --same as %Y-%m-%d

readVD :: Text -> Selection
readVD t = [ splitSel val | val <- splitVD t, not $ T.null val ]  

splitVD :: Text -> [Text]
splitVD = (T.splitOn comma).(fromJust).(T.stripPrefix openB).(fromJust).(T.stripSuffix closeB)

splitSel :: Text -> Sel
splitSel x = getSel $ T.splitOn dot x 

getSel :: [Text] -> Sel
getSel [d,s] 
  | s == r = RSel (readDim d)
  | s == l = LSel (readDim d)

space = T.pack " "

r = T.pack "r"
l= T.pack "l"

dot = T.pack "."

comma = T.pack ","

openB = T.pack "["

closeB = T.pack "]"
 
colon = T.pack ":"

pipe = T.pack "|" 
   
readSFile :: FilePath -> IO Text
readSFile f = E.catch (I.readFile f) readHandler >>= return


readHandler :: IOError -> IO Text
readHandler e = do 
  putStrLn ("Error occurred during read operation: " ++ show e)   
  return $ T.empty
