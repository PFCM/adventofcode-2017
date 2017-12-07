import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

-- let's actually build the tree as a data structure -- seems like a good
-- exercise and chances are part 2 will do something more complex
-- (I am suspicious of this `weight`)
data ProgramTree = ProgramTree
  { _name :: String
  , _weight :: Int
  , _children :: [ProgramTree]
  } deriving (Show)

type Intermediate = ([ProgramNodeInput], M.Map String ProgramTree)

convertNode :: ProgramNodeInput -> [ProgramTree] -> ProgramTree
convertNode node = ProgramTree (name node) (weight node)

multiMaybeLookup :: Ord k => M.Map k a -> [k] -> Maybe [a]
multiMaybeLookup dict = mapM (`M.lookup` dict)

maybeInsert :: Intermediate -> ProgramNodeInput -> Intermediate
maybeInsert (xs, seen) node =
  case multiMaybeLookup seen . children $ node of
    Just kids -> (xs, newSeen)
      where treeNode = convertNode node kids
            newSeen = M.insert (name node) treeNode seen
    Nothing -> (node : xs, seen)

buildTree :: [ProgramNodeInput] -> ProgramTree
buildTree inputs = go inputs M.empty
  where
    getChildren nodes = concat . maybeToList . multiMaybeLookup nodes . children
    -- TODO this is broken because it could be that the root node is the last
    -- one of the final intermediate list after all the other levels have been
    -- filtered out. In this case `leftovers` will be 0 and we're recursing forever
    go [x] nodes = convertNode x . getChildren nodes $ x
    go inputs seen = go (traceShow (length leftovers) leftovers) newSeen
      where
        (leftovers, newSeen) = foldl maybeInsert ([], seen) inputs

part1 :: [ProgramNodeInput] -> String
part1 = show . _name . buildTree

-- INPUT, it seemed like a good idea to learn about parsec...
data ProgramNodeInput = ProgramNodeInput
  { name :: String
  , weight :: Int
  , children :: [String]
  } deriving (Show)

childParser :: Parser [String]
childParser = do
  space
  string "->"
  space
  sepBy1 (many1 lower) (string ", ")

nodeParser :: Parser ProgramNodeInput
nodeParser = ProgramNodeInput <$> nameParser <*> weightParser <*> childrenParser
  where
    nameParser = many1 lower <* space
    weightParser = read <$> between (char '(') (char ')') (many1 digit)
    childrenParser = option [] (try childParser) <* newline

inputParser :: Parser [ProgramNodeInput]
inputParser = many1 nodeParser

parseStdInput :: Parser a -> IO a
parseStdInput p = do
  inputs <- getContents
  let result = parse (p <* eof) "<input>" inputs
  either (error . show) return result

main = do
  inputs <- parseStdInput inputParser
  putStrLn "Part 1"
  print $ part1 inputs
