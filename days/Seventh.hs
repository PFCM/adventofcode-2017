import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Monoid
import qualified Data.Set as S
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
    go [] nodes = findRoot nodes -- edge case, hopefully not often :(
    go [x] nodes = convertNode x . getChildren nodes $ x
    go inputs seen = go leftovers newSeen
      where
        (leftovers, newSeen) = foldl maybeInsert ([], seen) inputs

findRoot :: M.Map String ProgramTree -> ProgramTree
findRoot progMap = root
  where
    root = head remains
      where
        remains =
          filter ((`S.notMember` allChildrenNames) . _name) . map snd . M.toList $
          progMap
    allChildrenNames =
      foldl
        (\a b -> S.union a . S.fromList . map _name . _children $ b)
        S.empty
        progMap

part1 :: ProgramTree -> String
part1 = show . _name

-- part two -- find a child whose weight doesn't match its siblings and return
-- what it should be
part2 :: ProgramTree -> String
part2 = show . unMatchedWeight

-- seems like this may recompute a lot of the sums
unMatchedWeight :: ProgramTree -> Int
unMatchedWeight node =
  let sums = map treeSum . _children $ node
  in head sums

treeSum :: ProgramTree -> Int
treeSum node
  | null . _children $ node = _weight node
  | otherwise = getSum . foldMap (Sum . treeSum) . _children $ node

-- INPUT, it seemed like a good idea to learn about parsec...
data ProgramNodeInput = ProgramNodeInput
  { name :: String
  , weight :: Int
  , children :: [String]
  } deriving (Show)

childParser :: Parser [String]
childParser = do
  string " -> "
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
  let tree = buildTree inputs
  putStrLn "Part 1"
  print $ part1 tree
  putStrLn "Part 2"
  print $ part2 tree
