import Data.Functor
import Data.Maybe (catMaybes, mapMaybe)
import Data.Tree as T
import Text.Parsec
import Text.Parsec.String (Parser)

data GroupInfo
  = Group Int
  | Garbage Int

-- parse the input into some kind of depth tagged tree structure
-- shorthand for the wrapping
delimited :: Char -> Char -> Parser a -> Parser a
delimited start end = between (char start) (char end)

garbageAccum :: Char -> String -> String
garbageAccum '!' = id
garbageAccum c = (:) c

-- in between < and > but anything after a ! is ignored
garbageParser :: Parser (T.Tree GroupInfo)
garbageParser = do
  char '<'
  gs <- garbage
  char '>'
  return . T.Node (Garbage (length gs)) $ []
  where
    garbage = manyAccum garbageAccum garbageChar
    garbageChar = choice . map try $ [char '!' <* anyChar, noneOf ">"]

childrenParser :: Int -> Parser (T.Forest GroupInfo)
childrenParser d = (try . commaSep $ group) <|> return []
  where
    group = try garbageParser <|> groupParser d

groupParser :: Int -> Parser (T.Tree GroupInfo)
groupParser d = do
  children <- delimited '{' '}' (childrenParser (d + 1))
  return $ T.Node (Group d) children

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy (char ',')

parser :: Parser (T.Forest GroupInfo)
parser = (commaSep . groupParser $ 1) <* optional newline

-- parse all of stdin
withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

unwrapGroups :: GroupInfo -> Maybe Int
unwrapGroups (Group depth) = Just depth
unwrapGroups _ = Nothing

unwrapGarbage :: GroupInfo -> Maybe Int
unwrapGarbage (Garbage len) = Just len
unwrapGarbage _ = Nothing

main =
  withStdInput parser >>= \parsed -> do
    putStrLn "Part 1"
    print . sum . mapMaybe unwrapGroups . concatMap T.flatten $ parsed
    putStrLn "Part 2"
    print . sum . mapMaybe unwrapGarbage . concatMap T.flatten $ parsed
