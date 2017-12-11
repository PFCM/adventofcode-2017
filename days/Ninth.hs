import Data.Functor
import Data.Maybe (catMaybes)
import Data.Tree as T
import Text.Parsec
import Text.Parsec.String (Parser)

type GroupInfo = Int

-- parse the input into some kind of depth tagged tree structure
-- shorthand for the wrapping
delimited :: Char -> Char -> Parser a -> Parser a
delimited start end = between (char start) (char end)

-- in between < and > but anything after a ! is ignored
garbageParser :: Parser (Maybe (T.Tree GroupInfo))
garbageParser = char '<' *> garbage *> char '>' $> Nothing
  where
    garbage = skipMany garbageChar
    garbageChar = choice . map try $ [char '!' *> anyChar, noneOf ">"]

childrenParser :: Int -> Parser (T.Forest GroupInfo)
childrenParser d = (catMaybes <$> (try . commaSep $ group)) <|> return []
  where
    group = try garbageParser <|> groupParser d

groupParser :: Int -> Parser (Maybe (T.Tree GroupInfo))
groupParser d = do
  children <- delimited '{' '}' (childrenParser (d + 1))
  return . Just $ T.Node d children

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy (char ',')

parser :: Parser (T.Forest GroupInfo)
parser = catMaybes <$> (commaSep . groupParser $ 1) <* optional newline

-- parse all of stdin
withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

main =
  withStdInput parser >>= \parsed -> do
    putStrLn "Part 1"
    print . sum . concatMap T.flatten $ parsed
