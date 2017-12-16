import Text.Parsec
import Text.Parsec.String (Parser)
import TwelfthLib

lineParser :: Parser [Int]
lineParser = do
  skipMany1 digit
  space
  string "<->"
  space
  nums <- sepBy (many1 digit) (string ", ")
  return (map read nums)

-- assumes ascending order, seems to hold for the provided input
parser :: Parser [[Int]]
parser = sepEndBy1 lineParser newline

withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

main =
  withStdInput parser >>= \parsed -> do
    putStrLn "Part 1"
    let graph = fromLists parsed
    print . length . dfs 0 $ graph
    putStrLn "Part 2"
    print . length . connectedComponents $ graph
