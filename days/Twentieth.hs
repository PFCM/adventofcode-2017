import qualified Data.Vector.Unboxed as V
import Text.Parsec
import Text.Parsec.String (Parser)

data Particle = Particle
  { pos :: V.Vector Int
  , vel :: V.Vector Int
  , acc :: V.Vector Int
  } deriving (Show)

particleParser :: Parser Particle
particleParser =
  Particle <$> (vectorParser "p" <* string ", ") <*>
  (vectorParser "v" <* string ", ") <*>
  vectorParser "a"

numParser :: Parser Int
numParser = read <$> (neg <|> pos)
  where
    neg = (:) <$> char '-' <*> pos
    pos = many1 digit

vectorParser :: String -> Parser (V.Vector Int)
vectorParser key = do
  string key
  char '='
  vals <- between (char '<') (char '>') $ sepBy1 numParser (string ",")
  return $ V.fromList vals

parser :: Parser [Particle]
parser = sepEndBy particleParser newline

withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

main =
  withStdInput parser >>= \parsed -> do
    putStrLn "Part 1"
    print parsed
