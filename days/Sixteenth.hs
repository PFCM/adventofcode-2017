import Data.List (foldl')
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!), (//))
import Text.Parsec
import Text.Parsec.String (Parser)

data Move
  = Spin Int
  | Exchange Int
             Int
  | Partner Char
            Char
  deriving (Show)

type Positions = V.Vector Char

spin :: Positions -> Int -> Positions
spin pos amt = end V.++ start
  where
    (start, end) = V.splitAt (V.length pos - amt) pos

exchange :: Positions -> Int -> Int -> Positions
exchange pos a b = pos // [(a, p2), (b, p1)]
  where
    p1 = pos ! a
    p2 = pos ! b

partner :: Positions -> Char -> Char -> Positions
partner pos a b =
  let aPos = V.elemIndex a pos
      bPos = V.elemIndex b pos
  in case sequence [aPos, bPos] of
       Just [aP, bP] -> exchange pos aP bP
       _ -> pos

danceStep :: Positions -> Move -> Positions
danceStep p m =
  case m of
    Spin amt -> spin p amt
    Exchange a b -> exchange p a b
    Partner a b -> partner p a b

dance :: Positions -> [Move] -> Positions
dance = foldl' danceStep

initial :: Positions
initial = V.fromList "abcdefghijklmnop"

-- how many repetitions until we're back where we started
findPeriod :: [Move] -> Int
findPeriod moves =
  (+ 1) . length . takeWhile (/= initial) . drop 1 . scanl dance initial $
  repeat moves

moveParser :: Parser Move
moveParser = choice . map try $ [spinParser, exchangeParser, partnerParser]
  where
    spinParser = Spin <$> (char 's' *> num)
    exchangeParser = Exchange <$> (char 'x' *> num) <*> (char '/' *> num)
    partnerParser = Partner <$> (char 'p' *> lower) <*> (char '/' *> lower)
    num = read <$> many1 digit

parser :: Parser [Move]
parser = sepEndBy1 moveParser (char ',') <* optional newline

withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

main =
  withStdInput parser >>= \parsed -> do
    putStrLn "Part 1:"
    print $ dance initial parsed
    putStrLn "Part 2:"
    print $
      foldl'
        danceStep
        (V.fromList "abcdefghijklmnop")
        (concat . replicate (1000000000 `mod` findPeriod parsed) $ parsed)
