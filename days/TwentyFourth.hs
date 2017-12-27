import qualified Data.IntMap.Strict as I
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import Text.Parsec
import Text.Parsec.String (Parser)

type Part = (Int, Int)

type Options = I.IntMap [Part]

buildOptions :: [Part] -> Options
buildOptions = I.fromListWith (++) . concatMap options
  where
    options :: Part -> [(Int, [Part])]
    options part@(p1, p2) = [(p1, [part]), (p2, [part])]

starters :: [Part] -> [Part]
starters = filter (\(a, b) -> a == 0 || b == 0)

buildBridges :: Part -> Options -> [[Part]]
buildBridges start = go [start] [] S.empty
  where
    go [] paths _ _ = paths
    go (x:fringe) (p:paths) visited opts = undefined

parser :: Parser (V.Vector Part)
parser = V.fromList <$> sepEndBy1 partParser newline
  where
    partParser = (,) <$> (num <* char '/') <*> num
    num = read <$> many1 digit

withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

main =
  withStdInput parser >>= \parts -> do
    putStrLn "Part 1"
    print "it is np complete"
