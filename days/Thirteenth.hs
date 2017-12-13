import Data.Vector ((!), (//))
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String (Parser)

-- part 1, build an appropriate representation and see what happens
advanceState :: (Int, Int) -> (Int, Int)
advanceState (pos, range) = ((pos + 1) `mod` range, range)

runSimplePath :: V.Vector (Maybe (Int, Int)) -> Int
runSimplePath fwall = fst . foldr step (0, fwall) $ [0 .. length fwall - 1]
  where
    stateStep = V.map (fmap advanceState)
    step pos (cost, state) =
      case state ! pos of
        Just (fpos, range) ->
          let newState = stateStep state
              newCost =
                if fpos == 0
                  then cost + pos * range
                  else cost
          in (newCost, newState)
        Nothing -> (cost, state)

fromList :: [(Int, Int)] -> V.Vector (Maybe (Int, Int))
fromList vals = vec // initial
  where
    (depth, range) = unzip vals
    vec = V.replicate (maximum depth + 1) Nothing
    initial = zip depth . map (\r -> Just (0, r)) $ range

parser :: Parser (V.Vector (Maybe (Int, Int)))
parser = fromList <$> sepEndBy1 lineParser newline
  where
    lineParser = (,) <$> (num <* (skipMany1 . string $ ": ")) <*> num
    num = read <$> many1 digit

withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

main =
  withStdInput parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ runSimplePath parsed
    putStrLn "Part 2"
