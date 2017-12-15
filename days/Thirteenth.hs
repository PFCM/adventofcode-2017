import Data.Vector ((!), (//))
import qualified Data.Vector as V
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

-- part 1, build an appropriate representation and see what happens
advanceState :: (Int, Int) -> (Int, Int)
advanceState (pos, range) = (pos + 1, range)

-- mod would give us a saw, we want to flip the second part down
triangle :: Int -> Int -> Int
triangle y range =
  let range' = range - 1
      saw = y `mod` (2 * range')
  in if saw >= range'
       then range - (saw `mod` range')
       else saw

actualPos :: V.Vector (Maybe (Int, Int)) -> V.Vector (Maybe (Int, Int))
actualPos = V.map (fmap (\(a, r) -> (triangle a r, r)))

runSimplePath :: V.Vector (Maybe (Int, Int)) -> Int
runSimplePath fwall = fst . foldl step (0, fwall) $ [0 .. length fwall - 1]
  where
    stateStep = fmap $ fmap advanceState
    step (cost, state) pos =
      case state ! pos of
        Just (fpos, range) ->
          let newState = stateStep state
              newCost =
                if triangle fpos range == 0
                  then cost + pos * range
                  else cost
          in (newCost, newState)
        Nothing -> (cost, stateStep state)

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
