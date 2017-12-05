import Data.Array
import Data.Ix (inRange)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text.Read (decimal, signed)

countSteps :: [Int] -> Int
countSteps vals = length . steps . listArray (0, length vals - 1) $ vals

steps :: Array Int Int -> [Int]
steps jumps = go jumps [0]
  where
    go _ [] = []
    go j (p:xs)
      | inRange (bounds j) p = go (j // [(p, j ! p + 1)]) ((p + j ! p) : p : xs)
      | otherwise = xs

parse :: T.Text -> Either String [Int]
parse = fmap (map fst) . mapM (signed decimal) . T.lines

main = T.IO.interact (T.pack . show . fmap countSteps . parse)
