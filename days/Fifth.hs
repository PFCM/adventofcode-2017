import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text.Read (decimal, signed)
import qualified Data.Vector as V
import Data.Vector ((!?), (//))

countSteps :: [Int] -> Int
countSteps = length . steps1 . V.fromList

-- this doesn't feel real great
steps1 :: V.Vector Int -> [Int]
steps1 jumpTable = go jumpTable [0]
  where
    go _ [] = []
    go j (p:xs) =
      case j !? p of
        Just jump -> go (j // [(p, jump + 1)]) ((p + jump) : p : xs)
        Nothing -> xs

parse :: T.Text -> Either String [Int]
parse = fmap (map fst) . mapM (signed decimal) . T.lines

main = T.IO.interact (T.pack . show . fmap countSteps . parse)
