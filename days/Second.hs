import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read
import Data.Word

-- input
parseInput :: T.Text -> Either String [[(Int, T.Text)]]
parseInput = mapM (mapM decimal . T.words) . T.lines . T.strip

processInput :: T.Text -> [[Int]]
processInput inputs =
  case parseInput inputs of
    Right results -> map (map fst) results
    Left _ -> [[]]

-- Part 1
range :: (Num a, Ord a) => [a] -> a
range nums = maximum nums - minimum nums

checkSum :: [[Int]] -> Int
checkSum = sum . map range

-- Part 2
divisors :: [Int] -> [Int]
divisors inputs =
  let getDivs = map (\a -> filter (\b -> (a `mod` b) == 0) inputs)
      keepDivs = filter ((> 1) . length) . getDivs
      performDiv items = maximum items `div` minimum items
  in map performDiv . keepDivs $ inputs

sumDiv :: [[Int]] -> Int
sumDiv = sum . map (head . divisors)

main = do
  contents <- TIO.getContents
  putStr . show . sumDiv . processInput $ contents
