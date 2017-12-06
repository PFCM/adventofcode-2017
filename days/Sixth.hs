import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Debug.Trace

-- assumes positive values
argmax :: S.Seq Int -> (Int, Int)
argmax = S.foldlWithIndex keepmax (-1, -1)
  where
    keepmax (acc, pos) i val =
      if val > acc
        then (val, i)
        else (acc, pos)

-- have faith in GHC
spreadOver :: Int -> Int -> S.Seq Int -> S.Seq Int
spreadOver 0 _ banks = banks
spreadOver val pos banks =
  let newBanks = S.adjust (+ 1) pos banks
      newPos = (pos + 1) `mod` length banks
  in spreadOver (val - 1) newPos newBanks

redistribute :: S.Seq Int -> S.Seq Int
redistribute banks =
  let (maxVal, maxIndex) = argmax banks
      startPos = (maxIndex + 1) `mod` length banks
  in spreadOver maxVal startPos . S.update maxIndex 0 $ banks

reallocateCycles :: [Int] -> (Int, Int)
reallocateCycles banks = go 0 (S.fromList banks) M.empty
  where
    go i banks states
      | banks `M.member` states = (i, states M.! banks)
      | otherwise =
        let newBanks = redistribute banks
            newStates = M.insert banks i states
        in go (i + 1) newBanks newStates

part1 :: [Int] -> Int
part1 = fst . reallocateCycles

part2 :: [Int] -> Int
part2 banks = final - first
  where
    (final, first) = reallocateCycles banks

-- input handling etc
runAlgol :: ([Int] -> Int) -> String -> Int
runAlgol f = f . map read . words

main = do
  inputs <- getLine
  putStrLn ("Part 1: " ++ show (runAlgol part1 inputs))
  putStrLn ("Part 2: " ++ show (runAlgol part2 inputs))
