{-# LANGUAGE RecordWildCards #-}

import Data.Bits (xor)
import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text.Read (decimal)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!), (//))
import Numeric (showHex)

data Knots = Knots
  { tMarks :: V.Vector Int
  , tSkip :: Int
  , tPos :: Int
  }

initialKnots :: Int -> Knots
initialKnots size = Knots (V.generate size id) 0 0

-- part 1 is not so bad
parse1 :: T.Text -> Either String [Int]
parse1 = mapM (fmap fst . decimal) . T.split (== ',')

part1 :: [Int] -> Int
part1 = V.product . V.slice 0 2 . tMarks . runSwaps (initialKnots 256)

-- part 2 just adds a bunch of weird stuff
parse2 :: T.Text -> Either String [Int]
parse2 = Right . map ord . T.unpack . T.stripEnd

part2 :: [Int] -> String
part2 steps =
  let steps' = steps ++ [17, 31, 73, 47, 23]
      runMany 0 knots = knots
      runMany i knots = runMany (i - 1) . flip runSwaps steps' $ knots
      sparse = runMany 64 (initialKnots 256)
      dense = map (V.foldr1' xor) . vectorChunks 16 . tMarks $ sparse
      pad [c] = ['0', c]
      pad cs = cs
  in concatMap (pad . (`showHex` "")) dense

vectorChunks :: V.Unbox a => Int -> V.Vector a -> [V.Vector a]
vectorChunks = go 0
  where
    go pos size vec
      | pos < V.length vec = V.slice pos size vec : go (pos + size) size vec
      | otherwise = []

-- down here is the shared stuff
flipSlice :: V.Unbox a => Int -> Int -> V.Vector a -> V.Vector a
flipSlice start len vec = vec // update
  where
    update = zip indices values
    size = V.length vec
    indices = take len . drop start . cycle $ [0 .. size - 1]
    values = reverse . map (vec !) $ indices

runSwaps :: Knots -> [Int] -> Knots
runSwaps k [] = k
runSwaps Knots {..} (l:ls) = runSwaps newK ls
  where
    newK =
      Knots
        (flipSlice tPos l tMarks)
        (tSkip + 1)
        ((tPos + l + tSkip) `mod` V.length tMarks)

main = do
  inputs <- T.IO.getContents
  putStrLn "Part 1:"
  print (part1 <$> parse1 inputs)
  putStrLn "Part 2:"
  print (part2 <$> parse2 inputs)
