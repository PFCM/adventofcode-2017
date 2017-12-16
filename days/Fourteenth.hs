{-# LANGUAGE RecordWildCards #-}

import Data.Bits (xor)
import Data.Char (digitToInt, ord)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text.Read (decimal)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!), (//))
import Debug.Trace
import Numeric (readHex, showHex)
import Twelfth (Graph, connectedComponents)

data Knots = Knots
  { tMarks :: V.Vector Int
  , tSkip :: Int
  , tPos :: Int
  }

-- for part one we have to take a text, append some stuff and sum
-- the binary hashes
part1 :: T.Text -> Int
part1 = sum . map sum . makeGrid

makeGrid :: T.Text -> [[Int]]
makeGrid steps = map go [0 .. 127]
  where
    go num =
      map digitToInt . binaryHash . parseBytes $
      T.append (T.stripEnd steps) (T.pack $ "-" ++ show num)

binaryHash :: [Int] -> String
binaryHash = concatMap hexToBinary . hexHash

-- not super nice
hexToBinary :: Char -> String
hexToBinary char = binaryFourStrings !! (fst . head $ readHex [char])

-- probably a clever iterate to be more general
binaryFourStrings :: [String]
binaryFourStrings = map reverse $ ["0", "1"] >>= combine >>= combine >>= combine
  where
    combine x = ['0' : x, '1' : x]

initialKnots :: Int -> Knots
initialKnots size = Knots (V.generate size id) 0 0

parseBytes :: T.Text -> [Int]
parseBytes = map ord . T.unpack . T.stripEnd

hexHash :: [Int] -> String
hexHash steps =
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
  putStrLn "Part 1"
  print $ part1 inputs
