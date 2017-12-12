{-# LANGUAGE RecordWildCards #-}

import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text.Read (decimal)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!), (//))

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
parse2 = Right . map ord . T.unpack

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
  print (part1 <$> parse1 inputs)
