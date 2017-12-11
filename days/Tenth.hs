import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text.Read (decimal)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!), (//))

part1 :: V.Vector Int -> Int
part1 = V.product . V.slice 0 2

parse :: T.Text -> Either String [Int]
parse = mapM (fmap fst . decimal) . T.split (== ',')

flipSlice :: V.Unbox a => Int -> Int -> V.Vector a -> V.Vector a
flipSlice start len vec = vec // update
  where
    update = zip indices values
    size = V.length vec
    indices = take len . drop start . cycle $ [0 .. size - 1]
    values = reverse . map (vec !) $ indices

runSwaps :: Int -> [Int] -> V.Vector Int
runSwaps size steps = go (V.generate size id) steps 0 0
  where
    go vec [] _ _ = vec
    go vec (l:ls) skip pos =
      go (flipSlice pos l vec) ls (skip + 1) ((pos + l + skip) `mod` size)

main = do
  inputs <- T.IO.getContents
  print (part1 . runSwaps 256 <$> parse inputs)
