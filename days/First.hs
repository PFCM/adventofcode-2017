import Data.Char
import Data.Maybe

circularSqueeze :: Eq a => [a] -> [a]
circularSqueeze [] = []
circularSqueeze [x] = []
circularSqueeze list =
  let front = head list
      squeeze (x:y:xs)
        | x == y = x : squeeze (y : xs)
        | otherwise = squeeze $ y : xs
      squeeze [x] = [x | x == front]
      squeeze _ = []
  in squeeze list

cyclicShift :: Int -> [a] -> [a]
cyclicShift _ [] = []
cyclicShift k xs = zipWith const (drop k . cycle $ xs) xs

maybeEqual :: Eq a => a -> a -> Maybe a
maybeEqual x y =
  if x == y
    then Just x
    else Nothing

halfwaySqueeze :: Eq a => [a] -> [a]
halfwaySqueeze [] = []
halfwaySqueeze xs =
  catMaybes . zipWith maybeEqual xs $ cyclicShift (length xs `div` 2) xs

solveCaptcha :: [Int] -> Int
solveCaptcha = sum . circularSqueeze

solveCaptcha2 :: [Int] -> Int
solveCaptcha2 = sum . halfwaySqueeze

main = do
  inputs <- getLine
  print . solveCaptcha2 . map digitToInt $ inputs
