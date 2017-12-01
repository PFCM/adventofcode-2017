import Data.Char


circularSqueeze :: Eq a => [a] -> [a]
circularSqueeze [] = []
circularSqueeze [x] = []
circularSqueeze list =
  let
    front = head list
    squeeze (x:y:xs)
      | x == y = x : squeeze (y:xs)
      | otherwise = squeeze $ y:xs
    squeeze [x] = [x | x == front]
    squeeze _ = []
    in squeeze list


solveCaptcha :: [Int] -> Int
solveCaptcha = sum . circularSqueeze


main = do
  inputs <- getLine
  print . solveCaptcha . map digitToInt $ inputs
