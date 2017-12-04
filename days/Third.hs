import qualified Data.Map   as M
import           Data.Maybe

type Pos = (Int, Int)

-- number of steps along a spiral to coordinates
-- see https://math.stackexchange.com/questions/163080/on-a-two-dimensional-grid-is-there-a-formula-i-can-use-to-spiral-coordinates-in
-- to get started
spiralToEuclidean :: Int -> Pos
spiralToEuclidean steps =
  let
    n = steps - 1
    m = floor . sqrt . fromIntegral $ n
    k | odd m = (m - 1) `div` 2
      | n >= m * (m + 1) = m `div` 2
      | otherwise = (m `div` 2) - 1
    coords
      | (2*k*(2*k+1) <= n) && (n <= (2*k+1)*(2*k+1)) = (n - 4*k*k - 3*k, k)
      | ((2*k+1)*(2*k+1) < n) && (n <= 2*(k+1)*(2*k+1)) = (k+1, 4*k*k+5*k+1-n)
      | (2*(k+1)*(2*k+1) < n) && (n <= 4*(k+1)*(k+1)) = (4*k*k+7*k+3-n, -k-1)
      | n == 0 = (0, 0)
      | (4*k+1)*(4*k+1) < n && n <= 2*(k+1)*(2*k+3) = (-k-1, n-4*k*k-9*k-5)
      | otherwise = (0, 0)
  in
    coords

spiralToEuclidean' :: Int -> Pos
spiralToEuclidean' number = (x, y)
    where
        layer                = floor $ (sqrt (fromIntegral number - 1) + 1) / 2
        dimension            = layer * 2 + 1
        layerFirst           = (dimension - 2) ^ 2 + 1
        layerPosition        = number - layerFirst
        sideLength           = dimension - 1
        sideMidPoint         = sideLength `div` 2 - 1
        (side, sidePosition) = layerPosition `divMod` sideLength
        sideDistance         = sideMidPoint - sidePosition
        x = case side of 0 -> layer
                         1 -> sideDistance
                         2 -> -layer
                         3 -> -sideDistance
        y = case side of 0 -> -sideDistance
                         1 -> layer
                         2 -> sideDistance
                         3 -> -layer


manhattanDistance :: Num a => (a, a) -> a
manhattanDistance x =
  (abs . fst $ x) + (abs . snd $ x)

-- point in spiral coordinate to manahattan distance to origin
distanceToOrigin :: Int -> Int
distanceToOrigin = manhattanDistance . spiralToEuclidean


-- get neighbouring positions
neighbours :: Pos -> [Pos]
neighbours (x, y) =
  [ pos
  | dx <- [-1 .. 1]
  , dy <- [-1 .. 1]
  , let pos = (x + dx, y + dy)
  -- , pos /= (x, y)
  ]

values :: [Int]
values = rec M.empty [1..]
  where
    rec _ []            = []
    rec vals (1:rest)   = 1 : rec (M.insert (0, 0) 1 vals) rest
    rec vals (pos:rest) = let
      coords = spiralToEuclidean' pos
      val = sum . mapMaybe (`M.lookup` vals) . neighbours $ coords
      in val : rec (M.insert coords val vals) rest


getValue :: Int -> Int
getValue val = head . dropWhile (<= val) $ values


main = getLine >>= print . getValue . read
