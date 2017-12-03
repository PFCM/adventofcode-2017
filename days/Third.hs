
-- number of steps along a spiral to coordinates
-- see https://math.stackexchange.com/questions/163080/on-a-two-dimensional-grid-is-there-a-formula-i-can-use-to-spiral-coordinates-in
-- to get started
spiralToEuclidean :: Int -> (Int, Int)
spiralToEuclidean steps =
  let
    n = steps - 1
    m = floor . sqrt . fromIntegral $ n
    k | odd m = (m - 1) `div` 2
      | n >= m * (m + 1) = m `div` 2
      | otherwise = (m `div` 2) - 1
    coords
      | (2*k*(2*k+1) <= n) && (n <= (2*k+1)*(2*k+1)) = (n - 4*k*k -3*k, k)
      | ((2*k+1)*(2*k+1) < n) && (n <= 2*(k+1)*(2*k+1)) = (k+1, 4*k*k+5*k+1-n)
      | (2*(k+1)*(2*k+1) < n) && (n <= 4*(k+1)*(k+1)) = (4*k*k+7*k+3-n, -k-1)
      | n == 0 = (0, 0)
      | (4*k+1)*(4*k+1) < n && n <= 2*(k+1)*(2*k+3) = (-k-1, n-4*k*k-9*k-5)
      | otherwise = (0, 0)
  in
    coords

manhattanDistance :: Num a => (a, a) -> a
manhattanDistance x =
  (abs . fst $ x) + (abs . snd $ x)

-- point in spiral coordinate to manahattan distance to origin
distanceToOrigin :: Int -> Int
distanceToOrigin = manhattanDistance . spiralToEuclidean


main = getLine >>= print . distanceToOrigin . read
