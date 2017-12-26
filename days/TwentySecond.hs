{-# LANGUAGE RecordWildCards #-}

import Data.List (foldl')
import qualified Data.Set as S
import Debug.Trace

type Position = (Int, Int)

data Direction
  = N
  | E
  | S
  | W

data Infector = Infector
  { pos :: Position
  , dir :: Direction
  , count :: Int
  }

right :: Direction -> Direction
right N = E
right E = S
right S = W
right W = N

left :: Direction -> Direction
left = right . right . right

act :: S.Set Position -> Infector -> (S.Set Position, Infector)
act infd Infector {..} =
  if S.member pos infd
    then (S.delete pos infd, Infector (step (right dir) pos) (right dir) count)
    else ( S.insert pos infd
         , Infector (step (left dir) pos) (left dir) (count + 1))

step :: Direction -> Position -> Position
step N (x, y) = (x, y - 1)
step E (x, y) = (x + 1, y)
step S (x, y) = (x, y + 1)
step W (x, y) = (x - 1, y)

parse :: String -> S.Set Position
parse inputs =
  S.fromList . concatMap parseLine . zip [-size ..] . lines $ inputs
  where
    size = (length . lines $ inputs) `div` 2

parseLine :: (Int, String) -> [Position]
parseLine (y, line) =
  map (\(x, _) -> (x, y)) . filter ((== '#') . snd) . zip [-size ..] $ line
  where
    size = length line `div` 2

main = do
  inputs <- getContents
  let initialInfection = parse inputs
      infector = Infector (0, 0) N 0
      results = iterate (uncurry act) (initialInfection, infector)
  putStrLn "Part 1"
  print $ count . snd $ results !! 10000
