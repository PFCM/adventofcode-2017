{-# LANGUAGE RecordWildCards #-}

import Data.List (foldl')
import qualified Data.Set as S

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
left N = W
left W = S
left S = E
left E = N

act :: S.Set Position -> Infector -> (S.Set Position, Infector)
act infd Infector {..} =
  if S.member pos infd
    then (S.delete pos infd, Infector (step (right dir) pos) (right dir) count)
    else ( S.insert pos infd
         , Infector (step (left dir) pos) (left dir) (count + 1))

step :: Direction -> Position -> Position
step N (x, y) = (x, y + 1)
step E (x, y) = (x + 1, y)
step S (x, y) = (x, y - 1)
step W (x, y) = (x - 1, y)

parse :: String -> S.Set Position
parse inputs = snd . foldl' parseLine (initialPos, S.empty) . lines $ inputs
  where
    initialPos = (size, size)
    size = -(length . head . lines $ inputs) `div` 2

parseLine :: (Position, S.Set Position) -> String -> (Position, S.Set Position)
parseLine ((x, y), infected) = reset . foldl' fstep ((x, y), infected)
  where
    reset (_, infected) = ((x, y + 1), infected)
    fstep (pos, infected) '.' = (step E pos, infected)
    fstep (pos, infected) '#' = (newPos, S.insert pos infected)
      where
        newPos = step E pos

main = do
  inputs <- getContents
  let initialInfection = parse inputs
      infector = Infector (0, 0) N 0
      results = iterate (uncurry act) (initialInfection, infector)
  putStrLn "Part 1"
  print $ count . snd $ results !! 70
