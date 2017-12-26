{-# LANGUAGE RecordWildCards #-}

import Data.List (foldl')
import qualified Data.Map.Strict as M

type Position = (Int, Int)

data Direction
  = N
  | E
  | S
  | W
  deriving (Show)

data Node
  = Weakened
  | Infected
  | Flagged
  deriving (Show)

data Infector = Infector
  { pos :: Position
  , dir :: Direction
  , count :: Int
  } deriving (Show)

right :: Direction -> Direction
right N = E
right E = S
right S = W
right W = N

left :: Direction -> Direction
left = right . right . right

act :: M.Map Position Node -> Infector -> (M.Map Position Node, Infector)
act infd Infector {..} =
  case M.lookup pos infd of
    Just Flagged ->
      ( M.delete pos infd
      , Infector (step (right . right $ dir) pos) (right . right $ dir) count)
    Just Infected ->
      ( M.insert pos Flagged infd
      , Infector (step (right dir) pos) (right dir) count)
    Just Weakened ->
      (M.insert pos Infected infd, Infector (step dir pos) dir (count + 1))
    Nothing ->
      ( M.insert pos Weakened infd
      , Infector (step (left dir) pos) (left dir) count)

step :: Direction -> Position -> Position
step N (x, y) = (x, y - 1)
step E (x, y) = (x + 1, y)
step S (x, y) = (x, y + 1)
step W (x, y) = (x - 1, y)

parse :: String -> M.Map Position Node
parse inputs =
  M.fromList . concatMap parseLine . zip [-size ..] . lines $ inputs
  where
    size = (length . lines $ inputs) `div` 2

parseLine :: (Int, String) -> [(Position, Node)]
parseLine (y, line) =
  map (\(x, _) -> ((x, y), Infected)) . filter ((== '#') . snd) . zip [-size ..] $
  line
  where
    size = length line `div` 2

main = do
  inputs <- getContents
  let initialInfection = parse inputs
      infector = Infector (0, 0) N 0
      results = iterate (uncurry act) (initialInfection, infector)
  putStrLn "Part 1"
  print $ count . snd $ results !! 10000000
