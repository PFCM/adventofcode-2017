{-# LANGUAGE RecordWildCards #-}

import Data.Char (isAlpha)
import Data.List
import Data.Maybe (fromJust)

type Position = (Int, Int)

type Grid = [String]

data Direction
  = N
  | S
  | E
  | W
  deriving (Eq, Show)

data State = State
  { pos :: Position
  , dir :: Direction
  , letters :: String
  , steps :: Int
  } deriving (Eq)

step :: Position -> Direction -> Position
step (x, y) N = (x, y - 1)
step (x, y) S = (x, y + 1)
step (x, y) E = (x + 1, y)
step (x, y) W = (x - 1, y)

get :: Grid -> Position -> Char
get g (x, y)
  | x < 0 || y < 0 = ' '
  | x >= (length . head $ g) || y >= length g = ' '
  | otherwise = g !! y !! x

opposite :: Direction -> Direction
opposite N = S
opposite S = N
opposite E = W
opposite W = E

available :: Grid -> State -> Direction -> Bool
available grid state direction
  | (opposite . dir $ state) == direction = False
  | otherwise =
    case get grid . (`step` direction) . pos $ state of
      ' ' -> False
      _ -> True

options :: Grid -> State -> [Direction]
options grid state = filter (available grid state) [N, E, S, W]

advance :: Grid -> State -> State
advance grid state =
  case options grid state of
    [] -> state
    choices -> takeStep grid choices state

takeStep :: Grid -> [Direction] -> State -> State
takeStep g ds state@State {..}
  | dir `elem` ds = update g dir state
  | otherwise = update g (head ds) state

update :: Grid -> Direction -> State -> State
update g d s@State {..} =
  let newPos = step pos d
      newVal = get g newPos
      newLetters =
        if isAlpha newVal
          then newVal : letters
          else letters
  in State newPos d newLetters (steps + 1)

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

main = do
  inputs <- getContents
  let grid = lines inputs
      final =
        converge (advance grid) $
        State (fromJust . elemIndex '|' $ head grid, 0) S "" 1
  putStrLn "Part 1"
  print . reverse . letters $ final
  putStrLn "Part 2"
  print . steps $ final
