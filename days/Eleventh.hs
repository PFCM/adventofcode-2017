import Data.Functor (($>))
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

type Hex = (Int, Int)

data Direction
  = North
  | NorthEast
  | SouthEast
  | South
  | SouthWest
  | NorthWest
  deriving (Show)

-- general stuff for running around an implicit hex grid
-- NOTE this is the main one that depends on 2D hex coords
-- like we're on a square grid that got kicked over
step :: Direction -> Hex -> Hex
step dir (x, y) =
  case dir -- highly unintuitive magic numbers
        of
    North -> (x - 1, y + 1)
    NorthEast -> (x, y + 1)
    SouthEast -> (x + 1, y)
    South -> (x + 1, y - 1)
    SouthWest -> (x, y - 1)
    NorthWest -> (x - 1, y)

takeSteps :: Hex -> [Direction] -> Hex
takeSteps = foldr step

-- part 1, run a bunch of steps and find the number of steps away
-- it ends up ie. the hex manhattan distance
hexManhattanDistance :: Hex -> Hex -> Int
hexManhattanDistance (x0, y0) (x1, y1) =
  if signum dx == signum dy
    then abs (dx + dy)
    else max (abs dx) (abs dy)
  where
    dx = x1 - x0
    dy = y1 - y0

-- part two we just need to get all the positions
-- there's a lot but we're going to fold over them so it should
-- be fine
allSteps :: Hex -> [Direction] -> [Hex]
allSteps = scanl (flip step)

maxDistance :: Hex -> [Direction] -> Int
maxDistance start = maximum . map (hexManhattanDistance start) . allSteps start

-- input stuff
directionParsers :: [Parser Direction]
directionParsers =
  [ string "se" $> SouthEast
  , string "sw" $> SouthWest
  , string "s" $> South
  , string "ne" $> NorthEast
  , string "nw" $> NorthWest
  , string "n" $> North
  ]

parser :: Parser [Direction]
parser = sepBy1 (choice $ try <$> directionParsers) (char ',') <* newline

withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

main =
  withStdInput parser >>= \parsed -> do
    putStrLn "Part 1"
    let start = (0, 0)
        end = takeSteps start parsed
    print $ hexManhattanDistance start end
    putStrLn "Part 2"
    print $ maxDistance start parsed
