{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map as M

data State
  = A
  | B
  | C
  | D
  | E
  | F
  deriving (Show, Eq, Ord)

data TapeValue
  = Zero
  | One
  deriving (Show, Eq, Ord)

data Tape = Tape
  { _left :: [TapeValue]
  , _right :: [TapeValue]
  , _current :: TapeValue
  }

data Move
  = L
  | R

data Action = Action
  { _write :: TapeValue
  , _move :: Move
  , _newState :: State
  }

data ExecutionState = ExecutionState
  { _tape :: Tape
  , _state :: State
  }

type Rules = M.Map (State, TapeValue) Action

-- hardcode the rules because they aren't complex but the input is
rules :: Rules
rules =
  M.fromList
    [ ((A, Zero), Action One R B)
    , ((A, One), Action Zero L B)
    , ((B, Zero), Action Zero R C)
    , ((B, One), Action One L B)
    , ((C, Zero), Action One R D)
    , ((C, One), Action Zero L A)
    , ((D, Zero), Action One L E)
    , ((D, One), Action One L F)
    , ((E, Zero), Action One L A)
    , ((E, One), Action Zero L D)
    , ((F, Zero), Action One R A)
    , ((F, One), Action One L E)
    ]

-- deal with the tape
makeTape :: Tape
makeTape = Tape [] [] Zero

move :: Move -> Tape -> Tape
move L = moveLeft
move R = moveRight

moveLeft :: Tape -> Tape
moveLeft tape@Tape {..} = go _left _right _current
  where
    go [] r c = Tape [] (c : r) Zero
    go (x:l) r c = Tape l (c : r) x

moveRight :: Tape -> Tape
moveRight tape@Tape {..} = go _left _right _current
  where
    go l [] c = Tape (c : l) [] Zero
    go l (x:r) c = Tape (c : l) r x

-- actually run the thing
step :: Rules -> ExecutionState -> ExecutionState
step rs state@ExecutionState {..} =
  let val = _current _tape
      Action {_write = w, _move = m, _newState = ns} = rs M.! (_state, val)
  in ExecutionState (move m (_tape {_current = w})) ns

runs :: [ExecutionState]
runs = iterate (step rules) $ ExecutionState makeTape A

checksum :: ExecutionState -> Int
checksum ExecutionState {_tape = tape} =
  (checksum' . _left $ tape) + (checksum' . _right $ tape)
  where
    checksum' = sum . map convert
    convert One = 1
    convert Zero = 0

steps :: Int
-- steps = 6
steps = 12629077

main = do
  putStrLn "Part 1"
  print . checksum $ runs !! steps
