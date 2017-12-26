{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)

type Registers = M.Map Char Int

data ProcessorState = ProcessorState
  { regs :: Registers
  , pos :: Int
  , muls :: Int
  }

data Value
  = Literal Int
  | Register Char
  deriving (Show)

data Instruction
  = Set Char
        Value
  | Sub Char
        Value
  | Mul Char
        Value
  | Jnz Value
        Value
  deriving (Show)

getValue :: Value -> Registers -> Int
getValue (Literal val) _ = val
getValue (Register name) regs = fromMaybe 0 . M.lookup name $ regs

execute :: ProcessorState -> Instruction -> ProcessorState
execute state@ProcessorState {..} (Set name val) =
  state {regs = M.insert name (getValue val regs) regs, pos = pos + 1}
execute state@ProcessorState {..} (Sub name val) =
  state
  {regs = M.alter (Just . maybe (-v) (subtract v)) name regs, pos = pos + 1}
  where
    v = getValue val regs
execute state@ProcessorState {..} (Mul name val) =
  state
  { regs = M.alter (Just . maybe 0 (* v)) name regs
  , pos = pos + 1
  , muls = muls + 1
  }
  where
    v = getValue val regs
execute state@ProcessorState {..} (Jnz test offset) = state {pos = pos + jump}
  where
    jump =
      if testVal /= 0
        then offsetVal
        else 1
    testVal = getValue test regs
    offsetVal = getValue offset regs

step :: [Instruction] -> ProcessorState -> ProcessorState
step insts state = execute state (insts !! pos state)

programResults :: [Instruction] -> ProcessorState -> [ProcessorState]
programResults insts = iterate (step insts)

valid :: Int -> ProcessorState -> Bool
valid num state = not (pos state >= num || pos state < 0)

finalState :: [Instruction] -> ProcessorState -> ProcessorState
finalState insts =
  head . dropWhile (valid (length insts)) . programResults insts

numParser :: Parser Int
numParser = read <$> (neg <|> pos)
  where
    neg = (:) <$> char '-' <*> pos
    pos = many1 digit

valueParser :: Parser Value
valueParser = try (Literal <$> numParser) <|> (Register <$> lower)

binaryOpParser :: (Char -> Value -> Instruction) -> String -> Parser Instruction
binaryOpParser ctor pat =
  ctor <$> (string pat *> lower) <*> (space *> valueParser)

instructionParser :: Parser Instruction
instructionParser =
  choice . map try $
  [ binaryOpParser Set "set "
  , binaryOpParser Sub "sub "
  , binaryOpParser Mul "mul "
  , Jnz <$> (string "jnz " *> valueParser) <*> (space *> valueParser)
  ]

parser :: Parser [Instruction]
parser = sepEndBy1 instructionParser newline

withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

main =
  withStdInput parser >>= \instructions -> do
    let finalState1 = finalState instructions $ ProcessorState M.empty 0 0
        finalState2 =
          finalState instructions $ ProcessorState (M.fromList [('a', 1)]) 0 0
    putStrLn "Part 1"
    print $ muls finalState1
    putStrLn "Part 2"
    print $ regs finalState2 M.! 'h'
