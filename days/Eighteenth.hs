{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Parsec
import Text.Parsec.String (Parser)

data Val
  = Register Char
  | Value Int
  deriving (Show)

data Instruction
  = Snd Val
  | Set Char
        Val
  | Add Char
        Val
  | Mul Char
        Val
  | Mod Char
        Val
  | Rcv Val
  | Jgz Val
        Val

type Registers = M.Map Char Int

data ExecutionState = ExecutionState
  { registers :: Registers
  , position :: Int
  , instructions :: [Instruction]
  , frequency :: Int
  , recovered :: Bool
  }

getVal :: Val -> Registers -> Int
getVal (Register name) rs = fromMaybe 0 $ M.lookup name rs
getVal (Value val) _ = val

add :: Char -> Val -> Registers -> Registers
add name val rs = M.alter (Just . maybe v (+ v)) name rs
  where
    v = getVal val rs

mul :: Char -> Val -> Registers -> Registers
mul name val rs = M.alter (Just . maybe 0 (* v)) name rs
  where
    v = getVal val rs

modulo :: Char -> Val -> Registers -> Registers
modulo name val rs = M.alter (Just . maybe 0 (`mod` v)) name rs
  where
    v = getVal val rs

initialState :: [Instruction] -> ExecutionState
initialState insts = ExecutionState M.empty 0 insts 0 False

executionStep :: ExecutionState -> ExecutionState
executionStep state@ExecutionState {..} =
  case instructions !! position of
    Snd freq ->
      state {frequency = getVal freq registers, position = position + 1}
    Set name val ->
      state
      { registers = M.insert name (getVal val registers) registers
      , position = position + 1
      }
    Add name val ->
      state {registers = add name val registers, position = position + 1}
    Mul name val ->
      state {registers = mul name val registers, position = position + 1}
    Mod name val ->
      state {registers = modulo name val registers, position = position + 1}
    Rcv val ->
      state {recovered = getVal val registers == 0, position = position + 1}
    Jgz val1 val2 ->
      state
      { position =
          if getVal val1 registers > 0
            then position + getVal val2 registers
            else position + 1
      }

valueParser :: Parser Val
valueParser = choice . map try $ [Register <$> lower, Value . read <$> num]
  where
    num = neg <|> pos
    neg = (:) <$> char '-' <*> pos
    pos = many1 digit

binaryOpParser :: String -> (Char -> Val -> Instruction) -> Parser Instruction
binaryOpParser pat constructor =
  (string pat *> space) *> (constructor <$> (anyChar <* space) <*> valueParser)

instructionParser :: Parser Instruction
instructionParser =
  choice . map try $
  [ string "snd " *> (Snd <$> valueParser)
  , binaryOpParser "set" Set
  , binaryOpParser "add" Add
  , binaryOpParser "mul" Mul
  , binaryOpParser "mod" Mod
  , string "rcv " *> (Rcv <$> valueParser)
  , string "jgz " *> (Jgz <$> (valueParser <* space) <*> valueParser)
  ]

parser :: Parser [Instruction]
parser = sepEndBy1 instructionParser newline

withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

main =
  withStdInput parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ frequency . until recovered executionStep $ initialState parsed
