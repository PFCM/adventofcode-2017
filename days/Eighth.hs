import qualified Data.Map as M
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

type Registers = M.Map String Int

type Cond = (Registers -> Bool)

data Condition
  = Equals
  | NotEquals
  | Less
  | Greater
  | LessEquals
  | GreaterEquals

data Op
  = Inc
  | Dec

data Instruction = Instruction
  { tRegister :: String
  , tOp :: Op
  , tVal :: Int
  , tCond :: Cond
  }

-- PART 1
updateVal :: Registers -> Instruction -> Registers
updateVal rs inst = M.alter update name rs
  where
    name = tRegister inst
    val' = tVal inst
    val =
      case tOp inst of
        Inc -> val'
        Dec -> -val'
    update oldVal = Just (maybe val (+ val) oldVal)

-- TODO: if the cond is on a register that doesn't exist we should add it into
-- the register map
runInstructions :: [Instruction] -> Registers
runInstructions = foldl step M.empty
  where
    step reg inst =
      if tCond inst reg
        then updateVal reg inst
        else reg

makeCond :: String -> Condition -> Int -> Cond
makeCond reg cond val = op val . M.findWithDefault 0 reg
  where
    op =
      case cond of
        Equals -> (==)
        NotEquals -> (/=)
        Less -> (>)
        Greater -> (<)
        LessEquals -> (>=)
        GreaterEquals -> (<=)

instructionParser :: Parser Instruction
instructionParser =
  Instruction <$> nameP <* space <*> opP <*> valP <* space <*> condP
  where
    nameP = many1 lower
    opP = Inc <$ (try . string $ "inc ") <|> Dec <$ (try . string $ "dec ")
    valP = read <$> (minus <|> number)
      where
        minus = (:) <$> char '-' <*> number
        number = many1 digit
    condP = string "if " *> (makeCond <$> nameP <*> condition <*> valP)
      where
        condition =
          choice
            [ Equals <$ (try . string $ " == ")
            , NotEquals <$ (try . string $ " != ")
            , Less <$ (try . string $ " < ")
            , Greater <$ (try . string $ " > ")
            , LessEquals <$ (try . string $ " <= ")
            , GreaterEquals <$ (try . string $ " >= ")
            ]

inputParser :: Parser [Instruction]
inputParser = many1 $ instructionParser <* newline

parseStdInput :: Parser a -> IO a
parseStdInput p = do
  inputs <- getContents
  let result = parse (p <* eof) "<input>" inputs
  either (error . show) return result

main = do
  inputs <- parseStdInput inputParser
  putStrLn "Part 1: "
  print (maximum . runInstructions $ inputs)
