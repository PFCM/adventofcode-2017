import Data.Foldable (asum)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Monoid (Sum, (<>), mempty)
import qualified Data.Vector as V
import Text.Parsec
import Text.Parsec.String (Parser)

-- a 3x3 will always turn into a 4x4, which is 4 2x2s
-- a 2x2 always turns into a 3x3
-- therefore we can represent the dynamic structure nicely as a tree which
-- we just unfold from an initial value
data Node a
  = TwoNode (V.Vector a)
  | ThreeNode (V.Vector a)
  | InternalNode (Node a)
                 (Node a)
                 (Node a)
                 (Node a)
  deriving (Show)

instance Foldable Node where
  foldMap f (InternalNode a b c d) =
    foldMap f a <> foldMap f b <> foldMap f c <> foldMap f d
  foldMap f (TwoNode vec) = foldMap f vec
  foldMap f (ThreeNode vec) = foldMap f vec

type Rules = M.Map (V.Vector Bool) (V.Vector Bool)

initialNode :: Node Bool
initialNode =
  ThreeNode . V.fromList $
  [False, True, False, False, False, True, True, True, True]

split :: Rules -> Node Bool -> Node Bool
split rules (InternalNode a b c d) =
  InternalNode (split rules a) (split rules b) (split rules c) (split rules d)
split rules (TwoNode img) = ThreeNode $ ruleLookup rules img
split rules (ThreeNode img) =
  InternalNode
    (TwoNode . V.backpermute enhanced $ topLeft)
    (TwoNode . V.backpermute enhanced $ topRight)
    (TwoNode . V.backpermute enhanced $ bottomLeft)
    (TwoNode . V.backpermute enhanced $ bottomRight)
  where
    enhanced = ruleLookup rules img
    topLeft = V.fromList [0, 1, 4, 5]
    topRight = V.fromList [2, 3, 6, 7]
    bottomLeft = V.fromList [8, 9, 12, 13]
    bottomRight = V.fromList [10, 11, 14, 15]

-- have to do rotations and flips until it is a key then return the value
ruleLookup :: Rules -> V.Vector Bool -> V.Vector Bool
ruleLookup rs = fromJust . asum . map (`M.lookup` rs) . transformations

-- generate possible rotations and flips
transformations :: V.Vector Bool -> [V.Vector Bool]
transformations img = rotations img ++ (rotations . flipImg $ img)

rotations :: V.Vector Bool -> [V.Vector Bool]
rotations = take 4 . iterate rotate90

-- a bit gross? Very gross.
rotate90 :: V.Vector Bool -> V.Vector Bool
rotate90 img
  | V.length img == 4 = V.backpermute img $ V.fromList [2, 0, 3, 1]
  | otherwise = V.backpermute img $ V.fromList [6, 3, 0, 7, 4, 1, 8, 5, 2]

flipImg :: V.Vector Bool -> V.Vector Bool
flipImg = V.concat . map V.reverse . rows

rows :: V.Vector Bool -> [V.Vector Bool]
rows img =
  let size =
        if V.length img == 4
          then 2
          else 3
      go vec
        | V.null vec = []
        | otherwise = slice : go rest
        where
          (slice, rest) = V.splitAt size vec
  in go img

imgParser :: Parser (V.Vector Bool)
imgParser =
  (V.fromList . map (== '#')) <$> many1 (oneOf ".#" <* optional (char '/'))

ruleParser :: Parser (V.Vector Bool, V.Vector Bool)
ruleParser = (,) <$> (imgParser <* string " => ") <*> imgParser

parser :: Parser Rules
parser = M.fromList <$> sepEndBy ruleParser newline

withStdInput :: Parser a -> IO a
withStdInput p = do
  result <- parse (p <* eof) "<input>" <$> getContents
  either (error . show) return result

boolToSum :: Bool -> Sum Int
boolToSum True = 1
boolToSum False = 0

main =
  withStdInput parser >>= \parsed -> do
    putStrLn "Part 1"
    print $ foldMap boolToSum $ iterate (split parsed) initialNode !! 5
