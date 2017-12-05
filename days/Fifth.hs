import Control.Monad.ST
import Data.STRef
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Text.Read (decimal, signed)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!?), (//))

countSteps :: [Int] -> Int
countSteps = length . steps2 . V.fromList

countSteps' :: [Int] -> Int
countSteps' = steps2' . V.fromList

-- part 1
-- this doesn't feel real great
steps1 :: V.Vector Int -> [Int]
steps1 jumpTable = go jumpTable [0]
  where
    go _ [] = []
    go jtab (p:xs) =
      case jtab !? p of
        Just jump -> go (jtab // [(p, jump + 1)]) $ (p + jump) : p : xs
        Nothing -> xs

-- part 2
-- it's just as unpleasant
-- worse even, because now we're keeping a _lot_ of results
steps2 :: V.Vector Int -> [Int]
steps2 jumpTable = go jumpTable [0]
  where
    go _ [] = []
    go jtab (p:xs) =
      case jtab !? p of
        Just jump -> go (jtab // [(p, jump + inc)]) $ (p + jump) : p : xs
          where inc =
                  if jump >= 3
                    then -1
                    else 1
        Nothing -> xs

-- see if we can not generate the whole list
steps2' :: V.Vector Int -> Int
steps2' jumpTable = go jumpTable 0 0
  where
    go jtab pos steps =
      case jtab !? pos of
        Just jump -> go (jtab // [(pos, jump + inc)]) (pos + jump) (steps + 1)
          where inc =
                  if jump >= 3
                    then -1
                    else 1
        Nothing -> steps

-- try the ST monad so we don't have to keep everything
-- stepsST :: V.Vector Int -> Int
-- stepsST jumpTable = runST $ do
--   p <- newSTRef 0
--   steps <- newSTRef 0
--   jt <- V.thaw jumpTable
--   run jt p
--   where
--     run jtab pos =
--       case jtab !? pos of
--         Just jump ->
parse :: T.Text -> Either String [Int]
parse = fmap (map fst) . mapM (signed decimal) . T.lines

main = T.IO.interact (T.pack . show . fmap countSteps' . parse)
