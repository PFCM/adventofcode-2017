{-# LANGUAGE TemplateHaskell #-}

import           Data.List
import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO

isValid :: T.Text -> Bool
isValid txt =
  (length . nub $ wrds) == length wrds
  where
    wrds = T.words txt

countAllUnique :: T.Text -> Int
countAllUnique = length . filter isValid . T.lines


main = do
  contents <- T.IO.getContents
  putStr . show . countAllUnique $ contents
