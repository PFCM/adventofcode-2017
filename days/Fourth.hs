{-# LANGUAGE TemplateHaskell #-}

import           Data.List
import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO
import           Debug.Trace

noDuplicates :: T.Text -> Bool
noDuplicates = isValid . T.words

isValid :: Eq a => [a] -> Bool
isValid x =
  (length . nub $ x) == length x

countAllUnique :: T.Text -> Int
countAllUnique = length . filter noDuplicates . T.lines

updateCount :: Maybe Int -> Maybe Int
updateCount (Just val) = Just (val + 1)
updateCount Nothing    = Just 1

charCounts :: T.Text -> M.Map Char Int
charCounts = T.foldl (flip (M.alter updateCount)) M.empty

secondPolicy :: T.Text -> Bool
secondPolicy = isValid . map charCounts . T.words

countNoAnagrams :: T.Text -> Int
countNoAnagrams = length . filter secondPolicy . T.lines

main = do
  contents <- T.IO.getContents
  putStr . show . countNoAnagrams $ contents
