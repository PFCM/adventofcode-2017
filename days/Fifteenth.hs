import Data.Bits
import Data.List (foldl')

part1 :: String -> String
part1 = undefined


-- hardcoded from specific puzzle input
generatorA :: [Integer]
generatorA = iterate (generateNext 16807) 883 -- 65

generatorB :: [Integer]
generatorB = iterate (generateNext 48271) 879 -- 8921

-- general
generateNext :: (Integral a, Num a) => a -> a -> a
generateNext factor prev =
  (prev * factor) `mod` 2147483647

compareLower16 :: (Integral a, Bits a) => a -> a -> Bool
compareLower16 a b = (a .&. 0xffff) == (b .&. 0xffff)

isMultiple :: Integer -> Integer -> Bool
isMultiple a =  (== 0) . (`mod` a)

generatorA2 :: [Integer]
generatorA2 = filter (isMultiple 4) generatorA

generatorB2 :: [Integer]
generatorB2 = filter (isMultiple 8) generatorB

main = do
  putStrLn "Part 1"
  print . length . filter (uncurry compareLower16) . take 40000000 $ zip generatorA generatorB
  putStrLn "Part 2"
  print . length . filter (uncurry compareLower16) . take 5000000 $ zip generatorA2 generatorB2
