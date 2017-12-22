import qualified Data.Vector.Unboxed as V

data Spinlock = Spinlock
  { tPos :: Int
  , tLength :: Int
  , tValues :: V.Vector Int
  }

newSpinlock :: Spinlock
newSpinlock = Spinlock 0 2018 (V.replicate 2018 0)

main = do
  putStrLn "Part 1"
  print "ok"
