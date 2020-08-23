import Data.List

-- import Data.List (nub, sort)
-- import Data.List hiding (intersperse)
-- import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

main = do
  print (intersperse '.' "monkey")
  print (intercalate " " ["Hey", "there", "guys"])
  print (transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
  print (concat ["foo", "bar", "car"])
  print (concatMap (replicate 5) [1 .. 4])
  print (and $ map (> 4) [5 .. 8])
  print (all (> 4) [5 .. 8])
  print (or $ map (== 4) [1 .. 10])
  print (any (== 4) [1 .. 10])
