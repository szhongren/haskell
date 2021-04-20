import System.IO
-- reverse polish notation
-- 10 - (4 + 3) * 2
-- 10 4 3 + 2 * -
-- stack based

solveRPN :: (Num a) => String -> a
solveRPN expression = head (foldl foldingFunction [] (words expression))
  where
    foldingFunction stack item = []

solveRPN' :: (Num a, Read a) => String -> a
solveRPN' = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "*" = (y * x) : ys
    foldingFunction (x : y : ys) "+" = (y + x) : ys
    foldingFunction (x : y : ys) "-" = (y - x) : ys
    foldingFunction xs numberString = read numberString : xs

solveRPN'' :: String -> Float
solveRPN'' = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "^" = (y ** x) : ys
    foldingFunction (x : y : ys) "*" = (y * x) : ys
    foldingFunction (x : y : ys) "/" = (y / x) : ys
    foldingFunction (x : y : ys) "+" = (y + x) : ys
    foldingFunction (x : y : ys) "-" = (y - x) : ys
    foldingFunction (x : ys) "ln" = log x : ys
    foldingFunction xs "sum" = [sum xs]
    foldingFunction xs numberString = read numberString : xs

data Node = Node Road Road | EndNode Road

data Road = Road Int Node

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  -- takes the existing best paths, and the section to optimize for, and returns the new best paths
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      -- sum costs of all paths
      forwardPriceToA = priceA + a
      crossPriceToA = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      newPathToA =
        if forwardPriceToA <= crossPriceToA
          then (A, a) : pathA
          else (C, c) : (B, b) : pathB
      newPathToB =
        if forwardPriceToB <= crossPriceToB
          then (B, b) : pathB
          else (C, c) : (A, a) : pathA
   in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
   in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
  print (solveRPN' "10 4 3 + 2 * -")
  print (solveRPN' "2 3 +")
  print (solveRPN' "90 34 12 33 55 66 + * - +")
  print (solveRPN' "90 34 12 33 55 66 + * - + -")
  print (solveRPN' "90 34 12 33 55 66 + * - + - ")
  print (solveRPN' "90 3 -")
  print (solveRPN'' "2.7 ln")
  print (solveRPN'' "10 10 10 10 sum 4 /")
  print (solveRPN'' "10 10 10 10 10 sum 4 /")
  print (solveRPN'' "10 2 ^")
  print (solveRPN'' "43.2425 0.5 ^")
  print (roadStep ([], []) (head heathrowToLondon))
  print (optimalPath heathrowToLondon)
  contents <- readFile "paths.txt"
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathPrice = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice