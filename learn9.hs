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