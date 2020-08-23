-- currying
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- partial application
multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine = multThree 9

multWithEighteen :: (Num a) => a -> a
multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
  let smallerSorted = quicksort' (filter (<= x) xs)
      biggerSorted = quicksort' (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

largestDivisibleBy :: (Integral a) => a -> [a] -> a
largestDivisibleBy a ls = head (filter p (reverse (quicksort' ls)))
  where
    p x = x `mod` a == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > 15

-- lambdas
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1 .. 100]))

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

-- folds
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (+) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

main = do
  print (multThree 1 2 3)
  print (multTwoWithNine 2 3)
  print (multWithEighteen 4)
  print (compareWithHundred 4)
  print (divideByTen 6)
  print (applyTwice (+ 3) 10)
  print (applyTwice (++ " HAHA") "HEY")
  print (applyTwice ("HAHA " ++) "HEY")
  print (applyTwice (multThree 2 2) 9)
  print (applyTwice (3 :) [1])
  print (zipWith' (+) [4, 2, 5, 6] [2, 6, 2, 3])
  print (zipWith' max [6, 3, 2, 1] [7, 3, 1, 5])
  print
    ( zipWith'
        (++)
        ["foo ", "bar ", "baz "]
        ["fighters", "hoppers", "aldrin"]
    )
  print (zipWith' (*) (replicate 5 2) [1 ..])
  print
    ( zipWith'
        (zipWith' (*))
        [[1, 2, 3], [3, 5, 6], [2, 3, 4]]
        [[3, 2, 2], [3, 4, 5], [5, 4, 3]]
    )
  print (flip' zip [1, 2, 3, 4, 5] "hello")
  print (zipWith' (flip' div) [2, 2 ..] [10, 8, 6, 4, 2])
  print (largestDivisibleBy 5 [1 .. 23])
  print (sum (takeWhile (< 100000) (filter odd (map (^ 2) [1 ..]))))
  print numLongChains
  print numLongChains'
  print (zipWith (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5])
  print (map (\(a, b) -> a + b) [(1, 2), (3, 5), (6, 3), (2, 6), (2, 5)])
  print (sum' [3, 5, 2, 1])
  print (sum'' [3, 5, 2, 1])
  -- scanl and scanr shows the intermediate accumulators in a list
  print (scanl (+) 0 [3, 5, 2, 1])
  print (scanr (+) 0 [3, 5, 2, 1])
  print (scanl1 (\acc x -> if x > acc then x else acc) [3, 4, 5, 3, 7, 9, 2, 1])
  print (scanl (flip (:)) [] [3, 2, 1])
  -- function application operator
  -- right-associative
  -- evaluates the right expression first before passing to the left
  print (sum $ map sqrt [1 .. 130])
  print (map ($ 3) [(4 +), (10 *), (^ 2), sqrt])
  -- function composition
  -- . reads as "after"
  print (map (negate . abs) [5, -3, -6, 7, -3, 2, -19, 24])
  print (map (negate . sum . tail) [[1..5], [3..6], [1..7]]
  print (replicate 100 . product . map (*3) . zipWith max [1..5] $ [4..8])