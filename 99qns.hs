myLast :: [a] -> a
myLast [] = error "can't get last of an empty list"
myLast [last] = last
myLast (_ : xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "can't get penultimate of an empty list"
myButLast [_] = error "can't get penultimate of a singleton list"
myButLast [penultimate, _] = penultimate
myButLast (_ : xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "can't get kth item of an empty list"
elementAt (x : xs) y
  | y < 1 = error "can't get kth item if k is less than 1"
  | y == 1 = x
  | otherwise = elementAt xs (y - 1)

myLength :: [a] -> Int
myLength ls = myLengthHelper ls 0
  where
    myLengthHelper ls l = foldl (\l _ -> l + 1) l ls

-- myLengthHelper [] l = l
-- myLengthHelper (x : xs) l = myLengthHelper xs (l + 1)

qns1to10 = do
  print (myLast [1, 2, 3, 4] == 4)
  print (myLast ['x', 'y', 'z'] == 'z')
  print (myButLast [1, 2, 3, 4] == 3)
  print (myButLast ['a' .. 'z'] == 'y')
  print (elementAt ['a' .. 'e'] 3)
  print (elementAt [1, 2, 3] 2)
  print (elementAt "haskell" 5)
  print (myLength [123, 456, 789])
  print (myLength "Hello, world!")

main = qns1to10
