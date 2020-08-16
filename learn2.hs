import Data.Typeable

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

factorial :: Integer -> Integer
factorial n = product [1 .. n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

main = do
  -- :: means "has type of"
  print "TEST"
  print (removeNonUppercase "AbigCAT")
  print (addThree 8 2 12)
  print (factorial 10)
  print (circumference 5)
  print (circumference' 5)

  -- typeclasses
  -- :t (==) = (==) :: (Eq a) => a -> a -> Bool
  -- everything before the => is part of this function's class constraint
  -- every type a must be a member of the Eq typeclass

  -- Eq is for types that support equality testing
  print (5 == 5)
  print (5 /= 5)
  -- Ord is for types that have an ordering
  print ("CAT" > "RAT")
  print ("CAT" `compare` "RAT")
  -- Show is for types that can be presented as strings
  print (show 3)
  print (show 8.4312)
  -- Read is for strings that can be converted from strings
  print (read "True" || False)
  print (read "8.42" + 32.8)
  print (read "[1,2,3,4]" ++ [3])
  print ((read "5" :: Float) * 4)
  -- Enum is for types that can be enumerated, and can thus be used in texas ranges, and can also use succ and pred
  print ['a' .. 'e']
  print [LT .. GT]
  print (succ 'B')
  -- Bounded is for types that have an upper and lower bound
  print (maxBound :: Char)
  print (minBound :: Int)
  print (maxBound :: (Bool, Int, Char))

-- Num is for types that can act like numbers
-- :t 20 = 20 :: (Num t) => t
-- Integral is for types that can act like numbers, but only whole numbers, includes Int and Integer
-- Floating is for types that can act like numbers, but only floats, includes Float and Double
