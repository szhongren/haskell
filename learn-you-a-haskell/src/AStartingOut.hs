module AStartingOut (run) where

simpleArithmetic :: IO ()
simpleArithmetic = do
  putStrLn ("2 + 15 = " ++ show (2 + 15 :: Int))
  print ("49 * 100 = " ++ show (49 * 100 :: Int))
  print ("1892 - 1472 = " ++ show (1892 - 1472 :: Int))
  print ("5 / 2 = " ++ show (5 / 2 :: Float))

precedence :: IO ()
precedence = do
  print ("(50 * 100) - 4999 = " ++ show ((50 * 100) - 4999 :: Int))
  print ("50 * 100 - 4999 = " ++ show (50 * 100 - 4999 :: Int))
  print ("50 * (100 - 4999) = " ++ show (50 * (100 - 4999) :: Int))

booleanAlgebra :: IO ()
booleanAlgebra =
  do
    print ("True && False = " ++ show (True && False :: Bool))
    print ("True && True = " ++ show (True && True :: Bool))
    print ("False || True = " ++ show (False || True :: Bool))
    print ("not False = " ++ show (not False :: Bool))
    print ("not (True && True) = " ++ show (not (True && True) :: Bool))

equalityTest :: IO ()
equalityTest =
  do
    print ("5 == 5 = " ++ show ((5 :: Int) == (5 :: Int)))
    print ("1 == 0 = " ++ show ((1 :: Int) == (0 :: Int)))
    print ("5 /= 5 = " ++ show ((5 :: Int) /= (5 :: Int)))
    print ("5 /= 4 = " ++ show ((5 :: Int) /= (4 :: Int)))
    print ("\"hello\" == \"hello\" = " ++ show ("hello" == "hello" :: Bool))

functions :: IO ()
functions = do
  print ("succ 8 = " ++ show (succ 8 :: Int))
  print ("min 9 10 = " ++ show (min 9 10 :: Int))
  print ("max 100 101 = " ++ show (max 100 101 :: Int))

-- baby's first functions

doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = x * 2 + y * 2

doubleUs' :: (Num a) => a -> a -> a
doubleUs' x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' :: (Ord a, Num a) => a -> a
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien :: String
conanO'Brien = "It's a-me, Conan O'Brien!"

-- an intro to lists

lostNumbers :: [Integer]
lostNumbers = [4, 8, 15, 16, 23, 42]

listOps :: IO ()
listOps = do
  print ([1, 2, 3, 4] ++ [9, 10, 11, 12])
  print ("hello" ++ " " ++ "world")
  print (['w', 'o'] ++ ['o', 't'])
  print (5 : [1, 2, 3, 4, 5])
  print ("steve Buscemi" !! 6)
  print ([9.4, 33.2, 96.2, 11.2, 23.25] !! 1)

run = do
  simpleArithmetic
  precedence
  booleanAlgebra
  equalityTest
  functions
  print (doubleMe 9)
  print (doubleMe 8.3)
  print (doubleUs 4 9)
  print (doubleUs 2.3 34.2)
  print (doubleUs 28 88 + doubleMe 123)
  print conanO'Brien
