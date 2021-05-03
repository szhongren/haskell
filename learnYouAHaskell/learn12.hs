import Control.Monad.Writer
import qualified Data.ByteString.Lazy as B
import Data.Monoid

isBigGang :: Int -> Bool
isBigGang x = x > 9

isBigGang' :: Int -> (Bool, String)
isBigGang' x = (x > 9, "Compared gang size to 9")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

applyLog' :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog' (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

applyLog'' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog'' (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String

type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-- newtype Writer w a = Writer {runWriter :: (a, w)}

-- instance (Monoid w) => Monad (Writer w) where
--   return x = Writer (x, mempty)
--   (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a * b)

multWithLog' :: Writer [String] Int
multWithLog' = logNumber 3 >>= (\a -> logNumber 5 >>= (\b -> return (a * b)))

multWithLog'' :: Writer [String] Int
multWithLog'' = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a * b)

multWithLog''' :: Writer [String] Int
multWithLog''' = logNumber 3 >>= (\a -> logNumber 5 >>= (\b -> tell ["Gonna multiply these two"] >> return (a * b)))

gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
  | b == 0 = do
    tell ["finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd'' b (a `mod` b)

gcd''' :: Int -> Int -> Writer [String] Int
gcd''' a b
  | b == 0 = tell ["finished with " ++ show a] >> return a
  | otherwise =
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      >> gcd'' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
    tell ["finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result

gcdReverse' :: Int -> Int -> Writer (DiffList String) Int
gcdReverse' a b
  | b == 0 = do
    tell (toDiffList ["finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcdReverse' b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a)

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do
  tell ["0"]
finalCountDown x = do
  finalCountDown (x - 1)
  tell [show x]

finalCountDown' :: Int -> Writer (DiffList String) ()
finalCountDown' 0 = do
  tell (toDiffList ["0"])
finalCountDown' x = do
  finalCountDown' (x - 1)
  tell (toDiffList [show x])

writerMonad = do
  print (isBigGang' 3)
  print (isBigGang' 30)
  -- concat to log that we carry along
  print ((3, "Smallish gang.") `applyLog` isBigGang')
  print ((30, "A freaking platoon.") `applyLog` isBigGang')
  print (("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length")))
  print (("Bathcat", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length")))
  print ([1, 2, 3] `mappend` [4, 5, 6])
  print (B.pack [99, 104, 105] `mappend` B.pack [104, 117, 97, 104, 117, 97])
  print (Sum 3 `mappend` Sum 9)
  -- same, but can handle other monoids
  print (("beans", Sum 10) `applyLog''` addDrink)
  print (("jerky", Sum 25) `applyLog''` addDrink)
  print (("dogmeat", Sum 5) `applyLog''` addDrink)
  print (("dogmeat", Sum 5) `applyLog''` addDrink `applyLog''` addDrink)
  print (runWriter (return 3 :: Writer String Int))
  print (runWriter (return 3 :: Writer (Sum Int) Int))
  print (runWriter (return 3 :: Writer (Product Int) Int))
  print (runWriter multWithLog)
  print (runWriter multWithLog')
  print (runWriter multWithLog'')
  print (runWriter multWithLog''')
  print (gcd' 8 3)
  print (runWriter (gcd'' 8 3))
  print (runWriter (gcd''' 8 3))
  print (fst $ runWriter (gcd''' 8 3))
  mapM_ putStrLn $ snd $ runWriter (gcd''' 8 3)
  mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)
  print (fromDiffList (toDiffList [1, 2, 3, 4] `mappend` toDiffList [1, 2, 3]))
  mapM_ putStrLn . fromDiffList $ snd $ runWriter (gcdReverse' 110 34)
  -- mapM_ putStrLn . fromDiffList . snd . runWriter  $ finalCountDown' 50000
  -- mapM_ putStrLn . snd . runWriter  $ finalCountDown 50000

main = writerMonad