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

writerMonad = do
  print (isBigGang' 3)
  print (isBigGang' 30)
  print ((3, "Smallish gang.") `applyLog` isBigGang')
  print ((30, "A freaking platoon.") `applyLog` isBigGang')
  print (("Tobin", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length")))
  print (("Bathcat", "Got outlaw name.") `applyLog` (\x -> (length x, "Applied length")))
  print ([1, 2, 3] `mappend` [4, 5, 6])
  print (B.pack [99, 104, 105] `mappend` B.pack [104, 117, 97, 104, 117, 97])
  print (Sum 3 `mappend` Sum 9)
  print (("beans", Sum 10) `applyLog''` addDrink)
  print (("jerky", Sum 25) `applyLog''` addDrink)
  print (("dogmeat", Sum 5) `applyLog''` addDrink)
  print (("dogmeat", Sum 5) `applyLog''` addDrink `applyLog''` addDrink)

main = writerMonad