import Control.Applicative
import Data.Char
import Data.List

class Functor' f where -- for things that can be mapped over, f is a type that takes one arg, and is not concrete
-- give me a function that makes a b from an a
-- and a computational context with an a in it
-- and I will return a computational context with a b in it
-- f a is read as a functor over a
  fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
  fmap' f (Just a) = Just (f a)
  fmap' f Nothing = Nothing

instance Functor' IO where
  fmap' f action = do
    result <- action
    return (f result)

instance Functor' ((->) r) where
  -- this is basically function composition
  fmap' f g = (\x -> f (g x))

-- fmap' f g = \x -> f (g x)
-- fmap' = (.)

-- instance Applicative ((->) r) where
--   pure x = (\_ -> x)
--   f <*> g = \x -> f x (g x)

data CMaybe a = CNothing | CJust Int a deriving (Show)

-- C stands for counter

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- class (Functor f) => Applicative f where
--   -- all applicatives are functors
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b

-- return takes the value and wraps it in an IO action

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

reverseLine = do
  line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"

reverseLine' = do
  line <- fmap' reverse getLine
  -- we can do this because IO is defined as instance of Functor
  -- fmap' :: (a -> b) -> IO a -> IO b
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

intersperseWords = do
  line <- fmap' (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

compositionWithFmap = do
  print (fmap (* 3) (+ 100) 1)
  print ((* 3) `fmap'` (+ 100) $ 1)
  print ((* 3) . (+ 100) $ 1)
  print (fmap (show . (* 3)) (* 100) 1)

functorOver = do
  print (fmap (replicate 3) [1, 2, 3, 4])
  print (fmap (replicate 3) Just 4)
  print (fmap (replicate 3) (Right "blah") :: Either String [[Char]]) -- happy path
  print (fmap (replicate 3) Nothing :: Maybe String)
  print (fmap (replicate 3) (Left "foo") :: Either String [[Char]]) -- failure

-- 2 monad laws
-- 1. fmap id = id
-- 2. fmap (f . g) = fmap f . fmap g, or fmap (f . g) F = fmap f (fmap g F)
idFmap = do
  print (fmap id (Just 3))
  print (id (Just 3))
  print (fmap id [1 .. 5])
  print (id [1 .. 5])
  print (fmap id [] :: [String])
  print (fmap id Nothing :: Maybe String)

fgFmap = do
  print (fmap ((+ 3) . (* 5)) Nothing)
  print ((fmap (+ 3) . fmap (* 5)) Nothing)
  print (fmap (+ 3) (fmap (* 5) Nothing))
  print (fmap ((+ 3) . (* 5)) (Just 5))
  print ((fmap (+ 3) . fmap (* 5)) (Just 5))
  print (fmap (+ 3) (fmap (* 5) (Just 5)))
  print (Just ((+ 3) ((* 5) 5)))

-- does not obey functor laws, thus not a functor

cMaybe = do
  print (CNothing :: CMaybe String)
  print (CJust 0 "Haha")
  print (CJust 100 [1, 2, 3])
  print (fmap (++ "ha") (CJust 0 "ho"))
  print (fmap (++ "he") (fmap (++ "ha") (CJust 0 "ho")))
  print (fmap (++ "blah") CNothing)
  print (fmap id (CJust 0 "haha"))
  print (id (CJust 0 "haha"))

applicativeBasic = do
  let a = fmap (*) [1, 2, 3, 4]
  print (fmap (\f -> f 9) a)
  -- <*> is like an fmap that takes a function in a functor as well
  print (Just (+ 3) <*> Just 9)
  print (pure (+ 3) <*> Just 10)
  print (Just (++ "hahaha") <*> Nothing)
  print (Nothing <*> Just "woot" :: Maybe String)
  print (pure (+) <*> Just 3 <*> Just 5)
  print (pure (+) <*> Just 3 <*> Nothing)
  print (pure (+) <*> Nothing <*> Just 5)
  print (pure (+) <*> Just 3 <*> Just 5)
  print (fmap (+) (Just 3) <*> (Just 5))
  -- <$> is just fmap but infix
  print ((+) <$> (Just 3) <*> (Just 5))
  print ((++) <$> Just "johntra" <*> Just "volta")

applicativeList = do
  -- use a list comprehension to apply every function in first list to every arg in second
  -- lists can be thought of as non-determistic functions, so <*> 2 lists returns a combination
  print ([(* 0), (+ 100), (^ 2)] <*> [1, 2, 3])
  print ([(+), (*)] <*> [1, 2] <*> [3, 4])
  print ((++) <$> ["ha", "heh", "hmm"] <*> ["?", "!", "."])
  print ([x * y | x <- [2, 5, 10], y <- [8, 10, 11]])
  print ((*) <$> [2, 5, 10] <*> [8, 10, 11])
  print (filter (> 50) $ (*) <$> [2, 5, 10] <*> [8, 10, 11])

twoLinesConcat = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "The two lines concatenated turn out to be: " ++ a

applicativeFunction = do
  print (pure 3 "blah")
  -- k <$> f <*> g creates a function that will call k with the eventual results of f and g
  -- (+) <$> Just 3 <*> Just 5, means we use + on values that might or might not be there, which also results in a Maybe
  -- (+) <$> (+10) <*> (+5), means we will use + on the future return values of (+10) and (+5), which also results in a single argument function
  print ((+) <$> (+ 3) <*> (* 100) $ 5)
  print ((\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) <*> (/ 2) $ 5)

applicativeZipList = do
  print (getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList [100, 100, 100])
  print (getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList [100, 100 ..])
  print (getZipList $ max <$> ZipList [1, 2, 3, 4, 5, 3] <*> ZipList [5, 3, 1, 2])
  -- The (,,) function is the same as \x y z -> (x,y,z). Also, the (,) function is the same as \x y -> (x,y).
  print (getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat")

main = applicativeZipList