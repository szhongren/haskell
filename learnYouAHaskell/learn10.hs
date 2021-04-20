import Control.Applicative
import Data.Char
import Data.List
import Data.Monoid

class Functor' f where -- for things that can be mapped over, f is a type that takes one arg, and is not concrete
-- give me a function that makes a b from an a
-- and a computational context with an a in it
-- and I will return a computational context with a b in it
-- f a is read as a functor over a
  fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
  fmap' f (Just a) = Just (f a)
  fmap' f Nothing = Nothing

instance Functor' [] where
  fmap' = map

instance Functor' IO where
  fmap' f action = do
    result <- action
    return (f result)

instance Functor' ((->) r) where
  -- this is basically function composition
  fmap' f g = (\x -> f (g x))

-- fmap' f g = \x -> f (g x)
-- fmap' = (.)

data CMaybe a = CNothing | CJust Int a deriving (Show)

-- C stands for counter

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- class (Functor f) => Applicative f where
--   -- all applicatives are functors
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   (Just f) <*> something = fmap f something

-- instance Applicative [] where
--   pure x = [x]
--   fs <*> xs = [f x | f <- fs, x <- xs]

-- instance Applicative IO where
--   pure = return
--   a <*> b = do
--     f <- a
--     x <- b
--     return (f x)

-- instance Applicative ((->) r) where
--   pure x = (\_ -> x)
--   f <*> g = \x -> f x (g x)

myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b

-- return takes the value and wraps it in an IO action

myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x : xs) = (:) <$> x <*> sequenceA' xs

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' = foldr (liftA2 (:)) (pure [])

data ZipList' a = ZipList' [a]

data ZipList'' a = ZipList''
  { getZipList'' :: [a]
  }

newtype ZipList''' a = ZipList'''
  { getZipList''' :: [a]
  }

data Profession = Fighter | Archer | Accountant

data Race = Human | Elf | Orc | Goblin

data PlayerCharacter = PlayerCharacter Race Profession

newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

data CoolBool = CoolBool {getCoolBool :: Bool}

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

newtype CoolBool' = CoolBool' {getCoolBool' :: Bool}

helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"

class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'

instance Monoid' [a] where
  mempty' = []
  mappend' = (++)

instance Num a => Monoid' (Product a) where
  mempty' = Product 1
  Product x `mappend'` Product y = Product (x * y)

instance Monoid' Any where
  mempty' = Any False
  Any x `mappend'` Any y = Any (x || y)

instance Monoid' All where
  mempty' = All True
  All x `mappend'` All y = All (x && y)

instance Monoid' Ordering where
  mempty' = EQ
  LT `mappend'` _ = LT
  EQ `mappend'` y = y
  GT `mappend'` _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y =
  let a = length x `compare` length y
      b = x `compare` y
   in if a == EQ then b else a

lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend` (x `compare` y)

lengthCompare'' :: String -> String -> Ordering
lengthCompare'' x y =
  (length x `compare` length y)
    `mappend` (vowels x `compare` vowels y)
    `mappend` (x `compare` y)
  where
    vowels = length . filter (`elem` "aeiou")

-- Maybe a is only a Monoid if a is a Monoid
instance Monoid' a => Monoid' (Maybe a) where
  mempty' = Nothing
  Nothing `mappend'` m = m
  m `mappend'` Nothing = m
  Just m1 `mappend'` Just m2 = Just (m1 `mappend'` m2)

instance Monoid' (First a) where
  mempty' = First Nothing
  First (Just x) `mappend'` _ = First (Just x)
  First Nothing `mappend'` x = x

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

instance Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = foldMap f l `mappend` f x `mappend` foldMap f r

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

-- 2 functor laws
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

applicativeLift = do
  -- liftA2 takes a normal binary function and makes it a function that operates on 2 functors
  print (fmap (\x -> [x]) (Just 4))
  print (liftA2 (:) (Just 3) (Just [4]))
  print ((:) <$> Just 3 <*> Just [4])
  print (sequenceA [Just 1, Just 2])
  print (sequenceA [Just 3, Just 2, Just 1])
  print (sequenceA [Just 3, Nothing, Just 1])
  print (sequenceA [(+ 3), (+ 2), (+ 1)] 3)
  print (sequenceA [[1, 2, 3], [4, 5, 6]])
  print ((:) <$> [1, 2, 3] <*> sequenceA [[4, 5, 6]])
  print ((:) <$> [1, 2, 3] <*> ((:) <$> [4, 5, 6] <*> pure []))
  print (sequenceA [[1, 2, 3], [4, 5, 6], [3, 4, 4], []])
  print (map (\f -> f 7) [(> 4), (< 10), odd])
  print (and $ map (\f -> f 7) [(> 4), (< 10), odd])
  print (sequenceA [(> 4), (< 10), odd] 7)
  print (and $ sequenceA [(> 4), (< 10), odd] 7)
  print (sequenceA [[1, 2, 3], [4, 5, 6]])
  print ([[x, y] | x <- [1, 2, 3], y <- [4, 5, 6]])
  print (sequenceA [[1, 2], [3, 4]])
  print ([[x, y] | x <- [1, 2], y <- [3, 4]])
  print (sequenceA [[1, 2], [3, 4], [5, 6]])
  print ([[x, y, z] | x <- [1, 2], y <- [3, 4], z <- [5, 6]])

sequenceAGetLine = do
  list <- sequenceA [getLine, getLine, getLine]
  print (list)

-- applicative laws
-- pure f <*> x = fmap f x
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x /= pure (f x)
-- u <*> pure y = pure ($ y) <*> u

newtypeTest = do
  -- 2 ways for the list type to be an applicative functor
  print ([(+ 1), (* 100), (* 5)] <*> [1, 2, 3])
  print (getZipList $ ZipList [(+ 1), (* 100), (* 5)] <*> ZipList [1, 2, 3])
  print (getZipList''' $ ZipList''' [1, 2, 3])
  print (CharList "This will be shown!")
  print (CharList "benny" == CharList "benny")
  print (CharList "benny" == CharList "oysters")
  print (getPair $ fmap (* 100) (Pair (2, 3)))
  print (getPair $ fmap reverse (Pair ("london calling", 3)))

newtypeLaziness = do
  print (head [3, 4, 5, undefined, 2, undefined])
  -- print (helloMe undefined ) -- this will fail
  print (helloMe' undefined)

monoidBasics = do
  print (4 * 1)
  print (1 * 9)
  print ([1, 2, 3] ++ [])
  print ([] ++ [0.5, 2.5])
  -- associative
  print ((3 * 2) * (8 * 5))
  print (3 * (2 * (8 * 5)))
  print ("la" ++ ("di" ++ "da"))
  print (("la" ++ "di") ++ "da")
  -- a monoid is when you have an associative binary function, and a value that acts as an identity with respect to that function
  -- monoid laws
  print ([] `mappend` [123, 234] == [123, 234])
  print ([321, 432] `mappend` [] == [321, 432])
  print (([1] `mappend` [2]) `mappend` [3] == [1] `mappend` ([2] `mappend` [3]))

monoidList = do
  print ([1, 2, 3] `mappend` [4, 5, 6])
  print (("one" `mappend` "two") `mappend` "tree")
  print ("one" `mappend` ("two" `mappend` "tree"))
  print ("one" `mappend` "two" `mappend` "tree")
  print ("pang" `mappend` mempty)
  print (mconcat [[1, 2], [3, 6], [9]])
  print (mempty :: [Integer])
  -- order does matter for most monoids
  print (mappend "one" "two")
  print (mappend "two" "one")

monoidProductAndSum = do
  -- identity
  print (1 * 4)
  print (5 * 1)
  -- associativity
  print ((1 * 3) * 5)
  print (1 * (3 * 5))
  -- identity
  print (0 + 4)
  print (5 + 0)
  -- associativity
  print ((1 + 3) + 5)
  print (1 + (3 + 5))
  -- product
  print (getProduct $ Product 3 `mappend'` Product 9)
  print (getProduct $ Product 3 `mappend'` mempty')
  print (getProduct $ Product 3 `mappend'` Product 4 `mappend'` Product 2)
  print (getProduct . mconcat . map Product $ [3, 4, 2])
  -- sum
  print (getSum $ Sum 2 `mappend` Sum 9)
  print (getSum $ mempty `mappend` Sum 9)
  print (getSum . mconcat . map Sum $ [1, 2, 3])

monoidAnyAll = do
  print (getAny $ Any True `mappend` Any False)
  print (getAny $ mempty `mappend` Any False)
  print (getAny . mconcat . map Any $ [False, False, False, True])
  print (getAny $ mempty `mappend` mempty)
  print (getAll $ mempty `mappend` All True)
  print (getAll $ mempty `mappend` All False)
  print (getAll . mconcat . map All $ [True, True, True])
  print (getAll . mconcat . map All $ [True, True, False])

monoidOrdering = do
  print (compare 1 2)
  print (compare 2 2)
  print (compare 3 2)
  print (LT `mappend` GT)
  print (GT `mappend` LT)
  print (mempty `mappend` LT)
  print (mempty `mappend` GT)
  print (lengthCompare' "zen" "ants")
  print (lengthCompare' "zen" "ant")
  print (lengthCompare'' "zen" "anna")
  print (lengthCompare'' "zen" "ana")
  print (lengthCompare'' "zen" "ann")

monoidMaybe = do
  print (Nothing `mappend` Just "andy")
  print (Just LT `mappend` Nothing)
  print (Just (Sum 3) `mappend` Just (Sum 4))
  print (getFirst $ First (Just 'a') `mappend` First (Just 'b'))
  print (getFirst $ First Nothing `mappend` First (Just 'b'))
  print (getFirst $ First (Just 'a') `mappend` First Nothing)
  print (getFirst . mconcat . map First $ [Nothing, Just 9, Just 10])
  print (getLast . mconcat . map Last $ [Nothing, Just 9, Just 10])
  print (getLast $ Last (Just "one") `mappend` Last (Just "two"))

foldable = do
  print (foldr (*) 1 [1, 2, 3])
  print (foldl (+) 2 (Just 9))
  print (foldr (||) False (Just True))
  let testTree = Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree)) (Node 9 (Node 8 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree))
  print (testTree)
  print (foldl (+) 0 testTree)
  print (foldl (*) 1 testTree)
  -- function passed to foldMap has to return a monoid
  print (getAny $ foldMap (\x -> Any $ x == 3) testTree)
  print (getAny $ foldMap (\x -> Any $ x > 15) testTree)
  print (foldMap (\x -> [x]) testTree)

main = foldable