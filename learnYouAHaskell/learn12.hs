import Control.Monad.State
import Control.Monad.Writer
import qualified Data.ByteString.Lazy as B
import Data.Monoid
import Debug.Trace
import System.Random

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
  mempty = DiffList ([] ++)
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 =
  tell ["0"]
finalCountDown x = do
  finalCountDown (x - 1)
  tell [show x]

finalCountDown' :: Int -> Writer (DiffList String) ()
finalCountDown' 0 =
  tell (toDiffList ["0"])
finalCountDown' x = do
  finalCountDown' (x - 1)
  tell (toDiffList [show x])

-- instance Monad ((->) r) where
--   return x = \_ -> x
--   h >>= f = \w -> f (h w) w

addStuff :: Int -> Int
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)

addStuff' :: Int -> Int
addStuff' x =
  let a = (* 2) x
      b = (+ 10) x
   in a + b

type Stack = [Int]

-- returns (result, state)
pop' :: Stack -> (Int, Stack)
pop' (x : xs) = (x, xs)

-- returns (result, state)
push' :: Int -> Stack -> ((), Stack)
push' a xs = ((), a : xs)

-- returns (result, state)
stackManip' :: Stack -> (Int, Stack)
stackManip' stack =
  let ((), newStack1) = push' 3 stack
      (a, newStack2) = pop' newStack1
   in pop' newStack2

-- newtype State s a = State {runState :: s -> (a, s)}

-- instance Monad (State s) where
--   return x = State $ \s -> (x, s)
--   (State h) >>= f = State $ \s ->
--     let (a, newState) = h s
--         (State g) = f a
--      in g newState

pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a : xs)

-- State Stack Int == Stack -> (Int, Stack)
stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

-- we implicitly pass the state result of each operation to the next operation
stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
    then push 5
    else do
      push 3
      push 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManip
  if a == 100
    then stackStuff
    else return ()

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1, 2, 3]
    then put [8, 3, 1]
    else put [9, 2, 1]

-- random :: (RandomGen g, Random a) => g -> (a, g)
-- takes a random generator, and returns a random number and a new generator

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins' :: StdGen -> (Bool, Bool, Bool)
threeCoins' gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

-- instance (Error e) => Monad (Either e) where
--   return x = Right x
--   Right x >>= f = f x
--   Left err >>= f = Left err
--   fail msg = Left (strMsg msg)

-- liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f m = do
--   x <- m
--   return (f x)

-- fmap :: (Functor m) => (a -> b) -> m a -> m b

-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

-- implementation of <*> from applicatives but from Monad features
-- ap :: (Monad m) => m (a -> b) -> m a -> m b
-- ap mf m = do
--   f <- mf
--   x <- m
--   return (f x)

-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f x y = f <$> x <*> y

-- join :: (Monad m) => m (m a) -> m a
-- join mm = do
--   m <- mm
--   m

joinedMaybes :: Maybe Int
joinedMaybes = do
  m <- Just (Just 8)
  m

-- filter :: (a -> Bool) -> [a] -> [b]
-- filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [b]

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping" ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, chucking it"]
    return False

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x : y : ys) "*" = return ((x * y) : ys)
foldingFunction (x : y : ys) "+" = return ((x + y) : ys)
foldingFunction (x : y : ys) "-" = return ((x - y) : ys)
foldingFunction xs numberString = liftM (: xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r -1),
      (c + 2, r + 1),
      (c - 2, r -1),
      (c - 2, r + 1),
      (c + 1, r -2),
      (c + 1, r + 2),
      (c - 1, r -2),
      (c - 1, r + 2)
      ]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

-- first get list of x moveKnight functions, then use foldr with <=< to compose them together, then take the starting position and feed it to the composition
inMany :: Int -> KnightPos -> [KnightPos]
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

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

readerMonad = do
  let f = (* 5)
  let g = (+ 3)
  print (fmap f g 8)
  let f = (+) <$> (* 2) <*> (+ 10)
  print (f 3)
  print (addStuff 3)
  print (addStuff' 3)

stateMonad = do
  print (stackManip' [5, 8, 2, 1])
  print (runState stackManip [5, 8, 2, 1])
  print (runState stackStuff [9, 0, 2, 1, 0])
  print (runState stackyStack [1, 2, 3])
  print (runState stackyStack [1, 2, 4])
  print (runState threeCoins (mkStdGen 33))

errorMonad = do
  print (Right 100 >>= \x -> return (x + 1) :: Either String Int)
  print (Left "boom" >>= \x -> return (x + 1))
  print (Right 100 >>= \x -> Left "no way!" :: Either String Int)

usefulMonadicFunctions = do
  print (liftM (* 3) (Just 8))
  print (fmap (* 3) (Just 8))
  print (runWriter $ liftM not $ writer (True, "chickpeas"))
  print (runWriter $ fmap not $ writer (True, "chickpeas"))
  print (runState (liftM (+ 100) pop) [1, 2, 3, 4])
  print (runState (fmap (+ 100) pop) [1, 2, 3, 4])
  print ((+) <$> Just 3 <*> Just 5)
  print ((+) <$> Just 3 <*> Nothing)
  print ((+) <$> Right 3 <*> Left "test")
  print ((+) <$> Right 3 <*> Right 2 :: Either String Int)
  print (Just (+ 3) <*> Just 4)
  print (Just (+ 3) `ap` Just 4)
  print ([(+ 1), (+ 2), (+ 3)] <*> [10, 11])
  print ([(+ 1), (+ 2), (+ 3)] `ap` [10, 11])
  print ((+ 3) <$> Just 4)
  print ((+) <$> Just 3 <*> Just 4)
  print (join (Just (Just 9)))
  print (join (Just Nothing :: Maybe (Maybe Int)))
  print (join Nothing :: Maybe Int)
  print (join [[1, 2, 3], [4, 5, 6]])
  print (runWriter $ join (writer (writer (1, "aaa"), "bbb")))
  print (join (Right (Right 9)) :: Either String Int)
  print (join (Right (Left "error")) :: Either String Int)
  print (join (Left "error") :: Either String Int)
  print (runState (join (state $ \s -> (push 10, 1 : 2 : s))) [0, 0, 0])
  -- The lambda here takes a state and puts 2 and 1 onto the stack and presents push 10 as its result. So when this whole thing is flattened with join and then run, it first puts 2 and 1 onto the stack and then push 10 gets carried out, pushing a 10 on to the top.
  -- m >>= f always equals join (fmap f m)
  print (filter (\x -> x < 4) [9, 1, 5, 2, 10, 3])
  print (fst $ runWriter $ filterM keepSmall [9, 1, 5, 2, 10, 3])
  mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9, 1, 5, 2, 10, 3]
  print (powerset [1, 2, 3])
  print (foldl (\acc x -> acc + x) 0 [2, 8, 3, 1])
  print (foldM binSmalls 0 [2, 8, 3, 1])
  print (foldM binSmalls 0 [2, 11, 3, 1])

safeRpnCalculator = do
  print (readMaybe "1" :: Maybe Int)
  print (readMaybe "GO TO HELL" :: Maybe Int)
  print (foldingFunction [3, 2] "*")
  print (foldingFunction [3, 2, 5] "*")
  print (foldingFunction [3, 2] "-")
  print (foldingFunction [] "*")
  print (foldingFunction [] "1")
  print (foldingFunction [] "1 wawawawa")
  print (solveRPN "1 2 * 4 +")
  print (solveRPN "1 2 * 4 + 5 *")
  print (solveRPN "1 2 * 4")
  print (solveRPN "1 8 wharglbllargh")

composingMonadicFunctions = do
  let f = (+ 1) . (* 100)
  print (f 4)
  -- <=< is the same as composition just for monadic functions
  let g = (\x -> return (x + 1)) <=< (\x -> return (x * 100))
  print (Just 4 >>= g)
  let h = foldr (.) id [(+ 1), (* 100), (+ 1)]
  print (h 1)
  print (moveKnight (6, 2))
  print (moveKnight (8, 1))
  print (canReachIn 3 (6, 2) (6, 1))
  print (canReachIn 3 (6, 2) (7, 3))
  print (canReachIn 4 (6, 2) (7, 3))

main = composingMonadicFunctions