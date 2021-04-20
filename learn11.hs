import Data.Char

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--   (<$) :: a -> f b -> f a
--   (<$) = fmap . const

-- Functor laws
-- 1. fmap id = id
-- 2. fmap (g . h) = (fmap g) . (fmap h)

-- class Functor f => Applicative f where
--   pure :: a -> f a
--   infixl 4 <*>, *>, <*
--   (<*>) :: f (a -> b) -> f a -> f b
--   (*>) :: f a -> f b -> f b
--   a1 *> a2 = (id <$ a1) <*> a2
--   (<*) :: f a -> f b -> f a
--   (<*) = liftA2 const

-- Applicative laws
-- 1. pure id <*> v = v
-- 2. pure f <*> pure x = pure (f x)
-- 3. u <*> pure y = pure ($ y) <*> u
-- 4. u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

-- class Applicative m => Monad m where
--   return :: a -> m a -- same as pure
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   m >> n = m >>= \_ -> n
--   fail :: String -> m a
--   fail msg = error msg

-- Monad laws
-- 1. return a >>= k = k a
-- 2. m >>= return = m
-- 3. m >>= (\x -> k x >>= h) = (m >>= k) >>= h

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- instance Monad Maybe where
--   return x = Just x
--   Nothing >>= f = Nothing
--   Just x >>= f = f x
--   fail _ = Nothing

type Birds = Int

type Pole = (Birds, Birds)

landLeft' :: Birds -> Pole -> Pole
landLeft' n (left, right) = (left + n, right)

landRight' :: Birds -> Pole -> Pole
landRight' n (left, right) = (left, right + n)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

x -: f = f x

foo :: Maybe String
foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

marySue :: Maybe Bool
marySue = Just 9 >>= (\x -> Just (x > 8))

marySue' :: Maybe Bool
marySue' = do
  x <- Just 9
  Just (x > 8)

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

routine' :: Maybe Pole
routine' =
  case Just (0, 0) of
    Nothing -> Nothing
    Just start -> case landLeft 2 start of
      Nothing -> Nothing
      Just first -> case landRight 2 first of
        Nothing -> Nothing
        Just second -> landLeft 1 second

routine'' :: Maybe Pole
routine'' = do
  start <- return (0, 0)
  first <- landLeft 2 start
  Nothing -- this is the same as >> Nothing
  second <- landRight 2 first
  landLeft 1 second

justH :: Maybe Char 
justH = do
  (x:xs)  <- Just "hello"
  return x

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

applicativeRecap = do
  print ((*) <$> Just 2 <*> Just 8)
  print ((++) <$> Just "klingon" <*> Nothing)
  print ((-) <$> [3, 4] <*> [1, 2, 3])

maybeMonad = do
  print (fmap (++ "!") (Just "wisdom"))
  print ((++ "!") <$> (Just "wisdom"))
  print (fmap (++ "!") Nothing)
  print (Just (+ 3) <*> Just 3)
  print ((Nothing :: Maybe (a -> a)) <*> Just "greed")
  print (Just ord <*> Nothing)
  print (max <$> Just 3 <*> Just 6)
  print (max <$> Just 3 <*> Nothing)
  print ((\x -> Just (x + 1)) 1)
  print ((\x -> Just (x + 1)) 100)
  print (Just 3 `applyMaybe` \x -> Just (x + 1))
  print (Just "smile" `applyMaybe` \x -> Just (x ++ " :)"))
  print (Nothing `applyMaybe` \x -> Just (x + 1))
  print (Nothing `applyMaybe` \x -> Just (x ++ " :)"))
  print (Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing)
  print (Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing)
  print (return "WHAT" :: Maybe String)
  print (Just 9 >>= \x -> return (x * 10))
  print (Nothing >>= \x -> return (x * 10))

pierreAndBirds = do
  print (landLeft' 2 (0, 0))
  print (landLeft' 1 (1, 2))
  print (landLeft' (-1) (1, 2))
  print (landLeft' 2 (landRight' 1 (landLeft' 1 (0, 0))))
  print (100 -: (* 3))
  print (True -: not)
  print ((0, 0) -: landLeft' 2)
  print ((0, 0) -: landLeft' 1 -: landRight' 1 -: landLeft' 2)
  print (landLeft' 10 (0, 3))
  print ((0, 0) -: landLeft' 1 -: landRight' 4 -: landLeft' (-1) -: landRight' (-2))
  print (landLeft 2 (0, 0))
  print (landLeft 10 (0, 3))
  print (landRight 1 (0, 0) >>= landLeft 2)
  print (Nothing >>= landLeft 2)
  print (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2)
  print (return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2))
  print (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1)
  print (Nothing >> Just 3)
  print (Just 3 >> Just 4)
  print (Just 3 >> Nothing :: Maybe Int)
  print (return (0, 0) >>= landLeft 1 >> Nothing >>= landRight 1)

doNotation = do
  print (Just 3 >>= (\x -> Just (show x ++ "!")))
  print (Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))))
  print (let x = 3; y = "!" in Just (show x ++ y))
  print ((Nothing :: Maybe Int) >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))))
  print (Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y))))
  print (Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing :: Maybe String)))
  print (foo)
  print (foo')
  print (marySue)
  print (marySue')
  print (routine)
  print (routine')
  print (routine'')
  print (justH)
  print (wopwop)

main = doNotation