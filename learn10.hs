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

data CMaybe a = CNothing | CJust Int a deriving (Show)

-- C stands for counter

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter x) = CJust (counter + 1) (f x)

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

cMaybe = do
  print (CNothing :: CMaybe String)
  print (CJust 0 "Haha")
  print (CJust 100 [1, 2, 3])
  print (fmap (++ "ha") (CJust 0 "ho"))
  print (fmap (++ "he") (fmap (++ "ha") (CJust 0 "ho")))
  print (fmap (++ "blah") CNothing)
  print (fmap id (CJust 0 "haha"))
  print (id (CJust 0 "haha"))
  -- does not obey functor laws, thus not a functor

main = cMaybe