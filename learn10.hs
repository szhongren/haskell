import Data.Char
import Data.List

class Functor' f where -- for things that can be mapped over, f is a type that takes one arg, and is not concrete
-- give me a function that makes a b from an a
-- and a computational context with an a in it
-- and I will return a computational context with a b in it
  fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
  fmap' f (Just a) = Just (f a)
  fmap' f Nothing = Nothing

instance Functor' IO where
  fmap' f action = do
    result <- action
    return (f result)

instance Functor' ((->) r) where
  fmap' f g = (\x -> f (g x))

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

doThing = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

main = doThing