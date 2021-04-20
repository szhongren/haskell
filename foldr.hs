-- taken from this page http://web.jaguarpaw.co.uk/~tom/blog/posts/2012-11-04-what-is-foldr-made-of.html

func1 :: (a -> b) -> [a] -> [b]
func1 _ [] = []
func1 f (x : xs) = f x : func1 f xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

func2 :: (a -> b -> b) -> b -> [a] -> b
func2 _ z [] = z
func2 g z (x : xs) = g x (func2 g z xs)

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' g z (x : xs) = g x (foldr' g z xs)

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr' ((:) . f) []

-- map'' f [] == foldr ((:) . f) [] []
--            == []
-- map'' f (x:xs) == foldr ((:) . f) [] (x:xs)
--                == ((:) . f) x (foldr ((:) . f) [] xs)
--                == f x : (foldr ((:) . f) [] xs)
--                = f x : map'' f xs

compose' :: [a -> a] -> a -> a
compose' [] = id
compose' (f : fs) = f . compose' fs

foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' g z xs = compose' (map g xs) z

-- foldr'' g z [] == compose' [] z == id z == z
-- foldr'' g z (x:xs) == compose' (map g (x:xs)) z
--                    == compose' (g x : map g xs) z
--                    == (g x . compose' (map g xs)) z
--                    == g x (compose' (map g xs) z)
--                    == g x (foldr'' g z xs)

compose'' :: [a -> a] -> a -> a
compose'' fs = foldr (.) id fs

-- compose'' [] == foldr (.) id [] == id
-- compose'' (f:fs) == foldr (.) id (f:fs)
--                  == (.) f (foldr (.) id fs)
--                  == f . (foldr (.) id fs)
--                  = f . compose'' fs