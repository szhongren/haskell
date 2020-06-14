countdown :: (Integral a) => a -> [a]
countdown 0 = []
countdown x = x : countdown (pred x)

insertR :: (Eq a) => a -> a -> [a] -> [a]
insertR a b xs | null xs   = []
               | a == cons = cons : b : insertR a b cdr
               | otherwise = cons : insertR a b cdr
    where (cons : cdr) = xs

remv1st :: (Eq a) => a -> [a] -> [a]
remv1st a xs | null xs   = []
             | a == cons = cdr
             | otherwise = cons : remv1st a cdr
    where (cons : cdr) = xs

indexOf :: (Eq a) => a -> [a] -> Integer
indexOf a xs | null xs   = -1
             | a == cons = 0
             | otherwise = succ (indexOf a cdr)
    where (cons : cdr) = xs

main = do
    print (countdown 5)
    print (countdown 3)

    print (insertR 'x' 'y' "xzzxyx")
    print (remv1st 'x' "xyzx")
    print (remv1st 'y' "xyzyx")

    print (indexOf 'y' "abcdey")
    print (indexOf 'b' "abcdey")
