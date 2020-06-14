doubleMe x = x + x
doubleUs x y = (+) ((*) x 2) ((*) y 2)
doubleUs' x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
conanO'Brien = "it's a-me, Conan O'Brien"
boomBang xs = [ if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x ]
length' xs = sum [ 1 | _ <- xs ]

main = do
    print (2 + 6)
    print ((50 * 100) - 4999)
    print (50 * (100 - 4999))

    print (True && False)
    print (True && True)
    print (False || True)
    print (not False)
    print (not (True && True))
    print (5 == 5)
    print (1 == 0)
    print (5 /= 5)
    print ("Hello" == "Hello")

    print (succ 8)
    print (min 9 10)
    print (min 9.98 10)
    print (92 `div` 10)

    print (doubleMe 123)
    print (doubleUs 123 88.78)
    print (doubleSmallNumber 12)

    let lostNumbers = [4, 8, 15, 16, 23, 42]

    print (conanO'Brien)

    print ([1, 2, 3, 4] ++ [9, 10, 11, 12])
    print ("hello" ++ " " ++ "world")
    print (['h', 'i'] ++ [' '] ++ ['t', 'h', 'e', 'r', 'e'])

    print ('A' : " smol cat")

    print ("Hello" !! 2)
    print ([9.4, 33.2, 96.2, 11.2, 23.25] !! 1)

    print ([3, 2, 1] > [2, 1, 0])
    print ([3, 2, 1] > [2, 10, 100])
    print ([3, 4, 2] > [3, 4])
    print ([3, 4, 2] > [2, 4])
    print ([3, 4, 2] == [3, 4, 2])

    print (head "this")
    print (tail "place")
    print (last "right")
    print (init "there")

    print (length [1, 9, 123])
    print (null [])
    print (reverse [1, 3, 5])

    print (take 3 [1, 0, 123, 23, 89])
    print (take 10 "zygote")

    print (drop 2 "really")
    print (drop 10 "hell")

    print (maximum "abcde")
    print (minimum [3, 89, -1])

    print (sum [5, 2, 1, 6, 3, 2, 5, 7])
    print (product [6, 2, 1, 2])

    print (5 `elem` [1, 3, 5, 7, 9])

    -- texas ranges
    print [1 .. 10]
    print ['a' .. 'z']
    print ['K' .. 'B']
    print [2, 4 .. 30]
    print [3, 6 .. 20]
    print [0.1, 0.2 .. 5]
    print (take 50 (cycle "abc"))
    print (take 12 (repeat 32))

    -- list comprehension
    print [ x * 2 | x <- [1 .. 12] ]
    print [ x * 2 | x <- [1 .. 10], x * 2 >= 12 ]
    print [ x | x <- [50 .. 100], x `mod` 7 == 3 ]
    print (boomBang [7 .. 13])
    print [ x | x <- [10 .. 20], x /= 13, x /= 15 ]
    print [ x * y | x <- [2, 5, 10], y <- [8, 10, 11] ]
    print [ x * y | x <- [2, 5, 10], y <- [8, 10, 11], x * y > 30 ]

    print (length' [1 .. 4])

    -- tuples
    print (fst (1, 2))
    print (snd (1, 2))
    print (zip [1 .. 5] [5 .. 12])
    print
        [ (a, b, c)
        | c <- [1 .. 10]
        , b <- [1 .. c]
        , a <- [1 .. b]
        , a ^ 2 + b ^ 2 == c ^ 2
        ]
