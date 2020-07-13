lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "one"
sayMe 2 = "two"
sayMe 3 = "three"
sayMe 4 = "four"
sayMe 5 = "five"
sayMe x = "not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = factorial (pred n) * n

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' []      = error "can't get head of an empty list"
-- match head, and tail
head' (x : _) = x

tail' :: [a] -> [a]
tail' []       = error "can't get tail of an empty list"
tail' (_ : xs) = xs

tell :: (Show a) => [a] -> String
tell []           = "empty list"
tell (x     : []) = "one element: " ++ show x
tell (x : y : []) = "two elements: " ++ show x ++ ", " ++ show y
tell (x : y : _ ) = "long list: " ++ show x ++ ", " ++ show y ++ ", etc."

length' :: (Num b) => [a] -> b
length' []       = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' []       = 0
sum' (x : xs) = x + sum' xs

capital :: String -> String
capital ""           = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards are like if statements, but easier to read and work nicely with patterns
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi | bmi <= 18.5 = "you are underweight"
            | bmi <= 25.0 = "you are normal"
            | bmi <= 30.0 = "you are overweight"
            | otherwise   = "you are off the scale"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height | weight / height ^ 2 <= 18.5 = "you are underweight"
                       | weight / height ^ 2 <= 25.0 = "you are normal"
                       | weight / height ^ 2 <= 30.0 = "you are overweight"
                       | otherwise                   = "you are off the scale"

max' :: (Ord a) => a -> a -> a
max' a b | a > b     = a
         | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b | a > b     = GT
               | a == b    = EQ
               | otherwise = LT

-- where
bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height | bmi <= skinny = "you are underweight"
                        | bmi <= normal = "you are normal"
                        | bmi <= fat    = "you are overweight"
                        | otherwise     = "you are off the scale"
  -- any declarations in where are visible in the function








  where
    bmi                   = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstName
    (l : _) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs ]
    where bmi weight height = weight / height ^ 2

-- let
-- let bindings are also expressions, so we can put these anywhere we can put any other expression to be evaluated
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^ 2
    in  sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2 ]

-- case
head'' :: [a] -> a
head'' xs = case xs of
    []      -> error "No head"
    (x : _) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
    []  -> "empty."
    [x] -> "a singleton list."
    xs  -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where
    what []  = "empty."
    what [x] = "a singleton list."
    what xs  = "a longer list."

main = do
    print [ lucky x | x <- [1 .. 10] ]
    print [ sayMe x | x <- [1 .. 10] ]
    print [ factorial x | x <- [1 .. 10] ]
    print
        [ (x, y)
        | (x, y) <-
            [ addVectors (x1, y1) (x2, y2)
            | x1 <- [1 .. 3]
            , y1 <- [1 .. 3]
            , x2 <- [1 .. 3]
            , y2 <- [1 .. 3]
            ]
        , y + x == 6
        ]
    print (head' "ATL")
    print (tail' [1, 3, 5])
    print (tell "AB")
    print (tell "ABCDE")
    print (length' [1, 3, 5, 7])
    print (sum' [1, 3, 5, 7])
    print (capital "Dracula")

    -- guards
    print (bmiTell 23)
    print (bmiTell' 85 1.9)
    print (max' 1 2)
    print (compare' 1 2)

    -- where
    print (bmiTell'' 85 1.9)
    print (initials "Darth" "Vader")
    print (calcBmis [(85, 1.9), (89, 1.9)])

    -- let
    print (cylinder 4 5)
    print (4 * (let a = 9 in a + 1) + 2)
    print [let square x = x * x in (square 5, square 3, square 2)]
    print
        ( let a = 100
              b = 200
              c = 300
          in  a * b * c
        , let foo = "Hey "
              bar = "there!"
          in  foo ++ bar
        )
    print ((let (a, b, c) = (1, 2, 3) in a + b + c) * 100)
    print (calcBmis' [(85, 1.9), (89, 1.9)])

    -- case
    print (describeList [])
    print (describeList' [])
    print (describeList [1])
    print (describeList' [1])
    print (describeList [1, 2])
    print (describeList' [1, 2])
