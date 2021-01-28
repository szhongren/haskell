module Shapes
  ( Point (..),
    Shape (..),
    surface,
    nudge,
    baseCircle,
    baseRect,
  )
where

import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- data Person = Person String String Int Float String String deriving (Show)
-- firstName :: Person -> String
-- firstName (Person firstName _ _ _ _ _) = firstName

-- lastName :: Person -> String
-- lastName (Person _ lastName _ _ _ _) = lastName

-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age

-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height

-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ phoneNumber _) = phoneNumber

-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show, Eq, Read) -- read is for constructing a Person from a string

data Car = Car
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)

-- data Car a b c = Car
--   { company :: a,
--     model :: b,
--     year :: c
--   }
--   deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

type PhoneNumber = String

type Name = String

type PhoneBook = [(Name, PhoneNumber)]

type AssocList k v = [(k, v)]

type IntMap v = Map.Map Int v

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist"
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

infixr 5 :-: -- declare the fixity of this operator, adn it is right-associative

data List' a = Empty' | a :-: (List' a) deriving (Show, Read, Eq, Ord)

infixr 5 .++

(.++) :: List' a -> List' a -> List' a -- type signature
Empty' .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys) -- pattern match on first list, and prepend to recursive call

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where -- declare TrafficLight as an instance of Eq
  Red == Red = True -- only need to define for == because the definition of Eq uses == and /= in a mutually recursive way
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

class Functor' f where -- for things that can be mapped over, f is a type that takes one arg, and is not concrete
  fmap' :: (a -> b) -> f a -> f b

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

class Tofu t where
  tofu :: j a -> t a j

-- Frank has a kind of * -> (* -> *) -> *
-- a is *
-- b is a (* -> *)
-- returns *
data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
  tofu x = Frank x

data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

main = do
  --   print (surface $ Circle 10 20 10)
  --   print (surface $ Rectangle 0 0 100 100)
  --   print (Circle 10 20 5)
  --   print (Rectangle 50 230 60 90)
  --   print (map (Circle 10 20) [4, 5, 6, 6])
  print (surface (Rectangle (Point 0 0) (Point 100 100)))
  print (surface (Circle (Point 0 0) 24))
  print (nudge (Circle (Point 34 34) 10) 5 10)
  print (nudge (baseRect 40 100) 60 23)
  let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
  print (guy)
  print (firstName guy)
  print (height guy)
  print (flavor guy)
  print (Car {company = "Ford", model = "Mustang", year = 1967})
  print (Just "Haha")
  print (Just 84)
  print (Just 10 :: Maybe Double)
  print (Vector 3 5 8 `vplus` Vector 9 2 8)
  print (Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3)
  print (Vector 3 9 7 `vectMult` 10)
  print (Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0)
  print (Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4))
  let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43, height = 1.8, phoneNumber = "1234567890", flavor = "Vanilla"}
  let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41, height = 1.8, phoneNumber = "1234567890", flavor = "Vanilla"}
  let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44, height = 1.8, phoneNumber = "1234567890", flavor = "Vanilla"}
  print (mca == adRock)
  print (mikeD == adRock)
  print (mikeD == mikeD)
  let beastieBoys = [mca, adRock, mikeD]
  print (mikeD `elem` beastieBoys)
  print (True `compare` False)
  print (True > False)
  print (True < False)
  print (Nothing < Just 100)
  print (Nothing > Just (-49999))
  print (Just 3 `compare` Just 2)
  print (Just 100 > Just 50)
  print (Wednesday)
  print (show Wednesday)
  print (read "Saturday" :: Day)
  print (Saturday == Sunday)
  print (Saturday == Saturday)
  print (Saturday > Friday)
  print (Monday `compare` Wednesday)
  print (minBound :: Day)
  print (maxBound :: Day)
  print (succ Monday)
  print (pred Saturday)
  print ([Thursday .. Sunday])
  let lockers = Map.fromList [(100, (Taken, "ZD39I")), (101, (Free, "JAH3I")), (103, (Free, "IQSA9")), (105, (Free, "QOTSA")), (109, (Taken, "893JJ")), (110, (Taken, "99292"))]
  print (lockerLookup 101 lockers)
  print (lockerLookup 100 lockers)
  print (lockerLookup 102 lockers)
  print (lockerLookup 110 lockers)
  print (lockerLookup 105 lockers)
  print (5 `Cons` Empty)
  print (4 `Cons` (5 `Cons` Empty))
  print (3 `Cons` (4 `Cons` (5 `Cons` Empty)))
  print (3 :-: 4 :-: 5 :-: Empty')
  let a = 3 :-: 4 :-: 5 :-: Empty'
  print (100 :-: a)
  let b = 6 :-: 7 :-: Empty'
  print (a .++ b)
  let nums = [8, 6, 4, 1, 7, 3, 5]
  let numsTree = foldr treeInsert EmptyTree nums
  print (numsTree)
  print (8 `treeElem` numsTree)
  print (100 `treeElem` numsTree)
  print (1 `treeElem` numsTree)
  print (10 `treeElem` numsTree)
  print (Red == Red)
  print (Red == Yellow)
  print (Red `elem` [Red, Yellow, Green])
  print ([Red, Yellow, Green])
  print (yesno $ length [])
  print (yesno "haha")
  print (yesno "")
  print (yesno $ Just 0)
  print (yesno True)
  print (yesno EmptyTree)
  print (yesno [])
  print (yesno [0, 0, 0])
  print (yesnoIf [] "YEAH!" "NO!")
  print (yesnoIf [2, 3, 4] "YEAH!" "NO!")
  print (yesnoIf True "YEAH!" "NO!")
  print (yesnoIf (Just 500) "YEAH!" "NO!")
  print (yesnoIf Nothing "YEAH!" "NO!")
  print (fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious."))
  print (fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing)
  print (fmap (* 2) (Just 200))
  print (fmap (* 2) Nothing)
  print (fmap (* 2) EmptyTree)
  print (fmap (* 4) (foldr treeInsert EmptyTree [5, 7, 3, 2, 1, 7]))
  print (tofu (Just 'a') :: Frank Char Maybe)
  print (tofu ["HELLO"] :: Frank [Char] [])