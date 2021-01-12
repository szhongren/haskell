module Shapes
  ( Point (..),
    Shape (..),
    surface,
    nudge,
    baseCircle,
    baseRect,
  )
where

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

data Car = Car
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)

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
