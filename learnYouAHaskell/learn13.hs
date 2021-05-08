import Data.List

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        ( Node
            'L'
            (Node 'N' Empty Empty)
            (Node 'T' Empty Empty)
        )
        ( Node
            'Y'
            (Node 'S' Empty Empty)
            (Node 'A' Empty Empty)
        )
    )
    ( Node
        'L'
        ( Node
            'W'
            (Node 'C' Empty Empty)
            (Node 'R' Empty Empty)
        )
        ( Node
            'A'
            (Node 'A' Empty Empty)
            (Node 'C' Empty Empty)
        )
    )

changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)

data Direction = L | R deriving (Show)

type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' (L : ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R : ds) (Node x l r) = Node x l (changeToP' ds r)
changeToP' [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

type Breadcrumbs' = [Direction]

goLeft'' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goLeft'' (Node _ l _, bs) = (l, L : bs)

goRight'' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goRight'' (Node _ _ r, bs) = (r, R : bs)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r : bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l : bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r : bs) = (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

data List a = EmptyList | Cons a (List a) deriving (Show, Read, Eq, Ord)

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBackward :: ListZipper a -> ListZipper a
goBackward (xs, b : bs) = (b : xs, bs)

type Name = String

type Data = String

data FSItem = File String String | Folder String [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder
    "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa",
      File "pope_time.avi" "god bless",
      Folder
        "pics"
        [ File "ape_throwing_up.jpg" "bleargh",
          File "watermelon_smash.gif" "smash!!",
          File "skull_man(scary).bmp" "Yikes!"
        ],
      File "dijon_poupon.doc" "best mustard",
      Folder
        "programs"
        [ File "fartwizard.exe" "10gotofart",
          File "owl_bandit.dmg" "mov eax, h00t",
          File "not_a_virus.exe" "really not a virus",
          Folder
            "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)",
              File "random.hs" "main = print 4"
            ]
        ]
    ]

-- 2 [FSItem] because we want to keep files before and after the current item
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item : rs) = break (nameIs name) items
   in (item, FSCrumb folderName ls rs : bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File folderName _) = name == folderName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName (item : items), bs)

takingAWalk = do
  let newTree = changeToP' [R, L] freeTree
  print (elemAt [R, L] newTree)
  print (goLeft'' (goRight'' (freeTree, [])))
  print ((freeTree, []) -: goRight'' -: goLeft'')
  print (modify (\_ -> 'P') (goRight (goLeft (freeTree, []))))
  let newFocus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')
  print (newFocus)
  print (modify (\_ -> 'X') (goUp newFocus))
  print (newFocus -: goUp -: modify (\_ -> 'X'))
  let farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft
  let attachedTree = farLeft -: attach (Node 'Z' Empty Empty)
  print (attachedTree)
  print (attachedTree -: topMost)
  let xs = [1, 2, 3, 4]
  print (goForward (xs, []))
  print (goForward ([2, 3, 4], [1]))
  print (goForward ([3, 4], [2, 1]))
  print (goBackward ([4], [3, 2, 1]))
  let newFsFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
  print (newFsFocus)
  print (fst newFsFocus)
  let newFsFocus2 = newFsFocus -: fsUp -: fsTo "watermelon_smash.gif"
  print (newFsFocus2)
  print (fst newFsFocus2)
  print ((myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp)
  print ((myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp)

main = takingAWalk