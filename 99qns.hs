myLast :: [a] -> a
myLast []       = error "can't get last of an empty list"
myLast [last  ] = last
myLast (_ : xs) = myLast xs

myButLast :: [a] -> a
myButLast []               = error "can't get penultimate of an empty list"
myButLast [_]              = error "can't get penultimate of a singleton list"
myButLast [penultimate, _] = penultimate
myButLast (_ : xs)         = myButLast xs

main = do
    print (myLast [1, 2, 3, 4] == 4)
    print (myLast ['x', 'y', 'z'] == 'z')
    print (myButLast [1, 2, 3, 4] == 3)
    print (myButLast ['a' .. 'z'] == 'y')
