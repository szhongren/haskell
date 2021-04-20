import Data.Char (chr, digitToInt, generalCategory, intToDigit, isAlphaNum, isSpace, isUpper, ord)
import Data.Function (on)
import Data.List
  ( delete,
    elemIndex,
    elemIndices,
    find,
    findIndex,
    findIndices,
    group,
    groupBy,
    inits,
    insert,
    intercalate,
    intersect,
    intersperse,
    isInfixOf,
    isPrefixOf,
    isSuffixOf,
    nub,
    partition,
    sort,
    sortBy,
    tails,
    transpose,
    union,
    zip4,
    (\\),
  )
import qualified Data.Map as Map
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
   in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
   in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k, v) : xs) = if key == k then Just v else findKey' key xs

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
-- partially apply this function so it takes one more argument like foldr
findKey'' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

main = do
  print (intersperse '.' "monkey")
  print (intersperse 0 [1, 2, 3, 4, 5, 6])
  print (intercalate " " ["Hey", "there", "guys"])
  print (intercalate [0, 0, 0] [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
  print (transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
  print (transpose ["hey", "there", "guys"])
  print (map sum $ transpose [[0, 3, 5, 9], [10, 0, 0, 9], [8, 5, 1, -1]])
  print (concat ["foo", "bar", "car"])
  print (concat [[3, 4, 5], [2, 3, 4], [2, 1, 1]])
  print (concatMap (replicate 5) [1 .. 4])
  print (and $ map (> 4) [5 .. 8])
  print (and $ map (== 4) [4, 4, 4, 3, 4])
  print (or $ map (== 4) [2, 3, 4, 5, 6, 1])
  print (or $ map (> 4) [6, 9, 10])
  print (any (== 4) [2, 3, 5, 6, 1, 4])
  print (all (> 4) [6, 9, 10])
  print (all (`elem` ['A' .. 'Z']) "HEYGUYSwhatsup")
  print (any (`elem` ['A' .. 'Z']) "HEYGUYSwhatsup")
  print (take 10 $ iterate (* 2) 1)
  print (take 3 $ iterate (++ "haha") "haha")
  print (splitAt 3 "heyman")
  print (splitAt 100 "heyman")
  print (splitAt (-3) "heyman")
  print (let (a, b) = splitAt 3 "foobar" in b ++ a)
  print (takeWhile (> 3) [6, 5, 4, 3, 2, 1, 2, 3, 4, 5, 4, 3, 2, 1])
  print (takeWhile (/= ' ') "This is a sentence")
  print (sum $ takeWhile (< 10000) $ map (^ 3) [1 ..])
  print (dropWhile (/= ' ') "This is a sentence")
  print (dropWhile (< 3) [1, 2, 2, 2, 3, 4, 5, 4, 3, 2, 1])
  print (let (fw, rest) = span (/= ' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest)
  print (break (== 4) [1 .. 7])
  print (span (/= 4) [1 .. 7])
  print (sort [8, 5, 3, 2, 1, 6, 4, 2])
  print (sort "This will be sorted soon")
  print (group [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 2, 2, 2, 5, 6, 7])
  print (map (\l@(x : xs) -> (x, length l)) . group . sort $ [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 2, 2, 2, 5, 6, 7])
  print (inits "w00t")
  print (tails "w00t")
  print (let w = "w00t" in zip (inits w) (tails w))
  print ("cat" `isInfixOf` "im a cat burglar")
  print ("Cat" `isInfixOf` "im a cat burglar")
  print ("cats" `isInfixOf` "im a cat burglar")
  print ("hey" `isPrefixOf` "hey there!")
  print ("hey" `isPrefixOf` "oh hey there!")
  print ("there!" `isSuffixOf` "oh hey there!")
  print ("there!" `isSuffixOf` "oh hey there!")
  print (partition (`elem` ['A' .. 'Z']) "BOBsidneyMORGANeddy")
  print (partition (> 3) [1, 3, 5, 6, 3, 2, 1, 0, 3, 7])
  print (find (> 4) [1, 2, 3, 4, 5, 6])
  print (find (> 9) [1, 2, 3, 4, 5, 6])
  print (4 `elemIndex` [1, 2, 3, 4, 5, 6])
  print (10 `elemIndex` [1, 2, 3, 4, 5, 6])
  print (' ' `elemIndices` "Where are the spaces?")
  print (findIndex (== 4) [5, 3, 2, 1, 6, 4])
  print (findIndex (== 7) [5, 3, 2, 1, 6, 4])
  print (findIndices (`elem` ['A' .. 'Z']) "Where Are The Caps?")
  print (zipWith3 (\x y z -> x + y + z) [1, 2, 3] [4, 5, 2, 2] [2, 2, 3])
  print (zip4 [2, 3, 3] [2, 2, 2] [5, 5, 3] [2, 2, 2])
  print (lines "first line\nsecond line\nthird line")
  print (unlines ["first line", "second line", "third line"])
  print (words "hey these are the words in this sentence")
  print (words "hey these           are    the words in this\nsentence")
  print (unwords ["hey", "there", "mate"])
  print (nub [1, 2, 3, 4, 3, 2, 1, 2, 3, 4, 3, 2, 1])
  print (nub "Lots of words and stuff")
  print (delete 'h' "hey there ghang!")
  print (delete 'h' . delete 'h' $ "hey there ghang!")
  print (delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!")
  print ([1 .. 10] \\ [2, 5, 9])
  print ("Im a big baby" \\ "big")
  print ("hey man" `union` "man what's up")
  print ([1 .. 7] `union` [5 .. 10])
  print ([1 .. 7] `intersect` [5 .. 10])
  print (insert 4 [3, 5, 1, 2, 8, 2])
  print (insert 4 [1, 3, 4, 4, 1])
  print (insert 4 [1, 2, 3, 5, 6, 7])
  print (insert 'g' $ ['a' .. 'f'] ++ ['h' .. 'z'])
  print (insert 3 [1, 2, 4, 3, 2, 1])
  print (groupBy (\x y -> (x > 0) == (y > 0)) [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3])
  print (groupBy ((==) `on` (> 0)) [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3])
  print (sortBy (compare `on` length) [[5, 4, 5, 4, 4], [1, 2, 3], [3, 5, 4, 3], [], [2], [2, 2]])
  print (all isAlphaNum "bobby283")
  print (all isAlphaNum "Eddy the fish!")
  print (words "hey guys its me")
  print (groupBy ((==) `on` isSpace) "hey guys its me")
  print (filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me")
  print (generalCategory ' ')
  print (generalCategory 'A')
  print (generalCategory 'a')
  print (generalCategory '.')
  print (generalCategory '9')
  print (map generalCategory " \t\nA9?|")
  print (map digitToInt "34538")
  print (map digitToInt "FF85AB")
  print (intToDigit 15)
  print (intToDigit 5)
  print (ord 'a')
  print (chr 97)
  print (map ord "abcdefgh")
  print (encode 3 "Heeeeey")
  print (encode 4 "Heeeeey")
  print (encode 1 "abcd")
  print (encode 5 "Merry Christmas! Ho Ho Ho!")
  print (decode 3 (encode 3 "I'm a little teapot"))
  print (decode 5 . encode 5 $ "This is a sentence")
  -- association list
  let phoneBook = [("betty", "555-2938"), ("bonnie", "452-2928"), ("patsy", "493-2928"), ("lucille", "205-2928"), ("wendy", "939-8282"), ("penny", "853-2492")]
  print (findKey "penny" phoneBook)
  print (findKey "betty" phoneBook)
  -- print (findKey "wilma" phoneBook)
  print (findKey' "penny" phoneBook)
  print (findKey' "betty" phoneBook)
  print (findKey' "wilma" phoneBook)
  print (findKey'' "penny" phoneBook)
  print (findKey'' "betty" phoneBook)
  print (findKey'' "wilma" phoneBook)
  print (Map.fromList [("betty", "555-2938"), ("bonnie", "452-2928"), ("lucille", "205-2928")])
  print (Map.fromList [(1, 2), (3, 4), (3, 2), (5, 5)])
  print (Map.insert 3 100 Map.empty)
  print (Map.insert 5 600 (Map.insert 4 200 (Map.insert 3 100 Map.empty)))
  print (Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty)
  print (Map.null Map.empty)
  print (Map.null $ Map.fromList [(2, 3), (5, 5)])
  print (Map.size Map.empty)
  print (Map.size $ Map.fromList [(2, 3), (3, 3), (4, 2), (5, 4), (6, 4)])
  print (Map.singleton 3 9)
  print (Map.insert 5 9 $ Map.singleton 3 9)
  print (Map.member 3 $ Map.fromList [(3, 6), (4, 3), (6, 9)])
  print (Map.member 3 $ Map.fromList [(2, 5), (4, 5)])
  print (Map.map (* 100) $ Map.fromList [(1, 1), (2, 4), (3, 9)])
  print (Map.filter isUpper $ Map.fromList [(1, 'a'), (2, 'A'), (3, 'b'), (4, 'B')])
  print (Map.toList . Map.insert 9 2 $ Map.singleton 4 3)
  let phoneBook2 = [("betty", "555-2938"), ("betty", "342-2492"), ("bonnie", "452-2928"), ("patsy", "493-2928"), ("patsy", "943-2929"), ("patsy", "827-9162"), ("lucille", "205-2928"), ("wendy", "939-8282"), ("penny", "853-2492"), ("penny", "555-2111")]
  print (Map.lookup "patsy" $ phoneBookToMap phoneBook2)
  print (Map.lookup "wendy" $ phoneBookToMap phoneBook2)
  print (Map.lookup "betty" $ phoneBookToMap phoneBook2)
  print (Map.fromListWith max [(2, 3), (2, 5), (2, 100), (3, 29), (3, 22), (3, 11), (4, 22), (4, 15)])
  print (Map.fromListWith (+) [(2, 3), (2, 5), (2, 100), (3, 29), (3, 22), (3, 11), (4, 22), (4, 15)])
  print (Map.insertWith (+) 3 100 $ Map.fromList [(3, 4), (5, 103), (6, 339)])
  let text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
  let text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
  let set1 = Set.fromList text1
  let set2 = Set.fromList text2
  print (set1)
  print (set2)
  print (Set.intersection set1 set2)
  print (Set.difference set1 set2)
  print (Set.difference set2 set1)
  print (Set.union set1 set2)
  print (Set.null Set.empty)
  print (Set.null $ Set.fromList [3, 4, 5, 5, 4, 3])
  print (Set.size $ Set.fromList [3, 4, 5, 3, 4, 5])
  print (Set.singleton 9)
  print (Set.insert 4 $ Set.fromList [9, 3, 8, 1])
  print (Set.insert 8 $ Set.fromList [5 .. 10])
  print (Set.delete 4 $ Set.fromList [3, 4, 5, 4, 3, 4, 5])
  print (Set.fromList [2, 3, 4] `Set.isSubsetOf` Set.fromList [1, 2, 3, 4, 5])
  print (Set.fromList [1, 2, 3, 4, 5] `Set.isSubsetOf` Set.fromList [1, 2, 3, 4, 5])
  print (Set.fromList [1, 2, 3, 4, 5] `Set.isProperSubsetOf` Set.fromList [1, 2, 3, 4, 5])
  print (Set.fromList [2, 3, 4, 8] `Set.isSubsetOf` Set.fromList [1, 2, 3, 4, 5])
  print (Set.filter odd $ Set.fromList [3, 4, 5, 6, 7, 2, 3, 4])
  print (Set.map (+ 1) $ Set.fromList [3, 4, 5, 6, 7, 2, 3, 4])