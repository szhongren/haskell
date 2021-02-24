import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.IO

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = do
  putChar x
  putStr' xs

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
   in result

respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") (lines contents))
  where
    isPalindrome xs = xs == reverse xs

respondPalindromes' = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") . lines
  where
    isPalindrome xs = xs == reverse xs

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result

getName = do
  putStrLn "Hello, what is your name?"
  name <- getLine -- perform IO action getLine and bind to name
  putStrLn ("Hey " ++ name ++ ", you rock!")

capsName = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  -- <- for binding IO actions to names, let for binding expressions to names
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

revWords = do
  line <- getLine
  if null line
    then return () -- makes an IO action out of a pure value, does not end this function
    else do
      -- do will result in the same value as return does, an IO action
      putStrLn $ reverseWords line
      main

ioActions = do
  a <- return "hell"
  b <- return "yeah!" -- return is opposite to <-, puts a value in a box, here is is then taken out of the box by <-
  putStrLn $ a ++ " " ++ b
  putStr "Hey, "
  putStr "I'm "
  putStrLn "Andy!"
  putChar 't'
  putChar 'e'
  putChar 'h'
  print True
  print 2
  print "haha"
  print 3.2
  print [3, 4, 3]

eatTilSpace = do
  c <- getChar
  if c /= ' '
    then do
      putChar c
      main
    else return ()

eatTilSpace' = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main

sequencePrint = do
  a <- getLine
  b <- getLine
  c <- getLine
  print [a, b, c]

sequencePrint' = do
  rs <- sequence [getLine, getLine, getLine] -- takes a list of IO actions and performs those one after the other, and binds to variable
  print rs

mapAndSequence = do
  sequence (map print [1 .. 5])
  mapM print [1 .. 3]
  mapM_ print [1 .. 3] -- don't care about what the results of the sequenced IO Actions

capsForever = forever $ do
  putStr "Give me some input: "
  l <- getLine
  putStrLn $ map toUpper l

forMDemo = do
  colors <- -- put getLine values in colors
    forM -- basically the same as mapM, but args reversed
      [1, 2, 3, 4]
      ( \a -> do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          -- color <- getLine
          -- return color
          getLine
      )
  putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
  mapM putStrLn colors

capsPipe = do
  contents <- getContents
  putStr (map toUpper contents)

shortLinesOnlyPipe = do
  contents <- getContents
  putStr (shortLinesOnly contents)

shortLinesOnlyPipe' = interact shortLinesOnly

respondPalindromesPipe = interact respondPalindromes

readGirlfriend = do
  handle <- openFile "girlfriend.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

readGirlfriend' = do
  withFile
    "girlfriend.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr contents
    )

readGirlfriendToPipe = do
  contents <- readFile "girlfriend.txt"
  putStr contents

readGirlfriendToFileCaps = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriendcaps.txt" (map toUpper contents)

appendToTodo = do
  todoItem <- getLine
  appendFile "todo.txt" (todoItem ++ "\n")

readFileBuffered = do
  withFile
    "learn7.hs"
    ReadMode
    ( \handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents
    )

removeFromTodo = do
  handle <- openFile "todo.txt" ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStrLn "These are you TODO items:"
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"

main = removeFromTodo