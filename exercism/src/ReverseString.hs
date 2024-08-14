module ReverseString (reverseString) where

reverseString :: String -> String
reverseString s = reverseStringTailRecur s ""

reverseStringTailRecur :: String -> String -> String
reverseStringTailRecur "" acc = acc
reverseStringTailRecur (ch : rest) acc = reverseStringTailRecur rest (ch : acc)
