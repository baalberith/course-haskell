module LispData where

import Ratio
import Complex
import Data.Array

data LispVal = Atom String
             | String String
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Bool Bool
             | Character Char
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showLists :: [LispVal] -> String
showLists contents =
    case contents of
         [Atom "quote", cont] -> "'" ++ show cont
         [Atom "quasiquote", cont] -> "`" ++ show cont
         [Atom "unquote", cont] -> "," ++ show cont
         cont -> "(" ++ unwordsList cont ++ ")"

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio r) = (show . numerator) r ++ "/" ++ (show . denominator) r
showVal (Complex w) = (show . realPart) w ++ "+" ++ (show . imagPart) w ++ "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character ' ') = "#\\space"
showVal (Character '\n') = "#\\newline"
showVal (Character c) = "#\\" ++ [c]
showVal (List contents) = showLists contents
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector v) = "#" ++ (show . List . elems) v 

instance Show LispVal where show = showVal
