module LispData where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char
import Ratio
import Complex
import Data.Array
import Control.Monad.Error

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
     
showCharacters :: Char -> String
showCharacters ch 
    | ch == chr 0   = "#\\nul"
    | ch == chr 7   = "#\\alarm"
    | ch == chr 8   = "#\\backspace"
    | ch == '\t'    = "#\\tab"
    | ch == '\n'    = "#\\linefeed"
    | ch == chr 11  = "#\\vtab"
    | ch == chr 12  = "#\\page"
    | ch == '\r'    = "#\\return"
    | ch == chr 27  = "#\\esc"
    | ch == ' '     = "#\\space"
    | ch == chr 127 = "#\\delete"
    | isControl ch  = "#\\^" ++ [chr (ord ch + ord 'A' - 1)]
    | isPrint ch    = "#\\" ++ [ch]
    | otherwise     = [ch]

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Ratio r) = (show . numerator) r ++ "/" ++ (show . denominator) r
showVal (Complex w) = (show . realPart) w ++ "+" ++ (show . imagPart) w ++ "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character ch) = showCharacters ch 
showVal (List contents) = showLists contents
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector v) = "#" ++ (show . List . elems) v 

instance Show LispVal where show = showVal



data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either LispError

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError String -> String
extractValue (Right str) = str
