module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.IntMap as IntMap

import Data.Char
import Numeric

import Control.Monad
import Control.Monad.Error

import LispData


symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@^_~"

 
comment :: Parser Char
comment = do 
    char ';'
    many (noneOf "\r\n")
    return ' '

spaces :: Parser ()
spaces = skipMany1 (space <|> comment)


namedChar :: Parser Char
namedChar = do 
    name <- string "newline"
        <|> string "space"
    case name of
        "newline" -> return '\n'
        "space" -> return ' '

escapedChars :: Parser Char
escapedChars = do 
    char '\\'
    r <- oneOf "\\\"nrt" 
    return $ case r of 
                '\\' -> r
                '"'  -> r
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'
    

parseDigital1 :: Parser LispVal
parseDigital1 = do 
    r <- many1 digit
    return $ Integer (read r)   
    
parseDigital2 :: Parser LispVal
parseDigital2 = do 
    try $ string "#d"
    r <- many1 digit
    return $ Integer (read r)   
    
parseHex :: Parser LispVal
parseHex = do 
    try $ string "#x"
    r <- many1 hexDigit
    return $ Integer (hex2dig r)
    where hex2dig s = (fst . head) (readHex s)
    
parseOct :: Parser LispVal
parseOct = do 
    try $ string "#o"
    r <- many1 octDigit
    return $ Integer (oct2dig r)
    where oct2dig s = (fst . head) (readOct s)
    
parseBin :: Parser LispVal
parseBin = do 
    try $ string "#b"
    r <- many1 (oneOf "10")
    return $ Integer (bin2dig r) where 
        bin2dig s = bin2dig' 0 s where
            bin2dig' n "" = n
            bin2dig' n (x:xs) = 
                let n' = 2 * n + toInteger (digitToInt x) 
                in bin2dig' n' xs
    

parseSymbol :: Parser LispVal
parseSymbol = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Symbol atom
    
parseString :: Parser LispVal
parseString = do 
    char '"'
    res <- many $ escapedChars <|> noneOf "\\\""
    char '"'
    return $ String res
    
parseInteger :: Parser LispVal
parseInteger = do 
    num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
    return $ num
    
parseFloat :: Parser LispVal
parseFloat = do 
    n <- many1 digit
    char '.'
    r <- many1 digit
    let res = (fst . head) (readFloat $ n ++ "." ++ r)
    return $ Float res

parseBool :: Parser LispVal
parseBool = do 
    char '#'
    r <- oneOf "tf"
    return $ case r of 
                't' -> Bool True
                'f' -> Bool False
  
parseCharacter :: Parser Char
parseCharacter = do
    r <- anyChar
    notFollowedBy alphaNum 
    return r

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    value <- try namedChar <|> parseCharacter
    return $ Char value
    
parseList :: Parser LispVal
parseList = do 
    char '('
    skipMany space
    hd <- sepEndBy parseExpr spaces
    tl <- option (List []) (try (char '.' >> spaces >> parseExpr))
    skipMany space
    char ')'
    if isl tl
        then return $ List (hd ++ unpl tl)
        else if isdl tl
            then return $ DottedList (hd ++ unpdlh tl) (unpdlt tl)
            else return $ DottedList hd tl
    where isl (List ((Symbol s) : _)) = not $ s == "unquote" || s == "unquote-splicing"
          isl (List _) = True
          isl _ = False
          unpl (List l) = l
          isdl (DottedList _ _) = True
          isdl _ = False
          unpdlh (DottedList h _) = h
          unpdlt (DottedList _ t) = t
    
parseVector :: Parser LispVal
parseVector = do 
    string "#("
    skipMany space
    vals <- sepEndBy parseExpr spaces
    skipMany space
    char ')'
    let len = length vals
    let assoc = zip [0 .. (len - 1)] vals
    return $ Vector (toInteger len) (IntMap.fromList assoc)
    
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    res <- parseExpr
    return $ List [Symbol "quote", res]
    
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    res <- parseExpr
    return $ List [Symbol "quasiquote", res]
    
parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    res <- parseExpr
    return $ List [Symbol "unquote", res]
    
parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do 
    string ",@"
    res <- parseExpr
    return $ List [Symbol "unquote-splicing", res]
    
parseAnyQuoted :: Parser LispVal
parseAnyQuoted = try parseUnQuoteSplicing <|> try parseUnQuote <|> try parseQuasiQuoted <|> parseQuoted


parseExpr :: Parser LispVal
parseExpr = parseSymbol
        <|> parseString
        <|> try parseFloat
        <|> try parseInteger 
        <|> try parseBool
        <|> try parseChar
        <|> parseVector
        <|> parseAnyQuoted
        <|> parseList
        
    
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = 
    case parse parser "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val


readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
