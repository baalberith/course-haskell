module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Char
import Numeric
import Ratio
import Complex
import Data.Array

import LispData



symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do 
    char '\\'
    r <- oneOf "\\\"nrt" 
    let res = case r of 
                '\\' -> r
                '"'  -> r
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'
    return res
    
parseDigital1 :: Parser LispVal
parseDigital1 = do 
    r <- many1 digit
    return $ Number (read r)   
    
parseDigital2 :: Parser LispVal
parseDigital2 = do 
    try $ string "#d"
    r <- many1 digit
    return $ Number (read r)   
    
parseHex :: Parser LispVal
parseHex = do 
    try $ string "#x"
    r <- many1 hexDigit
    return $ Number (hex2dig r)
    where hex2dig s = fst $ head (readHex s)
    
parseOct :: Parser LispVal
parseOct = do 
    try $ string "#o"
    r <- many1 octDigit
    return $ Number (oct2dig r)
    where oct2dig s = fst $ head (readOct s)
    
parseBin :: Parser LispVal
parseBin = do 
    try $ string "#b"
    r <- many1 (oneOf "10")
    return $ Number (bin2dig r) where 
        bin2dig s = bin2dig' 0 s where
            bin2dig' n "" = n
            bin2dig' n (x:xs) = 
                let n' = 2 * n + toInteger (ord x - ord '0') 
                in bin2dig' n' xs
    
parseChar :: Parser String
parseChar = do
    r <- anyChar
    notFollowedBy alphaNum 
    return [r]
    
parseVector :: Parser LispVal
parseVector = do 
    arrayValues <- sepBy parseExpr spaces
    let len = length arrayValues
    return $ Vector (listArray (0, (len - 1)) arrayValues)



parseAtom :: Parser LispVal
parseAtom = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom
    
parseString :: Parser LispVal
parseString = do 
    char '"'
    res <- many $ escapedChars <|> noneOf "\\\""
    char '"'
    return $ String res
    
parseNumber :: Parser LispVal
parseNumber = do 
    num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
    return $ num
    
parseFloat :: Parser LispVal
parseFloat = do 
    n <- many1 digit
    char '.'
    r <- many1 digit
    let res = fst . head. readFloat $ n ++ "." ++ r
    return $ Float res
    
parseRatio :: Parser LispVal
parseRatio = do 
    n <- many1 digit
    char '/'
    m <- many1 digit
    return $ Ratio ((read n) % (read m))
    
parseComplex :: Parser LispVal
parseComplex = do 
    re <- (try parseFloat <|> parseNumber)
    char '+' 
    im <- (try parseFloat <|> parseNumber)
    char 'i' 
    return $ Complex (toDouble re :+ toDouble im) where
        toDouble (Float f) = f
        toDouble (Number n) = fromIntegral n

parseBool :: Parser LispVal
parseBool = do 
    string "#"
    r <- oneOf "tf"
    let res = case r of 
                't' -> Bool True
                'f' -> Bool False
    return res
    
parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space") <|> parseChar
    let res = case value of
                "space" -> ' '
                "newline" -> '\n'
                otherwise -> head value
    return $ Character res

parseList :: Parser LispVal
parseList = do
    res <- sepBy parseExpr spaces
    return $ List res 

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail
    
parseAnyList :: Parser LispVal
parseAnyList = do
    char '('
    res <- try parseList <|> parseDottedList
    char ')'
    return res
    
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    res <- parseExpr
    return $ List [Atom "quote", res]
    
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    res <- parseExpr
    return $ List [Atom "quasiquote", res]
    
parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    res <- parseExpr
    return $ List [Atom "unquote", res]
    
parseAnyVector :: Parser LispVal
parseAnyVector = do 
    string "#("
    x <- parseVector
    char ')'
    return x



parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseRatio
        <|> try parseFloat
        <|> try parseNumber 
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> try parseAnyVector
        <|> try parseAnyList
        


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val
