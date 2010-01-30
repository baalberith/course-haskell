module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Char
import Numeric
-- import Ratio
-- import Complex
import Data.Array
import Control.Monad.Error

import LispData


symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@^_~"



-- hashbang :: Parser Char
-- hashbang = do
--     string "#!" 
--     many (noneOf "\r\n")
--     return ' '
-- 
-- comment :: Parser Char
-- comment = do 
--     char ';'
--     many (noneOf "\r\n")
--     return ' '

spaces :: Parser ()
spaces = skipMany1 space


-- controlChar :: Parser Char
-- controlChar = do 
--     char '^'
--     c <- oneOf (['A' .. 'Z'] ++ "[\\]^_")
--     return (chr (ord c + 1 - ord 'A'))

-- namedChar :: Parser Char
-- namedChar = do 
--     name <- string "tab"
--         <|> string "linefeed"
--         <|> try (string "newline")
--         <|> string "return"
--         <|> string "space"
--         <|> string "nul"
--         <|> string "alarm"
--         <|> string "backspace"
--         <|> string "vtab"
--         <|> string "page"
--         <|> string "esc"
--         <|> string "delete"
--     case name of
--         "tab"       -> return '\t'
--         "linefeed"  -> return '\n'
--         "newline"   -> return '\n'
--         "return"    -> return '\r'
--         "space"     -> return ' '
--         "nul"       -> return (chr 0)
--         "alarm"     -> return (chr 7)
--         "backspace" -> return (chr 8)
--         "vtab"      -> return (chr 11)
--         "page"      -> return (chr 12)
--         "esc"       -> return (chr 27)
--         "delete"    -> return (chr 127)

namedChar :: Parser Char
namedChar = do 
    name <- string "newline"
        <|> string "space"
    case name of
        "newline" -> return '\n'
        "space" -> return ' '

-- escapedChars :: Parser Char
-- escapedChars = do 
--     char '\\'
--     r <- oneOf "\\\"nrtabvf" 
--     let res = case r of 
--                 '\\' -> r
--                 '"'  -> r
--                 'n'  -> '\n'
--                 'r'  -> '\r'
--                 't'  -> '\t'
--                 'a' -> chr 7
--                 'b' -> chr 8
--                 'v' -> chr 11
--                 'f' -> chr 12
--     return res

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
    where hex2dig s = (fst . head) (readHex s)
    
parseOct :: Parser LispVal
parseOct = do 
    try $ string "#o"
    r <- many1 octDigit
    return $ Number (oct2dig r)
    where oct2dig s = (fst . head) (readOct s)
    
parseBin :: Parser LispVal
parseBin = do 
    try $ string "#b"
    r <- many1 (oneOf "10")
    return $ Number (bin2dig r) where 
        bin2dig s = bin2dig' 0 s where
            bin2dig' n "" = n
            bin2dig' n (x:xs) = 
                let n' = 2 * n + toInteger (digitToInt x) 
                in bin2dig' n' xs
    

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
    let res = (fst . head) (readFloat $ n ++ "." ++ r)
    return $ Float res
    
-- parseRatio :: Parser LispVal
-- parseRatio = do 
--     n <- many1 digit
--     char '/'
--     m <- many1 digit
--     return $ Ratio ((read n) % (read m))
--     
-- parseComplex :: Parser LispVal
-- parseComplex = do 
--     re <- (try parseFloat <|> parseNumber)
--     char '+' 
--     im <- (try parseFloat <|> parseNumber)
--     char 'i' 
--     return $ Complex (toDouble re :+ toDouble im) where
--         toDouble (Float f) = f
--         toDouble (Number n) = fromIntegral n

parseBool :: Parser LispVal
parseBool = do 
    char '#'
    r <- oneOf "tf"
    let res = case r of 
                't' -> Bool True
                'f' -> Bool False
    return res
  
parseChar :: Parser Char
parseChar = do
    r <- anyChar
    notFollowedBy alphaNum 
    return r

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try namedChar <|> parseChar
    return $ Character value

parseList :: Parser LispVal
parseList = do
    res <- sepEndBy parseExpr spaces
    return $ List res 

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    char '.'
    spaces
    tail <- parseExpr
    return $ DottedList head tail
    
-- parseAnyList :: Parser LispVal
-- parseAnyList = do 
--     char '('
--     skipMany space
--     hd <- sepEndBy parseExpr spaces
--     tl <- option (List []) (try (char '.' >> spaces >> parseExpr))
--     skipMany space
--     char ')'
--     if isl tl
--         then return (List (hd ++ unpl tl))
--         else if isdl tl
--             then return (DottedList (hd ++ unpdlh tl) (unpdlt tl))
--             else return (DottedList hd tl) where 
--         isl (List ((Atom sym):_)) = not (sym == "unquote" || sym == "unquote-splicing")
--         isl (List _) = True
--         isl _ = False
--         unpl (List l) = l
--         isdl (DottedList _ _) = True
--         isdl _ = False
--         unpdlh (DottedList h _) = h
--         unpdlt (DottedList _ t) = t
    
parseAnyList :: Parser LispVal
parseAnyList = do
    char '('
    skipMany space
    res <- try parseDottedList <|> parseList
    skipMany space
    char ')'
    return res
    
parseVector :: Parser LispVal
parseVector = do 
    string "#("
    skipMany space
    vals <- sepEndBy parseExpr spaces
    skipMany space
    char ')'
    let len = length vals
    return $ Vector (listArray (0, (len - 1)) vals)
    
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
    
parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do 
    string ",@"
    res <- parseExpr
    return $ List [Atom "unquote-splicing", res]
    
parseAnyQuoted :: Parser LispVal
parseAnyQuoted = try parseUnQuoteSplicing <|> try parseUnQuote <|> try parseQuasiQuoted <|> parseQuoted


parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
--         <|> try parseComplex
        <|> try parseFloat
--         <|> try parseRatio
        <|> try parseNumber 
        <|> try parseBool
        <|> try parseCharacter
        <|> parseVector
        <|> parseAnyQuoted
        <|> parseAnyList
        
    
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = 
    case parse parser "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val


readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

-- readExprList :: String -> ThrowsError [LispVal]
-- readExprList = readOrThrow (optional hashbang >> skipMany spaces >> endBy parseExpr (spaces <|> eof))

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
