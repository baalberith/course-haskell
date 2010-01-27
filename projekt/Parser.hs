module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Char
import Numeric
import Ratio
import Complex
import Data.Array
import Control.Monad.Error

import LispData


symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@^_~"


hashbang :: Parser Char
hashbang = do
    string "#!" 
    many (noneOf "\r\n")
    return ' '

comment :: Parser Char
comment = do 
    char ';'
    many (noneOf "\r\n")
    return ' '

spaces :: Parser ()
spaces = skipMany1 (comment <|> space)


controlChar :: Parser Char
controlChar = do 
    char '^'
    c <- oneOf (['A' .. 'Z'] ++ "[\\]^_")
    return (chr (ord c + 1 - ord 'A'))

namedChar :: Parser Char
namedChar = do 
    name <- string "alarm"
        <|> string "backspace"
        <|> string "delete"
        <|> string "esc"
        <|> string "linefeed"
        <|> try (string "newline")
        <|> string "nul"
        <|> string "page"
        <|> string "return"
        <|> string "space"
        <|> string "tab"
        <|> string "vtab"
    case name of
        "nul"       -> return (chr 0)
        "alarm"     -> return (chr 7)
        "backspace" -> return (chr 8)
        "tab"       -> return '\t'
        "linefeed"  -> return '\n'
        "newline"   -> return '\n'
        "vtab"      -> return (chr 11)
        "page"      -> return (chr 12)
        "return"    -> return '\r'
        "esc"       -> return (chr 27)
        "space"     -> return ' '
        "delete"    -> return (chr 127)

escapedChars :: Parser Char
escapedChars = do 
    char '\\'
    r <- oneOf "\\\"nrtabvf" 
    let res = case r of 
                '\\' -> r
                '"'  -> r
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'
                'a' -> chr 7
                'b' -> chr 8
                'v' -> chr 11
                'f' -> chr 12
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
                

readBaseInt :: Integer -> String -> Integer
readBaseInt b s = foldl ma 0 s where 
    ma v1 v2 = b*v1 + toInteger (digitToInt v2)

readBaseFrac :: Integer -> String -> Double
readBaseFrac _ [] = 0.0
readBaseFrac b s = r * foldr ma 0 s where
    r = 1.0 / fromInteger b
    ma v1 v2 = fromIntegral (digitToInt v1) + r*v2

parseHdr :: Parser (Char, Integer)
parseHdr = do 
    b <- option 'd' (char '#' >> oneOf "bodx")
    s <- option '+' (oneOf "+-")
    let base = case b of
                'b' -> 2
                'o' -> 8
                'd' -> 10
                'x' -> 16
    return (s, base)

baseDigits :: Integer -> String
baseDigits 2  = "01"
baseDigits 8  = "01234567"
baseDigits 10 = "0123456789"
baseDigits 16 = "0123456789abcdef"

int :: String
int = "int"


parseFloat1 :: Integer -> Parser (String,String)
parseFloat1 b = do 
    ip <- many1 (oneOf (baseDigits b))
    fp <- option int (char '.' >> many (oneOf (baseDigits b)))
    return (ip,fp)

parseFloat2 :: Integer -> Parser (String,String)
parseFloat2 b = do 
    char '.'
    fp <- many1 (oneOf (baseDigits b))
    return ("0",fp)


parseExp :: Integer -> Parser Integer
parseExp b = do 
    oneOf (if b == 16 then "x" else "ex")
    s <- option '+' (oneOf "+-")
    num <- many1 (oneOf (baseDigits b))
    let e = readBaseInt b num
    return (if s == '-' then (-e) else e)

powi :: Integer -> Integer -> Integer
powi b e | e == 0    = 1
         | e < 0     = error "negative exponent in powi"
         | even e    = powi (b*b) (e `quot` 2)
         | otherwise = b * powi b (e - 1)

pow :: Integer -> Integer -> Double
pow b e =
  if e >= 0 
       then fromInteger (powi b e) 
       else recip (fromInteger (powi b (-e)))
       

parseNumOrFlt :: Parser LispVal
parseNumOrFlt = do 
    (s, b) <- parseHdr
    (ip, fp) <- parseFloat1 b <|> parseFloat2 b
    e <- option 0 (parseExp b)
    let fpi = if fp == int then "0" else fp
        vf = pow b e * (fromInteger (readBaseInt b ip) + readBaseFrac b fpi)
        vi = powi b e * readBaseInt b ip
    if fp == int && e >= 0
        then return (Number (if s == '-' then (-vi) else vi))
        else return (Float (if s == '-' then (-vf) else vf))

parseRat :: Parser LispVal
parseRat = do 
    (s, b) <- parseHdr
    nstr <- many1 (oneOf (baseDigits b))
    char '/'
    dstr <- many1 (oneOf (baseDigits b))
    let num = readBaseInt b nstr
        den = readBaseInt b dstr
        ns = if s == '-' then (-num) else num
        val = if den /= 0
                then ns % den
                 else if ns > 0
                    then myRatPInf
                    else if ns < 0
                        then myRatNInf
                        else myRatNaN
    if denominator val == 1
        then return (Number (numerator val))
        else return (Ratio val)

parseNaNInf :: Parser LispVal
parseNaNInf = do 
    val <- try (string "+nan.0")
       <|> try (string "-nan.0")
       <|> try (string "+inf.0")
       <|> try (string "-inf.0")
    case val of
        "+nan.0" -> return (Float myFltNaN)
        "-nan.0" -> return (Float myFltNaN)
        "+inf.0" -> return (Float myFltPInf)
        "-inf.0" -> return (Float myFltNInf)
        

parseNumOrFltOrRat :: Parser LispVal
parseNumOrFltOrRat = try parseNaNInf <|> try parseRat <|> parseNumOrFlt
    

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
  
parseChar :: Parser Char
parseChar = do
    r <- anyChar
    notFollowedBy alphaNum 
    return r

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try controlChar <|> try namedChar <|> parseChar
    return $ Character value

parseList :: Parser LispVal
parseList = do
    res <- sepBy parseExpr spaces
    return $ List res 

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    char '.'
    spaces
    tail <- parseExpr
    return $ DottedList head tail
    
parseAnyList :: Parser LispVal
parseAnyList = do
    char '('
    res <- try parseList <|> parseDottedList
    char ')'
    return res
 
parseVector :: Parser LispVal
parseVector = do 
    arrayValues <- sepBy parseExpr spaces
    let len = length arrayValues
    return $ Vector (listArray (0, (len - 1)) arrayValues)
    
parseAnyVector :: Parser LispVal
parseAnyVector = do 
    string "#("
    x <- parseVector
    char ')'
    return x
    
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


parseLstOrDtdLst :: Parser LispVal
parseLstOrDtdLst = do 
    char '('
    skipMany space
    hd <- sepEndBy parseExpr spaces
    tl <- option (List []) (try (char '.' >> spaces >> parseExpr))
    skipMany space
    char ')'
    if isl tl
        then return (List (hd ++ unpl tl))
        else if isdl tl
            then return (DottedList (hd ++ unpdlh tl) (unpdlt tl))
            else return (DottedList hd tl) where 
    isl (List ((Atom sym):_)) = not (sym == "unquote" || sym == "unquote-splicing")
    isl (List _) = True
    isl _ = False
    unpl (List l) = l
    isdl (DottedList _ _) = True
    isdl _ = False
    unpdlh (DottedList h _) = h
    unpdlt (DottedList _ t) = t


-- parseExpr :: Parser LispVal
-- parseExpr = parseAtom
--         <|> parseString
--         <|> try parseComplex
--         <|> try parseFloat
--         <|> try parseRatio
--         <|> try parseNumber 
--         <|> try parseBool
--         <|> try parseCharacter
--         <|> parseAnyQuoted
--         <|> try parseAnyVector
--         <|> try parseAnyList
        
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseNumOrFltOrRat
        <|> try parseBool
        <|> try parseCharacter
        <|> parseAnyQuoted
        <|> try parseAnyVector
        <|> try parseLstOrDtdLst
        
    
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = 
    case parse parser "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val


readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

-- readExprList :: String -> ThrowsError [LispVal]
-- readExprList = readOrThrow (endBy parseExpr spaces)

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (optional hashbang >> skipMany spaces >> endBy parseExpr (spaces <|> eof))
