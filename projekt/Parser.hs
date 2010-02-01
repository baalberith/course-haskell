module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.IntMap as IntMap

import Data.Char
import Numeric

import Control.Monad
import Control.Monad.Error

import LispData

 

-- komentarz zaczynajacy sie od ; i ciagnacy sie do konca linii

comment :: Parser Char
comment = do 
    char ';'
    many (noneOf "\r\n")
    return ' '
    
-- parsuje co najmniej jedna spacja lub komentarz

spaces :: Parser ()
spaces = skipMany1 (space <|> comment)



-- symbole mozliwe do uzycia w nazwie zmiennej

symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=>?@^_~"
    
-- parsuje nazwe zmiennej (pierwszy znak to litera lub symbol, nastepne moga byc tez cyframi)

parseSymbol :: Parser LispVal
parseSymbol = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Symbol (first:rest)



-- parsuje mozliwe znaki zaczynajace sie od '\'

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
    
-- parsuej string (zaczyna i konczy sie '"' oraz nie zawiera w srodku znakow '\' za wyjatkiem escaped chars)

parseString :: Parser LispVal
parseString = do 
    char '"'
    res <- many $ escapedChars <|> noneOf "\\\""
    char '"'
    return $ String res
    


-- funkcje parsujace liczby calkowite o roznych podstawach

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
 
-- parsuje liczbe calkowita

parseInteger :: Parser LispVal
parseInteger = do 
    num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
    return $ num



-- parsuje liczbe rzeczywista
    
parseFloat :: Parser LispVal
parseFloat = do 
    n <- many1 digit
    char '.'
    r <- many1 digit
    let res = (fst . head) (readFloat $ n ++ "." ++ r)
    return $ Float res
    


-- parsuje '#t' oraz '#f' (nazwy symboliczne prawdy i falszu)

parseBool :: Parser LispVal
parseBool = do 
    char '#'
    r <- oneOf "tf"
    return $ case r of 
                't' -> Bool True
                'f' -> Bool False
                


-- parsuje pojedynczy znak
  
parseCharacter :: Parser Char
parseCharacter = do
    r <- anyChar
    notFollowedBy alphaNum 
    return r
    
-- parsuje znaki specjalne 'newline' oraz 'space'
    
namedChar :: Parser Char
namedChar = do 
    name <- string "newline"
        <|> string "space"
    case name of
        "newline" -> return '\n'
        "space" -> return ' '
        
-- parsuje znak zaczynajacy sie od '#\'

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    value <- try namedChar <|> parseCharacter
    return $ Char value
    


-- parsuje List '(a b c)' lub DottedList '(a b c . d)' (jesli jest to DottedList 
-- oraz element wystepujacy po '.' to List lub DottedList to splaszczamy go w odpowiedni sposob)
    
parseList :: Parser LispVal
parseList = do 
    char '('
    skipMany space
    hd <- sepEndBy parseExpr spaces
    tl <- option (List []) (try (char '.' >> spaces >> parseExpr))
    skipMany space
    char ')'
    if isL tl
        then return $ List (hd ++ unpackL tl)
        else if isDL tl
            then return $ DottedList (hd ++ unpackDLH tl) (unpackDLT tl)
            else return $ DottedList hd tl
    where isL (List ((Symbol s):_)) = not $ s == "unquote" || s == "unquote-splicing"
          isL (List _) = True
          isL _ = False
          unpackL (List l) = l
          isDL (DottedList _ _) = True
          isDL _ = False
          unpackDLH (DottedList h _) = h
          unpackDLT (DottedList _ t) = t
          


-- parsuje wektor zaczynajacy sie od '#(' oraz konczacy ')'
    
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
    


-- funcke parsujace wyrazenia zaczynajace sie od ''' (quote), '`' (quasiquate), ',` (unquote), ',@' (unquote-splicing)
    
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



-- parsuje poszczegolne wyrazenia (try przy parsowniu Float oraz Integer bo oba zaczynaja sie od cyfry
-- oraz przy Integer, Bool, Char, Vector bo moga sie zaczynac od '#')

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
        
    

-- parsuje podany string za pomoca podanego parsera
-- w przypadku bledu zwraca go opakowanego w LispError

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = 
    case parse parser "lisp" input of
        Left err -> throwError $ Parser err
        Right val -> return val



-- parsuje pojedyncze wyrazenie

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

-- parsuje liste wyrazen rozdzielonych spacjami lub komentarzami

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)
