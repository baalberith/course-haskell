module Library where

import qualified Data.IntMap as IntMap
import Control.Monad.Error
import System.IO
import Data.Char hiding (isSymbol, isNumber)

import LispData
import Parser



primitives :: [(String , [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numBinop (+)),
              ("-", numBinop (-)),
              ("*", numBinop (*)),
              ("/", numBinop div),
              
              ("mod", numBinop mod),
              ("quotient", numBinop quot),
              ("remainder", numBinop rem),
              ("gcd", numBinop gcd),
              ("lcm", numBinop lcm),
              
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              
              ("sqrt", fltBinop sqrt),
              ("exp", fltBinop exp),
              ("log", fltBinop log),
              ("sin", fltBinop sin),
              ("cos", fltBinop cos),
              ("tan", fltBinop tan),
              
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              
              ("char=?", charBoolBinop (==)),
              ("char<?", charBoolBinop (<)),
              ("char>?", charBoolBinop (>)),
              ("char>=?", charBoolBinop (>=)),
              ("char<=?", charBoolBinop (<=)),
              
              ("char-alphabetic?", charIs isAlpha),
              ("char-numeric?", charIs isDigit),
              ("char-oct-digit?", charIs isOctDigit),
              ("char-hex-digit?", charIs isHexDigit),
              ("char-whitespace?", charIs isSpace),
              ("char-upper-case?", charIs isUpper),
              ("char-lower-case?", charIs isLower),
              ("char-alphanumeric?", charIs isAlphaNum),
              ("char-control?", charIs isControl),
              ("char-printable?", charIs isPrint),
              
              ("char-upcase", charTo toUpper),
              ("char-downcase", charTo toLower),
              ("string-upcase", charTo toUpper),
              ("string-downcase", charTo toLower),
              
              ("symbol?", unaryOp isSymbol),
              ("string?", unaryOp isString),
              ("integer?", unaryOp isInteger),
              ("real?", unaryOp isReal),
              ("number?", unaryOp isNumber),
              ("bool?", unaryOp isBool),
              ("char?", unaryOp isChar), 
              ("list?", unaryOp isList),
              ("pair?", unaryOp isPair),
              ("vector?", unaryOp isVector),
              ("procedure?", unaryOp isProcedure),
              ("port?", unaryOp isPort),
              
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol), 
              ("string->number", unaryOp string2integer),
              ("number->string", unaryOp integer2string),
              ("char->string", unaryOp char2string),
              ("string->char", unaryOp string2char),
              ("char->integer", unaryOp char2integer),
              ("integer->char", unaryOp integer2char),
              
              ("null?", unaryOp isNull),
              ("zero?", unaryOp isZero),
              ("positive?", unaryOp isPositive),
              ("negative?", unaryOp isNegative),
              ("even?", unaryOp isEven),
              ("odd?", unaryOp isOdd),
               
              ("eqv?", eqv),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("not", lispNot),
              
              ("list", lispListFromArgs),
              ("length", lispLength),
              ("list-ref", lispListRef),
              ("list->vector", lispListToVec),
              ("vector->list", lispVecToList),
              ("vector", lispVecFromArgs),
              ("vector-length", lispVecSize),
              ("vector-ref", lispVecRef)]
              


unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [arg] = return $ op arg
unaryOp _ args = throwError $ NumArgs 1 args
     


numBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numBinop op params = do
    params' <- mapM unpackInt params
    return $ Integer $ foldl1 op params'
 


fltBinop :: (Double -> Double) -> [LispVal] -> ThrowsError LispVal
fltBinop op args =
    if length args /= 1
        then throwError $ NumArgs 1 args
        else do
            arg <- unpackFltNum $ head args
            return $ Float (op arg)

unpackFltNum :: LispVal -> ThrowsError Double
unpackFltNum (Integer n) = return $ fromInteger n
unpackFltNum (Float n) = return n
unpackFltNum notNum = throwError $ TypeMismatch "number" notNum



boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = 
    if length args /= 2 
        then throwError $ NumArgs 2 args
        else do 
            left <- unpacker $ args !! 0
            right <- unpacker $ args !! 1
            return $ Bool (left `op` right)

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

charBoolBinop :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal                                     
charBoolBinop = boolBinop unpackChar

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Char c) = return c
unpackChar notChar = throwError $ TypeMismatch "char" notChar

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal                                     
numBoolBinop = boolBinop unpackInt

unpackInt :: LispVal -> ThrowsError Integer
unpackInt (Integer n) = return n
unpackInt (Float n) = return (truncate n)
unpackInt notNum = throwError $ TypeMismatch "number" notNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Integer n) = return (show n)
unpackStr (Float n) = return (show n)
unpackStr notString = throwError $ TypeMismatch "string" notString



isSymbol, isString, isInteger, isReal, isNumber, isBool, isChar, isList, isPair, isVector, isProcedure, isPort :: LispVal -> LispVal
isSymbol (Symbol _) = Bool True
isSymbol _ = Bool False
isString (String _) = Bool True
isString _ = Bool False
isInteger (Integer _) = Bool True
isInteger _ = Bool False
isReal (Float _) = Bool True
isReal _ = Bool False
isNumber (Integer _) = Bool True
isNumber (Float _) = Bool True
isNumber _ = Bool False
isBool (Bool _) = Bool True
isBool _ = Bool False
isChar (Char _) = Bool True
isChar _ = Bool False
isList (List _) = Bool True
isList _ = Bool False
isPair (List _) = Bool True
isPair (DottedList _ _) = Bool True
isPair _ = Bool False
isVector (Vector _ _) = Bool True
isVector _ = Bool False
isProcedure (Prim _) = Bool True
isProcedure (IOPrim _) = Bool True
isProcedure (Func _ _ _ _) = Bool True
isProcedure _ = Bool False
isPort (Port _) = Bool True
isPort _ = Bool False



isNull :: LispVal -> LispVal
isNull (List []) = Bool True
isNull _ = Bool False

isZero :: LispVal -> LispVal
isZero (Integer n) = Bool $ n == 0
isZero (Float f) = Bool $ f == 0
isZero _ = Bool False

isPositive :: LispVal -> LispVal
isPositive (Integer n) = Bool $ n > 0
isPositive (Float f) = Bool $ f > 0
isPositive _ = Bool False

isNegative :: LispVal -> LispVal
isNegative (Integer n) = Bool $ n < 0
isNegative (Float f) = Bool $ f < 0
isNegative _ = Bool False

isEven :: LispVal -> LispVal
isEven (Integer n) = Bool $ even n
isEven _ = Bool False

isOdd :: LispVal -> LispVal
isOdd (Integer n) = Bool $ odd n
isOdd _ = Bool False



symbol2string, string2symbol, string2integer, integer2string, char2string, string2char, char2integer, integer2char :: LispVal -> LispVal
symbol2string (Symbol s) = String s
symbol2string _ = String ""
string2symbol (String s) = Symbol s
string2symbol _ = Symbol ""
string2integer (String s) = Integer $ ((read s) :: Integer)
string2integer _ = Integer 0
integer2string (Integer n) = String $ show n
integer2string _ = String ""              
char2string (Char c) = String [c]
char2string _ = String ""
string2char (String s) = Char $ head s
string2char _ = Char ' '
char2integer (Char c) = Integer $ toInteger (digitToInt c)
char2integer _ = Integer 0
integer2char (Integer n) = Char $ head (show n)
integer2char _ = Char ' '



charIs :: (Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charIs op [Char c] = return $ Bool (op c)
charIs _ [notChar] = throwError $ TypeMismatch "char" notChar
charIs _ badArgList = throwError $ NumArgs 1 badArgList

charTo :: (Char -> Char) -> [LispVal] -> ThrowsError LispVal
charTo op [Char char] = return $ Char (op char)
charTo op [String str] = return $ String (map op str)
charTo _ [notChar] = throwError $ TypeMismatch "char" notChar
charTo _ badArgList = throwError $ NumArgs 1 badArgList



eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool v1), (Bool v2)] = return $ Bool $ v1 == v2
eqv [(Char c1), (Char c2)] = return $ Bool $ c1 == c2
eqv [(Integer v1), (Integer v2)] = return $ Bool $ v1 == v2
eqv [(Float v1), (Float v2)] = return $ Bool $ v1 == v2 
eqv [(String v1), (String v2)] = return $ Bool $ v1 == v2
eqv [(Symbol v1), (Symbol v2)] = return $ Bool $ v1 == v2
eqv [(DottedList l1 t1), (DottedList l2 t2)] = eqv [List (l1 ++ [t1]), List (l2 ++ [t2])]
eqv [(Vector l1 v1), (Vector l2 v2)] = eqv [List (IntMap.elems v1), List (IntMap.elems v2)] 
eqv [(List l1), (List l2)] = return $ Bool $ length l1 == length l2 && all eqvPair (zip l1 l2) where 
    eqvPair (x1, x2) = 
        case eqv [x1, x2] of
            Left err -> False
            Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList



car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

lispNot :: [LispVal] -> ThrowsError LispVal
lispNot [Bool False] = return $ Bool True
lispNot _ = return $ Bool False



lispListFromArgs :: [LispVal] -> ThrowsError LispVal
lispListFromArgs = return . List

lispLength :: [LispVal] -> ThrowsError LispVal
lispLength [List lst] = return $ Integer ((toInteger . length) lst)
lispLength [badArg] = throwError $ TypeMismatch "list" badArg
lispLength badArgList = throwError $ NumArgs 1 badArgList

lispListRef :: [LispVal] -> ThrowsError LispVal
lispListRef [List lst, Integer n] =
    if n >= 0 && n < len
        then return $ lst !! (fromInteger n)
        else return $ List []
    where len = toInteger $ length lst
lispListRef [badArg] = throwError $ TypeMismatch "list + integer" badArg
lispListRef badArgList = throwError $ NumArgs 2 badArgList

lispListToVec :: [LispVal] -> ThrowsError LispVal
lispListToVec [List vals] = lispVecFromArgs vals
lispListToVec [badArg] = throwError $ TypeMismatch "list" badArg
lispListToVec badArgList = throwError $ NumArgs 1 badArgList
 
lispVecToList :: [LispVal] -> ThrowsError LispVal
lispVecToList [Vector _ vec] = return $ List (IntMap.elems vec)
lispVecToList [badArg] = throwError $ TypeMismatch "vector" badArg
lispVecToList badArgList = throwError $ NumArgs 1 badArgList

lispVecFromArgs :: [LispVal] -> ThrowsError LispVal
lispVecFromArgs vals =
    let len = length vals
        assoc = zip [0 .. (len - 1)] vals
    in return $ Vector (toInteger len) (IntMap.fromList assoc)

lispVecSize :: [LispVal] -> ThrowsError LispVal
lispVecSize [Vector len _] = return (Integer len)
lispVecSize [badArg] = throwError $ TypeMismatch "vector" badArg
lispVecSize badArgList = throwError $ NumArgs 1 badArgList

lispVecRef :: [LispVal] -> ThrowsError LispVal
lispVecRef [Vector len vec, Integer n] =
  if n >= 0 && n < len
     then return (IntMap.findWithDefault (Bool False) (fromInteger n) vec)
     else throwError $ VectorBounds len n
lispVecRef [badArg] = throwError $ TypeMismatch "vector + integer" badArg
lispVecRef badArgList = throwError $ NumArgs 2 badArgList



ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]
 


makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: LispVal -> IOThrowsError [LispVal]
load (String filename) = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [arg@(String filename)] = liftM List $ load arg
