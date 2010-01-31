{-# LANGUAGE FlexibleContexts #-}

module Library where

import qualified Data.IntMap as IntMap
import Control.Monad.Error
import System.IO

import LispData
import Parser

primitives :: [(String , [LispVal] -> ThrowsError LispVal)]
primitives = [("+" , numericBinop (+)),
              ("-" , numericBinop (-)),
              ("*" , numericBinop (*)),
              ("/" , numericBinop div),
              
              ("mod" , numericBinop mod),
              ("quotient" , numericBinop quot),
              ("remainder" , numericBinop rem),
              
              ("symbol?" , unaryOp isSymbol),
              ("string?" , unaryOp isString),
              ("integer?" , unaryOp isInteger),
              ("real?" , unaryOp isReal),
              ("number?" , unaryOp isNumber),
              ("bool?", unaryOp isBool),
              ("char?" , unaryOp isChar), 
              ("list?" , unaryOp isList),
              ("pair?" , unaryOp isPair),
              ("vector?" , unaryOp isVector),
              ("procedure?" , unaryOp isProcedure),
              ("port?" , unaryOp isPort),
              
              ("null?", unaryOp isNull),
              
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol), 
              ("string->number" , unaryOp string2number),
              ("number->string" , unaryOp number2string),
              
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              
              ("eqv?", eqv),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons)]
                
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Integer n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = do
    params' <- mapM unpackNum params
    return $ Integer $ foldl1 op params'

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [arg] = return $ op arg
unaryOp _ args = throwError $ NumArgs 1 args

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal                                     
numBoolBinop = boolBinop unpackNum 

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = 
    if length args /= 2 
        then throwError $ NumArgs 2 args
        else do 
            left <- unpacker $ args !! 0
            right <- unpacker $ args !! 1
            return $ Bool $ left `op` right
        

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

symbol2string, string2symbol, string2number, number2string :: LispVal -> LispVal
symbol2string (Symbol s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Symbol s
string2symbol _          = Symbol ""
string2number (String s) = Integer $ ((read s) :: Integer)
string2number _          = Integer 0
number2string (Integer n) = String $ show n
number2string _          = String ""

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool v1), (Bool v2)] = return $ Bool $ v1 == v2
eqv [(Char c1), (Char c2)] = return $ Bool $ c1 == c2
eqv [(Integer v1), (Integer v2)] = return $ Bool $ v1 == v2
eqv [(Float v1), (Float v2)] = return $ Bool $ v1 == v2 
eqv [(String v1), (String v2)] = return $ Bool $ v1 == v2
eqv [(Symbol v1), (Symbol v2)] = return $ Bool $ v1 == v2
eqv [(DottedList l1 t1), (DottedList l2 t2)] = eqv [List (l1 ++ [t1]), List (l2 ++ [t2])]
eqv [(List l1), (List l2)] = return $ Bool $ length l1 == length l2 && all eqvPair (zip l1 l2) 
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [(Vector l1 v1), (Vector l2 v2)] = eqv [List (IntMap.elems v1), List (IntMap.elems v2)] 
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
