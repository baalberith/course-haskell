module Evaluator where

import Control.Monad.Error

import LispData

primitives :: [(String , [LispVal] -> ThrowsError LispVal)]
primitives = [("+" , numericBinop (+)) ,
              ("-" , numericBinop (-)) ,
              ("*" , numericBinop (*)) ,
              ("/" , numericBinop div) ,
              ("mod" , numericBinop mod) ,
              ("quotient" , numericBinop quot) ,
              ("remainder" , numericBinop rem) ,
              ("symbol?" , unaryOp symbolp) ,
              ("string?" , unaryOp stringp) ,
              ("number?" , unaryOp numberp) ,
              ("bool?", unaryOp boolp) ,
              ("list?" , unaryOp listp) ,
              ("symbol->string", unaryOp symbol2string) ,
              ("string->symbol", unaryOp string2symbol)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op where
    unpackNum (Number n) = return n
    unpackNum notNum = throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [arg] = return $ op arg
unaryOp _ args = throwError $ NumArgs 1 args

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False
numberp (Number _) = Bool True
numberp _ = Bool False
stringp (String _) = Bool True
stringp _ = Bool False
boolp (Bool _) = Bool True
boolp _ = Bool False
listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = 
    let res = lookup func primitives
    in case res of
        Just f -> f args
        Nothing -> throwError $ NotFunction "Unrecognized primitive function args" func
        


eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Ratio _) = return val
eval val@(Complex _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
