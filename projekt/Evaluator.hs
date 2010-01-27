module Evaluator where

import Control.Monad.Error
import System.IO

import LispData
import Parser


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
              ("string->symbol", unaryOp string2symbol), 
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
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("string->number" , unaryOp string2number),
              ("number->string" , unaryOp number2string)]
              
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
-- numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
-- numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = do
    params' <- mapM unpackNum params
    return $ Number $ foldl1 op params'

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
                                     

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

-- primitiveBindings :: IO Env
-- primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives) where 
--     makeFunc constructor (var, func) = (var, constructor func)

primitiveBindings :: IO Env
primitiveBindings = do
    env <- nullEnv
    bindVars env $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives where 
        makeFunc constructor (var, func) = (var, constructor func)

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs varargs = makeFunc $ Just (showVal varargs)

        
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply func@(Func _ _ _ _) args = applyFunc func args

applyFunc :: LispVal -> [LispVal] -> IOThrowsError LispVal       
applyFunc (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody where 
            num = toInteger . length
            remainingArgs = drop (length params) args
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env     
            evalBody env = liftM last $ mapM (eval env) body 
            

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom id) = getVar env id
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Ratio _) = return val
eval env val@(Complex _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = evalIf env pred conseq alt
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval env)
eval env list@(List _) = evalFunc env list
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalIf :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalIf env pred conseq alt = do 
    result <- eval env pred
    case result of
        Bool False -> eval env alt
        otherwise -> eval env conseq

evalFunc :: Env -> LispVal -> IOThrowsError LispVal
evalFunc env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
        

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

symbol2string, string2symbol, string2number, number2string :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""
string2number (String s) = Number $ ((read s) :: Integer)
string2number _          = Number 0
number2string (Number n) = String $ show n
number2string _          = String ""

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


applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

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

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
