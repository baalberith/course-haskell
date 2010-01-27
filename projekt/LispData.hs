module LispData where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char
-- import Ratio
-- import Complex
import Data.Array
import Control.Monad.Error
import Data.IORef
import System.IO
import Data.Maybe


data LispVal = Atom String
             | String String
             | Number Integer
             | Float Double
--              | Ratio Rational
--              | Complex (Complex Double)
             | Bool Bool
             | Character Char
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (Float contents) = show contents
-- showVal (Ratio r) = (show . numerator) r ++ "/" ++ (show . denominator) r
-- showVal (Complex w) = (show . realPart) w ++ "+" ++ (show . imagPart) w ++ "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character ch) = showCharacters ch 
showVal (List contents) = showLists contents
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector v) = "#" ++ (show . List . elems) v 
showVal (PrimitiveFunc _) = "<primitive>"
showVal func@(Func _ _ _ _) = showFuncs func
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

instance Show LispVal where 
    show = showVal


-- showCharacters :: Char -> String
-- showCharacters '\t' = "#\\tab"
-- showCharacters '\n' = "#\\linefeed"
-- showCharacters '\r' = "#\\return"
-- showCharacters ' '  = "#\\space"
-- showCharacters ch 
--     | ch == chr 0   = "#\\nul"
--     | ch == chr 7   = "#\\alarm"
--     | ch == chr 8   = "#\\backspace"
--     | ch == chr 11  = "#\\vtab"
--     | ch == chr 12  = "#\\page"
--     | ch == chr 27  = "#\\esc"
--     | ch == chr 127 = "#\\delete"
--     | isControl ch  = "#\\^" ++ [chr (ord ch + ord 'A' - 1)]
--     | isPrint ch    = "#\\" ++ [ch]

showCharacters :: Char -> String
showCharacters '\n' = "#\\newline"
showCharacters ' '  = "#\\space"
showCharacters ch   = "#\\" ++ [ch]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showLists :: [LispVal] -> String
showLists contents =
    case contents of
         [Atom "quote", cont] -> "'" ++ show cont
         [Atom "quasiquote", cont] -> "`" ++ show cont
         [Atom "unquote", cont] -> "," ++ show cont
         [Atom "unquote-splicing", cont] -> ",@" ++ show cont
         cont -> "(" ++ unwordsList cont ++ ")"
     
showFuncs :: LispVal -> String
showFuncs (Func {params = args, vararg = varargs}) = 
    "(lambda (" ++ unwords (map show args) ++ 
        (case varargs of 
            Just arg -> " . " ++ arg 
            Nothing -> "") 
    ++ ") ...)" 
        

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

instance Show LispError where 
    show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default


type ThrowsError = Either LispError

extractValue :: ThrowsError String -> String
extractValue (Right str) = str


type IOThrowsError = ErrorT LispError IO

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- runIOThrows :: IOThrowsError String -> IO String
-- runIOThrows action = runErrorT (trapError action) >>= return . extractValue

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
    traped <- runErrorT (trapError action) 
    return $ extractValue traped


type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

-- isBound :: Env -> String -> IO Bool
-- isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

isBound :: Env -> String -> IO Bool
isBound envRef var = do
    env <- liftIO $ readIORef envRef 
    return $ isJust (lookup var env)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do 
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Just env' -> liftIO $ readIORef env'
        Nothing -> throwError $ UnboundVar "Getting an unbound variable: " var
        
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do 
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Just env' -> liftIO $ writeIORef env' value
        Nothing -> throwError $ UnboundVar "Setting an unbound variable: " var
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then do
           setVar envRef var value 
           return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value
          
-- bindVars :: Env -> [(String, LispVal)] -> IO Env
-- bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef where 
--     extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
--     addBinding (var, value) = do 
--         ref <- newIORef value
--         return (var, ref)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
    env <- readIORef envRef 
    eenv <- extendEnv env bindings
    newIORef eenv where 
        extendEnv env bindings = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do 
            ref <- newIORef value
            return (var, ref)
