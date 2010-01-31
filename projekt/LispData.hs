module LispData where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.IntMap as IntMap

import Data.Char
import Data.IORef
import Data.Maybe

import Control.Monad.Error
import System.IO


data LispVal = Symbol String
             | String String
             | Integer Integer
             | Float Double
             | Bool Bool
             | Char Char
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector Integer (IntMap.IntMap LispVal)
             | Prim ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}
             | IOPrim ([LispVal] -> IOThrowsError LispVal)
             | Port Handle


showVal :: LispVal -> String
showVal (Symbol name) = name
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Integer contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char ch) = showCharacters ch 
showVal (List contents) = showLists contents
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ " . " ++ showVal tl ++ ")"
showVal (Vector _ vals) = "#(" ++ unwordsList (IntMap.elems vals) ++ ")"
showVal (Prim _) = "<primitive>"
showVal (Func args varargs _ _) = showFuncs args varargs
showVal (IOPrim _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

instance Show LispVal where 
    show = showVal


showCharacters :: Char -> String
showCharacters '\n' = "#\\newline"
showCharacters ' '  = "#\\space"
showCharacters ch   = "#\\" ++ [ch]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

showLists :: [LispVal] -> String
showLists contents =
    case contents of
         [Symbol "quote", cont] -> "'" ++ show cont
         [Symbol "quasiquote", cont] -> "`" ++ show cont
         [Symbol "unquote", cont] -> "," ++ show cont
         [Symbol "unquote-splicing", cont] -> ",@" ++ show cont
         cont -> "(" ++ unwordsList cont ++ ")"
     
showFuncs :: [String] -> Maybe String -> String
showFuncs args varargs = 
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
               | VectorBounds Integer Integer
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (VectorBounds len n) = "Vector index out of bounds: " ++ show n ++ " not in [0.." ++ show (len - 1) ++ "]"
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

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
    traped <- runErrorT (trapError action) 
    return $ extractValue traped


type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

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

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
    env <- readIORef envRef 
    eenv <- extendEnv env bindings
    newIORef eenv 
    where extendEnv env bindings = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do 
              ref <- newIORef value
              return (var, ref)
            

data EEnv = EEnv Env Integer

eeE :: EEnv -> Env
eeE  (EEnv e _) = e

eeQL :: EEnv -> Integer
eeQL (EEnv _ q) = q

eeNewE :: EEnv -> Env -> EEnv
eeNewE  (EEnv _ q) e = EEnv e q

eeQLIncr, eeQLDecr :: EEnv -> EEnv
eeQLIncr (EEnv e q) = EEnv e (q + 1)
eeQLDecr (EEnv e q) = EEnv e (q - 1)
