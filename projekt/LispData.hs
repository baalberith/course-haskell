module LispData where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified Data.IntMap as IntMap

import Data.Char
import Data.IORef
import Data.Maybe

import Control.Monad.Error
import System.IO



-- typ danych do przechowywania wartosci po sparsowaniu

data LispVal = Symbol String -- nazwy zmiennych i funkcji
             | String String
             | Integer Integer
             | Float Double
             | Bool Bool
             | Char Char
             | List [LispVal]
             | DottedList [LispVal] LispVal
             -- wektor (pierwszy element to dlugosc, drugi mapa z Int w LispVal)
             | Vector Integer (IntMap.IntMap LispVal) 
             -- funkcje juz zdefiniowane w bibliotece
             | Prim ([LispVal] -> ThrowsError LispVal) 
             -- funckcje zdefiniowane przez uzytkownika
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env} 
             -- funkcje wejscia/wyjscia zdefiniowane w bibliotece
             | IOPrim ([LispVal] -> IOThrowsError LispVal) 
             | Port Handle



-- wyswietla tekstowa reprezentacje wartosci

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

-- LispVal czynimy instancja Show (do wyswietlania bedzie sluzyc showVal)

instance Show LispVal where 
    show = showVal

-- funkcje pomocnicze dla showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

showCharacters :: Char -> String
showCharacters '\n' = "#\\newline"
showCharacters ' '  = "#\\space"
showCharacters ch   = "#\\" ++ [ch]

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
        


-- typ danych do przechowywania bledow pojawiajacych sie podczas ewaluacji

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | VectorBounds Integer Integer
               | Default String

-- wyswiela tekstowa reprezentacje bledu

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (VectorBounds len n) = "Vector index out of bounds: " ++ show n ++ " not in [0.." ++ show (len - 1) ++ "]"
showError (Parser parseErr) = "Parse error at " ++ show parseErr

-- LispError czynimy instancja klas Show (z funcja showError) oraz Error

instance Show LispError where 
    show = showError

instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default



-- podtsawowy typ bledow 

type ThrowsError = Either LispError

-- wyluskuje tekstowa reprezentacje LispValue lub LispError

extractValue :: ThrowsError String -> String
extractValue (Right str) = str

-- typ bledow przy korzystaniu z IORef (do pracy ze zmiennymi) 
-- lub funkcji wejscia/wyjscia

type IOThrowsError = ErrorT LispError IO

-- w przypadku przechwycenia bledu, zwraca jego tekstowa reprezentacje

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

-- promuje ThrowsError do IOThrowsError

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- wykonujemy akcje, przechwytujemy bledy
-- tekstowa reprezentacje wartosci lub bledow pakujemy do monady IO

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
    traped <- runErrorT (trapError action) 
    return $ extractValue traped



-- typ srodowiska do przechowywania zmiennych 
-- (mapa z nazw zmiennych w wartosci zmiennych)
-- srodowisko mozemy zmieniac w dwojaki sposob:
-- 1. za pomoca set! zmieniamy indywidualna zmienna
-- 2. przy pomocy define ddodajemy nowa zmienna

type Env = IORef [(String, IORef LispVal)]

-- wartosc poczatkowa (puste srodowisko)

nullEnv :: IO Env
nullEnv = newIORef []

-- sprawdza czy zmienna o podanej nazwie zostala juz zdefiniowana

isBound :: Env -> String -> IO Bool
isBound envRef var = do
    env <- liftIO $ readIORef envRef 
    return $ isJust (lookup var env)
    
-- pobiera wartosc zmiennej o podanej nazwie
-- jesli zmienna nie zostala jeszcze zdefiniowana - zwraca blad

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do 
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Just env' -> liftIO $ readIORef env'
        Nothing -> throwError $ UnboundVar "Getting an unbound variable: " var
        
-- zmienia wartosc istniejace zmiennej, a w przeciwnym wypadku zwraca blad
        
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do 
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Just env' -> liftIO $ writeIORef env' value
        Nothing -> throwError $ UnboundVar "Setting an unbound variable: " var
    return value
    
-- definiuje nowa zmienna jesli do tej pory nie istniala 
-- lub zmienia wartosc juz istniajacej

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
          
-- rozszerza srodowisko o zmienne z listy

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = do
    env <- readIORef envRef 
    eenv <- extendEnv env bindings
    newIORef eenv 
    where extendEnv env bindings = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do 
              ref <- newIORef value
              return (var, ref)
            
-- srodowisko rozszerzone o 'quotation level' potrzebny przy ewaluacji 'quasiquote'

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
