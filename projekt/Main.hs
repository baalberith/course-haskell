module Main where

import System.Environment
import Control.Monad.Error
import IO hiding (try)

import LispData
import Parser
import Evaluator
import Library



-- tworzy nowe srodowisko, ktore rozszerza o nazwy funkcji bibliotecznych

primitiveBindings :: IO Env
primitiveBindings = do
    env <- nullEnv
    bindVars env $ map (makeFunc IOPrim) ioPrimitives ++ map (makeFunc Prim) primitives where 
        makeFunc constructor (var, func) = (var, constructor func)



--  natychmiast wypisuje podany string na ekran

flushStr :: String -> IO ()
flushStr str = do
    putStr str 
    hFlush stdout
    
-- wypisuje na akran podny znak zachety, 
-- po czym wczytuje linie, ktora pozniej zostanie sparsowana

readPrompt :: String -> IO String
readPrompt prompt = do
    flushStr prompt 
    getLine



-- parsuje i ewaluuje podane wyrazenie

evalString :: Env -> String -> IO String
evalString env expr = do
    let eenv = EEnv env 0
    let evaled = (liftThrows $ readExpr expr) >>= eval eenv
    runIOThrows $ liftM show $ evaled
    
-- wypisuje na ekran zewaluowane wyrazenie 
-- lub tekstowa reprezentacje bledow, jesli takie sie pojawily
    
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
    str <- evalString env expr 
    putStrLn str



-- laduje plik o nazwie bedacej pierwszym argumentem
-- przypisuje zmiennej 'args' pozostale argumenty
-- ewaluuje wyrazenia zawarte we wczytanym pliku
-- wypisuje na wyjscie bledow wynik ostatniego wyrazenia

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings
    let args' = map String $ drop 1 args
    env' <- bindVars env [("args", List $ args')] 
    str <- evalString env' $ "(load \"" ++ head args ++ "\")"
    hPutStrLn stderr str
    
-- petla wczytujaca, ewaluujaca i wypisujaca

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else do
         action result
         until_ pred prompt action

runRepl :: IO ()
runRepl = do
    env <- primitiveBindings 
    until_ (== "quit") (readPrompt "Lisp>>> ") (evalAndPrint env)
        


-- jesli wywolano bez argumentow uruchamiamy interpreter,
-- w przeciwnym wypadku wywolujemy runOne z podanymi argumentami

main :: IO ()
main = do 
    args <- getArgs
    if null args 
        then runRepl 
        else runOne $ args
