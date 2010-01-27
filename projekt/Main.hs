module Main where

import System.Environment
import Control.Monad.Error
import IO hiding (try)

import LispData
import Parser
import Evaluator


flushStr :: String -> IO ()
flushStr str = do
    putStr str 
    hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = do
    flushStr prompt 
    getLine


-- evalString :: Env -> String -> IO String
-- evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalString :: Env -> String -> IO String
evalString env expr = do
    let evaled = (liftThrows $ readExpr expr) >>= eval env
    runIOThrows $ liftM show $ evaled
    
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
    str <- evalString env expr 
    putStrLn str


-- runOne :: [String] -> IO ()
-- runOne args = do
--     env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
--     (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings
    let args' = map String $ drop 1 args
    env' <- bindVars env [("args", List $ args')] 
    str <- runIOThrows $ liftM show $ eval env' (List [Atom "load", String (head args)])
    hPutStrLn stderr str

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else do
         action result
         until_ pred prompt action

-- runRepl :: IO ()
-- runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runRepl :: IO ()
runRepl = do
    env <- primitiveBindings 
    until_ (== "quit") (readPrompt "Lisp>>> ") (evalAndPrint env)
        

main :: IO ()
main = do 
    args <- getArgs
    if null args 
        then runRepl 
        else runOne $ args
