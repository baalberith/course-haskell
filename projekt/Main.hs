module Main where

import System.Environment
import Control.Monad.Error
import IO hiding (try)

import LispData
import Parser
import Evaluator
import Environment

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- evalString :: String -> IO String
-- evalString arg = return $ extractValue $ trapError (liftM show $ readExpr arg >>= eval) 

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
  result <- prompt
  if pred result 
     then return ()
     else action result >> until_ pred prompt action
     
-- runOne :: String -> IO ()
-- runOne expr = primitiveBindings >>= flip evalAndPrint expr

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr
     
-- runRepl :: IO ()
-- runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
        
-- main :: IO ()
-- main = do 
--     args <- getArgs
--     case length args of
--         0 -> runRepl
--         1 -> runOne $ head args
--         otherwise -> putStrLn "Program takes only 0 or 1 argument"

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

    