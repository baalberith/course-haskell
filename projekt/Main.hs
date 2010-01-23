module Main where

import System.Environment
import Control.Monad.Error

import LispData
import Parser
import Evaluator
    
main :: IO ()
main = do
    args <- getArgs
    let arg = head args
    let evaled = liftM show $ readExpr arg >>= eval
    putStrLn $ extractValue $ trapError evaled

    