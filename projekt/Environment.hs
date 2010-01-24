module Environment where

import Control.Monad.Error
import Data.IORef

import LispData



nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do 
    env <- liftIO $ readIORef envRef
    case lookup var env of
        Just e -> liftIO $ readIORef e
        Nothing -> throwError $ UnboundVar "Getting an unbound variable: " var
        
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do 
    env <- liftIO $ readIORef envRef
    case lookup var env of
             Just e -> liftIO $ writeIORef e value
             Nothing -> throwError $ UnboundVar "Setting an unbound variable: " var
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value
          
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef where 
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do 
        ref <- newIORef value
        return (var, ref)
        