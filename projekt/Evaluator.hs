module Evaluator where

import Control.Monad.Error

import LispData
import Parser
import Library
                                     

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs varargs = makeFunc $ Just (show varargs)

        
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply func@(Func _ _ _ _) args = applyFunc func args
apply func _ = throwError $ NotFunction "apply got non-function" (show func)

-- applyFunc :: LispVal -> [LispVal] -> IOThrowsError LispVal       
-- applyFunc (Func params varargs body closure) args = 
--     if num params /= num args && varargs == Nothing
--         then throwError $ NumArgs (num params) args
--         else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody where 
--             num = toInteger . length
--             remainingArgs = drop (length params) args
--             bindVarArgs arg env = case arg of
--                 Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
--                 Nothing -> return env     
--             evalBody env = liftM last $ mapM (eval env) body 

applyFunc :: LispVal -> [LispVal] -> IOThrowsError LispVal       
applyFunc (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else do
            let bindings = zip params args
            eenv <- liftIO $ bindVars closure bindings >>= bindVarArgs varargs 
            evalBody eenv where 
                num = toInteger . length
                remainingArgs = drop (length params) args
                bindVarArgs arg env = case arg of
                    Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                    Nothing -> return env     
                evalBody env = liftM last $ mapM (eval env) body 
                

specialForms :: [String]
specialForms = ["and", "apply", "begin", "case", "cond", "define",
                "defmacro", "delay", "do", "eval", "force", "gensym",
                "guard", "if", "lambda", "let", "let*", "letrec",
                "letrec*", "load", "or", "quasiquote", "quote", "set!",
                "unquote", "unquote-splicing", "vector-fill!",
                "vector-set!", "reset", "shift ",
                "trace", "dump-bindings"]

isSpecialForm :: LispVal -> Bool
isSpecialForm (Atom sym) = seek sym specialForms where 
    seek _ [] = False
    seek s (sf:sfs) = (s == sf) || seek s sfs
isSpecialForm _ = False
            

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (Atom id) = getVar env id
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
-- eval env val@(Ratio _) = return val
-- eval env val@(Complex _) = return val
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
    

-- primitiveBindings :: IO Env
-- primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives) where 
--     makeFunc constructor (var, func) = (var, constructor func)

primitiveBindings :: IO Env
primitiveBindings = do
    env <- nullEnv
    bindVars env $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives where 
        makeFunc constructor (var, func) = (var, constructor func)
    