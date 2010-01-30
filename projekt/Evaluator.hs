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

        
apply :: EEnv -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ (PrimitiveFunc func) args = liftThrows $ func args
apply _ (IOFunc func) args = func args
apply eenv func@(Func _ _ _ _) args = applyFunc eenv func args
apply _ func _ = throwError $ NotFunction "apply got non-function" (show func)

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

applyFunc :: EEnv -> LispVal -> [LispVal] -> IOThrowsError LispVal       
applyFunc eenv (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else do
            let bindings = zip params args
            env' <- liftIO $ bindVars closure bindings >>= bindVarArgs varargs 
            let eenv' = eeNewE eenv env'
            evalBody eenv' where 
                num = toInteger . length
                remainingArgs = drop (length params) args
                bindVarArgs arg env = case arg of
                    Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                    Nothing -> return env     
                evalBody eenv = liftM last $ mapM (eval eenv) body 
                

specialForms :: [String]
specialForms = ["and", "apply", "begin", "case", "cond", "define",
                "defmacro", "delay", "do", "eval", "force", "gensym",
                "guard", "if", "lambda", "let", "let*", "letrec",
                "letrec*", "load", "or", "quasiquote", "quote", "set!",
                "unquote", "unquote-splicing", "vector-fill!",
                "vector-set!", "reset", "shift ",
                "trace", "dump-bindings"]

-- isSpecialForm :: LispVal -> Bool
-- isSpecialForm (Atom sym) = seek sym specialForms where 
--     seek _ [] = False
--     seek s (sf:sfs) = (s == sf) || seek s sfs
-- isSpecialForm _ = False
            

eval :: EEnv -> LispVal -> IOThrowsError LispVal
eval eenv (Atom id) = getVar (eeE eenv) id
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Float _) = return val
-- eval _ val@(Ratio _) = return val
-- eval _ val@(Complex _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Character _) = return val
eval _ (List [Atom "quote", val]) = return val 
eval eenv (List [Atom "apply", fn, args]) = do { (List args') <- eval eenv args; evalFunc eenv $ List (fn:args') }
eval eenv (List (Atom "eval" : args)) = mapM (eval eenv) args >>= mapML (eval eenv)
eval eenv (List [Atom "if", pred, conseq, alt]) = evalIf eenv pred conseq alt
eval eenv (List [Atom "set!", Atom var, form]) = eval eenv form >>= setVar (eeE eenv) var
eval eenv (List [Atom "define", Atom var, form]) = eval eenv form >>= defineVar (eeE eenv) var
eval eenv (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc (eeE eenv) params body >>= defineVar (eeE eenv) var
eval eenv (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs (eeE eenv) params body >>= defineVar (eeE eenv) var
eval eenv (List (Atom "lambda" : List params : body)) = makeNormalFunc (eeE eenv) params body
eval eenv (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs (eeE eenv) params body
eval eenv (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs (eeE eenv) [] body
eval eenv (List [Atom "load", String filename]) = load filename >>= liftM last . mapM (eval eenv)
eval eenv list@(List _) = evalFunc eenv list
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


evalIf :: EEnv -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalIf eenv pred conseq alt = do 
    result <- eval eenv pred
    case result of
        Bool False -> eval eenv alt
        otherwise -> eval eenv conseq

evalFunc :: EEnv -> LispVal -> IOThrowsError LispVal
evalFunc eenv (List (function : args)) = do 
    func <- eval eenv function
    argVals <- mapM (eval eenv) args
    apply eenv func argVals
    

mapML :: Monad m => (a -> m LispVal) -> [a] -> m LispVal
mapML fn lst = mapML' (List []) fn lst where 
    mapML' acc _ [] = return acc
    mapML' _ f (x:xs) = do 
        res <- f x
        mapML' res f xs
    

-- primitiveBindings :: IO Env
-- primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives) where 
--     makeFunc constructor (var, func) = (var, constructor func)

primitiveBindings :: IO Env
primitiveBindings = do
    env <- nullEnv
    bindVars env $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives where 
        makeFunc constructor (var, func) = (var, constructor func)
    