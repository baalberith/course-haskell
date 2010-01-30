module Evaluator where

import Control.Monad.Error
import Data.Array

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
eval eenv (List [Atom "quasiquote", val]) = evalQQ (eeQLIncr eenv) val >>= liftSUnq
eval _ (List (Atom "unquote" : args)) = throwError $ Default ("naked unquote form: " ++ show args)
eval _ (List (Atom "unquote-splicing" : args)) = throwError $ Default ("naked unquote-splicing form: " ++ show args)

eval eenv (List [Atom "apply", fn, args]) = do { (List args') <- eval eenv args; evalFunc eenv $ List (fn:args') }
eval eenv (List (Atom "eval" : args)) = mapM (eval eenv) args >>= mapML (eval eenv)

eval eenv (List (Atom "begin" : args)) = mapML (eval eenv) args

eval eenv (List [Atom "if", pred, conseq, alt]) = evalIf eenv pred conseq alt
                          
eval eenv (List [Atom "set!", Atom var, val]) = eval eenv val >>= setVar (eeE eenv) var

eval eenv (List [Atom "define", Atom var, val]) = eval eenv val >>= defineVar (eeE eenv) var
eval eenv (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc (eeE eenv) params body >>= defineVar (eeE eenv) var
eval eenv (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarargs varargs (eeE eenv) params body >>= defineVar (eeE eenv) var

eval eenv (List (Atom "lambda" : List params : body)) = makeNormalFunc (eeE eenv) params body
eval eenv (List (Atom "lambda" : DottedList params varargs : body)) = makeVarargs varargs (eeE eenv) params body
eval eenv (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarargs varargs (eeE eenv) [] body

eval eenv (List [Atom "load", arg]) = eval eenv arg >>= load >>= liftM last . mapM (eval eenv)

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
        

isList (List _) = True
isList _ = False

getList :: LispVal -> [LispVal]
getList (List l) = l

isDL (DottedList _ _) = True
isDL _ = False

getDLh :: LispVal -> [LispVal]
getDLh (DottedList h _) = h

getDLt :: LispVal -> LispVal
getDLt (DottedList _ t) = t

unq :: String
unq = " unq"

liftLUnq :: [LispVal] -> [LispVal]
liftLUnq lst = liftLUnq' [] lst where
    liftLUnq' acc [] = acc
    liftLUnq' acc (l@(List ((Atom sym):vals)):ls) =
        if sym == unq
            then liftLUnq' (acc ++ vals) ls
            else liftLUnq' (acc ++ [l]) ls
    liftLUnq' acc (l:ls) = liftLUnq' (acc ++ [l]) ls
    
liftSUnq :: LispVal -> IOThrowsError LispVal
liftSUnq l@(List ((Atom sym):vals)) =
  if sym == unq
     then if length vals == 1
             then return (head vals)
             else throwError $ Default "list unquote form in scalar context"
     else return l
liftSUnq v = return v

evalQQ :: EEnv -> LispVal -> IOThrowsError LispVal
evalQQ eenv (List [Atom "quasiquote", arg]) = do 
    val <- evalQQ (eeQLIncr eenv) arg >>= liftSUnq
    return (List [Atom "quasiquote", val])
evalQQ eenv (List (Atom "unquote" : args)) =
    if eeQL eenv == 1
        then do 
            vals <- mapM (eval eenv') args
            return $ List (Atom unq : vals)
        else do 
            vals <- mapM (evalQQ eenv') args
            return $ List (Atom "unquote" : liftLUnq vals)
    where eenv' = eeQLDecr eenv
evalQQ eenv (List (Atom "unquote-splicing" : args)) =
    if eeQL eenv == 1
        then do 
            vals <- mapM (eval eenv') args
            if isLL vals
                then return $ List (Atom unq : peel vals)
                else throwError $ Default ("bad unquote-splicing form: " ++ show args)
        else do 
            vals <- mapM (evalQQ eenv') args
            return $ List (Atom "unquote-splicing" : liftLUnq vals)
  where eenv' = eeQLDecr eenv
        isLL [] = True
        isLL ((List _):ls) = isLL ls
        isLL _ = False
        peel [] = []
        peel ((List l):ls) = l ++ peel ls
evalQQ eenv (List con) = do
    vals <- mapM (evalQQ eenv) con 
    return $ List (liftLUnq vals)
evalQQ eenv (DottedList con cab) = do 
    vals <- mapM (evalQQ eenv) con
    tl <- evalQQ eenv cab >>= liftSUnq
    let hd = liftLUnq vals
    if isList tl
        then return (List (hd ++ getList tl))
        else if isDL tl
            then return (DottedList (hd ++ getDLh tl) (getDLt tl))
            else return (DottedList hd tl)
evalQQ eenv (Vector v) = do 
    vals <- mapM (evalQQ eenv) (elems v)
    return $ Vector (listArray (bounds v) (liftLUnq vals))
evalQQ _ val@_ = return val


-- primitiveBindings :: IO Env
-- primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives) where 
--     makeFunc constructor (var, func) = (var, constructor func)

primitiveBindings :: IO Env
primitiveBindings = do
    env <- nullEnv
    bindVars env $ map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives where 
        makeFunc constructor (var, func) = (var, constructor func)
    