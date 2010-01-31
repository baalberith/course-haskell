module Evaluator where

import qualified Data.IntMap as IntMap
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
apply _ (Prim func) args = liftThrows $ func args
apply _ (IOPrim func) args = func args
apply eenv func@(Func _ _ _ _) args = applyFunc eenv func args
apply _ func _ = throwError $ NotFunction "Apply got non-function" (show func)

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


eval :: EEnv -> LispVal -> IOThrowsError LispVal
eval eenv (Symbol id) = getVar (eeE eenv) id

eval _ val@(String _) = return val
eval _ val@(Integer _) = return val
eval _ val@(Float _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Char _) = return val

eval _ (List [Symbol "quote", val]) = return val 
eval eenv (List [Symbol "quasiquote", val]) = evalQQ (eeQLIncr eenv) val >>= liftSUnq
eval _ (List (Symbol "unquote" : args)) = throwError $ Default ("Naked unquote form: " ++ show args)
eval _ (List (Symbol "unquote-splicing" : args)) = throwError $ Default ("Naked unquote-splicing form: " ++ show args)

eval eenv (List (Symbol "apply" : fn : args)) = evalApply eenv fn args
eval eenv (List (Symbol "eval" : args)) = mapM (eval eenv) args >>= mapML (eval eenv)

eval eenv (List (Symbol "and" : args)) = evalAnd eenv args (Bool True)
eval eenv (List (Symbol "or" : args)) = evalOr eenv args (Bool False)

eval eenv (List [Symbol "if", pred, conseq, alt]) = evalIf eenv pred conseq alt
eval _ (List [Symbol "cond"]) = return $ Bool False
eval eenv (List (Symbol "cond" : args)) = evalCond eenv args
eval _ (List [Symbol "case"]) = return $ Bool False
eval eenv (List (Symbol "case" : k : as)) = eval eenv k >>= evalCase eenv as
                          
eval eenv (List [Symbol "set!", Symbol var, val]) = eval eenv val >>= setVar (eeE eenv) var
eval eenv (List [Symbol "vector-set!", Symbol var, indice, obj]) = evalVectSet eenv var indice obj
                                  
eval eenv (List [Symbol "define", Symbol var, val]) = eval eenv val >>= defineVar (eeE eenv) var
eval eenv (List (Symbol "define" : List (Symbol var : params) : body)) = makeNormalFunc (eeE eenv) params body >>= defineVar (eeE eenv) var
eval eenv (List (Symbol "define" : DottedList (Symbol var : params) varargs : body)) = makeVarargs varargs (eeE eenv) params body >>= defineVar (eeE eenv) var

eval eenv (List (Symbol "lambda" : List params : body)) = makeNormalFunc (eeE eenv) params body
eval eenv (List (Symbol "lambda" : DottedList params varargs : body)) = makeVarargs varargs (eeE eenv) params body
eval eenv (List (Symbol "lambda" : varargs@(Symbol _) : body)) = makeVarargs varargs (eeE eenv) [] body

eval eenv (List (Symbol "let" : List params : body)) = evalLet eenv Nothing params body
eval eenv (List (Symbol "let" : Symbol name : List params : body)) = evalLet eenv (Just name) params body
eval eenv (List (Symbol "let*" : List params : body)) = evalLet' eenv params body (eeE eenv)
eval eenv (List (Symbol "letrec" : List params : body)) = evalLetRec eenv params body (eeE eenv)    
    
eval eenv (List (Symbol "begin" : args)) = mapML (eval eenv) args
eval eenv (List (Symbol "do" : List params : List test : body)) = evalDo eenv params test body

eval eenv (List [Symbol "load", arg]) = eval eenv arg >>= load >>= liftM last . mapM (eval eenv)

eval eenv list@(List _) = evalFunc eenv list
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


evalApply :: EEnv -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalApply eenv fn args = do 
    func <- eval eenv fn
    argVals <- mapM (eval eenv) args
    applyProc (func:argVals) 
    where applyProc [func, List args] = apply eenv func args
          applyProc (func : args) = apply eenv func args
        
evalAnd :: EEnv -> [LispVal] -> LispVal -> IOThrowsError LispVal
evalAnd _ [] ret = return ret
evalAnd eenv (t:ts) _ = do 
    result <- eval eenv t
    case result of
        Bool False -> return $ Bool False
        _ -> evalAnd eenv ts result
  
evalOr :: EEnv -> [LispVal] -> LispVal -> IOThrowsError LispVal
evalOr _ [] ret = return ret
evalOr eenv (t:ts) _ = do 
    result <- eval eenv t
    case result of
        Bool False -> evalOr eenv ts result
        _ -> return result

evalIf :: EEnv -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalIf eenv pred conseq alt = do 
    result <- eval eenv pred
    case result of
        Bool False -> eval eenv alt
        otherwise -> eval eenv conseq
 
evalCond :: EEnv -> [LispVal] -> IOThrowsError LispVal
evalCond _ [] = return $ Bool False
evalCond eenv (cl:cls) = do 
    (tst, val) <- evalCondClause eenv cl
    if tst 
        then return val 
        else evalCond eenv cls 
    where 
        evalCondClause eenv (List (Symbol "else" : as)) = do 
            ret <- mapML (eval eenv) as
            return (True, ret)
        evalCondClause eenv (List (pr : as)) = do 
            tst <- eval eenv pr
            case tst of
                Bool False -> return (False, Bool False)
                _ -> do 
                    ret <- mapML (eval eenv) as
                    return (True, ret)
        evalCondClause _ _ = return (False, Bool False)

evalCase :: EEnv -> [LispVal] -> LispVal -> IOThrowsError LispVal
evalCase _ [] _ = return $ Bool False
evalCase eenv (cl:cls) k = do 
    (tst, val) <- evalCaseClause eenv cl k
    if tst 
        then return val 
        else evalCase eenv cls k 
    where
        evalCaseClause eenv (List (Symbol "else" : args)) _ = do 
            ret <- mapML (eval eenv) args
            return (True, ret)
        evalCaseClause env (List (List vals : args)) k =
            if match k vals
                then do 
                    ret <- mapML (eval eenv) args
                    return (True, ret)
            else return (False, Bool False)
        evalCaseClause _ _ _ = return (False, Bool False)
        match k (v:vs) = eqValue [k, v] || match k vs
        match _ [] = False
    
evalVectSet :: EEnv -> String -> LispVal -> LispVal -> IOThrowsError LispVal
evalVectSet eenv var indice obj = do 
    vec <- eval eenv (Symbol var)
    indx <- eval eenv indice
    if isVect vec && isInt indx
        then do
            let i = getInt indx
            let len = getVectL vec
            if i >= 0 && i < len
                then do 
                    val <- eval eenv obj
                    let vect = Vector len (IntMap.insert (fromInteger i) val (getVectV vec))
                    setVar (eeE eenv) var vect
                else throwError $ VectorBounds len i
        else throwError $ Default "Bad vector-set! form"
    
evalLet :: EEnv -> Maybe String -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
evalLet eenv maybeName params body = 
    case maybeName of
        Just name -> do
            env' <- liftIO (bindVars (eeE eenv) [(name, defaultValue)])
            func <- makeNormalFunc env' (map exn params) body
            setVar env' name func
            mapM (eval eenv . exv) params >>= apply eenv func
        Nothing -> do
            func <- makeNormalFunc (eeE eenv) (map exn params) body
            mapM (eval eenv . exv) params >>= apply eenv func
    where exn (List [Symbol var, _]) = Symbol var
          exv (List [Symbol _, val]) = val

evalLet' :: EEnv -> [LispVal] -> [LispVal] -> Env -> IOThrowsError LispVal
evalLet' eenv [] body env = do
    let eenv' = eeNewE eenv env 
    mapML (eval eenv') body
evalLet' eenv (p:ps) body env = do 
    let eenv' = eeNewE eenv env
    val <- eval eenv' (exv p)
    env' <- liftIO (bindVars env [(exn p, val)])
    evalLet' eenv ps body env' 
    where exn (List [Symbol var, _]) = var
          exv (List [Symbol _, val]) = val
   
evalLetRec :: EEnv -> [LispVal] -> [LispVal] -> Env -> IOThrowsError LispVal
evalLetRec eenv params body env = do 
    let vars = map exn params
    env' <- liftIO (bindVars env vars)
    let eenv' = eeNewE eenv env'
    vals <- mapM (eval eenv' . exv) params
    let varvals = bind vars vals
    mapM_ (set env') varvals 
    mapML (eval eenv') body 
    where exn (List [Symbol var, _]) = (var, defaultValue)
          exv (List [Symbol _, val]) = val
          bind [] [] = []
          bind ((n, defaultValue):ns) (v:vs) = (n, v) : bind ns vs
          set env (n,v) = setVar env n v
        
evalDo :: EEnv -> [LispVal] -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
evalDo eenv params test body = do 
    let names = map exn params
    let steps = map exs params
    inits <- mapM (eval eenv . exi) params
    env' <- liftIO (bindVars (eeE eenv) (zip names inits))
    loop env' names test steps
    where loop env names (tst:rets) steps = do 
              let eenv' = eeNewE eenv env
              tval <- eval eenv' tst
              if isTrue tval
                  then mapML (eval eenv') rets
                  else do 
                      mapM_ (eval eenv') body
                      svals <- mapM (eval eenv') steps
                      mapM_ (set env) (zip names svals)
                      loop env names (tst:rets) steps
          exn (List [Symbol var, _]) = var
          exn (List [Symbol var, _, _]) = var
          exi (List [Symbol _, ini]) = ini
          exi (List [Symbol _, ini, _]) = ini
          exs (List [Symbol var, _]) = Symbol var
          exs (List [Symbol _, _, step]) = step
          set env (n, v) = setVar env n v
          

evalFunc :: EEnv -> LispVal -> IOThrowsError LispVal
evalFunc eenv (List (function : args)) = do 
    func <- eval eenv function
    argVals <- mapM (eval eenv) args
    apply eenv func argVals
    

unq :: String
unq = " unq"

liftLUnq :: [LispVal] -> [LispVal]
liftLUnq lst = liftLUnq' [] lst where
    liftLUnq' acc [] = acc
    liftLUnq' acc (l@(List ((Symbol sym):vals)):ls) =
        if sym == unq
            then liftLUnq' (acc ++ vals) ls
            else liftLUnq' (acc ++ [l]) ls
    liftLUnq' acc (l:ls) = liftLUnq' (acc ++ [l]) ls
    
liftSUnq :: LispVal -> IOThrowsError LispVal
liftSUnq l@(List ((Symbol sym):vals)) =
  if sym == unq
     then if length vals == 1
             then return (head vals)
             else throwError $ Default "list unquote form in scalar context"
     else return l
liftSUnq v = return v

evalQQ :: EEnv -> LispVal -> IOThrowsError LispVal
evalQQ eenv (List [Symbol "quasiquote", arg]) = do 
    val <- evalQQ (eeQLIncr eenv) arg >>= liftSUnq
    return (List [Symbol "quasiquote", val])
evalQQ eenv (List (Symbol "unquote" : args)) =
    if eeQL eenv == 1
        then do 
            vals <- mapM (eval eenv') args
            return $ List (Symbol unq : vals)
        else do 
            vals <- mapM (evalQQ eenv') args
            return $ List (Symbol "unquote" : liftLUnq vals)
    where eenv' = eeQLDecr eenv
evalQQ eenv (List (Symbol "unquote-splicing" : args)) =
    if eeQL eenv == 1
        then do 
            vals <- mapM (eval eenv') args
            if isLL vals
                then return $ List (Symbol unq : peel vals)
                else throwError $ Default ("Bad unquote-splicing form: " ++ show args)
        else do 
            vals <- mapM (evalQQ eenv') args
            return $ List (Symbol "unquote-splicing" : liftLUnq vals)
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
    if isLst tl
        then return (List (hd ++ getLst tl))
        else if isDLst tl
            then return (DottedList (hd ++ getDLstH tl) (getDLstT tl))
            else return (DottedList hd tl)
evalQQ eenv (Vector n v) = do 
    vals <- mapM (evalQQ eenv) (IntMap.elems v)
    let len = (fromInteger n) :: Int
    let assoc = zip [0 .. (len - 1)] (liftLUnq vals)
    return $ Vector n (IntMap.fromList assoc)
evalQQ _ val@_ = return val

defaultValue :: LispVal
defaultValue  = Bool False
        
isTrue :: LispVal -> Bool
isTrue (Bool False) = False
isTrue _ = True

isInt (Integer _) = True
isInt _ = False

getInt :: LispVal -> Integer
getInt (Integer n) = n

isLst (List _) = True
isLst _ = False

getLst :: LispVal -> [LispVal]
getLst (List l) = l

isDLst (DottedList _ _) = True
isDLst _ = False

getDLstH :: LispVal -> [LispVal]
getDLstH (DottedList h _) = h

getDLstT :: LispVal -> LispVal
getDLstT (DottedList _ t) = t

isVect (Vector _ _) = True
isVect _ = False

getVectL :: LispVal -> Integer
getVectL (Vector l _) = l

getVectV :: LispVal -> IntMap.IntMap LispVal
getVectV (Vector _ v) = v

mapML :: Monad m => (a -> m LispVal) -> [a] -> m LispVal
mapML fn lst = mapML' (List []) fn lst where 
    mapML' acc _ [] = return acc
    mapML' _ f (x:xs) = do 
        res <- f x
        mapML' res f xs

eqValue :: [LispVal] -> Bool
eqValue [(Bool v1), (Bool v2)] = v1 == v2
eqValue [(Char c1), (Char c2)] = c1 == c2
eqValue [(Integer v1), (Integer v2)] = v1 == v2
eqValue [(Float v1), (Float v2)] = v1 == v2 
eqValue [(String v1), (String v2)] = v1 == v2
eqValue [(Symbol v1), (Symbol v2)] = v1 == v2
eqValue [(DottedList l1 t1), (DottedList l2 t2)] = eqValue [List (l1 ++ [t1]), List (l2 ++ [t2])]
eqValue [(List l1), (List l2)] = length l1 == length l2 && all eqvPair (zip l1 l2) 
    where eqvPair (x1, x2) = eqValue [x1, x2]
eqValue [(Vector l1 v1), (Vector l2 v2)] = l1 == l2 && eqValue [List (IntMap.elems v1), List (IntMap.elems v2)] 
eqValue _ = False


primitiveBindings :: IO Env
primitiveBindings = do
    env <- nullEnv
    bindVars env $ map (makeFunc IOPrim) ioPrimitives ++ map (makeFunc Prim) primitives where 
        makeFunc constructor (var, func) = (var, constructor func)
    