module Evaluator where

import qualified Data.IntMap as IntMap
import Control.Monad.Error

import LispData
import Parser
import Library



-- aplikuje liste argumentow do funkcji (jesli funkcja jest zdefiniowana przez urzytkownika,
-- korzysta z pomocniczej funkcji applyFunc)

apply :: EEnv -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ (Prim func) args = liftThrows $ func args
apply _ (IOPrim func) args = func args
apply eenv func@(Func _ _ _ _) args = applyFunc eenv func args
apply _ func _ = throwError $ NotFunction "Apply got non-function" (show func)

-- sprawdza liczbe ergumentow oraz wystepowanie erguemntow dodatkowych, 
-- parametry formalne zwiazuje z argumentami i tworzy nowe zmienne lokalne,
-- w tak zmienionym srodowiski wykonuje cialo funkcji

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
                                     
                

-- z podanych argumentow tworzy funkcje

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

-- tworzy funkcje domyslnie bez dodatkowych arguemntow

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

-- tworzy funkcje domyslnie z dodatkowymi argumentami

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs varargs = makeFunc $ Just (show varargs)



-- glowna funkcja ewaluatora (zachowuje sie odpowiednio w zaleznosci, czy argumentem jest zmienna, stala, 
-- forma specjalna, czy funkcja zdefiniowana przez uzytkownika)

eval :: EEnv -> LispVal -> IOThrowsError LispVal

-- pobiera wartosc zmiennej

eval eenv (Symbol id) = getVar (eeE eenv) id

-- stale zwraca niezmienione

eval _ val@(String _) = return val
eval _ val@(Integer _) = return val
eval _ val@(Float _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Char _) = return val
eval _ (List []) = return (List [])

-- zwraca niezmieniona wartosc

eval _ (List [Symbol "quote", val]) = return val 

-- po natrafieniu na 'quasiquote' wywoluje evalQQ, ktora przechodzi wszystkie podwyrazenia z args 
-- i wykonuje tylko te oznaczone przez 'unquote' lub 'unquote-splicing'
-- pozniej przekazuje wynik do liftSUnq, ktora splaszcza liste w miejscach oznaczonych przez evalQQ

eval eenv (List [Symbol "quasiquote", args]) = evalQQ (eeQLIncr eenv) args >>= liftSUnq

-- wywolanie 'unquote' oraz 'unquote-splicing' mozliwe tylko po wczesniejszym napotkaniu 'quasiquote'

eval _ (List (Symbol "unquote" : args)) = throwError $ Default ("Naked unquote form: " ++ show args)
eval _ (List (Symbol "unquote-splicing" : args)) = throwError $ Default ("Naked unquote-splicing form: " ++ show args)

-- aplikuje argumenty args do funkcji func

eval eenv (List (Symbol "apply" : func : args)) = evalApply eenv func args

-- ewaluuje wszystkie wyrazenia na liscie argumentow

eval eenv (List (Symbol "eval" : args)) = mapM (eval eenv) args >>= mapML (eval eenv)

eval eenv (List (Symbol "and" : args)) = evalAnd eenv args (Bool True)
eval eenv (List (Symbol "or" : args)) = evalOr eenv args (Bool False)

eval eenv (List [Symbol "if", pred, conseq, alt]) = evalIf eenv pred conseq alt
eval eenv (List (Symbol "cond" : args)) = evalCond eenv args

-- najpierw ewaluuje wyrazenie comp,  z ktorym wewnatrz evalCase beda porownywane poszczegolne podwyrazenia z args

eval eenv (List (Symbol "case" : comp : args)) = eval eenv comp >>= evalCase eenv args
                                  
-- definiuje nowa zmienna i inicjuje ja obliczona wartoscia val

eval eenv (List [Symbol "define", Symbol var, val]) = eval eenv val >>= defineVar (eeE eenv) var

-- oblicza wartosc zmiennej, a nastepnie przypisuje ja do nazwy
                          
eval eenv (List [Symbol "set!", Symbol var, val]) = eval eenv val >>= setVar (eeE eenv) var

eval eenv (List [Symbol "vector-set!", Symbol var, indice, obj]) = evalVectSet eenv var indice obj

-- tworzy i definiuje nowe funkcje o stalej/zmiennej liczbie argumentow

eval eenv (List (Symbol "define" : List (Symbol var : params) : body)) = makeNormalFunc (eeE eenv) params body >>= defineVar (eeE eenv) var
eval eenv (List (Symbol "define" : DottedList (Symbol var : params) varargs : body)) = makeVarargs varargs (eeE eenv) params body >>= defineVar (eeE eenv) var

-- tworzy funkcje anonimowe o stalej/zmiennej liczbie argumentow

eval eenv (List (Symbol "lambda" : List params : body)) = makeNormalFunc (eeE eenv) params body
eval eenv (List (Symbol "lambda" : DottedList params varargs : body)) = makeVarargs varargs (eeE eenv) params body
eval eenv (List (Symbol "lambda" : varargs@(Symbol _) : body)) = makeVarargs varargs (eeE eenv) [] body

-- rozne warianty let

eval eenv (List (Symbol "let" : List params : body)) = evalLet eenv Nothing params body
eval eenv (List (Symbol "let" : Symbol name : List params : body)) = evalLet eenv (Just name) params body
eval eenv (List (Symbol "let*" : List params : body)) = evalLet' eenv params body (eeE eenv)
eval eenv (List (Symbol "letrec" : List params : body)) = evalLetRec eenv params body (eeE eenv)   

-- ewaluuje wszystkie wyrazenia na liscie argumentow, zwraca ostatnie wykowane wyrazenie
    
eval eenv (List (Symbol "begin" : args)) = mapML (eval eenv) args

eval eenv (List (Symbol "do" : List params : List test : body)) = evalDo eenv params test body

-- ewalluuue arg do stringa zawierajacego nazwe pliku, ktory przekazuje do bibliotecznej funkcji 'load',
-- ta zas wczytuje z pliku o podanej nazwie liste wyrazen, ktore zostaja wykonane
-- ostatnie z nich zostaje zwrocone jako wynik eval

eval eenv (List [Symbol "load", arg]) = eval eenv arg >>= load >>= liftM last . mapM (eval eenv)

eval eenv list@(List _) = evalFunc eenv list

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm



-- liste argumentow bedacych wynikiem wykonania as aplikuje do funkcji bedacej wynikiem wykonania fn

evalApply :: EEnv -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalApply eenv fn as = do 
    func <- eval eenv fn
    args <- mapM (eval eenv) as
    applyProc (func:args) 
    where applyProc [func, List args] = apply eenv func args
          applyProc (func : args) = apply eenv func args
          
-- po pierwszym napotkaniu False zwraca False
        
evalAnd :: EEnv -> [LispVal] -> LispVal -> IOThrowsError LispVal
evalAnd _ [] ret = return ret
evalAnd eenv (t:ts) _ = do 
    result <- eval eenv t
    case result of
        Bool False -> return $ Bool False
        _ -> evalAnd eenv ts result
        
-- po pierwszym napotkaniu wartosci roznej od False zwraca ja
  
evalOr :: EEnv -> [LispVal] -> LispVal -> IOThrowsError LispVal
evalOr _ [] ret = return ret
evalOr eenv (t:ts) _ = do 
    result <- eval eenv t
    case result of
        Bool False -> evalOr eenv ts result
        _ -> return result
        
-- wykonuje pred, w przypadku jego nie spelnienia wykonuje alt, a w przeciwnym przypadku conseq

evalIf :: EEnv -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
evalIf eenv pred conseq alt = do 
    result <- eval eenv pred
    case result of
        Bool False -> eval eenv alt
        otherwise -> eval eenv conseq
        
-- wykonuje evalCondClause az do momentu spelnienia ktoregokolwiek z podwyrazen lub napotkania 'else'
 
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
        
-- wykonuje evalCaseClause az do napotkania wartosci comp w ktorymkolwiek z podwyrazen lub napotkania 'else'

evalCase :: EEnv -> [LispVal] -> LispVal -> IOThrowsError LispVal
evalCase _ [] _ = return $ Bool False
evalCase eenv (cl:cls) comp = do 
    (tst, val) <- evalCaseClause eenv cl comp
    if tst 
        then return val 
        else evalCase eenv cls comp
    where
        evalCaseClause eenv (List (Symbol "else" : args)) _ = do 
            ret <- mapML (eval eenv) args
            return (True, ret)
        evalCaseClause env (List (List vals : args)) comp =
            if match comp vals
                then do 
                    ret <- mapML (eval eenv) args
                    return (True, ret)
            else return (False, Bool False)
        evalCaseClause _ _ _ = return (False, Bool False)
        match comp (v:vs) = eqValue [comp, v] || match comp vs
        match _ [] = False
        
-- zmienia w wektorze o podanej nazwie na podanym indeksie wartosc na podana
    
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
        
-- let bez mozliwosci odwowylania sie do wczesniej zdefiniowanych w jego wnetrzu zmiennych
    
evalLet :: EEnv -> Maybe String -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
evalLet eenv maybeName params body = 
    case maybeName of
        Just name -> do
            env' <- liftIO (bindVars (eeE eenv) [(name, defaultValue)])
            func <- makeNormalFunc env' (map exN params) body
            setVar env' name func
            mapM (eval eenv . exV) params >>= apply eenv func
        Nothing -> do
            func <- makeNormalFunc (eeE eenv) (map exN params) body
            mapM (eval eenv . exV) params >>= apply eenv func
    where exN (List [Symbol var, _]) = Symbol var
          exV (List [Symbol _, val]) = val
          
-- let z mozliwosca odwowylania sie do wczesniej zdefiniowanych w jego wnetrzu zmiennych

evalLet' :: EEnv -> [LispVal] -> [LispVal] -> Env -> IOThrowsError LispVal
evalLet' eenv [] body env = do
    let eenv' = eeNewE eenv env 
    mapML (eval eenv') body
evalLet' eenv (p:ps) body env = do 
    let eenv' = eeNewE eenv env
    val <- eval eenv' (exV p)
    env' <- liftIO (bindVars env [(exN p, val)])
    evalLet' eenv ps body env' 
    where exN (List [Symbol var, _]) = var
          exV (List [Symbol _, val]) = val
          
-- let z mozliwosca rekurencyjjnego odwowylania sie do wczesniej zdefiniowanych w jego wnetrzu zmiennych
   
evalLetRec :: EEnv -> [LispVal] -> [LispVal] -> Env -> IOThrowsError LispVal
evalLetRec eenv params body env = do 
    let vars = map exN params
    env' <- liftIO (bindVars env vars)
    let eenv' = eeNewE eenv env'
    vals <- mapM (eval eenv' . exV) params
    let varvals = bind vars vals
    mapM_ (set env') varvals 
    mapML (eval eenv') body 
    where exN (List [Symbol var, _]) = (var, defaultValue)
          exV (List [Symbol _, val]) = val
          bind [] [] = []
          bind ((n, defaultValue):ns) (v:vs) = (n, v) : bind ns vs
          set env (n,v) = setVar env n v
          
-- petla wykonujaca sie dopoki wyrazenie test jest prawdziwe
        
evalDo :: EEnv -> [LispVal] -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
evalDo eenv params test body = do 
    let names = map exN params
    let steps = map exS params
    inits <- mapM (eval eenv . exI) params
    env' <- liftIO (bindVars (eeE eenv) (zip names inits))
    loop env' names test steps
    where loop env names (tst:rest) steps = do 
              let eenv' = eeNewE eenv env
              tval <- eval eenv' tst
              if isTrue tval
                  then mapML (eval eenv') rest
                  else do 
                      mapM_ (eval eenv') body
                      svals <- mapM (eval eenv') steps
                      mapM_ (set env) (zip names svals)
                      loop env names (tst:rest) steps
          exN (List [Symbol var, _]) = var
          exN (List [Symbol var, _, _]) = var
          exI (List [Symbol _, ini]) = ini
          exI (List [Symbol _, ini, _]) = ini
          exS (List [Symbol var, _]) = Symbol var
          exS (List [Symbol _, _, step]) = step
          set env (n, v) = setVar env n v
          
-- aplikuje zewaluowana liste argumentow do zewaluowanej funkcji

evalFunc :: EEnv -> LispVal -> IOThrowsError LispVal
evalFunc eenv (List (fn : as)) = do 
    func <- eval eenv fn
    args <- mapM (eval eenv) as
    apply eenv func args
    


-- domysla wartosc jeszcze niezainicjowanych zmiennych

defaultValue :: LispVal
defaultValue  = Bool False
    


-- oznaczenie zostawiane przez evalQQ do sygnalizacji koniecznosci splaszczenia wynikowej listy przez liftSUnq lub liftSUnq

unq :: String
unq = " unq"

-- splaszczaja liste po napotkaniu 'unq'

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
             else throwError $ Default "List unquote form in scalar context"
     else return l
liftSUnq v = return v

-- przechodzi przez wyszystkie podlisty rekurencyjnie i ewaluuje tylko te z nich, przy ktorych 
-- na wlasciwym poziomie (quotation level) sa symbole 'unquote' lub 'unquote-splicing'

evalQQ :: EEnv -> LispVal -> IOThrowsError LispVal
evalQQ eenv (List [Symbol "quasiquote", arg]) = do 
    val <- evalQQ (eeQLIncr eenv) arg >>= liftSUnq
    return $ List [Symbol "quasiquote", val]
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
        then return $ List (hd ++ getLst tl)
        else if isDLst tl
            then return $ DottedList (hd ++ getDLstH tl) (getDLstT tl)
            else return $ DottedList hd tl
evalQQ eenv (Vector n v) = do 
    vals <- mapM (evalQQ eenv) (IntMap.elems v)
    let len = (fromInteger n) :: Int
    let assoc = zip [0 .. (len - 1)] (liftLUnq vals)
    return $ Vector n (IntMap.fromList assoc)
evalQQ _ val@_ = return val



-- funkcja pomocnicza dla eval przechodzaca przez liste argumentow i wykonujaca na nich podana funkcje (zwraca ostatni wynik)

mapML :: (LispVal -> IOThrowsError LispVal) -> [LispVal] -> IOThrowsError LispVal
mapML fn as = mapML' (List []) fn as where 
    mapML' acc _ [] = return acc
    mapML' _ f (a:as) = do 
        res <- f a
        mapML' res f as
        
-- funkacja pomocnicza dla eval porownujaca strukturalnie poszczegolne wartosci LispVal

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

-- funkcje pomocnicze dla eval rozpakowujace poszczegolne wartosci LispVal

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
