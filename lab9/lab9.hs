import Control.Monad

-- problem 1

newtype StateComp state result =
	StateComp { perform :: state -> (state, result) } 

before :: StateComp state r1 -> (r1 -> StateComp state r2) -> StateComp state r2
StateComp f `before` h = 
	StateComp (\state0 ->
	let (state1, result1) = f state0
	in case h $ result1 of
		StateComp g -> g state1)

ret :: result -> StateComp state result
ret result = StateComp (\ state -> (state, result))

getState :: StateComp state state
getState = StateComp (\ state -> (state,state))

putState :: state -> StateComp state ()
putState state = StateComp (\ _ -> (state,()))

instance Monad (StateComp state) where
	(>>=) = before
	return = ret

-- data Expr = Number Integer | Neg Expr | Plus Expr Expr | Minus Expr Expr | Times Expr Expr

eval :: Expr -> Integer
eval expr = 
	case expr of
		Number i -> i
		Neg e -> - (eval e)
		Plus e1 e2 -> eval e1 + eval e2
		Minus e1 e2 -> eval e1 - eval e2
		Times e1 e2 -> eval e1 * eval e2

evalWithState :: Expr -> (Int, Integer)
evalWithState expr = eval 0 expr where
	eval n (Number i) = (n, i)
	eval n (Neg e) = 
		let (n', i) = eval n e
		in (n' + 1, -i)
	eval n (Plus e1 e2) = 
		let (n1, i1) = eval n e1
		    (n2, i2) = eval (n + n1) e2
		in (n2 + 1, i1 + i2)

evalWithImpliciteState :: Expr -> StateComp Int Integer
evalWithImpliciteState expr = eval expr where
	eval (Number i) = return i
	eval (Neg e) = eval e >>= \i -> getState >>= \n -> putState (n + 1) >> return (-i)
	eval (Plus e1 e2) = eval e1 >>= \i1 -> getState >>= \n -> putState (n + 1) >> eval e2 >>= \i2 -> return (i1 + i2)
	
evalWithImpliciteState' :: Expr -> StateComp Int Integer
evalWithImpliciteState' (Number i) = return i
evalWithImpliciteState' (Neg e) = do
	i <- evalWithImpliciteState' e
	n <- getState 
	putState (n + 1)
	return (-i)
evalWithImpliciteState' (Plus e1 e2) = do
	i1 <- evalWithImpliciteState' e1
	n <- getState 
	putState (n + 1)
	i2 <- evalWithImpliciteState' e2
	return (i1 + i2)

evaluate :: Expr -> (Int, Integer)	
evaluate expr = perform (evalWithImpliciteState' expr) 0


-- problem 2

data Expr = Number Integer | Neg Expr |
            Plus Expr Expr | Minus Expr Expr | Times Expr Expr |
            Div Expr Expr | Mod Expr Expr

evalWithExn :: Expr -> Maybe Integer
evalWithExn (Number i) = Just i
evalWithExn (Neg e) = 
  case evalWithExn e of
    Just i -> Just (- i)
    otherwise -> Nothing
evalWithExn (Plus e1 e2) = 
  case evalWithExn e1 of
    Just i1 ->
      case evalWithExn e2 of
        Just i2 -> Just (i1 + i2)
        otherwise -> Nothing
    otherwise -> Nothing
evalWithExn (Minus e1 e2) = 
  case evalWithExn e1 of
    Just i1 ->
      case evalWithExn e2 of
        Just i2 -> Just (i1 - i2)
        otherwise -> Nothing
    otherwise -> Nothing
evalWithExn (Times e1 e2) = 
  case evalWithExn e1 of
    Just i1 ->
      case evalWithExn e2 of
        Just i2 -> Just (i1 * i2)
        otherwise -> Nothing
    otherwise -> Nothing
evalWithExn (Div e1 e2) = 
  case evalWithExn e1 of
    Just i1 ->
      case evalWithExn e2 of
        Just i2 
          | i2 /= 0 -> Just (i1 `div` i2)
          | otherwise -> Nothing
        otherwise -> Nothing
    otherwise -> Nothing
evalWithExn (Mod e1 e2) = 
  case evalWithExn e1 of
    Just i1 ->
      case evalWithExn e2 of
        Just i2 
          | i2 /= 0 -> Just (i1 `mod` i2)
          | otherwise -> Nothing
        otherwise -> Nothing
    otherwise -> Nothing
    
evalWithExn' :: Monad m => Expr -> m Integer
evalWithExn' (Number i) = return i
evalWithExn' (Neg e) = do
  i <- evalWithExn' e
  return (- i)
evalWithExn' (Plus e1 e2) = do
  i1 <- evalWithExn' e1
  i2 <- evalWithExn' e2
  return (i1 + i2)
evalWithExn' (Minus e1 e2) = do
  i1 <- evalWithExn' e1
  i2 <- evalWithExn' e2
  return (i1 - i2)
evalWithExn' (Times e1 e2) = do
  i1 <- evalWithExn' e1
  i2 <- evalWithExn' e2
  return (i1 * i2)
evalWithExn' (Div e1 e2) = do
  i1 <- evalWithExn' e1
  i2 <- evalWithExn' e2
  if i2 /= 0 
    then  return (i1 `div` i2)
    else fail "Division by zero"
evalWithExn' (Mod e1 e2) = do
  i1 <- evalWithExn' e1
  i2 <- evalWithExn' e2
  if i2 /= 0 
    then  return (i1 `mod` i2)
    else fail "Division by zero"
    

-- problem 3
    
newtype StateCompWithExn m state result =
   StateCompWithExn { performStateCompWithExn :: state -> (state, m result) }
   
before1 :: StateCompWithExn Maybe state r1 -> (r1 -> StateCompWithExn Maybe state r2) -> StateCompWithExn Maybe state r2
StateCompWithExn f `before1` h = StateCompWithExn (\state0 -> 
  let (state1, result1) = f state0
  in case result1 of 
    Just res1 -> performStateCompWithExn (h res1) state1                
    Nothing -> (state1, Nothing))
    
ret1 :: result -> StateCompWithExn Maybe state result
ret1 result = StateCompWithExn (\state -> (state, Just result))
   
instance Monad (StateCompWithExn Maybe state) where
  (>>=) = before1
  return = ret1
  
getState1 :: StateCompWithExn Maybe state state
getState1 = StateCompWithExn (\state -> (state, Just state))

putState1 :: state -> StateCompWithExn Maybe state ()
putState1 state = StateCompWithExn (\ _ -> (state, Just ()))

fail1 :: StateCompWithExn Maybe state result
fail1 = StateCompWithExn (\state -> (state, Nothing))
   
evalWithImplicitStateAndExn :: Expr -> StateCompWithExn Maybe Int Integer
evalWithImplicitStateAndExn expr = eval expr where
  eval (Number i) = return i
  eval (Neg e) = do
    i <- evalWithImplicitStateAndExn e
    state <- getState1
    putState1 (state + 1)
    return i
  eval (Plus e1 e2) = do
    i1 <- evalWithImplicitStateAndExn e1
    i2 <- evalWithImplicitStateAndExn e2
    state <- getState1
    putState1 (state + 1)
    return (i1 + i2)
  eval (Div e1 e2) = do
    i1 <- evalWithImplicitStateAndExn e1
    i2 <- evalWithImplicitStateAndExn e2
    state <- getState1
    if i2 /= 0 
      then do 
        putState1 (state + 1)
        return (i1 `div` i2)
      else fail1

evaluate1 :: Expr -> (Int, Maybe Integer)      
evaluate1 expr = performStateCompWithExn (evalWithImplicitStateAndExn expr) 0

newtype ExnWithStateComp m state result =
   ExnWithStateComp { performExnWithStateComp :: state -> m (state, result) }
   
before2 :: Monad m => ExnWithStateComp m state r1 -> (r1 -> ExnWithStateComp m state r2) -> ExnWithStateComp m state r2
ExnWithStateComp f `before2` h = ExnWithStateComp (\state0 -> do
  (state1, result1) <- f state0
  performExnWithStateComp (h result1) state1)                
    
ret2 :: Monad m => result -> ExnWithStateComp m state result
ret2 result = ExnWithStateComp (\state -> return (state, result))
   
instance Monad m => Monad (ExnWithStateComp m state) where
  (>>=) = before2
  return = ret2
  
getState2 :: Monad m => ExnWithStateComp m state state
getState2 = ExnWithStateComp (\state -> return (state, state))

putState2 :: Monad m => state -> ExnWithStateComp m state ()
putState2 state = ExnWithStateComp (\ _ -> return (state, ()))
   
evalWithExnAndImplicitState :: Monad m => Expr -> ExnWithStateComp m Int Integer
evalWithExnAndImplicitState expr = eval expr where
  eval (Number i) = return i
  eval (Neg e) = do
    i <- eval e
    n <- getState2
    putState2 (n + 1)
    return (- i)
  eval (Plus e1 e2) = do
    i1 <- eval e1
    i2 <- eval e2
    n <- getState2
    putState2 (n + 1)
    return (i1 + i2)
  eval (Div e1 e2) = do
    i1 <- eval e1
    i2 <- eval e2
    n <- getState2
    if i2 /= 0 
      then do
        putState2 (n + 1)
        return (i1 `div` i2)
      else 
        fail "Division by zero"

evaluate2 :: Expr -> Maybe (Int, Integer)      
evaluate2 expr = performExnWithStateComp (evalWithExnAndImplicitState expr) 0
