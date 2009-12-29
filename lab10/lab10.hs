data ErrOrResult result = Error String | Result result deriving Show

instance Monad ErrOrResult where
  (Error s) >>= _ = Error s
  (Result r) >>= h = h r
  return r = Result r
  fail s = Error s
  
newtype StateCompT m result = StateCompT { performStateCompT :: Integer -> m (Integer, result) }

instance Monad m => Monad (StateCompT m)  where
  (StateCompT f) >>= h = StateCompT (\state -> do
    (state1, result1) <- f state
    performStateCompT (h result1) state1)
  return result = StateCompT (\state -> return (state, result))
  
tick :: Monad m => StateCompT m ()
tick = StateCompT (\state -> return (state + 1, ()))

promote :: Monad m => m a -> StateCompT m a
promote m = StateCompT (\state -> do
  result <- m
  return (state, result))
        

newtype StateComp result = StateComp { performStateComp :: Integer -> (Integer, result) } 

instance Monad StateComp where
  StateComp f >>= h = StateComp (\state -> do
    let (state1, result1) = f state 
    performStateComp (h result1) state1)
  return result = StateComp (\ state -> (state, result))
  
newtype ErrOrResultT m result = ErrOrResultT { runErrOrResultT :: m (ErrOrResult result) }

instance Monad m => Monad (ErrOrResultT m) where
  m >>= f = ErrOrResultT $ do
    r <- runErrOrResultT m 
    case r of
      Error s -> return (Error s)
      Result v -> runErrOrResultT $ f v
  return r = ErrOrResultT $ return (Result r)
  fail s = ErrOrResultT $ return (Error s)
  
tick' :: StateComp ()
tick' = StateComp (\state -> (state + 1, ()))

promote' :: (Monad m) => m result -> ErrOrResultT m result
promote' m = ErrOrResultT $ do
  a <- m
  return (Result a)
  

data Expr = Number Integer | Neg Expr 
          | Expr :+: Expr | Expr :-: Expr | Expr :*: Expr 
          | Expr :/: Expr | Expr :%: Expr
 
evalStateComp :: Expr -> (StateCompT ErrOrResult) Integer
evalStateComp (Number i) = return i
evalStateComp (Neg e) = do
  i <- evalStateComp e
  tick
  return (- i)
evalStateComp (e1 :+: e2) = do
  i1 <- evalStateComp e1
  i2 <- evalStateComp e2
  tick
  return (i1 + i2)
evalStateComp (e1 :/: e2) = do
  i1 <- evalStateComp e1
  i2 <- evalStateComp e2
  if i2 /= 0
    then do
      tick
      return (i1 `div` i2)    
    else promote $ fail "Division by zero!"
                
evaluateSC :: Expr -> ErrOrResult (Integer, Integer)      
evaluateSC expr = performStateCompT (evalStateComp expr) 0

evalErrOrResult :: Expr -> ErrOrResultT StateComp Integer
evalErrOrResult (Number i) = return i
evalErrOrResult (Neg e) = do
  i <- evalErrOrResult e
  promote' tick'
  return (- i)
evalErrOrResult (e1 :+: e2) = do
  i1 <- evalErrOrResult e1
  i2 <- evalErrOrResult e2
  promote' tick'
  return (i1 + i2)
evalErrOrResult (e1 :/: e2) = do
  i1 <- evalErrOrResult e1
  i2 <- evalErrOrResult e2
  if i2 /= 0
    then do
      promote' tick'
      return (i1 `div` i2)    
    else fail "Division by zero!"

evaluateER :: Expr -> (Integer, ErrOrResult Integer)    
evaluateER expr = performStateComp (runErrOrResultT (evalErrOrResult expr)) 0
