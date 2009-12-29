import qualified Data.Map as M

newtype StateExnComp state m result = StateExnComp { performStateExnComp :: state -> m (state, result) }

instance Monad m => Monad (StateExnComp state m) where
  StateExnComp f >>= h = StateExnComp (\state -> do
    (state1, result1) <- f state
    performStateExnComp (h result1) state1)     
  return result = StateExnComp (\state -> return (state, result))
  
getState :: Monad m => StateExnComp state m state
getState = StateExnComp (\state -> return (state, state))

putState :: Monad m => state -> StateExnComp state m ()
putState state = StateExnComp (\ _ -> return (state, ()))
  
type Memory = M.Map String Integer
type RAM m result = StateExnComp Memory m result

newVar :: Monad m => String -> RAM m ()
newVar name = do
  m <- getState
  case M.lookup name m of
         Just _ -> fail $ name ++ " exists"
         Nothing -> putState $ M.insert name 0 m
         
asgn :: Monad m => String -> Integer -> RAM m ()
asgn name v = do
  m <- getState
  case M.lookup name m of
         Just _ -> putState $ do
           let m' = M.delete name m
           M.insert name v m'
         Nothing -> fail $ name ++ " doesn't exist"
         
val :: Monad m => String -> RAM m Integer
val name = do
  m <- getState
  case M.lookup name m of
         Just v -> return v
         Nothing -> fail $ name ++ " doesn't exist"
         
ifelse :: Monad m => RAM m Bool -> RAM m a -> RAM m a -> RAM m a
ifelse pred conseq alt = do
  p <- pred
  if p
       then conseq
       else alt

while :: Monad m => RAM m Bool -> RAM m a -> RAM m ()
while pred action = do
  p <- pred
  if p
       then do
         action
         while pred action
       else return ()
           
fac :: Monad m => Integer -> RAM m Integer
fac n = do
  newVar "m"
  newVar "f"
  "m" `asgn` n
  "f" `asgn` 1
  while (val "m" >>= return . (> 1)) $ do
    m <- val "m"
    f <- val "f"
    "f" `asgn` (m*f)
    "m" `asgn` (m-1)
  val "f"
   
instance Monad (Either String) where
  (Left s) >>= _ = Left s
  (Right r) >>= h = h r
  return r = Right r
  fail s = Left s

run :: RAM (Either String) Integer -> IO ()
run fun = do
  case performStateExnComp fun M.empty of
         Right (_, res) -> putStrLn $ show res
         Left s -> putStrLn s
  