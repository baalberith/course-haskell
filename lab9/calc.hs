import Char

lexer :: (Pos, String) -> ((Pos, String), TOKEN)
lexer state@(pos, "") = (state, EOT)
lexer (pos@(Pos line col), ch : str)
   | ch == '(' = ((Pos line (col + 1), str), LParen)
   | ch == ')' = ((Pos line (col + 1), str), RParen)
   | ch `elem` ['+', '-', '*', '/', '%']
        = ((Pos line (col + 1), str), Op ch)
   | ch == '\n' = lexer (Pos (line + 1) 1, str)
   | isSpace ch = lexer (Pos line (col + 1), str)
   | isAlpha ch =
        let
            (name, rest) = span isAlphaNum str
            len = length name + 1
        in
            ((Pos line (col + len), rest), Ident (ch:name))
   | isDigit ch =
        let
            (digits, rest) = span isDigit str
            len = length digits + 1
            value = foldr (\ d n -> 10 * n + val d) (val ch) digits
            val ch = toInteger $ ord ch - ord '0'
        in
            ((Pos line (col + len), rest), Number value)
   | otherwise = ((Pos line (col + 1), str), None)
   

factor :: Monad m => (Pos, String) -> m ((Pos, String), Expr)
factor state0@(pos0,_) =
   let (state1, token1) = lexer state0
   in case token1 of
      LParen -> do
          (state2@(pos2,_), result2) <- exprN 2 state1
          let (state3, token3) = lexer state2
          case token3 of
             RParen -> return (state3, result2)
             _ -> fail ("Syntax error at " ++ show pos2
                           ++ ": right parenthesis expected")
      Number val -> return (state1, Num val)
      Ident name -> return (state1, Var name)
      _ -> fail ("Syntax error at " ++ show pos0
                     ++ ": bad token " ++ show token1
                     ++ " (expected LParen or Number or Ident)")
                     

exprN :: Monad m => Int -> (Pos, String) -> m ((Pos, String), Expr)
exprN 0 state0 = factor state0
exprN level state0 = exprN (level-1) state0 >>= whileExpr where
   whileExpr acc@(state1, result1) =
      let
          (state2, token2) = lexer state1
      in case token2 of
          Op ch ->

            if ch `elem` ([['*', '/', '%'], ['+','-']] !! (level-1))
                then do
                   (state3, result3) <- exprN (level-1) state2
                   whileExpr (state3, BinOp ch result1 result3)
                else return acc
          _ -> return acc
         

expr :: Monad m => String -> m Expr
expr str = do
   ((pos1, str1), result) <- exprN 2 (Pos 1 1, str)
   if null str1
      then return result
      else fail ("Syntax error at " ++ show pos1
                     ++ ": trailing garbage")


data Pos = Pos { line :: Int, col :: Int }

instance Show Pos where
   show (Pos line col) = "line " ++ show line ++ " column " ++ show col
   
data TOKEN = Number Integer | Ident String
            | LParen | RParen
            | Op Char | None | EOT deriving Show
            
data Expr = Num Integer | Var String | BinOp Char Expr Expr deriving Show


newtype StateExnComp state m result =
   StateExnComp { performStateExnComp :: state -> m (state, result) }

instance Monad m => Monad (StateExnComp state m) where
  StateExnComp f >>= h = StateExnComp (\state0 -> do
    (state1, result1) <- f state0
    performStateExnComp (h result1) state1)   
  return result = StateExnComp (\state -> return (state, result))
  fail = fail
  
getState :: Monad m => StateExnComp state m state
getState = StateExnComp (\state -> return (state, state))

putState :: Monad m => state -> StateExnComp state m ()
putState state = StateExnComp (\ _ -> return (state, ()))


type ParsingState = (Pos, String)
type Parser m result = StateExnComp ParsingState m result


nextLine :: Monad m => Parser m ()
nextLine = StateExnComp (\((Pos line col), str) -> return ((Pos (line + 1) col, str), ()))

advanceCol :: Monad m => Int -> Parser m ()
advanceCol n = StateExnComp (\((Pos line col), str) -> return ((Pos line (col + n), str), ()))

changeCol :: Monad m => Int -> Parser m ()
changeCol col = StateExnComp (\((Pos line _), str) -> return ((Pos line col, str), ()))

nextCol :: Monad m => Parser m ()
nextCol = StateExnComp (\((Pos line col), str) -> return ((Pos line (col + 1), str), ()))


getString :: Monad m => Parser m String
getString = StateExnComp (\state@(_, str) -> return (state, str))

putString :: Monad m => String -> Parser m ()
putString str = StateExnComp (\(pos, _) -> return ((pos, str), ()))


lexer' :: Monad m => Parser m TOKEN
lexer' = do
  s <- getString
  case s of
    "" -> return EOT
    ('(':str) -> do
      nextCol
      putString str
      return LParen
    (')':str) -> do
      nextCol
      putString str
      return RParen
    ('\n':str) -> do
      nextLine
      changeCol 1
      putString str
      lexer'
    (ch:str) 
      | ch `elem` ['+', '-', '*', '/', '%'] -> do 
          nextCol
          putString str
          return (Op ch)
      | isSpace ch -> do
          nextCol
          putString str
          lexer'
      | isAlpha ch -> do
          let (name, rest) = span isAlphaNum str
          let len = length name + 1
          advanceCol len
          putString rest
          return (Ident (ch:name))
      | isDigit ch -> do
          let (digits, rest) = span isDigit str
          let len = length digits + 1
          let value = foldr (\ d n -> 10 * n + val' d) (val' ch) digits
          advanceCol len
          putString rest
          return (Number value)
      | otherwise -> do
          nextCol
          putString str
          return None
 
val' :: Char -> Integer
val' ch = toInteger $ ord ch - ord '0'


factor' :: Monad m => Parser m Expr
factor' = do
  (pos0, _) <- getState
  token1 <- lexer'
  case token1 of
    LParen -> do
      result2 <- exprN' 2
      (pos2, _) <- getState
      token3 <- lexer'
      case token3 of
        RParen -> return result2
        _ -> fail $ "Syntax error at " ++ show pos2 ++ ": right parenthesis expected"
    Number val -> do return $ Num val
    Ident name -> return $ Var name
    _ -> fail $ "Syntax error at " ++ show pos0 ++ ": bad token " ++ show token1 ++ " (expected LParen or Number or Ident)"


exprN' :: Monad m => Int -> Parser m Expr
exprN' 0 = factor'
exprN' level = do
  exprN' (level - 1) >>= whileExpr where
    whileExpr result1 = do
      token2 <- lookup'
      case token2 of
        Op ch ->
          if ch `elem` ([['*', '/', '%'], ['+','-']] !! (level-1))
            then do
              checkToken'
              result3 <- exprN' $ level - 1
              whileExpr $ BinOp ch result1 result3
            else return result1
        _ -> return result1
        
lookup' :: Monad m => Parser m TOKEN
lookup' = do
  state <- getState
  tok <- lexer'
  putState state
  return tok

checkToken' :: Monad m => Parser m TOKEN
checkToken' = lexer'
        

-- expr' :: (Monad m) => String -> m Expr
-- expr' str = do
--   putState (Pos 1 1, str)
--   result <- exprN' 2 
--   (pos1, str1) <- getState
--   if null str1
--        then return result
--        else fail $ "Syntax error at " ++ show pos1 ++ ": trailing garbage"

evaluate :: (Monad m) => String -> m Expr
evaluate str = do
  (state, res) <- performStateExnComp (exprN' 2) (Pos 1 1, str)
  return res
