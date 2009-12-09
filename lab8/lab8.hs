import Control.Monad
import Data.Maybe
import Data.List ((\\))

type Generator = []

-- problem 1

-- queens :: (MonadPlus m) => Int -> m [Int]
-- queens n = place n [1..n] [] []
-- 
-- place :: (MonadPlus m) => Int -> [Int] -> [Int] -> [Int] -> m [Int]
-- place 0 _ _ _ = return []
-- place i rs d1 d2 = do
--   (q, rs') <- select rs
--   let q1 = q - i
--   let q2 = q + i
--   guard (q1 `notElem` d1)
--   guard (q2 `notElem` d2)
--   qs <- place (i - 1) rs' (q1:d1) (q2:d2)
--   return (q:qs)
--   
-- select :: (MonadPlus m) => [a] -> m (a, [a])
-- select [] = mzero
-- select (a:x) = return (a, x) `mplus` do
--   (b, x') <- select x
--   return (b, a:x')

place :: Int -> [Int] -> [Int] -> [Int] -> Generator [Int]
place 0 _ _ _ = return []
place i rs d1 d2 = do
  (q, rs') <- select rs
  let q1 = q - i
  let q2 = q + i
  guard (q1 `notElem` d1)
  guard (q2 `notElem` d2)
  qs <- place (i - 1) rs' (q1:d1) (q2:d2)
  return (q:qs) 
  
select :: [a] -> Generator (a, [a])
select [] = []
select (a:x) = return (a, x) ++ do
  (b, x') <- select x
  return (b, a:x')
  
queens :: Int -> Generator [Int]
queens n = place n [1..n] [] []
    

-- problem 2

perm1 :: [a] -> Generator [a]
perm1 [] = return []
perm1 (x:xs) = do
  ys <- perm1 xs
  zs <- insert x ys
  return zs where
    insert :: a -> [a] -> Generator [a]
    insert x [] = return [x]
    insert x ys'@(y:ys) = return (x:ys') ++ do
      zs <- insert x ys
      return (y:zs)
    
perm2 :: [a] -> Generator [a]
perm2 [] = return []
perm2 xs = do
  (y,ys) <- select xs
  zs <- perm2 ys
  return (y:zs) where
    select :: [a] -> Generator (a, [a])
    select [] = []
    select (x:xs) = (x,xs) : do
        (y, ys) <- select xs
        return (y, x:ys)


-- problem 3

knight :: (Int,Int) -> (Int,Int) -> Generator [(Int,Int)]
knight (n, m) start = solutions start [] where
  solutions start visited = do
    case nextMoves start \\ visited of
      [] -> do
        guard (isSolution (start:visited))
        return (reverse (start:visited))
      moves -> do
        move <- moves
        solutions move (start:visited)
  nextMoves (i, j) =  [(i', j') | (i', j') <- [(i + 1,j + 2), (i + 1, j - 2), (i - 1,j + 2), (i - 1, j - 2), 
    (i + 2,j + 1), (i + 2, j - 1), (i - 2,j + 1), (i - 2, j - 1)], 1 <= i' && i' <= n && 1 <= j' && j' <= m]
  isSolution moves = length moves == n * m
  

-- problem 4

prod :: [Integer] -> Maybe Integer
prod = foldr (\n p -> p >>= (\p' -> if n == 0 then Nothing else Just (n * p'))) (Just 1)

prod' :: [Integer] -> Integer
prod' = foldr (\n p -> if n == 0 then 0 else n * p) 1

  