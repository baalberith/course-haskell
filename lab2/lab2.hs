-- problem 1

iter :: (a -> a) -> a -> Integer -> a
iter f x n = iter' f n $ x where 
  iter' f 0 = id
  iter' f n = f . (iter' f $ n - 1)


-- problem 2

fib :: Integer -> Integer
fib n = fib' 1 1 n where
  fib' acc1 _ 0 = acc1
  fib' _ acc2 1 = acc2
  fib' acc1 acc2 n = fib' acc2 (acc1 + acc2) (n - 1)


-- problem 2 & 3

class Semigroup a where
  prod :: a -> a -> a
  e :: a
  
instance Semigroup Integer where
  prod = (*)
  e = 1     
        
type Matrix = ((Integer, Integer), (Integer, Integer))
        
instance Semigroup Matrix where
  prod ((a, b), (c, d)) ((e, f), (g, h)) = ((a*e + b*g, a*f + b*h), (c*e + d*g, c*f + d*h))
  e = ((1, 0), (0, 1))
  
(^^^) :: Semigroup a => a -> Integer -> a
x ^^^ n = iter (prod x) e n
  
(***) :: Semigroup a => a -> Integer -> a
x *** 0 = e
x *** n 
  | even n = y `prod` y
  | otherwise = y `prod` y `prod` x
  where y = x *** (n `div` 2)
  
fib' :: Integer -> Integer
fib' n = snd $ snd ((((0, 1), (1, 1)) :: Matrix) *** n)
  
-- (**) :: Num a => a -> Integer -> a
-- x ** 0 = 1
-- x ** n 
--   | odd n = x * (x ** (n - 1))
--   | otherwise = x ** (n `div` 2)
