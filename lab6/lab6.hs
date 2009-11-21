-- problem 1

loop :: a
loop = loop

ones :: [Integer]
ones = 1 : ones

-- problem 2

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
fibSum = sum [n | n <- takeWhile (< 4*10^6) fibs, even n] 

-- problem 3

fact = 1 : zipWith (*) fact [1..]
