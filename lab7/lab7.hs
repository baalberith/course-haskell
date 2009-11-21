-- problem 1

primes :: [Integer]
primes = 2 : sieve [3..] where
  sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]
  
fieldWidth = 5
numOfCols = 10

fill n xs = reverse (take n (reverse xs ++ (cycle " ")))

printPrimes :: Int -> String
printPrimes n = aux n (take n primes) where 
  aux n xs
    | n <= numOfCols = unwords (map ((fill fieldWidth).show) (take n xs))
    | otherwise = unwords (map ((fill fieldWidth).show) (take numOfCols xs)) ++ "\n" ++ aux (n - numOfCols) (drop numOfCols xs)
  
main = putStrLn (printPrimes 94)


-- problem 2

hamming = 1 : merge (merge (map (2*) hamming) (map (3*) hamming)) (map (5*) hamming) where
  merge (xs@(x:xs')) (ys@(y:ys'))
    | x < y = x : merge xs' ys
    | x > y = y : merge xs ys'
    | otherwise = x : merge xs' ys'
