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


-- problem 2

hamming = 1 : merge (merge (map (2*) hamming) (map (3*) hamming)) (map (5*) hamming) where
  merge (xs@(x:xs')) (ys@(y:ys'))
    | x < y = x : merge xs' ys
    | x > y = y : merge xs ys'
    | otherwise = x : merge xs' ys'
  

-- problem 3

fib ::  Int -> IO ()
fib 0 = putStr "a"
fib 1 = putStr "b"
fib n = fib (n - 2) >> fib (n - 1)

-- main = f5 >> f5 where f5 = fib 5


-- problem 4
	    
data Request = Write Integer | ReadReq 
data Response = Wrote | ReadRes Integer 
type Dialog = [Response] -> [Request]

fact :: Integer -> Integer
fact 0 = 1
fact n = product [1..n]

factMain :: Dialog
factMain res =
  ReadReq: 
    case res of 
      ReadRes i: res' ->
	if (i < 0) then
	  []
	else
        Write (fact i):
	  case res' of
	    Wrote: res'' -> factMain res''
            
doRequest :: Request -> IO Response
doRequest ReadReq = 
  getLine >>= \s -> return (ReadRes ((read s) :: Integer))
doRequest (Write i) = 
  putStrLn (show i) >> return Wrote
            
performIO :: Dialog -> IO ()
performIO dialog = 
  case (dialog undefined) of
    []         -> return ()
    (req:req') -> 
      doRequest req >>= 
        (\res -> performIO 
          (\res' -> tail (dialog (res:res'))))
          
-- main = performIO factMain


-- ptoblem 5

b = "%%!PS\n%Orientation: Portrait\n%%BoundingBox: 0 0 595 842\n%%DocumentPaperSizes: a4\nnewpath\n"
e = "stroke\nshowpage\n%%Trailer\n%EOF\n"

curve = iterate vects [] where 
  vects xs = concat [rot xs, [(0,1)], xs, [(1,0)], xs, [(0,-1)], rotneg xs]
  rot xs = map (\(x, y) -> (y, x)) xs
  rotneg xs = map (\(x, y) -> (-y, -x)) xs
  
hilbert :: Int -> IO ()
hilbert n = putStr (show len) >> putStr " " >> putStr (show len) >> putStrLn " moveto" >> hilbert' (curve !! n) len len where
  hilbert' [] x y = putStr (show x) >> putStr " " >> putStr (show y) >> putStrLn " lineto"
  hilbert' ((x',y'):xs) x y = putStr (show (x + len * x')) >> putStr " " >> putStr (show (y + len * y')) >> putStrLn " lineto" >> hilbert' xs (x + len * x') (y + len * y')
  len = 595 / ((len' n) + 2) where
    len' 0 = 0
    len' n = 2 * (len' (n - 1)) + 1
    
main = putStr b >> hilbert 8 >> putStr e
