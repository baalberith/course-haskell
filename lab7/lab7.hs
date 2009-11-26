-- import Graphics.Rendering.Diagrams

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

data Request' = Getq | Putq Char deriving (Show)
data Response' = Getp Char | Putp deriving (Show)
type Dialog' = [Response'] -> [Request']

echo' :: Dialog'
echo' p =
  Getq : 
    case p of 
      Getp c : p' ->
	if (c == '\n') then
	  []
	else
	  Putq c :
	  case p' of
	    Putp : p'' -> echo' p''
	    
data Request = Write Integer | ReadReq deriving (Show)
data Response = Wrote | ReadRes Integer deriving (Show)
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

-- ptoblem 5

b = "%%!PS\n%Orientation: Portrait\n%%BoundingBox: 0 0 595 842\n%%DocumentPaperSizes: a4\nnewpath\n"
e = "stroke\nshowpage\n%%Trailer\n%EOF\n"


hilbert :: Int -> IO ()
hilbert n = putStrLn "0 0 moveto" >> hilbert' 0 0 595 0 0 595 n where
  hilbert' x0 y0 xis xjs yis yjs 0 = 
    putStr (show (x0+(xis+yis)/2)) >> putStr " " >> putStr (show (y0+(xjs+yjs)/2)) >> putStrLn " lineto"
  hilbert' x0 y0 xis xjs yis yjs n = do 
    hilbert' x0 y0 (yis/2) (yjs/2) (xis/2) (xjs/2) (n-1)
    hilbert' (x0+xis/2) (y0+xjs/2) (xis/2) (xjs/2) (yis/2) (yjs/2) (n-1)
    hilbert' (x0+xis/2+yis/2) (y0+xjs/2+yjs/2) (xis/2) (xjs/2) (yis/2) (yjs/2) (n-1)
    hilbert' (x0+xis/2+yis) (y0+xjs/2+yjs) (-yis/2) (-yjs/2) (-xis/2) (-xjs/2) (n-1)

main = putStr b >> hilbert 7 >> putStr e

-- (define (hilbert n turn)
--  (cond ((= n 0) (empty-hilbert-curve))
--        ((> n 0)
--          (cond 
--              ((eq? turn 'up) 
--                (concat-path
--                  (hilbert (- n 1) 'right)  
--                  (up-line)
--                  (hilbert (- n 1) 'up)  
--                  (right-line)
--                  (hilbert (- n 1) 'up)  
--                  (down-line)
--                  (hilbert (- n 1) 'left) ))
-- 
--              ((eq? turn 'left) 
--                (concat-path
--                  (hilbert (- n 1) 'down)  
--                  (left-line)
--                  (hilbert (- n 1) 'left)  
--                  (down-line)
--                  (hilbert (- n 1) 'left)  
--                  (right-line)
--                  (hilbert (- n 1) 'up)))
-- 
--              ((eq? turn 'right)
--                (concat-path
--                  (hilbert (- n 1) 'up)  
--                  (right-line)
--                  (hilbert (- n 1) 'right)  
--                  (up-line)
--                  (hilbert (- n 1) 'right)  
--                  (left-line)
--                  (hilbert (- n 1) 'down)))
-- 
--              ((eq? turn 'down)
--                (concat-path
--                  (hilbert (- n 1) 'left)  
--                  (down-line)
--                  (hilbert (- n 1) 'down)  
--                  (left-line)
--                  (hilbert (- n 1) 'down)  
--                  (up-line)
--                  (hilbert (- n 1) 'right)))
--            ))))

-- ptoblem 6
