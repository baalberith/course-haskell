import Maybe

-- problem 1

genTails :: [a] -> [[a]]
genTails []        = [[]]
genTails ys@(_:xs) = ys : genTails xs


-- problem 2

sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (x:xs) = sbs ++ map (x:) sbs
	where sbs = sublists xs


-- problem 3

halflist :: [a] -> [a]
halflist (x:y:zs) = y : halflist zs
halflist _        = []

halflists :: [a] -> [[a]]
halflists [] = []
halflists ls = ls : halflists hls
	where hls = halflist ls


-- problem 4

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n  
  | even n = n : collatz (n `div` 2)
	| otherwise = n : collatz (3 * n + 1)


-- problem 5

add :: [Integer] -> [Integer] -> [Integer]
add xs ys = add' 0 xs ys where 
  add' r []     []     = if r /= 0 then [r] else []
  add' r (x:xs) []     = (r + x) `mod` 10 : add' ((r + x) `div` 10) xs []
  add' r []     (y:ys) = (r + y) `mod` 10 : add' ((r + y) `div` 10) [] xs
  add' r (x:xs) (y:ys) = (r + x + y) `mod` 10 : add' ((r + x + y) `div` 10) xs ys
  
intToList :: Integer -> [Integer]
intToList 0 = []
intToList n = n `mod` 10 : intToList (n `div` 10)

listToInt :: [Integer] -> Integer
listToInt []     = 0
listToInt (x:xs) = x + 10 * (listToInt xs) 

palindrom :: [Integer] -> Bool
palindrom xs = xs == (reverse xs)

digitalPalindrom :: Integer -> [Integer]
digitalPalindrom n
  | palindrom (intToList n) = [n]
  | otherwise = n : digitalPalindrom n' where 
      ni = intToList n
      n' = listToInt (ni `add` (reverse ni))


-- problem 6

roman = [('I', 1), ('V', 5), ('X', 10), ('L', 50), ('C', 100), ('D', 500), ('M', 1000)]

fromRoman :: [Char] -> Integer
fromRoman [] = 0
fromRoman (x:[]) = fromJust (lookup x roman)
fromRoman (x:(ys@(y:_))) 
  | lx < ly   = (fromRoman ys) - lx
  | otherwise = (fromRoman ys) + lx
  where lx = fromJust (lookup x roman)
        ly = fromJust (lookup y roman )


-- problem 7

toRoman :: Integer -> [Char]
toRoman n = reverse (toRoman' 0 n) where
  toRoman' i 0 = ""
  toRoman' i n = roman (n `mod` 10) ++ toRoman' (i + 1) (n `div` 10) where 
    roman r
      | r <= 3    = replicate (fromInteger r) (x1 i)
      | r == 4    = [x5 i] ++ [x1 i]
      | r <= 8    = replicate (fromInteger (r - 5)) (x1 i) ++ [x5 i]
      | otherwise = [x1 (i + 1)] ++ [x1 i]
    x1 i = "IXCM" !! i
    x5 i = "VLD" !! i
    
toRoman' :: Integer -> [Char]
toRoman' 0 = ""
toRoman' n
  | n >= 1000 = 'M' : toRoman' (n - 1000)
  | n >= 100  = roman x2 (n `div` 100) ++ toRoman' (n `mod` 100)
  | n >= 10   = roman x1 (n `div` 10) ++ toRoman' (n `mod` 10)
  | otherwise = roman x0 n
  where roman x r
          | r <= 3    = replicate (fromInteger r) (x 1)
          | r == 4    = [x 1] ++ [x 2]
          | r <= 8    = [x 2] ++ replicate (fromInteger (r - 5)) (x 1) 
          | otherwise = [x 1] ++ [x 3]
        x0 i = "IVX" !! (i - 1)
        x1 i = "XLC" !! (i - 1)
        x2 i = "CDM" !! (i - 1)
        
roman' x y z k = [[x], [x, x], [x, x, x], [x, y], [y], [y, x], [y, x, x], [y, x, x, x], [x, z]] !! (fromInteger k - 1)     

toRoman'' :: Integer -> [Char]
toRoman'' 0 = ""
toRoman'' n 
  | n >= 4000 = "(" ++ toRoman'' (n `div` 1000) ++ ")" ++ toRoman'' (n `mod` 1000)
  | n >= 1000 = 'M' : toRoman'' (n - 1000)
  | n >= 100  = roman' 'C' 'D' 'M' (n `div` 100) ++ toRoman'' (n `mod` 100)
  | n >= 10   = roman' 'X' 'L' 'C' (n `div` 10) ++ toRoman'' (n `mod` 10)
  | otherwise = roman' 'I' 'V' 'X' n

test = [1..3999] == (map (fromRoman.toRoman) [1..3999])
