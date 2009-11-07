import Data.List (delete, intersperse)

-- problem 1

ex1 = [ 2 * x | x <- [1..4]]
ex2 = [ x | x <- [1..4], x `mod` 2 == 0]
ex3 = [ (x, n) | x <- [1..4], n <- [1..x]]


-- problem 2

sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (x:xs) = subs ++ [ x:sub | sub <- subs ]
  where subs = sublists xs
      
sublists' :: [a] -> [[a]]
sublists' []     = [[]]
sublists' (x:xs) = [ s | sub <- sublists' xs, s <- insOrNot x sub] 
  where insOrNot x sub = [x:sub, sub]


-- problem 3

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x:perms | x <- xs, perms <- permutations (delete x xs)]

permutations' :: Eq a => [a] -> [[a]]
permutations' []     = [[]]
permutations' (x:xs) = [ p | perm <- permutations' xs, p <- insToAll x perm]
  where insToAll x []         = [[x]]
        insToAll x ys'@(y:ys) = [x:ys'] ++ map (y:) (insToAll x ys) 


-- problem 4

succ' :: Char -> Char
succ' c 
  | c == 'Z'  = 'A'
  | otherwise = succ c

nextpal :: String -> String
nextpal w = nextpal' (take n2 w) n2 where 
  nextpal' half n
    | p > w     = p
    | otherwise = nextpal' half' (n - 1) where
        p = half ++ drop r (reverse half)
        c = half !! (n - 1)
        half' = take (n - 1) half ++ [succ' c] ++ drop n half
  r = length w `mod` 2
  n2 = (length w + 1) `div` 2
  
pred' :: Char -> Char
pred' c 
  | c == 'A'  = 'Z'
  | otherwise = pred c

prevpal :: String -> String
prevpal w = prevpal' (take n2 w) n2 where 
  prevpal' half n
    | p < w     = p
    | otherwise = prevpal' half' (n - 1) where
        p = half ++ drop r (reverse half)
        c = half !! (n - 1)
        half' = take (n - 1) half ++ [pred' c] ++ drop n half 
  r = length w `mod` 2
  n2 = (length w + 1) `div` 2


-- problem 5

merge :: Ord a => [a] -> [a] -> [a]
merge []         ys         = ys
merge xs         []         = xs
merge xs'@(x:xs) ys'@(y:ys) 
  | x <= y    = x : merge xs ys'
  | otherwise = y : merge xs' ys

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort h1) (mergesort h2) where
  (h1, h2) = splitAt (length xs `div` 2) xs


-- problem 6

jednosc, dziesiatka, setka :: Integer -> String
jednosc n = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", 
  "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"] !! fromInteger n
dziesiatka n = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"] !! fromInteger n
setka n = if n > 0 then if n == 1 then "one hundred" else (jednosc n) ++ " hundreds" else ""

rzad :: Integer -> Integer -> String
rzad r n = 
  if r > 0 then 
    if n == 1 then rzad' r 
    else rzad' r ++ "s" 
  else "" 
  where 
    rzad' n = ["", "thousand", "million", "billion", "trillion", "quadrillion", 
      "quintillion", "sextillion", "septillion"] !! fromInteger n

inWords :: Integer -> String
inWords n 
  | n < 0     = "minus " ++ concat (intersperse " " (inWords' (-n) 0 True))
  | n == 0    = "zero"
  | otherwise = concat (intersperse " " (inWords' n 0 True)) where
    inWords' n r z
      | n == 0    = if z then [] else [rzad r n] 
      | n < 20    = (jednosc n) : [rzad r n]
      | n < 100   = (dziesiatka (n `div` 10)) : inWords' (n `mod` 10) r False
      | n < 1000  = (setka (n `div` 100)) : inWords' (n `mod` 100) r False
      | otherwise = inWords' (n `div` 1000) (r + 1) True ++ inWords' (n `mod` 1000) r True
