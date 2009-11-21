import Data.List (unfoldr, delete)
import Maybe (fromJust)

-- problem 1

plusplus' :: [a] -> [a] -> [a]
plusplus' l1 l2 = foldr (\x ys -> x:ys) l2 l1

map' :: (a -> b) -> [a] -> [b]
map' f ls = foldr (\x xs -> (f x):xs) [] ls

filter' :: (a -> Bool) -> [a] -> [a]
filter' p ls = foldr (\x xs -> if p x then x:xs else xs) [] ls

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' s ls = init (foldr (\x xs -> x:s:xs) [] ls)

unzip' :: [(a, b)] -> ([a], [b])
unzip' ls = foldr (\(x, y) (xs, ys) -> (x:xs, y:ys) ) ([], []) ls


-- problem 2

enumFromTo' :: (Num a, Ord a) => a -> a -> [a]
enumFromTo' n m = unfoldr (\x -> if x > m then Nothing else Just (x, x + 1)) n

zip' :: ([a], [b]) -> [(a, b)]
zip' p = unfoldr aux p where
  aux (x:xs, y:ys) = Just ((x, y), (xs, ys))
  aux (_, _)       = Nothing
  
-- split :: [a] -> Either a ([a], [a])
-- split (x:[]) = Left x
-- split (x:xs) = 
--   case split xs of
--     Left y -> Right ([y], [x])
--     Right (ys, zs) -> Right (zs, x:ys)
    
-- split' :: [a] -> Maybe ([a], [a])
-- split' [] = Nothing
-- split' (x:xs) = 
--   case split' xs of
--   Nothing -> Just ([x], [])
--   Just (ys, zs) -> Just (x:zs, ys)
  
merge :: Ord a => [a] -> [a] -> [a]
merge l1 l2 = unfoldr aux (l1, l2) where
  aux ([], []) = Nothing
  aux ([], y:ys) = Just (y, ([], ys))
  aux (x:xs, []) = Just (x, (xs, []))
  aux (xs'@(x:xs), ys'@(y:ys)) 
    | x <= y = Just (x, (xs, ys'))
    | otherwise = Just (y, (xs', ys))
  
halve :: [a] -> ([a], [a])
halve xs = (unfoldr aux (xs, xs), reverse (unfoldr aux' (reverse xs, xs))) where
  aux (_, []) = Nothing
  aux (x:xs, ys) = Just (x, (xs, drop 2 ys))
  aux' (_, []) = Nothing
  aux' (_, _:[]) = Nothing
  aux' (x:xs, ys) = Just (x, (xs, drop 2 ys))

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort h1) (mergesort h2) where
  (h1, h2) = halve xs    


-- problem 3

insToAll :: a -> [a] -> [[a]]
insToAll x []         = [[x]]
insToAll x ys'@(y:ys) = [x:ys'] ++ map (y:) (insToAll x ys) 
 
permutations' :: Eq a => [a] -> [[a]]
permutations' []     = [[]]
permutations' (x:xs) = foldr ((++) . (insToAll x)) [] (permutations' xs) 


-- problem 4

format :: Int -> String -> String
format n s = (concat . concat) (map aux' (reverse (map reverse (foldl aux [[]] (words s))))) where
  aux xs x = if len1 + len2 <= n + 1 then ((x ++ " "):(head xs)):(tail xs) else [(x ++ " ")]:xs where
    len1 = length x + 1
    len2 = length (unwords (head xs))
  aux' xs =  (map (++ " ") (take nsr mapped)) ++ (drop nsr mapped) ++ [init (last xs) ++ "\n"] where
    len = length (unwords xs) - 1
    ns = n - len
    nw = length xs - 1
    (nsq, nsr) = if nw == 0 then (0, 0) else ns `divMod` nw
    mapped = map (++ (replicate nsq ' ')) (take nw xs)

n = 32
s = "   Haskell  is a general purpose, \n" ++ "purely   functional programming language\n" ++ " incorporating   many\n" ++ " recent innovations in  programming\n" ++ "language design. \n"
main = putStrLn (format n s)


-- problem 5

validRoman xs = if toRoman fr == xs then fr else error "not a roman numeral" where
  fr = fromRoman' xs where
    fromRoman' [] = 0
    fromRoman' (x:[]) = fromJust (lookup x roman')
    fromRoman' (x:(ys@(y:_))) 
      | lx < ly   = (fromRoman' ys) - lx
      | otherwise = (fromRoman' ys) + lx
      where lx = fromJust (lookup x roman')
            ly = fromJust (lookup y roman')
    roman' = [('I', 1), ('V', 5), ('X', 10), ('L', 50), ('C', 100), ('D', 500), ('M', 1000)]

fromRoman :: String -> Integer
fromRoman ('(':rest) =  fr1000 + frrest where 
  fr1000 = 1000 * fromRoman (reverse (tail (dropWhile (`elem` "IVXLCDM") (reverse rest))))
  frrest = validRoman (reverse (takeWhile (`elem` "IVXLCDM") (reverse rest))) where
fromRoman xs = validRoman xs


-- problem 6
    
roman x y z n = [[x], [x, x], [x, x, x], [x, y], [y], [y, x], [y, x, x], [y, x, x, x], [x, z]] !! (fromInteger n - 1)     

toRoman :: Integer -> String
toRoman 0 = ""
toRoman n 
  | n >= 4000 = "(" ++ toRoman (n `div` 1000) ++ ")" ++ toRoman (n `mod` 1000)
  | n >= 1000 = 'M' : toRoman (n - 1000)
  | n >= 100  = roman 'C' 'D' 'M' (n `div` 100) ++ toRoman (n `mod` 100)
  | n >= 10   = roman 'X' 'L' 'C' (n `div` 10) ++ toRoman (n `mod` 10)
  | otherwise = roman 'I' 'V' 'X' n
