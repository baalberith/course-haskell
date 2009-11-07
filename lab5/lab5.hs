import Data.List (unfoldr, delete)

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

merge :: Ord a => [a] -> [a] -> [a]
merge l1 l2 = unfoldr aux (l1, l2) where
  aux ([], []) = Nothing
  aux ([], y:ys) = Just (y, ([], ys))
  aux (x:xs, []) = Just (x, (xs, []))
  aux (xs'@(x:xs), ys'@(y:ys)) 
    | x <= y = Just (x, (xs, ys'))
    | otherwise = Just (y, (xs', ys))

zip' :: ([a], [b]) -> [(a, b)]
zip' p = unfoldr aux p where
  aux (x:xs, y:ys) = Just ((x, y), (xs, ys))
  aux (_, _)       = Nothing


-- problem 3

insToAll :: a -> [a] -> [[a]]
insToAll x []         = [[x]]
insToAll x ys'@(y:ys) = [x:ys'] ++ map (y:) (insToAll x ys) 

permutations :: Eq a => [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = concatMap (insToAll x) (permutations xs) 
 
permutations' :: Eq a => [a] -> [[a]]
permutations' []     = [[]]
permutations' (x:xs) = foldr ((++) . (insToAll x)) [] (permutations' xs) 
