-- problem 7

fibSum :: Integer
fibSum = 
  let fibSum' :: Integer -> Integer -> Integer -> Integer
      fibSum' acc1 acc2 acc =
        if acc1 > limit then acc
        else 
          if even acc1 then fibSum' acc2 (acc1 + acc2) (acc + acc1)
          else fibSum' acc2 (acc1 + acc2) acc
      limit = 4 * 10 ^ 6
  in fibSum' 1 1 0


-- problem 8
  
maxSeqLen :: Integer
maxSeqLen = maximum [ seqLen n | n <- [1..10^4] ] where
  seqLen :: Integer -> Integer
  seqLen m = seqLen' m 0 where 
    seqLen' prev len =
      if prev /= 1 then
        if even prev then
          seqLen' (prev `div` 2) (len + 1)
        else
          seqLen' (3 * prev + 1) (len + 1)
      else
        len + 1

maxSeqLen' :: Integer
maxSeqLen' = seqLen' limit 0 0 0 limit where 
  seqLen' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
  seqLen' _    _   _    maxlen 0 = maxlen
  seqLen' prev len maxn maxlen n =
    if prev == maxn then
      seqLen' (n - 1) 0 n (maxlen + len) (n - 1)
    else
      if prev /= 1 then
        if even prev then
          seqLen' (prev `div` 2) (len + 1) maxn maxlen n
        else
          seqLen' (3 * prev + 1) (len + 1) maxn maxlen n
      else
        if len + 1 > maxlen then seqLen' (n - 1) 0 n (len + 1) (n - 1)
        else seqLen' (n - 1) 0 maxn maxlen (n - 1)
  limit = 10 ^ 4


-- problem 9

digSum :: Integer
digSum = digSum' 0 (fact number) where 
  digSum' :: Integer -> Integer -> Integer
  digSum' acc n = 
    if n == 0 then acc
    else digSum' (acc + (n `mod` 10)) (n `div` 10)
  fact :: Integer -> Integer
  fact n =
    if n == 0 then 1
    else n * fact (n - 1) 
  number = 100
