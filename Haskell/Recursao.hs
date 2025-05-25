insertion :: Ord a => a -> [a] -> [a]
insertion x [] = [x]
insertion x (y:ys)
    | x <= y = x : y : ys
    |otherwise = y : insertion x ys --isola o numero que tava e pega a cauda pra tenatr inserir o elemento nela

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insertion x (sort xs)

