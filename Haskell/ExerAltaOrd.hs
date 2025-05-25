cresc_ord :: [Int] -> [Int]
cresc_ord = sort . filter odd

insertion :: Ord a => a -> [a] -> [a]
insertion x [] = [x]
insertion x (y:ys)
    | x <= y = x : y : ys
    |otherwise = y : insertion x ys --isola o numero que tava e pega a cauda pra tenatr inserir o elemento nela

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = insertion x (sort xs)

posicao :: Int -> [a] -> a
posicao a xs = xs !! a

repete :: Int -> [[Int]]
repete = [, ]