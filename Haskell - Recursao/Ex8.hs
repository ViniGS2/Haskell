--a
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs
--b
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs
--c
last' :: [a] -> a
last' (x:xs)
    |tamanho xs == 0 = x
    |otherwise = last' xs