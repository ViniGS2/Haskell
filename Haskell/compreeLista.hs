fator :: Int -> [Int]
fator n = [x | x <- [1..n], mod n x == 0]

primo :: Int -> Bool
primo l = fator l == [1, l]

zip' :: [a] -> [b] -> [(a, b)] --junta duas listas
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

pares :: [a] -> [(a, a)] --divide a lista em pares
pares xs = zip' xs (tail xs)

ordenado :: Ord a => [a] -> Bool --fala se a lista tá ordenada
ordenado xs = and [x <= y | (x, y) <- pares xs]

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

map :: (t -> a) -> [t] -> [a]
map f xs = [f x| x <- xs]