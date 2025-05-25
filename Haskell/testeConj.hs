p = [x ^ 2| x <- [1..12], x ^ 2 <= 18]

length' xs = sum [1 | x <- xs]

removeNomUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

length2 :: [a] -> Int
length2 [] = 0
length2 (_:xs) = 1 + length2 xs

inverte :: [a] -> [a]
inverte [] = []
inverte x = last x : inverte (init x)

epalindromo :: Eq a => [a] -> Bool -- seta dupla é uma restrição (o tipo a tem que ser instancia de Eq para igualdade)
epalindromo l = l == inverte l

compress' :: Eq a => [a] -> [a]
compress' [] = []
compress' l = aux l []

aux :: Eq a => [a] -> [a] -> [a]
aux [] r = r
aux (x:xs) r = if not(null r) && last r == x then aux xs r else aux xs (r ++ [x])

dividir :: Eq a => [a] -> [[a]]
dividir [] = []
dividir l = aux2 l []

aux2 :: Eq a => [a] -> [[a]] -> [[a]]
aux2 [] r = r
aux2 (x:xs) r = if not(null r) && (x `elem` last r) then aux2 xs (init r ++ [(x:(last r))]) else aux2 xs (r ++ [[x]] {-junta duas listas-}) 

quantelem :: Eq a => [a] -> [[a]]
quantelem [] = []
quantelem l = aux3 (dividir l) []

aux3 :: Eq a => [[a]] -> [[a]] -> [[a]]
aux3 [] r = r
aux3 (x:xs) r = if not(null r) && last(last r) == head x then aux3 xs (show (length x) !! r) else aux3 xs (r ++ [head x])
    