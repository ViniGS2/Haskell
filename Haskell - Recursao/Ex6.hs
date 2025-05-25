merge' :: Ord a => [a] -> [a] -> [a]
merge' [] [] = []
merge' a [] = a
merge' [] b = b
merge' e@(x:xs) d@(y:ys) -- colocando uma letra e um @ antes da representação da lista você consegue representar ela com a letra, mas também pode repetir a mesma representação
    |y >= x = x : merge' xs d
    |y <= x = y : merge' e ys
