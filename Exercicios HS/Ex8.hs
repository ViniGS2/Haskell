posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = [i | (z, i) <- zip xs [0 ..], x == z] --encontra o as posições das ocorrencias de um valor em uma lista

buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k'] --encontra elementos em uma tabela por meio de uma chave

posicoes' :: Eq a => a -> [a] -> [Int]
posicoes' x xs = buscar x [(z, i) | (z, i) <- zip xs [0 ..]]