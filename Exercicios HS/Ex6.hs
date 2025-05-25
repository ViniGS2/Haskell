fator :: Int -> [Int]
fator n = [x | x <- [1..n], mod n x == 0]

perfeitos :: Int -> [Int]
perfeitos a = [x | x <- [1..a], x == sum (fator x) - x]