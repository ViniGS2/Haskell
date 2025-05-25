prodEscalar :: [Int] -> [Int] -> Int
prodEscalar [] _ = 0
prodEscalar _ [] = 0
prodEscalar (x:xs) (y:ys) = (x * y) + prodEscalar xs ys

prodEscalar' :: [Int] -> [Int] -> Int
prodEscalar' xs ys = sum [x * y | (x, y) <- zip xs ys]