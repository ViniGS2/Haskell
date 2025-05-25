grid' :: Int -> Int -> [(Int, Int)]
grid' a b = [(x, y) | x <- [0..a], y <- [0..b]]

quadrado :: Int -> [(Int, Int)]
quadrado a = [x | x <- (grid' a a), fst x /= snd x]