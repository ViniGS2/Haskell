replicate' :: Int -> a -> [a]
replicate' a b = [ b | _ <- [1..a]]