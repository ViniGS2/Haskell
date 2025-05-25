--a
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs
--b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs
--c
{-replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a ++ replicate' (n - 1) a-}
--d
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)
--e
elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    |a == x = True
    |a /= x = elem' a xs
