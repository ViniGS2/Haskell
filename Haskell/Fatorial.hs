fac :: Int -> Int
fac 0 = 1
fac a = a * fac (a - 1)

asc :: Int -> Int -> [Int]
asc n m
    | n > m = []
    | n == m = [m]
    | m > n = n : asc ( n + 1) m

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs)
    |mod x 2 == 0 = x : evens xs
    |otherwise = evens xs

somaTuplas :: [(Int,Int)] -> [Int]
somaTuplas [] = []
somaTuplas xs = [x + y | (x,y) <- xs]

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    |a /= x = elem' a xs
    |otherwise = True

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 _ [] = False
elem2 a (x:xs) = a == x || elem2 a xs

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' r = reverse (acumul r [])
    where
        acumul [] acc = acc
        acumul (x:xs) acc 
            |x `elem` acc = acumul xs acc 
            |otherwise = acumul xs (x : acc) 

nub2 :: (Eq a) => [a] -> [a]
nub2 [] = []
nub2 (x:xs)
    |x `elem` xs = nub2 xs
    |otherwise = x : nub2 xs

isAscen' :: [Int] -> Bool
isAscen' [] = True
isAscen' [x] = True
isAscen' (x:y:xs) = (x <= y) && isAscen' (y:xs)

somLam :: Int -> Int -> Int
somLam = \x y -> x + y

somLam2 :: Int -> Int -> Int
somLam2 = \x -> (\y -> x + y) --forma mais profunda da linguagem