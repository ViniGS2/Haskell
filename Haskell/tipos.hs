xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor a b = True

a = [x | x <- [1..], x <= 30]

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = [x] ++ take' (n-1) xs

take2 :: Int -> [a] -> [a]
take2 0 _ = []
take2 _ [] = []
take2 n (x:xs) = x : take' (n-1) xs

--Tupla

b = ("choremos", True, 1, [1,2,3,4])

---------------mais de um parâmetro-------------------
aplica :: (a -> b) -> a -> b --a notação de uma função é (x -> n)
aplica f x = f x

multi :: Int -> (Int -> (Int -> Int)) 
multi x y z = x * y * z

------------------------------------------
