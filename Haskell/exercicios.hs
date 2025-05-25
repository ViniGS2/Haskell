import Data.List (sort, elemIndex)

qualEIgual :: Int -> Int -> Int -> Int
qualEIgual a b c
    |a == b && b == c = 3
    |a == b = 2
    |a == c = 2
    |b == c = 2
    |otherwise = 0

maiorMedia :: Int -> Int -> Int -> Int
maiorMedia a b c
    |a > (div (a + b + c)  3) && b > (div (a + b + c)  3) && c > (div (a + b + c)  3) = 3
    |a > (div (a + b + c)  3) && b > (div (a + b + c)  3) = 2
    |b > (div (a + b + c)  3) && c > (div (a + b + c)  3) = 2
    |a > (div (a + b + c)  3) && c > (div (a + b + c)  3) = 2
    |a > div (a + b + c)  3 = 1
    |b > div (a + b + c)  3 = 1
    |c > div (a + b + c)  3 = 1

potencia2 :: Int -> Int
potencia2 a = a * a

potencia4 :: Int -> Int
potencia4 a = potencia2 a * potencia2 a

(@) :: Bool -> Bool-> Bool
True @ True = False
True @ False = True
False @ True = True
False @ False = False

x_maior :: Float -> Float -> Float -> Float
x_maior a b c = (-b + sqrt (delta)) / 2 * a 
    where
        delta = (b * b) - 4 * a * c

x_menor :: Float -> Float -> Float -> Float
x_menor a b c = (-b - sqrt (delta)) / 2 * a
    where
        delta = (b * b) - 4 * a * c

somatorio :: Int -> Int -> Int
somatorio a b = sum [(a + 1)..(b - 1)]

multiplos :: Int -> Int -> Int -> [Int]
multiplos n1 n2 n3 = [x | x <- [n1..n2], x `mod` n3 == 0]

--para usar negativo no terminal tem que colocar entre parenteses
multi :: Int -> Int -> Int
multi 0 _ = 0
multi _ 0 = 0
multi a b 
    |b < 0 && a < 0 = -a + multi a (b + 1)
    |a > 0 && b > 0 = a + multi a (b - 1)
    |a < 0 = a + multi a (b - 1)
    |b < 0 = b + multi (a - 1) b

multi2 :: Int -> Int -> Int
multi2 a b = sum[a | _ <- [1..b]] --terminar

mod2 :: Int -> Int -> Int
mod2 a b = -(div a b) * b + a

calcRaizD6 :: Int -> Float
calcRaizD6 0 = 0
calcRaizD6 a = sqrt(6 + calcRaizD6 (a - 1))

combinacao :: Int -> Int -> Int
combinacao m n = div (fat m) ((fat n) * (fat (m - n)))
    where
        fat 1 = 1
        fat a = a * fat (a - 1)

--maybe, nothing, just x
--tupla, indice de qunto rodou
encontreMaior :: [Int] -> (Int, Int)
encontreMaior l = (maximum l, x)
                  where (Just x) = elemIndex (maximum l) l

--t = tupla de saida, pi é o indice, eu qual num tá, (x:xs) é a lista que tem os elementos
encontreMaior1 :: [Int] -> (Int, Int)
encontreMaior1 [] = error "lista vazia"
encontreMaior1 (x:xs) = aux (x,0) 1 xs --já inicia uma tupla com o primeiro valor e seu indice, o proximo indice e o resto da lista
    where
        aux t _ [] = t --quando a lista estiver vazia vai retornar a tupla atual
        aux (v, i) pi (x:xs) = if x > v then aux (x, pi) (pi + 1) xs else aux (v, i) (pi + 1) xs --v i e pi são formas genericas de referenciar

--zip [1,2,3] [1,3,4] = [(1,1), (2,2), (3,4)] da pra usar uma lista infinita [0..] e conseguir um lista de tuplas com os itens e seus indices
encontreMaior2 :: [Int] -> (Int, Int)
encontreMaior2 [] = error "lista vazia" 
encontreMaior2 l = head (reverse(sort (zip l [0..])))

convertListDigt :: [Int] -> [String]
dic :: [(Int,String)]
dic = [(0, "zero"), (1, "um"), (2, "dois"), (3, "tres"), (4, "quatro"), (5, "cinco"), (6, "seis"), (7, "sete"), (8, "oito"), (9, "nove")]
convertListDigt [] = []
convertListDigt (x:xs) = buscar x dic ++ convertListDigt xs
                            where
                                buscar x xs = [v | (k, v) <- dic, k == x]

del_posicao_x :: [Int] -> Int -> [Int]
del_posicao_x xs x = [x' | x' <- xs, x' /= x]

posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = [i | (z, i) <- zip xs [0 ..], x == z]

{-inserir_posicao_x :: [Int] -> Int -> Int -> [Int]
inserir_posicao_x (x:xs) i e = [(x, i)| (x, i) <- zip xs [0..]]-}

return_val :: [Int] -> Int -> Int
return_val xs i = head [z | (z, i') <- zip xs [0..], i' == i]



main :: IO ()
--runhaskell exercicios.hs
main = do
    let a = -1
    let b = 3
    print(multi a b)