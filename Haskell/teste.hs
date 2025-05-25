import Data.List (partition)

soma :: Int -> Int -> Int
soma x y = x + y

produto :: [Int] -> Int
produto[] = 1
produto(x:xs) = x * produto xs

a = b + c
    where
        b = 1
        c = 1

menor x y = if x <= y then x else y

quicksort :: [Int] -> [Int]
quicksort[] = []
quicksort(x:xs) = 
    let 
        (menores, maiores) = partition (<= x) xs {-partition divide uma lista em duas passando uma preposição como parametro
        primeiro as que atendem e depois as que não atendem-} 
    in 
        quicksort menores ++ [x] ++ quicksort maiores
{-então ele pega o primeiro e usa ele como parametro para dividir
 em duas listas que vão tbm encontrar um pivo para se dividir em 
 duas e quando um dos quicksort tiver
 vazia ele vai começar a desempacotar e concatenar no final-}