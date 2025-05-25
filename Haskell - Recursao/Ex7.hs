tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

{-metades :: [a] -> ([a],[a])
metades l@(x:xs) = aux l t
                    where
                        aux :: Ord a => [a] -> ([a],[a]) -> ([a],[a])
                        aux (x:y:xs) t@(e, d)
                            |x <= y = x : fst t
                            |y <= x = y : snd t-}
