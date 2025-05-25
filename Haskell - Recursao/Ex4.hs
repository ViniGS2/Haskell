euclidesMDC :: Int -> Int -> Int
euclidesMDC a b
    |a == b = a
    |a > b = euclidesMDC (a - b) b
    |b > a = euclidesMDC (b - a) a