somar :: Int -> Int
somar 0 = 0
somar a = a + somar (a - 1)