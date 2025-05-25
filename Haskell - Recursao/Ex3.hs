exponenciacao :: Float -> Float -> Float
exponenciacao b 0 = 1
exponenciacao b e 
    |e > 0 = b * exponenciacao b (e - 1)
    |e < 0 = 1 / (b * exponenciacao b ((negate e) - 1))