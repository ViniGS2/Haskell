--Com numeros negativos a função fatorial se comportaria de forma infinita, uma vez que n seria possivel ir diminuindo o num

fatorial :: Int -> Int
fatorial 0 = 1
fatorial a
    |a > 0 = a * fatorial (a - 1)
    |a < 0 = negate a * fatorial ((negate a) - 1)