a = (2 ^ 3) * 4
b = (2 * 3) + (4 * 5)
c = 2 + (3 * (4 ^ 5))

ultimo n = head (reverse n) --o parenteses indica que reverse é uma função para head
ultimo' n = head (drop (length n - 1) n)
tiraultimo n = reverse((tail(reverse n)))
penultimo n = head(drop (length n - 2) n)
pega x n = head (drop (x - 1) n )
