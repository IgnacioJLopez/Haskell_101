factorial :: Integer -> Integer
factorial n | n==0 = 1 -- caso base
    | otherwise = n* factorial (n-1)

{--
Para hacer comentarios
--}

fibonacci :: Integer -> Integer
fibonacci n | n==0 = 0
    |n==1 = 1
    |otherwise = fibonacci(n-1)+fibonacci(n-2)

-- Otra forma usando Pattern Matching
fibonacci2 0 = 0
fibonacci2 1 = 1
fibo2 x = fibonacci(x-1)+fibonacci(x-2)

parteEntera :: Float -> Integer
parteEntera x | x>=0 && x<=1 = 0
    |x>=1 = parteEntera(x-1) + 1
    |x<=0 && x>(-1) = -1 -- Este caso base se podría obviar
    |x<=0 = parteEntera(x+1) - 1

ultimoDigito :: Integer -> Integer
ultimoDigito x = mod x 10

sacoUltimoDigito :: Integer -> Integer
sacoUltimoDigito x = div x 10

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x | x<10 = True
    | otherwise = ultimoDigito x == ultimoDigito(sacoUltimoDigito x) && todosDigitosIguales (sacoUltimoDigito x)

cantDigitos :: Integer -> Integer
cantDigitos x |x==0 = 1 -- no haría falta ponerlo
    |x<10=1
    |otherwise = cantDigitos(sacoUltimoDigito x) + 1

iesimoDigito :: Integer -> Integer -> Integer
-- iesimoDigito n i = mod 10  (div n 10^(cantDigitos(n)-i))
iesimoDigito n i | i==cantDigitos n = ultimoDigito n
    |otherwise = iesimoDigito(sacoUltimoDigito n) i