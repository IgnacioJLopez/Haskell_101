-- sumatoria
-- sum_{j=1}^m n^j
sum1 :: Integer -> Integer -> Integer
sum1 n m |m==1 = n 
         | otherwise = n^m + sum1 n (m-1) 

-- Esta no la hicimos en clase, la copie yo
sum2 :: Integer -> Integer -> Integer
sum2 n j |n==1 = 1^j
    | otherwise = n^j + sum2 (n-1) j

-- Ej 13
sumDoble :: Integer -> Integer -> Integer
sumDoble n m
    | n==1 = sum1 1 m -- esto es igual a m
    | otherwise = sumDoble (n-1) m + sum1 n m


-- Ej 16
-- a)
menorDivisordesde :: Integer -> Integer -> Integer
menorDivisordesde m n | mod n m == 0 = m
    | otherwise = menorDivisordesde (m+1) n

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisordesde 2 n

--b)
esPrimo :: Integer -> Bool
esPrimo n | menorDivisor n == n = True
    | otherwise = False

-- Ej 19
-- La docente nos dijo que hagamos esto:
-- Armar una función cantidadPrimosHasta
-- Luego armar otra función que llame a la anterior, que pregunte ¿tiene n-1 primos?