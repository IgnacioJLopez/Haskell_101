cantidadElementos :: [a] -> Int
cantidadElementos [] = 0 -- Caso base: lista vacía, cantidad de elementos es 0
cantidadElementos (_:xs) = 1 + cantidadElementos xs -- Caso recursivo: avanzar en la lista y sumar 1

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True -- Lista vacía, todos los elementos son iguales (vacío se considera igual a sí mismo)
todosIguales [x] = True -- Lista con un solo elemento, todos los elementos son iguales
todosIguales (x:y:xs) = x == y && todosIguales (y:xs) -- Comparamos el primer elemento con el segundo y llamamos recursivamente



pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise pertenece n xs