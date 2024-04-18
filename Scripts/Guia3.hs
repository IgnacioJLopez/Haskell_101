dobleSumatoria :: Int -> Int -> Int
dobleSumatoria n m = sum [sum [i^j | j <- [1..m]] | i <- [1..n]]

