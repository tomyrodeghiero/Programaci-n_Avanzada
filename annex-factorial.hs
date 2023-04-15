-- Actividad complementaria: ¿Cómo redefinirías factorial haciendo uso de guardas?
factorial :: Int -> Int
factorial n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = n * factorial (n-1)

-- Actividad: ¿La puedes completar?
{- factoriales_2 :: Int -> [Int]
factoriales_2 n = reverse (aux (n+1) 0 [1])
    where aux n m (x:xs) = if n==m then.............. -}

factoriales_2 :: Int -> [Int]
factoriales_2 n = reverse (aux (n+1) 0 [1])
    where aux n m (x:xs) = if n == m+1 then (x:xs) else aux n (m+1) ((x*(m+1)):x:xs)

{- Actividad: ¿Te animás a buscar una quinta solución? Investiga la función scanl y propone una
solución haciendo uso de dicha función. -}
factorial_5 :: Int -> Int
factorial_5 n = last (scanl (*) 1 [1..n])