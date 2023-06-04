-- Exercise 3
{- Especificar y derivar la siguiente función:
f xs dice si todos los elementos son iguales. -}
f' :: (Eq a) => [a] -> Bool
f' [] = True
f' (x:xs) = g' (x:xs) x

g' :: (Eq a) => [a] -> a -> Bool
g' [] k = True
g' (x:xs) k = k == x && g' xs k

-- Exercise 4
{- Especificar y derivar una función que dada una lista de n ́umeros
devuelva su producto. -}
f'' :: [Int] -> Int
f'' [] = 1
f'' (x:xs) = x * f'' xs

-- * Exercise 5
{- Derivar una función que dada una lista determina si los elementos 
están ordenados de forma creciente. -}
f''' :: [Int] -> Bool
f''' [] = True
f''' [x] = True
f''' (x:y:xs) = x <= y && f''' (y:xs)