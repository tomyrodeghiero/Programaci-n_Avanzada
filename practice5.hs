-- Exercise 1
{- 1. Generar una lista infinita de unos. -}
infiniteList1 :: [Int]
infiniteList1 = [x | x <- [1,1..]]

infiniteList1' :: [Int]
infiniteList1' = [1,1..]

infiniteList1'' :: [Int]
infiniteList1'' = repeat 1

-- Exercise 2
{- 2. Generar una lista infinita de naturales comenzando desde un número dado. -}
data Nat = Zero | Succ Nat

instance Eq Nat where
    Zero == Zero = True
    Succ n == Succ m = n == m
    _ == _ = False

instance Ord Nat where
    Zero <= _ = True
    Succ n <= Succ m = n <= m
    _ <= _ = False

-- instance Show Nat where
--     show Zero = "Zero"
--     show (Succ n) = "Succ " ++ show n

instance Show Nat where
    show n = show (natToInt n)

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

infiniteListNaturals :: Nat -> [Nat]
infiniteListNaturals n = n : infiniteListNaturals (Succ n)

infiniteListNaturals' :: Int -> [Int]
infiniteListNaturals' n
    | n < 0 = [-1]
    | otherwise = [n..]

-- Exercise 3
{- 3. Generar una lista con los primeros n naturales. -}
firstNaturalsN :: Int -> [Int]
firstNaturalsN n = [0..n]

-- Exercise 4
{- 4. Retornar los primeros 5 elementos de una lista infinita de enteros positivos. -}
first5ElementsOfInfiniteList :: [Int]
first5ElementsOfInfiniteList = take 5 ([1..])

-- Utilizando funciones de alto orden resolver:
-- Exercise 5
{- 5. Dada una lista de enteros, retornar sus cuadrados. -}
{- squaresListInt :: [Int] -> [Int]
squaresListInt xs = [x * x | x <- xs] -}

-- Exercise 6
{- 6. Dado un entero positivo, retornar sus divisores. -}
{- divsN :: Int -> [Int]
divsN n = [x | x <- [1..n], n `mod` x == 0] -}

-- Exercise 7
{- 7. Dada una lista de naturales, obtener la lista que contenga solo los núumeros primos
de la lista original. -}

-- Exercise 10
{- 10. Dada una lista de enteros, sumar todos sus elementos. -}
{- sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs -}

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- Exercise 11
{- 11. Definir el factorial usando fold. -}
factorial :: Int -> Int
factorial n = foldl (*) 1 [1..n]

-- Utilizando listas por comprensión resolver:
-- Exercise 12
{- 12. Dada una lista de enteros, retornar sus sucesores. -}
successors :: [Int] -> [Int]
successors xs = [x + 1 | x <- xs]

-- Exercise 13
{- 15. Dado un entero, retornar sus divisores. -}
dividers :: Int -> [Int]
dividers n = [x | x <- [1..n], n `mod` x == 0]

-- Exercise 18
{- 18. Definir la lista infinita de los números pares. -}
infiniteListOfEvenNumbers :: [Int]
infiniteListOfEvenNumbers = [x | x <- [0..], x `mod` 2 == 0]