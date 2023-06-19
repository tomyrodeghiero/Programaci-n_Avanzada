-- Exercise 1
{- 1. Generar una lista infinita de unos. -}
infiniteList1 :: [Int]
infiniteList1 = 1 : infiniteList1

infiniteList1' :: [Int]
infiniteList1' = [x | x <- [1,1..]]

infiniteList1'' :: [Int]
infiniteList1'' = [1,1..]

infiniteList1''' :: [Int]
infiniteList1''' = repeat 1

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

infiniteListNaturals :: Int -> [Int]
infiniteListNaturals n
    | n < 0 = error "The input value must be greater than or equal zero"
    | otherwise = n : infiniteListNaturals (n+1)

infiniteListNaturals' :: Nat -> [Nat]
infiniteListNaturals' n = n : infiniteListNaturals' (Succ n)

infiniteListNaturals'' :: Int -> [Int]
infiniteListNaturals'' n
    | n < 0 = error "The input value must be greater than or equal zero"
    | otherwise = [n, n + 1..]

-- Exercise 3
{- 3. Generar una lista con los primeros n naturales. -}
firstNaturalsN :: Int -> [Int]
firstNaturalsN n = [0..n]

-- Exercise 4
{- 4 *. Retornar los primeros 5 elementos de una lista infinita de enteros positivos. -}
first5ElementsOfInfiniteList :: [Int]
first5ElementsOfInfiniteList = take 5 ([1..])

-- Utilizando funciones de alto orden resolver:
-- Exercise 5
{- 5. Dada una lista de enteros, retornar sus cuadrados, es decir, dado [x0, x1, . . . , xn]
deberia retornar [x^2.0, x^2.1, x^2.2] -}
squaresListInt :: [Int] -> [Int]
squaresListInt xs = map (^2) xs

-- Exercise 6
{- 6. Dado un entero positivo, retornar sus divisores. -}
dividersZPositive :: Int -> [Int]
dividersZPositive n = filter (\x -> n `mod` x == 0) [1..n]

-- Exercise 7
{- 7. Dada una lista de naturales, obtener la lista que contenga solo los núumeros primos
de la lista original. -}
primeNumbers :: [Int] -> [Int]
primeNumbers xs = filter (\x -> isPrime x) xs

-- helper function
isPrime :: Int -> Bool
isPrime n = length (filter (\x -> n `mod` x == 0) [1..n]) == 2

-- Exercise 8
{- 8 *. Dada una lista de naturales, retornar la suma de los cuadrados de la lista. -}
sumSquaresList :: [Int] -> Int
sumSquaresList xs = sum (map (^2) xs)

-- Exercise 9
{- 9. Dada una lista de naturales, retornar la lista con sus sucesores. -}
successorsList :: [Int] -> [Int]
successorsList xs = map (+1) xs

-- Exercise 10
{- 10. Dada una lista de enteros, sumar todos sus elementos. -}
sum' :: (Num a) => [a] -> a
sum' xs = foldr (+) 0 xs

-- Exercise 11
{- 11 *. Definir el factorial usando fold. -}
factorial :: Int -> Int
factorial n = foldl (*) 1 [1..n]

-- Exercise 12
{- 12 *. Redefinir la función and tal que and xs se verifica si todos los elementos de xs son verdaderos. Por ejemplo: and [1<2, 2<3, 1/=0] = True,
and [1<2, 2<3, 1 == 0] = False. -}
and' :: [Bool] -> Bool
and' [] = error "The list must have at least one element"
and' xs = foldr (&&) True xs

-- Exercise 13
{- 13. Usando foldl o foldr definir una función tam::[a]->Int que devuelve la
cantidad de elementos de una lista dada. Dar un ejemplo en los cuales foldr y
foldl evaluen diferente con los mismos parametros. -}
tam :: [a] -> Int
tam xs = foldr increment 0 xs
    where
        increment :: a -> Int -> Int
        increment _ qtyElem = qtyElem + 1

{- Ejemplo en los cuales foldr y foldl evaluen diferente con los mismos parametros:
foldr
=====
tam xs = foldr increment 0 xs
       = increment 1 (increment 2 (increment 3 0))
       = 3

foldl
=====
tam xs = foldr increment 0 xs
       = increment (increment (increment 0 1) 2) 3
       = 3 -}

-- Utilizando listas por comprensión resolver:
-- Exercise 14
{- 14. Dada una lista de enteros, retornar sus sucesores. -}
successors :: [Int] -> [Int]
successors xs = [x + 1 | x <- xs]

-- Exercise 15
{- 15 *. Dada una lista de naturales, retornar sus cuadrados. -}
squares :: [Int] -> [Int]
squares xs = [x ^ 2 | x <- xs]

-- Exercise 16
{- 16. Dada una lista de enteros, retornar los elementos pares que sean mayores a 10. -}
pairsGreater10 :: [Int] -> [Int]
pairsGreater10 xs = [x | x <- xs, x `mod` 2 == 0, x > 10]

-- Exercise 17
{- 17. Dado un entero, retornar sus divisores. -}
dividers :: Int -> [Int]
dividers n = [x | x <- [1..n], n `mod` x == 0]

-- Exercise 18
{- 18 *. Definir la función todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son elementos de ys. Por ejemplo: todosOcurrenEn [1,5,2,5] [5,1,2,4] = True,
todosOcurrenEn [1,5,2,5] [5,2,4] = False -}
todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
todosOcurrenEn xs ys = and [elem' x ys | x <- xs]

allTrue :: [Bool] -> Bool
allTrue xs = and [x == True | x <- xs]

-- helper function
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
    | e == x = True
    | otherwise = elem' e xs

-- Exercise 19
{- 19. Dado un natural n, retornar los números primos comprendidos entre 2 y n. -}
primesFrom2UntilN :: Int -> [Int]
primesFrom2UntilN n = [x | x <- [2..n],isPrime x]
    where
        isPrime :: Int -> Bool
        isPrime n = length [x | x <- [2..(n-1)], n `mod` x == 0] == 0

-- Exercise 20
{- 20. Dadas dos listas de naturales, retornar su producto cartesiano. -}
cartesianProduct :: [Int] -> [Int] -> [(Int, Int)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

-- Exercise 21
{- 21 *. Dadas una lista y un elemento retornar el número de ocurrencias del
elemento x en la lista ys. -}
numberOfOcurrences :: (Eq a) => [a] -> a -> Int
numberOfOcurrences xs e = length ([x | x <- xs, x == e])

-- Exercise 22
{- 22. Escribir la función split2 :: [a] - > [([a],[a])], que dada una lista
xs, devuelve la lista con todas las formas de partir xs en dos. Por ejemplo:
split2 [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]),([1,2,3],[])]. -}
split2 :: [a] -> [([a], [a])]
split2 xs = [(take i xs, drop i xs) | i <- [0..length xs]]

-- Exercise 23
{- 23 *. Definir una funci´on que, dada una lista de enteros, devuelva la suma de
la suma de todos los segmentos iniciales.
Por ejemplo: sumaSeg [1,2,3] = 0 + 1 + 3 + 6 = 10. -}
-- sumaSeg :: [Int] -> Int
sumaSeg xs = sum [sum seg | seg <- initSegs xs]
    where
        initSegs :: [Int] -> [[Int]]
        initSegs [] = [[]]
        initSegs xs = [take i xs | i <- [0..length xs]]

-- Exercise 24
{- 24. Definir la lista infinita de los números pares. -}
infiniteListOfEvenNumbers :: [Int]
infiniteListOfEvenNumbers = [x | x <- [0..], x `mod` 2 == 0]