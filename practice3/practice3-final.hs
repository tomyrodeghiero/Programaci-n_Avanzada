import Data.List (elemIndices)

-- Exercise 1
{- 1. Define una función que, dadas dos listas ys y xs de naturales ordenadas,
retorne el merge de estas listas, es decir, la lista ordenada compuesta por los
elementos de ys y xs. -}
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Exercise 2
{- 2. Define una función que, dada una lista de naturales, la ordene. -}
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort left ++ [x] ++ quickSort right
    where
        left = [a | a <- xs, a <= x]
        right = [b | b <- xs, b > x]

-- Exercise 3
{- 3. Define una función que, recursivamente y sólo utilizando adición y multiplicación, 
calcule, dado un natural n, el número 2^n. -}
powInBase2 :: Int -> Int
powInBase2 0 = 1
powInBase2 1 = 2
powInBase2 n = 2 * powInBase2 (n-1)

-- Exercise 4
{- 4. Define una función que, dado un número natural n, retorne su representación
binaria como secuencia de bits. -}
toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = reverse(toBinaryHelper n)
    where
        toBinaryHelper 0 = []
        toBinaryHelper x = (x `mod` 2) : toBinaryHelper (x `div` 2)

-- Exercise 5
{- 5 *. Define una función que, dado un número natural n en su representación
binaria, decida si n es par o no. -}
isEvenNumberInBinary :: [Int] -> Bool
isEvenNumberInBinary numberBinary = last numberBinary == 0

-- Exercise 6
{- 6. Define la función que retorne la distancia de Hamming: dadas dos listas es el
número de posiciones en que los correspondientes elementos son distintos. Por
ejemplo: distanciaH ”roma””camino”− > 3
distanciaH ”romano””rama”− > 1 -}
hamming :: Eq a => [a] -> [a] -> Int
hamming [] [] = 0
hamming xs [] = 0
hamming [] ys = 0
hamming (x:xs) (y:ys)
    | x /= y = 1 + hamming xs ys
    | otherwise = hamming xs ys

-- Exercise 7
{- 7. Define la función que, dado un número natural, decida si el mismo es un
cuadrado perfecto o no. -}
perfectSquare :: Int -> Bool
perfectSquare n
    | n == head(reverse (divsSqrt n)) * head (reverse (divsSqrt n)) = True
    | otherwise = False

-- helper functions
isqrt :: Float -> Int
isqrt n = floor(sqrt n)

divsSqrt :: Int -> [Int]
divsSqrt n = [x | x <- [1..isqrt(fromIntegral n)], n `mod` x == 0]

-- Exercise 8
{- 8. Define la función repetidos de forma tal que dado un elemento z y un entero n; 
z aparece n veces. -}
repetidos :: a -> Int -> [a]
repetidos _ 0 = []
repetidos z 1 = [z]
repetidos z n = z : repetidos z (n-1)

-- Exercise 9
{- 9. Define la función nelem tal que nelem xs n es elemento enésimo de xs,
empezando a numerar desde el 0. Por ejemplo:
nelem [1, 3, 2, 4, 9, 7] 3 -> 4 -}
nelem :: [a] -> Int -> a
nelem (x:xs) n
    | n == 0 = x
    | otherwise = nelem xs (n-1)

-- Exercise 10
{- 10 *. Define la función posicionesC tal que posicionesC xs c es la lista de la
posiciones del caracter c en la cadena xs. Por ejemplo:
posicionesC ”Catamarca”'a'− > [1, 3, 5, 8] -}
-- Version 1
posicionesC :: [Char] -> Char -> [Int]
posicionesC [] c = []
posicionesC (x:xs) c = calculatePositions (x:xs) c 0

calculatePositions :: [Char] -> Char -> Int -> [Int]
calculatePositions [] c _ = []
calculatePositions (x:xs) c init = if (x == c) then init : calculatePositions xs c (init + 1) else calculatePositions xs c (init + 1)

-- Version 2
posicionesC' :: [Char] -> Char -> [Int]
posicionesC' xs c = elemIndices c xs

-- Exercise 11
{- 11. Define la función compact, dada una lista retorna la lista sin los elementos
repetidos consecutivos. Por ejemplo: compact [1, 3, 3, 5, 8, 3] = [1, 3, 5, 8, 3] -}
compact :: Eq a => [a] -> [a]
compact [] = []
compact [x] = [x]
compact (x:y:xs)
    | x == y = compact (y:xs)
    | otherwise = x : compact (y:xs)