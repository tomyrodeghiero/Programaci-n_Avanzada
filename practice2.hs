-- Exercise 1
{- 1. Leer los captulos 1 y 2 del libro Aprende Haskell por el bien de todos! -}
-- Resolved.

-- Exercise 2
{- 2. Definir las siguientes funciones: -}
-- • hd :: [A] -> A retorna el primer elemento de una lista.
hd :: [a] -> a
hd (x:xs) = x

-- • tl :: [A] -> [A] retorna toda la lista menos el primer elemento.
tl :: [a] -> [a]
tl (x:xs) = xs

-- • last :: [A] -> A retorna el último elemento de la lista.
last' :: [a] -> a
last' (x:xs) = head(reverse(xs))

-- • init:: [A] -> [A] retorna toda la lista menos el último elemento.
init' :: [a] -> [a]
init' [] = []
init' (x:[]) = []
init' (x:xs) = x : init' (xs)

-- Exercise 3
{- 3. Defina una función máximo de tres, tal que maxTres x y z es el máximo
valor entre x, y, z . Por ejemplo: maxTres 6 7 4 = 7. -}
maxTres :: Int -> Int -> Int -> Int
maxTres x y z = max x (max y z)

-- Exercise 4
{- 4. Defina las siguientes operaciones sobre listas (vistas en el teórico): concatenar, tomar, tirar y C. -}
concatenate :: a -> [a] -> [a]
concatenate x ys = x : ys

take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 xs = []
take' n (x:xs)
    | n >= length (x:xs) = (x:xs)
    | otherwise = x : take' (n-1) xs

take'' :: Int -> [a] -> [a]
take'' n (x:xs)
    | n == 0 = []
    | n >= length (x:xs) = (x:xs)
    | otherwise =  x : take'' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop (n-1) xs

insertToFinish :: a -> [a] -> [a]
insertToFinish y [] = [y]
insertToFinish y (x:xs) = x : insertToFinish y xs

-- Exercise 5
{- 5. Defina una función abs: Int -> Int que calcula el valor absoluto de un número. -}
abs' :: Int -> Int
abs' n = if (n >= 0) then n else (n * (-1))

abs'' :: Int -> Int
abs'' n
    | n >= 0 = n
    | otherwise = n * (-1)

-- Exercise 6
{- 6 *. Defina una función edad :: (Nat,Nat,Nat) -> (Nat,Nat,Nat) -> Int
que dada dos fechas indica los añoos transcurridos entre ellas. Por ejemplo: edad (20,10,1968) (30,4,1987) = 18 -}
data Nat = Zero | Succ Nat
edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (day1, month1, year1) (day2, month2, year2) = (year2 - year1) - 1

-- Exercise 7
{- 7. La disyunción excluyente xor de dos fórmulas se verifica si una es verdadera
y la otra es falsa. Defina la función xor que calcule la disyunción excluyente a
partir de la tabla de verdad. -}
xor :: Bool -> Bool -> Bool
xor p q
    | p == True && q == True = False
    | p == True && q == False = True
    | p == False && q == True = True
    | p == False && q == False = False

{- *. Ahora defina la función xor2 que calcule la disyunción excluyente pero sin
que considere todos los posibles valores de las entradas. Cuál será la diferencias
entre ambas definiciones? -}
xor' :: Bool -> Bool -> Bool
xor' p q = if (p == q) then False else True
-- La diferencia entre ambas definiciones es la simplifación de la definición en la función.

-- Exercise 8
{- 8. Defina una función que dado un n ́umero natural, decida si el mismo es primo o no. -}
isqrt :: Float -> Int
isqrt n = floor(sqrt n)

divsSqrt :: Int -> [Int]
divsSqrt n = [x | x <- [2..isqrt(fromIntegral n)], n `mod` x == 0]

prime :: Int -> Bool
prime n = divsSqrt n == []

-- Exercise 9
{- 9 *. Defina una función que dado un número natural n, retorne la lista de todos
los números naturales primos menores que n. -}
listPrimesLessThanN :: Int -> [Int]
listPrimesLessThanN n = [x | x <- [1..n-1], prime x]

-- Exercise 10
{- 10. Defina una función que dada una lista, retorne la reversa de la misma. -}
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:[]) = [x]
reverseList (x:xs) = last' (x:xs) : reverseList (take (length xs) (x:xs))

-- Exercise 11
{- 11. Defina una función que dadas dos listas, decida si las listas son iguales. -}
equalLists :: Eq a => [a] -> [a] -> Bool
equalLists [] [] = True
equalLists [] _ = False
equalLists _ [] = False
equalLists (x:xs) (y:ys) = x == y && equalLists xs ys

-- Exercise 12
{- 12 *. Defina una función que dada una lista decida si es un palíndromo o no. -}
palindromeList :: Eq a => [a] -> Bool
palindromeList [] = True
palindromeList [x] = True
palindromeList (x:xs) = x == head (reverse (x:xs)) && palindromeList (init xs)

-- Exercise 13
{- 13. Defina una función que dados tres números a, b, c devuelva la cantidad de
raíces reales de la ecuación ax2 + bx + c = 0 -}
linearRootsQuadraticEquation :: Int -> Int -> Int -> Int
linearRootsQuadraticEquation a b c
    | discriminat < 0 = 0 -- no real roots
    | discriminat == 0 = 1 -- one real root (double root)
    | otherwise = 2
    where discriminat = b ^ 2 - 4 * a * c

-- Function to calculate the length of a list
lengthList :: [a] -> Int
lengthList [] = 0
lengthList (x:[]) = 1
lengthList (x:xs) = 1 + lengthList xs