-- Exercise 1
{- 1. Leer los captulos 1 y 2 del libro Aprende Haskell por el bien de todos! -}

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
last' xs = head (reverse xs)

-- • init:: [A] -> [A] retorna toda la lista menos el último elemento.
init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' (x:xs) = x : init' xs

-- Exercise 3
{- 3. Defina una función máximo de tres, tal que maxTres x y z es el máximo
valor entre x, y, z . Por ejemplo: maxTres 6 7 4 = 7. -}
maxTres :: Int -> Int -> Int -> Int
maxTres x y z = max x (max y z)

-- Exercise 4
{- 4. Defina las siguientes operaciones sobre listas (vistas en el teórico): concatenar, tomar, tirar y C. -}
concatenate :: [a] -> [a] -> [a]
concatenate [] ys = ys
concatenate (x:xs) ys = x : concatenate xs ys

take' :: Int -> [a] -> [a]
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

insertToFinish :: a -> [a] -> [a]
insertToFinish z [] = [z]
insertToFinish z (x:xs) = x : insertToFinish z xs

-- Exercise 5
{- 5. Defina una función abs: Int -> Int que calcula el valor absoluto de un número. -}
abs' :: Int -> Int
abs' n
    | n < 0 = (-n)
    | otherwise = n

-- Exercise 6
{- 6 *. Defina una función edad :: (Nat,Nat,Nat) -> (Nat,Nat,Nat) -> Int
que dada dos fechas indica los años transcurridos entre ellas. Por ejemplo: edad (20,10,1968) (30,4,1987) = 18 -}
edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (day1, month1, year1) (day2, month2, year2)
    | year2 < year1 = error "The second day must be later than the first"
    | year2 == year1 = 0
    | month2 < month1 = year2 - year1 - 1
    | month2 > month1 = year2 - year1
    | otherwise = if (day2 < day1) then year2 - year1 - 1 else year2 - year1

-- Exercise 7
{- 7. La disyunción excluyente xor de dos fórmulas se verifica si una es verdadera
y la otra es falsa. Defina la función xor que calcule la disyunción excluyente a
partir de la tabla de verdad. -}
xor :: Bool -> Bool -> Bool
xor False False = False
xor False True = True
xor True False = True
xor True True = False

{- *. Ahora defina la función xor2 que calcule la disyunción excluyente pero sin
que considere todos los posibles valores de las entradas. Cuál será la diferencias
entre ambas definiciones? -}
xor' :: Bool -> Bool -> Bool
xor' p q
    | p == q = False
    | otherwise = True

-- La diferencia entre ambas definiciones es la simplifación de la definición en la función.

-- Exercise 8
{- 8. Defina una función que dado un número natural, decida si el mismo es primo o no. -}
-- Version 1
prim :: Int -> Bool
prim 1 = False
prim 2 = True
prim n = not (prim' n (n-1))

-- Auxiliary function
prim' :: Int -> Int -> Bool
prim' x 1 = False
prim' x y = mod x y == 0 || prim' x (y-1)

-- Version 1
prime :: Int -> Bool
prime n = divsSqrt n == []

-- Auxiliary functions
isqrt :: Float -> Int
isqrt n = floor(sqrt n)

divsSqrt :: Int -> [Int]
divsSqrt n = [x | x <- [2..isqrt(fromIntegral n)], n `mod` x == 0]

-- Exercise 9
{- 9 *. Defina una función que dado un número natural n, retorne la lista de todos
los números naturales primos menores que n. -}
listPrimesLessThanN :: Int -> [Int]
listPrimesLessThanN n = [x | x <- [2..n-1], prime x]

-- Exercise 10
{- 10. Defina una función que dada una lista, retorne la reversa de la misma. -}
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

-- Exercise 11
{- 11. Defina una función que dadas dos listas, decida si las listas son iguales. -}
equalLists :: Eq a => [a] -> [a] -> Bool
equalLists [] [] = True
equalLists _ [] = False
equalLists [] _ = False
equalLists (x:xs) (y:ys) = x == y && equalLists xs ys

-- Exercise 12
{- 12 *. Defina una función que dada una lista decida si es un palíndromo o no. -}
palindrome:: Eq a => [a] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome (x:xs) = x == (head (reverse (x:xs))) && palindrome (tail (init (x:xs)))

-- Exercise 13
{- 13. Defina una función que dados tres números a, b, c devuelva la cantidad de
raíces reales de la ecuación ax2 + bx + c = 0 -}
linearRootsQuadraticEquation :: Int -> Int -> Int -> Int
linearRootsQuadraticEquation a b c
    | discriminant < 0 = 0 -- no real roots
    | discriminant == 0 = 1 -- one real root (double root)
    | otherwise = 2
    where discriminant = b ^ 2 - 4 * a * c

-- Function to calculate the length of a list
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs