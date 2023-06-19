-- Taller: Introducción a GHCI

-- Exercise 1
{- 1. Utilize Ghci para decidir si las expresiones (2^29)/(2^9) y 2^20 son iguales.
Recuerde que el operador de potenciación en Hugs es infijo y se escribe “ˆ”. -}
equalExpressions :: (Num a, Eq a) => a -> a -> Bool
equalExpressions x y = x == y

-- Exercise 2
{- 2. Utilizando las funciones head y tail, y dada la lista “hola mundo”,
obtenga el segundo elemento de la misma (la letra “o”). -}
getSecondElementString :: String -> Char
getSecondElementString str = head (tail str)

-- Exercise 3
{- 3. Utilizando las funciones head y reverse, y dada la lista “hola mundo”,
obtenga el último elemento de la misma (la letra “o”). -}
getLastElementString :: String -> Char
getLastElementString str = head (reverse str)

-- Exercise 4
{- 4. Podemos examinar los tipos de algunas expresiones a partir del comando :t, el cual, 
seguido de una expresión válida nos dice su tipo.
Ej:
:t tail
tail :: [a] −→ [a]
Examina los tipos de las expresiones de los ejercicios que siguen. -}
-- Resolved.

-- Exercise 5
{- 5. Utilizando la función realizada en el ejercicio anterior y la función mod
determine si un número, representado como la lista de sus dígitos (ej: 123
= [1,2,3]) es par. -}
-- Auxiliary function
getLastInt :: [Int] -> Int
getLastInt xs = head (reverse xs)

isNumberEven :: [Int] -> Bool
isNumberEven xs = getLastInt (xs) `mod` 2 == 0

-- Exercise 6
{- 6. Utilizando la función sum, la función mod y un número representado de
igual manera que en el [ item 5 ] determine si un número es múltiplo de 3. -}
isMultiple3 :: [Int] -> Bool
isMultiple3 xs = sum xs `mod` 3 == 0

-- Exercise 7
{- 7. Utilizando las funciones de los [ items 5, 6 ] determine si un número es
múltiplo de 6. -}
isMultiple6 :: [Int] -> Bool
isMultiple6 xs = isNumberEven xs && isMultiple3 xs

-- Exercise 8
{- 8. Escriba una función que dado un número retorne la lista de sus digitos. -}
intToListDigits :: Int -> [Int]
intToListDigits n
    | n < 10 = [n]
    | otherwise = intToListDigits (n `div` 10) ++ [n `mod` 10]

-- Exercise 9
{- 9. Utilizando las funciones reverse y == determine si una frase, representada como un string, 
es un palíndromo. -}
isPalindromePhrase :: String -> Bool
isPalindromePhrase phrase = phrase == reverse phrase

-- Exercise 10
{- 10. ¿Que arrojará como resultado la evaluación de la siguiente expresión en Hugs?
(head.(drop 3)) "0123456"
¿Que tipo tiene el valor resultante? ¿Que función sobre listas de las dadas
en clase se podría implemntar de esta manera? ¿ Investigue qué comando
se usa en ghci para saber el número de pasos realizados por el interprete. -}

{- La evaluación de la expresión {(head.(drop 3)) "0123456"} arrojá como resultado el valor '3', 
   el cual es de tipo caracter. El tipo de valor resultante es de tipo Char. 
La función sobre listas dadas en clase que se podría implementar de esta manera sería la denominada 'tail' 
y el tipo de valor resultado es un 'int'.
El comando se usa en ghci para saber el número de pasos realizados por el interprete es trace. -}

-- Exercise 11
{- 11. Utilizando ghc compile alguna de sus funciones para obtener código ejecutable. -}
main :: IO ()
main = do
    let resultEqualExpression = equalExpressions ((2^29) / (2^9)) (2^20)
    let listDigitsToInt = 12345
    print resultEqualExpression
    print listDigitsToInt
-- Resolved.

-- Exercise 12
{- Utilice Cabal para crear un proyecto, el proyecto debe ejecutar la función
reverse sobre una texto que se de como entrada. -}
-- Resolved.