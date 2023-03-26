-- Exercise 1
{- 1. Utilize Ghci para decidir si las expresiones (229)/(29) y 220 son iguales.
Recuerde que el operador de potenciaci ́on en Hugs es infijo y se escribe “ˆ”. -}
equalExpressions :: Eq a => a -> a -> Bool
equalExpressions x y = x == y

-- Exercise 2
{- 2. Utilizando las funciones head y tail, y dada la lista “hola mundo”,
obtenga el segundo elemento de la misma (la letra “o”). -}

getSecondElementString :: String -> Char
getSecondElementString str = head(tail str)

-- Exercise 3
{- 3. Utilizando las funciones head y reverse, y dada la lista “hola mundo”,
obtenga el último elemento de la misma (la letra “o”). -}
getLastElementString :: String -> Char
getLastElementString str = head(reverse str)

getLastInt :: [Int] -> Int
getLastInt int = head(reverse int)

-- Exercise 4
{- 4. Podemos examinar los tipos de algunas expresiones a partir del comando :t, el cual, 
seguido de una expresión válida nos dice su tipo.
Ej:
:t tail
tail :: [a] −→ [a]
Examina los tipos de las expresiones de los ejercicios que siguen. -}

-- Exercise 5
{- 5. Utilizando la función realizada en el ejercicio anterior y la función mod
determine si un número, representado como la lista de sus dígitos (ej: 123
= [1,2,3]) es par. -}
isNumberEven :: [Int] -> Bool
isNumberEven number = getLastInt number `mod` 2 == 0

isNumberEven2 :: [Int] -> Bool
isNumberEven2 number = ((foldl (\acc x -> acc * 10 + x) 0 number) `mod` 2) == 0

-- Exercise 6
{- 6. Utilizando la función sum, la función mod y un número representado de
igual manera que en el [ item 5 ] determine si un número es múltiplo de 3. -}
isAMultipleNumberOf3 :: [Int] -> Bool
isAMultipleNumberOf3 number = (sum number) `mod` 3 == 0

-- Exercise 7
{- 7. Utilizando las funciones de los [ items 5, 6 ] determine si un número es
múltiplo de 6. -}
isAMultipleNumberOf6 :: [Int] -> Bool
isAMultipleNumberOf6 number = isNumberEven number && isAMultipleNumberOf3 number

-- Exercise 8
{- 8. Escriba una función que dado un número retorne la lista de sus digitos. -}
listDigitsToInt :: Int -> [Int]
listDigitsToInt number
                | number == 0 = [0] -- base case for number 0
                | otherwise = reverse (listDigitsToInt' number) -- reverse the list of digits so they are in the correct order

listDigitsToInt' :: Int -> [Int]
listDigitsToInt' number
                | number == 0 = []  -- base case for number 0
                | otherwise = (number `mod` 10) : listDigitsToInt' (number `div` 10)  -- add the current digit to the list and continue with the next one
-- On each call, module n with 10 return the rightmost digit, and integer diviison of n by 10 removes that digit, leaving us with the rest of the number

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
La función sobre listas dadas en clase que se podría implementar de esta manera sería la denominada 'tail'.
El comando se usa en ghci para saber el número de pasos realizados por el interprete es :set +s. -}

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
reverse sobre una texto que se d ́e como entrada. -}
-- Resolved.

