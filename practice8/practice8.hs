-- Exercise 1
{- Definir la función nand a b = not (a && b) en Haskell sin utilizar not y &&. -}
nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

-- Exercise 2
{- Definir en Haskell la función
    maj : : Bool −> Bool −> Bool −> Bool
    −−retorna True sii al menos 2 argumentos son True -}
maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj True _ True = True
maj _ True True = True
maj _ _ _ = False

-- Exercise 3
-- ---------------------------------------------------- 
-- Para las siguientes funciones se debe respetar el 
-- perfil propuesto.
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a].
-- Mientras que (Int -> [a] -> Bool) es la propiedad.
--		Ejemplo: paraTodo [0,1,2,3] [4,1,2,6] even 
--		retorna False, ya que existe una posición 
--		en la que el elemento de la lista es impar. 
--		paraTodo [0,2,4,6] [2,2,4,4,4,5,6] even  
--		retorna True.
-- ----------------------------------------------------
paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo [] _ _ = True
paraTodo (i:is) xs p
    | p i xs = paraTodo is xs p
    | otherwise = False

-- Auxiliary function
even' :: Integral a => Int -> [a] -> Bool
even' i xs = even (xs !! i)

-- ----------------------------------------------------
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a]. 
-- (Int -> [a] -> Bool) es la propiedad.
--
--		Ejemplo: existe [0,1,2,3] [4,1,2,6] odd
--		retorna True.
-- ----------------------------------------------------
existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe [] _ _ = False
existe (i:is) xs p
    | p i xs = True
    | otherwise = existe is xs p

-- Auxiliary function
odd' :: Integral a => Int -> [a] -> Bool
odd' i xs = odd (xs !! i)

-- Exercise 4
{- Utilizando las ideas asociadas a listas por comprensión, y las funciones
sum, product, y length, escribir los cuantificadores de sumatoria, productoria y
contatoria para ejemplos concretos. -}
sumatoria :: [Int] -> Int
sumatoria xs = sum [xs !! i | i <- [0..length xs-1]]

productoria :: [Int] -> Int
productoria xs = product [xs !! i | i <- [0..length xs-1]]

contatoria :: [a] -> (a -> Bool) -> Int
contatoria xs p = sum [1 | x <- xs, p x]