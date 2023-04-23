-- Exercise 1
{- 1. Definir el tipo Nat, visto en el teórico. -}
data Nat = Zero | Succ Nat  deriving (Show)

-- Exercise 2
{- 2. Definir la función natToInt : Nat → Int que dado un número Nat retorna
su entero correspondiente. Por ejemplo: natToInt (Suc(Suc Zero)) = 2. -}
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

-- Exercise 3
{- 3. Definir la función intToNat : Int → Nat que dado un número entero retorna
su Nat correspondiente. Por ejemplo: intToNat 2 = (Suc(Suc Zero)). -}
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n-1))

-- Exercise 4
{- 4. Definir la función sumaNat : Nat → Nat → Nat, la cual suma dos números Nat. -}
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero Zero = Zero
sumaNat Zero (Succ n) = Succ n
sumaNat (Succ n) Zero = Succ n
sumaNat (Succ n1) (Succ n2) = sumaNat n1 (Succ (Succ n2))

-- Exercise 5
{- 5. Definir los árboles binarios. -}
data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)

-- Definir las siguientes funciones sobre  ́arboles binarios: size y height
-- Exercise 6
{- 6. La función size, que dado un  árbol retorna el número de nodos del árbol. -}
size :: Tree a -> Int
size Nil = 0
size (Node hi r hd) = 1 + size hi + size hd

-- Exercise 7
{- 7. La función height, que dado un árbol retorna la altura del mismo. -}
-- La rama de un árbol es la longitud de la rama más larga del Arbol, en la cual se cuentan por cantidad de nodos
height :: Tree a -> Int
height Nil = 0
height (Node hi r hd) = 1 + max (height hi) (height hd)

-- Annex
balance :: Tree a -> Bool
balance Nil = error "The tree must have at least one branch"
balance (Node hi r hd) = size hi == size hd
    where
        size :: Tree a -> Int
        size Nil = 0
        size (Node hi r hd) = 1 + size hi + size hd