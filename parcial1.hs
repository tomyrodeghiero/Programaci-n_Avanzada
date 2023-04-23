-- Exercise 1
data BinTree a = Nil | Node (BinTree a) a (BinTree a) deriving (Show)

sumNodes :: BinTree Int -> Int
sumNodes Nil = 0
sumNodes (Node hi r hd) = r + (sumNodes hi) + (sumNodes hd)
-- Node (Node Nil 4 Nil) 4 (Node (Node Nil 7 Nil) 9 (Node Nil 3 Nil))

-- Exercise 2
{- 
. Cte 2
. Sum (Cte 1) (Cte 2)
. Mul (Cte 1) (Sum (Cte 2) (Cte 3))
. Sum (Mul  (Cte 1) (Cte 2)) (Mul (Cte 3) (Cte 4)))
 -}

 -- Sum (Cte 4) (Mul (Cte 4) (Cte 9))

 -- Exercise 3
compact :: [Int] -> [Int]
compact [] = error "La lista debe contener al menos un elemento"
compact (x:y:xs)
    | x == y = compact (y:xs)
    | otherwise = x : compact (y:xs)

 -- Exercise 4
square :: Int -> Int
square x = x * x

and' :: Bool -> Bool -> Bool
and' True y = y
and' False x = False

inf :: Int
inf = inf + 1