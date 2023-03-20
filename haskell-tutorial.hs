-- Comments
{- Comments -}

-- Import modules
import Data.List
import System.IO

-- module SampFunctions (getClass, doubleEvenNumbers) where
-- import SampFunctions

-- Int -2^63 2^63

maxInt = maxBound :: Int
minInt = minBound :: Int

-- Integer
-- Float
-- Double
bigFloat = 3.99999 + 0.00005

-- Bool True False
-- Char ''
-- Tuple

always5 :: Int
always5 = 5

sumOfNums = sum[1..1000]
addEx = 5 + 4
subEx = 5 - 4
multEx = 5 * 4
divEx = 5 / 4

modEx = mod 5 4
modEx2 = 5 `mod` 4

negNumEx = 5 + (-4)
num9 = 9 :: Int
sqrtOf9 = sqrt (fromIntegral num9)


-- Built in math functions
piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
truncateVal = truncate 9.999
roundVal = round 9.999
ceilingVal = ceiling 9.999
floorVal = floor 9.999

-- Also sin, cos, tan, asin, atan, acos, sinh, tanh, cosh, asinh, atanh, acosh

-- Logic operators
trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

primeNumbers = [3, 5, 7, 11]
morePrimes = primeNumbers ++ [13, 17, 19, 23, 29]

favNums = 2 : 7 : 21 : 66 : []

multList = [[3, 5, 7], [11, 13, 17]]
morePrimes2 = 2 : morePrimes
lenPrime = length morePrimes2

revPrime = reverse morePrimes2
isListEmpty = null morePrimes2
secondPrime = morePrimes2 !! 1

firstPrime = head morePrimes2
lastPrime = last morePrimes2
primeInit = init morePrimes2
first3Primes = take 3 morePrimes2
removedPrimes = drop 3 morePrimes2

is7InList = 7 `elem` morePrimes2

maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2

newList = [2, 3, 5]
prodPrimes = product newList

zeroToTen = [0..10]

evenList = [2, 4..20]

letterList = ['A', 'C'.. 'Z']

infinPow10 = [10,20..]

many2s = take 10 (repeat 2)

many3s = replicate 10 3

cycleList = take 10 (cycle [1,2,3,4,5])

listTimes2 = [x * 2 | x <- [1..10]]
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]

divisBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]
sortedList = sort [9,1,8,3,4,7,6]

sumOfList = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

listBiggerThen5 = filter (>5) morePrimes
evensUpTo20 = takeWhile (<=20) [2,4..20]
evensUpTo20Lazzy = takeWhile (<=20) [2,4..]

multOfListL = foldl (*) 1 [2,3,4,5]
multOfListR = foldr (*) 1 [2,3,4,5]

pow3List = [3^n | n <- [1..10]]
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]

randTuple = (1, "Random Tuple")
tomiRodeghiero = ("Tomi Rodeghiero", 19)
tomisName = fst tomiRodeghiero
tomisAge = snd tomiRodeghiero

names = ["Fer", "Fran", "Delfi", "Tomi"]
ages = ["43 years old", "24 years old", "10 years old", "19 years old"]

namesNAges = zip names ages

main = do
    putStrLn "What's your name"
    name <- getLine
    putStrLn ("Hello " ++ name)

addMe :: Int -> Int -> Int

-- funcName param1 param2 = operations (returned value)

addMe x y = x + y
sumMe x y = x + y
addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You're an adult"

-- whatAge x = "Nothing important"
whatAge _ = "Nothing important"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 3 * factorial(2) : 2 = 6
-- 2 * factorial(1) : 1 = 2
-- 1 * factorial(0) : 1

prodFact n = product [1..n]

isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

whatGrade :: Int -> String
whatGrade age
    | (age >= 5) && (age <= 6) = "Kindergarten"
    | (age > 6) && (age <= 10) = "Elementary School"
    | (age > 10) && (age <= 14) = "Middle School"
    | (age > 14) && (age <= 18) = "High School"
    | otherwise = "Go to college"

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
    | avg <= 0.200 = "Terrible Batting Average"
    | avg <= 0.250 = "Average Player"
    | avg <= 0.280 = "Your doing pretty good"
    | otherwise = "You're a superstar"
    where avg = hits / atBats

getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and" ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ " and the rest are " ++ show xs

getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]

times4 :: Int -> Int
times4 x = x * 4

listTimes4 :: Int -> [Int]
listTimes4 _ = map times4 [1,2,3,4,5]
multBy4 :: [Int] -> [Int]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs

-- [1,2,3,4] : x = 1 | xs = [2,3,4]
-- [2,3,4] : x = 2 | xs = [3,4]
-- xs = [3,4] : x = 3 | xs = [4]

areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False

doMult :: (Int -> Int) -> Int
doMult func = func 3
num3Times4 = doMult times4

getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y

adds3 = getAddFunc 3

fourPlus3 = adds3 4

threePlusList = map adds3 [1,2,3,4,5]

dbl1To10 = map (\x -> x * 2) [1..10]

-- < > <= >= == /=
-- && || not
doubleEvenNumber y =
    if (y `mod` 2 /= 0)
        then y
        else y * 2

getClass :: Int -> String
getClass n = case n of
    5 -> "Go to Kindergarten"
    6 -> "Go to elementary school"
    _ -> "Go away"

data BaseballPlayer = Pitcher
                    | Catcher
                    | Infielder
                    | Outfield
                deriving Show

player :: BaseballPlayer -> Bool
player Outfield = True
playerInOF = print(player Outfield)

data Customer = Customer String String Double
            deriving Show

customer :: Customer
customer = Customer "Tomás Rodeghiero" "399 Perú" 19

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b


data Numbers = Even | Odd
number :: Numbers -> Numbers -> String
number Even Odd = "Even Odd"
number _ _ = "Error"

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y)

sumValue = putStrLn (show (1 + 2))

sumValue2 = putStrLn . show $ 1 + 2

areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 10 10 100 100

-- Num Eq Or Show
-- (+)
data Employee = Employee { name :: String, position :: String, idNum:: Int } deriving (Eq, Show)
franRodeghieroEmployee = Employee { name = "Francisco", position = "Employe1", idNum = 1000 }
tomiRodeghieroEmployee = Employee {name = "Tomás", position = "Employe2", idNum = 1001 }

isFranTomi = franRodeghieroEmployee == tomiRodeghieroEmployee

tomiRodeghieroData = show tomiRodeghieroEmployee

data ShirtSize = S | M | L
instance Eq ShirtSize where
    S == S = True
    M == M = True
    L == L = True
    _ == _ = False

instance Show ShirtSize where
    show S  = "Small"
    show M = "Medium"
    show L = "Large"

smallAvail = S `elem` [S, M, L]
theSize = show S

class MyEq a where
    areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
    areEqual S S = True
    areEqual M M = True
    areEqual L L = True
    areEqual _ _ = False

newSize = areEqual M M

sayHello = do
    putStrLn "What's your name"
    name <- getLine
    putStrLn $ "Hello " ++ name

writeToFile = do
    theFile <- openFile "test.txt" WriteMode
    hPutStrLn theFile ("Random line of text")
    hClose theFile

readFromFile = do
    theFile2 <- openFile "test.txt" ReadMode
    contents <- hGetContents theFile2
    putStr contents
    hClose theFile2

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]
-- 1, 1, 2, 3, 5, 8 .....
-- [1, 1, 2]
-- 1st : fib = 1 and (tail fib) = 1
-- [1, 1, 2] : a : 1 + b: 1 = 2

-- 2nd : fib = 1 and (tail fib) = 2
-- [1, 1, 2, 3] : a: 1 + b: 1 = 2

-- 3th : fib = 1 and (tail fib) = 2
-- [1, 1, 2, 3] : a: 2 + b: 2 = 3

fib300 = fib !! 300