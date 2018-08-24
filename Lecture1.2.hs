import           Data.List

x :: Int
x = 123 + 321

f :: Int -> Int
f n = n + 1

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = factorial (n - 1) * n

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

(+-*/^++-%#$&&) :: Int -> Int -> Int
(+-*/^++-%#$&&) a b = a + b + a + b
infixr 2 +-*/^++-%#$&&

(+-*/^++-%#$) :: Int -> Int -> Int
(+-*/^++-%#$) a b = a + b + a + b
infixl 2 +-*/^++-%#$

data Point
    = Point Int Int
    | Point3d Int Int Int
    deriving Show

getX :: Point -> Int
getX (Point x y)     = x
getX (Point3d x y z) = x

getY :: Point -> Int
getY (Point x y)     = y
getY (Point3d x y z) = y

data IntList
    = EmptyList
    | Cons Int IntList
    deriving Show

intListToList :: IntList -> [Int]
intListToList EmptyList   = []
intListToList (Cons x xs) = x : intListToList xs

listToIntList :: [Int] -> IntList
listToIntList []       = EmptyList
listToIntList (x : xs) = Cons x (listToIntList xs)
