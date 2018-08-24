add :: Int -> Int -> Int
add a b = a + b

addd a b = a + b

andd' :: Bool -> (Bool -> Bool)
andd' a b = a && b

(+-*/^++==<>/=) :: Int -> Int -> Int
(+-*/^++==<>/=) a b = min a b
infixr 9 +-*/^++==<>/=

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)


fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

head' :: [a] -> a
head' (x : xs) = x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] b              = []
zip' a []              = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

head2 :: [a] -> a
head2 (x : y : ys) = y

--data Nat = One | Next Nat

getAt :: Int -> [a] -> a
getAt 0 (x : xs) = x
getAt n (x : xs) = getAt (n - 1) xs

data Point
    = Point2 Int Int
    | Point3 Int Int Int
    deriving Show

getX (Point2 x y)   = x
getX (Point3 x y z) = x

getY (Point2 x y) = y


data IntList
    = EmptyList
    | ConsList Int IntList
    deriving Show


