module Lecture1 where

andd :: Bool -> (Bool -> Bool)
andd a b = a && b
infixr 3 `andd`

main :: IO ()
main = putStrLn "Hello, world!"

(*^%$++^-) :: Int -> Int -> Int
a *^%$++^- b = a + a + b + b
infixl 9 *^%$++^-

len :: [a] -> Int
len []       = 0
len (x : xs) = 1 + len xs

fact :: Integer -> Integer
fact 0 = 1
fact a = a * fact (a - 1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

data IntList
    = EmptyIntList
    | ConsIntList Int IntList
    deriving (Show)

data Nat
    = Zero
    | Next Nat

toInt :: Nat -> Int
toInt Zero     = 0
toInt (Next a) = 1 + toInt a

listToIntList :: [Int] -> IntList
listToIntList []       = EmptyIntList
listToIntList (x : xs) = ConsIntList x (listToIntList xs)
--listToIntList a  = head a `ConsIntList` listToIntList (tail a)
