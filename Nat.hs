module Nat where

data Nat
    = Zero
    | Succ Nat
    deriving (Show)

natToInt :: Nat -> Int
natToInt Zero     = 0
natToInt (Succ n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n - 1))

add :: Nat -> Nat -> Nat
add Zero a     = a
add (Succ a) b = add a (Succ b)

sub :: Nat -> Nat -> Nat
sub (Succ a) (Succ b) = sub a b
sub Zero a            = Zero
sub a Zero            = a

mul :: Nat -> Nat -> Nat
mul Zero a     = Zero
mul (Succ a) b = add b (mul a b)

-- TODO fix
div' :: Nat -> Nat -> Nat
div' a Zero = Zero
div' Zero a = Zero
div' a b    = Succ (div' (sub a b) b)

pow :: Nat -> Nat -> Nat
pow = undefined

eq :: Nat -> Nat -> Bool
eq Zero     Zero     = True
eq (Succ a) (Succ b) = eq a b
eq a        b        = False

le :: Nat -> Nat -> Bool
le = undefined

leq :: Nat -> Nat -> Bool
leq a b = le a b || eq a b

ge :: Nat -> Nat -> Bool
ge a b = le b a

geq :: Nat -> Nat -> Bool
geq a b = leq b a
