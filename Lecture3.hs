data Pair a b = Pair a b
    deriving (Eq, Ord, Show, Read)

first :: Pair a b -> a
first (Pair x y) = x

second :: Pair a b -> b
second (Pair x y) = y

class Eq' a where
    isEqual :: a -> a -> Bool

instance Eq' Int where
    isEqual a b = (a == b)
{-
instance (Eq a, Eq b) => Eq (Pair a b) where
    (==) (Pair x1 y1) (Pair x2 y2) = x1 == x2 && y1 == y2

instance (Ord a, Ord b) => Ord (Pair a b) where
    (<=) (Pair x1 y1) (Pair x2 y2) = x1 < x2 || x1 == x2 && y1 <= y2
    -}

data Nat = Zero | Succ Nat
    deriving Show

instance Num Nat where
    (+) Zero n     = n
    (+) (Succ n) m = Succ (n + m)

    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger (n - 1))
