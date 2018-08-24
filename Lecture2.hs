data Point = MkPoint Double Double -- | MkPoint3 Int Int Int
    deriving Show

getX :: Point -> Double
getX (MkPoint x y)    = x
--getX (MkPoint3 x y z) = x

getY :: Point -> Double
getY (MkPoint x y)    = y
--getY (MkPoint3 x y z) = y

last3 :: [a] -> a
--last3 (x : y : z : []) = x
last3 [x, y, z] = x
last3 (x : xs)  = last3 xs

dist :: Point -> Point -> Double
--dist (MkPoint x1 y1) (MkPoint x2 y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)
--dist (MkPoint x1 y1) (MkPoint x2 y2) =
--    let xx = (x1 - x2) ** 2
--        yy = (y1 - y2) ** 2
--    in  sqrt (xx + yy)

dist (MkPoint x1 y1) (MkPoint x2 y2) = sqrt (xx + yy)
  where
    xx :: Double
    xx = (x1 - x2) ** 2
    yy = (y1 - y2) ** 2

if' :: Bool -> a -> a -> a
if' True x y  = x
if' False x y = y

{- if cond then a else b -}

{- guards -}
f :: Int -> Int
--f x = if x `mod` 2 == 0 then x `div` 2 else 3 * x + 1
f x | x `mod` 2 == 0     =     x `div` 2
    | x `mod` 2 /= 0     =     3 * x + 1

factorial :: Integer -> Integer
factorial n
    | n == 0    = 1
    | otherwise = n * factorial (n - 1)

last3' :: [a] -> a
last3' xs = case xs of
    [x, y, z] -> x
    (y : ys)  -> last3' ys

{- lambda function -}
add = \a b -> a + b
