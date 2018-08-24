findAns :: Int -> Int -> Int
findAns a b = undefined

main :: IO ()
main = do
    [a, b] <- map read <$> (words <$> getLine)
    putStrLn $ findAns a b
