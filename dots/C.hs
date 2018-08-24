findAns :: Int -> [Int] -> Maybe Int
findAns n xs = undefined

main :: IO ()
main = do
    n <- read <$> getLine
    xs <- map read <$> (words <$> getLine)
    case findAns n xs of
        Nothing -> putStrLn "NO"
        Just ans -> do
            putStrLn "YES"
            print ans
