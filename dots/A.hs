makeTriangle :: Int -> String
makeTriangle n = undefined

main :: IO ()
main = do
    n <- read <$> getLine
    putStrLn $ makeTriangle n

{- Equivalent definitions

main :: IO ()
main = read <$> getLine >>= putStrLn . makeTriangle

main :: IO ()
main =
    read <$> getLine >>= \n ->
    putStr (makeTriangle n)

-}
