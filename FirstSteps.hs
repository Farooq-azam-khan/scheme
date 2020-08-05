module Main where 
import System.Environment

main :: IO()
main = do
    args <- getArgs
    -- putStrLn ("Hello, " ++ args !! 0)
    -- excercise 1
    -- putStrLn ("Hello, " ++ args !! 0 ++ " " ++ args !! 1)
    
    -- excercise 2
    -- putStrLn $ show $ (read (args !! 0) :: Int) + (read (args !! 1) :: Int)

    -- excercise 3
    putStrLn "What is you Name: "
    name <- getLine
    putStrLn ("Hello, " ++ name)

