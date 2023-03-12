module Main where 
import System.Environment

main :: IO ()
main = do 
    putStr "What is your Name: "
    name <- getLine 
    putStrLn $ "Hello " ++ name


    
