module Main where 
import System.Environment

main :: IO ()
main = do 
    args <- getArgs 
    if length args == 2 then 
         putStrLn $ "Hello: " ++ args !! 0 ++ "\nYou are " ++ args !! 1 ++ " years old" 
        else 
         putStrLn "Illegal number of arguments" 

    
