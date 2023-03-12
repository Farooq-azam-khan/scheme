module Main where 
import System.Environment

main :: IO ()
main = do 
    args <- getArgs 
    if length args == 2 then 
        let 
            num1 = args !! 0
            num2 = args !! 1
            result = read num1 + read num2 
        in 
            putStrLn $ num1 ++ "+" ++ num2 ++ "=" ++ show result
        else 
         putStrLn "Illegal number of arguments" 


    
