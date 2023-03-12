module Main where 
import System.Environment 

get_debug_value maybe_debug = 
    case maybe_debug of 
        Nothing -> "False" 
        Just x -> x 

main :: IO () 
main = do 
    args <- getArgs
    prog_name <- getProgName 
    maybe_debug <- lookupEnv "DEBUG"
    putStrLn $ "DEBUG="++ get_debug_value maybe_debug
    case length args > 0 of 
        True -> putStrLn $ "(From: " ++ prog_name ++ ") " ++ "Hello, " ++ args !! 0
        False -> putStrLn $ "(From: " ++ prog_name ++ ") "

new_fn :: Int -> Int -> Int 
new_fn x =  (+) x
