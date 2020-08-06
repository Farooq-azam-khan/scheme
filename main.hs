module Main where
import System.Environment
import Control.Monad -- for liftM

import Datatypes
import Parser 
import Eval 

main :: IO()
main = do 
    args <- getArgs 
    putStrLn $ lispExecute (args !! 0)

lispExecute x = do 
    evaled <- return $ liftM show $ readExpr x >>= eval 
    extractValue $ trapError evaled
