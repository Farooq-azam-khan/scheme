module Main where
import System.Environment
import Control.Monad
import System.IO 


import Datatypes
import Parser 
import Eval 

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"

lispExecute x = runOne x 

-- ========= repl =========
flushStr :: String -> IO() 
flushStr str = putStr str >> hFlush stdout 

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String 
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env 

evalAndPrint :: Env -> String -> IO () 
evalAndPrint env expr = evalString env expr >>= putStrLn 

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint


runOne :: String -> IO () 
runOne expr = nullEnv >>= flip evalAndPrint expr 
