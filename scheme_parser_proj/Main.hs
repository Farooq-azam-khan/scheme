module Main where

import Text.ParserCombinators.Parsec hiding (spaces) 
import System.Environment 
import Control.Monad 

data LispVal = Atom String 
                | List [LispVal]
                | DottedList [LispVal] LispVal 
                | Number Integer 
                | String String 
                | Bool Bool 
                deriving (Show)

symbol :: Parser Char 
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" 

parseString :: Parser LispVal 
parseString = do 
    char '"' 
    x <- many (noneOf "\"") 
    char '"' 
    return $ String x 

parseAtom :: Parser LispVal 
parseAtom = do 
    first <- letter <|> symbol 
    rest <- many (letter <|> digit <|> symbol) 
    let atom = first:rest 
    return $ case atom of 
                "#t" -> Bool True 
                "#f" -> Bool False 
                _ -> Atom atom 

parseNumber :: Parser LispVal 
-- parseNumber = liftM (Number . read) $ many1 digit 
{-parseNumber = do 
    opt <- many1 digit 
    return $ (Number . read) opt 
-}
parseNumber = many1 digit >>=  (return . Number . read )
language_name :: String 
language_name = "lisp" 

scheme_parser :: Parser LispVal 
scheme_parser = parseAtom 
                <|> parseString 
                <|> parseNumber  

readExpr :: String -> String 
readExpr input = case parse scheme_parser language_name input of 
                    Left err -> "No Match: " ++ show err 
                    Right val -> "Found Value: " ++ show val 

spaces :: Parser ()
spaces = skipMany1 space -- could use: lexeme  
main :: IO ()
main = do 
    (expr:_) <- getArgs 
    putStrLn (readExpr expr) 
