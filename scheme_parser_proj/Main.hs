module Main where

import Text.ParserCombinators.Parsec hiding (spaces) 
import System.Environment 
import Control.Monad 
import Numeric 

data LispVal = Atom String 
                | List [LispVal]
                | DottedList [LispVal] LispVal 
                | Number Integer 
                | BinaryNumber String 
                | String String 
                | Bool Bool 
                deriving (Show)

symbol :: Parser Char 
symbol = oneOf "!$%&|*+-/:<=>?@^_~" 

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
    return $ Atom atom 

parseBool :: Parser LispVal 
parseBool = do 
    char '#' 
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))
parseNumber :: Parser LispVal 
parseNumber = parseDecimal <|> parseBinaryHexOrOct 

parseDecimal :: Parser LispVal 
parseDecimal = liftM (Number . read) $ many1 digit 
{-parseNumber = do 
    opt <- many1 digit 
    return $ (Number . read) opt 
-}
-- parseNumber = many1 digit >>=  (return . Number . read )


parseBinaryHexOrOct :: Parser LispVal 
parseBinaryHexOrOct = do 
    char '#' 
    which_base <- oneOf "box" 
    spaces 
    case which_base of 
        'b' -> do 
            number_as_string <- many1 (oneOf "01") 
            return $ BinaryNumber number_as_string
        'o' -> do 
            number_as_string <- many1 (oneOf "01234567")
            return $ Number $ oct_to_dec number_as_string 
        'x' -> do 
            number_as_string <- many1 (oneOf "0123456789abcdefABCDEF") 
            return $ Number $ hex_to_dec number_as_string

hex_to_dec :: String -> Integer 
hex_to_dec x = fst $ readHex x !! 0

oct_to_dec :: String -> Integer 
oct_to_dec x = fst $ readOct x !! 0
            
language_name :: String 
language_name = "lisp" 

scheme_parser :: Parser LispVal 
scheme_parser = parseAtom <|> 
                parseNumber <|> 
                parseBool <|>
                parseString 

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
