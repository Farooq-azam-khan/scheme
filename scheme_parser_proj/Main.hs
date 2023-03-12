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
                | Float Float 
                | String String 
                | Bool Bool 
                deriving (Show)

symbol :: Parser Char 
symbol = oneOf "!$%&|*+-/:<=>?@^_~" 

binary_digits :: Parser Char 
binary_digits = oneOf "01" 

oct_digits :: Parser Char 
oct_digits = oneOf "01234567" 

hex_digits :: Parser Char 
hex_digits = oneOf "0123456789abcdefABCDEF"

escape_special_chars :: Parser Char 
escape_special_chars = do 
    char '\\' 
    x <- oneOf "\\\"ntr" 
    return $ case x of 
        'n' -> '\n' 
        'r' -> '\r' 
        't' -> '\t' 
        '\\' -> x 
        '"' -> x 

parseString :: Parser LispVal 
parseString = do 
    char '"' 
    x <- many $ escape_special_chars <|> noneOf "\"\\"
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

parseBinaryHexOrOct :: Parser LispVal 
parseBinaryHexOrOct = do 
    char '#' 
    which_base <- oneOf "box" 
    spaces 
    case which_base of 
        'b' -> do 
            number_as_string <- many1 binary_digits
            return $ Number $ bin_to_dec number_as_string 
        'o' -> do 
            number_as_string <- many1 oct_digits 
            return $ Number $ oct_to_dec number_as_string 
        'x' -> do 
            number_as_string <- many1 hex_digits 
            return $ Number $ hex_to_dec number_as_string

parseFloat :: Parser LispVal 
parseFloat = do 
    left <- many1 digit 
    char '.' 
    right <- many1 digit 
    return $ Float $ get_float  (left ++ "." ++ right) 

get_float :: String -> Float 
get_float x = fst $ readFloat x !! 0 

bin_to_dec :: String -> Integer 
bin_to_dec [] = 0 

bin_to_dec (x:xs) = read [x] + 2 * bin_to_dec xs 

hex_to_dec :: String -> Integer 
hex_to_dec x = fst $ readHex x !! 0

oct_to_dec :: String -> Integer 
oct_to_dec x = fst $ readOct x !! 0
            
language_name :: String 
language_name = "lisp" 

scheme_parser :: Parser LispVal 
scheme_parser = parseAtom <|> 
                parseFloat <|> 
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
