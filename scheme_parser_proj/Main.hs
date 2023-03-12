module Main where

import Text.ParserCombinators.Parsec hiding (spaces) 
import System.Environment 
import Control.Monad 

data LispVal = Atom String 
                | List [LispVal]
                | DottedList [LispVal] LispVal 
                | Number Integer 
                | BinaryNumber String 
                | OctNumber String 
                | HexNumber String 
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
parseNumber = liftM (Number . read) $ many1 digit 
{-parseNumber = do 
    opt <- many1 digit 
    return $ (Number . read) opt 
-}
-- parseNumber = many1 digit >>=  (return . Number . read )


-- e.g. 010010
parseBinaryNumber :: Parser LispVal 
parseBinaryNumber = do 
    string "#b " 
    number_as_string <- many1 (oneOf "01")
    return $ BinaryNumber number_as_string

parseOctNumber :: Parser LispVal 
parseOctNumber = do 
    string "#o " 
    number_as_string <- many1 (oneOf "01234567") 
    return $ OctNumber number_as_string

parseHexNumber :: Parser LispVal 
parseHexNumber = do 
    string "#x "
    number_as_string <- many1 (oneOf "0123456789abcdefABCDEF") 
    return $ HexNumber number_as_string

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
            return $ OctNumber number_as_string
        'x' -> do 
            number_as_string <- many1 (oneOf "0123456789abcdefABCDEF") 
            return $ HexNumber number_as_string

            
language_name :: String 
language_name = "lisp" 

scheme_parser :: Parser LispVal 
scheme_parser = parseBinaryHexOrOct <|> 
                parseNumber <|> 
                parseAtom <|> 
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
