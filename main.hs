module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- for liftM

main :: IO()
main = do 
    -- gets the first argument and ignores the rest
    (expr:_) <- getArgs 
    putStrLn (readExpr expr)

-- =========== datatypes ===========
data LispVal = Atom String 
            | List [ListVal] 
            | DottedList [ListVal] ListVal 
            | Number Integer 
            | String String 
            | Bool Bool

symbol :: Parser Char 
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No Match: " ++ show err 
    Right val -> "Found Value"

spaces :: Parser () 
spaces = skipMany1 space

-- =========== parser ===========
parseExpr :: Parser LispVal 
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber 

parseString :: Parser LispVal 
parseString = do 
    char '"'
    x <- many (noneOf '\"')
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