module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- for liftM

main :: IO()
main = do 
    -- gets the first argument and ignores the rest
    (expr:_) <- getArgs 
    putStrLn $ readExpr expr

-- =========== datatypes ===========
data LispVal = Atom String 
            | List [LispVal] 
            | DottedList [LispVal] LispVal 
            | Number Integer 
            | String String 
            | Bool Bool
            deriving(Show)

symbol :: Parser Char 
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No Match: " ++ show err 
    Right val -> "Found Value: " ++ input

spaces :: Parser () 
spaces = skipMany1 space

-- =========== parser ===========
parseExpr :: Parser LispVal 
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber 

 
-- parseString = do 
--     char '"'
--     x <- many (noneOf "\"")
--     char '"'
--     return $ String x 

-- Parsing.exercises.2
-- TODO: make a test for it 
escapedChars :: Parser Char
escapedChars = do char '\\' 
                  x <- oneOf "\\\"nrt" 
                  return $ case x of 
                    '\\' -> x
                    '"'  -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
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

-- Parsing.exercises.1.1
-- parseNumber = do 
--     digs <- many1 digit 
--     return $ Number $ read digs

-- Parsing.exercises.1.2
-- parseNumber = (many1 digit) >>= \x -> return $ Number $ read x

