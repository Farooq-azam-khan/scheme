module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- for liftM
import Numeric

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
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No Match: " ++ show err 
    Right val -> "Found Value" -- : " ++ (show val)

spaces :: Parser () 
spaces = skipMany1 space

-- =========== parser ===========
parseExpr :: Parser LispVal 
parseExpr = parseAtom 
        <|> parseString
        <|> parseNumber 
        <|> parseBool

 
-- parseString = do 
--     char '"'
--     x <- many (noneOf "\"")
--     char '"'
--     return $ String x 

-- Parsing.exercises.2
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

parseBool :: Parser LispVal
parseBool = do
    char '#' 
    x <- oneOf "tf"
    return $ case x of 
        't' -> Bool True 
        'f' -> Bool False 

parseNumber :: Parser LispVal 
parseNumber = parseDecimal <|> parseDecimalExplicit <|> parseHex <|> parseBin <|> parseOct 

parseDecimal :: Parser LispVal 
parseDecimal = liftM (Number . read) $ many1 digit 

parseDecimalExplicit :: Parser LispVal
parseDecimalExplicit = do 
    try $ string "#d"
    x <- many1 digit 
    return $ Number $ read x 

parseHex :: Parser LispVal
parseHex = do 
    try $ string "#x"
    x <- many1 hexDigit 
    return $ Number $ hex2dig x 

parseBin :: Parser LispVal
parseBin = do 
    try $ string "#b"
    x <- many1 (oneOf "01")
    return $ Number $ bin2dig x 

parseOct :: Parser LispVal
parseOct = do 
    try $ string "#o"
    x <- many1 (octDigit)
    return $ Number $ oct2dig x 

hex2dig x = fst $ readHex "123" !! 0
oct2dig x = fst $ readOct "123" !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

-- Parsing.exercises.1.1
-- parseNumber = do 
--     digs <- many1 digit 
--     return $ Number $ read digs

-- Parsing.exercises.1.2
-- parseNumber = (many1 digit) >>= \x -> return $ Number $ read x

