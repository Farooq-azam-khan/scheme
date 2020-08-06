module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- for liftM
import Control.Monad.Except -- gives throwError and catchError to the Either monad
import Numeric
import Datatypes


readExpr :: String -> ThrowsError LispVal 
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> throwError $ Parser err 
    Right val -> return val


spaces :: Parser () 
spaces = skipMany1 space

symbol :: Parser Char 
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- =========== parser ===========
parseExpr :: Parser LispVal 
parseExpr = parseAtom 
        <|> try parseCharacter
        <|> parseString
        <|> try parseFloat 
        <|> try parseNumber 
        <|> try parseBool
        <|> parseQuoted
        <|> do 
            char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x


parseFloat :: Parser LispVal
parseFloat = do 
    x <- many1 digit 
    char '.' 
    y <- many1 digit 
    -- output of readFloat = [(123.123, "")] (use fst.head to access the list and the tuple's values)
    return $ Float $ (fst.head$readFloat (x++"."++y))

parseCharacter :: Parser LispVal 
parseCharacter = do 
    try $ string "#\\"
    value <- try (string "newline" <|> string "space") 
                <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> (value !! 0)

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
    return $ Atom atom 

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

-- Recursive Parser (adding lists, dotted lists, and quoted datums)
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces 

parseDottedList :: Parser LispVal 
parseDottedList = do 
    head <- endBy parseExpr spaces 
    tail <- char '.' >> spaces >> parseExpr 
    return $ DottedList head tail 

parseQuoted :: Parser LispVal 
parseQuoted = do 
    char '\'' 
    x <- parseExpr
    return $ List [Atom "quote", x]
