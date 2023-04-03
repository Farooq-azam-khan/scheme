module Main where

import Text.ParserCombinators.Parsec hiding (spaces) 
import System.Environment 
import Control.Monad 
import Numeric 
import Control.Monad.Except 

-- ****** LISP VAL *******
data LispVal = Atom String 
                | List [LispVal]
                | DottedList [LispVal] LispVal 
                | Number Integer 
                | Float Float 
                | String String 
                | Character Char
                | Bool Bool 
                deriving (Show)

-- instance Show LispVal where show = showVal

showVal :: LispVal -> String 
showVal (String contents) = "\"" ++ contents ++ "\"" 
showVal (Atom name) = name 
showVal (Number contents) = show contents 
showVal (Float f) = show f 
showVal (Character ch) = show ch 
showVal (Bool True) = "#t" 
showVal (Bool False) = "#f" 
showVal (List contents) = "(" ++ unwordsList contents ++ ")" 
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String 
unwordsList = unwords . map showVal -- referred to as point-free style (i.e. without any arguments needs) 
-- unwordsList lst = unwords $ map showVal lst 

-- ****** LISP ERROR *******
data LispError = NumArgs Integer [LispVal] 
                | TypeMismatch String LispVal 
                | Parser ParseError 
                | BadSpecialForm String LispVal 
                | NotFunction String String 
                | UnboundVar String String 
                | Default String 
                deriving (Show)

type ThrowsError = Either LispError 
-- will curry LispVal since type variable can curry types 

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a 
extractValue (Right val) = val 

-- ****** EVAL *******
eval :: LispVal -> ThrowsError LispVal 
eval val@(String _) = return val 
eval val@(Float _) = return val 
eval val@(Number _) = return val 
eval val@(Bool _) = return val 
eval (List [Atom "quote", val]) = return val 
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized Special Form" badForm 

apply :: String -> [LispVal] -> ThrowsError LispVal 
apply func args = maybe 
            (throwError $ NotFunction "Unrecognized Primitive Function args" func) 
            ($ args) 
            (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolOp),
              ("string?", unaryOp stringOp),
              ("number?", unaryOp numberOp),
              ("symbol->string", unaryOp symbToStr),
              ("string->symbol", unaryOp strToSymb)
             ]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal 
unaryOp op [] = throwError $  NumArgs 1 [] 
unaryOp op [v] = return $ op v 

symbToStr :: LispVal -> LispVal 
symbToStr (Atom val) =  String val 
symbToStr _ = String "" -- throwError $ TypeMismatch "atom" invalid_arg

strToSymb :: LispVal -> LispVal
strToSymb (String val) = Atom val 
strToSymb _ = String "" -- throwError $ TypeMismatch "string" invalid_arg

symbolOp :: LispVal -> LispVal  
symbolOp (Atom _) = Bool True 
symbolOp _ = String "" -- throwError $ TypeMismatch "atom" invalid_arg

numberOp:: LispVal -> LispVal  
numberOp (Number _) = Bool True 
numberOp _ = String "" -- throwError $ TypeMismatch "number" invalid_arg

stringOp :: LispVal -> LispVal  
stringOp (String _) = Bool True 
stringOp _ = String "" --  throwError $ TypeMismatch "string" invalid_arg


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal 
numericBinop op [] = throwError $ NumArgs 2 []  
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params =  mapM unpackNum params  >>= return . Number . foldl1 op 

unpackNum :: LispVal -> ThrowsError Integer 
unpackNum (Number n) = return n 
unpackNum (List [n]) = unpackNum n 
unpackNum notNum = throwError $ TypeMismatch "number" notNum 

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

{- Case 1: #\a, #\A, #\1 -> Character 'a', Character 'A' Character '1'
 - Case 2: #\ -> Character ' ' 
 - Case 3: #\space, #\newline -> Character ' ', Character '\n'
 -} 

parseOneCharacterAsString :: Parser String 
parseOneCharacterAsString = do 
    x <- anyChar 
    return [x] 

parseCharacter :: Parser LispVal 
parseCharacter = do 
    string "#\\"
    sp_nl <- try (string "space" <|> string "newline") 
            <|> try parseOneCharacterAsString 
    return $ Character $ case sp_nl of 
                            "space" -> ' ' 
                            "newline" -> '\n' 
                            otherwise -> (sp_nl !!0)

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

parseList :: Parser LispVal 
parseList = liftM List $ sepBy scheme_parser spaces 
{-
 parseList = do 
    vals <- sepBy scheme_parser spaces 
    return $ List vals 
 -}

parseDottedList :: Parser LispVal 
parseDottedList = do 
    -- endBy : separated by spaces and last element ends in space 
    head <- endBy scheme_parser spaces 
    tail <- char '.' >> spaces >> scheme_parser
    return $ DottedList head tail 

parseQuoted :: Parser LispVal 
parseQuoted = do 
    char '\''
    x <- scheme_parser 
    return $ List [Atom "quote", x] 

language_name :: String 
language_name = "lisp" 


-- parseExpr
scheme_parser :: Parser LispVal 
scheme_parser = parseBool 
                <|> parseAtom 
                <|> do 
                    x <- try parseFloat <|> parseNumber 
                    return x 
                <|> parseQuoted
                <|> parseCharacter 
                <|> parseString 
                <|> do 
                    char '('
                    x <- try parseList <|> parseDottedList 
                    char ')' 
                    return x 

readExpr :: String -> ThrowsError LispVal 
readExpr input = case parse scheme_parser language_name input of 
                    Left err -> throwError $ Parser err -- $ "No Match: " ++ show err 
                    Right val -> return val 

spaces :: Parser ()
spaces = skipMany1 space -- could use: lexeme  

main :: IO ()
main = do 
    args <- getArgs 
    if length args < 1 then putStrLn "Provide args" else do 
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval 
        putStrLn $ extractValue $ trapError evaled 

    
{-    case length args > 1 of 
        True -> do
            (expr:_) <- args 
            putStrLn (readExpr expr) 
        False -> do 
            putStrLn "provide in args"-}
