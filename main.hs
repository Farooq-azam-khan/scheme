module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- for liftM
import Control.Monad.Except -- gives throwError and catchError to the Either monad
import Numeric


main :: IO()
main = do 
    args <- getArgs 
    putStrLn $ lispExecute (args !! 0)

lispExecute x = do 
    evaled <- return $ liftM show $ readExpr x >>= eval 
    extractValue $ trapError evaled

-- =========== datatypes ===========
data LispVal = Atom String 
            | List [LispVal] 
            | DottedList [LispVal] LispVal 
            | Number Integer 
            | String String 
            | Bool Bool
            | Character Char
            | Float Double 
            deriving(Show, Eq)

data LispError = NumArgs Integer [LispVal]
            | TypeMismatch String LispVal 
            | Parser ParseError 
            | BadSpecialForm String LispVal 
            | NotFunction String String
            | UnboundVar String String 
            | Default String 
            deriving(Show, Eq)


symbol :: Parser Char 
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal 
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> throwError $ Parser err 
    Right val -> return val

spaces :: Parser () 
spaces = skipMany1 space

-- =========== error handeling ===========
type ThrowsError = Either LispError

trapError action = catchError action (return .show)

extractValue :: ThrowsError a -> a 
extractValue (Right val) = val 


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

-- =========== eval ===========
eval :: LispVal -> ThrowsError LispVal
-- binds val to the whole LispVal, and not just the contents of the String constructor
eval val@(String _) = return val 
eval val@(Float _) = return val 
eval val@(Character _) = return val 
eval val@(Number _) = return val 
eval val@(Bool _) = return val 
eval val@(Atom _) = return val 
eval (List [Atom "quote", val]) = return val 
eval (List [Atom "if", pred, conseq, alt]) = do 
    result <- eval pred 
    case result of 
        Bool False -> eval alt 
        otherwise -> eval conseq 
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval val@(List _) = return val
eval val@(DottedList _ _) = return val 
-- eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives= [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("/", numericBinop div),
    ("*", numericBinop (*)),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("number?", unaryop isNumber),
    ("string?", unaryop isString),
    ("boolean?", unaryop isBoolean), 
    ("list?", unaryop isList),
    ("not", unaryop invert), 
    ("symbol?", unaryop isSymbol), 
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv)
    -- ("equal?", equal) -- TODO: implement this (eval pt 2)
    ]

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True 
isSymbol _ = Bool False 

isList :: LispVal -> LispVal
isList (List _) = Bool True
isList (DottedList _ _) = Bool True 
isList _ = Bool False 

isBoolean :: LispVal -> LispVal
isBoolean (Bool _) = Bool True 
isBoolean _ = Bool False 

invert :: LispVal -> LispVal
invert (Bool True) = Bool False 
invert (Bool False) = Bool True
invert _ = Bool False 

isString :: LispVal -> LispVal 
isString (Atom _) = Bool True 
isString (String _) = Bool True 
isString _ = Bool False 

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True 
isNumber _ = Bool False 

unaryop :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryop f [] = throwError $ NumArgs 1 [] 
unaryop f [v] = return $ f v 


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 [] 
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

binaryBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
binaryBinop op [] = throwError $ NumArgs 2 [] 
binaryBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
binaryBinop op params = mapM unpackBool params >>= return . Bool . foldl1 op 

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 then throwError $ NumArgs 2 args else do
    left <- unpacker $ args !! 0
    right <- unpacker $ args !! 1 
    return $ Bool $ left `op` right 

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackBool :: LispVal -> ThrowsError Bool 
unpackBool (Bool v) = return v 
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String 
unpackStr (String s) = return s 
unpackStr (Number s) = return $ show s 
unpackStr (Bool s) = return $ show s 
unpackStr notString = throwError $ TypeMismatch "string" notString 


-- =========== pattern matching lists ===========
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x 
car [DottedList (x:xs) _] = return x 
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs 
cdr [DottedList [_] x] = return x 
cdr [DottedList (_ : xs) x] = return $ DottedList xs x 
cdr [badArg] = throwError $ TypeMismatch "pair" badArg 
cdr badArgList = throwError $ NumArgs 1 badArgList 

cons :: [LispVal] -> ThrowsError LispVal 
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs 
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast 
cons [x1, x2] = return $ DottedList [x1] x2 
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2 
eqv [(Number arg1), (Number  arg2)] = return $ Bool $ arg1 == arg2 
eqv [(Float arg1), (Float arg2)] = return $ Bool $ arg1 == arg2 
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2 
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2 
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]] 
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                (all eqvPair $ zip arg1 arg2)
                            where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                        Left err -> False 
                                                        Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- eqvPair (x1, x2) = case eqv [x1, x2] of 
--                     Left err -> False 
--                     Right (Bool val) -> val





