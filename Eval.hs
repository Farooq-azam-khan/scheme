module Eval where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad -- for liftM
import Control.Monad.Except -- gives throwError and catchError to the Either monad
import Numeric

import Datatypes
import Parser 

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
    ("addFloat", floatBinop (+)),
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

floatBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
floatBinop op [] = throwError $ NumArgs 2 [] 
floatBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal 
floatBinop op params = mapM unpackFloat params >>= return . Float . foldl1 op 

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

unpackFloat :: LispVal -> ThrowsError Double 
unpackFloat (Float n) = return n
unpackFloat (List [n]) = unpackFloat n
unpackFloat notNum = throwError $ TypeMismatch "number" notNum


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





