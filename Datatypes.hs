module Datatypes where 
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except -- gives throwError and catchError to the Either monad

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

type ThrowsError = Either LispError

trapError action = catchError action (return .show)

extractValue :: ThrowsError a -> a 
extractValue (Right val) = val 