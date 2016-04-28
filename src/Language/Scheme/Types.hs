{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Language.Scheme.Types
  ( Env
  , liftThrows
  , LispError(..)
  , LispNumber(..)
  , LispVal(..)
  , lispValToSExpr
  , IOThrowsError
  , SExpr(..)
  , sexprToLispVal
  , ThrowsError
  , EvalM(..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Array.IArray ( Array )
import qualified Data.Array.IArray as IArray
import Data.IORef
import System.IO
import Text.Megaparsec

type IOThrowsError = ExceptT LispError IO
type ThrowsError = Either LispError

-- FIXME:: Use Data.StRef instead of Data.IORef.
type Env = IORef [(String, IORef LispVal)]
newtype EvalM a = EvalM {
        run :: ReaderT Env (ExceptT LispError IO) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError LispError)

liftThrows :: ThrowsError a -> EvalM a
liftThrows (Left err) = EvalM $ lift $ throwError err
liftThrows (Right val) = EvalM $ lift $ pure val

data SExpr = SAtom String
           | SList [SExpr]
           | SDottedList [SExpr] SExpr
           | SNumber LispNumber
           | SChar Char
           | SString String
           | SVector (Array Int SExpr)
           | SBool Bool
           deriving (Eq)

instance Show SExpr where
  show (SChar c) = case c of
                       ' '       -> "#\\space"
                       '\n'      -> "#\\newline"
                       _         -> show c
  show (SString contents) = "\"" ++ contents ++ "\""
  show (SVector elements) = "#(" ++ unwordsList (IArray.elems elements) ++ ")"
  show (SAtom name) = name
  show (SNumber contents) = show contents
  show (SBool True) = "#t"
  show (SBool False) = "#f"
  show (SList contents) = "(" ++ unwordsList contents ++ ")"
  show (SDottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

data LispVal = Unspecified
             | Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNumber
             | Char Char
             | String String
             | Vector (Array Int LispVal)
             | Bool Bool
             | Port Handle
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> EvalM LispVal)
             | Func {params :: [String], vararg :: Maybe String,
                      body :: [SExpr], closure :: Env}

instance Eq LispVal where
  (Bool arg1) == (Bool arg2) = arg1 == arg2
  (Number arg1) == (Number arg2) = arg1 == arg2
  (Char arg1) == (Char arg2) = arg1 == arg2
  (String arg1) == (String arg2) = arg1 == arg2
  (Vector arg1) == (Vector arg2) = arg1 == arg2
  (Atom arg1) == (Atom arg2) = arg1 == arg2
  (DottedList xs x) == (DottedList ys y) = xs == ys && x == y
  (List arg1) == (List arg2) = arg1 == arg2
  _ == _ = False

instance Show LispVal where
  show Unspecified = "<unspecified>"
  show (Char c) = case c of
                       ' '       -> "#\\space"
                       '\n'      -> "#\\newline"
                       _         -> show c
  show (String contents) = "\"" ++ contents ++ "\""
  show (Vector elements) = "#(" ++ unwordsList (IArray.elems elements) ++ ")"
  show (Atom name) = name
  show (Number contents) = show contents
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (Port _) = "<IO port>"
  show (IOFunc _) = "<IO primitive>"
  show (PrimitiveFunc _) = "<primitive>"
  show Func {params = args, vararg = varargs, body = body, closure = env} =
    "(lambda (" ++ unwords (map show args) ++
       (case varargs of
          Nothing -> ""
          Just arg -> " . " ++ arg) ++ ") ...)"

sexprToLispVal :: SExpr -> LispVal
sexprToLispVal = \case
  SAtom s           -> Atom s
  SList xs          -> List $ map sexprToLispVal xs
  SDottedList xs x  -> DottedList (map sexprToLispVal xs) (sexprToLispVal x)
  SNumber n         -> Number n
  SChar c           -> Char c
  SString s         -> String s
  SVector v         -> Vector (fmap sexprToLispVal v)
  SBool b           -> Bool b

lispValToSExpr :: LispVal -> SExpr
lispValToSExpr = \case
  Atom s           -> SAtom s
  List xs          -> SList $ map lispValToSExpr xs
  DottedList xs x  -> SDottedList (map lispValToSExpr xs) (lispValToSExpr x)
  Number n         -> SNumber n
  Char c           -> SChar c
  String s         -> SString s
  Vector v         -> SVector (fmap lispValToSExpr v)
  Bool b           -> SBool b

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String SExpr
               | NotFunction String String
               | UnboundVar String String
               | OutOfRange (Int, Int) Int
               deriving (Eq)

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (OutOfRange range index) = "Value out of range " ++ show range ++ ": " ++ show index

instance Show LispError where show = showError

unwordsList :: Show a => [a] -> String
unwordsList = unwords . map show

data LispNumber = Integer Integer | Double Double

instance Num LispNumber where
  (+) = addNumber
  (*) = mulNumber
  negate = negateNumber
  abs = absNumber
  signum = signumNumber
  fromInteger = Integer

instance Fractional LispNumber where
  (/) = divNumber
  recip = divNumber 1
  fromRational = Double . fromRational

instance Real LispNumber where
  -- toRational

-- Double은 Enum타입이 아니니까 사용 할수  없음 => mod 구현은 어디다가?
-- instance Integral LispNumber where
--   toInteger = toIntegerNumber

instance RealFrac LispNumber where
  properFraction = properFractionNumber

instance Eq LispNumber where (==) a b = (compare a b) == EQ
instance Ord LispNumber where compare = compareNumber
instance Show LispNumber where show = showNumber

showNumber :: LispNumber -> String
showNumber (Integer a) = show a
showNumber (Double a) = show a

toIntegerNumber :: LispNumber -> Integer
toIntegerNumber (Integer a) = a
toIntegerNumber (Double a) = floor a

toDouble :: LispNumber -> Double
toDouble (Integer a) = fromInteger a
toDouble (Double a) = a

addNumber :: LispNumber -> LispNumber -> LispNumber
addNumber (Integer a) (Integer b) = Integer $ a + b
addNumber a b = Double $ (toDouble a) + (toDouble b)

mulNumber :: LispNumber -> LispNumber -> LispNumber
mulNumber (Integer a) (Integer b) = Integer $ a * b
mulNumber a b = Double $ (toDouble a) * (toDouble b)

divNumber :: LispNumber -> LispNumber -> LispNumber
divNumber a b = Double $ (toDouble a) / (toDouble b)

negateNumber :: LispNumber -> LispNumber
negateNumber (Integer a) = Integer $ negate a
negateNumber a = Double $ negate (toDouble a)

absNumber :: LispNumber -> LispNumber
absNumber (Integer a) = Integer $ abs a
absNumber a = Double $ abs (toDouble a)

signumNumber :: LispNumber -> LispNumber
signumNumber (Integer a) = Integer $ signum a
signumNumber a = Double $ signum $ toDouble a

properFractionNumber :: Integral b => LispNumber -> (b, LispNumber)
properFractionNumber (Integer a) = (fromInteger a, Integer 0)
properFractionNumber (Double a) = (fromInteger $ round a, Double $ a - (fromInteger $ round a))

compareNumber :: LispNumber -> LispNumber -> Ordering
compareNumber (Integer a) (Integer b) = compare a b
compareNumber a b = compare (toDouble a) (toDouble b)
