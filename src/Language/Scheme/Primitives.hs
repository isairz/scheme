{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
module Language.Scheme.Primitives
  ( ioPrimitives
  , primitives
  ) where

import Control.Monad.Except
import qualified Data.Array.IArray as IArray
import           Data.CaseInsensitive  ( CI )
import qualified Data.CaseInsensitive as CI
import Data.Char ( toUpper, toLower, isAlpha, isDigit
                 , isUpper, isLower, isSpace, ord, chr )
import System.IO

import Language.Scheme.Reader
import Language.Scheme.Types

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

numericBinop :: (LispNumber -> LispNumber -> LispNumber) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = traverse unpackNum params >>= pure . Number . foldl1 op

boolBinopCI :: CI.FoldCase a => (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinopCI unpacker op args = if length args /= 2
                                 then throwError $ NumArgs 2 args
                                 else do left <- unpacker $ head args
                                         right <- unpacker $ args !! 1
                                         pure $ Bool $ CI.foldCase left `op` CI.foldCase right

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     pure $ Bool $ left `op` right

numBoolBinop :: (LispNumber -> LispNumber -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

charBoolBinopCI :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charBoolBinopCI = boolBinopCI unpackChar

charBoolBinop :: (Char -> Char -> Bool) -> [LispVal] -> ThrowsError LispVal
charBoolBinop = boolBinop unpackChar

strBoolBinopCI :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinopCI = boolBinopCI unpackStr

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError LispNumber
unpackNum = \case
  (Number n) -> pure n
  (String n) -> let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else pure $ Integer $ fst $ head parsed
  (List [n]) -> unpackNum n
  notNum     -> throwError $ TypeMismatch "number" notNum

unpackChar :: LispVal -> ThrowsError Char
unpackChar = \case
  (Char c)  -> pure c
  notChar   -> throwError $ TypeMismatch "char" notChar

unpackStr :: LispVal -> ThrowsError String
unpackStr = \case
  (String s)  -> pure s
  (Number s)  -> pure $ show s
  (Bool s)    -> pure $ show s
  notString   -> throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool = \case
  (Bool b)  -> pure b
  notBool   -> throwError $ TypeMismatch "boolean" notBool

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                pure $ unpacked1 == unpacked2
                `catchError` const (pure False)

car :: [LispVal] -> ThrowsError LispVal
car = \case
  [List (x : xs)]         -> pure x
  [DottedList (x : xs) _] -> pure x
  [badArg]                -> throwError $ TypeMismatch "pair" badArg
  badArgList              -> throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr = \case
  [List (x : xs)]         -> pure $ List xs
  [DottedList (_ : xs) x] -> pure $ DottedList xs x
  [DottedList [xs] x]     -> pure x
  [badArg]                -> throwError $ TypeMismatch "pair" badArg
  badArgList              -> throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons = \case
  [x1, List []]             -> pure $ List [x1]
  [x, List xs]              -> pure $ List $ x : xs
  [x, DottedList xs xlast]  -> pure $ DottedList (x : xs) xlast
  [x1, x2]                  -> pure $ DottedList [x1] x2
  badArgList                -> throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal = \case
  [arg1, arg2] -> do
    primitiveEquals <- or <$> traverse (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    pure $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
  badArgList -> throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv = \case
  [arg1, arg2]  -> pure . Bool $ arg1 == arg2
  badArgList    -> throwError $ NumArgs 2 badArgList

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean = \case
  [Bool _]    -> pure . Bool $ True
  [_]         -> pure . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isPair :: [LispVal] -> ThrowsError LispVal
isPair = \case
  [List (x:y:_)]    -> pure . Bool $ True
  [DottedList _ _]  -> pure . Bool $ True
  [_]               -> pure . Bool $ False
  badArgList        -> throwError $ NumArgs 1 badArgList

isList :: [LispVal] -> ThrowsError LispVal
isList = \case
  [List _]    -> pure . Bool $ True
  [_]         -> pure . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol = \case
  [Atom _]   -> pure . Bool $ True
  [_]        -> pure . Bool $ False
  badArgList -> throwError $ NumArgs 1 badArgList

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString = \case
  [Atom s]    -> pure . String $ s
  [badArg]    -> throwError $ TypeMismatch "symbol" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol = \case
  [String s]  -> pure . Atom $ s
  [badArg]    -> throwError $ TypeMismatch "string" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber = \case
  [Number _]  -> pure . Bool $ True
  [_]         -> pure . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isChar :: [LispVal] -> ThrowsError LispVal
isChar = \case
  [Char _]    -> pure . Bool $ True
  [_]         -> pure . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isString :: [LispVal] -> ThrowsError LispVal
isString = \case
  [String _]  -> pure . Bool $ True
  [_]         -> pure . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

mustInteger :: LispNumber -> ThrowsError Integer
mustInteger (Integer a) = pure a
mustInteger a = throwError $ TypeMismatch "integer" (Number a)

substring :: [LispVal] -> ThrowsError LispVal
substring = \case
  [String s, Number start, Number end] -> do
    start' <- fromIntegral <$> mustInteger start
    end' <- fromIntegral <$> mustInteger end

    unless (inRange s start') $ throwError $ OutOfRange (0, length s) start'
    unless (inRange s end')   $ throwError $ OutOfRange (0, length s) start'
    when (start' > end')      $ throwError $ OutOfRange (start', length s) end'
    pure . String $ substring' start' end' s
  [badArg, Number _, Number _] -> throwError $ TypeMismatch "string" badArg
  [_, badArg, Number _] -> throwError $ TypeMismatch "number" badArg
  [_, _, badArg] -> throwError $ TypeMismatch "number" badArg
  badArgList  -> throwError $ NumArgs 3 badArgList
  where
    inRange s index = index >=0 && index <= length s
    substring' start end = take (end - start) . drop start

isVector :: [LispVal] -> ThrowsError LispVal
isVector = \case
  [Vector _]  -> pure . Bool $ True
  [_]         -> pure . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

listToVector :: [LispVal] -> ThrowsError LispVal
listToVector = \case
  [List xs]   -> pure . Vector $ IArray.listArray (0, length xs - 1) xs
  [badArg]    -> throwError $ TypeMismatch "list" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

vectorToList :: [LispVal] -> ThrowsError LispVal
vectorToList = \case
  [Vector vs] -> pure . List $ IArray.elems vs
  [badArg]    -> throwError $ TypeMismatch "vector" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

isPort :: [LispVal] -> ThrowsError LispVal
isPort = \case
  [Port _]    -> pure . Bool $ True
  [_]         -> pure . Bool $ False
  badArgList  -> throwError $ NumArgs 1 badArgList

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure = \case
  [PrimitiveFunc _] -> pure . Bool $ True
  [IOFunc _]        -> pure . Bool $ True
  [Func {}]         -> pure . Bool $ True
  [_]               -> pure . Bool $ False
  badArgList        -> throwError $ NumArgs 1 badArgList

charIsAlphabetic :: [LispVal] -> ThrowsError LispVal
charIsAlphabetic = \case
  [Char c]    -> pure . Bool $ isAlpha c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charIsNumeric :: [LispVal] -> ThrowsError LispVal
charIsNumeric = \case
  [Char c]    -> pure . Bool $ isDigit c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charIsWhitespace :: [LispVal] -> ThrowsError LispVal
charIsWhitespace = \case
  [Char c]    -> pure . Bool $ isSpace c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charIsUpperCase :: [LispVal] -> ThrowsError LispVal
charIsUpperCase = \case
  [Char c]    -> pure . Bool $ isUpper c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charIsLowerCase :: [LispVal] -> ThrowsError LispVal
charIsLowerCase = \case
  [Char c]    -> pure . Bool $ isLower c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charToInteger :: [LispVal] -> ThrowsError LispVal
charToInteger = \case
  [Char c]    -> pure . Number . fromIntegral . ord $ c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

integerToChar :: [LispVal] -> ThrowsError LispVal
integerToChar = \case
  [Number c]  -> mustInteger c >>= \c -> pure . Char . chr . fromIntegral $ c
  [badArg]    -> throwError $ TypeMismatch "number" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charUpcase :: [LispVal] -> ThrowsError LispVal
charUpcase = \case
  [Char c]    -> pure . Char $ toUpper c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

charDowncase :: [LispVal] -> ThrowsError LispVal
charDowncase = \case
  [Char c]    -> pure . Char $ toLower c
  [badArg]    -> throwError $ TypeMismatch "char" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

strStrAppend :: [LispVal] -> ThrowsError LispVal
strStrAppend = \case
  []                          -> pure $ String ""
  [s@(String _)]              -> pure s
  (String s1:String s2:rest)  -> strStrAppend ((String $ s1 ++ s2):rest)
  (String _:badArg:_)         -> throwError $ TypeMismatch "string" badArg
  (badArg:_)                  -> throwError $ TypeMismatch "string" badArg

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength = \case
  [String s]  -> pure . Number $ fromIntegral . length $ s
  [badArg]    -> throwError $ TypeMismatch "string" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop (/)),
              -- FIXME: Modulo Operators
              -- ("modolo", numericBinop mod),
              -- ("quotient", numericBinop quot),
              -- ("remainder", numericBinop rem),
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
              ("string-ci=?", strBoolBinopCI (==)),
              ("string-ci<?", strBoolBinopCI (<)),
              ("string-ci>?", strBoolBinopCI (>)),
              ("string-ci<=?", strBoolBinopCI (<=)),
              ("string-ci>=?", strBoolBinopCI (>=)),
              ("string-append", strStrAppend),
              ("string-length", stringLength),
              ("char=?", charBoolBinop (==)),
              ("char<?", charBoolBinop (<)),
              ("char>?", charBoolBinop (>)),
              ("char<=?", charBoolBinop (<=)),
              ("char>=?", charBoolBinop (>=)),
              ("char-ci=?", charBoolBinopCI (==)),
              ("char-ci<?", charBoolBinopCI (<)),
              ("char-ci>?", charBoolBinopCI (>)),
              ("char-ci<=?", charBoolBinopCI (<=)),
              ("char-ci>=?", charBoolBinopCI (>=)),
              ("char-alphabetic?", charIsAlphabetic),
              ("char-numeric?", charIsNumeric),
              ("char-whitespace?", charIsWhitespace),
              ("char-upper-case?", charIsUpperCase),
              ("char-lower-case?", charIsLowerCase),
              ("char->integer", charToInteger),
              ("integer->char", integerToChar),
              ("char-upcase", charUpcase),
              ("char-downcase", charDowncase),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("boolean?", isBoolean),
              ("pair?", isPair),
              ("list?", isList),
              ("symbol?", isSymbol),
              ("symbol->string", symbolToString),
              ("string->symbol", stringToSymbol),
              ("number?", isNumber),
              ("char?", isChar),
              ("string?", isString),
              ("substring", substring),
              ("vector?", isVector),
              ("vector->list", vectorToList),
              ("list->vector", listToVector),
              ("port?", isPort),
              ("procedure?", isProcedure)]

-- FIXME: Add more IO primitives
ioPrimitives :: [(String, [LispVal] -> EvalM LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("newline", newline),
                ("display", display)]

makePort :: IOMode -> [LispVal] -> EvalM LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort mode [badArg] = throwError $ TypeMismatch "string" badArg
makePort _ badArgList  = throwError $ NumArgs 1 badArgList

closePort :: [LispVal] -> EvalM LispVal
closePort = \case
  [Port port] -> liftIO $ hClose port >> pure (Bool True)
  _           -> pure $ Bool False

readProc :: [LispVal] -> EvalM LispVal
readProc = \case
  []          -> readProc [Port stdin]
  [Port port] -> liftIO (hGetLine port) >>= liftThrows . fmap sexprToLispVal . readExpr
  [badArg]    -> throwError $ TypeMismatch "port" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

writeProc :: [LispVal] -> EvalM LispVal
writeProc = \case
  [obj]             -> writeProc [obj, Port stdout]
  [obj, Port port]  -> liftIO $ hPrint port obj >> pure (Bool True)
  [_, badArg]       -> throwError $ TypeMismatch "port" badArg
  badArgList        -> throwError $ NumArgs 1 badArgList -- FIXME: NumArgs 1 or 2

readContents :: [LispVal] -> EvalM LispVal
readContents = \case
  [String filename] -> fmap String $ liftIO $ readFile filename
  [badArg]          -> throwError $ TypeMismatch "string" badArg
  badArgList        -> throwError $ NumArgs 1 badArgList

newline :: [LispVal] -> EvalM LispVal
newline = \case
  []          -> newline [Port stdout]
  [Port port] -> liftIO $ hPutStr port "\n" >> pure Unspecified
  [badArg]    -> throwError $ TypeMismatch "port" badArg
  badArgList  -> throwError $ NumArgs 1 badArgList

display :: [LispVal] -> EvalM LispVal
display = \case
  [val]       -> liftIO $ putStr (show val) >> pure Unspecified
  badArgList  -> throwError $ NumArgs 1 badArgList

