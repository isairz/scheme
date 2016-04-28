module Language.Scheme.Reader
  ( readExpr
  , readExprList
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import qualified Data.Array.IArray as IArray
import Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import Text.Megaparsec.String

import Language.Scheme.Types

sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void spaceChar) lineComment empty
  where lineComment  = L.skipLineComment ";"

symbol :: String -> Parser String
symbol = L.symbol sc

symbol' :: String -> Parser String
symbol' = L.symbol' sc

quotes :: Parser a -> Parser a
quotes = between (symbol "\"") (symbol "\"")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> pure val

readExpr :: String -> ThrowsError SExpr
readExpr = readOrThrow $ sc >> schemeExpr

readExprList :: String -> ThrowsError [SExpr]
readExprList = readOrThrow $ sc >> endBy schemeExpr sc

schemeSymbolChar :: Parser Char
schemeSymbolChar = oneOf "!$%&|*+-/:<=>?@^_~#"

schemeChar :: Parser SExpr
schemeChar = do
  _  <- try (string "#\\")
  c  <- anyChar
  cs <- many (letterChar <|> digitChar)
  let chracterName = c:cs
  case chracterName of
    "space"   -> pure $ SChar ' '
    "newline" -> pure $ SChar '\n'
    [ch] -> pure $ SChar ch
    _ -> fail $ "Unknown character name " ++ chracterName

schemeVector :: Parser SExpr
schemeVector = do
  _  <- char '#'
  exprs  <- parens $ endBy schemeExpr sc
  pure $ SVector $ IArray.listArray (0, length exprs - 1) exprs

schemeString :: Parser SExpr
schemeString = SString <$> quotes (many (noneOf "\""))

schemeAtom :: Parser SExpr
schemeAtom = do first <- letterChar <|> schemeSymbolChar
                rest <- many (letterChar <|> digitChar <|> schemeSymbolChar)
                let atom = first : rest
                pure $ case atom of
                  "#t" -> SBool True
                  "#f" -> SBool False
                  _    -> SAtom atom

-- FIXME: Recognize floating point numbers
schemeNumber :: Parser SExpr
schemeNumber = do sign <- optional $ oneOf "+-"
                  digits <- some digitChar
                  pure $ case sign of
                           Just '-' -> SNumber . Integer . negate . read $ digits
                           _        -> SNumber . Integer . read $ digits

schemeList :: Parser SExpr
schemeList = SList <$> endBy schemeExpr sc

schemeDottedList :: Parser SExpr
schemeDottedList = SDottedList <$> endBy schemeExpr sc <* symbol "." <*> schemeExpr

schemeQuoted :: Parser SExpr
schemeQuoted = do
    symbol "'"
    x <- schemeExpr
    pure $ SList [SAtom "quote", x]

schemeDottedListOrList :: Parser SExpr
schemeDottedListOrList = parens (try schemeDottedList <|> schemeList)

schemeExpr :: Parser SExpr
schemeExpr = choice [ try schemeNumber
                    , schemeChar
                    , try schemeVector
                    , schemeAtom
                    , schemeString
                    , schemeQuoted
                    , schemeDottedListOrList
                    ]

