module Main where

import qualified Data.Array.IArray as IArray
import Data.Either
import Data.Foldable
import Data.List
import System.Directory
import System.FilePath
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Language.Scheme
import Language.Scheme.Desugarer
import Language.Scheme.Pretty
import Language.Scheme.Reader

evalStringOne env = (fmap . fmap) last . (evalString env)

evalSpec :: Spec
evalSpec = do
  describe "char" $ do
    it "evaluates to itself" $ do
      env <- newEnv
      evalStringOne env "#\\space" `shouldReturn` (Right $ Char ' ')
      evalStringOne env "#\\newline" `shouldReturn` (Right $ Char '\n')
      evalStringOne env "#\\a" `shouldReturn` (Right $ Char 'a')
      evalStringOne env "#\\A" `shouldReturn` (Right $ Char 'A')
      evalStringOne env "#\\(" `shouldReturn` (Right $ Char '(')
      evalStringOne env "#\\1" `shouldReturn` (Right $ Char '1')

  describe "string" $ do
    it "evaluates to itself" $ do
      env <- newEnv
      evalStringOne env "\"foo\"" `shouldReturn` (Right $ String "foo")

  describe "number" $ do
    it "evaluates a positive integer number" $ do
      env <- newEnv
      evalStringOne env "123" `shouldReturn` (Right $ Number 123)

    it "evaluates a negative integer number" $ do
      env <- newEnv
      evalStringOne env "-123" `shouldReturn` (Right . Number . negate $ 123)

    it "evaluates a positive floating number" $ do
      env <- newEnv
      evalStringOne env "123.4" `shouldReturn` (Right $ Number 123.4)

    it "evaluates a negative floating number" $ do
      env <- newEnv
      evalStringOne env "-123.4" `shouldReturn` (Right . Number . negate $ 123.4)

  describe "atom" $ do
    it "evaluates to itself" $ do
      env <- newEnv
      evalStringOne env "'foo" `shouldReturn` (Right $ Atom "foo")

  describe "bool" $ do
    it "evaluates to itself" $ do
      env <- newEnv
      evalStringOne env "#t"  `shouldReturn` (Right $ Bool True)
      evalStringOne env "#f"  `shouldReturn` (Right $ Bool False)
      evalStringOne env "'#f" `shouldReturn` (Right $ Bool False)

  describe "vector" $ do
    it "evaluates to a vector" $ do
      env <- newEnv
      evalStringOne env "#()"  `shouldReturn` (Right $ Vector $ IArray.listArray (0, -1) [])
      evalStringOne env "#(1 'foo \"bar\")"  `shouldReturn`
        (Right $ Vector $ IArray.listArray (0, 2) [Number 1, List [Atom "quote", Atom "foo"], String "bar"])

  describe "begin" $ do
    it "evaluates begin expressions sequentially from left to right" $ do
      env <- newEnv
      evalStringOne env "(begin (define x 0) (set! x 5) (+ x 1))"
        `shouldReturn` (Right $ Number 6)

  describe "define" $ do
    it "defines a variable" $ do
      env <- newEnv
      evalStringOne env "(define x 28)" `shouldReturn` (Right $ Number 28)
      evalStringOne env "x" `shouldReturn` (Right $ Number 28)

  describe "number arithmetic procedures" $ do
    it "impose a total arithmetic on the set of numbers" $ do
      env <- newEnv
      evalStringOne env "(+ 3 4)" `shouldReturn` (Right $ Number 7)
      evalStringOne env "(* 3 4)" `shouldReturn` (Right $ Number 12)
      evalStringOne env "(- 3 4)" `shouldReturn` (Right $ Number $ -1)
      evalStringOne env "(/ 3 4)" `shouldReturn` (Right $ Number $ 3 / 4)
      evalStringOne env "(+ 1.2 3.4)" `shouldReturn` (Right $ Number 4.6)
      evalStringOne env "(* 1.2 3.4)" `shouldReturn` (Right $ Number 4.08)
      evalStringOne env "(- 1.2 3.4)" `shouldReturn` (Right $ Number $ -2.2)
      evalStringOne env "(/ 1.2 3.4)" `shouldReturn` (Right $ Number $ 1.2 / 3.4)
      -- FIXME: one or zero arguments
      -- evalStringOne env "(+)" `shouldReturn` (Right $ Number 0)
      -- evalStringOne env "(*)" `shouldReturn` (Right $ Number 1)
      -- evalStringOne env "(+ 3)" `shouldReturn` (Right $ Number 3)
      -- evalStringOne env "(* 3)" `shouldReturn` (Right $ Number 3)
      -- evalStringOne env "(- 3)" `shouldReturn` (Right $ Number $ -3)
      -- evalStringOne env "(/ 3)" `shouldReturn` (Right $ Number $ 1 / 4)

  describe "number comparison procedures" $ do
    it "impose a total oredering on the set of numbers" $ do
      env <- newEnv
      evalStringOne env "(= 1 1)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(= 1 1.0)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(= 1.0 1.0)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(= 1 2)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(= 1 2.0)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(= 1.0 2.0)" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(< 1 2)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(< 1 2.0)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(< 1.0 2.0)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(< 1 1)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(< 1 1.0)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(< 1.0 1.0)" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(> 2 1)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(> 2 1.0)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(> 2 1.0)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(> 1 1)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(> 1 1.0)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(> 1.0 1.0)" `shouldReturn` (Right $ Bool False)

  describe "type predicates" $ do
    it "return #t if the object is of the named type" $ do
      env <- newEnv
      evalStringOne env "(boolean? #f)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(boolean? 0)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(boolean? '())" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(pair? '(a . b))" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(pair? '(a b c))" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(pair? '())" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(list? '(a b c))" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(list? '())" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(list? '(a . b))" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(symbol? 'foo)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(symbol? (car '(a b)))" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(symbol? \"bar\")" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(symbol? 'nil)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(symbol? '())" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(symbol? #f)" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(number? 123)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(number? -123)" `shouldReturn` (Right $ Bool True)

      evalStringOne env "(char? #\\a)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char? #\\newline)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char? 'foo)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char? \"bar\")" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char? 'nil)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char? '())" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char? #f)" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(string? 'foo)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(string? \"bar\")" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(string? 'nil)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(string? '())" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(string? #f)" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(vector? '#())" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(vector? '#(1 'foo))" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(vector? 'foo)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(vector? \"bar\")" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(vector? '())" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(vector? #f)" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(procedure? car)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(procedure? 'car)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(procedure? (lambda (x) (* x x)))" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(procedure? '(lambda (x) (* x x)))" `shouldReturn` (Right $ Bool False)

  describe "char comparison procedures" $ do
    it "impose a total oredering on the set of characters" $ do
      env <- newEnv
      evalStringOne env "(char=? #\\a #\\a)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char=? #\\A #\\A)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char=? #\\1 #\\1)" `shouldReturn` (Right $ Bool True)

      evalStringOne env "(char<? #\\a #\\a)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char<? #\\a #\\b)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char<? #\\A #\\B)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char<? #\\1 #\\2)" `shouldReturn` (Right $ Bool True)

      evalStringOne env "(char>? #\\a #\\a)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char>? #\\a #\\b)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char>? #\\A #\\B)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char>? #\\1 #\\2)" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(char<=? #\\a #\\a)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char<=? #\\a #\\b)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char<=? #\\A #\\B)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char<=? #\\1 #\\2)" `shouldReturn` (Right $ Bool True)

      evalStringOne env "(char>=? #\\a #\\a)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char>=? #\\a #\\b)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char>=? #\\A #\\B)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char>=? #\\1 #\\2)" `shouldReturn` (Right $ Bool False)

  describe "char predicates" $ do
    it "return #t if their arguments are of the named characters" $ do
      env <- newEnv
      evalStringOne env "(char-alphabetic? #\\a)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char-alphabetic? #\\A)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char-alphabetic? #\\1)" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(char-numeric? #\\a)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char-numeric? #\\A)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char-numeric? #\\1)" `shouldReturn` (Right $ Bool True)

      evalStringOne env "(char-whitespace? #\\a)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char-whitespace? #\\A)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char-whitespace? #\\space)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char-whitespace? #\\newline)" `shouldReturn` (Right $ Bool True)

      evalStringOne env "(char-upper-case? #\\a)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char-upper-case? #\\A)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char-upper-case? #\\1)" `shouldReturn` (Right $ Bool False)

      evalStringOne env "(char-lower-case? #\\a)" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(char-lower-case? #\\A)" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(char-lower-case? #\\1)" `shouldReturn` (Right $ Bool False)

  describe "char-upcase/char-downcase" $ do
    it "return a character char2 such that (char-ci=? char char2)" $ do
      env <- newEnv
      evalStringOne env "(char-upcase #\\a)" `shouldReturn` (Right $ Char 'A')
      evalStringOne env "(char-upcase #\\A)" `shouldReturn` (Right $ Char 'A')
      evalStringOne env "(char-upcase #\\1)" `shouldReturn` (Right $ Char '1')
      evalStringOne env "(char-downcase #\\A)" `shouldReturn` (Right $ Char 'a')
      evalStringOne env "(char-downcase #\\a)" `shouldReturn` (Right $ Char 'a')
      evalStringOne env "(char-downcase #\\1)" `shouldReturn` (Right $ Char '1')

  describe "string comparison procedures" $ do
    it "impose a total oredering on the set of strings" $ do
      env <- newEnv
      evalStringOne env "(string=? \"foo\" \"foo\")" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(string<? \"bar\" \"foo\")" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(string>? \"bar\" \"foo\")" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(string<? \"foo\" \"foz\")" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(string>? \"foo\" \"foz\")" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(string<=? \"bar\" \"foo\")" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(string>=? \"bar\" \"foo\")" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(string<=? \"foo\" \"foz\")" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(string>=? \"foo\" \"foz\")" `shouldReturn` (Right $ Bool False)
      evalStringOne env "(string<=? \"foo\" \"foo\")" `shouldReturn` (Right $ Bool True)
      evalStringOne env "(string>=? \"foo\" \"foo\")" `shouldReturn` (Right $ Bool True)

  describe "string-append" $ do
    it "obeys right-identity" $ do
      env <- newEnv
      evalStringOne env "(string-append \"scheme\" \"\")" `shouldReturn` (Right $ String "scheme")

    it "obeys left-identity" $ do
      env <- newEnv
      evalStringOne env "(string-append \"\" \"lisp\")" `shouldReturn` (Right $ String "lisp")

    describe "given no arguments" $ do
      it "returns an empty string" $ do
        env <- newEnv
        evalStringOne env "(string-append)" `shouldReturn` (Right $ String "")

    describe "given one argument" $ do
      it "returns that argument" $ do
        env <- newEnv
        evalStringOne env "(string-append \"program\")" `shouldReturn` (Right $ String "program")

    describe "given two arguments" $ do
      it "returns a string with the first arg on the left and the second on the right" $ do
        env <- newEnv
        evalStringOne env "(string-append \"scheme-\" \"lisp\")" `shouldReturn` (Right $ String "scheme-lisp")

    describe "given more than two arguments" $ do
      it "returns a string of equal length to the sum of the args, with the passed values in order" $ do
        env <- newEnv
        evalStringOne env "(string-append \"a\" \".\" \"b\" \".\" \"c\")" `shouldReturn` (Right $ String "a.b.c")

  describe "string-length" $ do
    it "returns the number of characters in the given string" $ do
      env <- newEnv
      evalStringOne env "(string-length \"\")" `shouldReturn` (Right $ Number 0)
      evalStringOne env "(string-length \"foo\")" `shouldReturn` (Right $ Number 3)
      evalStringOne env "(string-length 'foo)" `shouldReturn` (Left $ TypeMismatch "string" (Atom "foo"))

  describe "list->vector/vector->list" $ do
    it "are the inverse of each other" $ do
      env <- newEnv
      evalStringOne env "(list->vector '(1 2 3))" `shouldReturn`
        (Right $ Vector $ IArray.listArray (0, 2) [Number 1, Number 2, Number 3])
      evalStringOne env "(vector->list #(1 2 3))" `shouldReturn`
        (Right $ List [Number 1, Number 2, Number 3])

  describe "symbol->string/string->symbol" $ do
    it "converts one type to the other" $ do
      env <- newEnv
      evalStringOne env "(symbol->string 'foo)" `shouldReturn` (Right $ String "foo")
      evalStringOne env "(string->symbol \"foo\")" `shouldReturn` (Right $ Atom "foo")

    it "are the inverse of each other" $ property $
      \s -> monadicIO $ do
        pre $ not $ null s
        env  <- run $ newEnv
        let p1 = SList [SAtom "symbol->string", SList [SAtom "quote", SAtom s]]
        (Right (String res1)) <- run $ evalLispVal env p1
        let p2 = SList [SAtom "string->symbol", SString res1]
        (Right (Atom res2)) <- run $ evalLispVal env p2
        assert $ res2 == s

  describe "char->integer/integer->char" $ do
    it "converts one type to the other" $ do
      env <- newEnv
      evalStringOne env "(char->integer #\\a)" `shouldReturn` (Right $ Number 97)
      evalStringOne env "(integer->char 97)" `shouldReturn` (Right $ Char 'a')

    it "are the inverse of each other" $ property $
      \c -> monadicIO $ do
        env  <- run $ newEnv
        let p1 = SList [SAtom "char->integer", SChar c]
        (Right (Number res1)) <- run $ evalLispVal env p1
        let p2 = SList [SAtom "integer->char", SNumber res1]
        (Right (Char res2)) <- run $ evalLispVal env p2
        assert $ res2 == c

desugarSpec :: Spec
desugarSpec =
  describe "desugarer" $ do
    let readAndDesugar str = readExpr str >>= desugar

    it "desugars let" $ do
      let infix 1 `shouldDesugaredInto`
          sugared `shouldDesugaredInto` desugared =
              (readAndDesugar sugared) `shouldBe` (readExpr desugared)

      "(let ((x 1)) x)" `shouldDesugaredInto` "((lambda (x) x) 1)"
      "(let ((x 1) (y 2)) (+ x y)))" `shouldDesugaredInto` "((lambda (x y) (+ x y)) 1 2)"
      "(let ((x 1)) (print x) (+ x 1)))"
          `shouldDesugaredInto` "((lambda (x) (print x) (+ x 1)) 1)"

    it "throws on malformed let bindings" $ do
      let shouldFailToDesugar str = readAndDesugar str `shouldSatisfy` isLeft
      shouldFailToDesugar "(let x x)"
      shouldFailToDesugar "(let (x) x)"
      shouldFailToDesugar "(let ((1 2)) 1)"
      shouldFailToDesugar "(let ((x)) x)"
      shouldFailToDesugar "(let ((x 1 2) x)"

    it "does not desugar" $ do
      let shouldNotDesugar sugared =
              (readAndDesugar sugared) `shouldBe` (readExpr sugared)
      shouldNotDesugar "(lambda (x y) (some x y))"
      shouldNotDesugar "(define x 28)"
      shouldNotDesugar "#t"
      shouldNotDesugar "()"

prettyPrintTest :: FilePath -> IO ()
prettyPrintTest path = do
  contents <- readFile path
  let Right exprs = readExprList contents
  let pp = prettyPrint exprs
  contents `shouldBe` pp

prettyPrintSpec :: Spec
prettyPrintSpec = do
  describe "prettyPrint" $ do
    let testSchemeDirectory = "test" </> "scheme"
    let isSchemeFile f = ".scm" `isSuffixOf` f
    let getAllSchemePaths path = map (path </>) . filter isSchemeFile <$> getDirectoryContents path

    it "is the inverse of readExprList" $ do
      allSchemes <- getAllSchemePaths testSchemeDirectory
      traverse_ prettyPrintTest allSchemes

main = hspec $ do
  evalSpec
  desugarSpec
  prettyPrintSpec
