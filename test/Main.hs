module Main where
import Test.Hspec (hspec, describe, it, shouldBe, context, shouldSatisfy, shouldReturn, shouldStartWith)

import qualified Calc.Lexer     as Lexer (lex)
import qualified Calc.Parser    as Parser (parse)
import qualified Calc.Evaluator as Evaluator (eval)
import Calc.Utils ( initOpts, errMessage )
import Calc.Types
import Data.Either (fromLeft)

-- TODO: Test for underflows and overflows!
-- TODO: Figure out how to test interactive mode and options.
-- TODO: Figure out how to do my property test with QuickCheck-in-Hspec
-- TODO: Test the evaluator
-- TODO: Move stuff to their own spec files!

main :: IO ()
main = hspec $ do

  describe "lexer" $ do
    describe "operators" $ do
      it "*"      $ Lexer.lex "*"      `shouldBe` Right [TBinOp Mul]
      it "^"      $ Lexer.lex "^"      `shouldBe` Right [TBinOp Pow]
      it "-"      $ Lexer.lex "-"      `shouldBe` Right [TBinOp Sub]
      it "+"      $ Lexer.lex "+"      `shouldBe` Right [TBinOp Add]
      it "/"      $ Lexer.lex "/"      `shouldBe` Right [TBinOp Div]
    describe "parenthesis" $ do
      it "("      $ Lexer.lex "("      `shouldBe` Right [TParen L]
      it ")"      $ Lexer.lex ")"      `shouldBe` Right [TParen R]
    describe "literals" $ do
      it "1"      $ Lexer.lex "1"      `shouldBe` Right [TLit (LInt 1)]
      it "1.1"    $ Lexer.lex "1.1"    `shouldBe` Right [TLit (LFloat 1.1)]
      it "0.1"    $ Lexer.lex "0.1"    `shouldBe` Right [TLit (LFloat 0.1)]
      it ".1"     $ Lexer.lex ".1"     `shouldBe` Right [TLit (LFloat 0.1)]
    describe "literals with parens" $ do
      it "(1)"    $ Lexer.lex "(1)"    `shouldBe` Right [TParen L, TLit (LInt 1), TParen R]
      it "(1.1)"  $ Lexer.lex "(1.1)"  `shouldBe` Right [TParen L, TLit (LFloat 1.1), TParen R]
      it "(0.1)"  $ Lexer.lex "(0.1)"  `shouldBe` Right [TParen L, TLit (LFloat 0.1), TParen R]
      it "(.1)"   $ Lexer.lex "(.1)"   `shouldBe` Right [TParen L, TLit (LFloat 0.1), TParen R]
    describe "unary operators" $ do
      it "- 1"    $ Lexer.lex "- 1"    `shouldBe` Right [TBinOp Sub, TLit (LInt 1)]
      it "-1"     $ Lexer.lex "-1"     `shouldBe` Right [TBinOp Sub, TLit (LInt 1)]
      it "-1.1"   $ Lexer.lex "-1.1"   `shouldBe` Right [TBinOp Sub, TLit (LFloat 1.1)]
      it "-0.1"   $ Lexer.lex "-0.1"   `shouldBe` Right [TBinOp Sub, TLit (LFloat 0.1)]
      it "-.1"    $ Lexer.lex "-.1"    `shouldBe` Right [TBinOp Sub, TLit (LFloat 0.1)]
    describe "unary operators with parens" $ do
      it "(-)"    $ Lexer.lex "(-)"    `shouldBe` Right [TParen L, TBinOp Sub, TParen R]
      it "(- 1)"  $ Lexer.lex "(- 1)"  `shouldBe` Right [TParen L, TBinOp Sub, TLit (LInt 1), TParen R]
      it "(-1)"   $ Lexer.lex "(-1)"   `shouldBe` Right [TParen L, TBinOp Sub, TLit (LInt 1), TParen R]
      it "(-1.1)" $ Lexer.lex "(-1.1)" `shouldBe` Right [TParen L, TBinOp Sub, TLit (LFloat 1.1), TParen R]
      it "(-0.1)" $ Lexer.lex "(-0.1)" `shouldBe` Right [TParen L, TBinOp Sub, TLit (LFloat 0.1), TParen R]
      it "(-.1)"  $ Lexer.lex "(-.1)"  `shouldBe` Right [TParen L, TBinOp Sub, TLit (LFloat 0.1), TParen R]
    describe "binary operators" $ do
      it "1+1"    $ Lexer.lex "1+1"    `shouldBe` Right [TLit (LInt 1), TBinOp Add, TLit (LInt 1)]
      it "1-1"    $ Lexer.lex "1-1"    `shouldBe` Right [TLit (LInt 1), TBinOp Sub, TLit (LInt 1)]
      it "1/1"    $ Lexer.lex "1/1"    `shouldBe` Right [TLit (LInt 1), TBinOp Div, TLit (LInt 1)]
      it "1*1"    $ Lexer.lex "1*1"    `shouldBe` Right [TLit (LInt 1), TBinOp Mul, TLit (LInt 1)]
      it "1^1"    $ Lexer.lex "1^1"    `shouldBe` Right [TLit (LInt 1), TBinOp Pow, TLit (LInt 1)]
      it "1+.1"   $ Lexer.lex "1+.1"   `shouldBe` Right [TLit (LInt 1), TBinOp Add, TLit (LFloat 0.1)]
      it "1-.1"   $ Lexer.lex "1-.1"   `shouldBe` Right [TLit (LInt 1), TBinOp Sub, TLit (LFloat 0.1)]
      it "1/.1"   $ Lexer.lex "1/.1"   `shouldBe` Right [TLit (LInt 1), TBinOp Div, TLit (LFloat 0.1)]
      it "1*.1"   $ Lexer.lex "1*.1"   `shouldBe` Right [TLit (LInt 1), TBinOp Mul, TLit (LFloat 0.1)]
      it "1^.1"   $ Lexer.lex "1^.1"   `shouldBe` Right [TLit (LInt 1), TBinOp Pow, TLit (LFloat 0.1)]
    describe "binary operators with parens" $ do
      it "(1+1)"  $ Lexer.lex "(1+1)"  `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Add, TLit (LInt 1), TParen R]
      it "(1-1)"  $ Lexer.lex "(1-1)"  `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Sub, TLit (LInt 1), TParen R]
      it "(1/1)"  $ Lexer.lex "(1/1)"  `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Div, TLit (LInt 1), TParen R]
      it "(1*1)"  $ Lexer.lex "(1*1)"  `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Mul, TLit (LInt 1), TParen R]
      it "(1^1)"  $ Lexer.lex "(1^1)"  `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Pow, TLit (LInt 1), TParen R]
      it "(1+.1)" $ Lexer.lex "(1+.1)" `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Add, TLit (LFloat 0.1), TParen R]
      it "(1-.1)" $ Lexer.lex "(1-.1)" `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Sub, TLit (LFloat 0.1), TParen R]
      it "(1/.1)" $ Lexer.lex "(1/.1)" `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Div, TLit (LFloat 0.1), TParen R]
      it "(1*.1)" $ Lexer.lex "(1*.1)" `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Mul, TLit (LFloat 0.1), TParen R]
      it "(1^.1)" $ Lexer.lex "(1^.1)" `shouldBe` Right [TParen L, TLit (LInt 1), TBinOp Pow, TLit (LFloat 0.1), TParen R]
    describe "a full expression" $ it "((1 + (1)) - (-1) * 1) / (1^1)" $ Lexer.lex "((1 + (1)) - (-1) * 1) / (1^1)" `shouldBe` Right [TParen L, TParen L, TLit (LInt 1), TBinOp Add, TParen L, TLit (LInt 1), TParen R, TParen R, TBinOp Sub, TParen L, TBinOp Sub, TLit (LInt 1), TParen R, TBinOp Mul, TLit (LInt 1), TParen R, TBinOp Div, TParen L, TLit (LInt 1), TBinOp Pow, TLit (LInt 1), TParen R]
    describe "expects failures" $ do
      it "a"      $ fromLeft "expected failure, but did not fail" (Lexer.lex "a")      `shouldBe` show errMessage { during = "lexing", reason = "unknown symbol: 'a'"}
      it "A"      $ fromLeft "expected failure, but did not fail" (Lexer.lex "A")      `shouldBe` show errMessage { during = "lexing", reason = "unknown symbol: 'A'"}
      it "cheese" $ fromLeft "expected failure, but did not fail" (Lexer.lex "cheese") `shouldBe` show errMessage { during = "lexing", reason = "unknown symbol: 'c'"}

  describe "parser" $ do
    describe "positive literals" $ do
      it "[TLit (LInt 1)]"     $ Parser.parse [TLit (LInt 1)]       `shouldBe` Right (ELit (LInt 1))
      it "[TLit (LFloat 1.1)]" $ Parser.parse [TLit (LFloat 1.1)]   `shouldBe` Right (ELit (LFloat 1.1))
    describe "negative literals" $ do
      it "[TBinOp Sub, TLit (LInt 1)]"      $ Parser.parse [TBinOp Sub, TLit (LInt 1)]     `shouldBe` Right (EUnaryOp Neg (ELit (LInt 1)))
      it "[TBinOp Sub, TLit (LFloat 1.1)]"  $ Parser.parse [TBinOp Sub, TLit (LFloat 1.1)] `shouldBe` Right (EUnaryOp Neg (ELit (LFloat 1.1)))
    describe "positive literals with parens" $ do
      it "[TParen L, TLit (LInt 1), TParen R]"     $ Parser.parse [TParen L, TLit (LInt 1), TParen R]     `shouldBe` Right (ELit (LInt 1))
      it "[TParen L, TLit (LFloat 1.1), TParen R]" $ Parser.parse [TParen L, TLit (LFloat 1.1), TParen R] `shouldBe` Right (ELit (LFloat 1.1))
    describe "negative literals with parens" $ do
      it "[TParen L, TBinOp Sub, TLit (LInt 1), TParen R]"     $ Parser.parse [TParen L, TBinOp Sub, TLit (LInt 1), TParen R]     `shouldBe` Right (EUnaryOp Neg (ELit (LInt 1)))
      it "[TParen L, TBinOp Sub, TLit (LFloat 1.1), TParen R]" $ Parser.parse [TParen L, TBinOp Sub, TLit (LFloat 1.1), TParen R] `shouldBe` Right (EUnaryOp Neg (ELit (LFloat 1.1)))
      it "[TBinOp Sub, TParen L, TLit (LInt 1), TParen R]"     $ Parser.parse [TBinOp Sub, TParen L, TLit (LInt 1), TParen R]     `shouldBe` Right (EUnaryOp Neg (ELit (LInt 1)))
      it "[TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R]" $ Parser.parse [TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R] `shouldBe` Right (EUnaryOp Neg (ELit (LFloat 1.1)))
    describe "binary operators" $ do
      it "[TLit (LInt 1), TBinOp Add, TLit (LInt 1)]"     $ Parser.parse [TLit (LInt 1), TBinOp Add, TLit (LInt 1)]      `shouldBe` Right (EBinOp Add (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TLit (LInt 1), TBinOp Sub, TLit (LInt 1)]"     $ Parser.parse [TLit (LInt 1), TBinOp Sub, TLit (LInt 1)]      `shouldBe` Right (EBinOp Sub (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TLit (LInt 1), TBinOp Div, TLit (LInt 1)]"     $ Parser.parse [TLit (LInt 1), TBinOp Div, TLit (LInt 1)]      `shouldBe` Right (EBinOp Div (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TLit (LInt 1), TBinOp Mul, TLit (LInt 1)]"     $ Parser.parse [TLit (LInt 1), TBinOp Mul, TLit (LInt 1)]      `shouldBe` Right (EBinOp Mul (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TLit (LInt 1), TBinOp Pow, TLit (LInt 1)]"     $ Parser.parse [TLit (LInt 1), TBinOp Pow, TLit (LInt 1)]      `shouldBe` Right (EBinOp Pow (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TLit (LInt 1), TBinOp Add, TLit (LFloat 0.1)]" $ Parser.parse [TLit (LInt 1), TBinOp Add, TLit (LFloat 0.1)]  `shouldBe` Right (EBinOp Add (ELit (LInt 1)) (ELit (LFloat 0.1)))
      it "[TLit (LInt 1), TBinOp Sub, TLit (LFloat 0.1)]" $ Parser.parse [TLit (LInt 1), TBinOp Sub, TLit (LFloat 0.1)]  `shouldBe` Right (EBinOp Sub (ELit (LInt 1)) (ELit (LFloat 0.1)))
      it "[TLit (LInt 1), TBinOp Div, TLit (LFloat 0.1)]" $ Parser.parse [TLit (LInt 1), TBinOp Div, TLit (LFloat 0.1)]  `shouldBe` Right (EBinOp Div (ELit (LInt 1)) (ELit (LFloat 0.1)))
      it "[TLit (LInt 1), TBinOp Mul, TLit (LFloat 0.1)]" $ Parser.parse [TLit (LInt 1), TBinOp Mul, TLit (LFloat 0.1)]  `shouldBe` Right (EBinOp Mul (ELit (LInt 1)) (ELit (LFloat 0.1)))
      it "[TLit (LInt 1), TBinOp Pow, TLit (LFloat 0.1)]" $ Parser.parse [TLit (LInt 1), TBinOp Pow, TLit (LFloat 0.1)]  `shouldBe` Right (EBinOp Pow (ELit (LInt 1)) (ELit (LFloat 0.1)))
    describe "binary operators with parens" $ do
      it "[TParen L, TLit (LInt 1), TBinOp Add, TLit (LInt 1), TParen R]"     $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Add, TLit (LInt 1), TParen R]     `shouldBe` Right (EBinOp Add (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TParen L, TLit (LInt 1), TBinOp Sub, TLit (LInt 1), TParen R]"     $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Sub, TLit (LInt 1), TParen R]     `shouldBe` Right (EBinOp Sub (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TParen L, TLit (LInt 1), TBinOp Div, TLit (LInt 1), TParen R]"     $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Div, TLit (LInt 1), TParen R]     `shouldBe` Right (EBinOp Div (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TParen L, TLit (LInt 1), TBinOp Mul, TLit (LInt 1), TParen R]"     $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Mul, TLit (LInt 1), TParen R]     `shouldBe` Right (EBinOp Mul (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TParen L, TLit (LInt 1), TBinOp Pow, TLit (LInt 1), TParen R]"     $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Pow, TLit (LInt 1), TParen R]     `shouldBe` Right (EBinOp Pow (ELit (LInt 1)) (ELit (LInt 1)))
      it "[TParen L, TLit (LInt 1), TBinOp Add, TLit (LFloat 0.1), TParen R]" $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Add, TLit (LFloat 0.1), TParen R] `shouldBe` Right (EBinOp Add (ELit (LInt 1)) (ELit (LFloat 0.1)))
      it "[TParen L, TLit (LInt 1), TBinOp Sub, TLit (LFloat 0.1), TParen R]" $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Sub, TLit (LFloat 0.1), TParen R] `shouldBe` Right (EBinOp Sub (ELit (LInt 1)) (ELit (LFloat 0.1)))
      it "[TParen L, TLit (LInt 1), TBinOp Div, TLit (LFloat 0.1), TParen R]" $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Div, TLit (LFloat 0.1), TParen R] `shouldBe` Right (EBinOp Div (ELit (LInt 1)) (ELit (LFloat 0.1)))
      it "[TParen L, TLit (LInt 1), TBinOp Mul, TLit (LFloat 0.1), TParen R]" $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Mul, TLit (LFloat 0.1), TParen R] `shouldBe` Right (EBinOp Mul (ELit (LInt 1)) (ELit (LFloat 0.1)))
      it "[TParen L, TLit (LInt 1), TBinOp Pow, TLit (LFloat 0.1), TParen R]" $ Parser.parse [TParen L, TLit (LInt 1), TBinOp Pow, TLit (LFloat 0.1), TParen R] `shouldBe` Right (EBinOp Pow (ELit (LInt 1)) (ELit (LFloat 0.1)))
    describe "a full expression" $ do
      it "[TParen L, TParen L, TLit (LInt 1), TBinOp Add, TParen L, TLit (LInt 1), TParen R, TParen R, TBinOp Sub, TParen L, TBinOp Sub, TLit (LInt 1), TParen R, TBinOp Mul, TLit (LInt 1), TParen R, TBinOp Div, TParen L, TLit (LInt 1), TBinOp Pow, TLit (LInt 1), TParen R]" $ Parser.parse [TParen L, TParen L, TLit (LInt 1), TBinOp Add, TParen L, TLit (LInt 1), TParen R, TParen R, TBinOp Sub, TParen L, TBinOp Sub, TLit (LInt 1), TParen R, TBinOp Mul, TLit (LInt 1), TParen R, TBinOp Div, TParen L, TLit (LInt 1), TBinOp Pow, TLit (LInt 1), TParen R] `shouldBe` Right (EBinOp Div (EBinOp Sub (EBinOp Add (ELit (LInt 1)) (ELit (LInt 1))) (EBinOp Mul (EUnaryOp Neg (ELit (LInt 1))) (ELit (LInt 1)))) (EBinOp Pow (ELit (LInt 1)) (ELit (LInt 1))))
    describe "expects failures" $ do
      describe "no tokens" $ do
        it "[]" $ fromLeft "expected failure, but did not fail" (Parser.parse []) `shouldStartWith` "Error during parsing"
      describe "single operator" $ do
        it "[TBinOp Sub]"                    $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub])                    `shouldStartWith` "Error during parsing"
        it "[TBinOp Add]"                    $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Add])                    `shouldStartWith` "Error during parsing"
        it "[TBinOp Mul]"                    $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Mul])                    `shouldStartWith` "Error during parsing"
        it "[TBinOp Div]"                    $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Div])                    `shouldStartWith` "Error during parsing"
        it "[TBinOp Pow]"                    $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Pow])                    `shouldStartWith` "Error during parsing"
      describe "missing rhs" $ do
        it "[TLit (LInt 1), TBinOp Sub]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LInt 1), TBinOp Sub])     `shouldStartWith` "Error during parsing"
        it "[TLit (LInt 1), TBinOp Add]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LInt 1), TBinOp Add])     `shouldStartWith` "Error during parsing"
        it "[TLit (LFloat 1.1), TBinOp Add]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LFloat 1.1), TBinOp Add]) `shouldStartWith` "Error during parsing"
        it "[TLit (LInt 1), TBinOp Mul]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LInt 1), TBinOp Mul])     `shouldStartWith` "Error during parsing"
        it "[TLit (LFloat 1.1), TBinOp Mul]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LFloat 1.1), TBinOp Mul]) `shouldStartWith` "Error during parsing"
        it "[TLit (LInt 1), TBinOp Div]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LInt 1), TBinOp Div])     `shouldStartWith` "Error during parsing"
        it "[TLit (LFloat 1.1), TBinOp Div]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LFloat 1.1), TBinOp Div]) `shouldStartWith` "Error during parsing"
        it "[TLit (LInt 1), TBinOp Pow]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LInt 1), TBinOp Pow])     `shouldStartWith` "Error during parsing"
        it "[TLit (LFloat 1.1), TBinOp Pow]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LFloat 1.1), TBinOp Pow]) `shouldStartWith` "Error during parsing"
      describe "missing lhs" $ do
        it "[TBinOp Add, TLit (LInt 1)]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Add, TLit (LInt 1)])     `shouldStartWith` "Error during parsing"
        it "[TBinOp Add, TLit (LFloat 1.1)]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Add, TLit (LFloat 1.1)]) `shouldStartWith` "Error during parsing"
        it "[TBinOp Mul, TLit (LInt 1)]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Mul, TLit (LInt 1)])     `shouldStartWith` "Error during parsing"
        it "[TBinOp Mul, TLit (LFloat 1.1)]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Mul, TLit (LFloat 1.1)]) `shouldStartWith` "Error during parsing"
        it "[TBinOp Div, TLit (LInt 1)]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Div, TLit (LInt 1)])     `shouldStartWith` "Error during parsing"
        it "[TBinOp Div, TLit (LFloat 1.1)]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Div, TLit (LFloat 1.1)]) `shouldStartWith` "Error during parsing"
        it "[TBinOp Pow, TLit (LInt 1)]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Pow, TLit (LInt 1)])     `shouldStartWith` "Error during parsing"
        it "[TBinOp Pow, TLit (LFloat 1.1)]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Pow, TLit (LFloat 1.1)]) `shouldStartWith` "Error during parsing"
      describe "empty blocks" $ do
        it "[TParen L, TParen R]"                     $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L, TParen R])                     `shouldStartWith` "Error during parsing"
        it "[TParen L, TParen R, TParen L, TParen R]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L, TParen R, TParen L, TParen R]) `shouldStartWith` "Error during parsing"
        it "[TParen L, TParen L, TParen R, TParen R]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L, TParen L, TParen R, TParen R]) `shouldStartWith` "Error during parsing"
      describe "mismatched parens" $ do
        it "[TParen L]"                                           $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L])                                           `shouldStartWith` "Error during parsing"
        it "[TParen R]"                                           $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen R])                                           `shouldStartWith` "Error during parsing"
        it "[TParen L, TLit (LInt 1), TBinOp Add, TLit (LInt 1)]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L, TLit (LInt 1), TBinOp Add, TLit (LInt 1)]) `shouldStartWith` "Error during parsing"
      describe "numbers without operators" $ do
        it "[TLit (LInt 1), TLit (LInt 1)]"                     $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LInt 1), TLit (LInt 1)])                      `shouldStartWith` "Error during parsing"
        it "[TLit (LFloat 1.1), TLit (LInt 1)]"                 $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LFloat 1.1), TLit (LInt 1)])                  `shouldStartWith` "Error during parsing"
        it "[TLit (LFloat 1.1), TLit (LFloat 1.1)]"             $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LFloat 1.1), TLit (LFloat 1.1)])              `shouldStartWith` "Error during parsing"
        it "[TLit (LInt 1), TLit (LFloat 1.1)]"                 $ fromLeft "expected failure, but did not fail" (Parser.parse [TLit (LInt 1), TLit (LFloat 1.1)])                  `shouldStartWith` "Error during parsing"
      describe "too many operands" $ do
        it "[TBinOp Sub, TLit (LInt 1), TLit (LInt 1)]"         $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TLit (LInt 1), TLit (LInt 1)])          `shouldStartWith` "Error during parsing"
        it "[TBinOp Sub, TLit (LFloat 1.1), TLit (LInt 1)]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TLit (LFloat 1.1), TLit (LInt 1)])      `shouldStartWith` "Error during parsing"
        it "[TBinOp Sub, TLit (LFloat 0.1), TLit (LFloat 0.1)]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TLit (LFloat 0.1), TLit (LFloat 0.1)])  `shouldStartWith` "Error during parsing"
        it "[TBinOp Sub, TLit (LFloat 0.0), TLit (LFloat 1.1)]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TLit (LFloat 0.0), TLit (LFloat 1.1)])  `shouldStartWith` "Error during parsing"
        it "[TBinOp Sub, TLit (LFloat 0.0), TLit (LInt 1)]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TLit (LFloat 0.0), TLit (LInt 1)])      `shouldStartWith` "Error during parsing"
      describe "remaining tokens" $ do
        it "[TParen L, TLit (LInt 1), TParen R, TLit (LInt 1)]"                     $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L, TLit (LInt 1), TParen R, TLit (LInt 1)])                     `shouldStartWith` "Error during parsing"
        it "[TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)]"                 $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)])                 `shouldStartWith` "Error during parsing"
        it "[TParen L, TLit (LFloat 1.1), TParen R, TLit (LFloat 1.1)]"             $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L, TLit (LFloat 1.1), TParen R, TLit (LFloat 1.1)])             `shouldStartWith` "Error during parsing"
        it "[TParen L, TLit (LFloat 1.1), TParen R, TLit (LFloat 1.1)]"             $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L, TLit (LFloat 1.1), TParen R, TLit (LFloat 1.1)])             `shouldStartWith` "Error during parsing"
        it "[TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)]"                 $ fromLeft "expected failure, but did not fail" (Parser.parse [TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)])                 `shouldStartWith` "Error during parsing"
        it "[TBinOp Sub, TParen L, TLit (LInt 1), TParen R, TLit (LInt 1)]"         $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TParen L, TLit (LInt 1), TParen R, TLit (LInt 1)])         `shouldStartWith` "Error during parsing"
        it "[TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)])     `shouldStartWith` "Error during parsing"
        it "[TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R, TLit (LFloat 1.1)]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R, TLit (LFloat 1.1)]) `shouldStartWith` "Error during parsing"
        it "[TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R, TLit (LFloat 1.1)]" $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R, TLit (LFloat 1.1)]) `shouldStartWith` "Error during parsing"
        it "[TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)]"     $ fromLeft "expected failure, but did not fail" (Parser.parse [TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)])     `shouldStartWith` "Error during parsing"

  -- Set up testing for default options, different bases, and scientific notation
  -- Note: name shadowing of opts is intentional.
  describe "evaluator" $ do
    context "default options" $ do
      let opts = initOpts
      describe "scientific notation" $ do
        let opts = opts { numberFormat = Scientific }
        it "" True
      describe "expects failures" $ it "" True

    context "base 2 (binary)" $ do
      let opts = initOpts { numberMode = Binary }
      describe "scientific notation" $ do
        let opts = opts { numberFormat = Scientific }
        it "" True
      describe "expects failures" $ it "" True
    context "base 16 (hex)" $ do
      let opts = initOpts { numberMode = Hex }
      describe "scientific notation" $ do
        let opts = opts { numberFormat = Scientific }
        it "" True
      describe "expects failures" $ it "" True

  describe "Properties" $
    it "has no tests yet" True


-- Below is kept in case I need it to add property tests in hspec format
{-
runPropertyTests :: Maybe Int -> IO ()
runPropertyTests maybeSeed = do
  putStr "Property tests..."
  -- If a seed was passed, use that instead of a randomly generated one.
  seed <- case maybeSeed of
    Nothing   -> uniformM globalStdGen :: IO Int
    Just seed -> return seed
  putStrLn $ " (seed: " ++ show seed ++ ")"
  let randomGenerator = mkStdGen seed
  let inputs    = generateRoundtripInputs randomGenerator 1000
  let names     = map (\n -> "Roundtrip #" ++ show n) [1::Int ..]
  let testcases = map (\(tc, input) -> RoundtripTest tc input) (zip names inputs)
  let results   = runTests testcases
  showResults ShowFail (==) results

-- | Generate a random literal uniformly.
generateLiteral :: RandomGen g => g -> Literal
generateLiteral randomGenerator =
  let (generateLInt, randomGenerator') = uniform randomGenerator
  in  if generateLInt
    then LInt   (fst (uniformR (0, 9001)     randomGenerator'))
    -- Haskell, by default, writes floats smaller than 0.1 using scientific notation.
    else LFloat (fst (uniformR (0.1, 9000.1) randomGenerator'))

-- | Generate the random unary operator.
generateUnaryOp :: RandomGen g => g -> UnaryOp
generateUnaryOp _ = Neg

-- | Generate a random binary operator uniformly.
generateBinOp :: RandomGen g => g -> BinOp
generateBinOp randomGenerator = [Add, Sub, Mul, Div, Pow] !! fst (uniformR (0, 4) randomGenerator)

-- | Sum type for generating an expression.
data ExprTag = ETBinOp | ETUnaryOp | ETLiteral

-- | Generate a random expression according to following distribution:
--   8 out of 14 times: ELiteral
--   3 out of 14 times: EBinOp
--   3 out of 14 times: EBinOp
generateExpr :: RandomGen g => g -> Expr
generateExpr randomGenerator =
  let
    (genFreq, randomGenerator')            = split randomGenerator
    tagToGenerate                          = frequency genFreq [(3, ETBinOp), (3, ETUnaryOp), (8, ETLiteral)]
    (randomGenerator1, randomGenerator'')  = split randomGenerator'
    (randomGenerator2, randomGenerator3)   = split randomGenerator''
  in
    case tagToGenerate of
      ETBinOp    -> EBinOp   (generateBinOp randomGenerator1)   (generateExpr randomGenerator2) (generateExpr randomGenerator3)
      ETUnaryOp  -> EUnaryOp (generateUnaryOp randomGenerator1) (generateExpr randomGenerator2)
      ETLiteral  -> ELit     (generateLiteral randomGenerator1)

-- Function is impure due to `error`. But, this really is unreachable code, so it's OK.
frequency :: RandomGen g => g -> [(Int, a)] -> a
frequency randomGenerator lst =
  let
    generateMaxBound  = sum (map fst lst) - 1
    (randomNumber, _) = uniformR (0, generateMaxBound) randomGenerator
  in
    go lst randomNumber
  where
    go ((freq, x) : rest) n = if n < freq then x else go rest (n - freq)
    go [] _                 = error "Empty list in frequency recursion - should not happen."

generateRoundtripInputs :: RandomGen g => g -> Int -> [Expr]
generateRoundtripInputs randomGenerator amountToGenerate =
  unfoldr
    (\ (randomGenerator', currentCount) -> if currentCount == amountToGenerate
        then Nothing
        else let (randomGeneratorA, randomGeneratorB) = split randomGenerator'
             in  Just (generateExpr randomGeneratorA, (randomGeneratorB, currentCount + 1)))
    (randomGenerator, 0)
-}
