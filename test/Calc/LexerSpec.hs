module Calc.LexerSpec (spec) where

import qualified Calc.Lexer as Lexer
import Calc.Utils (errMessage)
import Calc.Types

import Test.Hspec
import Data.Either (fromLeft)

spec :: Spec
spec = do
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
