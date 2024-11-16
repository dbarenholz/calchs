module Calc.ParserSpec (spec) where

import qualified Calc.Parser as Parser
import Calc.Types

import Test.Hspec
import Data.Either (fromLeft)

spec :: Spec
spec = do
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
  describe "a full expression" $ it "[TParen L, TParen L, TLit (LInt 1), TBinOp Add, TParen L, TLit (LInt 1), TParen R, TParen R, TBinOp Sub, TParen L, TBinOp Sub, TLit (LInt 1), TParen R, TBinOp Mul, TLit (LInt 1), TParen R, TBinOp Div, TParen L, TLit (LInt 1), TBinOp Pow, TLit (LInt 1), TParen R]" $ Parser.parse [TParen L, TParen L, TLit (LInt 1), TBinOp Add, TParen L, TLit (LInt 1), TParen R, TParen R, TBinOp Sub, TParen L, TBinOp Sub, TLit (LInt 1), TParen R, TBinOp Mul, TLit (LInt 1), TParen R, TBinOp Div, TParen L, TLit (LInt 1), TBinOp Pow, TLit (LInt 1), TParen R] `shouldBe` Right (EBinOp Div (EBinOp Sub (EBinOp Add (ELit (LInt 1)) (ELit (LInt 1))) (EBinOp Mul (EUnaryOp Neg (ELit (LInt 1))) (ELit (LInt 1)))) (EBinOp Pow (ELit (LInt 1)) (ELit (LInt 1))))
  describe "expects failures" $ do
    describe "no tokens" $ it "[]" $ fromLeft "expected failure, but did not fail" (Parser.parse []) `shouldStartWith` "Error during parsing"
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
