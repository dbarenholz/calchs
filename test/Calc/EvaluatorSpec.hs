module Calc.EvaluatorSpec (spec) where

import qualified Calc.Evaluator as Evaluator
import Calc.Types

import Test.Hspec
import Calc.Utils (initOpts)


spec :: Spec
spec = do
  -- Set up testing for default options, different bases, and scientific notation
  -- Note: name shadowing of opts is intentional.
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

