module CalcSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
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
