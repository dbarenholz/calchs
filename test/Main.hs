module Main where

import System.Environment (getArgs)
import System.Random (RandomGen, mkStdGen, split, uniform, uniformR)
import System.Random.Stateful (globalStdGen, uniformM)
import Data.List (isInfixOf, unfoldr)
import Data.Char (isDigit)

import Calc (compute)
import Calc.Types
import Calc.Utils
import qualified Calc.Lexer     as Lexer (lex)
import qualified Calc.Parser    as Parser (parse)
import qualified Calc.Evaluator as Evaluator (eval)

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runSuites ["all"]
    _ -> runSuites args


runSuite :: String -> IO ()
runSuite "all"       = runAllTests  -- the default
runSuite "property"  = runPropertyTests Nothing
runSuite "lexer"     = runLexerTests
runSuite "parser"    = runParserTests
runSuite "evaluator" = runEvaluatorTests
runSuite maybeSeed   = if take 5 maybeSeed == "seed-" && all isDigit (drop 5 maybeSeed)
  then let theSeed = read $ (filter isDigit maybeSeed)
       in  runPropertyTests (Just theSeed)
  else putStrLn $ red $ "Unknown testsuite: " ++ maybeSeed

runSuites :: [String] -> IO ()
runSuites args = mapM_ runSuite args

runAllTests:: IO ()
runAllTests = do
  putStrLn "Running all tests..."
  runPropertyTests Nothing
  runHappyTests
  runUnhappyTests
  runLexerTests
  runParserTests
  runEvaluatorTests


-- | Each testcase contains:
--   name, input, expected output
data TestCase
  = MainTest       String String  String   -- Tests the main application
  | RoundtripTest  String Expr             -- Tests the roundtrip property (the input is equivalent to the expected output)
  | LexTest        String String  [Token]  -- Tests the lexer
  | ParseTest      String [Token] Expr     -- Tests the parser
  | EvalTest       String Expr    String   -- Tests the evaluator
  | FailTest       String String  String   -- Tests for expected failures

-- | Sum type for showing results.
data ShowMe
  = ShowAll    -- show complete test output
  | ShowFail   -- show all failures and errors
  | ShowErr    -- show all errors
  | ShowNone   -- show nothing, only the summary

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

runHappyTests :: IO ()
runHappyTests = do
  putStrLn "Happy flow..."
  let names     = map (\n -> "Happy #" ++ show n) [1::Int ..]
  let inouts    = map (\(input, _, _, expected) -> (input, expected)) happyTable
  let testcases = map (\(name, (input, expected)) -> MainTest name input expected) (zip names inouts)
  let results   = runTests testcases
  showResults ShowFail (==) results

runUnhappyTests :: IO ()
runUnhappyTests = do
  putStrLn "Unhappy flow..."
  let names      = map (\n -> "Unhappy #" ++ show n) [1::Int ..]
  let inouts     = map (\(input, _, _, expected) -> (input, expected)) unhappyTable
  let testcases  = map (\(name, (input, expected)) -> FailTest name input expected) (zip names inouts)
  let results    = runTests testcases
  showResults ShowFail (\actual expected -> expected `isInfixOf` actual) results

runLexerTests :: IO ()
runLexerTests = do
  putStrLn "Lexer tests..."
  let names      = map (\n -> "Lexer #" ++ show n) [1::Int ..]
  let inouts     = map (\(input, expected, _, _) -> (input, expected)) happyTable
  let testcases  = map (\(name, (input, expected)) -> LexTest name input expected) (zip names inouts)
  let results    = runTests testcases
  showResults ShowFail (==) results

runParserTests :: IO ()
runParserTests = do
  putStrLn "Parser tests..."
  let names      = map (\n -> "Parser #" ++ show n) [1::Int ..]
  let inouts     = map (\(_, input, expected, _) -> (input, expected)) happyTable
  let testcases  = map (\(name, (input, expected)) -> ParseTest name input expected) (zip names inouts)
  let results    = runTests testcases
  showResults ShowFail (==) results

runEvaluatorTests :: IO ()
runEvaluatorTests = do
  putStrLn "Evaluator tests..."
  let names      = map (\n -> "Evaluator #" ++ show n) [1::Int ..]
  let inouts     = map (\(_, _, input, expected) -> (input, expected)) happyTable
  let testcases  = map (\(name, (input, expected)) -> EvalTest name input expected) (zip names inouts)
  let results    = runTests testcases
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


-- | Each testresult contains 4 fields:
--   name, input, expected output, either an error message or actual output
type TestResult = (String, String, String, Either String String)

runTest :: TestCase -> TestResult
runTest tc = case tc of
  RoundtripTest name input ->
    let maybeTokens = Lexer.lex (prettyprint input)
    in  case maybeTokens of
      Left errMessage -> (name, show input, show input, Left errMessage)
      Right tokens    ->
          let maybeExpr = Parser.parse tokens
          in  case maybeExpr of
            Left errMessage -> (name, show input, show input, Left errMessage)
            Right expr      -> (name, show input, show input, Right (show expr))
  LexTest       name input expected -> (name, show input, show expected, fmap show (Lexer.lex input))
  ParseTest     name input expected -> (name, show input, show expected, fmap show (Parser.parse input))
  EvalTest      name input expected -> (name, show input, show expected, Right (show (Evaluator.eval input)))
  -- A 'MainTest' tests the happy flow. We do not expect an error here.
  MainTest      name input expected -> (name, show input, show expected, fmap show (compute input))
  -- A 'FailTest' tests the unhappy flow. An error message is GOOD here.
  FailTest      name input expected -> case compute input of
    Left desiredErrMessage ->
      (name, show input, expected, Right (desiredErrMessage))
    Right unwantedResult ->
      (name, show input, show expected, Left $ "Test '" ++ name ++ "' with input " ++ show input ++ " gave a result (" ++ unwantedResult ++ ") when in stead we expected an error: " ++ expected)

runTests :: [TestCase] -> [TestResult]
runTests = map runTest

data ShowResult
  = Error String
  | Ok String
  | Fail String

showTheShowResult :: ShowResult -> String
showTheShowResult (Error err)  = red   $ err
showTheShowResult (Ok ok)      = green $ ok
showTheShowResult (Fail fail') = red   $ fail'

showResult :: (String -> String -> Bool) -> TestResult -> ShowResult
showResult f_ok (name, input, expected, result) =
  let base = "Test: " ++ name ++ " (input=" ++ input ++ ")"
  in case result of
    Left errMessage -> Error $ base ++ "\n\t" ++ errMessage
    Right actual    -> if actual `f_ok` expected
      then Ok   $ green $ base
      else Fail $ base ++ "\n\texpected='" ++ expected ++ "'\n\tactual='" ++ actual ++ "'"


partitionShowResults :: [ShowResult] -> ([String], [String], [String])
partitionShowResults (sr : lst) = let (oks, errs, fails) = partitionShowResults lst in case sr of
  Ok ok   -> ((green ok) : oks, errs,    fails  )
  Error e -> (oks,     (red e) : errs, fails    )
  Fail f  -> (oks,     errs,     (red f) : fails)
partitionShowResults [] = ([], [], [])

showResults :: ShowMe -> (String -> String -> Bool) -> [TestResult] -> IO ()
showResults toShow f testResults = do
  -- Compute some statistics about the testuite.
  let allResults                           = map (\r -> showResult f r) testResults
  let allCount                             = length allResults
  let (okResults, errResults, failResults) = partitionShowResults allResults
  let okCount                              = length okResults
  let errCount                             = length errResults
  let failCount                            = length failResults

  let summary = "OK: "     ++ show okCount   ++ "/" ++ show allCount
              ++ " FAIL: " ++ show failCount ++ "/" ++ show allCount
              ++ " ERR: "  ++ show errCount  ++ "/" ++ show allCount

  case toShow of
    ShowAll  -> mapM_ putStrLn (map showTheShowResult allResults)
    ShowFail -> mapM_ putStrLn (errResults ++ failResults)
    ShowErr  -> mapM_ putStrLn errResults
    ShowNone -> undefined

  putStrLn summary


-- | For table-based testing. A row in the table contains:
-- * input to the program
-- * resulting token list from calling Lexer.lex on the input
-- * resulting expression from parsing the list of tokens with Parser.parse
-- * result after evaluating the parsed expression with Evaluator.eval
type TableEntry = (String, [Token], Expr, String)

happyTable :: [TableEntry]
happyTable = [
   -- Literal
   ("1", [TLit (LInt 1)], ELit (LInt 1), "1")
  ,("1.1", [TLit (LFloat 1.1)], ELit (LFloat 1.1), "1.1")
  ,(".1", [TLit (LFloat 0.1)], ELit (LFloat 0.1), "0.1")
   -- Unary Operator
  ,("-1", [TBinOp Sub, TLit (LInt 1)], EUnaryOp (Neg) (ELit (LInt 1)), "-1")
  ,("-.1", [TBinOp Sub, TLit (LFloat 0.1)], EUnaryOp (Neg) (ELit (LFloat 0.1)), "-0.1")
   -- Binary Operator
  ,("1+1", [TLit (LInt 1), TBinOp Add, TLit (LInt 1)], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("1-1", [TLit (LInt 1), TBinOp Sub, TLit (LInt 1)], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("1/1", [TLit (LInt 1), TBinOp Div, TLit (LInt 1)], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1.0") -- no integer division
  ,("1*1", [TLit (LInt 1), TBinOp Mul, TLit (LInt 1)], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("1^1", [TLit (LInt 1), TBinOp Pow, TLit (LInt 1)], EBinOp (Pow) (ELit (LInt 1)) (ELit (LInt 1)), "1")
   -- Parens around literals
  ,("(1)", [TParen L, TLit (LInt 1), TParen R], ELit (LInt 1), "1")
  ,("(1.1)", [TParen L, TLit (LFloat 1.1), TParen R], ELit (LFloat 1.1), "1.1")
  ,("(.1)", [TParen L, TLit (LFloat 0.1), TParen R], ELit (LFloat 0.1), "0.1")
   -- Parens around unary operators
  ,("(-1)", [TParen L, TBinOp Sub, TLit (LInt 1), TParen R], EUnaryOp (Neg) (ELit (LInt 1)), "-1")
   -- Parens around binary operators
  ,("(1+1)", [TParen L, TLit (LInt 1), TBinOp Add, TLit (LInt 1), TParen R], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("(1-1)", [TParen L, TLit (LInt 1), TBinOp Sub, TLit (LInt 1), TParen R], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("(1/1)", [TParen L, TLit (LInt 1), TBinOp Div, TLit (LInt 1), TParen R], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1.0") -- no integer division
  ,("(1*1)", [TParen L, TLit (LInt 1), TBinOp Mul, TLit (LInt 1), TParen R], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
   -- Combining parens of literals with operators
  ,("(1)+(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Add, TParen L, TLit (LInt 1), TParen R], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("(1)-(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Sub, TParen L, TLit (LInt 1), TParen R], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("(1)/(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Div, TParen L, TLit (LInt 1), TParen R], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1.0") -- no integer division
  ,("(1)*(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Mul, TParen L, TLit (LInt 1), TParen R], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
   -- Whitespace: all above tests with arbitrary spaces and tabs added (all fields except input are identical)
  ,(" 1", [TLit (LInt 1)], ELit (LInt 1), "1")
  ,("1.1 ", [TLit (LFloat 1.1)], ELit (LFloat 1.1), "1.1")
  ,("   .1", [TLit (LFloat 0.1)], ELit (LFloat 0.1), "0.1")
  ,("-1 ", [TBinOp Sub, TLit (LInt 1)], EUnaryOp (Neg) (ELit (LInt 1)), "-1")
  ,("   -.1", [TBinOp Sub, TLit (LFloat 0.1)], EUnaryOp (Neg) (ELit (LFloat 0.1)), "-0.1")
  ,("1 + 1", [TLit (LInt 1), TBinOp Add, TLit (LInt 1)], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("1 -1", [TLit (LInt 1), TBinOp Sub, TLit (LInt 1)], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("1/ 1", [TLit (LInt 1), TBinOp Div, TLit (LInt 1)], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1.0") -- no integer division
  ,("1  *       1", [TLit (LInt 1), TBinOp Mul, TLit (LInt 1)], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("(  1)", [TParen L, TLit (LInt 1), TParen R], ELit (LInt 1), "1")
  ,("(1.1 )", [TParen L, TLit (LFloat 1.1), TParen R], ELit (LFloat 1.1), "1.1")
  ,(" ( .1)", [TParen L, TLit (LFloat 0.1), TParen R], ELit (LFloat 0.1), "0.1")
  ,(" ( - 1 ) ", [TParen L, TBinOp Sub, TLit (LInt 1), TParen R], EUnaryOp (Neg) (ELit (LInt 1)), "-1")
  ,("(1 +       1)", [TParen L, TLit (LInt 1), TBinOp Add, TLit (LInt 1), TParen R], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("(1-1         )", [TParen L, TLit (LInt 1), TBinOp Sub, TLit (LInt 1), TParen R], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("(  1       /1)", [TParen L, TLit (LInt 1), TBinOp Div, TLit (LInt 1), TParen R], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1.0") -- no integer division
  ,("(1  *1)", [TParen L, TLit (LInt 1), TBinOp Mul, TLit (LInt 1), TParen R], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("(1) +(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Add, TParen L, TLit (LInt 1), TParen R], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("(  1 )-(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Sub, TParen L, TLit (LInt 1), TParen R], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("   ( 1)/(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Div, TParen L, TLit (LInt 1), TParen R], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1.0") -- no integer division
  ,("(1)*( 1 ) ", [TParen L, TLit (LInt 1), TParen R, TBinOp Mul, TParen L, TLit (LInt 1), TParen R], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  -- smoke test: Order of operations: pow > mul+div > add+sub
  ,("1 + 2 ^ 2 * 2", [TLit (LInt 1), TBinOp Add, TLit (LInt 2), TBinOp Pow, TLit (LInt 2), TBinOp Mul, TLit (LInt 2)], EBinOp (Add) (ELit (LInt 1)) (EBinOp (Mul) (EBinOp (Pow) (ELit (LInt 2))(ELit (LInt 2))) (ELit (LInt 2)) ), "9")
  ,("2 ^ 3 ^ 2", [TLit (LInt 2), TBinOp Pow, TLit (LInt 3), TBinOp Pow, TLit (LInt 2)], EBinOp (Pow) (ELit (LInt 2)) (EBinOp (Pow) (ELit (LInt 3)) (ELit (LInt 2))), "512")
  ]

-- | A way to test for bad flows
unhappyTable :: [TableEntry]
unhappyTable = [
  -- Lexer Error: unrecognised symbol
   ("kaas", undefined, undefined, "unrecognised symbol")
  -- Parse Error: remaining tokens
  -- This error gets thrown when the parser thinks it's done, but there's still input to consume.
  ,("1 1",     [TLit (LInt 1), TLit (LInt 1)],                                           undefined, "remaining tokens")
  ,("1.1 1",   [TLit (LFloat 1.1), TLit (LInt 1)],                                       undefined, "remaining tokens")
  ,(".1 .1",   [TLit (LFloat 0.1), TLit (LFloat 0.1)],                                   undefined, "remaining tokens")
  ,(". 1.1",   [TLit (LFloat 0.0), TLit (LFloat 1.1)],                                   undefined, "remaining tokens")
  ,(". 1",     [TLit (LFloat 0.0), TLit (LInt 1)],                                       undefined, "remaining tokens")
  ,("- 1 1",   [TBinOp Sub, TLit (LInt 1), TLit (LInt 1)],                               undefined, "remaining tokens")
  ,("- 1.1 1", [TBinOp Sub, TLit (LFloat 1.1), TLit (LInt 1)],                           undefined, "remaining tokens")
  ,("- .1 .1", [TBinOp Sub, TLit (LFloat 0.1), TLit (LFloat 0.1)],                       undefined, "remaining tokens")
  ,("- . 1.1", [TBinOp Sub, TLit (LFloat 0.0), TLit (LFloat 1.1)],                       undefined, "remaining tokens")
  ,("- . 1",   [TBinOp Sub, TLit (LFloat 0.0), TLit (LInt 1)],                           undefined, "remaining tokens")
  ,("(1) 1",     [TParen L, TLit (LInt 1), TParen R, TLit (LInt 1)],                     undefined, "remaining tokens")
  ,("(1.1) 1",   [TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)],                 undefined, "remaining tokens")
  ,("(.1) .1",   [TParen L, TLit (LFloat 0.1), TParen R, TLit (LFloat 0.1)],             undefined, "remaining tokens")
  ,("(.) 1.1",   [TParen L, TLit (LFloat 0.0), TParen R, TLit (LFloat 1.1)],             undefined, "remaining tokens")
  ,("(.) 1",     [TParen L, TLit (LFloat 0.0), TParen R, TLit (LInt 1)],                 undefined, "remaining tokens")
  ,("- (1) 1",   [TBinOp Sub, TParen L, TLit (LInt 1), TParen R, TLit (LInt 1)],         undefined, "remaining tokens")
  ,("- (1.1) 1", [TBinOp Sub, TParen L, TLit (LFloat 1.1), TParen R, TLit (LInt 1)],     undefined, "remaining tokens")
  ,("- (.1) .1", [TBinOp Sub, TParen L, TLit (LFloat 0.1), TParen R, TLit (LFloat 0.1)], undefined, "remaining tokens")
  ,("- (.) 1.1", [TBinOp Sub, TParen L, TLit (LFloat 0.0), TParen R, TLit (LFloat 1.1)], undefined, "remaining tokens")
  ,("- (.) 1",   [TBinOp Sub, TParen L, TLit (LFloat 0.0), TParen R, TLit (LInt 1)],     undefined, "remaining tokens")

  -- Parse Error: mismatched parenthesis
  -- This error gets thrown when the parser is parsing a (block), but _something_ goes wrong.
  ,("(1 1)",     [TParen L, TLit (LInt 1), TLit (LInt 1), TParen R],                     undefined, "mismatched parenthesis")
  ,("(1.1 1)",   [TParen L, TLit (LFloat 1.1), TLit (LInt 1), TParen R],                 undefined, "mismatched parenthesis")
  ,("(.1 .1)",   [TParen L, TLit (LFloat 0.1), TLit (LFloat 0.1), TParen R],             undefined, "mismatched parenthesis")
  ,("(. 1.1)",   [TParen L, TLit (LFloat 0.0), TLit (LFloat 1.1), TParen R],             undefined, "mismatched parenthesis")
  ,("(. 1)",     [TParen L, TLit (LFloat 0.0), TLit (LInt 1), TParen R],                 undefined, "mismatched parenthesis")
  ,("(- 1 1)",   [TParen L, TBinOp Sub, TLit (LInt 1), TLit (LInt 1), TParen R],         undefined, "mismatched parenthesis")
  ,("(- 1.1 1)", [TParen L, TBinOp Sub, TLit (LFloat 1.1), TLit (LInt 1), TParen R],     undefined, "mismatched parenthesis")
  ,("(- .1 .1)", [TParen L, TBinOp Sub, TLit (LFloat 0.1), TLit (LFloat 0.1), TParen R], undefined, "mismatched parenthesis")
  ,("(- . 1.1)", [TParen L, TBinOp Sub, TLit (LFloat 0.0), TLit (LFloat 1.1), TParen R], undefined, "mismatched parenthesis")
  ,("(- . 1)",   [TParen L, TBinOp Sub, TLit (LFloat 0.0), TLit (LInt 1), TParen R],     undefined, "mismatched parenthesis")

  -- Parse Error: unknown symbol or EOF
  -- This gets thrown when we're parsing, but the input unexpectedly stops.
  -- It also gets thrown when parsing a block, and something happens we don't expect.
  ,("",  [],         undefined, "EOF")
  ,(" ", [],         undefined, "EOF")
  ,("(", [TParen L], undefined, "EOF")
  ,(")", [TParen R], undefined, "unknown symbol")
  ]

