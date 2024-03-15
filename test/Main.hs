module Main where

import System.Environment (getArgs)
import System.Random
import Data.List (isInfixOf, unfoldr)

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
    0 -> runSuites ["main"]
    _ -> runSuites args

-- | Almost each testcase contains 3 fields:
--   name, input, expected output
data TestCase 
  = MainTest       String String  String   -- Tests the main application
  | RoundtripTest  String Expr             -- Tests the roundtrip property (the input is equivalent to the output)
  | LexTest        String String  [Token]  -- Tests the lexer
  | ParseTest      String [Token] Expr     -- Tests the parser
  | EvalTest       String Expr    String   -- Tests the evaluator
  | FailTest       String String  String   -- Tests for failures

-- | Each testresult contains 4 fields:
--   name, input, expected output, actual output
type TestResult = (String, String, String, String)

-- | For table-based testing. A row in the table contains:
-- * input to the program
-- * resulting token list from calling Lexer.lex on the input
-- * resulting expression from parsing the list of tokens with Parser.parse
-- * result after evaluating the parsed expression with Evaluator.eval
type TableEntry = (String, [Token], Expr, String)


runSuite :: String -> IO ()
runSuite "main"      = runMainTests
runSuite "prop"      = runPropTests
runSuite "property"  = runPropTests
runSuite "lex"       = runLexerTests
runSuite "lexer"     = runLexerTests
runSuite "parse"     = runParserTests
runSuite "parser"    = runParserTests
runSuite "eval"      = runEvaluatorTests
runSuite "evaluator" = runEvaluatorTests
runSuite s           = putStrLn $ red $ "Unknown testsuite: " ++ s

runSuites :: [String] -> IO ()
runSuites args = mapM_ runSuite args

runMainTests:: IO ()
runMainTests = do
  putStrLn "Testing application..."
  runPropTests
  runTableTests
  putStrLn "Testing application: done!"

runTableTests :: IO ()
runTableTests = do
  putStrLn "Table-based tests..."
  testHappyFlow
  testUnhappyFlow
  putStrLn "Table-based tests: done!"

runPropTests :: IO ()
runPropTests = do
  putStrLn "Property tests..."
  randomGenerator <- newStdGen
  let inputs    = generateRoundtripInputs randomGenerator 1000
  let names     = map (\n -> "Roundtrip #" ++ show n) [1::Int ..]
  let testcases = map (\(tc, input) -> RoundtripTest tc input) (zip names inputs)
  let results   = runTests testcases
  -- TODO: adapt showResults so it takes a boolean to only print FAIL ones
  showResults (==) results
  putStrLn "Property tests: done!"

testHappyFlow :: IO ()
testHappyFlow = do
  putStrLn "Happy flow..."
  let names     = map (\n -> "Happy #" ++ show n) [1::Int ..]
  let inouts    = map (\(input, _, _, expected) -> (input, expected)) happyTable
  let testcases = map (\(name, (input, expected)) -> MainTest name input expected) (zip names inouts)
  let results   = runTests testcases
  showResults (==) results
  putStrLn "Happy flow: done!"

testUnhappyFlow :: IO ()
testUnhappyFlow = do
  putStrLn "Unhappy flow..."
  let names      = map (\n -> "Unhappy #" ++ show n) [1::Int ..]
  let inouts     = map (\(input, _, _, expected) -> (input, expected)) unhappyTable
  let testcases  = map (\(name, (input, expected)) -> FailTest name input expected) (zip names inouts)
  let results    = runTests testcases
  showResults (\actual expected -> expected `isInfixOf` actual) results
  putStrLn "Unhappy flow: done!"

runLexerTests :: IO ()
runLexerTests = do
  putStrLn "Lexer tests..."
  let names      = map (\n -> "Lexer #" ++ show n) [1::Int ..]
  let inouts     = map (\(input, expected, _, _) -> (input, expected)) happyTable
  let testcases  = map (\(name, (input, expected)) -> LexTest name input expected) (zip names inouts)
  let results    = runTests testcases
  showResults (==) results 
  putStrLn "Lexer tests: done!"

runParserTests :: IO ()
runParserTests = do
  putStrLn "Parser tests..."
  let names      = map (\n -> "Parser #" ++ show n) [1::Int ..]
  let inouts     = map (\(_, input, expected, _) -> (input, expected)) happyTable
  let testcases  = map (\(name, (input, expected)) -> ParseTest name input expected) (zip names inouts)
  let results    = runTests testcases
  showResults (==) results
  putStrLn "Parser tests: done!"

runEvaluatorTests :: IO ()
runEvaluatorTests = do
  putStrLn "Evaluator tests..."
  let names      = map (\n -> "Evaluator #" ++ show n) [1::Int ..]
  let inouts     = map (\(_, _, input, expected) -> (input, expected)) happyTable
  let testcases  = map (\(name, (input, expected)) -> EvalTest name input expected) (zip names inouts)
  let results    = runTests testcases
  showResults (==) results
  putStrLn "Evaluator tests: done!"


-- | Generate a random literal uniformly.
generateLiteral :: RandomGen g => g -> Literal
generateLiteral gen = 
  let
    (generateLInt, gen') = uniform gen
  in
    if generateLInt
      then LInt   (fst (uniformR (0, 9001) gen'))
      else LFloat (fst (uniformR (0, 9000.1) gen'))

-- | Generate the random unary operator.
generateUnaryOp :: RandomGen g => g -> UnaryOp
generateUnaryOp _ = Neg

-- | Generate a random binary operator uniformly.
generateBinOp :: RandomGen g => g -> BinOp
generateBinOp gen = [Add, Sub, Mul, Div] !! fst (uniformR (0, 3) gen)

-- | Sum type for generating an expression.
data ExprTag
  = ETBinOp
  | ETUnaryOp
  | ETLiteral

-- | Generate a random expression according to following distribution:
--   80 % ELiteral
--   10 % EBinOp
--   10 % EUnaryOp
generateExpr :: RandomGen g => g -> Expr
generateExpr gen = 
  let
    (genFreq, gen') = split gen
    x               = frequency genFreq [(3, ETBinOp), (3, ETUnaryOp), (8, ETLiteral)]
    (gen1, gen'')   = split gen'
    (gen2, gen3)    = split gen''
  in
    case x of
      ETBinOp    -> EBinOp   (generateBinOp gen1)   (generateExpr gen2) (generateExpr gen3)
      ETUnaryOp  -> EUnaryOp (generateUnaryOp gen1) (generateExpr gen2)
      ETLiteral  -> ELit     (generateLiteral gen1)

frequency :: RandomGen g => g -> [(Int, a)] -> a
frequency gen lst =
  let
    randBound         = sum (map fst lst) - 1
    (randomNumber, _) = uniformR (0, randBound) gen
  in 
    go lst randomNumber
  where
    go ((freq, x) : rest) n = if n < freq then x else go rest (n - freq)
    go [] _                 = error "Empty list in frequency recursion - should not happen."

generateRoundtripInputs :: RandomGen g => g -> Int -> [Expr]
generateRoundtripInputs gen n = 
  unfoldr
    (\ (gen', i) -> if i == n
        then Nothing
        else let (genA, genB) = split gen'
             in Just (generateExpr genA, (genB, i + 1)))
    (gen, 0)


runTest :: TestCase -> TestResult
runTest tc =
  case tc of
    RoundtripTest name input          -> (name, show input, show input,    show (Parser.parse $ Lexer.lex $ prettyprint input))
    LexTest       name input expected -> (name, show input, show expected, show (Lexer.lex input))
    ParseTest     name input expected -> (name, show input, show expected, show (Parser.parse input))
    EvalTest      name input expected -> (name, show input, show expected, show (Evaluator.eval input))
    MainTest      name input expected -> (name, show input, show expected, show (compute input))
    -- This probably can only be done once I move to using Either String a everywhere.
    FailTest      name input expected -> error "not yet implemented"

runTests :: [TestCase] -> [TestResult]
runTests = map runTest

showResult :: (String -> String -> Bool) -> TestResult -> String
showResult isok (name, input, expected, actual) = 
  let
    base = ("Test: " ++ name ++ " (input='" ++ input ++ "')") 
  in
    if actual `isok` expected
      then green $ base ++ " -> OK"
      else red   $ base ++ "FAIL\n\texpected='" ++ expected ++ "'\n\tactual=" ++ actual ++ "'"

showResults :: (String -> String -> Bool) -> [TestResult] -> IO ()
showResults f results = mapM_ putStrLn (map (\r -> showResult f r) results)


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
  ]

-- | A way to test for bad flows
unhappyTable :: [TableEntry]
unhappyTable = [
  -- Parse Error: remaining tokens
  -- This error gets thrown when the parser thinks it's done, but there's still input to consume.
   ("1 1",     [TLit (LInt 1), TLit (LInt 1)],                     undefined, "remaining tokens")
  ,("1.1 1",   [TLit (LFloat 1.1), TLit (LInt 1)],                 undefined, "remaining tokens")
  ,(".1 .1",   [TLit (LFloat 0.1), TLit (LFloat 0.1)],             undefined, "remaining tokens")
  ,(". 1.1",   [TLit (LFloat 0.0), TLit (LFloat 1.1)],             undefined, "remaining tokens")
  ,(". 1",     [TLit (LFloat 0.0), TLit (LInt 1)],                 undefined, "remaining tokens")
  ,("- 1 1",   [TBinOp Sub, TLit (LInt 1), TLit (LInt 1)],         undefined, "remaining tokens")
  ,("- 1.1 1", [TBinOp Sub, TLit (LFloat 1.1), TLit (LInt 1)],     undefined, "remaining tokens")
  ,("- .1 .1", [TBinOp Sub, TLit (LFloat 0.1), TLit (LFloat 0.1)], undefined, "remaining tokens")
  ,("- . 1.1", [TBinOp Sub, TLit (LFloat 0.0), TLit (LFloat 1.1)], undefined, "remaining tokens")
  ,("- . 1",   [TBinOp Sub, TLit (LFloat 0.0), TLit (LInt 1)],     undefined, "remaining tokens")
  
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

