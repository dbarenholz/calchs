module Main where

import System.Process (readProcess)
import System.Environment
import Data.Char (toLower)

import Types
import Lexer (lex)
import Parser (parse)
import Evaluator (eval)

main :: IO ()
main = test

-- | To reduce duplication between test cases, this table entry stores the following as tuple:
-- 0: input to the program
-- 1: resulting token list from calling Lexer.lex on the input
-- 2: resulting expression from parsing the list of tokens with Parser.parse
-- 3: result after evaluating the parsed expression with Evaluator.eval
type TableEntry = (String, [Token], Expr, String)
testTable :: [TableEntry]
testTable = [
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
  ,("1/1", [TLit (LInt 1), TBinOp Div, TLit (LInt 1)], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("1*1", [TLit (LInt 1), TBinOp Mul, TLit (LInt 1)], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
   -- Parens around literals
  ,("(1)", [TParen L, TLit (LInt 1), TParen R], ELit (LInt 1), "1")
  ,("(1.1)", [TParen L, TLit (LFloat 1.1), TParen R], ELit (LFloat 1.1), "1.1")
  ,("(.1)", [TParen L, TLit (LFloat 0.1), TParen R], ELit (LFloat 0.1), "0.1")
   -- Parens around unary operators
  ,("(-1)", [TParen L, TBinOp Sub, TLit (LInt 1), TParen R], EUnaryOp (Neg) (ELit (LInt 1)), "-1")
   -- THIS IS A BUG IN THE PROGRAM,("(-.1)", [TParen L, TBinOp Sub, TLit (LFloat 0.1), TParen R], EUnaryOp (Neg) (ELit (LFloat 0.1)), "-0.1")
   -- Parens around binary operators
  ,("(1+1)", [TParen L, TLit (LInt 1), TBinOp Add, TLit (LInt 1), TParen R], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("(1-1)", [TParen L, TLit (LInt 1), TBinOp Sub, TLit (LInt 1), TParen R], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("(1/1)", [TParen L, TLit (LInt 1), TBinOp Div, TLit (LInt 1), TParen R], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("(1*1)", [TParen L, TLit (LInt 1), TBinOp Mul, TLit (LInt 1), TParen R], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
   -- Combining parens of literals with operators
  ,("(1)+(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Add, TParen L, TLit (LInt 1), TParen R], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("(1)-(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Sub, TParen L, TLit (LInt 1), TParen R], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("(1)/(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Div, TParen L, TLit (LInt 1), TParen R], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("(1)*(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Mul, TParen L, TLit (LInt 1), TParen R], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
   -- Whitespace: all above tests with arbitrary spaces and tabs added (all fields except input are identical)
  ,(" 1", [TLit (LInt 1)], ELit (LInt 1), "1")
  ,("1.1 ", [TLit (LFloat 1.1)], ELit (LFloat 1.1), "1.1")
  ,("   .1", [TLit (LFloat 0.1)], ELit (LFloat 0.1), "0.1")
  ,("-1 ", [TBinOp Sub, TLit (LInt 1)], EUnaryOp (Neg) (ELit (LInt 1)), "-1")
  ,("   -.1", [TBinOp Sub, TLit (LFloat 0.1)], EUnaryOp (Neg) (ELit (LFloat 0.1)), "-0.1")
  ,("1 + 1", [TLit (LInt 1), TBinOp Add, TLit (LInt 1)], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("1 -1", [TLit (LInt 1), TBinOp Sub, TLit (LInt 1)], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("1/ 1", [TLit (LInt 1), TBinOp Div, TLit (LInt 1)], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("1  *       1", [TLit (LInt 1), TBinOp Mul, TLit (LInt 1)], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("(  1)", [TParen L, TLit (LInt 1), TParen R], ELit (LInt 1), "1")
  ,("(1.1 )", [TParen L, TLit (LFloat 1.1), TParen R], ELit (LFloat 1.1), "1.1")
  ,(" ( .1)", [TParen L, TLit (LFloat 0.1), TParen R], ELit (LFloat 0.1), "0.1")
  ,(" ( - 1 ) ", [TParen L, TBinOp Sub, TLit (LInt 1), TParen R], EUnaryOp (Neg) (ELit (LInt 1)), "-1")
  -- THIS IS A BUG IN THE PROGRAM ,(" ( - . 1 ) ", [TParen L, TBinOp Sub, TLit (LFloat 0.1), TParen R], EUnaryOp (Neg) (ELit (LFloat 0.1)), "-0.1")
  ,("(1 +       1)", [TParen L, TLit (LInt 1), TBinOp Add, TLit (LInt 1), TParen R], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("(1-1         )", [TParen L, TLit (LInt 1), TBinOp Sub, TLit (LInt 1), TParen R], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("(  1       /1)", [TParen L, TLit (LInt 1), TBinOp Div, TLit (LInt 1), TParen R], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("(1  *1)", [TParen L, TLit (LInt 1), TBinOp Mul, TLit (LInt 1), TParen R], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("(1) +(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Add, TParen L, TLit (LInt 1), TParen R], EBinOp (Add) (ELit (LInt 1)) (ELit (LInt 1)), "2")
  ,("(  1 )-(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Sub, TParen L, TLit (LInt 1), TParen R], EBinOp (Sub) (ELit (LInt 1)) (ELit (LInt 1)), "0")
  ,("   ( 1)/(1)", [TParen L, TLit (LInt 1), TParen R, TBinOp Div, TParen L, TLit (LInt 1), TParen R], EBinOp (Div) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ,("(1)*( 1 ) ", [TParen L, TLit (LInt 1), TParen R, TBinOp Mul, TParen L, TLit (LInt 1), TParen R], EBinOp (Mul) (ELit (LInt 1)) (ELit (LInt 1)), "1")
  ]

type LexTest   = (String,  [Token])
type ParseTest = ([Token], Expr   )
type EvalTest  = (Expr,    String )
type MainTest  = (String,  String )

asLexTest   :: TableEntry -> LexTest
asLexTest   (input, tokens,    _,   _) = ( input, tokens)
asParseTest :: TableEntry -> ParseTest
asParseTest (_,     tokens, expr,   _) = (tokens,   expr)
asEvalTest  :: TableEntry -> EvalTest
asEvalTest  (_,          _, expr, res) = (  expr,    res)
asMainTest  :: TableEntry -> MainTest
asMainTest  (input,      _,    _, res) = ( input,    res)

-- | A test failure is a tuple of (expected output, actual output) as string types.
type TestFailure = (String, String)


-- | A test result is either OK, or a FAILure with a particular TestFailure
data TestResult
  = OK
  | FAIL TestFailure
  deriving (Show)

-- | A test has a test number, the input, and the test result
type TestCase = (Int, String, TestResult)


-- | The testLexer function passes all testcases for the lexer to the lex function, and constructs TestResults from its outputs
testLexer :: [LexTest] -> [TestResult]
testLexer [] = [OK]
testLexer ( (input, expected) : ts) = 
  let
    actual = Lexer.lex input
    res = if expected == actual then OK else FAIL ("Expected: '" ++ show expected ++ "'", "Actual: '" ++ show actual ++ "'")
  in
    res : testLexer ts


-- | The testParser function passes all testcases for the parser to the parse function, and constructs TestResults from its outputs
testParser :: [ParseTest] -> [TestResult]
testParser [] = [OK]
testParser ( (input, expected) : ts) = 
  let
    actual = Parser.parse input
    res = if expected == actual then OK else FAIL ("Expected: '" ++ show expected ++ "'", "Actual: '" ++ show actual ++ "'")
  in
    res : testParser ts



-- | The testParser function passes all testcases for the parser to the parse function, and constructs TestResults from its outputs
testEvaluator :: [EvalTest] -> [TestResult]
testEvaluator [] = [OK]
testEvaluator ( (input, expected) : ts) = 
  let
    actual = Evaluator.eval input
    res = if expected == actual then OK else FAIL ("Expected: '" ++ show expected ++ "'", "Actual: '" ++ show actual ++ "'")
  in
    res : testEvaluator ts

-- | Run a single testcase for the entire application
singleTest :: MainTest -> IO TestResult
singleTest (input, expected) = do
  {- TODO: use binary of the built project as opposed to running cabal on everything
    -- get path of binary
    binPath <- readProcess "cabal" ["list-bin", "calchs"] []
    -- run the binary with the input
    actual <- readProcess binPath [input] []
    -- obtain the result from comparing actual with expected, and "return" (`pure`) it.
  -}
  actualWithNewline <- readProcess "cabal" ["run", "calchs", "--", input] []
  let actual = init actualWithNewline
  return $ if expected == actual then OK else FAIL ("Expected: '" ++ expected ++ "'", "Actual: '" ++ actual ++ "'")

-- | Run a testsuite to a list of results
testMain :: [MainTest] -> IO [TestResult]
testMain [] = return [OK]
testMain testcases = mapM singleTest testcases

-- | Helper method to print a particular string as red text to the terminal.
red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

-- | Helper method to print a particular string as green text to the terminal.
green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"

-- | Helper method to show a particular test case.
showTestCase :: TestCase -> String
showTestCase (num, inp, OK)                      = green ("Test #" ++ show num ++ ": '" ++ inp ++ "' -> OK")
showTestCase (num, inp, FAIL (expected, actual)) = red   ("Test #" ++ show num ++ ": '" ++ inp ++ "' -> FAIL\n\t" ++ expected ++ "\n\t" ++ actual)

-- | Helper method: runs the main testsuite
doSuiteMain :: IO ()
doSuiteMain = do
  putStrLn "Running testsuite: main"
  let mainTests = fmap asMainTest testTable
  mainResults <- testMain mainTests
  let mainTestCases = zipWith3 (,,) [1..] (map fst mainTests) mainResults
  mapM_ putStrLn (fmap showTestCase mainTestCases)

doSuiteLexer :: IO ()
doSuiteLexer = do
  putStrLn "Lexer Tests:"
  let lexTests = fmap asLexTest testTable
  let lexResults = testLexer lexTests
  let lexTestCases = fmap (\(num, (inp, _), res) -> (num, inp, res) :: TestCase) (zip3 [1..] lexTests lexResults)
  mapM_ putStrLn (fmap showTestCase lexTestCases)

doSuiteParser :: IO ()
doSuiteParser = do
  putStrLn "Parser Tests:"
  let parseTests     = fmap asParseTest testTable 
  let parseResults   = testParser parseTests
  let parseTestCases = fmap ( \(num, (inp, _), res) -> (num, show inp, res) :: TestCase) (zip3 [1..] parseTests parseResults)
  mapM_ putStrLn (fmap showTestCase parseTestCases)

doSuiteEvaluator :: IO ()
doSuiteEvaluator = do
  putStrLn "Evaluator Tests:"
  let evalTests     = fmap asEvalTest testTable 
  let evalResults   = testEvaluator evalTests
  let evalTestCases = fmap ( \(num, (inp, _), res) -> (num, show inp, res) :: TestCase) (zip3 [1..] evalTests evalResults)
  mapM_ putStrLn (fmap showTestCase evalTestCases)

testForArg :: String -> IO ()
testForArg arg = case arg of
  "lexer"     -> doSuiteLexer
  "parser"    -> doSuiteParser
  "evaluator" -> doSuiteEvaluator
  a           -> error ("Unknown argument: '" ++ a ++ "'")


test :: IO ()
test = do
  args <- getArgs
  case length args of
    0 -> doSuiteMain
    1 -> testForArg (map toLower (head args))
    _ -> error "TODO: Support multiple arguments"
