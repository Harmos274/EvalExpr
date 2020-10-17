import Test.Tasty (TestTree, defaultMain, testGroup)

import LexerTest (lexerTests)
import ParserTest (parserTests)
import ComputeTest (computeTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "EvalExpr Tests" [lexerTests, parserTests, computeTests]