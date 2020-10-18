module ParserTest
  ( parserTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Control.Exception (evaluate)

import Compute (Operation(..), UnaryOperator(..), BinaryOperator(..))
import Parser (parser)
import qualified Lexer as L (Token (..))
import Exception (EEExceptions (ParserException))
import Assert (assertEEException)

newtype TestOperation = TestOperation Operation deriving (Eq, Show)

parserTests :: TestTree
parserTests = testGroup "Parser" [emptyList, singleValue, simplePlus, simpleMinus, simpleDiv,
                                  simpleMult, simplePow, unaryMinus, unaryPlus, parenthesis,
                                  priorityMult, priorityDiv, priorityPow, wrongParenthesis,
                                  invalidMember]

emptyList :: TestTree
emptyList = testCase "Empty list" $ assertEEException  (ParserException "No operation given") (evaluate $ parser [])

singleValue :: TestTree
singleValue = testCase "Single value"  $ assertEqual "Value isn't parsed correctly"
    (TestOperation $ Value 6.00) (TestOperation $ parser [L.Value "6.00"])

simplePlus :: TestTree
simplePlus = testCase "Simple add operation" $ assertEqual "Problem in operation parsing"
    (TestOperation (BinaryOperation BPlus (Value 1) (Value 2))) (TestOperation $ parser [L.Value "1", L.Plus, L.Value "2"])

simpleMinus :: TestTree
simpleMinus = testCase "Simple minus operation" $ assertEqual "Problem in operation parsing"
    (TestOperation (BinaryOperation BMinus (Value 1) (Value 2))) (TestOperation $ parser [L.Value "1", L.Minus, L.Value "2"])

simpleDiv :: TestTree
simpleDiv = testCase "Simple div operation" $ assertEqual "Problem in operation parsing"
    (TestOperation (BinaryOperation Div (Value 1) (Value 2))) (TestOperation $ parser [L.Value "1", L.Div, L.Value "2"])

simpleMult :: TestTree
simpleMult = testCase "Simple mult operation" $ assertEqual "Problem in operation parsing"
    (TestOperation (BinaryOperation Mult (Value 1) (Value 2))) (TestOperation $ parser [L.Value "1", L.Mult, L.Value "2"])

simplePow :: TestTree
simplePow = testCase "Simple pow operation" $ assertEqual "Problem in operation parsing"
    (TestOperation (BinaryOperation Pow (Value 1) (Value 2))) (TestOperation $ parser [L.Value "1", L.Pow, L.Value "2"])


unaryMinus :: TestTree
unaryMinus = testCase "Unary minus" $ assertEqual "Can't parse unary"
    (TestOperation (UnaryOperation Minus (Value 1))) (TestOperation $ parser [L.Minus, L.Value "1"])

unaryPlus :: TestTree
unaryPlus = testCase "Unary plus" $ assertEqual "Can't parse unary"
    (TestOperation (UnaryOperation Plus (Value 1))) (TestOperation $ parser [L.Plus, L.Value "1"])

parenthesis :: TestTree
parenthesis = testCase "Parenthesis" $ assertEqual "Can't parse contexts"
    (TestOperation $ BinaryOperation Mult (UnaryOperation Parenthesis (BinaryOperation BPlus (Value 1) (Value 2))) (Value 2))
    (TestOperation $ parser [L.OpenParen, L.Value "1", L.Plus, L.Value "2", L.CloseParen, L.Mult, L.Value "2"])

priorityMult :: TestTree
priorityMult = testCase "Priority mult" $ assertEqual "Invalid priority weight"
    (TestOperation $ BinaryOperation BPlus (BinaryOperation Mult (Value 1) (Value 2)) (Value 1))
    (TestOperation $ parser [L.Value "1", L.Mult, L.Value "2", L.Plus, L.Value "1"])

priorityDiv :: TestTree
priorityDiv = testCase "Priority div" $ assertEqual "Invalid priority weight"
    (TestOperation $ BinaryOperation BPlus (BinaryOperation Div (Value 1) (Value 2)) (Value 1))
    (TestOperation $ parser [L.Value "1", L.Div, L.Value "2", L.Plus, L.Value "1"])

priorityPow :: TestTree
priorityPow = testCase "Priority pow" $ assertEqual "Invalid priority weight"
    (TestOperation $ BinaryOperation BPlus (BinaryOperation Pow (Value 1) (Value 2)) (Value 1))
    (TestOperation $ parser [L.Value "1", L.Pow, L.Value "2", L.Plus, L.Value "1"])

wrongParenthesis :: TestTree
wrongParenthesis = testCase "Wrong parenthesis" $ assertEEException
    (ParserException "parenthesis incoherence")
    (evaluate $ parser [L.OpenParen, L.Value "1", L.Plus, L.Value "1", L.CloseParen, L.CloseParen])

invalidMember :: TestTree
invalidMember = testCase "Invalid member" $ assertEEException
    (ParserException "invalid operation member")
    (evaluate $ parser [L.Value "1", L.Plus, L.Plus, L.Value "2"])
