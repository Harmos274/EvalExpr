module ComputeTest
  ( computeTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Control.Exception (evaluate)

import Compute (Operation (..), UnaryOperator (..), BinaryOperator (..), Computable (..))
import Assert (assertEEException)
import Exception (EEExceptions (ComputeException))

newtype TestOperation = TestOperation Operation deriving (Eq, Show)

computeTests :: TestTree
computeTests = testGroup "Compute" [value, simpleOperation, unaryMinus, unaryPlus, unaryParenthesis, floatingPoint]

value :: TestTree
value = testCase "Single value" $ assertEqual "Compute change the inner value of Value"
    (TestOperation $ Value 6.00) (TestOperation (compute $ Value 6.00))

simpleOperation :: TestTree
simpleOperation = testCase "Simple operation" $ assertEqual "Invalid computing"
    (TestOperation $ Value 6.00) (TestOperation (compute $ BinaryOperation Mult (Value 3) (Value 2)))

unaryMinus :: TestTree
unaryMinus = testCase "Unary minus" $ assertEqual "Invalid unary minus computing"
    (TestOperation $ Value $ -1.00) (TestOperation (compute $ UnaryOperation Minus $ Value 1))

unaryPlus :: TestTree
unaryPlus = testCase "Unary plus" $ assertEqual "Invalid unary plus computing"
    (TestOperation $ Value 1.00) (TestOperation (compute $ UnaryOperation Plus $ Value 1))

unaryParenthesis :: TestTree
unaryParenthesis = testCase "Unary parenthesis" $ assertEqual "Invalid parenthesis computing"
    (TestOperation $ Value 1.00) (TestOperation (compute $ UnaryOperation Parenthesis $ Value 1))

floatingPoint :: TestTree
floatingPoint = testCase "Floating point" $ assertEEException
    (ComputeException "Floating point exception") (evaluate $ compute $ compute $ BinaryOperation Div (Value 1) (Value 0))