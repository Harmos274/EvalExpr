module LexerTest
  ( lexerTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Control.Exception (evaluate)

import Lexer (lexer, Token (..))
import Exception (EEExceptions (LexerException))
import Assert (assertEEException)

newtype TestToken = TestToken Token deriving (Show)

instance Eq TestToken where
    TestToken OpenParen  == TestToken OpenParen  = True
    TestToken CloseParen == TestToken CloseParen = True
    TestToken Plus       == TestToken Plus       = True
    TestToken Minus      == TestToken Minus      = True
    TestToken Mult       == TestToken Mult       = True
    TestToken Div        == TestToken Div        = True
    TestToken Pow        == TestToken Pow        = True
    TestToken (Value s)  == TestToken (Value s2) = s == s2
    _                    == _                    = False

ttLexer :: String -> [TestToken]
ttLexer = map TestToken . lexer

lexerTests :: TestTree
lexerTests = testGroup "Lexer" [emptyList, singleValue, allTokens, withWhitespaces, simpleOperation, complexeOperation, invalidToken]

emptyList :: TestTree
emptyList = testCase "Empty list" $ assertEqual "Token list isn't empty"
    (map TestToken []) (ttLexer "")

singleValue :: TestTree
singleValue = testCase "Single value"  $ assertEqual "Value isn't parsed correctly"
    [TestToken $ Value "6.00"] (ttLexer "6.00")

allTokens :: TestTree
allTokens = testCase "All tokens" $ assertEqual "Token desn't exist"
    (map TestToken [OpenParen, CloseParen, Plus, Minus, Mult , Div, Pow, Value "1"]) (ttLexer "()+-*/^1")

withWhitespaces :: TestTree
withWhitespaces = testCase "Whitespaces" $ assertEqual "Whitespace are not handled correctly"
    (map TestToken  [Value "1", Plus, Value "2"]) (ttLexer "\t      1    +2\n")

simpleOperation :: TestTree
simpleOperation = testCase "Simple operation" $ assertEqual "Invalid operation parsing"
    (map TestToken [Value "1", Plus, Value "2"]) (ttLexer "1+2")

complexeOperation :: TestTree
complexeOperation = testCase "Complexe operation" $ assertEqual "Invalid operation parsing"
    (map TestToken [Value "1", Plus, Value "3", Mult, Value "5.00", Pow, Value "2"]) (ttLexer "1 + 3 * 5.00^2")

invalidToken :: TestTree
invalidToken = testCase "Invalid token" $ assertEEException (LexerException "Invalid token") (evaluate $ lexer "a")


