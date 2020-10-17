module Assert
  ( assertEEException
  ) where

import Test.HUnit (assertFailure, Assertion)
import Control.Exception (handleJust)
import Control.Monad (guard)

import Exception (EEExceptions (..))

newtype TestException = TestException EEExceptions deriving (Show)

instance Eq TestException where
    (TestException SendHelp)              == (TestException SendHelp)              = True
    (TestException (ArgumentException _)) == (TestException (ArgumentException _)) = True
    (TestException (ParserException _))   == (TestException (ParserException _))   = True
    (TestException (LexerException _))    == (TestException (LexerException _))    = True
    (TestException (ComputeException _))  == (TestException (ComputeException _))  = True
    _                                     == _                                     = False

assertEEException :: EEExceptions -> IO a -> Assertion
assertEEException ex action = handleJust handler (const $ return ()) $
        do  _ <- action
            assertFailure $ "Expected exception: " ++ show ex
        where handler :: EEExceptions -> Maybe ()
              handler e = guard (TestException e == TestException ex)

