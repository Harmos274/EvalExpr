module Exception
    (   EEExceptions (..),
        exceptionHandler
    ) where

import Control.Exception (Exception)
import Epitech.ReturnType

data EEExceptions = SendHelp
                | ArgumentException String
                | LexerException String
                | ParserException String
                | ComputeException String
                deriving (Show)

instance Exception EEExceptions

sendHelp :: IO ()
sendHelp = mapM_ putStrLn ["USAGE: ./funEvalExpr e",
                           "\te\tThe expression to compute"]

exceptionHandler :: EEExceptions -> IO ()
exceptionHandler SendHelp              = sendHelp >> success
exceptionHandler (ArgumentException s) = putStrLn ("Argument exception : " ++ s) >> failure
exceptionHandler (LexerException s)    = putStrLn ("Lexing exception : " ++ s) >> failure
exceptionHandler (ParserException s)   = putStrLn ("Parser exception : " ++ s) >> failure
exceptionHandler (ComputeException s)  = putStrLn "Compute exception" >> failure
