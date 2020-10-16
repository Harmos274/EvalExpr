module Main where

-- System
import Control.Exception (handle, throw)
import System.Environment (getArgs)
--

import Exception (exceptionHandler, EEExceptions(SendHelp, ArgumentException))
import Parser (parser)
import Lexer (lexer)
import Compute(Computable(..))

main :: IO ()
main = handle exceptionHandler $ getArgs >>= command

command :: [String] -> IO ()
command ["--help"] = throw SendHelp
command ["-h"]     = throw SendHelp
command [s]        = print $ compute $ (parser . lexer) s
command _          = throw $ ArgumentException "Invalid arguments, retry with -h"
