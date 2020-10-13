module Lexer
    ( lexer,
      Token(..),
    ) where

import Control.Exception (throw)
import Text.Read (readMaybe)
import Data.Char (isDigit)

import Exception (EEExceptions (LexerException))

data Token = OpenParen | CloseParen | Plus | Minus | Mult | Div | Pow | Value String deriving (Show)

lexer :: String -> [Token]
lexer []       = []
lexer (' ':xs) = lexer xs
lexer ('(':xs) = OpenParen : lexer xs
lexer (')':xs) = CloseParen : lexer xs
lexer ('+':xs) = Plus : lexer xs
lexer ('-':xs) = Minus : lexer xs
lexer ('*':xs) = Mult : lexer xs
lexer ('/':xs) = Div : lexer xs
lexer ('^':xs) = Pow : lexer xs
lexer str      = (lexer' . getNumber) str

{-# INLINE lexer' #-}
lexer' :: (String, String) -> [Token]
lexer' (nbr, xs) = parseNumber nbr : lexer xs

getNumber :: String -> (String, String)
getNumber = tupleIsAFloat . span isPartOfNumber

tupleIsAFloat :: (String, String) -> (String, String)
tupleIsAFloat t@(a, _)  | isValidFloat a = t
                        | otherwise      = throw LexerException (a ++ " is not a float.")

isValidFloat :: String -> Bool
isValidFloat []       = True
isValidFloat ('.':lx) = isValidFloat' lx
isValidFloat (_:lx)   = isValidFloat lx

isValidFloat' :: String -> Bool
isValidFloat' []      = True
isvalidFloat' ('.':_) = False
isvalidFloat' (_:lx)  = isValidFloat' lx

parseNumber :: String -> Token
parseNumber = parseNumber' . readMaybe

parseNumber' :: Maybe Float -> Token
parseNumber' (Just value) = Value value
parseNumber' _            = throw $ LexerException "Invalid token"

isPartOfNumber :: Char -> Bool
isPartOfNumber '.' = True
isPartOfNumber x   = isDigit x

