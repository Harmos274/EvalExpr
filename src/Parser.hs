module Parser
    ( parser
    ) where

import Control.Exception (throw)
import Text.Read (readMaybe)

import Lexer (Token (..))
import Compute (Operation (..), UnaryOperator (..), BinaryOperator (..))
import Exception (EEExceptions (ParserException, LexerException))

parser :: [Token] -> Operation
parser [] = throw $ ParserException "No operation given"
parser s  = parser' s

parser' :: [Token] -> Operation
parser' (Lexer.Minus:(Lexer.Value x):xs) = parseOperation xs . UnaryOperation Compute.Minus $ parseNumber x
parser' (Lexer.Plus :(Lexer.Value x):xs) = parseOperation xs . UnaryOperation Compute.Plus  $ parseNumber x
parser' (Lexer.Minus:Lexer.OpenParen:xs) = manageUnaryContexts Compute.Minus $ isolateContexts xs
parser' (Lexer.Plus :Lexer.OpenParen:xs) = manageUnaryContexts Compute.Plus $ isolateContexts xs
parser' s                                = parseInner s

manageUnaryContexts :: UnaryOperator -> ([Token], [Token]) -> Operation
manageUnaryContexts o (inner, after) = parseOperation after . UnaryOperation o $ UnaryOperation Parenthesis $ parser' inner

parseInner :: [Token] -> Operation
parseInner (OpenParen    :xs) = (manageContexts . isolateContexts) xs
parseInner (Lexer.Value v:xs) = parseOperation xs $ parseNumber v
parseInner _                  = throw $ ParserException "invalid operation member"

parseOperation :: [Token] -> Operation -> Operation
parseOperation []               t = t
parseOperation (Lexer.Minus:xs) t = assemble Compute.BMinus t (parseInner xs)
parseOperation (Lexer.Plus :xs) t = assemble Compute.BPlus  t (parseInner xs)
parseOperation (Lexer.Mult :xs) t = assemble Compute.Mult   t (parseInner xs)
parseOperation (Lexer.Div  :xs) t = assemble Compute.Div    t (parseInner xs)
parseOperation (Lexer.Pow  :xs) t = assemble Compute.Pow    t (parseInner xs)
parseOperation _                _ = throw $ ParserException "invalid operation member"

assemble :: BinaryOperator -> Operation -> Operation -> Operation
assemble o v1@(Compute.Value _) v2@(Compute.Value _)                           = BinaryOperation o v1 v2
assemble o v1@UnaryOperation {} v2@(Compute.Value _)                           = BinaryOperation o v1 v2
assemble o v1@UnaryOperation {} v2@UnaryOperation {}                           = BinaryOperation o v1 v2
assemble o v1@UnaryOperation {} (BinaryOperation ope iop1 iop2)                = BinaryOperation ope (BinaryOperation o v1 iop1) iop2
assemble o v1@(Compute.Value _) v2@UnaryOperation {}                           = BinaryOperation o v1 v2
assemble o v1@(Compute.Value _) v2@(BinaryOperation ope iop1 iop2) | o >= ope  = BinaryOperation ope (BinaryOperation o v1 iop1) iop2
                                                                   | otherwise = BinaryOperation o v1 v2
assemble _ _                    _                                              = throw $ ParserException "the impossible has happened"

manageContexts :: ([Token], [Token]) -> Operation
manageContexts (inner, after) = parseOperation (tail after) . UnaryOperation Parenthesis $ parser' inner

isolateContexts :: [Token] -> ([Token], [Token])
isolateContexts ts = splitAt (isolateContexts' 0 0 ts) ts

isolateContexts' :: Int -> Int -> [Token] -> Int
isolateContexts' 0   0 (CloseParen:_)  = throw $ ParserException "parenthesis incoherence"
isolateContexts' _   _ []              = throw $ ParserException "parenthesis incoherence"
isolateContexts' 0   i (CloseParen:_)  = i
isolateContexts' nbr i (OpenParen :xs) = isolateContexts' (nbr + 1) (i + 1) xs
isolateContexts' nbr i (CloseParen:xs) = isolateContexts' (nbr - 1) (i + 1) xs
isolateContexts' nbr i (_         :xs) = isolateContexts'  nbr      (i + 1) xs

parseNumber :: String -> Operation
parseNumber = parseNumber' . readMaybe

parseNumber' :: Maybe Float -> Operation
parseNumber' (Just value) = Compute.Value value
parseNumber' _            = throw $ LexerException "Invalid value token"
