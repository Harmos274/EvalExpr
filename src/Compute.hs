module Compute
    ( UnaryOperator (..),
      BinaryOperator (..),
      Operation (..),
    ) where

data UnaryOperator  = Minus  | Plus  | Parenthesis
data BinaryOperator = BMinus | BPlus | Pow | Mult | Div

data Operation = UnaryOperation UnaryOperator Operation
               | BinaryOperation BinaryOperator Operation Operation
               | Value Float
