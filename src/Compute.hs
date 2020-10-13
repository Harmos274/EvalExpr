module Compute
    ( UnaryOperator (..),
      BinaryOperator (..),
      Operation (..),
    ) where

import Exception (EEExceptions(ComputeException))
import Control.Exception (throw)

data UnaryOperator  = Minus  | Plus  | Parenthesis
data BinaryOperator = BMinus | BPlus | Pow | Mult | Div

data Operation = UnaryOperation UnaryOperator Operation
               | BinaryOperation BinaryOperator Operation Operation
               | Value Float -- Computable, Num, Floating, Fractional

class Computable a where
    compute :: a -> a

instance Computable Operation where
    compute v@(Value _)                           = v
    compute (UnaryOperation Minus o)              = -(compute o)
    compute (UnaryOperation _ o)                  = compute o
    compute (BinaryOperation BMinus o1 o2)        = compute o1 - compute o2
    compute (BinaryOperation BPlus o1 o2)         = compute o1 + compute o2
    compute (BinaryOperation Pow o1 o2)           = compute o1 ** compute o2
    compute (BinaryOperation Mult o1 o2)          = compute o1 * compute o2
    compute (BinaryOperation Div o1 (Value 0))    = throw $ ComputeException "Floating point exception"
    compute (BinaryOperation Div o1 o2@(Value _)) = compute o1 / o2
    compute (BinaryOperation Div o1 o2)           = compute o1 / haltIfNull (compute o2)

instance Floating Operation where
    pi                     = Value pi
    exp (Value a)          = Value $ exp a
    log (Value a)          = Value $ log a
    sin (Value a)          = Value $ sin a
    cos (Value a)          = Value $ cos a
    asin (Value a)         = Value $ asin a
    acos (Value a)         = Value $ acos a
    atan (Value a)         = Value $ atan a
    sinh (Value a)         = Value $ sinh a
    cosh (Value a)         = Value $ cosh a
    asinh (Value a)        = Value $ asinh a
    acosh (Value a)        = Value $ acosh a
    atanh (Value a)        = Value $ atanh a

    (Value a) ** (Value b) = Value $ a ** b

instance Num Operation where
    (Value a) - (Value b) = Value $ a - b
    (Value a) + (Value b) = Value $ a + b
    (Value a) * (Value b) = Value $ a * b
    abs (Value a)         = Value $ abs a
    signum (Value a)      = Value $ signum a
    fromInteger a         = Value $ fromInteger a

instance Fractional Operation where
    (Value a) / (Value b) = Value $ a / b
    fromRational a        = Value $ fromRational a

instance Show Operation where
    show (Value a) = show a

haltIfNull :: Operation -> Operation
haltIfNull (Value 0)   = throw $ ComputeException "Floating point exception"
haltIfNull v@(Value _) = v
haltIfNull _           = throw $ ComputeException "Invalid Operation"
