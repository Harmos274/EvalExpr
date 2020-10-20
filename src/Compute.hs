module Compute
    ( UnaryOperator (..),
      BinaryOperator (..),
      Operation (..),
      Computable (..),
    ) where

import Exception (EEExceptions(ComputeException))
import Control.Exception (throw)
import Numeric(showFFloat)

data UnaryOperator  = Minus  | Plus  | Parenthesis deriving (Eq, Show)
data BinaryOperator = BMinus | BPlus | Pow | Mult | Div deriving (Eq, Show) -- Ord

data Operation = UnaryOperation UnaryOperator Operation
               | BinaryOperation BinaryOperator Operation Operation
               | Value Float deriving (Eq) -- Computable, Num, Floating, Fractional

class Computable a where
    compute :: a -> a

instance Computable Operation where
    compute v@(Value _)                       = v
    compute (UnaryOperation Minus o)          = -(compute o)
    compute (UnaryOperation _ o)              = compute o
    compute (BinaryOperation BMinus o1 o2)    = compute o1 - compute o2
    compute (BinaryOperation BPlus o1 o2)     = compute o1 + compute o2
    compute (BinaryOperation Pow o1 o2)       = compute o1 ** compute o2
    compute (BinaryOperation Mult o1 o2)      = compute o1 * compute o2
    compute (BinaryOperation Div _ (Value 0)) = throw $ ComputeException "Floating point exception"
    compute (BinaryOperation Div o1 o2)       = compute o1 / haltIfNull (compute o2)

instance Ord BinaryOperator where
    compare Pow Pow       = EQ
    compare Pow Div       = GT
    compare Pow BPlus     = GT
    compare Pow BMinus    = GT
    compare Pow Mult      = GT
    compare BPlus BMinus  = LT
    compare BPlus Pow     = LT
    compare BPlus Mult    = LT
    compare BPlus BPlus   = EQ
    compare BPlus Div     = LT
    compare BMinus BMinus = EQ
    compare BMinus BPlus  = GT
    compare BMinus Pow    = LT
    compare BMinus Div    = LT
    compare BMinus Mult   = LT
    compare Mult Mult     = EQ
    compare Mult Div      = LT
    compare Mult BPlus    = GT
    compare Mult BMinus   = GT
    compare Mult Pow      = LT
    compare Div Div       = EQ
    compare Div Mult      = GT
    compare Div Pow       = LT
    compare Div BPlus     = GT
    compare Div BMinus    = GT

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
    show (Value a)                         = showFFloat (Just 2) a ""
    show (BinaryOperation operator op op2) = "(" ++ show op ++ " " ++ show operator ++ " " ++ show op2 ++ ")"
    show (UnaryOperation operator op )     = "(" ++ show operator ++ " " ++ show op ++ ")"

haltIfNull :: Operation -> Operation
haltIfNull (Value 0)   = throw $ ComputeException "Floating point exception"
haltIfNull v@(Value _) = v
haltIfNull _           = throw $ ComputeException "Invalid Operation"
