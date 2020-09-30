module Compute
    ( Algebra (..),
      Computer (..),
      Computable (..),
    ) where

newtype Computer = Computer (Algebra -> Algebra -> Algebra)
data Algebra = Operation Computer Algebra Algebra | Value Double | Invalid String

class Computable a where
    compute :: a -> a

instance Computable Algebra where
    compute v@(Value _)                    = v
    compute i@(Invalid _)                  = i
    compute (Operation (Computer f) a1 a2) = f (compute a1) (compute a2)

instance Show Algebra where
    show (Value v) = show v
    show (Invalid s) = "Algebra error : " ++ s
    show (Operation f a1 a2) = "(" ++ show a1 ++ "Computed with " ++ show a2 ++ ")"

addAlgebra :: Algebra -> Algebra -> Algebra
addAlgebra (Value v1) (Value v2)           = Value $ v1 + v2
addAlgebra o1@Operation {} o2@Operation {} = Operation (Computer addAlgebra) o1 o2
addAlgebra i@(Invalid _) _                 = i
addAlgebra _ i@(Invalid _)                 = i

sousAlgebra :: Algebra -> Algebra -> Algebra
sousAlgebra (Value v1) (Value v2)           = Value $ v1 - v2
sousAlgebra o1@Operation {} o2@Operation {} = Operation (Computer sousAlgebra) o1 o2
sousAlgebra i@(Invalid _) _                 = i
sousAlgebra _ i@(Invalid _)                 = i

mulAlgebra :: Algebra -> Algebra -> Algebra
mulAlgebra (Value v1) (Value v2)           = Value $ v1 * v2
mulAlgebra o1@Operation {} o2@Operation {} = Operation (Computer mulAlgebra) o1 o2
mulAlgebra i@(Invalid _) _                 = i
mulAlgebra _ i@(Invalid _)                 = i

divAlgebra :: Algebra -> Algebra -> Algebra
divAlgebra (Value v1) (Value 0)            = Invalid $ "floating point exception on : " ++ show v1 ++ " / 0"
divAlgebra (Value v1) (Value v2)           = Value $ v1 / v2
divAlgebra o1@Operation {} o2@Operation {} = Operation (Computer divAlgebra) o1 o2
divAlgebra i@(Invalid _) _                 = i
divAlgebra _ i@(Invalid _)                 = i

powAlgebra :: Algebra -> Algebra -> Algebra
powAlgebra (Value v1) (Value v2)           = Value $ v1 ** v2
powAlgebra o1@Operation {} o2@Operation {} = Operation (Computer powAlgebra) o1 o2
powAlgebra i@(Invalid _) _                 = i
powAlgebra _ i@(Invalid _)                 = i
