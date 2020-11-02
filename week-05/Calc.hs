module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

-- Exercise 1

eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)

-- Exercise 2






