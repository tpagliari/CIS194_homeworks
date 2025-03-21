{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Calc where
import ExprT ( ExprT(..) )
import Parser (parseExp)
import Data.Bool (bool)
import StackVM (Program, StackExp (..))
import qualified ExprT as StackExp

------------------------------------------
-- Exercise 1

{- Evaluator for ExprT -}
eval :: ExprT -> Integer
eval (Lit n) = n
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

------------------------------------------
-- Exercise 2

{- Evaluator for arithmetic expressions given as a String -}
evalString :: String -> Maybe Integer
evalString str = case parseExp Lit ExprT.Add ExprT.Mul str of
    Just exp -> Just (eval exp)  -- If parsing is successful, evaluate and wrap in Just
    Nothing  -> Nothing          -- If parsing fails, return Nothing

------------------------------------------
-- Exercise 3

{- Type class with methods that parallel the constructors of ExprT
to create an abstraction layer over the data constructors -}
class Expr e where
    lit :: Integer -> e
    mul, add :: e -> e -> e

-- Instance of Expr for ExprT
instance Expr ExprT where
    lit = Lit
    mul = ExprT.Mul
    add = ExprT.Add

-- Using lit, mul, add is ambiguous, bc GHC doesn’t know what concrete type to use.
-- but the context determines the type, so we can leverage a function like:
reify :: ExprT -> ExprT
reify = id

------------------------------------------
-- Exercise 4

{- Some instances of Expr for different types -}

-- Integers
instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

-- Booleans
instance Expr Bool where
    lit n = n >= 0
    add = (||)
    mul = (&&)

-- Custom types
newtype MinMax = MinMax Integer deriving Show
instance Expr MinMax where
    lit = MinMax
    add (MinMax n) (MinMax m) = MinMax (max n m)
    mul (MinMax n) (MinMax m) = MinMax (min n m)

data Mod7 where
    Mod7 :: Integer -> Mod7 -- Same idea as newtype
    deriving Show
instance Expr Mod7 where
    lit n = Mod7 $ n `mod` 7
    add (Mod7 n) (Mod7 m) = Mod7 $ (n+m) `mod` 7
    mul (Mod7 n) (Mod7 m) = Mod7 $ (n*m) `mod` 7

------------------------------------------
-- Exercise 5

{- Instance of the Expr type class for Program, so that
arithmetic expressions can be interpreted as compiled programs. -}
instance Expr Program where
        lit n = [PushI n]
        add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
        mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]

{- Function that takes Strings representing arithmetic expressions and
compiles them into programs  -}

compile :: String -> Maybe Program
compile = parseExp lit add mul



