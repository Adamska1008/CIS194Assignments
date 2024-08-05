{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT (ExprT (Add, Lit, Mul))
import Parser (parseExp)
import StackVM qualified

class Expr a where
  add :: a -> a -> a
  lit :: Integer -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add :: MinMax -> MinMax -> MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right

-- before exer 3
-- evalStr :: String -> Maybe Integer
-- evalStr str = case parseExp Lit Add Mul str of
--   Just x -> Just (eval x)
--   Nothing -> Nothing

-- after exer 3
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp lit add mul

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul