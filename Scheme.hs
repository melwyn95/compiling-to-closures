module Scheme where

data Expr
  = Var String
  | Quote String
  | Cst Int
  | Set String Expr
  | If Expr Expr Expr
  | Lam [String] Expr
  | App Expr [Expr]
  deriving (Show)
