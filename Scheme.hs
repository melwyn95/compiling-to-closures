module Scheme where

data Expr
  = Var String
  | Quote Expr
  | Cst Int
  | List [Expr]
  | If Expr Expr Expr
  | Lam [String] Expr
  | App Expr [Expr]
  deriving (Show, Eq)

data TopExpr = Define String Expr | Expr Expr deriving (Show, Eq)

isDefine :: TopExpr -> Bool
isDefine (Define _ _) = True
isDefine _ = False

data Value
  = Q String
  | C Int
  | U ()
  | Lst [Value]
  | L0 (() -> Value)
  | L1 (Value -> Value)
  | L2 (Value -> Value -> Value)
  | L3 (Value -> Value -> Value -> Value)

instance Show Value where
  show (Q q) = "'" ++ q
  show (C c) = show c
  show (U ()) = show "()"
  show (Lst vs) = show vs
  show (L0 _) = "#<procedure-0>"
  show (L1 _) = "#<procedure-1>"
  show (L2 _) = "#<procedure-2>"
  show (L3 _) = "#<procedure-3>"

instance Eq Value where
  (Q q1) == (Q q2) = q1 == q2
  (C c1) == (C c2) = c1 == c2
  (U ()) == (U ()) = True
  (Lst xs) == (Lst ys) = xs == ys
  _ == _ = False