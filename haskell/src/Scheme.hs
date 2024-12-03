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

-- data TopExpr = Define String Expr | Expr Expr deriving (Show, Eq)

-- isDefine :: TopExpr -> Bool
-- isDefine (Define _ _) = True
-- isDefine _ = False

data Value
  = Q String
  | C Int
  | U ()
  | Lst [Value]
  | L0 (Env -> () -> (Env, Value))
  | L1 (Env -> Value -> (Env, Value))
  | L2 (Env -> Value -> Value -> (Env, Value))
  | L3 (Env -> Value -> Value -> Value -> (Env, Value))

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

newtype Env = Env [(String, Value)] deriving (Show)

emptyEnv :: Env
emptyEnv = Env []

initEnv :: Env
initEnv =
  Env
    [ ("cons", L2 (\env x (Lst xs) -> (env, Lst (x : xs)))),
      ("car", L1 (\env (Lst xs) -> (env, head xs))),
      ("cdr", L1 (\env (Lst xs) -> (env, Lst (tail xs)))),
      ("null?", L1 (\env (Lst xs) -> (env, if null xs then Q "#t" else Q "#f"))),
      ( "not",
        L1
          ( \env x ->
              ( env,
                case x of
                  Q "#t" -> Q "#f"
                  Q "#f" -> Q "#t"
              )
          )
      ),
      ("<", L2 (\env (C c1) (C c2) -> (env, if c1 < c2 then Q "#t" else Q "#f"))),
      ("+", L2 (\env (C c1) (C c2) -> (env, C (c1 + c2)))),
      ("-", L2 (\env (C c1) (C c2) -> (env, C (c1 - c2)))),
      ("define", L2 (\(Env env) (Q f) v -> (Env ((f, v) : env), U ())))
    ]
