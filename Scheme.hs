data Expr
  = Var String
  | Quote String
  | Cst Int
  | Set String Expr
  | If Expr Expr Expr
  | Lam [String] Expr
  | App Expr [Expr]
  deriving Show

-- Finonacci
fib :: Expr
fib = App (Var "define") [Quote "fib", 
    Lam ["x"]
        (If (App (Var "<") [Var "x", Cst 2]) 
            (Var "x") 
            (App (Var "+") 
                [App (Var "fib") [App (Var "-") [Var "x", Cst 1]], 
                 App (Var "fib") [App (Var "-") [Var "x", Cst 2]]]))]

-- Takeuchi

-- Selection Sort