module Programs where

import Scheme

-- Finonacci
fibExpr :: [TopExpr]
fibExpr =
  [ Define
      "fib"
      ( Lam
          ["x"]
          ( If
              (App (Var "<") [Var "x", Cst 2])
              (Var "x")
              ( App
                  (Var "+")
                  [ App (Var "fib") [App (Var "-") [Var "x", Cst 1]],
                    App (Var "fib") [App (Var "-") [Var "x", Cst 2]]
                  ]
              )
          )
      ),
    Expr (App (Var "fib") [Cst 20])
  ]

fib :: Int -> Int
fib x = if x < 2 then x else fib (x - 1) + fib (x - 2)

-- Takeuchi

-- Selection Sort