import Scheme

-- Finonacci
fibExpr :: Expr
fibExpr =
  App
    (Var "define")
    [ Quote "fib",
      Lam
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
    ]

fib :: Int -> Int
fib x = if x < 2 then x else fib (x - 1) + fib (x - 2)

-- Takeuchi

-- Selection Sort