module Programs where

import Interpreter
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

fibTest :: Bool
fibTest = C (fib 20) == interpret fibExpr

-- Takeuchi
takExpr :: [TopExpr]
takExpr =
  [ Define
      "tak"
      ( Lam
          ["x", "y", "z"]
          ( If
              (App (Var "not") [App (Var "<") [Var "y", Var "x"]])
              (Var "z")
              ( App
                  (Var "tak")
                  [ App (Var "tak") [App (Var "-") [Var "x", Cst 1], Var "y", Var "z"],
                    App (Var "tak") [App (Var "-") [Var "y", Cst 1], Var "z", Var "x"],
                    App (Var "tak") [App (Var "-") [Var "z", Cst 1], Var "x", Var "y"]
                  ]
              )
          )
      ),
    Expr (App (Var "tak") [Cst 18, Cst 12, Cst 6])
  ]

tak :: Int -> Int -> Int -> Int
tak x y z =
  if not (y < x)
    then z
    else tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)

takTest :: Bool
takTest = C (tak 18 12 6) == interpret takExpr

-- Selection Sort
sortExpr :: [TopExpr]
sortExpr =
  [ Define
      "sort"
      ( Lam
          ["lst"]
          ( If
              (App (Var "null?") [Var "lst"])
              (List [])
              ( App
                  (Var "sort-aux")
                  [App (Var "cdr") [Var "lst"], List [], App (Var "car") [Var "lst"]]
              )
          )
      ),
    Define
      "sort-aux"
      ( Lam
          ["lst", "rest", "min"]
          ( If
              (App (Var "null?") [Var "lst"])
              (App (Var "cons") [Var "min", App (Var "sort") [Var "rest"]])
              ( If
                  (App (Var "<") [App (Var "car") [Var "lst"], Var "min"])
                  (App (Var "sort-aux") [App (Var "cdr") [Var "lst"], App (Var "cons") [Var "min", Var "rest"], App (Var "car") [Var "lst"]])
                  (App (Var "sort-aux") [App (Var "cdr") [Var "lst"], App (Var "cons") [App (Var "car") [Var "lst"], Var "rest"], Var "min"])
              )
          )
      ),
    Expr (App (Var "sort") [])
  ]

sort :: [Int] -> [Int]
sort lst = if null lst then [] else sortAux (tail lst) [] (head lst)

sortAux :: [Int] -> [Int] -> Int -> [Int]
sortAux lst rest min =
  if null lst
    then min : sort rest
    else
      if head lst < min
        then sortAux (tail lst) (min : rest) (head lst)
        else sortAux (tail lst) (head lst : rest) min