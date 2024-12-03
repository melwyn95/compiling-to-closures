module Interpreter where

import Scheme

interpret :: [TopExpr] -> Value
interpret es =
  let (Env init) = initEnv
   in let topDefs =
            [ d | t <- es, isDefine t, let d =
                                             let Define f v = t
                                                 Env e = initEnv
                                                 v' = interp v (Env ((f, v') : topDefs ++ e))
                                              in (f, v')
            ]
              ++ init
       in snd $
            foldl
              ( \(env@(Env es), _) e ->
                  case e of
                    Define f v ->
                      let v' = interp v env
                       in (Env ((f, v') : es), U ())
                    Expr e -> (env, interp e env)
              )
              (Env topDefs, U ())
              es

interp :: Expr -> Env -> Value
interp (Var x) env = interpVar x env
interp (Quote (Var x)) env = Q x
interp (Cst c) env = C c
interp (List xs) env = Lst (map (`interp` env) xs)
interp (If cond csqt alt) env = interpCond cond csqt alt env
interp (Lam [] body) env = interpPcr0 body env
interp (Lam [x] body) env = interpPcr1 x body env
interp (Lam [x, y] body) env = interpPcr2 x y body env
interp (Lam [x, y, z] body) env = interpPcr3 x y z body env
interp (App f []) env = interpApp0 f env
interp (App f [x]) env = interpApp1 x f env
interp (App f [x, y]) env = interpApp2 x y f env
interp (App f [x, y, z]) env = interpApp3 x y z f env

interpVar :: String -> Env -> Value
interpVar x (Env env) =
  case lookup x env of
    Just v -> v
    Nothing -> error ("Unbound variable: " ++ x)

interpCond :: Expr -> Expr -> Expr -> Env -> Value
interpCond cond csqt alt env =
  if interp cond env == Q "#t"
    then interp csqt env
    else interp alt env

interpPcr0 :: Expr -> Env -> Value
interpPcr0 body env = L0 (\() -> interp body env)

interpPcr1 :: String -> Expr -> Env -> Value
interpPcr1 a body (Env env) =
  L1 (\x -> interp body (Env ((a, x) : env)))

interpPcr2 :: String -> String -> Expr -> Env -> Value
interpPcr2 a b body (Env env) =
  L2 (\x y -> interp body (Env ((a, x) : (b, y) : env)))

interpPcr3 :: String -> String -> String -> Expr -> Env -> Value
interpPcr3 a b c body (Env env) =
  L3 (\x y z -> interp body (Env ((a, x) : (b, y) : (c, z) : env)))

interpApp0 :: Expr -> Env -> Value
interpApp0 f env =
  let L0 f' = interp f env
   in f' ()

interpApp1 :: Expr -> Expr -> Env -> Value
interpApp1 x f env =
  let L1 f' = interp f env
      x' = interp x env
   in f' x'

interpApp2 :: Expr -> Expr -> Expr -> Env -> Value
interpApp2 x y f env =
  let L2 f' = interp f env
      x' = interp x env
      y' = interp y env
   in f' x' y'

interpApp3 :: Expr -> Expr -> Expr -> Expr -> Env -> Value
interpApp3 x y z f env =
  let L3 f' = interp f env
      x' = interp x env
      y' = interp y env
      z' = interp z env
   in f' x' y' z'