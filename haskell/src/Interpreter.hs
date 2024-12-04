module Interpreter where

import Scheme

interpret :: [Expr] -> Value
interpret es = snd $ foldl (\(env, _) e -> interp e env) (initEnv, U) es

interp :: Expr -> Env -> (Env, Value)
interp (Var x) env = (env, interpVar x env)
interp (Quote (Var x)) env = (env, Q x)
interp (Cst c) env = (env, C c)
interp (List xs) env =
  let ys =
        foldl
          (\ys x -> let (env1, y) = interp x env in y : ys)
          []
          xs
   in (env, Lst (reverse ys))
interp (If cond csqt alt) env = interpCond cond csqt alt env
interp (Lam [] body) env = (env, interpPcr0 body)
interp (Lam [x] body) env = (env, interpPcr1 x body)
interp (Lam [x, y] body) env = (env, interpPcr2 x y body)
interp (Lam [x, y, z] body) env = (env, interpPcr3 x y z body)
interp (App f []) env = interpApp0 f env
interp (App f [x]) env = interpApp1 x f env
interp (App f [x, y]) env = interpApp2 x y f env
interp (App f [x, y, z]) env = interpApp3 x y z f env

interpVar :: String -> Env -> Value
interpVar x (Env env) =
  case lookup x env of
    Just v -> v
    Nothing -> error ("Unbound variable: " ++ x)

interpCond :: Expr -> Expr -> Expr -> Env -> (Env, Value)
interpCond cond csqt alt env =
  let (env1, v1) = interp cond env
   in if v1 == Q "#t"
        then interp csqt env
        else interp alt env

interpPcr0 :: Expr -> Value
interpPcr0 body = L0 (\env () -> interp body env)

interpPcr1 :: String -> Expr -> Value
interpPcr1 a body =
  L1 (\(Env env) x -> interp body (Env ((a, x) : env)))

interpPcr2 :: String -> String -> Expr -> Value
interpPcr2 a b body =
  L2 (\(Env env) x y -> interp body (Env ((a, x) : (b, y) : env)))

interpPcr3 :: String -> String -> String -> Expr -> Value
interpPcr3 a b c body =
  L3 (\(Env env) x y z -> interp body (Env ((a, x) : (b, y) : (c, z) : env)))

interpApp0 :: Expr -> Env -> (Env, Value)
interpApp0 f env =
  let (env1, L0 f') = interp f env
   in f' env ()

interpApp1 :: Expr -> Expr -> Env -> (Env, Value)
interpApp1 x f env =
  let (_, L1 f') = interp f env
      (_, x') = interp x env
   in f' env x'

interpApp2 :: Expr -> Expr -> Expr -> Env -> (Env, Value)
interpApp2 x y f env =
  let (_, L2 f') = interp f env
      (_, x') = interp x env
      (_, y') = interp y env
   in f' env x' y'

interpApp3 :: Expr -> Expr -> Expr -> Expr -> Env -> (Env, Value)
interpApp3 x y z f env =
  let (_, L3 f') = interp f env
      (_, x') = interp x env
      (_, y') = interp y env
      (_, z') = interp z env
   in f' env x' y' z'
